-module(dcerl_api).
-author('yoshiyuki.kanno@stoic.co.jp').

-include("dcerl.hrl").

-export([start/4, stop/1]).
-export([stats/1]).
-export([put/3, put_begin/2, put_chunk/3, put_end/3, remove/2, get/2, get_chunk/2, delete/1]). 

-define(SUFFIX_TMP, ".tmp").
-define(SUFFIX_BAK, ".bak").

%
% @doc
-spec(start(string(), string(), integer(), integer()) -> 
      {ok,#dcerl_state{}}|{error, any()}).
start(DataDir, JournalDir, MaxSize, ChunkSize) 
      when MaxSize > 0 andalso ChunkSize > 0 ->
    try
        JP = journal_filename(JournalDir),
        BakJP = JP ++ ?SUFFIX_BAK,
        case filelib:is_regular(BakJP) of
            true ->
                case filelib:is_regular(JP) of
                    true  -> file:delete(BakJP);
                    false -> file:rename(BakJP, JP)
                end;
            false -> void
        end,
        {ok, CE} = dcerl:start(),
        DS = #dcerl_state{
                          journalfile_iodev = undefined,
                          journaldir_path   = JournalDir,
                          datadir_path      = DataDir,
                          ongoing_keys      = sets:new(),
                          datafile_sizes    = dict:new(),
                          cache_stats       = #dcerl_cache_stats{},
                          max_cache_size    = MaxSize,
                          chunk_size        = ChunkSize,
                          cache_entries     = CE
                         },
        DS4 = case filelib:is_regular(JP) of
            true ->
                {ok, DS2} = journal_read(DS),
                {ok, DS3} = journal_process(DS2),
                {ok, IoDev} = file:open(JP, [raw, append]),
                DS3#dcerl_state{journalfile_iodev = IoDev};
            false -> DS
        end,
        ok = filelib:ensure_dir(JournalDir),
        ok = filelib:ensure_dir(DataDir),
        journal_rebuild(DS4)
    catch
        error:Reason ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                  [{module, ?MODULE_STRING}, 
                                   {function, "start/4"},
                                   {line, ?LINE}, 
                                   {body, Reason}]),
            {error, Reason}
    end;
start(_, _, _, _) ->
    {error, badarg}.

%
% @doc
-spec(put(#dcerl_state{}, BinKey::binary(), Val::binary()) ->
      {ok, #dcerl_state{}}|{error, any()}).
put(#dcerl_state{journalfile_iodev = undefined} = _State, _Key, _Val) ->
    {error, badarg};
put(#dcerl_state{cache_entries     = CE, 
                 cache_stats       = CS,
                 datadir_path      = DataDir,
                 ongoing_keys      = OnKeys,
                 datafile_sizes    = FileSizes,
                 redundant_op_cnt  = OpCnt,
                 journalfile_iodev = IoDev} = State, BinKey, Val) ->
    try
        %%% write to tmp
        StrKey = filename_bin2str(BinKey),
        Line = io_lib:format("~s ~s~n",[?JOURNAL_OP_DIRTY, StrKey]),
        ok = file:write(IoDev, Line),
        ok = file:datasync(IoDev),
        OnKeys2 = sets:add_element(BinKey, OnKeys),
        DP = data_filename(DataDir, BinKey),
        TmpDP = DP ++ ?SUFFIX_TMP,
        DiffRec = case filelib:is_regular(DP) of
            true -> 0;
            false -> 1
        end,
        OldSize = case dict:find(BinKey, FileSizes) of
            {ok, Size} -> Size;
            _ -> 0
        end,
        DiffSize = erlang:size(Val) - OldSize,
        ok = file:write_file(TmpDP, Val),
        %%% commit(rename AND write to journal)
        ok = file:rename(TmpDP, DP),
        CommitLine = io_lib:format("~s ~s ~B~n",[?JOURNAL_OP_CLEAN, StrKey, erlang:size(Val)]),
        ok = file:write(IoDev, CommitLine),
        OnKeys3 = sets:del_element(BinKey, OnKeys2),
        FileSizes2 = dict:store(BinKey, erlang:size(Val), FileSizes),
        ok = dcerl:put(CE, BinKey),
        Puts = CS#dcerl_cache_stats.puts,
        PrevSize = CS#dcerl_cache_stats.cached_size,
        PrevRec = CS#dcerl_cache_stats.records,
        NewState = State#dcerl_state
            {redundant_op_cnt = OpCnt + 1,
             ongoing_keys     = OnKeys3,
             datafile_sizes   = FileSizes2,
             cache_stats      = CS#dcerl_cache_stats
                    {puts        = Puts + 1,
                     records     = PrevRec + DiffRec,
                     cached_size = PrevSize + DiffSize}},
        {ok, TrimedState} = trim_to_size(NewState),
        journal_rebuild_as_need(TrimedState)
    catch
        error:Reason ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                  [{module, ?MODULE_STRING}, 
                                   {function, "put/3"},
                                   {line, ?LINE}, 
                                   {body, Reason}]),
            {error, Reason}
    end.

%
% @doc
-spec(put_begin(#dcerl_state{}, BinKey::binary()) ->
     {ok, #dcerl_state{}, #dcerl_fd{}}|{error, any()}).
put_begin(#dcerl_state{journalfile_iodev = undefined} = _State, _Key) ->
    {error, badarg};
put_begin(#dcerl_state{datadir_path      = DataDir,
                       ongoing_keys      = OnKeys,
                       journalfile_iodev = IoDev} = State, BinKey) ->
    try
        StrKey = filename_bin2str(BinKey),
        Line = io_lib:format("~s ~s~n",[?JOURNAL_OP_DIRTY, StrKey]),
        ok = file:write(IoDev, Line),
        ok = file:datasync(IoDev),
        OnKeys2 = sets:add_element(BinKey, OnKeys),
        TmpDP = data_filename(DataDir, BinKey) ++ ?SUFFIX_TMP,
        {ok, TmpIoDev} = file:open(TmpDP, [write, raw, delayed_write]),
        {ok, State#dcerl_state{ongoing_keys = OnKeys2}, 
                  #dcerl_fd{key                = BinKey, 
                            tmp_datafile_iodev = TmpIoDev}}
    catch
        error:Reason ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                  [{module, ?MODULE_STRING}, 
                                   {function, "put_begin/2"},
                                   {line, ?LINE}, 
                                   {body, Reason}]),
            {error, Reason}
    end.

%
% @doc
-spec(put_chunk(#dcerl_state{}, #dcerl_fd{}, Chunk::binary()) ->
      ok|{error, any()}).
put_chunk(_State, #dcerl_fd{tmp_datafile_iodev = TmpIoDev} = _Fd, Chunk) ->
    file:write(TmpIoDev, Chunk).

%
% @doc
-spec(put_end(#dcerl_state{}, #dcerl_fd{}, Commit::boolean()) ->
     {ok, #dcerl_state{}}|{error, any()}).
put_end(#dcerl_state{cache_entries     = CE, 
                     cache_stats       = CS,
                     datadir_path      = DataDir,
                     datafile_sizes    = FileSizes,
                     ongoing_keys      = OnKeys,
                     redundant_op_cnt  = OpCnt,
                     journalfile_iodev = IoDev} = State, 
           #dcerl_fd{tmp_datafile_iodev = TmpIoDev,
                     key                = BinKey} = _Fd, true) ->
    try
        _ = file:close(TmpIoDev),
        DP = data_filename(DataDir, BinKey),
        TmpDP = DP ++ ?SUFFIX_TMP,
        DiffRec = case filelib:is_regular(DP) of
            true -> 0;
            false -> 1
        end,
        OldSize = case dict:find(BinKey, FileSizes) of
            {ok, Size} -> Size;
            _ -> 0
        end,
        NewSize = filelib:file_size(TmpDP),
        DiffSize = NewSize - OldSize,
        ok = file:rename(TmpDP, DP),
        StrKey = filename_bin2str(BinKey),
        CommitLine = io_lib:format("~s ~s ~B~n",[?JOURNAL_OP_CLEAN, StrKey, NewSize]),
        ok = file:write(IoDev, CommitLine),
        OnKeys2 = sets:del_element(BinKey, OnKeys),
        FileSizes2 = dict:store(BinKey, NewSize, FileSizes),
        ok = dcerl:put(CE, BinKey),
        Puts = CS#dcerl_cache_stats.puts,
        PrevSize = CS#dcerl_cache_stats.cached_size,
        PrevRec = CS#dcerl_cache_stats.records,
        NewState = State#dcerl_state
            {redundant_op_cnt = OpCnt + 1,
             ongoing_keys     = OnKeys2,
             datafile_sizes   = FileSizes2,
             cache_stats      = CS#dcerl_cache_stats
                    {puts        = Puts + 1,
                     records     = PrevRec + DiffRec,
                     cached_size = PrevSize + DiffSize}},
        {ok, TrimedState} = trim_to_size(NewState),
        journal_rebuild_as_need(TrimedState)
    catch
        error:Reason ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                  [{module, ?MODULE_STRING}, 
                                   {function, "put_end/3"},
                                   {line, ?LINE}, 
                                   {body, Reason}]),
            {error, Reason}
    end;

put_end(#dcerl_state{datadir_path      = DataDir,
                     ongoing_keys      = OnKeys,
                     redundant_op_cnt  = OpCnt} = State, 
           #dcerl_fd{tmp_datafile_iodev = TmpIoDev,
                     key                = BinKey} = _Fd, false) ->
    try
        _ = file:close(TmpIoDev),
        DP = data_filename(DataDir, BinKey),
        TmpDP = DP ++ ?SUFFIX_TMP,
        ok = file:delete(TmpDP),
        OnKeys2 = sets:del_element(BinKey, OnKeys),
        {ok, State#dcerl_state
                    {redundant_op_cnt = OpCnt + 1,
                     ongoing_keys     = OnKeys2}}
    catch
        error:Reason ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                  [{module, ?MODULE_STRING}, 
                                   {function, "put_end/3"},
                                   {line, ?LINE}, 
                                   {body, Reason}]),
            {error, Reason}
    end.

%
% @doc
-spec(remove(#dcerl_state{}, BinKey::binary()) ->
     {ok, #dcerl_state{}}|{error, any()}).
remove(#dcerl_state{journalfile_iodev = undefined} = _State, _Key) ->
    {error, badarg};
remove(#dcerl_state{cache_entries     = CE, 
                    cache_stats       = CS,
                    datadir_path      = DataDir,
                    ongoing_keys      = OnKeys,
                    datafile_sizes    = FileSizes,
                    redundant_op_cnt  = OpCnt,
                    journalfile_iodev = IoDev} = State, BinKey) ->
    DP = data_filename(DataDir, BinKey),
    case filelib:is_regular(DP) of
        true ->
            try
                DiffSize = case dict:find(BinKey, FileSizes) of
                               {ok, Size} -> Size;
                               _ -> 0
                           end,
                ok = file:delete(DP),
                StrKey = filename_bin2str(BinKey),
                Line = io_lib:format("~s ~s~n",[?JOURNAL_OP_REMOVE, StrKey]),
                ok = file:write(IoDev, Line),
                OnKeys2 = sets:del_element(BinKey, OnKeys),
                FileSizes2 = dict:erase(BinKey, FileSizes),
                ok = dcerl:remove(CE, BinKey),
                Dels = CS#dcerl_cache_stats.dels,
                PrevSize = CS#dcerl_cache_stats.cached_size,
                PrevRec = CS#dcerl_cache_stats.records,
                NewState = State#dcerl_state
                    {redundant_op_cnt = OpCnt + 1,
                     ongoing_keys     = OnKeys2,
                     datafile_sizes   = FileSizes2,
                     cache_stats      = CS#dcerl_cache_stats
                            {dels        = Dels + 1,
                             records     = PrevRec - 1,
                             cached_size = PrevSize - DiffSize}},
                journal_rebuild_as_need(NewState)
            catch
                error:Reason ->
                    error_logger:error_msg("~p,~p,~p,~p~n",
                                  [{module, ?MODULE_STRING}, 
                                   {function, "remove/2"},
                                   {line, ?LINE}, 
                                   {body, Reason}]),
                {error, Reason}
            end;
        false ->
            {ok, State}
    end.

%
% @doc
-spec(get(#dcerl_state{}, BinKey::binary()) -> 
      {ok, #dcerl_state{}, binary()}|
      {ok, #dcerl_state{}, #dcerl_fd{}}|
      {error, any()}).
get(#dcerl_state{journalfile_iodev = undefined} = _State, _Key) ->
    {error, badarg};
get(#dcerl_state{cache_entries     = CE, 
                 cache_stats       = CS,
                 datadir_path      = DataDir,
                 datafile_sizes    = FileSizes,
                 chunk_size        = ChunkSize,
                 redundant_op_cnt  = OpCnt,
                 journalfile_iodev = IoDev} = State, BinKey) ->
    case dcerl:get(CE, BinKey) of
        ok ->
            try
                Chunked = case dict:find(BinKey, FileSizes) of
                    {ok, Size} -> Size > ChunkSize;
                    _ -> false
                end,
                case Chunked of
                    false ->
                        DataPath = data_filename(DataDir, BinKey),
                        {ok, Bin} = file:read_file(DataPath),
                        StrKey = filename_bin2str(BinKey),
                        Line = io_lib:format("~s ~s~n",[?JOURNAL_OP_READ, StrKey]),
                        ok = file:write(IoDev, Line),
                        Gets = CS#dcerl_cache_stats.gets,
                        Hits = CS#dcerl_cache_stats.hits,
                        NewState = State#dcerl_state
                               {redundant_op_cnt = OpCnt + 1,
                                cache_stats      = CS#dcerl_cache_stats
                                    {gets = Gets + 1,
                                     hits = Hits + 1}},
                        {ok, TrimedState} = trim_to_size(NewState),
                        {ok, RebuildState} = journal_rebuild_as_need(TrimedState),
                        {ok, RebuildState, Bin};
                    true ->
                        DataPath = data_filename(DataDir, BinKey),
                        {ok, TmpIoDev} = file:open(DataPath, [read, binary, raw, read_ahead]),
                        {ok, State, #dcerl_fd{key = BinKey, 
                                              tmp_datafile_iodev = TmpIoDev}}
                end
            catch
                error:Reason ->
                    error_logger:error_msg("~p,~p,~p,~p~n",
                                       [{module, ?MODULE_STRING}, 
                                        {function, "get/2"},
                                        {line, ?LINE}, 
                                        {body, Reason}]),
                    {error, Reason}
            end;
        _ ->
            Gets = CS#dcerl_cache_stats.gets,
            {not_found, State#dcerl_state{
                    cache_stats = CS#dcerl_cache_stats{
                        gets = Gets + 1}}}
    end.

%
% @doc
-spec(get_chunk(#dcerl_state{}, #dcerl_fd{}) ->
     {ok, #dcerl_state{}, #dcerl_fd{}, Chunk::binary(), Tail::boolean()}|
     {error, any()}).
get_chunk(#dcerl_state{cache_stats       = CS,
                       chunk_size        = ChunkSize,
                       redundant_op_cnt  = OpCnt,
                       journalfile_iodev = IoDev} = State, 
          #dcerl_fd{key                = BinKey,
                    tmp_datafile_iodev = TmpIoDev} = Fd) ->
    case file:read(TmpIoDev, ChunkSize) of
        {ok, Data} ->
            {ok, State, Fd, Data, false};
        eof ->
            try
                _ = file:close(TmpIoDev),
                StrKey = filename_bin2str(BinKey),
                Line = io_lib:format("~s ~s~n",[?JOURNAL_OP_READ, StrKey]),
                ok = file:write(IoDev, Line),
                Gets = CS#dcerl_cache_stats.gets,
                Hits = CS#dcerl_cache_stats.hits,
                NewState = State#dcerl_state
                               {redundant_op_cnt = OpCnt + 1,
                                cache_stats = CS#dcerl_cache_stats{
                                    gets = Gets + 1,
                                    hits = Hits + 1}},
                {ok, TrimedState} = trim_to_size(NewState),
                {ok, RebuildState} = journal_rebuild_as_need(TrimedState),
                {ok, RebuildState, Fd, <<>>, true}
            catch
                error:Reason ->
                    error_logger:error_msg("~p,~p,~p,~p~n",
                                       [{module, ?MODULE_STRING}, 
                                        {function, "get_chunk/2"},
                                        {line, ?LINE}, 
                                        {body, Reason}]),
                    {error, Reason}
            end;
        {error, Reason} ->
            _ = file:close(TmpIoDev),
            {error, Reason}
    end.

%
% @doc
% dcerl:delete(Descriptor),
-spec(delete(#dcerl_state{}) -> {ok, #dcerl_state{}}|{error, any()}).
delete(#dcerl_state{datadir_path      = DataDir,
                    journaldir_path   = JournalDir} = State) ->
    try
        {ok, NewState} = stop(State),
        file_delete_all(JournalDir),
        file_delete_all(DataDir),
        {ok, NewState}
    catch
        error:Reason ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                  [{module, ?MODULE_STRING}, 
                                   {function, "delete/1"},
                                   {line, ?LINE}, 
                                   {body, Reason}]),
            {error, Reason}
    end.

%
% @doc
% dcerl:stop(Descriptor),
-spec(stop(#dcerl_state{}) -> {ok, #dcerl_state{}}|{error, any()}).
stop(#dcerl_state{journalfile_iodev = undefined} = State) ->
    {ok, State};
stop(#dcerl_state{journalfile_iodev = IoDev} = State) ->
    try
        {ok, NewState} = trim_to_size(State),
        file:close(IoDev),
        {ok, NewState#dcerl_state{journalfile_iodev = undefined}}
    catch
        error:Reason ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                  [{module, ?MODULE_STRING}, 
                                   {function, "stop/1"},
                                   {line, ?LINE}, 
                                   {body, Reason}]),
            {error, Reason}
    end.

-spec(stats(#dcerl_state{}) -> {ok, #dcerl_cache_stats{}}).
stats(#dcerl_state{cache_stats = CS} = _State) ->
    {ok, CS}.

%
% ==== private functions ====
%

%
% @doc
% journal file operations
trim_to_size(#dcerl_state{cache_entries = CE} = State) ->
    trim_to_size(State, dcerl:eldest(CE)).

trim_to_size(#dcerl_state{cache_stats = 
                            #dcerl_cache_stats{
                                cached_size = TotalSize
                            } = _CS,
                          max_cache_size = MaxSize} = State, _) 
        when TotalSize < MaxSize ->
    {ok, State};
trim_to_size(State, not_found) ->
    {ok, State};
trim_to_size(#dcerl_state{cache_entries = CE} = State, {ok, BinKey}) ->
    {ok, NewState} = remove(State, BinKey),
    trim_to_size(NewState, dcerl:eldest(CE)).

-spec(journal_read(#dcerl_state{}) -> {ok, #dcerl_state{}}|{error, any()}).
journal_read(#dcerl_state{journaldir_path = JD} = DState) ->
    JF = journal_filename(JD),
    case file:open(JF, [read, raw, read_ahead]) of
        {ok, IoDev} ->
            try
                {ok, ?JOURNAL_MAGIC} = file:read_line(IoDev),
                journal_read_line(
                    DState#dcerl_state{
                        journalfile_iodev = IoDev
                    })
            catch
                error:Reason ->
                    error_logger:error_msg("~p,~p,~p,~p~n",
                                           [{module, ?MODULE_STRING}, 
                                            {function, "journal_read/1"},
                                            {line, ?LINE}, 
                                            {body, Reason}]),
                    {error, Reason}
            after
                file:close(IoDev)
            end; 
        Error ->
            Error
    end.

journal_read_line(#dcerl_state{journalfile_iodev = IoDev} = DState) ->
    Line = file:read_line(IoDev),
    journal_read_line(DState, Line).

journal_read_line(#dcerl_state{journalfile_iodev = IoDev, 
                               redundant_op_cnt  = OpCnt,
                               ongoing_keys      = OnKeys,
                               datafile_sizes    = FileSizes,
                               cache_entries     = CE} = DState, 
                              {ok, Line}) ->
    [Op,StrKey|Rest]= string:tokens(Line, ?JOURNAL_SEP),
    BinKey = filename_str2bin(StrKey),
    case Op of
        ?JOURNAL_OP_REMOVE ->
            dcerl:remove(CE, BinKey),
            FileSizes2 = dict:erase(BinKey, FileSizes),
            journal_read_line(DState#dcerl_state{
                              datafile_sizes   = FileSizes2,
                              redundant_op_cnt = OpCnt + 1},
                              file:read_line(IoDev));
        _ -> 
            case dcerl:get(CE, BinKey) of
                not_found ->
                    dcerl:put(CE, BinKey);
                _ -> void
            end,
            case Op of
                ?JOURNAL_OP_DIRTY ->
                    journal_read_line(
                        DState#dcerl_state{
                            redundant_op_cnt = OpCnt + 1,
                            ongoing_keys     = sets:add_element(BinKey, OnKeys)},
                        file:read_line(IoDev));
                ?JOURNAL_OP_CLEAN ->
                    Size = list_to_integer(hd(Rest)),
                    FileSizes2 = dict:store(BinKey, Size, FileSizes),
                    journal_read_line(
                        DState#dcerl_state{
                            redundant_op_cnt = OpCnt + 1,
                            datafile_sizes   = FileSizes2,
                            ongoing_keys     = sets:del_element(BinKey, OnKeys)},
                        file:read_line(IoDev));
                ?JOURNAL_OP_READ ->
                    journal_read_line(
                        DState#dcerl_state{
                            redundant_op_cnt = OpCnt + 1},
                        file:read_line(IoDev));
                _ ->
                    {error, invalid_journal_format}
            end
    end;
journal_read_line(_DState, {error, Reason}) ->
    {error, Reason};
journal_read_line(#dcerl_state{redundant_op_cnt = OpCnt,
                               cache_entries    = CE
                              } = DState, 
                              eof) ->
    {ok, NumItem} = dcerl:items(CE),
    {ok, DState#dcerl_state{redundant_op_cnt = OpCnt - NumItem}}.

journal_process(#dcerl_state{journaldir_path = JournalDir} = DState) ->
    TmpPath = journal_filename(JournalDir) ++ ?SUFFIX_TMP,
    journal_process(DState, delete_file(TmpPath)).

journal_process(_DState, {error, Reason}) ->
    {error, Reason};
journal_process(#dcerl_state{cache_entries = CE} = DState, ok) ->
    journal_process_2(DState, dcerl:iterator(CE)).

journal_process_2(#dcerl_state{cache_entries   = CE,
                               cache_stats     = CS,
                               datadir_path    = DataDir,
                               datafile_sizes  = FileSizes,
                               ongoing_keys    = Keys} = DState, {ok, BinKey}) ->
    case sets:is_element(BinKey, Keys) of
        true -> 
            NewKeys = sets:del_element(BinKey, Keys),
            dcerl:remove(CE, BinKey),
            DataPath = data_filename(DataDir, BinKey),
            TmpPath = DataPath ++ ?SUFFIX_TMP,
            file:delete(DataPath),
            file:delete(TmpPath),
            journal_process_2(DState#dcerl_state{ongoing_keys = NewKeys}, dcerl:iterator_next(CE));
        false ->
            PrevSize = CS#dcerl_cache_stats.cached_size,
            PrevRec = CS#dcerl_cache_stats.records,
            NewSize = case dict:find(BinKey, FileSizes) of
                       {ok, Size} -> Size + PrevSize;
                       _ -> PrevSize
                   end,
            journal_process_2(DState#dcerl_state{
                    cache_stats = CS#dcerl_cache_stats{
                        cached_size = NewSize,
                        records     = PrevRec + 1}}, dcerl:iterator_next(CE))
    end;
journal_process_2(DState, not_found) ->
    {ok, DState}.

journal_rebuild_as_need(#dcerl_state{cache_entries     = CE,
                                     redundant_op_cnt  = OpCnt} = DState) ->
    {ok, NumItem} = dcerl:items(CE),
    case OpCnt >= ?JOURNAL_MAX_RED_OP_CNT andalso 
         OpCnt >= NumItem of
        true ->
            {ok, NewState} = journal_rebuild(DState),
            {ok, NewState#dcerl_state{redundant_op_cnt = 0}};
        false ->
            {ok, DState}
    end.

journal_rebuild(#dcerl_state{cache_entries     = CE,
                             journalfile_iodev = undefined,
                             journaldir_path   = JD} = DState) ->
    JP = journal_filename(JD),
    TmpJP = JP ++ ?SUFFIX_TMP,
    BakJP = JP ++ ?SUFFIX_BAK,
    Ret = case file:open(TmpJP, [append, raw, delayed_write]) of
        {ok, IoDev} ->
            try
                ok = file:write(IoDev, ?JOURNAL_MAGIC),
                journal_rebuild_write_line(
                    DState#dcerl_state{journalfile_iodev = IoDev}, dcerl:iterator(CE))
            catch
                error:Reason ->
                    error_logger:error_msg("~p,~p,~p,~p~n",
                                           [{module, ?MODULE_STRING}, 
                                            {function, "journal_rebuild/1"},
                                            {line, ?LINE}, 
                                            {body, Reason}]),
                    {error, Reason}
            after
                file:close(IoDev)
            end;
        Error ->
            Error
    end,
    case Ret of
        ok ->
            try
                case filelib:is_regular(JP) of
                    true ->
                        file:delete(BakJP),
                        ok = file:rename(JP, BakJP);
                    false ->
                        void
                end,
                ok = file:rename(TmpJP, JP),
                file:delete(BakJP),
                {ok, IoDev2} = file:open(JP, [raw, append]),
                {ok, DState#dcerl_state{journalfile_iodev = IoDev2}}
            catch
                error:Reason2 ->
                    error_logger:error_msg("~p,~p,~p,~p~n",
                                           [{module, ?MODULE_STRING}, 
                                            {function, "journal_rebuild/1"},
                                            {line, ?LINE}, 
                                            {body, Reason2}]),
                    {error, Reason2}
            end;
        Error2 ->
            Error2
    end;
journal_rebuild(#dcerl_state{journalfile_iodev = IoDev} = DState) ->
    file:close(IoDev),
    journal_rebuild(DState#dcerl_state{journalfile_iodev = undefined}).

journal_rebuild_write_line(
    #dcerl_state{cache_entries     = CE,
                 journalfile_iodev = IoDev,
                 datafile_sizes    = FileSizes,
                 ongoing_keys      = Keys} = DState, {ok, BinKey}) ->
    StrKey = filename_bin2str(BinKey),
    case sets:is_element(BinKey, Keys) of
        true ->
            ok = file:write(IoDev, io_lib:format("~s ~s~n",[?JOURNAL_OP_DIRTY, StrKey]));
        false ->
            NewSize = case dict:find(BinKey, FileSizes) of
                       {ok, Size} -> Size;
                       _ -> 0
                   end,
            Line = io_lib:format("~s ~s ~B~n",[?JOURNAL_OP_CLEAN, StrKey, NewSize]),
            ok = file:write(IoDev, Line)
    end,
    journal_rebuild_write_line(DState, dcerl:iterator_next(CE));
journal_rebuild_write_line(_DState, not_found) ->
    ok.

filename_bin2str(BinKey) when is_binary(BinKey) ->
    StrKey = binary_to_list(BinKey),
    http_uri:encode(StrKey).
filename_str2bin(StrKey) when is_list(StrKey) ->
    DecStr = http_uri:decode(StrKey),
    list_to_binary(DecStr).

data_filename(DataDir, BinKey) ->
    StrKey = binary_to_list(BinKey),
    filename:join(DataDir, http_uri:encode(StrKey)).

journal_filename(JournalDir) ->
    filename:join(JournalDir, ?JOURNAL_FNAME).

delete_file(Path) ->
    case filelib:is_regular(Path) of
        true ->
            file:delete(Path);
        false ->
            ok
    end.

file_delete_all(Path) ->
    case filelib:is_dir(Path) of
        true ->
            case file:list_dir(Path) of
                {ok, Files} ->
                    lists:foreach(
                      fun(A) ->
                              file_delete_all(filename:join(Path, A))
                      end, Files);
                Error ->
                    throw(Error)
            end,
            case file:del_dir(Path) of
                ok ->
                    ok;
                ErrorDel ->
                    throw(ErrorDel)
            end;
        false ->
            case filelib:is_regular(Path) of
                true ->
                    case file:delete(Path) of
                        ok ->
                            ok;
                        Error ->
                            throw(Error)
                    end;
                false ->
                    ok
            end
    end.
