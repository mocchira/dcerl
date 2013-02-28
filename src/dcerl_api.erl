-module(dcerl_api).
-author('yoshiyuki.kanno@stoic.co.jp').

-include("dcerl.hrl").

-export([start/3, stop/1]).
-export([put/3, put_begin/2, put_chunk/3, put_end/3, remove/2, get/2, get_chunk/3, flush/1, delete/1]). 


%
% @doc
-spec(start(string(), string(), integer()) -> {ok,#dcerl_state{}}|{error, any()}).
start(DataDir, JournalDir, MaxSize) when MaxSize > 0 ->
    try
        JP = journal_filename(JournalDir),
        BakJP = JP ++ ".bak",
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
                          journaldir_path = JournalDir,
                          datadir_path    = DataDir,
                          max_cache_size  = MaxSize,
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
        journal_rebuild(DS4)
    catch
        error:Reason ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                  [{module, ?MODULE_STRING}, 
                                   {function, "start/3"},
                                   {line, ?LINE}, 
                                   {body, Reason}]),
            {error, Reason}
    end;
start(_, _, _) ->
    {error, badarg}.

%
% @doc
-spec(put(#dcerl_state{}, Key::binary(), Val::binary()) -> ok|{error, any()}).
put(_State, _Key, _Val) -> ok.

%
% @doc
-spec(put_begin(#dcerl_state{}, Key::binary()) -> #dcerl_fd{}|{error, any()}).
put_begin(_State, _Key) -> #dcerl_fd{}.

%
% @doc
-spec(put_chunk(#dcerl_state{}, #dcerl_fd{}, Chunk::binary()) -> ok|{error, any()}).
put_chunk(_State, _Fd, _Chunk) -> ok.

%
% @doc
-spec(put_end(#dcerl_state{}, #dcerl_fd{}, Commit::boolean()) -> ok|{error, any()}).
put_end(_State, _Fd, _Commit) -> ok.

%
% @doc
-spec(remove(#dcerl_state{}, Key::binary()) -> ok|{error, any()}).
remove(_State, _Key) -> ok.

%
% @doc
-spec(get(#dcerl_state{}, Key::binary()) -> 
      {ok, #dcerl_state{}, binary()}|
      {ok, #dcerl_state{}, #dcerl_fd{}}|
      {error, any()}).
get(#dcerl_state{cache_entries     = CE, 
                 cache_stats       = CS,
                 datadir_path      = DataDir,
                 redundant_op_cnt  = OpCnt,
                 journalfile_iodev = IoDev} = State, Key) ->
    case dcerl:get(CE, Key) of
        ok ->
            DataPath = data_filename(DataDir, Key),
            {ok, Bin} = file:read_file(DataPath),
            Line = io_lib:format("~s ~s~n",[?JOURNAL_OP_READ, Key]),
            ok = file:write(IoDev, Line),
            Gets = CS#dcerl_cache_stats.gets,
            {ok, State#dcerl_state
                {redundant_op_cnt = OpCnt + 1,
                 cache_stats = CS#dcerl_cache_stats{gets = Gets + 1}}, Bin};
        _ ->
            {not_found, State}
    end.

%
% @doc
-spec(get_chunk(#dcerl_state{}, #dcerl_fd{}, Key::binary()) -> {Chunk::binary(), Tail::boolean()}|{error, any()}).
get_chunk(_State, _Fd, _Key) ->
    {<<>>, true}.

%
% @doc
% dcerl:flush(Descriptor),
-spec(flush(#dcerl_state{}) -> ok|{error, any()}).
flush(_State) ->
    ok.

%
% @doc
% dcerl:delete(Descriptor),
-spec(delete(#dcerl_state{}) -> ok|{error, any()}).
delete(_State) ->
    ok.

%
% @doc
% dcerl:stop(Descriptor),
-spec(stop(#dcerl_state{}) -> ok|{error, any()}).
stop(_State) ->
    ok.

%
% ==== private functions ====
%

%
% @doc
% journal file operations
-spec(journal_read(#dcerl_state{}) -> {ok, #dcerl_state{}}|{error, any()}).
journal_read(#dcerl_state{journaldir_path = JD} = DState) ->
    JF = journal_filename(JD),
    case file:open(JF, [read, raw, read_ahead]) of
        {ok, IoDev} ->
            try
                {ok, ?JOURNAL_MAGIC} = file:read_line(IoDev),
                journal_read_line(DState#dcerl_state{
                        journalfile_iodev = IoDev,
                        ongoing_keys      = sets:new(),
                        cache_stats       = #dcerl_cache_stats{}
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
                               cache_entries     = CE} = DState, 
                              {ok, Line}) ->
    [Op,Key|_]= string:tokens(Line, ?JOURNAL_SEP),
    BinKey = list_to_binary(Key),
    case Op of
        ?JOURNAL_OP_REMOVE ->
            dcerl:remove(CE, BinKey),
            journal_read_line(DState#dcerl_state{redundant_op_cnt = OpCnt + 1},
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
                    journal_read_line(
                        DState#dcerl_state{
                            redundant_op_cnt = OpCnt + 1,
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
    TmpPath = journal_filename(JournalDir) ++ ".tmp",
    journal_process(DState, delete_file(TmpPath)).

journal_process(_DState, {error, Reason}) ->
    {error, Reason};
journal_process(#dcerl_state{cache_entries = CE} = DState, ok) ->
    journal_process_2(DState, dcerl:iterator(CE)).

journal_process_2(#dcerl_state{cache_entries   = CE,
                               cache_stats     = CS,
                               datadir_path    = DataDir,
                               ongoing_keys    = Keys} = DState, {ok, BinKey}) ->
    case sets:is_element(BinKey, Keys) of
        true -> 
            NewKeys = sets:del_element(BinKey, Keys),
            dcerl:remove(CE, BinKey),
            DataPath = data_filename(DataDir, BinKey),
            TmpPath = DataPath ++ ".tmp",
            file:delete(DataPath),
            file:delete(TmpPath),
            journal_process_2(DState#dcerl_state{ongoing_keys = NewKeys}, dcerl:iterator_next(CE));
        false ->
            DataPath = data_filename(DataDir, BinKey),
            PrevSize = CS#dcerl_cache_stats.cached_size,
            PrevRec = CS#dcerl_cache_stats.records,
            Size = filelib:file_size(DataPath),
            journal_process_2(DState#dcerl_state{
                    cache_stats = CS#dcerl_cache_stats{
                        cached_size = PrevSize + Size,
                        records     = PrevRec + 1}}, dcerl:iterator_next(CE))
    end;
journal_process_2(DState, not_found) ->
    {ok, DState}.

journal_rebuild(#dcerl_state{cache_entries     = CE,
                             journalfile_iodev = undefined,
                             journaldir_path   = JD} = DState) ->
    JP = journal_filename(JD),
    TmpJP = JP ++ ".tmp",
    BakJP = JP ++ ".bak",
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

journal_rebuild_write_line(#dcerl_state{cache_entries     = CE,
                                        datadir_path      = DataDir,
                                        journalfile_iodev = IoDev,
                                        ongoing_keys      = Keys} = DState, {ok, BinKey}) ->
    case sets:is_element(BinKey, Keys) of
        true ->
            ok = file:write(IoDev, io_lib:format("~s ~s~n",[?JOURNAL_OP_DIRTY, BinKey]));
        false ->
            DataPath = data_filename(DataDir, BinKey),
            Size = filelib:file_size(DataPath),
            io:format("test key:~s size:~B~n",[BinKey, Size]),
            Line = io_lib:format("~s ~s ~B~n",[?JOURNAL_OP_CLEAN, BinKey, Size]),
            ok = file:write(IoDev, Line)
    end,
    journal_rebuild_write_line(DState, dcerl:iterator_next(CE));
journal_rebuild_write_line(_DState, not_found) ->
    ok.

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

