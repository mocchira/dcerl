-module(dcerl_api_tests).
-author('yoshiyuki.kanno@stoic.co.jp').

-include("dcerl.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST FUNCTIONS
%%--------------------------------------------------------------------
-ifdef(EUNIT).
%% gen test data
init_source() ->
    SourceSz = 1024 * 1024,
    {SourceSz, crypto:rand_bytes(SourceSz)}.
data_block({SourceSz, Source}, BlockSize) ->
    case SourceSz - BlockSize > 0 of
        true ->
            Offset = random:uniform(SourceSz - BlockSize),
            <<_:Offset/bytes, Slice:BlockSize/bytes, _Rest/binary>> = Source,
            Slice;
        false ->
            Source
    end.

normal_test() ->
    {ok, Dir} = file:get_cwd(),
    Src = init_source(),
    DataDir = filename:join(Dir, "data"),
    JournalDir = filename:join(Dir, "journal"),
    {ok, DS} = dcerl_api:start(DataDir, JournalDir, 1024, 128),
    BinBody = data_block(Src, 128),
    BinKey = <<"test.com/b/path_to_file.jpg">>,
    {ok, DS2} = dcerl_api:put(DS, BinKey, BinBody),
    {ok, CS} = dcerl_api:stats(DS2),
    ?assertEqual(1, CS#dcerl_cache_stats.puts),
    ?assertEqual(1, CS#dcerl_cache_stats.records),
    {ok, DS3, BinBody} = dcerl_api:get(DS2, BinKey),
    {ok, DS4} = dcerl_api:remove(DS3, BinKey),
    {ok, CS2} = dcerl_api:stats(DS4),
    ?assertEqual(1, CS2#dcerl_cache_stats.dels),
    ?assertEqual(0, CS2#dcerl_cache_stats.records),
    {ok, DS6, FD} = dcerl_api:put_begin(DS4, BinKey),
    Chunk = data_block(Src, 128),
    ok  = dcerl_api:put_chunk(DS6, FD, Chunk),
    ok  = dcerl_api:put_chunk(DS6, FD, Chunk),
    ok  = dcerl_api:put_chunk(DS6, FD, Chunk),
    {ok, DS7} = dcerl_api:put_end(DS6, FD, true),
    {ok, CS3} = dcerl_api:stats(DS7),
    ?assertEqual(2, CS3#dcerl_cache_stats.puts),
    ?assertEqual(1, CS3#dcerl_cache_stats.records),
    {ok, DS8, FD2} = dcerl_api:get(DS7, BinKey),
    {ok, DS9} = get_chunked(DS8, FD2, Chunk),
    {ok, CS4} = dcerl_api:stats(DS9),
    ?assertEqual(2, CS4#dcerl_cache_stats.gets),
    ?assertEqual(2, CS4#dcerl_cache_stats.hits),
    ?assertEqual(1, CS4#dcerl_cache_stats.records),
    {ok, _} = dcerl_api:delete(DS9),
    ok.

roll_test() ->
    {ok, Dir} = file:get_cwd(),
    Src = init_source(),
    DataDir = filename:join(Dir, "data"),
    JournalDir = filename:join(Dir, "journal"),
    {ok, DS} = dcerl_api:start(DataDir, JournalDir, 1024, 512),
    BinBody = data_block(Src, 384),
    BinKey = <<"test.com/b/path_to_file.jpg">>,
    {ok, DS2} = dcerl_api:put(DS, BinKey, BinBody),
    {ok, DS3} = dcerl_api:put(DS2, <<BinKey/binary, <<".1">>/binary >>, BinBody),
    {ok, DS4} = dcerl_api:put(DS3, <<BinKey/binary, <<".2">>/binary >>, BinBody),
    {not_found, DS5} = dcerl_api:get(DS4, BinKey),  
    {ok, CS} = dcerl_api:stats(DS5),
    ?assertEqual(2, CS#dcerl_cache_stats.records),
    {ok, DS6, BinBody} = dcerl_api:get(DS5, <<BinKey/binary, <<".1">>/binary >>),  
    {ok, DS7} = dcerl_api:put(DS6, <<BinKey/binary, <<".3">>/binary >>, BinBody),
    {not_found, DS8} = dcerl_api:get(DS7, <<BinKey/binary, <<".2">>/binary >>),  
    {ok, CS2} = dcerl_api:stats(DS8),
    ?assertEqual(2, CS2#dcerl_cache_stats.records),
    ok.

recover_test() ->
    {ok, Dir} = file:get_cwd(),
    Src = init_source(),
    DataDir = filename:join(Dir, "data"),
    JournalDir = filename:join(Dir, "journal"),
    {ok, DS} = dcerl_api:start(DataDir, JournalDir, 1024, 512),
    BinBody = data_block(Src, 384),
    BinKey = <<"test.com/b/path_to_file.jpg">>,
    {ok, DS2} = dcerl_api:put(DS, BinKey, BinBody),
    {ok, DS3} = dcerl_api:put(DS2, <<BinKey/binary, <<".1">>/binary >>, BinBody),
    {ok, DS4} = dcerl_api:put(DS3, <<BinKey/binary, <<".2">>/binary >>, BinBody),
    dcerl_api:stop(DS4),
    {ok, DS5} = dcerl_api:start(DataDir, JournalDir, 1024, 512),
    {ok, CS} = dcerl_api:stats(DS5),
    ?assertEqual(2, CS#dcerl_cache_stats.records),
    ok.

get_chunked(DS, FD, Chunk) ->
    case dcerl_api:get_chunk(DS, FD) of
        {ok, DS2, FD, Chunk, false} ->
            get_chunked(DS2, FD, Chunk);
        {ok, DS2, _FD, <<>>, true} ->
            {ok, DS2}
    end.

-endif.
