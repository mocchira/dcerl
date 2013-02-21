-module(dcerl_api).
-author('yoshiyuki.kanno@stoic.co.jp').

-include("dcerl.hrl").

-export([start/3, stop/1]).
-export([put/3, put_begin/2, put_chunk/3, put_end/3, remove/2, get/2, get_chunk/3, flush/1, delete/1]). 


%
% @doc
-spec(start(string(), string(), integer()) -> #dcerl_state{}|{error, any()}).
start(_DataDir, _JournalDir, _MaxSize) -> #dcerl_state{}.

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
-spec(get(#dcerl_state{}, Key::binary()) -> binary()|#dcerl_fd{}|{error, any()}).
get(_State, _Key) ->
    <<>>.

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

