-module(dcerl).
-author('yoshiyuki.kanno@stoic.co.jp').

-export([start/3, stop/1]).
-export([put/3, put4chunked/4, remove/2, get/2, getpath/2, get4chunked/2, flush/1, delete/1]). 

%
% @doc
% Descriptor = dcerl:start(DataDir, JournalDir, MaxSize),
-spec(start(string(), string(), integer()) -> reference()).
start(_DataDir, _JournalDir, _MaxSize) ->
    true.

%
% @doc
% dcerl:put(Descriptor, Key, BinBody),
-spec(put(reference(), binary(), binary()|string()) -> boolean()).
put(_Descriptor, _Key, Body) when is_binary(Body) ->
    true.

%
% @doc
% dcerl:put4chunked(Descriptor, Key, BinChunkedBody, Tail),
-spec(put4chunked(reference(), binary(), binary(), boolean()) -> boolean()).
put4chunked(_Descriptor, _Key, _BinChunkedBody, _Tail) ->
    true.

%
% @doc
% dcerl:remove(Descriptor, Key),
-spec(remove(reference(), binary()) -> boolean()).
remove(_Descriptor, _Key) ->
    true.
%
% @doc
% RespBody = dcerl:get(Descriptor, Key),
-spec(get(reference(), binary()) -> binary()).
get(_Descriptor, _Key) ->
    <<>>.

%
% @doc
% CachedPath = dcerl:getpath(Descriptor, Key),
-spec(getpath(reference(), binary()) -> string()).
getpath(_Descriptor, _Key) ->
    "".

%
% @doc
% {ChunkedBody, Tail} = dcerl:get4chunked(Descriptor, Key),
-spec(get4chunked(reference(), binary()) -> {binary(), boolean()}).
get4chunked(_Descriptor, _Key) ->
    {<<>>, true}.

%
% @doc
% dcerl:flush(Descriptor),
-spec(flush(reference()) -> boolean()).
flush(_Descriptor) ->
    true.

%
% @doc
% dcerl:delete(Descriptor),
-spec(delete(reference()) -> boolean()).
delete(_Descriptor) ->
    true.

%
% @doc
% dcerl:stop(Descriptor),
-spec(stop(reference()) -> boolean()).
stop(_Descriptor) ->
    true.
