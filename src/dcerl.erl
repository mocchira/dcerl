-module(dcerl).
-author('Yoshiyuki Kanno').

-export([start/0, put/2, get/2, remove/2, eldest/1, items/1, stop/1]).
-export([iterator/1, iterator_next/1, iterator_has_next/1]).
-on_load(init/0).


%% @doc Initialize
%%
-spec(init() ->
             ok).
init() ->
    SoName = case code:priv_dir(?MODULE) of
                 {error, bad_name} ->
                     case code:which(?MODULE) of
                         Filename when is_list(Filename) ->
                             filename:join([filename:dirname(Filename),"../priv", ?MODULE_STRING]);
                         _ ->
                             filename:join("../priv", ?MODULE_STRING)
                     end;
                 Dir ->
                     filename:join(Dir, ?MODULE_STRING)
             end,
    erlang:load_nif(SoName, 0).


%% @doc Launch dcerl
%%
-spec(start() ->
             {ok, any()}).
start() ->
    exit(nif_library_not_loaded).


%% @doc Insert an object into the dcerl
%%
-spec(put(any(), binary()) ->
             ok | {error, any()}).
put(_Res, _Key) ->
    exit(nif_library_not_loaded).


%% @doc Retrieve an object from the dcerl
%%
-spec(get(any(), binary()) ->
             ok | not_found | {error, any()}).
get(_Res, _Key) ->
    exit(nif_library_not_loaded).

%% @doc Remove an object from the dcerl
%%
-spec(remove(any(), binary()) ->
             ok | {error, any()}).
remove(_Res, _Key) ->
    exit(nif_library_not_loaded).


%% @doc Retrieve size of cached objects
%%
-spec(eldest(any()) ->
             {ok, binary()} | {error, any()}).
eldest(_Res) ->
    exit(nif_library_not_loaded).

-spec(iterator(any()) ->
             {ok, binary()} | {error, any()}).
iterator(_Res) ->
    exit(nif_library_not_loaded).

-spec(iterator_next(any()) ->
             {ok, binary()} | {error, any()}).
iterator_next(_Res) ->
    exit(nif_library_not_loaded).

-spec(iterator_has_next(any()) ->
        boolean()).
iterator_has_next(_Res) ->
    exit(nif_library_not_loaded).

%% @doc Retrieve total of cached objects
%%
-spec(items(any()) ->
             {ok, integer()} | {error, any()}).
items(_Res) ->
    exit(nif_library_not_loaded).


%% @doc Halt the dcerl
%%
-spec(stop(any()) ->
             ok | {error, any()}).
stop(_Res) ->
    exit(nif_library_not_loaded).

