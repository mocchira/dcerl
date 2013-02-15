-module(dcerl_sup).

-behaviour(supervisor).

%% External API
-export([start_link/0, stop/0]).

%% Callbacks
-export([init/1]).

%%-----------------------------------------------------------------------
%% External API
%%-----------------------------------------------------------------------
%% @spec (Params) -> ok
%% @doc start link.
%% @end
start_link() ->
%%%    TotalCacheSize = case application:get_env(dcerl, total_cache_size) of
%%%                         {ok, Value1} when is_integer(Value1) ->
%%%                             Value1;
%%%                         _ ->
%%%                             ?DEF_TOTA_CACHE_SIZE
%%%                     end,
%%%    supervisor:start_link({local, ?MODULE}, ?MODULE, [TotalCacheSize]).
    void.

%% @spec () -> ok |
%%             not_started
%% @doc stop process.
%% @end
stop() ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) == true ->
            exit(Pid, shutdown),
            ok;
        _ -> not_started
    end.


%% ---------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------
%% @spec (Params) -> ok
%% @doc stop process.
%% @end
%% @private
init([TotalCacheSize]) ->
    {ok, {{one_for_one, 3000, 3000},
          [{dcerl_server, {dcerl_server, start_link, [TotalCacheSize]},
            permanent, 3000, worker, [dcerl_server]}]}}.

