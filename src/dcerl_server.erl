-module(dcerl_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: {ok,Pid} | ignore | {error, Error}
%% Description: Starts the server.
%%%start_link(CacheSize) ->
%%%    ?debugVal(CacheSize),
%%%    gen_server:start_link({local, ?MODULE}, ?MODULE, [CacheSize], []).


%% Function: -> ok
%% Description: Manually stops the server.
%%%stop() ->
%%%    gen_server:cast(?MODULE, stop).

%%====================================================================
%% GEN_SERVER CALLBACKS
%%====================================================================
init([]) ->
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, undefined, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


%% ----------------------------------------------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to terminate. When it returns,
%% the gen_server terminates with Reason. The return value is ignored.
%% ----------------------------------------------------------------------------------------------------------
terminate(_Reason, _State) ->
    terminated.


%% ----------------------------------------------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed.
%% ----------------------------------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

