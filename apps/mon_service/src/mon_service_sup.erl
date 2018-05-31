%%%-------------------------------------------------------------------
%% @doc mon_service top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mon_service_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    case supervisor:start_link({local, ?SERVER}, ?MODULE, []) of
        {ok, Pid} ->
            start_http_server(get_metrics_port()),
            agent:initialize_metrics(),
            {ok, Pid};
        Error ->
            Error
    end.


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    RestartFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10
    },
    ChildSpecList = [
        child(cowboy_sup)
    ],
    %{ok, { {one_for_all, 0, 1}, []} }.
    {ok, {RestartFlags, ChildSpecList} }.

%%====================================================================
%% Internal functions
%%====================================================================
get_metrics_port() ->
    case application:get_env(metrics_port) of
        {ok, Port} -> Port;
        undefined -> 9000
    end.

start_http_server(Port) ->
    ok = application:start(cowlib),
	ok = application:start(ranch),
	

    HostMatch = '_', % Match any host
    PathList = [
        %PathMatch,  Handler, InitialState
        {"/metrics", agent,   []}
    ],
    CurrentHost = {HostMatch, PathList},
    Dispatch = cowboy_router:compile([
       CurrentHost
    ]),
    TransportOpts = [{port, Port}],
    ProtocolOpts = #{env => #{dispatch => Dispatch}},
    cowboy:start_clear(http, TransportOpts, ProtocolOpts).
    

child(Module) ->
    #{id => Module,
      start => {Module, start_link, []},
      restart => permanent,
      shutdown => 2000
    }.
