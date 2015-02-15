%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. фев 2015 14:54
%%%-------------------------------------------------------------------
-module(pyfleet_player_pool_worker_sup).
-author("davidqo").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  io:format("Pyfleet player pool worker supervisor. Start link~n", []),
    supervisor:start_link(?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  io:format("Pyfleet player pool worker supervisor. Init~n", []),
    SupFlags = {simple_one_for_one, 1000, 3600},
    Child = {'pyfleet_player', {'pyfleet_player', start_link, []},
        transient, 2000, worker, ['pyfleet_player']},
    {ok, {SupFlags, [Child]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
