-module(pyfleet_player_pool).

%% pyfleet_player_pool: pyfleet_player_pool library's entry point.

-export([async_login/2, start/0]).


%% API

start() ->
  pyfleet_player_pool_sup:start_link().
%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec async_login(Username :: string(), SecurityData :: term()) -> {ok, PlayerSessionRef :: pid()} | {logged_in, PlayerSessionRef :: pid()} | {error, Reason :: term()}.
%%%
%%% After success logging in the logged_in message is sended to the proccess that initiates async_login call
%%% {logged_in, PlayerSessionRef} - user is already logged in
%%%
async_login(Username, SecurityData) ->
  gen_server:call(pyfleet_player_manager, {async_login, Username, SecurityData}).
%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
