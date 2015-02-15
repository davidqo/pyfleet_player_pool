%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. фев 2015 12:54
%%%-------------------------------------------------------------------
-module(pyfleet_player_manager).
-author("davidqo").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    sup_ref,
    user_sup_ref,
    user_count = 0,
    max_user_count = 1000,
    logged_in_users = []
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link([SupRef :: pid()]) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(SupRef) ->
    io:format("Pyfleet player manager. Start link. Sup ref: ~p~n", [SupRef]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [SupRef], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([SupRef]) ->
  io:format("Pyfleet player manager init~n", []),
  self() ! init,
  {ok, #state{sup_ref = SupRef}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
        State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_call({async_login, User, SecurityData}, {ApiRef, _} = _From, State = #state{user_sup_ref = UserSupRef, user_count = UserCount, max_user_count = MaxUserCount, logged_in_users = LoggedInUsers}) when UserCount < MaxUserCount ->
  case is_logged_in(User, LoggedInUsers) of
    {true, {User, PlayerSessionRef, _ApiRef}} ->
      io:format("Logged in~p~n", [User]),
      {reply, {logged_in, PlayerSessionRef}, State};
    false ->
      io:format("Async login user ~p~n", [User]),
      case pyfleet_player:async_login(UserSupRef, self(), ApiRef, User, SecurityData) of
        {ok, PlayerSessionRef} ->
          PlayerSessionMonitorRef = erlang:monitor(process, PlayerSessionRef),
          {reply, {ok, PlayerSessionRef}, State#state{user_count = UserCount + 1, logged_in_users = [{User, PlayerSessionMonitorRef, ApiRef} | LoggedInUsers]}};
        {error, Reason} ->
          {reply, {error, Reason}, State}
      end
  end;
handle_call({async_login, User, _}, _From, State) ->
  io:format("Pyfleet player manager. User ~p try to login but user limit exeeded~n",  [User]),
  {reply, {error, user_limit_exeeded}, State};
handle_call(Request, _From, State) ->
  io:format("Unexpected call: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_cast({logged_in, Username, UserRef, ApiRef}, State) ->
  io:format("Notification about player session is started. Api ref: ~p~n", [ApiRef]),
  ApiRef ! {logged_in, Username, UserRef},
  {noreply, State};
handle_cast({logged_out, Username}, State = #state{logged_in_users = LoggedInUsers}) ->
  {noreply, State#state{logged_in_users = lists:keydelete(Username, 1, LoggedInUsers)}};
handle_cast(Msg, State) ->
  io:format("Pyfleet player manager. Unexpected cast: ~p~n", [Msg]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_info({'DOWN', MonitorRef, _, _, _}, State = #state{logged_in_users = LoggedInUsers, user_count = UserCount}) ->
  io:format("Pyfleet player manager. Player session stopped..~n", []),
  case lists:keytake(MonitorRef, 2, LoggedInUsers) of
    {value, {User, _, _}, RestUsers} ->
      io:format("Pyfleet player manager. User ~p session is down~n", [User]),
      {noreply, State#state{logged_in_users = RestUsers, user_count = UserCount - 1}};
    false ->
      io:format("Pyfleet player manager. Error: Unknown user session is down~n", []),
      {noreply, State}
    end;
handle_info(init, State = #state{sup_ref = SupRef}) ->
  io:format("Pyfleet player manager. Initialization..~n", []),
  {ok, UserSupRef} = create_player_pool_worker_supervisor(SupRef),
  io:format("Pyfleet player manager. Initialization.. done!~n", []),
  {noreply, State#state{user_sup_ref = UserSupRef}};
handle_info(Info, State) ->
  io:format("Pyfleet player manger. Unexpected info: ~p~n", [Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
        State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
        Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec is_logged_in(User :: string(), LoggedInUsers :: [{User :: string(), Ref :: pid()}, ... ]) -> {User :: string(), Ref :: pid()}.
is_logged_in(User, LoggedInUsers) ->
    case lists:keysearch(User, 1, LoggedInUsers) of
        {value, {_User, _Ref, _ApiRef} = LoggedIn} ->
            {true, LoggedIn};
        false ->
            false
    end.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++

create_player_pool_worker_supervisor(SupRef) ->
  ChildSpec =
    {pyfleet_player_pool_worker_supervisor,
    {pyfleet_player_pool_worker_sup, start_link, []},
    temporary,
    10000,
    supervisor,
    [pyfleet_player_pool_worker_sup]},
  supervisor:start_child(SupRef, ChildSpec).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++
