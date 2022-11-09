%%%-------------------------------------------------------------------
%%% @author nkescec
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Nov 2022 9:30 PM
%%%-------------------------------------------------------------------
-module(storage_sup).
-author("nkescec").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 3,
  MaxSecondsBetweenRestarts = 10,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  NumberChild = {number_serv, {storage_serv, start_link, [number_serv, char_serv]},
    Restart, Shutdown, Type, [storage_serv]},
  CharChild = {char_serv, {storage_serv, start_link, [char_serv, number_serv]},
    Restart, Shutdown, Type, [storage_serv]},
  RedirectChild = {redirect_serv, {storage_redirect_api, start_link, [{number_serv, char_serv}]},
    Restart, Shutdown, Type, [storage_redirect_api]},

  {ok, {SupFlags, [NumberChild, CharChild, RedirectChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
