%%%-------------------------------------------------------------------
%%% @author nkescec
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Nov 2022 8:32 PM
%%%-------------------------------------------------------------------
-module(storage_serv).
-author("nkescec").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: {[], [], atom()}} | {ok, State :: {[], [], atom()}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(BackupId) ->
  try gen_server:call(BackupId, {get}) of
    {Backup, Main} -> {ok, {Main, Backup, BackupId}}
  catch
    exit:_ -> {ok, {[], [], BackupId}}
  end.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: {[], [], atom()}) ->
  {reply, Reply :: term(), NewState :: {[], [], atom()}} |
  {reply, Reply :: term(), NewState :: {[], [], atom()}, timeout() | hibernate} |
  {noreply, NewState :: {[], [], atom()}} |
  {noreply, NewState :: {[], [], atom()}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: {[], [], atom()}} |
  {stop, Reason :: term(), NewState :: {[], [], atom()}}).
handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};
handle_call({get}, _From, {Main, Backup, BackupId}) ->
  {reply, {Main, Backup}, {Main, Backup, BackupId}}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: {[], [], atom()}) ->
  {noreply, NewState :: {[], [], atom()}} |
  {noreply, NewState :: {[], [], atom()}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: {[], [], atom()}}).
handle_cast({add_backup, Element}, {Main, Backup, BackupId}) ->
  {noreply, {Main, [Element] ++ Backup, BackupId}};
handle_cast({add, Element}, {Main, Backup, BackupId}) ->
  gen_server:cast(BackupId, {add_backup, Element}),
  {noreply, {[Element] ++ Main, Backup, BackupId}}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: {[], [], atom()}) ->
  {noreply, NewState :: {[], [], atom()}} |
  {noreply, NewState :: {[], [], atom()}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: {[], [], atom()}}).
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: {[], [], atom()}) -> term()).
terminate(_Reason, _State) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: {[], [], atom()},
    Extra :: term()) ->
  {ok, NewState :: {[], [], atom()}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(ServerId :: term(), BackupId :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ServerId, BackupId) ->
  gen_server:start_link({local, ServerId}, ?MODULE, BackupId, []).
