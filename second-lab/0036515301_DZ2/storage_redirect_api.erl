%%%-------------------------------------------------------------------
%%% @author nkescec
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Nov 2022 8:32 PM
%%%-------------------------------------------------------------------
-module(storage_redirect_api).
-author("nkescec").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%% Sequential API export

-export([stop/0, add/1, get/1, stop_char/0, stop_number/0]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: {}} | {ok, State :: {}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(Servers) ->
  {ok, Servers}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: {}) ->
  {reply, Reply :: term(), NewState :: {}} |
  {reply, Reply :: term(), NewState :: {}, timeout() | hibernate} |
  {noreply, NewState :: {}} |
  {noreply, NewState :: {}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: {}} |
  {stop, Reason :: term(), NewState :: {}}).
handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};
handle_call({get, Target}, _From, State) ->
  Reply = gen_server:call(Target, {get}),
  {reply, Reply, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: {}) ->
  {noreply, NewState :: {}} |
  {noreply, NewState :: {}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: {}}).
handle_cast({stop, number}, {NumberId, CharId}) ->
  gen_server:call(NumberId, stop),
  {noreply, {NumberId, CharId}};
handle_cast({stop, char}, {NumberId, CharId}) ->
  gen_server:call(CharId, stop),
  {noreply, {NumberId, CharId}};
handle_cast({add, Element}, {NumberId, CharId}) when is_integer(Element)->
  gen_server:cast(NumberId, {add, Element}),
  {noreply, {NumberId, CharId}};
handle_cast({add, Element}, {NumberId, CharId}) when is_atom(Element) ->
  gen_server:cast(CharId, {add, Element}),
  {noreply, {NumberId, CharId}}.


%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: {}) ->
  {noreply, NewState :: {}} |
  {noreply, NewState :: {}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: {}}).
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: {}) -> term()).
terminate(_Reason, _State) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: {},
    Extra :: term()) ->
  {ok, NewState :: {}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Sequential API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Servers :: tuple()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Servers) ->
  gen_server:start_link({local, redirect}, ?MODULE, Servers, []).

stop() ->
  gen_server:call(redirect, stop).

stop_number() ->
  gen_server:cast(redirect, {stop, number}).

stop_char() ->
  gen_server:cast(redirect, {stop, char}).

add(Element) ->
  gen_server:cast(redirect, {add, Element}).

get(Target) ->
  gen_server:call(redirect, {get, Target}).
