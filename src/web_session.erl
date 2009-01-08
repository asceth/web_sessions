%%%-------------------------------------------------------------------
%%% File    : web_session.erl
%%% Author  : asceth <machinist@asceth.com>
%%% Description : Web Session
%%%
%%% Created : 28 Sep 2008 by asceth <machinist@asceth.com>
%%%-------------------------------------------------------------------
-module(web_session).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([session_id/1, clone/1, sync/2, set_timeout/2]).

%% Web Session API
-export([sid/1]).
-export([data/1, flash/1, modifiers/1]).
-export([destroy/1, regenerate/1]).
-export([data_fetch/2, data_set/3, data_merge/2, data_fun/3]).
-export([flash_add_now/3, flash_merge_now/2, flash_add/3, flash_lookup/2, flash_lookup_s/2]).


-record(session_info, {session_id,
                  data=[],
                  flash=[],
                  modifiers=[],
                  updated_at}).

-define(DEFAULT_RECORD, #session_info{session_id=web_util:uuid(256),
                                      data=[],
                                      updated_at=httpd_util:rfc1123_date()}
       ).
-define(DEFAULT_TIMEOUT, infinity).

-record(state, {session, timeout}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link(?MODULE, [], []).

start_link([Sid, Options]) ->
  gen_server:start_link(?MODULE, [Sid, Options], []);
start_link([Sid]) ->
  gen_server:start_link(?MODULE, [Sid], []).



%%====================================================================
%% API
%%====================================================================
session_id(WebSessionPid) ->
  gen_server:call(WebSessionPid, session_id).

clone(WebSessionPid) ->
  gen_server:call(WebSessionPid, clone).

sync(WebSessionPid, ModifierList) ->
  gen_server:cast(WebSessionPid, {sync, ModifierList}).

set_timeout(WebSessionPid, Timeout) ->
  gen_server:cast(WebSessionPid, {set_timeout, Timeout}).


%%====================================================================
%% Web Session API
%%====================================================================
destroy(WebSession) ->
  data_reset(WebSession).

regenerate(WebSession) ->
  Sid1 = web_util:uuid(256),
  web_sessions:rename(WebSession#session_info.session_id, Sid1),
  WebSession#session_info{session_id=Sid1, data=[], flash=[], modifiers=[{regenerate, Sid1}]}.

data_fetch(WebSession, Key) ->
  case lists:keysearch(Key, 1, WebSession#session_info.data) of
    {value, {Key, Val}} ->
      {ok, Val};
    false ->
      {error, 404}
  end.

data_set(WebSession, Key, NewValue) ->
  data_merge(WebSession, [{Key, NewValue}]).

data_merge(WebSession, TupleList) ->
  NewData = lists:ukeysort(1, TupleList ++ WebSession#session_info.data),
  NewModifiers = WebSession#session_info.modifiers ++ [{data_merge, TupleList}],
  WebSession#session_info{data=NewData, modifiers=NewModifiers}.

data_reset(WebSession) ->
  NewModifiers = WebSession#session_info.modifiers ++ [{data_reset}],
  WebSession#session_info{data=[], modifiers=NewModifiers}.

data_fun(WebSession, Fun, Args) ->
  NewData = Fun(Args, WebSession#session_info.data),
  NewModifiers = WebSession#session_info.modifiers ++ {data_fun, Fun, Args},
  WebSession#session_info{data=NewData, modifiers=NewModifiers}.

%% @doc Add a Key/Value pair to the current session's flash data.
%%      Flash data added this way is only available to this current request
%%      and can be looked up with flash_lookup/1.
%%
flash_add_now(WebSession, Key, NewValue) ->
  NewFlash = lists:ukeysort(1, [{Key, NewValue}] ++ WebSession#session_info.flash),
  WebSession#session_info{flash=NewFlash}.

%% @doc Merge a property list to the current session's flash data.
%%      Flash data added this way is only available to this current request
%%      and can be looked up with flash_lookup/1.
%%
flash_merge_now(WebSession, PropList) ->
  NewFlash = lists:ukeysort(1, PropList ++ WebSession#session_info.flash),
  WebSession#session_info{flash=NewFlash}.

%% @doc Add a key/value pair to the next session's flash data.
%%      Flash data added this way is only available on the next request!
%%
flash_add(WebSession, Key, NewValue) ->
  FlashStore = case lists:keysearch(<<"flash">>, 1, WebSession#session_info.data) of
    {value, {<<"flash">>, Value}} ->
      lists:ukeysort(1, [{Key, NewValue}] ++ Value);
    false ->
      [{Key, NewValue}]
  end,
  data_merge(WebSession, [{<<"flash">>, FlashStore}]).

%% @doc Lookup a key in the flash data that was set in the previous request
%%      or through a flash_add_now/2 call.
%%
flash_lookup(WebSession, Key) ->
  case lists:keysearch(Key, 1, WebSession#session_info.flash) of
    {value, {Key, Val}} ->
      Val;
    false ->
      {error, 404}
  end.

flash_lookup_s(WebSession, Key) ->
  case lists:keysearch(Key, 1, WebSession#session_info.flash) of
    {value, {Key, Val}} ->
      Val;
    false ->
      <<"">>
  end.

sid(WebSession) ->
  WebSession#session_info.session_id.
data(WebSession) ->
  WebSession#session_info.data.
flash(WebSession) ->
  WebSession#session_info.flash.
modifiers(WebSession) ->
  WebSession#session_info.modifiers.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state{session=?DEFAULT_RECORD, timeout=?DEFAULT_TIMEOUT}, ?DEFAULT_TIMEOUT};

%% TODO
%% Should look for persisted session first
init([Sid, Options]) ->
  Timeout = proplists:get_value(timeout, Options, ?DEFAULT_TIMEOUT),
  Session = ?DEFAULT_RECORD,
  State = #state{session=Session#session_info{session_id=Sid}, timeout=Timeout},
  {ok, State, Timeout};
init([Sid]) ->
  Session = ?DEFAULT_RECORD,
  State = #state{session=Session#session_info{session_id=Sid}, timeout=?DEFAULT_TIMEOUT},
  {ok, State, State#state.timeout}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(session_id, _From, #state{session=Session} = State) ->
  Sid = case Session#session_info.session_id of
          undefined ->
            "";
          Sid1 ->
            Sid1
        end,
  {reply, Sid, State, State#state.timeout};

handle_call(clone, _From, #state{session=Session} = State) ->
  {reply, Session, State, State#state.timeout};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State, State#state.timeout}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({set_timeout, Timeout}, State) ->
  {noreply, State#state{timeout=Timeout}, Timeout};
handle_cast({sync, ModifierList}, #state{session=Session} = State) ->
  Session1 = lists:foldl(fun do_modifier/2, Session, ModifierList),
  {noreply, State#state{session=Session1}, State#state.timeout};
handle_cast(_Msg, State) ->
  {noreply, State, State#state.timeout}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
  {stop, timeout, State};
handle_info(_Info, State) ->
  {noreply, State, State#state.timeout}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{session=Session} = _State) ->
  SessionId = Session#session_info.session_id,
  web_sessions:unregister_session(SessionId),
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

master_data_merge(TupleList, Session) ->
  NewData = lists:ukeysort(1, TupleList ++ Session#session_info.data),
  Session#session_info{data=NewData}.
master_data_reset(Session) ->
  Session#session_info{data=[]}.

do_modifier({data_reset}, Session) ->
  master_data_reset(Session);
do_modifier({data_merge, TupleList}, Session) ->
  master_data_merge(TupleList, Session);
do_modifier({regenerate, Sid}, Session) ->
  Session#session_info{session_id=Sid};
do_modifier({data_fun, Fun, Args}, Session) ->
  NewData = Fun(Args, Session#session_info.data),
  Session#session_info{data=NewData};
do_modifier(_Other, Session) ->
  Session.
