%%%-------------------------------------------------------------------
%%% File    : web_sessions.erl
%%% Author  : asceth <machinist@asceth.com>
%%% Description : Session Manager
%%%
%%% Created : 27 Sep 2008 by asceth <machinist@asceth.com>
%%%-------------------------------------------------------------------
-module(web_sessions).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% External API
-export([rename/2, anonymous/1, register_web_router/1]).
-export([register_session/2, unregister_session/1]).

%% External Hooks
-export([pre_request/1, post_request/1]).

%% Logging
-include("logger.hrl").

-record(state, {routers=[],
                session_timeout,
                table,
                domain}).

-define(SERVER, ?MODULE).
-define(DEFAULT_SESSION_TIMEOUT, infinity).
-define(DEFAULT_DOMAIN, ".").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link([]) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [undefined, ?DEFAULT_DOMAIN, ?DEFAULT_SESSION_TIMEOUT], []);
start_link(WebRouter) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [WebRouter, ?DEFAULT_DOMAIN, ?DEFAULT_SESSION_TIMEOUT], []).
start_link(WebRouter, Domain) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [WebRouter, Domain, ?DEFAULT_SESSION_TIMEOUT], []).
start_link(WebRouter, Domain, Timeout) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [WebRouter, Domain, Timeout], []).


%%====================================================================
%% External API
%%====================================================================
%%--------------------------------------------------------------------
register_session(SessionId, WebSession) ->
  gen_server:cast(?SERVER, {register_session, SessionId, WebSession}).
unregister_session(SessionId) ->
  gen_server:cast(?SERVER, {unregister_session, SessionId}).
register_web_router(WebRouter) ->
  gen_server:call(?SERVER, {register_web_router, WebRouter}).

%% Function: anonymous() -> SessionId
%% Description: Creates a session and returns a session clone - useful for testing
anonymous(SessionId) ->
  gen_server:call(?SERVER, {anonymous, SessionId}).

rename(OriginalSid, NewSid) ->
  gen_server:cast(?SERVER, {rename, OriginalSid, NewSid}).

pre_request(Env) ->
  gen_server:call(?SERVER, {pre_request, Env}).

post_request(Env) ->
  gen_server:call(?SERVER, {post_request, Env}).


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
init([undefined, Domain, Timeout]) ->
  Table = ets:new(web_sessions, [set, public]),
  {ok, #state{routers=[], table=Table, domain=Domain, session_timeout=Timeout}};
init([WebRouter, Domain, Timeout]) ->
  Table = ets:new(web_sessions, [set, public]),
  register_web_router_hooks(WebRouter),
  {ok, #state{routers=[WebRouter], table=Table, domain=Domain, session_timeout=Timeout}}.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |case Req:get_cookie_value("_session_id") of
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({pre_request, Env}, From, #state{table=Table, session_timeout=Timeout} = State) ->
  spawn(fun() -> do_pre_request(From, Env, Table, Timeout) end),
  {noreply, State};

handle_call({post_request, Env}, From,
            #state{table=Table, domain=Domain} = State) ->
  spawn(fun() -> do_post_request(From, Env, Table, Domain) end),
  {noreply, State};
handle_call({anonymous, SessionId}, From, #state{table=Table, session_timeout=Timeout} = State) ->
  spawn(fun() -> do_anonymous(From, SessionId, Table, Timeout) end),
  {noreply, State};
handle_call({register_web_router, WebRouter}, _From, #state{routers=Routers} = State) ->
  State1 = State#state{routers=[WebRouter|Routers]},
  register_web_router_hooks(WebRouter),
  {reply, ok, State1};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({register_session, SessionId, WebSession}, #state{table=Table} = State) ->
  spawn(fun() -> internal_register_session(Table, SessionId, WebSession) end),
  {noreply, State};
handle_cast({unregister_session, SessionId}, #state{table=Table} = State) ->
  spawn(fun() -> internal_unregister_session(Table, SessionId) end),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
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

do_anonymous(From, SessionId, Table, Timeout) ->
  Session = case SessionId of
              undefined ->
                {ok, WebSession} = web_session:start_link(),
                web_session:set_timeout(WebSession, Timeout),
                unlink(WebSession),
                internal_register_session(Table, web_session:session_id(WebSession), WebSession),
                web_session:clone(WebSession);
              SessionId ->
                case ets:lookup(Table, SessionId) of
                  [{_Sid, Pid}|_] ->
                    web_session:clone(Pid);
                  [] ->
                    {ok, WebSession} = web_session:start_link([SessionId, [{timeout, Timeout}]]),
                    unlink(WebSession),
                    internal_register_session(Table, SessionId, WebSession),
                    web_session:clone(WebSession)
                end
            end,
  gen_server:reply(From, Session).

do_pre_request(From, Env, Table, Timeout) ->
  Req = proplists:get_value("request", Env),
  Session = case Req:get_cookie_value("_session_id") of
              undefined ->
                {ok, WebSession} = web_session:start_link(),
                web_session:set_timeout(WebSession, Timeout),
                unlink(WebSession),
                internal_register_session(Table, web_session:session_id(WebSession), WebSession),
                web_session:clone(WebSession);
              SessionId ->
                UnquotedSessionId = mochiweb_util:unquote(SessionId),
                case ets:lookup(Table, UnquotedSessionId) of
                  [{_Sid, Pid}|_] ->
                    web_session:clone(Pid);
                  [] ->
                    {ok, WebSession} = web_session:start_link(),
                    web_session:set_timeout(WebSession, Timeout),
                    unlink(WebSession),
                    internal_register_session(Table, web_session:session_id(WebSession), WebSession),
                    web_session:clone(WebSession)
                end
            end,
  SessionFinal = web_session:flash_merge_now(Session, [{"request", Req},
                                                       {"method", proplists:get_value("method", Env)},
                                                       {"path_tokens", proplists:get_value("path_tokens", Env)}]),
  gen_server:reply(From, SessionFinal).

do_post_request(From, Session, Table, Domain) ->
  Sid = web_session:sid(Session),
  case ets:lookup(Table, Sid) of
    [{_Sid, Pid}|_] ->
      ?DEBUG("Syncing Session (~p) with ~p~n~n", [Sid, web_session:modifiers(Session)]),
      web_session:sync(Pid, web_session:modifiers(Session));
    [] ->
      ?DEBUG("Could not find Session (~p)~n~n", [Sid]),
      ok
  end,
  QuotedSessionId = mochiweb_util:quote_plus(Sid),
  Cookie = mochiweb_cookies:cookie("_session_id", QuotedSessionId, [{domain, Domain}, {path, "/"}]),
  Session1 = web_session:flash_add_now(Session, "headers", web_session:flash_lookup(Session, "headers") ++ [Cookie]),
  gen_server:reply(From, Session1).

internal_register_session(Table, SessionId, WebSession) ->
  ets:insert(Table, {SessionId, WebSession}).
internal_unregister_session(Table, SessionId) ->
  ets:delete(Table, SessionId).

register_web_router_hooks(WebRouter) ->
  web_router:add(WebRouter, pre_request, global, web_sessions, pre_request, 1),
  web_router:add(WebRouter, post_request, global, web_sessions, post_request, 1).

