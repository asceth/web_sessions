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
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% External API
-export([next/1, rename/2, anonymous/1, register_web_router/1]).

%% External Hooks
-export([pre_request/3, post_request/7]).

%% Logging
-include("logger.hrl").

-record(state, {routers=[],
                next=[],
                done=[],
                last=undefined,
                table}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link([]) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []);
start_link(WebRouter) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [WebRouter], []).

%%====================================================================
%% External API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: next(WebSessionsName, Module:atom()) -> Node
%% Description: Gets the next node the session should be started at
%%--------------------------------------------------------------------
next(Mod) ->
  gen_server:call(?SERVER, {next, Mod}).

register_web_router(WebRouter) ->
  gen_server:call(?SERVER, {register_web_router, WebRouter}).

%% Function: anonymous() -> SessionId
%% Description: Creates a session and returns a session clone - useful for testing
anonymous(SessionId) ->
  gen_server:call(?SERVER, {anonymous, SessionId}).

rename(OriginalSid, NewSid) ->
  gen_server:cast(?SERVER, {rename, OriginalSid, NewSid}).

pre_request(Method, Path, Req) ->
  gen_server:call(?SERVER, {pre_request, Method, Path, Req}).

post_request(Method, Path, Req, Session, Status, Headers, Body) ->
  gen_server:call(?SERVER, {post_request, Method, Path, Req, Session, Status, Headers, Body}).


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
  Table = ets:new(web_sessions, [set, public]),
  {ok, #state{routers=[], next=[local], done=[], table=Table}};
init([WebRouter]) ->
  Table = ets:new(web_sessions, [set, public]),
  register_web_router_hooks(WebRouter),
  {ok, #state{routers=[WebRouter], next=[local], done=[], table=Table}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |case Req:get_cookie_value("_session_id") of
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({pre_request, Method, Path, Req}, From, #state{table=Table} = State) ->
  spawn(fun() -> do_pre_request(From, Method, Path, Req, Table) end),
  {noreply, State};
handle_call({post_request, Method, Path, Req, Session, Status, Headers, Body}, From,
            #state{table=Table} = State) ->
  spawn(fun() -> do_post_request(From, Method, Path, Req, Session, Status,
                                 Headers, Body, Table) end),
  {noreply, State};
handle_call({anonymous, SessionId}, From, #state{table=Table} = State) ->
  spawn(fun() -> do_anonymous(From, SessionId, Table) end),
  {noreply, State};
handle_call({register_web_router, WebRouter, Domain}, _From, #state{routers=Routers} = State) ->
  State1 = State#state{routers=[{WebRouter, Domain}|Routers]},
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

do_anonymous(From, SessionId, Table) ->
  Session = case SessionId of
              undefined ->
                {ok, WebSession} = web_session:start_link(),
                unlink(WebSession),
                register_session(Table, web_session:session_id(WebSession), WebSession),
                web_session:clone(WebSession);
              SessionId ->
                case ets:lookup(Table, SessionId) of
                  [{_Sid, Pid}|_] ->
                    web_session:clone(Pid);
                  [] ->
                    {ok, WebSession} = web_session:start_link([SessionId]),
                    unlink(WebSession),
                    register_session(Table, SessionId, WebSession),
                    web_session:clone(WebSession)
                end
            end,
  gen_server:reply(From, Session).

do_pre_request(From, _Method, _Path, Req, Table) ->
  Session = case Req:get_cookie_value("_session_id") of
              undefined ->
                {ok, WebSession} = web_session:start_link(),
                unlink(WebSession),
                register_session(Table, web_session:session_id(WebSession), WebSession),
                web_session:clone(WebSession);
              SessionId ->
                UnquotedSessionId = mochiweb_util:unquote(SessionId),
                case ets:lookup(Table, UnquotedSessionId) of
                  [{_Sid, Pid}|_] ->
                    web_session:clone(Pid);
                  [] ->
                    {ok, WebSession} = web_session:start_link(),
                    unlink(WebSession),
                    register_session(Table, web_session:session_id(WebSession), WebSession),
                    web_session:clone(WebSession)
                end
            end,
  gen_server:reply(From, {request, Req, session, Session}).

do_post_request(From, _Method, _Path, _Req, Session, Status, Headers, Body, Table) ->
  Sid = Session:session_id(),
  case ets:lookup(Table, Sid) of
    [{_Sid, Pid}|_] ->
      ?DEBUG("Syncing Session (~p) with ~p~n~n", [Sid, Session:modifiers()]),
      web_session:sync(Pid, Session:modifiers());
    [] ->
      ?DEBUG("Could not find Session (~p)~n~n", [Sid]),
      ok
  end,
  QuotedSessionId = mochiweb_util:quote_plus(Sid),
  Cookie = mochiweb_cookies:cookie("_session_id", QuotedSessionId, [{domain, ".amaterasu.com"}, {path, "/"}]),
  Headers1 = Headers ++ [Cookie],
  gen_server:reply(From, {status, Status, headers, Headers1, body, Body}).

register_session(Table, SessionId, WebSession) ->
  ets:insert(Table, {SessionId, WebSession}).

register_web_router_hooks(WebRouter) ->
  web_router:add(WebRouter, pre_request, global, web_sessions, pre_request, 1),
  web_router:add(WebRouter, post_request, global, web_sessions, post_request, 1).
