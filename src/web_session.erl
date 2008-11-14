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

-export([session_id/1, clone/1, sync/2]).

-define(DEFAULT_RECORD, [
                         {<<"session_id">>, web_util:uuid(256)},
                         {<<"data">>, []},
                         {<<"updated_at">>, httpd_util:rfc1123_date()}
                        ]).

-record(state, {session}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link(?MODULE, [], []).

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
  {ok, #state{session=?DEFAULT_RECORD}};

%% TODO
%% Should look for persisted session first
init([Sid]) ->
  State = #state{session=record_merge([{<<"session_id">>, Sid}], ?DEFAULT_RECORD)},
  {ok, State}.

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
  Sid = case lists:keysearch(<<"session_id">>, 1, Session) of
          {value, {<<"session_id">>, Sid1}} ->
            Sid1;
          false ->
            ""
        end,
  {reply, Sid, State};

handle_call(clone, _From, #state{session=Session} = State) ->
  Sid = case lists:keysearch(<<"session_id">>, 1, Session) of
          {value, {<<"session_id">>, Sid1}} ->
            Sid1;
          false ->
            web_util:uuid(256)
        end,
  Data = case lists:keysearch(<<"data">>, 1, Session) of
           {value, {<<"data">>, Data1}} ->
             Data1;
           false ->
             []
         end,
  Flash = case lists:keysearch(<<"flash">>, 1, Data) of
           {value, {<<"flash">>, Flash1}} ->
             Flash1;
           false ->
             []
         end,
  Session1 = record_merge([{<<"session_id">>, Sid}, {<<"data">>, Data}], Session),
  Clone = web_session_clone:new([], [], [], []),
  Reply = Clone:init([Sid, Data, Flash, []]),
  {reply, Reply, State#state{session=Session1}};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({sync, ModifierList}, #state{session=Session} = State) ->
  Session1 = lists:foldl(fun do_modifier/2, Session, ModifierList),
  {noreply, State#state{session=Session1}};
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

record_merge(TupleList, Session) ->
  lists:ukeysort(1, TupleList ++ Session).

data_merge(TupleList, Session) ->
  NewData = case lists:keysearch(<<"data">>, 1, Session) of
    {value, {<<"data">>, Value}} ->
      lists:ukeysort(1, TupleList ++ Value);
    false ->
      TupleList
  end,
  record_merge([{<<"data">>, NewData}], Session).

data_reset(Session) ->
  record_merge([{<<"data">>, []}], Session).

do_modifier({data_reset}, Session) ->
  data_reset(Session);
do_modifier({data_merge, TupleList}, Session) ->
  data_merge(TupleList, Session);
do_modifier({regenerate, Sid}, Session) ->
  record_merge([{<<"session_id">>, Sid}], Session);
do_modifier({data_fun, Fun, Args}, Session) ->
  Data = case lists:keysearch(<<"data">>, 1, Session) of
           {value, {<<"data">>, Value}} ->
             Value;
           false ->
             []
         end,
  NewData = Fun(Args, Data),
  record_merge(NewData, Session);
do_modifier(_Other, Session) ->
  Session.
