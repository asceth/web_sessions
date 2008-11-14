-module(web_session_clone, [Sid, Data, Flash, Modifiers]).
-author('machinist@asceth.com').

-export([session_id/0]).
-export([data/0, flash/0, modifiers/0]).


-define(DEFAULT_RECORD, [
                         {<<"session_id">>, web_util:uuid(256)},
                         {<<"data">>, []},
                         {<<"updated_at">>, httpd_util:rfc1123_date()}
                        ]).

-export([init/1]).
-export([destroy/0, regenerate/0]).
-export([data_fetch/1, data_set/2, data_merge/1, data_fun/2]).
-export([flash_add_now/2, flash_add/2, flash_lookup/1]).

init([]) ->
  ?MODULE:new(web_util:uuid(256), [], [], []);

init([Sid1, Data1, Flash1, Modifiers1]) ->
  ?MODULE:new(Sid1, Data1, Flash1, Modifiers1).


destroy() ->
  data_reset().

regenerate() ->
  Sid1 = web_util:uuid(256),
  web_sessions:rename(Sid, Sid1),
  init([Sid1, [], [], [{regenerate, Sid1}]]).

data_fetch(Key) ->
  case lists:keysearch(Key, 1, Data) of
    {value, {Key, Val}} ->
      {ok, Val};
    false ->
      {error, 404}
  end.

data_set(Key, NewValue) ->
  data_merge([{Key, NewValue}]).

data_merge(TupleList) ->
  NewData = lists:ukeysort(1, TupleList ++ Data),
  NewModifiers = Modifiers ++ [{data_merge, TupleList}],
  ?MODULE:new(Sid, NewData, Flash, NewModifiers).

data_reset() ->
  NewModifiers = Modifiers ++ [{data_reset}],
  ?MODULE:new(Sid, [], Flash, NewModifiers).

data_fun(Fun, Args) ->
  NewData = Fun(Args, Data),
  NewModifiers = Modifiers ++ {data_fun, Fun, Args},
  ?MODULE:new(Sid, NewData, Flash, NewModifiers).

%% @doc Add a Key/Value pair to the current session's flash data.
%%      Flash data added this way is only available to this current request
%%      and can be looked up with flash_lookup/1.
%%
flash_add_now(Key, NewValue) ->
  NewFlash = lists:ukeysort(1, [{Key, NewValue}] ++ Flash),
  ?MODULE:new(Sid, Data, NewFlash, Modifiers).

%% @doc Add a key/value pair to the next session's flash data.
%%      Flash data added this way is only available on the next request!
%%
flash_add(Key, NewValue) ->
  FlashStore = case lists:keysearch(<<"flash">>, 1, Data) of
    {value, {<<"flash">>, Value}} ->
      lists:ukeysort(1, [{Key, NewValue}] ++ Value);
    false ->
      [{Key, NewValue}]
  end,
  data_merge([{<<"flash">>, FlashStore}]).

%% @doc Lookup a key in the flash data that was set in the previous request
%%      or through a flash_add_now/2 call.
%%
flash_lookup(Key) ->
  case lists:keysearch(Key, 1, Flash) of
    {value, {Key, Val}} ->
      Val;
    false ->
      {error, 404}
  end.


session_id() ->
  Sid.
data() ->
  Data.
flash() ->
  Flash.
modifiers() ->
  Modifiers.
