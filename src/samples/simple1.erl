
-module(simple1).

-behaviour(een_coord).

-export([start/0]).
-export([reinit/3, ext_in_if/1, ext_out_if/1, int_in_if/1, int_out_if/1,
         handle_in/4, handle_reply/3]).

%% ----------------------------------------------------------------------------

start() ->
    een_coord:start(?MODULE, []).

%% ----------------------------------------------------------------------------

reinit(_, _, []) ->
    none.

ext_in_if(_) ->
    [{do_test, {call, 0}}].

ext_out_if(_) ->
    [{rpc_out, {call, 2}}].

int_in_if(_) ->
    [].

int_out_if(_) ->
    [].

handle_in(do_test, {call, {}}, From, none) ->
    MsgId = een_coord:out(rpc_out, {call, {param1, param2}}),
    {ok, {From, MsgId}}.

handle_reply(MsgId, {param3, param4, param5}, {From, MsgId}) ->
    een_coord:reply(From, ok),
    {ok, none}.

terminate(Reason, _State) ->
    Reason.
