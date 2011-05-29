
-module(t2_a).

-behaviour(een_comp).

-include_lib("erleen.hrl").

-export([reinit/3, handle_in/4, handle_reply/3, terminate/2]).

-record(state, {sent_ping = false,
                got_reply = false,
                n}).

reinit(_, _, [N]) ->
    {ok,
     #een_interface_spec{ext_in = [#een_port_spec{name = ping_a,
                                                  msg_type = call,
                                                  arrity = 0}],
                         ext_out = [#een_port_spec{name = ping_a,
                                                   type = multi,
                                                   msg_type = call,
                                                   arrity = 0}]},
     #state{n = N}}.

handle_in(ping_a, {}, From, State = #state{sent_ping = false}) ->
    {ok, MsgId} = een:out(ping_a, {}),
    {ok, State#state{sent_ping = {true, MsgId, From}}}.

handle_reply(MsgId, Replies, State = #state{sent_ping = {true, MsgId, From},
                                            got_reply = false,
                                            n = N}) ->
    N = length(Replies), %% assertion
    een:reply(From, pong_a),
    {stop, normal, State#state{got_reply = true}}.

terminate(Reason, #state{sent_ping = {true, _, _},
                         got_reply = true}) ->
    Reason;
terminate(_, State) ->
    {failed_state, State}.
