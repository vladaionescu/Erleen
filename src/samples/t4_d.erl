
-module(t4_d).

-behaviour(een_comp).

-include_lib("erleen.hrl").

-export([reinit/3, handle_in/4, handle_reply/3, terminate/2]).

-record(state, {sent_ping = 0}).

reinit(_, _, []) ->
    {ok,
     #een_interface_spec{ext_in = [#een_port_spec{name = ping_d,
                                                  msg_type = call,
                                                  arrity = 0}],
                         ext_out = [#een_port_spec{name = pong_d,
                                                   msg_type = cast,
                                                   arrity = 0}]},
     #state{}}.

handle_in(ping_d, {}, From, State = #state{sent_ping = SentPing}) when SentPing < 2 ->
    een:out(pong_d, {}),
    een:reply(From, pong),
    {ok, State#state{sent_ping = SentPing + 1}}.

handle_reply(_, _, _) ->
    unexpected.

terminate(Reason, #state{sent_ping = 2}) ->
    Reason;
terminate(_, State) ->
    {failed_state, State}.
