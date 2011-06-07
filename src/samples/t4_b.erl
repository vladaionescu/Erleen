
-module(t4_b).

-behaviour(een_comp).

-include_lib("erleen.hrl").

-export([reinit/3, handle_in/4, handle_reply/3, terminate/2]).

-record(state, {sent_ping = 0,
                n}).

reinit(_, _, [N]) ->
    {ok,
     #een_interface_spec{ext_in = [#een_port_spec{name = ping_b,
                                                  msg_type = cast,
                                                  arrity = 1}],
                         ext_out = [#een_port_spec{name = pong_b,
                                                   msg_type = cast,
                                                   arrity = 1}]},
     #state{n = N}}.

handle_in(ping_b, {pong1}, _From, State = #state{sent_ping = SentPing,
                                                 n = N}) when SentPing < N ->
    een:out(pong_b, {pong2}),
    {ok, State#state{sent_ping = SentPing + 1}}.

handle_reply(_, _, _) ->
    unexpected.

terminate(Reason, #state{sent_ping = N, n = N}) ->
    Reason;
terminate(_, State) ->
    {failed_state, State}.
