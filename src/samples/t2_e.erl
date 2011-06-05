
-module(t2_e).

-behaviour(een_comp).

-include_lib("erleen.hrl").

-export([reinit/3, handle_in/4, handle_reply/3, terminate/2]).

-record(state, {sent_ping = false,
                got_pong = false,
                n}).

reinit(_, _, [N]) ->
    {ok,
     #een_interface_spec{ext_in = [#een_port_spec{name = ping_e,
                                                  msg_type = call,
                                                  arrity = 0},
                                   #een_port_spec{name = pong_e,
                                                  type = multi,
                                                  msg_type = cast,
                                                  arrity = 0}],
                         ext_out = [#een_port_spec{name = ping_e,
                                                   type = multi,
                                                   msg_type = cast,
                                                   arrity = 0}]},
     #state{n = N}}.

handle_in(ping_e, {}, From, State = #state{sent_ping = false}) ->
    een:out(ping_e, {}),
    {ok, State#state{sent_ping = {true, From}}};
handle_in(pong_e, Params, _OtherFrom, State = #state{sent_ping = {true, From},
                                                     n = N}) ->
    N = length(Params), %% assertion
    een:reply(From, pong_e),
    {ok, State#state{got_pong = true}}.

handle_reply(_, _, _) ->
    unexpected.

terminate(Reason, #state{sent_ping = {true, _},
                         got_pong = true}) ->
    Reason;
terminate(_, State) ->
    {failed_state, State}.
