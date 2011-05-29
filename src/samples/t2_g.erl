
-module(t2_g).

-behaviour(een_comp).

-include_lib("erleen.hrl").

-export([reinit/3, handle_in/4, handle_reply/3, terminate/2]).

-record(state, {sent_pong = false}).

reinit(_, _, []) ->
    {ok,
     #een_interface_spec{ext_in = [#een_port_spec{name = ping_g,
                                                  type = multi,
                                                  msg_type = call,
                                                  arrity = 0}],
                         ext_out = [#een_port_spec{name = pong_g,
                                                   msg_type = cast,
                                                   arrity = 0}]},
     #state{}}.

handle_in(ping_g, Params, From, State = #state{sent_pong = false}) ->
    3 = length(Params), %% assertion
    een:reply(From, pong_g),
    een:out(pong_g, {}),
    {stop, normal, State#state{sent_pong = true}}.

handle_reply(_, _, _) ->
    unexpected.

terminate(Reason, #state{sent_pong = true}) ->
    Reason;
terminate(_, State) ->
    {failed_state, State}.
