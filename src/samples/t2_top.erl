
-module(t2_top).

-behaviour(een_comp).

-include_lib("erleen.hrl").

-export([reinit/3, handle_in/4, handle_reply/3, terminate/2]).

-record(state, {sent_ping = false,
                got_pong = false,
                got_reply = false,
                phase = 0,
                caller,
                n}).

reinit(_, _, [Caller, N]) ->
    {ok,
     #een_interface_spec{ext_in = [#een_port_spec{name = ping_in,
                                                  msg_type = cast,
                                                  arrity = 0}],
                         int_out = [#een_port_spec{name = ping_top,
                                                   type = multi,
                                                   msg_type = call,
                                                   arrity = 0}],
                         int_in = [#een_port_spec{name = pong_top,
                                                  type = multi,
                                                  msg_type = cast,
                                                  arrity = 0}]},
     #state{caller = Caller, n = N}}.

handle_in(ping_in, {}, _From, State = #state{sent_ping = false}) ->
    {ok, MsgId} = een:out(ping_top, {}),
    {ok, State#state{sent_ping = {true, MsgId},
                     phase = 1}};
handle_in(pong_top, Params, _From, State = #state{sent_ping = {true, _},
                                                  got_pong = false,
                                                  phase = Phase,
                                                  n = N}) ->
    N = length(Params), %% assertion
    maybe_done(State#state{got_pong = true, phase = Phase + 1}).

handle_reply(MsgId, Replies, State = #state{sent_ping = {true, MsgId},
                                            got_reply = false,
                                            phase = Phase,
                                            n = N}) ->
    N = length(Replies), %% assertion
    maybe_done(State#state{got_reply = true, phase = Phase + 1}).

terminate(Reason, #state{phase = 3,
                         sent_ping = {true, _},
                         got_pong = true,
                         got_reply = true}) ->
    Reason;
terminate(_, State) ->
    {failed_state, State}.

maybe_done(#state{phase = 3,
                  caller = Caller} = State) ->
    Caller ! pong_out,
    {stop, normal, State};
maybe_done(State) ->
    {ok, State}.
