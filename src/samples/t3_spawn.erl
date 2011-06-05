
-module(t3_spawn).

-behaviour(een_comp).

-include_lib("erleen.hrl").

-export([reinit/3, handle_in/4, handle_reply/3, terminate/2]).

-record(state, {sent_ping = false,
                got_pong = false,
                got_pong_single = false,
                got_reply = false,
                got_reply_single = false,
                phase = 0,
                ping_from,
                n = 3}).

reinit(_, _, []) ->
    {ok,
     #een_interface_spec{ext_in = [#een_port_spec{name = ping_spawn,
                                                  msg_type = call,
                                                  arrity = 0},
                                   #een_port_spec{name = spawn,
                                                  msg_type = cast,
                                                  arrity = 0}],
                         int_out = [#een_port_spec{name = spawn,
                                                   msg_type = cast,
                                                   arrity = 0},
                                    #een_port_spec{name = ping_call,
                                                   type = multi,
                                                   msg_type = call,
                                                   arrity = 0},
                                    #een_port_spec{name = ping_cast,
                                                   type = multi,
                                                   msg_type = cast,
                                                   arrity = 0},
                                    #een_port_spec{name = ping_call_single,
                                                   msg_type = call,
                                                   arrity = 0},
                                    #een_port_spec{name = ping_cast_single,
                                                   msg_type = cast,
                                                   arrity = 0}],
                         int_in = [#een_port_spec{name = pong_cast,
                                                  type = multi,
                                                  msg_type = cast,
                                                  arrity = 0},
                                   #een_port_spec{name = pong_cast_single,
                                                  msg_type = cast,
                                                  arrity = 0}]},
     #state{}}.

handle_in(ping_spawn, {}, From, State = #state{sent_ping = false,
                                               phase = Phase}) ->
    {ok, MsgIdCall} = een:out(ping_call, {}),
    {ok, MsgIdCallSingle} = een:out(ping_call_single, {}),
    een:out(ping_cast, {}),
    een:out(ping_cast_single, {}),
    {ok, State#state{sent_ping = {true, MsgIdCall, MsgIdCallSingle},
                     phase = Phase + 1,
                     ping_from = From}};
handle_in(spawn, {}, _From, State = #state{n = N}) ->
    io:format("******************** GOT SPAWN~n"),
    check_final_state(State),
    een:out(spawn, {}),
    %% Reset state
    {ok, #state{n = N + 1}};
handle_in(pong_cast, Params, _From, State = #state{sent_ping = {true, _, _},
                                                   got_pong = false,
                                                   phase = Phase,
                                                   n = N}) ->
    N = length(Params), %% assertion
    {ok, maybe_pong(State#state{got_pong = true, phase = Phase + 1})};
handle_in(pong_cast_single, {}, _From, State = #state{sent_ping = {true, _, _},
                                                      got_pong_single = false,
                                                      phase = Phase}) ->
    {ok, maybe_pong(State#state{got_pong_single = true, phase = Phase + 1})}.

handle_reply(MsgIdCall, Replies, State = #state{sent_ping = {true, MsgIdCall, _},
                                                got_reply = false,
                                                phase = Phase,
                                                n = N}) ->
    N = length(Replies), %% assertion
    {ok, maybe_pong(State#state{got_reply = true, phase = Phase + 1})};
handle_reply(MsgIdCallSingle, {reply, pong},
             State = #state{sent_ping = {true, _, MsgIdCallSingle},
                            got_reply_single = false,
                            phase = Phase}) ->
    {ok, maybe_pong(State#state{got_reply_single = true, phase = Phase + 1})}.

check_final_state(#state{sent_ping = {true, _ , _},
                         got_pong = true,
                         got_reply = true,
                         got_pong_single = true,
                         got_reply_single = true,
                         phase = 5}) ->
    ok;
check_final_state(State) ->
    exit({fail_state, State}).

maybe_pong(#state{sent_ping = {true, _, _},
                  got_pong = true,
                  got_reply = true,
                  got_pong_single = true,
                  got_reply_single = true,
                  ping_from = From} = State) ->
    een:reply(From, pong),
    State;
maybe_pong(State) ->
    State.

terminate(Reason, State) ->
    check_final_state(State),
    Reason.
