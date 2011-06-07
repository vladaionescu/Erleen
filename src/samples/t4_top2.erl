
-module(t4_top2).

-behaviour(een_comp).

-compile(export_all).

-include_lib("erleen.hrl").

-record(state, {sent = false,
                got_reply1 = false,
                got_reply2 = false,
                got_pong = false}).

reinit(_, _, []) ->
    {ok, #een_interface_spec{int_in  = [#een_port_spec{name = pong_top,
                                                       msg_type = cast,
                                                       arrity = 1}],
                             int_out = [#een_port_spec{name = ping_top1,
                                                       msg_type = call,
                                                       arrity = 0},
                                        #een_port_spec{name = ping_top2,
                                                       msg_type = call,
                                                       arrity = 0}],
                             ext_in  = [#een_port_spec{name = ping_in,
                                                       msg_type = cast,
                                                       arrity = 0}],
                             ext_out = [#een_port_spec{name = pong_out,
                                                       msg_type = cast,
                                                       arrity = 1}]},
     #state{}}.

handle_in(pong_top, {pong1}, _From, State = #state{sent = {true, _, _},
                                                   got_pong = false}) ->
    NewState = State#state{got_pong = true},
    case NewState of
        #state{got_reply1 = true, got_reply2 = true} ->
            een:out(pong_out, {pong2}),
            {ok, NewState};
        _ ->
            {ok, NewState}
    end;
handle_in(ping_in, {}, _From, State = #state{sent = false}) ->
    {ok, MsgId1} = een:out(ping_top1, {}),
    {ok, MsgId2} = een:out(ping_top2, {}),
    {ok, State#state{sent = {true, MsgId1, MsgId2}}}.

handle_reply(MsgId1, {reply, pong}, State = #state{sent = {true, MsgId1, _},
                                                   got_reply1 = false}) ->
    NewState = State#state{got_reply1 = true},
    case NewState of
        #state{got_pong = true, got_reply2 = true} ->
            een:out(pong_out, {pong1}),
            {ok, NewState};
        _ ->
            {ok, NewState}
    end;
handle_reply(MsgId2, {reply, pong}, State = #state{sent = {true, _, MsgId2},
                                                   got_reply2 = false}) ->
    NewState = State#state{got_reply2 = true},
    case NewState of
        #state{got_pong = true, got_reply1 = true} ->
            een:out(pong_out, {pong1}),
            {ok, NewState};
        _ ->
            {ok, NewState}
    end.

terminate(Reason, #state{sent = {true, _, _},
                         got_reply1 = true,
                         got_reply2 = true,
                         got_pong = true}) ->
    Reason;
terminate(Reason, State) ->
    {fail_state, Reason, State}.
