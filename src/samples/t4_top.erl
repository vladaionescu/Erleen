
-module(t4_top).

-behaviour(een_comp).

-compile(export_all).

-include_lib("erleen.hrl").

-record(state, {caller,
                sent = false,
                got_reply = false,
                got_pong = false,
                n_pings,
                n_pongs,
                auto_stop}).

reinit(_, _, [Caller, NPings, NPongs, AutoStop]) ->
    {ok, #een_interface_spec{int_in  = [#een_port_spec{name = pong_top,
                                                       type = multi,
                                                       msg_type = cast,
                                                       arrity = 1}],
                             int_out = [#een_port_spec{name = ping_top,
                                                       type = multi,
                                                       msg_type = call,
                                                       arrity = 0}],
                             ext_in  = [#een_port_spec{name = ping_in,
                                                       msg_type = cast,
                                                       arrity = 0}],
                             ext_out = [#een_port_spec{name = pong_out,
                                                       msg_type = cast,
                                                       arrity = 1}]},
     #state{caller = Caller,
            n_pings = NPings,
            n_pongs = NPongs,
            auto_stop = AutoStop}}.

handle_in(pong_top, PongParams, _From, State = #state{sent = {true, _},
                                                      got_pong = false,
                                                      caller = Caller,
                                                      auto_stop = AutoStop,
                                                      n_pongs = NPongs}) ->
    NPongs = length(PongParams), %% assertion
    [{pong2} = PongParam || PongParam <- PongParams], %% assertion
    NewState = State#state{got_pong = true},
    case NewState of
        #state{got_reply = true} ->
            een:out(pong_out, {pong2}),
            if
                is_pid(Caller) -> Caller ! pong_out;
                true           -> ok
            end,
            case AutoStop of
                true  -> {stop, normal, NewState};
                false -> {ok, NewState}
            end;
        _ ->
            {ok, NewState}
    end;
handle_in(ping_in, {}, _From, State = #state{sent = false}) ->
    {ok, MsgId} = een:out(ping_top, {}),
    {ok, State#state{sent = {true, MsgId}}}.

handle_reply(MsgId, Replies, State = #state{sent = {true, MsgId},
                                            got_reply = false,
                                            caller = Caller,
                                            auto_stop = AutoStop,
                                            n_pings = NPings}) ->
    NPings = length(Replies), %% assertion
    NewState = State#state{got_reply = true},
    case NewState of
        #state{got_pong = true} ->
            een:out(pong_out, {pong2}),
            if
                is_pid(Caller) -> Caller ! pong_out;
                true           -> ok
            end,
            case AutoStop of
                true  -> {stop, normal, NewState};
                false -> {ok, NewState}
            end;
        _ ->
            {ok, NewState}
    end.

terminate(Reason, #state{sent = {true, _}, got_reply = true, got_pong = true}) ->
    Reason;
terminate(Reason, State) ->
    {fail_state, Reason, State}.
