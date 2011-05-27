
-module(t1_top).

-behaviour(een_comp).

-compile(export_all).

-include_lib("erleen.hrl").

-record(state, {caller,
                sent = false,
                got_reply = false,
                got_pong1 = false}).

start(Caller) ->
    een_comp:start(?MODULE, [Caller]).

reinit(_, _, [Caller]) ->
    {ok, #een_interface_spec{int_in  = [#een_port_spec{name = pong1_top,
                                                       msg_type = cast,
                                                       arrity = 0}],
                             int_out = [#een_port_spec{name = ping_top,
                                                       msg_type = call,
                                                       arrity = 0}],
                             ext_in  = [#een_port_spec{name = ping_in,
                                                       msg_type = cast,
                                                       arrity = 0}]},
     #state{caller = Caller}}.

handle_in(pong1_top, {}, _From, State = #state{sent = {true, _}, got_pong1 = false, caller = Caller}) ->
    NewState = State#state{got_pong1 = true},
    case NewState of
        #state{got_reply = true} -> Caller ! pong_out,
                                    {stop, normal, NewState};
        _                        -> {ok, NewState}
    end;
handle_in(ping_in, {}, _From, State = #state{sent = false}) -> %% fake in
    MsgId = een:out(ping_top, {}),
    {ok, State#state{sent = {true, MsgId}}}.

handle_reply(MsgId, {reply, pong}, State = #state{sent = {true, MsgId}, got_reply = false, caller = Caller}) ->
    NewState = State#state{got_reply = true},
    case NewState of
        #state{got_pong1 = true} -> Caller ! pong_out,
                                    {stop, normal, NewState};
        _                        -> {ok, NewState}
    end.

terminate(Reason, #state{sent = {true, _}, got_reply = true, got_pong1 = true}) ->
    Reason;
terminate(Reason, State) ->
    {fail_state, Reason, State}.
