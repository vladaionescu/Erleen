
-module(t5_sender).

-behaviour(een_comp).

-compile(export_all).

-include_lib("erleen.hrl").

-record(state, {start = false,
                phase = 0,
                phase_msg_id,
                phase_msg_fun,
                check_reply_fun,
                nphases}).

reinit(_, _, [PhaseMsgFun, CheckReplyFun, NPhases, PortType]) ->
    {ok, #een_interface_spec{ext_in  = [#een_port_spec{name = start,
                                                       msg_type = call,
                                                       arrity = 0}],
                             ext_out = [#een_port_spec{name = route_out,
                                                       type = PortType,
                                                       msg_type = call,
                                                       arrity = 1}]},
     #state{phase_msg_fun = PhaseMsgFun,
            check_reply_fun = CheckReplyFun,
            nphases = NPhases}}.

handle_in(start, {}, From, State = #state{start = false,
                                          phase = Phase,
                                          phase_msg_fun = PhaseMsgFun}) ->
    {ok, MsgId} = een:out(route_out, PhaseMsgFun(Phase)),
    {ok, State#state{start = {true, From},
                     phase = Phase + 1,
                     phase_msg_id = MsgId}}.

handle_reply(MsgId, Reply, State = #state{phase_msg_id = MsgId,
                                          phase = Phase,
                                          nphases = NPhases,
                                          start = {true, From},
                                          check_reply_fun = CheckReplyFun,
                                          phase_msg_fun = PhaseMsgFun}) ->
    CheckReplyFun(Reply, Phase - 1),
    case Phase of
        NPhases -> een:reply(From, ok),
                   {ok, State#state{phase = Phase + 1}};
        _       -> {ok, NewMsgId} = een:out(route_out, PhaseMsgFun(Phase)),
                   {ok, State#state{phase = Phase + 1,
                                    phase_msg_id = NewMsgId}}
    end.

terminate(Reason, #state{start = {true, _}, phase = Phase, nphases = NPhases})
        when Phase =:= NPhases + 1 ->
    Reason;
terminate(Reason, State) ->
    {fail_state, Reason, State}.
