
-module(t5_top).

-behaviour(een_comp).

-compile(export_all).

-include_lib("erleen.hrl").

-record(state, {caller,
                sent = false,
                got_reply = false}).

reinit(_, _, [Caller]) ->
    {ok, #een_interface_spec{int_out = [#een_port_spec{name = start,
                                                       msg_type = call,
                                                       arrity = 0}],
                             ext_in  = [#een_port_spec{name = start,
                                                       msg_type = cast,
                                                       arrity = 0}]},
     #state{caller = Caller}}.

handle_in(start, {}, _From, State = #state{sent = false}) ->
    {ok, MsgId} = een:out(start, {}),
    {ok, State#state{sent = {true, MsgId}}}.

handle_reply(MsgId, {reply, ok}, State = #state{sent = {true, MsgId}, got_reply = false, caller = Caller}) ->
    Caller ! pong_out,
    {stop, normal, State#state{got_reply = true}}.

terminate(Reason, #state{sent = {true, _}, got_reply = true}) ->
    Reason;
terminate(Reason, State) ->
    {fail_state, Reason, State}.
