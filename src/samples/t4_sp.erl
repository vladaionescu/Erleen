
-module(t4_sp).

-behaviour(een_comp).

-compile(export_all).

-include_lib("erleen.hrl").

-record(state, {sent = false,
                got_reply = false}).

reinit(_, _, []) ->
    {ok, #een_interface_spec{int_out = [#een_port_spec{name = ping_sp,
                                                       msg_type = call,
                                                       arrity = 0}],
                             ext_in  = [#een_port_spec{name = ping_in,
                                                       msg_type = cast,
                                                       arrity = 0}]},
     #state{}}.

handle_in(ping_in, {}, From, State = #state{sent = false}) ->
    {ok, MsgId} = een:out(ping_sp, {}),
    {ok, State#state{sent = {true, MsgId, From}}}.

handle_reply(MsgId, {reply, pong}, State = #state{sent = {true, MsgId, From},
                                             got_reply = false}) ->
    een:reply(From, pong),
    {ok, State#state{got_reply = true}}.

terminate(Reason, #state{sent = {true, _, _}, got_reply = true}) ->
    Reason;
terminate(Reason, State) ->
    {fail_state, Reason, State}.
