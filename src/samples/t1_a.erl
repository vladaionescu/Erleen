
-module(t1_a).

-behaviour(een_comp).

-compile(export_all).

-include_lib("erleen.hrl").

-record(state, {got_ping = false,
                got_pong2 = false,
                got_reply = false}).

start() ->
    een_comp:start(?MODULE, []).

reinit(_, _, []) ->
    {ok, #een_interface_spec{ext_in  = [#een_port_spec{name = ping_a,
                                                       msg_type = call,
                                                       arrity = 0},
                                        #een_port_spec{name = pong2_a,
                                                       msg_type = call,
                                                       arrity = 0}],
                             ext_out = [#een_port_spec{name = ping_a,
                                                       msg_type = call,
                                                       arrity = 0}]},
     #state{}}.

handle_in(ping_a, {}, From, State = #state{got_ping = false, got_pong2 = false, got_reply = false}) ->
    MsgId = een:out(ping_a, {}),
    {ok, State#state{got_ping = {true, MsgId, From}}};
handle_in(pong2_a, {}, _From, State = #state{got_ping = {true, _, From}, got_pong2 = false}) ->
    case State of
        #state{got_reply = true} -> een:reply(From, pong);
        _                        -> ok
    end,
    {ok, State#state{got_pong2 = true}}.

handle_reply(MsgId, {reply, pong_reply},
             State = #state{got_ping = {true, MsgId, From}, got_reply = false}) ->
    case State of
        #state{got_pong2 = true} -> een:reply(From, pong);
        _                        -> ok
    end,
    {ok, State#state{got_reply = true}}.

terminate(Reason, #state{got_ping = {true, _}, got_pong2 = true, got_reply = true}) ->
    Reason;
terminate(Reason, State) ->
    {fail_state, Reason, State}.
