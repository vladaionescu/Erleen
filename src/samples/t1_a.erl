
-module(t1_a).

-behaviour(een_coord).

-compile(export_all).

-record(state, {got_ping = false,
                got_pong2 = false,
                got_reply = false}).

start() ->
    een_coord:start(?MODULE, []).

reinit(_, _, []) ->
    {ok, #state{}}.

ext_in_if(_) ->
    [{ping_a, basic, call, 0},
     {pong2_a, basic, cast, 0}].

ext_out_if(_) ->
    [{{ping_a, call}, 0}].

int_in_if(_) ->
    [].

int_out_if(_) ->
    [].

handle_in({ping_a, call}, [], From, State = #state{got_ping = false, got_pong2 = false, got_reply = false}) ->
    MsgId = een:out(ping_a, {}),
    {ok, State#state{got_ping = {true, MsgId, From}}};
handle_in({pong2_a, cast}, [], _From, State = #state{got_ping = {true, _, From}, got_pong2 = false}) ->
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
