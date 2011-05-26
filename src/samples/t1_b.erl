
-module(t1_b).

-behaviour(een_coord).

-compile(export_all).

-record(state, {got_ping = false}).

start() ->
    een_coord:start(?MODULE, []).

reinit(_, _, []) ->
    {ok, #state{}}.

ext_in_if(_) ->
    [{ping_b, {call, 0}}].

ext_out_if(_) ->
    [{pong1_b, {cast, 0}},
     {pong2_b, {cast, 0}}].

int_in_if(_) ->
    [].

int_out_if(_) ->
    [].

handle_in(ping_b, {call, []}, _From, State = #state{got_ping = false}) ->
    een:out(pong1_b, {cast, []}),
    een:out(pong2_b, {cast, []}),
    {reply, pong_reply, State#state{got_ping = true}}.

handle_reply(_, _, State) ->
    {stop, unexpected_reply, State}.

terminate(Reason, #state{got_ping = true}) ->
    Reason;
terminate(Reason, State) ->
    {fail_state, Reason, State}.
