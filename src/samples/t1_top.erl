
-module(t1_top).

-behaviour(een_top).

-compile(export_all).

-record(state, {caller,
                sent = false,
                got_reply = false,
                got_pong1 = false}).

start(Caller) ->
    een_top:start(?MODULE, [Caller]).

reinit(_, _, [Caller]) ->
    {ok, #state{caller = Caller}}.

int_in_if(_) ->
    [{pong1_top, {cast, 0}}].

int_out_if(_) ->
    [{ping_top, {call, 0}}].

handle_in(pong1_top, {cast, []}, _From, State = #state{sent = {true, _}, got_pong1 = false, caller = Caller}) ->
    NewState = State#state{got_pong1 = true},
    case NewState of
        #state{got_reply = true} -> Caller ! pong_out,
                                    {stop, normal, NewState};
        _                        -> {ok, NewState}
    end;
handle_in(ping_in, {cast, []}, _From, State = #state{sent = false}) -> %% fake in
    MsgId = een:out(ping_top, {call, []}),
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
