
-module(een_gen).

-export([behaviour_info/1]).
-export([start/2, cast/2, call/2, async_call/2, do_call/2, reply/2]).

-record(state, {id = een_gen,
                mod,
                mst}).

%% ----------------------------------------------------------------------------
%% Interface
%% ----------------------------------------------------------------------------

start(Module, Args) ->
    {ok, spawn(fun () -> init(#state{mod = Module, mst = Args}) end)}.

cast(Pid, Msg) ->
    Pid ! {'$een_cast', Msg},
    ok.

call(Pid, Msg) ->
    Monitor = do_call(Pid, Msg),
    receive
        {'$een_reply', Monitor, Reply} ->
            erlang:demonitor(Monitor, [flush]),
            Reply;
        {'DOWN', Monitor, process, Pid, Reason} ->
            throw({'DOWN', Reason})
    end.

async_call(Pid, Msg) ->
    Monitor = do_call(Pid, Msg),
    %% TODO: check reply against futures
    %%Futures = get('$reply_futures_set'),
    %%put('$reply_futures_set', ordsets:add_element(Monitor, Futures)),
    Monitor.

do_call(Pid, Msg) ->
    Monitor = erlang:monitor(process, Pid),
    receive {'DOWN', Monitor, process, Pid, noproc} -> throw(noproc)
    after 0 -> ok
    end,
    Pid ! {'$een_call', Msg, {self(), Monitor}},
    Monitor.

reply({Pid, Monitor}, Msg) ->
    Pid ! {'$een_reply', Monitor, Msg}.

%% ----------------------------------------------------------------------------
%% Behaviour spec
%% ----------------------------------------------------------------------------

behaviour_info(callback) ->
    [
        %% HandleReturn =
        %%     {ok, NewState} | {reply, Msg, NewState} |
        %%         {stop, Reason, NewState}

        %% (OldModule, OldState, Args) ->
        %%     {ok, State} | {error, Error}
        {reinit, 3},

        %% (Msg, State) -> HandleReturn
        {handle_cast, 2},

        %% (Msg, From, State) -> HandleReturn
        {handle_call, 3},

        %% (MsgId, {reply, Reply} | {'DOWN', Reason}, State) -> HandleReturn
        {handle_reply, 3},

        %% (Reason, State) -> NewReason
        {terminate, 2}
    ];
behaviour_info(_) ->
    undefined.

%% ----------------------------------------------------------------------------
%% Main loop
%% ----------------------------------------------------------------------------

init(State = #state{mod = Module, mst = Args}) ->
    Mst0 = case Module:reinit(none, none, Args) of
               {ok, S0}       -> S0;
               {error, Error} -> exit({init_fail, Error})
           end,
    loop(State#state{mst = Mst0}).

loop(State = #state{mod = Mod, mst = Mst}) ->
    loop(
        receive
            {'$een_cast', Msg} ->
                handle_return(Mod:handle_cast(Msg, Mst), none, State);
            {'$een_call', Msg, From} ->
                handle_return(Mod:handle_call(Msg, From, Mst), From, State);
            {'$een_reply', Monitor, Reply} ->
                erlang:demonitor(Monitor, [flush]),
                handle_return(Mod:handle_reply(Monitor, {reply, Reply}, Mst),
                              none, State);
            {'DOWN', Monitor, process, _Pid, Reason} ->
                handle_return(Mod:handle_reply(Monitor, {'DOWN', Reason}, Mst),
                              none, State)
        end).

handle_return({ok, NewMst}, _From, State) ->
    State#state{mst = NewMst};
handle_return({reply, Reply, NewMst}, From, State) ->
    reply(From, Reply),
    State#state{mst = NewMst};
handle_return({stop, Reason, NewMst}, _From, #state{mod = Mod}) ->
    exit(Mod:terminate(Reason, NewMst)).
