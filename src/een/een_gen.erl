
-module(een_gen).

-export([behaviour_info/1]).
-export([start/2, start_child/2, cast/2, call/2, async_call/2, do_call/2, reply/2]).

-record(state, {id = een_gen,
                parent,
                mod,
                mst}).

%% ----------------------------------------------------------------------------
%% Interface
%% ----------------------------------------------------------------------------

start(Module, Args) ->
    {ok, spawn(fun () -> State0 = init(#state{mod = Module, mst = Args}),
                         loop(State0)
               end)}.

start_child(Module, Args) ->
    Parent = self(),
    Node = get('$een_child_node'),
    Child =
        spawn_link(Node,
                   fun () -> State0 = init(#state{mod = Module,
                                                  mst = Args,
                                                  parent = Parent}),
                             Parent ! '$een_init_done',
                             loop(State0)
                   end),
    receive
        '$een_init_done'        -> register_child(Child),
                                   {ok, Child};
        {'EXIT', Child, Reason} -> throw({start_child_failed, Reason})
    end.

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
    Futures = case get('$reply_futures_set') of
                  undefined -> ordsets:new();
                  F         -> F
              end,
    put('$reply_futures_set', ordsets:add_element(Monitor, Futures)),
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

        %% (Reason, State) -> HandleReturn
        {handle_parent_exit, 2},

        %% (Child, Reason, State) -> HandleReturn
        {handle_child_exit, 3},

        %% (Reason, State) -> NewReason
        {terminate, 2}
    ];
behaviour_info(_) ->
    undefined.

%% ----------------------------------------------------------------------------
%% Main loop
%% ----------------------------------------------------------------------------

init(State = #state{mod = Module, mst = Args}) ->
    process_flag(trap_exit, true),
    put('$een_children', ordsets:new()),
    Mst0 = case Module:reinit(none, none, Args) of
               {ok, S0}       -> S0;
               {error, Error} -> exit({init_fail, Error})
           end,
    State#state{mst = Mst0}.

loop(State = #state{mod = Mod, mst = Mst, parent = Parent}) ->
    loop(
        receive
            {'$een_cast', Msg} ->
                handle_return(Mod:handle_cast(Msg, Mst), none, State);
            {'$een_call', Msg, From} ->
                handle_return(Mod:handle_call(Msg, From, Mst), From, State);
            {'$een_reply', Monitor, Reply} ->
                check_expected_reply(Monitor),
                erlang:demonitor(Monitor, [flush]),
                handle_return(Mod:handle_reply(Monitor, {reply, Reply}, Mst),
                              none, State);
            {'DOWN', Monitor, process, _Pid, Reason} ->
                check_expected_reply(Monitor),
                handle_return(Mod:handle_reply(Monitor, {'DOWN', Reason}, Mst),
                              none, State);
            {'EXIT', Parent, Reason} ->
                handle_return(Mod:handle_parent_exit(Reason, Mst), none, State);
            {'EXIT', Other, Reason} ->
                case is_child(Other) of
                    true  -> unregister_child(Other),
                             handle_return(
                                 Mod:handle_child_exit(Other, Reason, Mst),
                                 none, State);
                    false -> exit(Reason)
                end
        end).

check_expected_reply(Monitor) ->
    true = ordsets:is_element(Monitor, get('$reply_futures_set')).

handle_return({ok, NewMst}, _From, State) ->
    State#state{mst = NewMst};
handle_return({reply, Reply, NewMst}, From, State) ->
    reply(From, Reply),
    State#state{mst = NewMst};
handle_return({stop, Reason, NewMst}, _From, #state{mod = Mod}) ->
    exit(Mod:terminate(Reason, NewMst)).

register_child(Pid) ->
    put('$een_children', ordsets:add_element(Pid, get('$een_children'))).

unregister_child(Pid) ->
    put('$een_children', ordsets:del_element(Pid, get('$een_children'))).

is_child(Pid) ->
    ordsets:is_element(Pid, get('$een_children')).
