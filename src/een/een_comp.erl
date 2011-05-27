
-module(een_comp).

-behaviour(een_gen).

-export([behaviour_info/1]).
-export([start/4]).
-export([reinit/3, handle_cast/2, handle_call/3, handle_reply/3, terminate/2,
         handle_child_exit/3, handle_parent_exit/2]).

-include_lib("erleen.hrl").

-record(state, {id = een_comp,
                mod,
                mst,
                if_spec,
                spec,
                map_comps,
                map_pid_compid}).

-record(component, {pid,
                    in_binds = orddict:new(),
                    out_binds = orddict:new(),
                    spec,
                    children_config}).

%% ----------------------------------------------------------------------------
%% Internal interface
%% ----------------------------------------------------------------------------

start(OldModule, OldState, Module, Args) ->
    Spec = get('$een_child_component_spec'),
    een_gen:start_child(?MODULE, [OldModule, OldState, Module, Args, Spec]).

%% ----------------------------------------------------------------------------
%% Behaviour spec
%% ----------------------------------------------------------------------------

behaviour_info(callback) ->
    [
        %% Msg = Params | [Params] | {Key, Params}
        %% Params = tuple()
        %% Key = any()
     
        %% HandleReturn = {ok, NewState} |
        %%                {reply, Reply, NewState} |
        %%                {stop, Reason, NewState}

        %% (OldModule, OldState, Args) ->
        %%     {ok, InterfaceSpec, State} | {error, Error}
        {reinit, 3},

        %% (PortName, Msg, From, State) -> HandleReturn
        {handle_in, 4},

        %% (MsgId, Reply, State) -> HandleReturn
        {handle_reply, 3},

        %% (Reason, State) -> NewReason
        {terminate, 2}
    ];
behaviour_info(_) ->
    undefined.

%% ----------------------------------------------------------------------------
%% Gen callbacks
%% ----------------------------------------------------------------------------

reinit(_OldModule, _OldState, [OldMod, OldMst, Mod, Args,
                               Spec = #een_component_spec{id = Id}]) ->
    put('$een_component_id', Id),
    een_out:reset(),
    case Mod:reinit(OldMod, OldMst, Args) of
        {ok, InterfaceSpec = #een_interface_spec{}, Mst0} ->
            {ok, #state{mod = Mod,
                        mst = Mst0,
                        spec = Spec,
                        if_spec = adjust_interface_spec(InterfaceSpec)}};
        {error, _} = E -> E
    end.

handle_cast({msg, LocalId, Msg}, State) ->
    do_handle_in(LocalId, Msg, none, State).

handle_call({set_ext_bindings, _InBindings, OutBindings}, _From,
            State = #state{if_spec = IfSpec}) ->
    een_out:set(OutBindings, IfSpec, ext),
    {reply, ok, State};
handle_call({set_children_config, Config}, _From, State) ->
    handle_children_config(Config, State);
handle_call({msg, LocalId, Msg}, From, State) ->
    do_handle_in(LocalId, Msg, From, State).

handle_reply(MsgId, Reply, State = #state{mod = Mod, mst = Mst}) ->
    handle_return(Mod:handle_reply(MsgId, Reply, Mst), State).

handle_parent_exit(Reason, State) ->
    NewReason = case Reason of
                    {parent_death, _} -> Reason;
                    _                 -> {parent_death, Reason}
                end,
    {stop, NewReason, State}.

handle_child_exit(_Pid, _Reason, State) ->
    %% TODO
    {ok, State}.

terminate(Reason, #state{mod = Mod, mst = Mst}) ->
    Mod:terminate(Reason, Mst).

%% ----------------------------------------------------------------------------
%% Internal
%% ----------------------------------------------------------------------------

handle_children_config(Config = #een_children_config{children = Children,
                                                     bindings = Bindings},
                       State = #state{spec = Spec}) ->
    een:report("Setting children config ~p~n", [Config]),
    Id = Spec#een_component_spec.id,
    %% TODO: check against existing components
    MCs = orddict:store(Id, #component{pid = self(), spec = Spec}, orddict:new()),
    State1 = lists:foldl(fun register_child/2, State#state{map_comps = MCs,
                                                           map_pid_compid = orddict:new()},
                         Children),
    State2 = lists:foldl(fun spawn_child/2, State1, orddict:fetch_keys(State1#state.map_comps)),
    State3 = lists:foldl(fun register_binding/2, State2, Bindings),
    send_bindings_and_children_config(State3).

register_child({Spec = #een_component_spec{id = Id}, ChildrenConfig},
               State = #state{map_comps = MCs}) ->
    MCs1 = orddict:store(Id, #component{spec = Spec,
                                        children_config = ChildrenConfig}, MCs),
    State#state{map_comps = MCs1}.

spawn_child(Id, State) ->
    respawn_child(Id, none, none, State).

respawn_child(Id, _, _, State = #state{spec = #een_component_spec{id = Id}}) ->
    State;
respawn_child(Id, OldModule, OldState, State = #state{map_comps = MCs,
                                                      map_pid_compid = MPC}) ->
    %% TODO: handle failures
    Component = #component{pid = OldPid, spec = Spec} = orddict:fetch(Id, MCs),
    {ok, NewPid} = een_config:respawn_child(Spec, OldModule, OldState),
    NewMCs = orddict:store(Id, Component#component{pid = NewPid}, MCs),
    MPC1 = orddict:erase(OldPid, MPC),
    MPC2 = orddict:store(NewPid, Id, MPC1),
    State#state{map_comps = NewMCs, map_pid_compid = MPC2}.

register_binding(#een_binding{from = #een_port{comp_id = Id1,
                                               port_name = Port1},
                              to   = #een_port{comp_id = Id2,
                                               port_name = Port2}},
                 State = #state{map_comps = MCs}) ->
    %% TODO: check validity
    #component{pid = Pid1, out_binds = OutBinds0} = orddict:fetch(Id1, MCs),
    #component{pid = Pid2, in_binds = InBinds0} = orddict:fetch(Id2, MCs),
    Entry2 = {Pid2, Port2},
    OutBinds1 =
        orddict:update(Port1,
                       fun (PortList) -> [Entry2 | PortList] end, [Entry2],
                       OutBinds0),
    %% TODO: do we need pids in inbinds ?? - perhaps keep symmetric somehow, if not
    Entry1 = {Pid1, Port1},
    InBinds1 =
        orddict:update(Port2,
                       fun (PortList) -> [Entry1 | PortList] end, [Entry1],
                       InBinds0),
    MCs1 = orddict:update(
               Id1, fun (C) -> C#component{out_binds = OutBinds1} end, MCs),
    MCs2 = orddict:update(
               Id2, fun (C) -> C#component{in_binds = InBinds1} end, MCs1),
    State#state{map_comps = MCs2}.

send_bindings_and_children_config(State = #state{map_comps = MCs,
                                                 spec = #een_component_spec{id = SelfId},
                                                 if_spec = IfSpec}) ->
    %% TODO: parallelize
    %% TODO: handle failures
    orddict:fold(
        fun (_, #component{spec = #een_component_spec{id = Id},
                           out_binds = OutBinds}, ok)
                    when Id =:= SelfId ->
                ok = een_out:set(OutBinds, IfSpec, int);
            (_, #component{pid = Pid,
                           in_binds = InBinds,
                           out_binds = OutBinds,
                           children_config = ChildrenConfig}, ok) ->
                ok = een_config:set_ext_bindings(Pid, InBinds, OutBinds),
                ok = een_config:set_children_config(Pid, ChildrenConfig)
        end, ok, MCs),
    {reply, ok, State}.

adjust_interface_spec(Spec = #een_interface_spec{ext_in = ExtIn,
                                                 ext_out = ExtOut,
                                                 int_in = IntIn,
                                                 int_out = IntOut}) ->
    Spec#een_interface_spec{ext_in = orddict:from_list(lists:map(fun port_spec_to_entry/1, ExtIn)),
                            ext_out = orddict:from_list(lists:map(fun port_spec_to_entry/1, ExtOut)),
                            int_in = orddict:from_list(lists:map(fun port_spec_to_entry/1, IntIn)),
                            int_out = orddict:from_list(lists:map(fun port_spec_to_entry/1, IntOut))}.

port_spec_to_entry(PortSpec = #een_port_spec{name = Name}) ->
    {Name, PortSpec}.

do_handle_in(IfId, Msg, From, State = #state{mod = Mod, mst = Mst}) ->
    handle_return(Mod:handle_in(IfId, Msg, From, Mst), State).

handle_return({ok, NewMst}, State) ->
    {ok, State#state{mst = NewMst}};
handle_return({reply, Reply, NewMst}, State) ->
    {reply, Reply, State#state{mst = NewMst}};
handle_return({stop, Reason, NewMst}, State) ->
    {stop, Reason, State#state{mst = NewMst}}.
