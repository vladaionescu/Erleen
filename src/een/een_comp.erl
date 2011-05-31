
-module(een_comp).

-behaviour(een_gen).

-export([behaviour_info/1]).
-export([start/4]).
-export([reinit/3, handle_cast/2, handle_call/3, handle_reply/3, terminate/2,
         handle_child_exit/3, handle_parent_exit/2]).

-include_lib("erleen.hrl").

-define(CONTROL_INTERFACE_SPEC,
        #een_interface_spec{ext_in  = [#een_port_spec{name = shutdown,
                                                      msg_type = cast,
                                                      arrity = 1}],
                            int_out = [#een_port_spec{name = shutdown,
                                                      type = multi,
                                                      msg_type = cast,
                                                      arrity = 1}]}).

-define(SPAWN_CONTROL_INTERFACE_SPEC,
        #een_interface_spec{int_out = [#een_port_spec{name = kill,
                                                      type = route,
                                                      msg_type = cast,
                                                      arrity = 1}]}).

%% ----------------------------------------------------------------------------
%% Internal interface
%% ----------------------------------------------------------------------------

start(OldModule, OldState, Module, Args) ->
    Spec = get('$een_child_component_spec'),
    een_gen:start_child(?MODULE, [OldModule, OldState, Module, Args, Spec]).

get_s() ->
    get('$een_comp_state').

put_s(State = #een_state{}) ->
    put('$een_comp_state', State).

%% ----------------------------------------------------------------------------
%% Behaviour spec
%% ----------------------------------------------------------------------------

behaviour_info(callback) ->
    [
        %% Msg = Params | [Params] | {Key, Params}
        %% Params = tuple()
        %% Key = any()
     
        %% HandleReturn = {ok, NewState} |
        %%                {stop, Reason, NewState}

        %% HandleChildExitReturn = {restart, NewState} |
        %%                         {stop, Reason, NewState}

        %% (OldModule, OldState, Args) ->
        %%     {ok, InterfaceSpec, State} | {error, Error}
        {reinit, 3},

        %% (PortName, Msg, From, State) -> HandleReturn
        {handle_in, 4},

        %% (MsgId, Reply, State) -> HandleReturn
        {handle_reply, 3},

        %% Optional:
        %% (CompId, Reason, State) -> HandleChildExitReturn
        %{handle_child_exit, 3},

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
    een_multi_reply_buffer:new(),
    case Mod:reinit(OldMod, OldMst, Args) of
        {ok, InterfaceSpec = #een_interface_spec{}, Mst0} ->
            State0 = #een_state{mod = Mod,
                               mst = Mst0,
                               spec = Spec,
                               if_spec = build_interface_spec(InterfaceSpec)},
            put_s(State0),
            set_out(),
            {ok, nostate};
        {error, _} = E -> E
    end.

handle_cast({msg, LocalId, SenderId, Msg}, nostate) ->
    do_handle_in(LocalId, SenderId, Msg, none).

handle_call({set_ext_bindings, InBindings, OutBindings}, _From, nostate) ->
    set_ext_bindings(InBindings, OutBindings),
    {reply, ok, nostate};
handle_call({set_children_config, Config}, _From, nostate) ->
    set_children_config(Config),
    {reply, ok, nostate};
handle_call({msg, LocalId, SenderId, Msg}, From, nostate) ->
    do_handle_in(LocalId, SenderId, Msg, From).

handle_reply(MsgId, Reply, nostate) ->
    do_handle_reply(MsgId, Reply).

handle_parent_exit(Reason, nostate) ->
    NewReason = case Reason of
                    {parent_death, _} -> Reason;
                    _                 -> {parent_death, Reason}
                end,
    {stop, NewReason, nostate}.

handle_child_exit(_Pid, _Reason, nostate) ->
    {ok, nostate}.

terminate(Reason, nostate) ->
    #een_state{mod = Mod, mst = Mst} = get_s(),
    Mod:terminate(Reason, Mst).

%% ----------------------------------------------------------------------------
%% Internal
%% ----------------------------------------------------------------------------

set_ext_bindings(InBinds, OutBinds) ->
    State = #een_state{if_spec = #een_interface_spec{ext_in = InSpec}} = get_s(),
    put_s(State#een_state{ext_in_binds = InBinds, ext_out_binds = OutBinds}),
    reset_multi_buf(InBinds, InSpec),
    set_out().

set_int_bindings(InBinds, OutBinds) ->
    State = #een_state{if_spec = #een_interface_spec{int_in = InSpec}} = get_s(),
    put_s(State#een_state{int_in_binds = InBinds, int_out_binds = OutBinds}),
    reset_multi_buf(InBinds, InSpec),
    set_out().

reset_multi_buf(InBinds, InSpec) ->
    State = #een_state{multi_buf = MultiBuf} = get_s(),
    %% TODO: allow for multi buffered messages not to be lost on changing bindings
    NewMultiBuf =
        orddict:fold(fun (PortName, #een_port_spec{type = multi}, CurMultiBuf) ->
                             een_multi_buffer:new_buffer(
                                 PortName,
                                 length(orddict:fetch(PortName, InBinds)),
                                 CurMultiBuf);
                         (_, _, CurMultiBuf) ->
                             CurMultiBuf
                     end, MultiBuf, InSpec),
    put_s(State#een_state{multi_buf = NewMultiBuf}).

set_children_config(Config = #een_children_config{is_spawn = false,
                                                  children = Children,
                                                  bindings = Bindings}) ->
    een:report("Setting children config ~p~n", [Config]),
    State = #een_state{spec = Spec = #een_component_spec{id = Id}} = get_s(),
    %% TODO: check against existing components
    MCs = orddict:store(Id, #een_component{pid = self(), spec = Spec}, orddict:new()),
    State1 = lists:foldl(fun register_child/2,
                         State#een_state{map_comps = MCs,
                                         map_pid_compid = orddict:new(),
                                         config = Config},
                         Children),
    put_s(State1),
    lists:foreach(fun spawn_child/1, orddict:fetch_keys(State1#een_state.map_comps)),
    State2 = get_s(),
    State3 = lists:foldl(fun register_binding/2, State2, Bindings),
    put_s(State3),
    set_children_int_bindings_and_config();
set_children_config(Config = #een_children_config{is_spawn = true,
                                                  children = [{ChildSpec, ChildChildrenConfig}],
                                                  spawn_min = SpawnMin,
                                                  spawn_max = SpawnMax,
                                                  spawn_init = SpawnInit}) ->
    een:report("Setting spawn child config ~p~n", [Config]),
    State = #een_state{spec = Spec = #een_component_spec{id = Id}} = get_s(),
    %% TODO: check against existing components
    MCs = orddict:store(Id, #een_component{pid = self(), spec = Spec}, orddict:new()),
    State1 = State#een_state{spawn_child_comp = #een_component{spec = ChildSpec,
                                                               children_config = ChildChildrenConfig},
                             spawn_min = SpawnMin,
                             spawn_max = SpawnMax,
                             map_comps = MCs,
                             map_pid_compid = orddict:new(),
                             config = Config},
    put_s(State1),
    lists:foreach(fun (_) -> ok = spawn_spawn_child() end,
                  lists:seq(1, SpawnInit)).

spawn_spawn_child() ->
    State = #een_state{spawn_child_comp =
                           #een_component{spec = Spec = #een_component_spec{id = Id},
                                          children_config = ChildrenConfig},
                       spawn_index = SpawnIndex,
                       spawn_max = SpawnMax,
                       spawn_current = SpawnCurrent,
                       config = #een_children_config{bindings = Bindings}} = get_s(),
    if
        SpawnCurrent >= SpawnMax ->
            too_many;
        true ->
            ThisId = list_to_atom(atom_to_list(Id) ++ "_" ++ integer_to_list(SpawnIndex)),
            ThisChildrenConfig = replace_id_in_children_config(ThisId, Id, ChildrenConfig),
            State1 = register_child({Spec#een_component_spec{id = ThisId}, ThisChildrenConfig}, State),
            spawn_child(ThisId),
            ThisBindings = replace_id_in_bindings(ThisId, Id, Bindings),
            State2 = lists:foldl(fun register_binding/2, State1, ThisBindings),
            put_s(State2#een_state{spawn_index = SpawnIndex + 1,
                                   spawn_current = SpawnCurrent + 1}),
            set_children_int_bindings_and_config(),
            ok
    end.

replace_id_in_children_config(
        NewId, OldId,
        ChildrenConfig = #een_children_config{spawn_binding = SpawnBinding,
                                              bindings = Bindings}) ->
    ChildrenConfig#een_children_config{
        spawn_binding = replace_id_in_binding(NewId, OldId, SpawnBinding),
        bindings = replace_id_in_bindings(NewId, OldId, Bindings)}.

replace_id_in_bindings(NewId, OldId, Bindings) ->
    lists:map(fun (Binding) -> replace_id_in_binding(NewId, OldId, Binding) end,
              Bindings).

replace_id_in_binding(NewId, OldId, {From, To}) ->
    {replace_id_in_port(NewId, OldId, From), replace_id_in_port(NewId, OldId, To)}.

replace_id_in_port(NewId, OldId, {OldId, PortName}) ->
    {NewId, PortName};
replace_id_in_port(_, _, Ret) ->
    Ret.
    

register_child({Spec = #een_component_spec{id = Id}, ChildrenConfig},
               State = #een_state{map_comps = MCs}) ->
    MCs1 = orddict:store(Id, #een_component{spec = Spec,
                                            children_config = ChildrenConfig}, MCs),
    State#een_state{map_comps = MCs1}.

spawn_child(Id) ->
    respawn_child(Id, none, none).

respawn_child(Id, OldModule, OldState) ->
    State = #een_state{map_comps = MCs,
                   map_pid_compid = MPC,
                   spec = #een_component_spec{id = OwnId}} = get_s(),
    case Id of
        OwnId ->
            ok;
        _ ->
            %% TODO: handle failures
            Component = #een_component{pid = OldPid, spec = Spec} = orddict:fetch(Id, MCs),
            {ok, NewPid} = een_config:respawn_child(Spec, OldModule, OldState),
            NewMCs = orddict:store(Id, Component#een_component{pid = NewPid}, MCs),
            MPC1 = orddict:erase(OldPid, MPC),
            MPC2 = orddict:store(NewPid, Id, MPC1),
            put_s(State#een_state{map_comps = NewMCs, map_pid_compid = MPC2}),
            ok
    end.

register_binding({{Id1, Port1}, {Id2, Port2}}, State = #een_state{map_comps = MCs}) ->
    %% TODO: check validity
    #een_component{pid = Pid1, out_binds = OutBinds0} = orddict:fetch(Id1, MCs),
    #een_component{pid = Pid2, in_binds = InBinds0} = orddict:fetch(Id2, MCs),
    Entry2 = {Id2, Pid2, Port2},
    OutBinds1 =
        orddict:update(Port1,
                       fun (PortList) -> [Entry2 | PortList] end, [Entry2],
                       OutBinds0),
    Entry1 = {Id1, Pid1, Port1},
    InBinds1 =
        orddict:update(Port2,
                       fun (PortList) -> [Entry1 | PortList] end, [Entry1],
                       InBinds0),
    MCs1 = orddict:update(
               Id1, fun (C) -> C#een_component{out_binds = OutBinds1} end, MCs),
    MCs2 = orddict:update(
               Id2, fun (C) -> C#een_component{in_binds = InBinds1} end, MCs1),
    State#een_state{map_comps = MCs2}.

set_children_int_bindings_and_config() ->
    #een_state{map_comps = MCs,
               spec = #een_component_spec{id = SelfId}} = get_s(),
    %% TODO: parallelize
    %% TODO: handle failures
    orddict:fold(
        fun (Id, #een_component{in_binds = InBinds,
                                out_binds = OutBinds}, ok)
                    when Id =:= SelfId ->
                set_int_bindings(InBinds, OutBinds),
                ok;
            (_, #een_component{pid = Pid,
                               in_binds = InBinds,
                               out_binds = OutBinds,
                               children_config = ChildrenConfig}, _) ->
                ok = een_config:set_ext_bindings(Pid, InBinds, OutBinds),
                ok = een_config:set_children_config(Pid, ChildrenConfig),
                ok
        end, ok, MCs).

interface_spec_dict(Spec = #een_interface_spec{ext_in = ExtIn,
                                               ext_out = ExtOut,
                                               int_in = IntIn,
                                               int_out = IntOut}) ->
    Spec#een_interface_spec{
        ext_in = orddict:from_list(lists:map(fun port_spec_to_entry/1, ExtIn)),
        ext_out = orddict:from_list(lists:map(fun port_spec_to_entry/1, ExtOut)),
        int_in = orddict:from_list(lists:map(fun port_spec_to_entry/1, IntIn)),
        int_out = orddict:from_list(lists:map(fun port_spec_to_entry/1, IntOut))}.

port_spec_to_entry(PortSpec = #een_port_spec{name = Name}) ->
    {Name, PortSpec}.

build_interface_spec(Spec = #een_interface_spec{}) ->
    merge_interface_specs(interface_spec_dict(Spec),
                          interface_spec_dict(?CONTROL_INTERFACE_SPEC)).

merge_interface_specs(Spec1 = #een_interface_spec{ext_in  = ExtIn1,
                                                  ext_out = ExtOut1,
                                                  int_in  = IntIn1,
                                                  int_out = IntOut1},
                              #een_interface_spec{ext_in  = ExtIn2,
                                                  ext_out = ExtOut2,
                                                  int_in  = IntIn2,
                                                  int_out = IntOut2}) ->
    Spec1#een_interface_spec{ext_in  = merge_orddict(ExtIn1, ExtIn2),
                             ext_out = merge_orddict(ExtOut1, ExtOut2),
                             int_in  = merge_orddict(IntIn1, IntIn2),
                             int_out = merge_orddict(IntOut1, IntOut2)}.

do_handle_in(Port, SenderId, Msg, From) ->
    State = #een_state{mod = Mod,
                       mst = Mst,
                       if_spec = #een_interface_spec{ext_in = ExtInPorts,
                                                 int_in = IntInPorts},
                   multi_buf = MultiBuf} = get_s(),
    %% TODO: check validity
    Type =
        case orddict:find(Port, IntInPorts) of
            {ok, #een_port_spec{type = T}} ->
                T;
            error ->
                #een_port_spec{type = T} = orddict:fetch(Port, ExtInPorts),
                T
        end,
    case Type of
        basic -> handle_return(Mod:handle_in(Port, Msg, From, Mst));
        multi -> {Outcome, NewMultiBuf} = een_multi_buffer:in(Port, SenderId,
                                                              Msg, From,
                                                              MultiBuf),
                 put_s(State#een_state{multi_buf = NewMultiBuf}),
                 case Outcome of
                     {out, MsgList, FromList} ->
                         handle_return(Mod:handle_in(Port, MsgList, FromList, Mst));
                     noout ->
                         {ok, nostate}
                 end
    end.

do_handle_reply(MsgId, Reply) ->
    #een_state{mod = Mod, mst = Mst} = get_s(),
    case een_multi_reply_buffer:in(MsgId, Reply) of
        not_multi ->
            handle_return(Mod:handle_reply(MsgId, Reply, Mst));
        {out, NewMsgId, Replies} ->
            handle_return(Mod:handle_reply(NewMsgId, Replies, Mst));
        noout ->
            {ok, nostate}
    end.

set_out() ->
    #een_state{if_spec = #een_interface_spec{ext_out = ExtOutSpec,
                                             int_out = IntOutSpec},
               ext_out_binds = ExtOutBinds,
               int_out_binds = IntOutBinds} = get_s(),
    een_out:set(merge_orddict(ExtOutBinds, IntOutBinds),
                merge_orddict(ExtOutSpec, IntOutSpec)).

handle_return({ok, NewMst}) ->
    State = get_s(),
    put_s(State#een_state{mst = NewMst}),
    {ok, nostate};
handle_return({stop, Reason, NewMst}) ->
    State = get_s(),
    put_s(State#een_state{mst = NewMst}),
    {stop, Reason, nostate}.

merge_orddict(Dict1, Dict2) ->
    orddict:merge(fun (K, _, _) -> throw({merge_conflict, K}) end, Dict1, Dict2).
