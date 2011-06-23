
-module(een_comp).

-behaviour(een_gen).

%-export([behaviour_info/1]).
%-export([start/0]).
%-export([reinit/3, handle_cast/2, handle_call/3, handle_reply/3, terminate/2,
%         handle_child_exit/3, handle_parent_exit/2]).

-compile(export_all).

-include_lib("erleen.hrl").

-define(CONTROL_INTERFACE_SPEC,
        #een_interface_spec{ext_in  = [#een_port_spec{name = shutdown,
                                                      msg_type = call,
                                                      arrity = 1}],
                            int_out = [#een_port_spec{name = shutdown,
                                                      type = multi,
                                                      msg_type = call,
                                                      arrity = 1},
                                       #een_port_spec{name = kill,
                                                      type = route,
                                                      msg_type = cast,
                                                      arrity = 1}]}).

%% ----------------------------------------------------------------------------
%% Internal interface
%% ----------------------------------------------------------------------------

start() ->
    een_gen:start_child(?MODULE, []).

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
        %%                         {stop, Reason, NewState} |
        %%                         {ok, NewState}

        %% (OldModule, OldState, Args) ->
        %%     {ok, InterfaceSpec, State} | {error, Error}
        {reinit, 3},

        %% (PortName, Msg, From, State) -> HandleReturn
        {handle_in, 4},

        %% (MsgId, Reply, State) -> HandleReturn
        {handle_reply, 3},

        %% (CompId, Reason, State) -> HandleChildExitReturn
        %% Optional
        %{handle_child_exit, 3},

        %% (Reason, State) -> NewReason
        {terminate, 2}
    ];
behaviour_info(_) ->
    undefined.

%% ----------------------------------------------------------------------------
%% Gen callbacks
%% ----------------------------------------------------------------------------

reinit(_OldModule, _OldState, []) ->
    een_multi_reply_buffer:new(),
    put_s(#een_state{}),
    {ok, nostate}.

handle_cast({msg, LocalId, SenderId, Msg}, nostate) ->
    do_handle_in(LocalId, SenderId, Msg, none).

handle_call({set_ext_bindings, InBindings, OutBindings}, _From, nostate) ->
    set_ext_bindings(InBindings, OutBindings),
    {reply, ok, nostate};
handle_call({reconfig, #een_component_spec{version = removed}, _}, From, nostate) ->
    %% TODO: shutdown / kill all children before this
    een_gen:reply(From, ok),
    {stop, removed, nostate};
handle_call({reconfig, Spec, ChildrenConfig}, _From, nostate) ->
    ok = reconfig(Spec, ChildrenConfig),
    {reply, ok, nostate};
handle_call({msg, LocalId, SenderId, Msg}, From, nostate) ->
    do_handle_in(LocalId, SenderId, Msg, From);
handle_call({nobinds_call, Type}, From, nostate) ->
    Reply =
        case Type of
            multi -> [];
            basic -> {down, nobinds}
        end,
    een_gen:reply(From, {'$nobinds_reply', Reply}),
    {ok, nostate}.

handle_reply(MsgId, {reply, {'$nobinds_reply', Reply}}, nostate) ->
    #een_state{mst = Mst, mod = Mod} = get_s(),
    handle_return(Mod:handle_reply(MsgId, Reply, Mst));
handle_reply(MsgId, Reply, nostate) ->
    do_handle_reply(MsgId, Reply).

handle_parent_exit(Reason, nostate) ->
    NewReason = case Reason of
                    {parent_death, _} -> Reason;
                    _                 -> {parent_death, Reason}
                end,
    {stop, NewReason, nostate}.

handle_child_exit(Pid, removed, nostate) ->
    State = #een_state{map_comps = MCs, map_pid_compid = MPC} = get_s(),
    Id = orddict:fetch(Pid, MPC),
    NewMPC = orddict:erase(Pid, MPC),
    NewMCs = orddict:erase(Id, MCs),
    put_s(State#een_state{map_comps = NewMCs, map_pid_compid = NewMPC}),
    {ok, nostate};
handle_child_exit(_Pid, {shutdown, _}, nostate) ->
    {ok, nostate};
handle_child_exit(Pid, Reason, nostate) ->
    State = #een_state{map_pid_compid = MPC,
                       mod = Mod,
                       mst = Mst,
                       is_spawn = IsSpawn} = get_s(),
    ChildId = orddict:fetch(Pid, MPC),
    NewMPC = orddict:erase(Pid, MPC),
    put_s(State#een_state{map_pid_compid = NewMPC}),
    case {erlang:function_exported(Mod, handle_child_exit, 3),
          catch Mod:handle_child_exit(ChildId, Reason, Mst),
          IsSpawn} of
        {false, _, false} ->
            {stop, {child_exit, ChildId, Reason}, nostate};
        {false, _, true} ->
            een:report("spawn child death: (~p:~p) -> ~p~n", [ChildId, Pid, Reason]),
            ok = erase_spawn_child(ChildId),
            {ok, nostate};
        {true, {stop, Reason, NewMst}, _} ->
            State1 = get_s(),
            put_s(State1#een_state{mst = NewMst}),
            {stop, Reason, nostate};
        {true, {restart, NewMst}, _} ->
            State1 = get_s(),
            put_s(State1#een_state{mst = NewMst}),
            respawn_child(ChildId, true),
            set_all_int_bindings(),
            {ok, nostate};
        {true, {ok, NewMst}, true} ->
            State1 = get_s(),
            put_s(State1#een_state{mst = NewMst}),
            ok = erase_spawn_child(ChildId),
            {ok, nostate}
    end.

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

set_int_bindings() ->
    #een_state{map_comps = MCs,
               spec = #een_component_spec{id = OwnId}} = get_s(),
    #een_component{in_binds = InBinds, out_binds = OutBinds} =
        orddict:fetch(OwnId, MCs),
    set_int_bindings(InBinds, OutBinds).

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
                                 case orddict:find(PortName, InBinds) of
                                     {ok, Found} -> length(Found);
                                     error       -> 0
                                 end,
                                 CurMultiBuf);
                         (_, _, CurMultiBuf) ->
                             CurMultiBuf
                     end, MultiBuf, InSpec),
    put_s(State#een_state{multi_buf = NewMultiBuf}).

reconfig(NewSpec = #een_component_spec{id = Id,
                                       module = NewMod,
                                       args = Args,
                                       version = Version},
         ChildrenConfig = #een_children_config{version = VersionChildren}) ->
    case Version =:= changed orelse VersionChildren =:= changed of
        true  -> drop_all_int_bindings();
        false -> ok
    end,
    case is_reconfig(Version) of
        true ->
            OldState = #een_state{mod = OldMod, mst = OldMst} = get_s(),
            put('$een_component_id', Id),
            {ok, InterfaceSpec = #een_interface_spec{}, NewMst} =
                NewMod:reinit(OldMod, OldMst, Args),
            put_s(OldState#een_state{mod = NewMod,
                                     mst = NewMst,
                                     spec = NewSpec,
                                     if_spec =
                                         build_interface_spec(InterfaceSpec),
                                     ext_in_binds = orddict:new(),
                                     ext_out_binds = orddict:new(),
                                     int_in_binds = orddict:new(),
                                     int_out_binds = orddict:new()});
        false ->
            ok
    end,
    case is_reconfig(VersionChildren) of
        true  -> reconfig_children(ChildrenConfig);
        false -> ok
    end,
    case is_reconfig(Version) orelse is_reconfig(VersionChildren) of
        true  -> set_all_int_bindings();
        false -> ok
    end.

reconfig_children(Config = #een_children_config{is_spawn = IsSpawn,
                                                children = Children,
                                                version = Version,
                                                spawn_init = SpawnInit,
                                                bindings = Bindings}) ->
    State = #een_state{spec = Spec = #een_component_spec{id = Id},
                       map_comps = MCs} = get_s(),
    MCs1 =
        orddict:update(
            Id,
            fun (SelfComponent) ->
                    SelfComponent#een_component{spec = Spec,
                                                in_binds = orddict:new(),
                                                out_binds = orddict:new()}
            end,
            #een_component{spec = Spec, pid = self()}, MCs),
    State1 = State#een_state{map_comps = MCs1,
                             config = Config,
                             map_pid_compid = orddict:new()},
    case IsSpawn of
        false ->
            MCs2 = lists:foldl(fun register_child/2, MCs1, Children),
            put_s(State1#een_state{is_spawn = false, map_comps = MCs2});
        true ->
            put_s(State1#een_state{is_spawn = true,
                                   spawn_binding = find_spawn_binding(Bindings)}),
            case Version of
                new      -> [{ok, _} = register_new_spawn_child() ||
                                _ <- lists:seq(1, SpawnInit)];
                changed -> ok
            end
    end,
    spawn_new_children(),
    register_bindings(),
    send_reconfig_children().

find_spawn_binding([]) ->
    error;
find_spawn_binding([{_, _, spawn} = Binding | _Rest]) ->
    Binding;
find_spawn_binding([_Binding | Rest]) ->
    find_spawn_binding(Rest).

register_bindings() ->
    #een_state{config = #een_children_config{bindings = Bindings},
               spec = #een_component_spec{id = OwnId},
               is_spawn = IsSpawn,
               map_comps = MCs} = get_s(),
    ChildrenIds = lists:filter(fun (ChildId) when ChildId =:= OwnId -> false;
                                   (_)                              -> true
                               end, orddict:fetch_keys(MCs)),
    BindingsToRegister =
        case IsSpawn of
            false ->
                Bindings;
            true ->
                lists:foldl(
                    fun (ThisId, BTR) -> spawn_child_bindings(ThisId) ++ BTR end,
                    [], ChildrenIds)
        end ++
            lists:map(fun (Child) -> {{OwnId, shutdown}, {Child, shutdown}} end,
                      ChildrenIds),
    do_register_bindings(BindingsToRegister).

do_register_bindings(Bindings) ->
    State = #een_state{map_comps = MCs} = get_s(),
    NewMCs = lists:foldl(fun register_binding/2, MCs, Bindings),
    put_s(State#een_state{map_comps = NewMCs}).

register_child({Spec = #een_component_spec{id = Id,
                                           version = Version},
                ChildrenConfig = #een_children_config{version = ChildrenVersion}},
               MCs) ->
    case {Version, ChildrenVersion} of
        {new, new} ->
            orddict:store(Id, #een_component{spec = Spec,
                                             children_config = ChildrenConfig},
                          MCs);
        {removed, _} ->
            orddict:update(
                Id,
                fun (#een_component{pid = Pid}) ->
                        #een_component{
                            spec = #een_component_spec{id = Id,
                                                       version = removed},
                            pid = Pid}
                end,
                MCs);
        {unchanged, unchanged} ->
            orddict:update(
                Id,
                fun (Component = #een_component{spec = OldSpec,
                                                children_config = OldConfig}) ->
                        Component#een_component{
                            spec = OldSpec#een_component_spec{version = unchanged},
                            children_config = OldConfig#een_children_config{version = unchanged},
                            in_binds = orddict:new(),
                            out_binds = orddict:new()}
                end,
                MCs);
        {changed, unchanged} ->
            orddict:update(
                Id,
                fun (Component = #een_component{children_config = OldConfig}) ->
                        Component#een_component{
                            spec = Spec#een_component_spec{version = new},
                            children_config = OldConfig#een_children_config{version = unchanged},
                            in_binds = orddict:new(),
                            out_binds = orddict:new()}
                end,
                MCs);
        {unchanged, changed} ->
            orddict:update(
                Id,
                fun (Component = #een_component{spec = OldSpec}) ->
                        Component#een_component{
                            spec = OldSpec#een_component_spec{version = unchanged},
                            children_config =
                                ChildrenConfig#een_children_config{
                                    version = new},
                            in_binds = orddict:new(),
                            out_binds = orddict:new()}
                end,
                MCs);
        {changed, changed} ->
            orddict:update(
                Id,
                fun (Component) ->
                        Component#een_component{
                            spec = Spec#een_component_spec{version = new},
                            children_config =
                                ChildrenConfig#een_children_config{
                                    version = new},
                            in_binds = orddict:new(),
                            out_binds = orddict:new()}
                end,
                MCs)
    end.

register_new_spawn_child() ->
    State = #een_state{config = #een_children_config{
                                    children =
                                        [{Spec = #een_component_spec{id = Id},
                                          ChildrenConfig}],
                                    spawn_max = SpawnMax},
                       spawn_index = SpawnIndex,
                       spawn_current = SpawnCurrent,
                       map_comps = MCs} = get_s(),
    if
        SpawnCurrent >= SpawnMax ->
            too_many;
        true ->
            ThisId = list_to_atom(atom_to_list(Id) ++ "_" ++ integer_to_list(SpawnIndex)),
            ThisChildrenConfig = replace_id_in_children_config(ThisId, Id, ChildrenConfig),
            NewMCs = register_child({Spec#een_component_spec{id = ThisId}, ThisChildrenConfig}, MCs),
            put_s(State#een_state{map_comps = NewMCs,
                                  spawn_index = SpawnIndex + 1,
                                  spawn_current = SpawnCurrent + 1}),
            {ok, ThisId}
    end.

spawn_new_children() ->
    State = get_s(),
    lists:foreach(fun (Id) -> respawn_child(Id, false) end,
                  orddict:fetch_keys(State#een_state.map_comps)).

spawn_new_spawn_child() ->
    een:report("Spawn binding triggered~n", []),
    {ok, ThisId} = register_new_spawn_child(),
    {ok, Pid} = respawn_child(ThisId, false),
    reconfig_new_spawn_child(ThisId),
    do_register_bindings(spawn_child_bindings(ThisId)),
    send_new_spawn_child_bindings(ThisId),
    set_int_bindings(),
    {ok, ThisId, Pid}.

reconfig_new_spawn_child(ThisId) ->
    #een_state{map_comps = MCs} = get_s(),
    #een_component{spec = Spec,
                   children_config = ChildrenConfig,
                   pid = Pid} = orddict:fetch(ThisId, MCs),
    ok = een_config:reconfig(Pid, Spec, ChildrenConfig).

send_new_spawn_child_bindings(ThisId) ->
    #een_state{map_comps = MCs} = get_s(),
    #een_component{in_binds = InBinds,
                   out_binds = OutBinds,
                   pid = Pid} = orddict:fetch(ThisId, MCs),
    ok = een_config:set_ext_bindings(Pid, InBinds, OutBinds).

spawn_child_bindings(ThisId) ->
    #een_state{spec = #een_component_spec{id = OwnId},
               config = #een_children_config{
                            children = [{#een_component_spec{id = Id}, _}],
                            bindings = Bindings}} = get_s(),
    replace_id_in_bindings(ThisId, Id, Bindings) ++
        [{{OwnId, shutdown}, {ThisId, shutdown}}].

erase_spawn_child(ChildId) ->
    een:report("Erasing spawn child ~p because it died~n", [ChildId]),
    State = #een_state{map_comps = MCs,
                       config = #een_children_config{spawn_min = SpawnMin},
                       spawn_current = SpawnCurrent} = get_s(),
    NewMCs = orddict:erase(ChildId, MCs),
    put_s(State#een_state{map_comps = NewMCs,
                          spawn_current = SpawnCurrent - 1}),
    if
        SpawnCurrent =< SpawnMin -> {too_few_children, SpawnCurrent - 1};
        true                     -> ok
    end.

replace_id_in_children_config(
        NewId, OldId,
        ChildrenConfig = #een_children_config{bindings = Bindings}) ->
    ChildrenConfig#een_children_config{
        bindings = replace_id_in_bindings(NewId, OldId, Bindings)}.

replace_id_in_bindings(NewId, OldId, Bindings) ->
    lists:map(fun (Binding) -> replace_id_in_binding(NewId, OldId, Binding) end,
              Bindings).

replace_id_in_binding(NewId, OldId, {From, To, Type}) ->
    {replace_id_in_port(NewId, OldId, From), replace_id_in_port(NewId, OldId, To),
     Type};
replace_id_in_binding(NewId, OldId, {From, To}) ->
    {replace_id_in_port(NewId, OldId, From), replace_id_in_port(NewId, OldId, To)};
replace_id_in_binding(_, _, undefined) ->
    undefined.

replace_id_in_port(NewId, OldId, {OldId, PortName}) ->
    {NewId, PortName};
replace_id_in_port(_, _, Ret) ->
    Ret.

respawn_child(Id, Restart) ->
    State = #een_state{map_comps = MCs,
                       map_pid_compid = MPC,
                       spec = #een_component_spec{id = OwnId}} = get_s(),
    case Id of
        OwnId ->
            {ok, self()};
        _ ->
            %% TODO: handle failures
            Component =
                #een_component{pid = OldPid,
                               spec = Spec,
                               children_config = ChildrenConfig} =
                orddict:fetch(Id, MCs),
            NewComponent = #een_component{pid = NewPid} =
                case Restart of
                    false when is_pid(OldPid) ->
                        Component;
                    false when OldPid =:= undefined ->
                        {ok, Pid} = een_config:respawn_child(Spec),
                        Component#een_component{pid = Pid};
                    true when is_pid(OldPid) ->
                        {ok, Pid} = een_config:respawn_child(Spec),
                        %% IMPORTANT TODO: replace all bindings containing old pid !!!
                        ok = een_config:reconfig(Pid, Spec, ChildrenConfig),
                        Component#een_component{pid = Pid}
                end,
            NewMCs = orddict:store(Id, NewComponent, MCs),
            MPC1 = orddict:erase(OldPid, MPC),
            MPC2 = orddict:store(NewPid, Id, MPC1),
            put_s(State#een_state{map_comps = NewMCs, map_pid_compid = MPC2}),
            {ok, NewPid}
    end.

register_binding({From, To}, MCs) ->
    register_binding1(From, To, basic, MCs);
register_binding({From, To, Type}, MCs) ->
    register_binding1(From, To, Type, MCs).

register_binding1({Id1, Port1}, {Id2, Port2}, Type, MCs) ->
    %% TODO: check validity
    #een_component{pid = Pid1, out_binds = OutBinds0} = orddict:fetch(Id1, MCs),
    #een_component{pid = Pid2, in_binds = InBinds0} = orddict:fetch(Id2, MCs),
    Entry2 = {Id2, Pid2, Port2},
    OutBinds1 =
        case Type of
            {route, Route} ->
                orddict:update(Port1,
                               fun ({route, PortList, Route1}) when Route =:= Route1 ->
                                       {route, [Entry2 | PortList], Route}
                               end, {route, [Entry2], Route}, OutBinds0);
            _ ->
                orddict:update(Port1,
                               fun (PortList) -> [Entry2 | PortList] end,
                               [Entry2], OutBinds0)
        end,
    Entry1 = {Id1, Pid1, Port1},
    InBinds1 =
        orddict:update(Port2,
                       fun (PortList) -> [Entry1 | PortList] end, [Entry1],
                       InBinds0),
    MCs1 = orddict:update(
               Id1, fun (C) -> C#een_component{out_binds = OutBinds1} end, MCs),
    MCs2 = orddict:update(
               Id2, fun (C) -> C#een_component{in_binds = InBinds1} end, MCs1),
    MCs2.

set_all_int_bindings() ->
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
            (_, #een_component{spec = #een_component_spec{version = removed}},
                    ok) ->
                ok;
            (_, #een_component{pid = Pid,
                               in_binds = InBinds,
                               out_binds = OutBinds}, _) ->
                ok = een_config:set_ext_bindings(Pid, InBinds, OutBinds),
                ok
        end, ok, MCs).

drop_all_int_bindings() ->
    State = #een_state{map_comps = MCs,
                       spec = #een_component_spec{id = SelfId}} = get_s(),
    %% TODO drop only int multi buf / reply buf
    put_s(State#een_state{multi_buf = een_multi_buffer:new()}),
    een_multi_reply_buffer:new(),
    %% TODO: parallelize
    orddict:fold(
        fun (Id, _, ok) when Id =:= SelfId ->
                set_int_bindings(orddict:new(), orddict:new()),
                ok;
            (_, #een_component{spec = #een_component_spec{version = removed}},
                    ok) ->
                ok;
            (_, #een_component{pid = Pid}, _) ->
                ok = een_config:set_ext_bindings(Pid, orddict:new(), orddict:new()),
                ok
        end, ok, MCs).

send_reconfig_children() ->
    #een_state{map_comps = MCs,
               spec = #een_component_spec{id = SelfId}} = get_s(),
    %% TODO: parallelize
    %% TODO: handle failures
    orddict:fold(
        fun (Id, _, ok) when Id =:= SelfId ->
                ok;
            (_Id, #een_component{pid = Pid,
                                 spec = Spec,
                                 children_config = ChildrenConfig}, _) ->
                ok = een_config:reconfig(Pid, Spec, ChildrenConfig),
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
    {stop, Reason, nostate};
handle_return({shutdown, Reason, NewMst}) ->
    handle_return({stop, {shutdown, Reason}, NewMst}).

merge_orddict(Dict1, Dict2) ->
    orddict:merge(fun (K, _, _) -> throw({merge_conflict, K}) end, Dict1, Dict2).

is_reconfig(new)       -> true;
is_reconfig(changed)   -> true;
is_reconfig(unchanged) -> false.
