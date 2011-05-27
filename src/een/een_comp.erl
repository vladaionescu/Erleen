
-module(een_comp).

-behaviour(een_gen).

-export([behaviour_info/1]).
-export([start/2]).
-export([reinit/3, handle_cast/2, handle_call/3, handle_reply/3, terminate/2,
         handle_child_exit/3, handle_parent_exit/2]).

-include_lib("erleen.hrl").

-record(state, {id = een_comp,
                mod,
                mst,
                if_spec,
                comp_id,
                comp_type,
                map_comps,
                map_pid_compid}).

-record(component, {id,
                    pid,
                    mfa,
                    children_config,
                    in_binds = orddict:new(),
                    out_binds = orddict:new()}).

%% ----------------------------------------------------------------------------
%% Interface
%% ----------------------------------------------------------------------------

start(Module, Args) ->
    Props = get('$een_child_props'),
    een_gen:start_child(?MODULE, [Module, Args, Props]).

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

reinit(_OldModule, _OldState, [Mod, Args, {Id, Type}]) ->
    een_out:reset(),
    case Mod:reinit(none, none, Args) of
        {ok, InterfaceSpec = #een_interface_spec{}, Mst0} ->
            {ok, #state{mod = Mod,
                        mst = Mst0,
                        comp_id = Id,
                        comp_type = Type,
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

%% TODO: 'DOWN' ?
handle_reply(MsgId, Reply, State = #state{mod = Mod, mst = Mst}) ->
    handle_return(Mod:handle_reply(MsgId, Reply, Mst), State).

handle_parent_exit(Reason, State) ->
    NewReason = case Reason of
                    {parent_death, _} -> Reason;
                    _                 -> {parent_death, Reason}
                end,
    {stop, NewReason, State}.

handle_child_exit(Child, Reason, State = #state{mod = Mod, mst = Mst}) ->
    handle_return(Mod:handle_child_exit(Child, Reason, Mst), State).

terminate(Reason, #state{mod = Mod, mst = Mst}) ->
    Mod:terminate(Reason, Mst).

%% ----------------------------------------------------------------------------
%% Internal
%% ----------------------------------------------------------------------------

handle_children_config(Config = #een_children_config{children = Children,
                                                     bindings = Bindings},
                       State = #state{comp_id = CompId}) ->
    io:format("~p: Setting children config ~p~n", [self(), Config]),
    SpawnedChildren = een_config:spawn_children(Children),
    MCs =
        orddict:store(
            CompId,
            #component{id = CompId,
                       pid = self(),
                       mfa = none,
                       children_config = none,
                       in_binds = [],
                       out_binds = []},
            orddict:new()),
    MPC = orddict:store(self(), CompId, orddict:new()),
    State1 =
        lists:foldl(fun register_child/2, State#state{map_comps = MCs,
                                                      map_pid_compid = MPC},
                    SpawnedChildren),
    State2 = lists:foldl(fun register_binding/2, State1, Bindings),
    send_bindings_and_children_config(State2).

register_child({Pid, #een_component_spec{id = Id,
                                         mfa = MFA,
                                         children_config = ChildrenConfig}},
               State = #state{map_comps = MCs, map_pid_compid = MPC}) ->
    MPC1 = orddict:store(Pid, Id, MPC),
    MCs1 = orddict:store(Id, #component{id = Id,
                                        pid = Pid,
                                        mfa = MFA,
                                        children_config = ChildrenConfig},
                         MCs),
    State#state{map_pid_compid = MPC1, map_comps = MCs1}.

register_binding(#een_binding{from = #een_port{comp_id = Id1,
                                               port_name = Port1},
                              to   = #een_port{comp_id = Id2,
                                               port_name = Port2}},
                 State = #state{map_comps = MCs}) ->
    %% TODO: check validity
    #component{pid = Pid1, out_binds = OutBinds0} = orddict:fetch(Id1, MCs),
    #component{pid = Pid2, in_binds = InBinds0} = orddict:fetch(Id2, MCs),
    OutBinds = [{Port1, {Pid2, Port2}} | OutBinds0],
    InBinds = [{Port2, {Pid1, Port1}} | InBinds0],
    MCs1 = orddict:update(
               Id1, fun (C) -> C#component{out_binds = OutBinds} end, MCs),
    MCs2 = orddict:update(
               Id2, fun (C) -> C#component{in_binds = InBinds} end, MCs1),
    State#state{map_comps = MCs2}.

send_bindings_and_children_config(State = #state{comp_id = CompId1,
                                                 map_comps = MCs,
                                                 if_spec = IfSpec}) ->
    %% TODO: parallelize
    %% TODO: handle failures
    orddict:fold(
        fun (_, #component{id = CompId2, out_binds = OutBinds}, ok)
                    when CompId1 =:= CompId2 ->
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
