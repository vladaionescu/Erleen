
-module(een_coord).

-behaviour(een_comp).

-export([behaviour_info/1]).
-export([start/2]).
-export([reinit/3, in_if/1, out_if/1, handle_children_config/2,
         handle_child_exit/3, shutdown_children/2, handle_in/4, handle_reply/3,
         terminate/2]).

-record(state, {id = een_coord,
                comp_id,
                comp_type,
                mod,
                mst,
                map_comps,
                map_pid_compid}).

-record(component, {id,
                    pid,
                    mfa,
                    children_config,
                    in_binds = [],
                    out_binds = []}).

%% ----------------------------------------------------------------------------
%% Interface
%% ----------------------------------------------------------------------------

start(Module, Args) ->
    Props = get('$een_child_props'),
    een_comp:start(?MODULE, [Module, Args, Props]).

%% ----------------------------------------------------------------------------
%% Behaviour spec
%% ----------------------------------------------------------------------------

behaviour_info(callback) ->
    [
        %% (OldModule, OldState, Args) -> {ok, State} | {error, Error}
        {reinit, 3},

        %% (State) -> IfList
        {ext_in_if, 1},
        %% (State) -> IfList
        {ext_out_if, 1},

        %% (State) -> IfList
        {int_in_if, 1},
        %% (State) -> IfList
        {int_out_if, 1},

        %% (IfId, Msg, From, State) -> HandleReturn
        {handle_in, 4},

        %% (MsgId, Reply, State) -> HandleReturn
        {handle_reply, 3},

        %% (Reason, State) -> NewReason
        {terminate, 2}
    ];
behaviour_info(_) ->
    undefined.

%% ----------------------------------------------------------------------------
%% Comp callbacks
%% ----------------------------------------------------------------------------

reinit(_OldModule, _OldState, [Mod, Args, {Id, Type}]) ->
    case Mod:reinit(none, none, Args) of
        {ok, Mst0}     -> {ok, #state{mod = Mod,
                                      mst = Mst0,
                                      comp_id = Id,
                                      comp_type = Type}};
        {error, _} = E -> E
    end.

in_if(#state{mod = Mod, mst = Mst}) ->
    Mod:int_in_if(Mst) ++ Mod:ext_in_if(Mst).

out_if(#state{mod = Mod, mst = Mst}) ->
    Mod:int_out_if(Mst) ++ Mod:ext_out_if(Mst).

handle_in(IfId, Msg, From, State = #state{mod = Mod, mst = Mst}) ->
    handle_return(Mod:handle_in(IfId, Msg, From, Mst), State).

handle_reply(MsgId, Reply, State = #state{mod = Mod, mst = Mst}) ->
    handle_return(Mod:handle_reply(MsgId, Reply, Mst), State).

handle_child_exit(_Child, _Reason, State) ->
    %% TODO
    {ok, State}.

handle_children_config({Comps, Bindings}, State = #state{comp_id = CompId}) ->
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
    {ok, State1} = spawn_children(Comps, State#state{map_comps = MCs,
                                                     map_pid_compid = MPC}),
    {ok, State2} = generate_pid_bindings(Bindings, State1),
    send_bindings_and_children_config(State2).

shutdown_children(_Reason, _State) ->
    todo.

terminate(Reason, #state{mod = Mod, mst = Mst}) ->
    Mod:terminate(Reason, Mst).

%% ----------------------------------------------------------------------------
%% Internal
%% ----------------------------------------------------------------------------

spawn_children([], State) ->
    {ok, State};
spawn_children([{Id, Type, MFA = {M, F, A}, Node, ChildrenConfig} = Config | Rest],
               State = #state{map_pid_compid = MPC, map_comps = MCs}) ->
    %% TODO: link here
    %% TODO: handle failures
    %% TODO: parallelize
    %% TODO: type?
    %% TODO: Node
    io:format("Spawning child of ~p with config ~p~n", [self(), Config]),
    put('$een_child_node', Node),
    put('$een_child_props', {Id, Type}),
    {ok, Pid} = apply(M, F, A),
    put('$een_child_node', undefined),
    put('$een_child_props', undefined),
    io:format("Done spawning child of ~p. ~p : ~p~n", [self(), Id, Pid]),
    link(Pid),
    MPC1 = orddict:store(Pid, Id, MPC),
    MCs1 = orddict:store(Id, #component{id = Id,
                                        pid = Pid,
                                        mfa = MFA,
                                        children_config = ChildrenConfig},
                         MCs),
    spawn_children(Rest, State#state{map_pid_compid = MPC1, map_comps = MCs1}).

generate_pid_bindings([], State) ->
    {ok, State};
generate_pid_bindings([{Id1, IfId1, Id2, IfId2} | Rest],
                      State = #state{map_comps = MCs}) ->
    #component{pid = Pid1, out_binds = OutBinds0} = orddict:fetch(Id1, MCs),
    #component{pid = Pid2, in_binds = InBinds0} = orddict:fetch(Id2, MCs),
    OutBinds = [{IfId1, {Pid2, IfId2}} | OutBinds0],
    InBinds = [{IfId2, {Pid1, IfId1}} | InBinds0],
    MCs1 = orddict:update(
               Id1, fun (C) -> C#component{out_binds = OutBinds} end, MCs),
    MCs2 = orddict:update(
               Id2, fun (C) -> C#component{in_binds = InBinds} end, MCs1),
    generate_pid_bindings(Rest, State#state{map_comps = MCs2}).

send_bindings_and_children_config(State = #state{comp_id = CompId1,
                                                 map_comps = MCs}) ->
    %% TODO: parallelize
    %% TODO: handle failures
    orddict:fold(
        fun (_, #component{id = CompId2, out_binds = OutBinds}, ok)
                    when CompId1 =:= CompId2 ->
                ok = een_out:set(OutBinds, int);
            (_, #component{pid = Pid,
                           in_binds = InBinds,
                           out_binds = OutBinds,
                           children_config = ChildrenConfig}, ok) ->
                ok = een_gen:call(Pid, {set_bindings, InBinds, OutBinds}),
                ok = een_gen:call(Pid, {set_children_config, ChildrenConfig})
        end, ok, MCs),
    {reply, ok, State}.

handle_return({ok, NewMst}, State) ->
    {ok, State#state{mst = NewMst}};
handle_return({reply, Reply, NewMst}, State) ->
    {reply, Reply, State#state{mst = NewMst}};
handle_return({stop, Reason, NewMst}, State) ->
    {stop, Reason, State#state{mst = NewMst}}.
