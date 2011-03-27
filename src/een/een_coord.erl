
-module(een_coord).

-behaviour(een_comp).

-export([behaviour_info/1]).
-export([start/2, out/2, reply/2]).
-export([reinit/3, in_if/1, out_if/1, handle_children_config/2,
         shutdown_children/2, handle_in/4, handle_reply/3, terminate/2]).

-record(state, {id = een_coord,
                mod,
                mst,
                map_comps = orddict:new(),
                map_pid_compid = orddict:new()}).

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
    een_comp:start(?MODULE, [Module, Args]).

out(LocalIfId, Msg) ->
    een_comp:out(LocalIfId, Msg).

reply(From, Msg) ->
    een_comp:reply(From, Msg).

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

reinit(_OldModule, _OldState, [Mod, Args]) ->
    case Mod:reinit(none, none, Args) of
        {ok, Mst0}     -> {ok, #state{mod = Mod, mst = Mst0}};
        {error, _} = E -> E
    end.

in_if(#state{mod = Mod, mst = Mst}) ->
    Mod:in_if(Mst).

out_if(#state{mod = Mod, mst = Mst}) ->
    Mod:out_if(Mst).

handle_in(IfId, Msg, From, #state{mod = Mod, mst = Mst}) ->
    Mod:handle_in(IfId, Msg, From, Mst).

handle_reply(MsgId, Reply, #state{mod = Mod, mst = Mst}) ->
    Mod:handle_reply(MsgId, Reply, Mst).

handle_children_config({Comps, Bindings}, State) ->
    {ok, State1} = spawn_children(Comps, State),
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
spawn_children([{Id, _Type, MFA = {M, F, A}, _Node, ChildrenConfig} | Rest],
               State = #state{map_pid_compid = MPC, map_comps = MCs}) ->
    %% TODO: link here
    %% TODO: handle failures
    %% TODO: parallelize
    %% TODO: type?
    %% TODO: Node
    {ok, Pid} = apply(M, F, A),
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
    OutBinds = [{IfId1, Pid2, IfId2} | OutBinds0],
    InBinds = [{IfId2, Pid1, IfId1} | InBinds0],
    MCs1 = orddict:update(
               Id1, fun (C) -> C#component{out_binds = OutBinds} end, MCs),
    MCs2 = orddict:update(
               Id2, fun (C) -> C#component{in_binds = InBinds} end, MCs1),
    generate_pid_bindings(Rest, State#state{map_comps = MCs2}).

send_bindings_and_children_config(State = #state{map_comps = MCs}) ->
    %% TODO: parallelize
    %% TODO: handle failures
    orddict:fold(
        fun (_, #component{pid = Pid,
                           in_binds = InBinds,
                           out_binds = OutBinds,
                           children_config = ChildrenConfig},
             ok) ->
                ok = een_gen:call(Pid, {set_bindings, InBinds, OutBinds}),
                ok = een_gen:call(Pid, {set_children_config, ChildrenConfig})
        end, ok, MCs),
    {ok, State}.
