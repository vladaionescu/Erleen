
-module(een_config).

-export([spawn/1, respawn_child/3, set_children_config/2, set_ext_bindings/3]).

-include_lib("erleen.hrl").

%% ----------------------------------------------------------------------------
%% Interface
%% ----------------------------------------------------------------------------

spawn({Spec = #een_component_spec{}, ChildrenConfig}) ->
    put('$een_children', ordsets:new()),
    {ok, Pid} = respawn_child(Spec, none, none),
    put('$een_children', undefined),
    ok = set_children_config(Pid, ChildrenConfig),
    {ok, Pid}.

respawn_child(Spec = #een_component_spec{id = Id,
                                         module = Module,
                                         args = Args,
                                         node = Node},
              OldModule, OldState) ->
    %% TODO: type ?
    put('$een_child_node', Node),
    put('$een_child_component_spec', Spec),
    {ok, Pid} = een_comp:start(OldModule, OldState, Module, Args),
    put('$een_child_node', undefined),
    put('$een_child_component_spec', undefined),
    een:report("(Re)spawned child (~p:~p) on node ~p~n", [Id, Pid, Node]),
    {ok, Pid}.

set_children_config(Pid, ChildrenConfig) ->
    een_gen:call(Pid, {set_children_config, ChildrenConfig}).

set_ext_bindings(Pid, InBinds, OutBinds) ->
    een_gen:call(Pid, {set_ext_bindings, InBinds, OutBinds}).
