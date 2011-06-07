
-module(een_config).

-export([spawn/1, respawn_child/1, reconfig/3, set_ext_bindings/3]).

-include_lib("erleen.hrl").

%% ----------------------------------------------------------------------------
%% Interface
%% ----------------------------------------------------------------------------

spawn({Spec = #een_component_spec{}, ChildrenConfig = #een_children_config{}}) ->
    put('$een_children', ordsets:new()),
    {ok, Pid} = respawn_child(Spec),
    put('$een_children', undefined),
    reconfig(Pid, Spec, ChildrenConfig),
    {ok, Pid}.

respawn_child(#een_component_spec{id = Id, node = Node}) ->
    %% TODO: type ?
    put('$een_child_node', Node),
    {ok, Pid} = een_comp:start(),
    put('$een_child_node', undefined),
    een:report("(Re)spawned child (~p:~p) on node ~p~n", [Id, Pid, Node]),
    {ok, Pid}.

reconfig(Pid, Spec, ChildrenConfig) ->
    een_gen:call(Pid, {reconfig, Spec, ChildrenConfig}).

set_ext_bindings(Pid, InBinds, OutBinds) ->
    een_gen:call(Pid, {set_ext_bindings, InBinds, OutBinds}).
