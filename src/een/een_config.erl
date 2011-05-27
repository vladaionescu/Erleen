
-module(een_config).

-export([spawn/1, spawn_children/1, set_children_config/2, set_bindings/3]).

%% ----------------------------------------------------------------------------
%% Interface
%% ----------------------------------------------------------------------------

spawn(Config = {_, _, _, _, ChildrenConfig}) ->
    put('$een_children', ordsets:new()),
    {ok, Pid} = spawn_child(Config),
    put('$een_children', undefined),
    ok = set_children_config(Pid, ChildrenConfig),
    {ok, Pid}.

spawn_child({Id, Type, {M, F, A}, Node, _ChildrenConfig}) ->
    %% TODO: type ?
    put('$een_child_node', Node),
    put('$een_child_props', {Id, Type}),
    {ok, Pid} = apply(M, F, A),
    put('$een_child_node', undefined),
    put('$een_child_props', undefined),
    io:format("~p: Spawned child (~p : ~p) on node ~p.~n", [self(), Id, Pid, Node]),
    {ok, Pid}.

set_children_config(Pid, ChildrenConfig) ->
    een_gen:call(Pid, {set_children_config, ChildrenConfig}).

set_bindings(Pid, InBinds, OutBinds) ->
    een_gen:call(Pid, {set_bindings, InBinds, OutBinds}).

spawn_children(ChildConfigs) ->
    %% TODO: handle failures
    %% TODO: parallelize
    lists:map(fun (Config) ->
                      {ok, Pid} = spawn_child(Config),
                      {Pid, Config}
              end, ChildConfigs).