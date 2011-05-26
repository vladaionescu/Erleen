
-module(een).

-export([spawn_config/1, set_children_config/2, out/2, reply/2, is_of/3]).

%% ----------------------------------------------------------------------------
%% Interface
%% ----------------------------------------------------------------------------

spawn_config({Id, _Type, {M, F, A}, _Node, ChildrenConfig} = Top) ->
    %% TODO: handle failures
    %% TODO: type?
    %% TODO: Node
    io:format("Spawning config with top ~p~n", [Top]),
    {ok, Pid} = apply(M, F, A),
    io:format("Done spawning top ~p : ~p~n", [Id, Pid]),
    ok = set_children_config(Pid, ChildrenConfig),
    {ok, Pid}.

set_children_config(Pid, ChildrenConfig) ->
    een_gen:call(Pid, {set_children_config, ChildrenConfig}).

out(LocalIfId, Msg) ->
    een_comp:out(LocalIfId, Msg).

reply(From, Msg) ->
    een_comp:reply(From, Msg).

is_of(_, _, []) ->
    false;
is_of(IfId, {Type, Args}, [{IfId, {Type, Arrity}} | _])
        when size(Args) =:= Arrity->
    true;
is_of(IfId, Msg, [_ | Rest]) ->
    is_of(IfId, Msg, Rest).
