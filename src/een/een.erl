
-module(een).

-export([spawn_config/1, reconfig/2, out/2, reply/2, report/2]).

%% ----------------------------------------------------------------------------
%% Interface
%% ----------------------------------------------------------------------------

spawn_config(Config) ->
    een_config:spawn(Config).

reconfig(Pid, {Spec, ChildrenConfig}) ->
    io:format("Reconfiguring...~n"),
    een_config:reconfig(Pid, Spec, ChildrenConfig).

out(PortName, Msg) ->
    een_out:send(PortName, Msg).

reply(FromList, Msg) when is_list(FromList) ->
    lists:foreach(fun (From) -> reply(From, Msg) end, FromList);
reply(From, Msg) ->
    een_gen:reply(From, Msg).

report(Format, Args) ->
    io:format("(~p:~p) $ " ++ Format, [get('$een_component_id'), self()] ++ Args).
