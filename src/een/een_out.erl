
-module(een_out).

-export([send/2, set/2]).

-include_lib("erleen.hrl").

send(PortName, Msg) ->
    OutFun = get('$een_out_fun'),
    OutFun(PortName, Msg).

set(Bindings, PortSpecs) ->
    een:report("Setting bindings ~p~n", [Bindings]),
    OutFun =
        fun (PortName, Msg) ->
                case {orddict:find(PortName, PortSpecs),
                      orddict:find(PortName, Bindings)} of
                    {error, _} ->
                        throw({invalid_port, PortName});
                    {{ok, #een_port_spec{msg_type = cast}}, error} ->
                        ok;
                    {{ok, #een_port_spec{msg_type = call}}, error} ->
                        nobinds;
                    {{ok, #een_port_spec{type = Type, msg_type = MsgType}},
                     {ok, DestList}} ->
                        SenderId = {get('$een_compomnent_id'), PortName},
                        Dest = case Type of
                                   basic -> random_pick(DestList);
                                   multi -> todo;
                                   route -> todo
                               end,
                        do_send(PortName, SenderId, MsgType, Dest, Msg)
                end
        end,
    put('$een_out_fun', OutFun),
    ok.

do_send(PortName, SenderId, MsgType, {Pid, RemotePortName}, Msg) ->
    io:format("msg ~p from (~p:~p:~p) to (?:~p:~p)~n", [Msg, get('$een_component_id'), self(), PortName, Pid, RemotePortName]),
    case MsgType of
        cast -> een_gen:cast(Pid, {msg, RemotePortName, SenderId, Msg});
        call -> {ok, een_gen:async_call(Pid, {msg, RemotePortName, SenderId, Msg})}
    end.

random_pick(List) ->
    lists:nth(random:uniform(length(List)), List).
