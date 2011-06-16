
-module(een_out).

-export([send/2, set/2]).

-include_lib("erleen.hrl").

send(PortName, Msg) ->
    OutFun = get('$een_out_fun'),
    OutFun(PortName, Msg).

set(Bindings, PortSpecs) ->
    een:report("Setting bindings ~p~n", [Bindings]),
    {SpawnPort, DestSpawnPort} =
        case een_comp:get_s() of
            #een_state{is_spawn = true,
                       spawn_binding = {{_, SP}, {_, DestSP}}} ->
                {SP, DestSP};
            #een_state{is_spawn = false} ->
                {undefined, undefined}
        end,
    OutFun =
        fun (PortName, Msg) ->
                case PortName of
                    SpawnPort ->
                        {ok, DestCompId, DestPid} =
                            een_comp:spawn_new_spawn_child(),
                        case orddict:find(PortName, PortSpecs) of
                            error ->
                                throw({invalid_port, PortName});
                            {ok, #een_port_spec{type = basic, msg_type = MsgType}} ->
                                SenderId = {get('$een_component_id'), PortName},
                                Dest = {DestCompId, DestPid, DestSpawnPort},
                                do_send(PortName, SenderId, MsgType, Dest, Msg)
                        end;
                    _ ->
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
                                SenderId = {get('$een_component_id'), PortName},
                                Dest = case Type of
                                           basic -> random_pick(DestList);
                                           multi -> DestList;
                                           route -> todo
                                       end,
                                do_send(PortName, SenderId, MsgType, Dest, Msg)
                        end
                end
        end,
    put('$een_out_fun', OutFun),
    ok.

do_send(PortName, SenderId, MsgType, {CompId, Pid, RemotePortName}, Msg) ->
    %io:format("msg ~p from (~p:~p:~p) to (~p:~p:~p)~n", [Msg, get('$een_component_id'), self(), PortName, CompId, Pid, RemotePortName]),
    case MsgType of
        cast -> een_gen:cast(Pid, {msg, RemotePortName, SenderId, Msg});
        call -> {ok, een_gen:async_call(Pid, {msg, RemotePortName, SenderId, Msg})}
    end;
do_send(PortName, SenderId, MsgType, DestList, Msg) ->
    SendList =
        lists:foldl(
            fun (Dest, Acc) ->
                    Send = do_send(PortName, SenderId, MsgType, Dest, Msg),
                    case {MsgType, Send} of
                        {cast, ok}          -> ok;
                        {call, {ok, MsgId}} -> [MsgId | Acc]
                    end
            end, [], DestList),
    case MsgType of
        cast -> ok;
        call -> {ok, een_multi_reply_buffer:new_call(SendList)}
    end.

random_pick(List) ->
    lists:nth(random:uniform(length(List)), List).
