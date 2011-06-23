
-module(een_out).

-export([send/2, set/2]).

-include_lib("erleen.hrl").

send(PortName, Msg) ->
    OutFun = get('$een_out_fun'),
    OutFun(PortName, Msg).

set(Bindings, PortSpecs) ->
    een:report("Setting bindings ~p~n", [Bindings]),
    State = #een_state{config = #een_children_config{routes = Routes}} =
        een_comp:get_s(),
    {SpawnPort, DestSpawnPort} =
        case State of
            #een_state{is_spawn = true,
                       spawn_binding = {{_, SP}, {_, DestSP}, spawn}} ->
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
                            {{ok, #een_port_spec{msg_type = call,
                                                 type = Type}}, error} ->
                                {ok, een_gen:async_call(self(),
                                                        {nobinds_call, Type})};
                            {{ok, #een_port_spec{type = Type, msg_type = MsgType}},
                             {ok, Dests}} ->
                                SenderId = {get('$een_component_id'), PortName},
                                Dest =
                                    case {Type, Dests} of
                                         {basic, _} when is_list(Dests) ->
                                             random_pick(Dests);
                                         {multi, _} when is_list(Dests) ->
                                            Dests;
                                         {basic, {route, DestList, Route}} ->
                                            random_pick(route(Msg, DestList, Route, Routes));
                                         {multi, {route, DestList, Route}} ->
                                            route(Msg, DestList, Route, Routes)
                                    end,
                                do_send(PortName, SenderId, MsgType, Dest, Msg)
                        end
                end
        end,
    put('$een_out_fun', OutFun),
    ok.

do_send(_PortName, SenderId, MsgType, {_CompId, Pid, RemotePortName}, Msg) ->
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

route(Msg, DestList, Route, Routes) ->
    RouteFun = orddict:fetch(Route, Routes),
    RouteFun(Msg, DestList).
