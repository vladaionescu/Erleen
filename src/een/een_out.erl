
-module(een_out).

-export([send/2, set/3, reset/0]).

-include_lib("erleen.hrl").

%% ----------------------------------------------------------------------------
%% Internal interface
%% ----------------------------------------------------------------------------

send(PortName, Msg) ->
    OutFun = get('$een_out_fun'),
    OutFun(PortName, Msg).

%% TODO: make this a single out function (not two) - by merging int and ext bindings (shouldn't be hard)
set(Bindings, IfSpec, Loc) ->
    een:report("Setting ~p bindings ~p~n", [Loc, Bindings]),
    BindingsDict = orddict:from_list(Bindings),
    {Key, PortSpecs} =
        case Loc of
            int -> {'$een_int_out_fun', IfSpec#een_interface_spec.int_out};
            ext -> {'$een_ext_out_fun', IfSpec#een_interface_spec.ext_out}
        end,
    %% TODO: optimize
    OutFun =
        fun (PortName, Msg) ->
                case orddict:find(PortName, PortSpecs) of
                    {ok, #een_port_spec{type = Type, msg_type = MsgType}} ->
                        case orddict:find(PortName, BindingsDict) of
                            {ok, DestList} ->
                                SenderId = {get('$een_component_id'), PortName},
                                Dest = case Type of
                                           basic -> random_pick(DestList);
                                           multi -> todo;
                                           route -> todo
                                       end,
                                do_send(PortName, SenderId, MsgType, Dest, Msg)
                        end;
                    error ->
                        throw({invalid_port, PortName})
                end
        end,
    put(Key, OutFun),
    join().

do_send(PortName, SenderId, cast, {Pid, RemotePortName}, Msg) ->
    io:format("msg ~p from (~p:~p:~p) to (?:~p:~p)~n", [Msg, get('$een_component_id'), self(), PortName, Pid, RemotePortName]),
    een_gen:cast(Pid, {msg, RemotePortName, SenderId, Msg});
do_send(PortName, SenderId, call, {Pid, RemotePortName}, Msg) ->
    io:format("msg ~p from (~p:~p:~p) to (?:~p:~p)~n", [Msg, get('$een_component_id'), self(), PortName, Pid, RemotePortName]),
    een_gen:async_call(Pid, {msg, RemotePortName, SenderId, Msg}).
    

reset() ->
    OutFun = fun (PortName, _Msg) -> throw({invalid_port, PortName}) end,
    put('$een_ext_out_fun', OutFun),
    put('$een_int_out_fun', OutFun),
    put('$een_out_fun', OutFun),
    ok.

join() ->
    ExtOutFun = get('$een_ext_out_fun'),
    IntOutFun = get('$een_int_out_fun'),
    OutFun =
        fun (PortName, Msg) ->
            try IntOutFun(PortName, Msg) of
                Ret -> Ret
            catch
                throw:{invalid_port, PortName} -> ExtOutFun(PortName, Msg)
            end
        end,
    put('$een_out_fun', OutFun),
    ok.

random_pick(List) ->
    lists:nth(random:uniform(length(List)), List).
