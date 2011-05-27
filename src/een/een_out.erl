
-module(een_out).

-export([send/2, set/3, reset/0]).

-include_lib("erleen.hrl").

%% ----------------------------------------------------------------------------
%% Internal interface
%% ----------------------------------------------------------------------------

send(PortName, Msg) ->
    OutFun = get('$een_out_fun'),
    OutFun(PortName, Msg).

set(Bindings, IfSpec, Loc) ->
    io:format("~p: Setting ~p bindings ~p~n", [self(), Loc, Bindings]),
    BindingsDict = orddict:from_list(Bindings),
    %% TODO: optimize
    OutFun =
        fun (PortName, Msg) ->
                case orddict:find(PortName, BindingsDict) of
                    {ok, {Pid, RemotePortName}} ->
                        %io:format("msg ~p from (~p:~p) to (~p:~p)~n", [Msg, self(), PortName, Pid, RemotePortName]),
                        case port_msg_type(PortName, IfSpec, Loc) of
                            cast -> een_gen:cast(Pid, {msg, RemotePortName, Msg});
                            call -> een_gen:async_call(Pid, {msg, RemotePortName, Msg})
                        end;
                    error ->
                        throw({invalid_message, PortName})
                end
        end,
    Key = case Loc of
               int -> '$een_int_out_fun';
               ext -> '$een_ext_out_fun'
          end,
    put(Key, OutFun),
    join().

reset() ->
    OutFun = fun (PortName, _Msg) -> throw({invalid_message, PortName}) end,
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
                throw:{invalid_message, PortName} -> ExtOutFun(PortName, Msg)
            end
        end,
    put('$een_out_fun', OutFun),
    ok.

port_msg_type(Name, #een_interface_spec{ext_out = OutSpec}, ext) ->
    port_msg_type2(Name, OutSpec);
port_msg_type(Name, #een_interface_spec{int_out = OutSpec}, int) ->
    port_msg_type2(Name, OutSpec).

port_msg_type2(Name, OutSpec) ->
    #een_port_spec{msg_type = MsgType} = orddict:fetch(Name, OutSpec),
    MsgType.
