
-module(een_out).

-export([send/2, set/2, reset/0]).

%% ----------------------------------------------------------------------------
%% Internal interface
%% ----------------------------------------------------------------------------

send(PortName, Msg) ->
    OutFun = get('$een_out_fun'),
    OutFun(PortName, Msg).

set(Bindings, Loc) ->
    io:format("~p: Setting ~p bindings ~p~n", [self(), Loc, Bindings]),
    BindingsDict = orddict:from_list(Bindings),
    %% TODO: optimize
    OutFun =
        fun (PortName, Msg) ->
                case orddict:find(PortName, BindingsDict) of
                    {ok, {Pid, RemoteId}} ->
                        %io:format("msg ~p to ~p~n", [{IfId, Msg}, {Pid, RemoteId}]),
                        case PortName of
                            {_, cast} -> een_gen:cast(Pid, {msg, RemoteId, Msg});
                            {_, call} -> een_gen:async_call(Pid, {msg, RemoteId, Msg})
                            %{sync_call, SyncCall} ->
                            %    een_gen:call(Pid, {RemoteId, SyncCall})
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
