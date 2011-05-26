
-module(een_out).

-export([send/2, set/2, reset/0]).

%% ----------------------------------------------------------------------------
%% Internal interface
%% ----------------------------------------------------------------------------

send(LocalIfId, Msg) ->
    OutFun = get('$een_out_fun'),
    OutFun(LocalIfId, Msg).

set(Bindings, Loc) ->
    io:format("Setting ~p bindings ~p in ~p~n", [Loc, Bindings, self()]),
    BindingsDict = orddict:from_list(Bindings),
    %% TODO: optimize
    OutFun =
        fun (IfId, Msg) ->
                case orddict:find(IfId, BindingsDict) of
                    {ok, {Pid, RemoteId}} ->
                        %io:format("msg ~p to ~p~n", [{IfId, Msg}, {Pid, RemoteId}]),
                        case Msg of
                            {cast, Cast} -> een_gen:cast(Pid, {msg, RemoteId, Cast});
                            {call, Call} -> een_gen:async_call(Pid, {msg, RemoteId, Call})
                            %{sync_call, SyncCall} ->
                            %    een_gen:call(Pid, {RemoteId, SyncCall})
                        end;
                    error ->
                        throw({invalid_message, IfId})
                end
        end,
    Key = case Loc of
               int -> '$een_int_out_fun';
               ext -> '$een_ext_out_fun'
          end,
    put(Key, OutFun),
    join().

reset() ->
    OutFun = fun (IfId, _Msg) -> throw({invalid_message, IfId}) end,
    put('$een_ext_out_fun', OutFun),
    put('$een_int_out_fun', OutFun),
    put('$een_out_fun', OutFun),
    ok.

join() ->
    ExtOutFun = get('$een_ext_out_fun'),
    IntOutFun = get('$een_int_out_fun'),
    OutFun =
        fun (IfId, Msg) ->
            try IntOutFun(IfId, Msg) of
                Ret -> Ret
            catch
                throw:{invalid_message, IfId} -> ExtOutFun(IfId, Msg)
            end
        end,
    put('$een_out_fun', OutFun),
    ok.
