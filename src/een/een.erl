
-module(een).

-export([spawn_config/1, out/2, reply/2, is_of/3]).

%% ----------------------------------------------------------------------------
%% Interface
%% ----------------------------------------------------------------------------

spawn_config(Config) ->
    een_config:spawn(Config).

out(LocalIfId, Msg) ->
    een_out:send(LocalIfId, Msg).

reply(From, Msg) ->
    een_comp:reply(From, Msg).

is_of(_, _, []) ->
    false;
is_of(IfId, {Type, Args}, [{IfId, {Type, Arrity}} | _])
        when length(Args) =:= Arrity->
    true;
is_of(IfId, Msg, [_ | Rest]) ->
    is_of(IfId, Msg, Rest).
