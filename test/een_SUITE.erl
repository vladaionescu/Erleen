
-module(een_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

t1_test_() ->
    {timeout, 60, repeat(fun t1/0, 100)}.

t1() ->
    Config =
        {top, type, {t1_top, start, [self()]}, make_node(w3),
         {[{a, type, {t1_a, start, []}, make_node(w1), {[], []}},
           {b, type, {t1_b, start, []}, make_node(w2), {[], []}}],
          [{top, ping_top, a, ping_a},
           {a, ping_a, b, ping_b},
           {b, pong1_b, top, pong1_top},
           {b, pong2_b, a, pong2_a}]}},
    {ok, Top} = een:spawn_config(Config),
    een_gen:cast(Top, {msg, ping_in, []}), %% Fake
    receive pong_out -> ok end.

repeat(Fun, Times) ->
    fun () -> [Fun() || _ <- lists:seq(1, Times)] end.

make_node(N) ->
    list_to_atom(atom_to_list(N) ++ "@" ++ net_adm:localhost()).