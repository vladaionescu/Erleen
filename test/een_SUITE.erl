
-module(een_SUITE).

-include_lib("eunit/include/eunit.hrl").

t1_test() ->
    Config =
        {top, type, {t1_top, start, [self()]}, node(),
         {[{a, type, {t1_a, start, []}, node(), {[], []}},
           {b, type, {t1_b, start, []}, node(), {[], []}}],
          [{sup, ping_top, a, ping_a},
           {a, ping_a, b, ping_b},
           {b, pong1_b, sup, pong1_top},
           {b, pong2_b, a, pong2_a}]}},
    {ok, Top} = een:spawn_config(Config),
    io:format("spawn done~n"),
    een_gen:cast(Top, {msg, ping_in, []}),
    receive pong_out -> ok end.
