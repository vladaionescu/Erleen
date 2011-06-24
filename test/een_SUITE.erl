
-module(een_SUITE).

-include_lib("eunit/include/eunit.hrl").

-include_lib("erleen.hrl").

-compile(export_all).

t1_test_() ->
    {timeout, 60, repeat(fun t1/0, 100)}.

t2_test_() ->
    {timeout, 60, repeat(fun t2/0, 100)}.

t3_test_() ->
    {timeout, 60, repeat(fun t3/0, 100)}.

t4_test_() ->
    {timeout, 60, repeat(fun t4/0, 100)}.

t5_test_() ->
    {timeout, 60, repeat(fun t5/0, 100)}.

t6_test_() ->
    {timeout, 60, repeat(fun t6/0, 100)}.

t7_test_() ->
    {timeout, 60, repeat(fun t7/0, 100)}.

t8_test_() ->
    {timeout, 60, repeat(fun t8/0, 100)}.

t9_test_() ->
    {timeout, 60, repeat(fun t9/0, 100)}.

t10_test_() ->
    {timeout, 60, repeat(fun t10/0, 10)}.

t1() ->
    Config =
        {#een_component_spec{id = top,
                             module = t1_top,
                             args = [self()],
                             node = make_node(w1)},
         #een_children_config{
             children = [{#een_component_spec{id = a,
                                              module = t1_a,
                                              node = make_node(w2)},
                          #een_children_config{}},
                         {#een_component_spec{id = b,
                                              module = t1_b,
                                              node = make_node(w3)},
                          #een_children_config{}}],
             bindings = [{{top, ping_top}, {a, ping_a}},
                         {{a, ping_a}, {b, ping_b}},
                         {{b, pong1_b}, {top, pong1_top}},
                         {{b, pong2_b}, {a, pong2_a}}]}},
    {ok, Top} = een:spawn_config(Config),
    een_gen:cast(Top, {msg, ping_in, {undefined, undefined}, {}}), %% Fake
    receive pong_out -> ok end.

t1_java() ->
    run_java(),
    Config =
        {#een_component_spec{id = top,
                             module = t1_top,
                             args = [self()],
                             node = make_node(w1)},
         #een_children_config{
             children = [{#een_component_spec{id = a,
                                              module = t1_a,
                                              node = make_node(w2)},
                          #een_children_config{}},
                         {#een_component_spec{id = b,
                                              module = een_java,
                                              args = [make_node(een_java), "com.erleen.samples.T1B", []]},
                          #een_children_config{}}],
             bindings = [{{top, ping_top}, {a, ping_a}},
                         {{a, ping_a}, {b, ping_b}},
                         {{b, pong1_b}, {top, pong1_top}},
                         {{b, pong2_b}, {a, pong2_a}}]}},
    {ok, Top} = een:spawn_config(Config),
    een_gen:cast(Top, {msg, ping_in, {undefined, undefined}, {}}), %% Fake
    receive pong_out -> ok end.

t2() ->
    Config =
        {#een_component_spec{id = top,
                             module = t2_top,
                             args = [self(), 6],
                             node = make_node(w1)},
         #een_children_config{
             children = [{#een_component_spec{id = a1,
                                              module = t2_a,
                                              args = [3],
                                              node = make_node(w2)},
                          #een_children_config{}},
                         {#een_component_spec{id = b11,
                                              module = t2_b,
                                              node = make_node(w3)},
                          #een_children_config{}},
                         {#een_component_spec{id = b12,
                                              module = t2_b,
                                              node = make_node(w4)},
                          #een_children_config{}},
                         {#een_component_spec{id = b13,
                                              module = t2_b,
                                              node = make_node(w5)},
                          #een_children_config{}},
                         {#een_component_spec{id = c1,
                                              module = t2_c,
                                              args = [3],
                                              node = make_node(w1)},
                          #een_children_config{}},
                         {#een_component_spec{id = e1,
                                              module = t2_e,
                                              args = [3],
                                              node = make_node(w2)},
                          #een_children_config{}},
                         {#een_component_spec{id = f11,
                                              module = t2_f,
                                              node = make_node(w3)},
                          #een_children_config{}},
                         {#een_component_spec{id = f12,
                                              module = t2_f,
                                              node = make_node(w4)},
                          #een_children_config{}},
                         {#een_component_spec{id = f13,
                                              module = t2_f,
                                              node = make_node(w5)},
                          #een_children_config{}},
                         {#een_component_spec{id = g1,
                                              module = t2_g,
                                              args = [3],
                                              node = make_node(w1)},
                          #een_children_config{}},
                         
                         {#een_component_spec{id = a2,
                                              module = t2_a,
                                              args = [1],
                                              node = make_node(w2)},
                          #een_children_config{}},
                         {#een_component_spec{id = b21,
                                              module = t2_b,
                                              node = make_node(w3)},
                          #een_children_config{}},
                         {#een_component_spec{id = c2,
                                              module = t2_c,
                                              args = [1],
                                              node = make_node(w1)},
                          #een_children_config{}},
                         {#een_component_spec{id = e2,
                                              module = t2_e,
                                              args = [1],
                                              node = make_node(w2)},
                          #een_children_config{}},
                         {#een_component_spec{id = f21,
                                              module = t2_f,
                                              node = make_node(w3)},
                          #een_children_config{}},
                         {#een_component_spec{id = g2,
                                              module = t2_g,
                                              args = [1],
                                              node = make_node(w1)},
                          #een_children_config{}},

                         {#een_component_spec{id = a3,
                                              module = t2_a,
                                              args = [7],
                                              node = make_node(w2)},
                          #een_children_config{}},
                         {#een_component_spec{id = b31,
                                              module = t2_b,
                                              node = make_node(w3)},
                          #een_children_config{}},
                         {#een_component_spec{id = b32,
                                              module = t2_b,
                                              node = make_node(w4)},
                          #een_children_config{}},
                         {#een_component_spec{id = b33,
                                              module = t2_b,
                                              node = make_node(w5)},
                          #een_children_config{}},
                         {#een_component_spec{id = b34,
                                              module = t2_b,
                                              node = make_node(w3)},
                          #een_children_config{}},
                         {#een_component_spec{id = b35,
                                              module = t2_b,
                                              node = make_node(w4)},
                          #een_children_config{}},
                         {#een_component_spec{id = b36,
                                              module = t2_b,
                                              node = make_node(w5)},
                          #een_children_config{}},
                         {#een_component_spec{id = b37,
                                              module = t2_b,
                                              node = make_node(w5)},
                          #een_children_config{}},
                         {#een_component_spec{id = c3,
                                              module = t2_c,
                                              args = [7],
                                              node = make_node(w1)},
                          #een_children_config{}},
                         {#een_component_spec{id = e3,
                                              module = t2_e,
                                              args = [7],
                                              node = make_node(w2)},
                          #een_children_config{}},
                         {#een_component_spec{id = f31,
                                              module = t2_f,
                                              node = make_node(w3)},
                          #een_children_config{}},
                         {#een_component_spec{id = f32,
                                              module = t2_f,
                                              node = make_node(w4)},
                          #een_children_config{}},
                         {#een_component_spec{id = f33,
                                              module = t2_f,
                                              node = make_node(w5)},
                          #een_children_config{}},
                         {#een_component_spec{id = f34,
                                              module = t2_f,
                                              node = make_node(w3)},
                          #een_children_config{}},
                         {#een_component_spec{id = f35,
                                              module = t2_f,
                                              node = make_node(w4)},
                          #een_children_config{}},
                         {#een_component_spec{id = f36,
                                              module = t2_f,
                                              node = make_node(w5)},
                          #een_children_config{}},
                         {#een_component_spec{id = f37,
                                              module = t2_f,
                                              node = make_node(w5)},
                          #een_children_config{}},
                         {#een_component_spec{id = g3,
                                              module = t2_g,
                                              args = [7],
                                              node = make_node(w1)},
                          #een_children_config{}}],
             bindings = [{{top, ping_top}, {a1, ping_a}},
                         {{top, ping_top}, {e1, ping_e}},
                         {{a1, ping_a}, {b11, ping_b}},
                         {{a1, ping_a}, {b12, ping_b}},
                         {{a1, ping_a}, {b13, ping_b}},
                         {{b11, ping_b}, {c1, ping_c}},
                         {{b12, ping_b}, {c1, ping_c}},
                         {{b13, ping_b}, {c1, ping_c}},
                         {{c1, pong_c}, {b11, pong_b}},
                         {{c1, pong_c}, {b12, pong_b}},
                         {{c1, pong_c}, {b13, pong_b}},
                         {{c1, pong_c}, {top, pong_top}},
                         {{e1, ping_e}, {f11, ping_f}},
                         {{e1, ping_e}, {f12, ping_f}},
                         {{e1, ping_e}, {f13, ping_f}},
                         {{f11, pong_f}, {e1, pong_e}},
                         {{f12, pong_f}, {e1, pong_e}},
                         {{f13, pong_f}, {e1, pong_e}},
                         {{f11, ping_f}, {g1, ping_g}},
                         {{f12, ping_f}, {g1, ping_g}},
                         {{f13, ping_f}, {g1, ping_g}},
                         {{g1, pong_g}, {top, pong_top}},
                         
                         {{top, ping_top}, {a2, ping_a}},
                         {{top, ping_top}, {e2, ping_e}},
                         {{a2, ping_a}, {b21, ping_b}},
                         {{b21, ping_b}, {c2, ping_c}},
                         {{c2, pong_c}, {b21, pong_b}},
                         {{c2, pong_c}, {top, pong_top}},
                         {{e2, ping_e}, {f21, ping_f}},
                         {{f21, pong_f}, {e2, pong_e}},
                         {{f21, ping_f}, {g2, ping_g}},
                         {{g2, pong_g}, {top, pong_top}},

                         {{top, ping_top}, {a3, ping_a}},
                         {{top, ping_top}, {e3, ping_e}},
                         {{a3, ping_a}, {b31, ping_b}},
                         {{a3, ping_a}, {b32, ping_b}},
                         {{a3, ping_a}, {b33, ping_b}},
                         {{a3, ping_a}, {b34, ping_b}},
                         {{a3, ping_a}, {b35, ping_b}},
                         {{a3, ping_a}, {b36, ping_b}},
                         {{a3, ping_a}, {b37, ping_b}},
                         {{b31, ping_b}, {c3, ping_c}},
                         {{b32, ping_b}, {c3, ping_c}},
                         {{b33, ping_b}, {c3, ping_c}},
                         {{b34, ping_b}, {c3, ping_c}},
                         {{b35, ping_b}, {c3, ping_c}},
                         {{b36, ping_b}, {c3, ping_c}},
                         {{b37, ping_b}, {c3, ping_c}},
                         {{c3, pong_c}, {b31, pong_b}},
                         {{c3, pong_c}, {b32, pong_b}},
                         {{c3, pong_c}, {b33, pong_b}},
                         {{c3, pong_c}, {b34, pong_b}},
                         {{c3, pong_c}, {b35, pong_b}},
                         {{c3, pong_c}, {b36, pong_b}},
                         {{c3, pong_c}, {b37, pong_b}},
                         {{c3, pong_c}, {top, pong_top}},
                         {{e3, ping_e}, {f31, ping_f}},
                         {{e3, ping_e}, {f32, ping_f}},
                         {{e3, ping_e}, {f33, ping_f}},
                         {{e3, ping_e}, {f34, ping_f}},
                         {{e3, ping_e}, {f35, ping_f}},
                         {{e3, ping_e}, {f36, ping_f}},
                         {{e3, ping_e}, {f37, ping_f}},
                         {{f31, pong_f}, {e3, pong_e}},
                         {{f32, pong_f}, {e3, pong_e}},
                         {{f33, pong_f}, {e3, pong_e}},
                         {{f34, pong_f}, {e3, pong_e}},
                         {{f35, pong_f}, {e3, pong_e}},
                         {{f36, pong_f}, {e3, pong_e}},
                         {{f37, pong_f}, {e3, pong_e}},
                         {{f31, ping_f}, {g3, ping_g}},
                         {{f32, ping_f}, {g3, ping_g}},
                         {{f33, ping_f}, {g3, ping_g}},
                         {{f34, ping_f}, {g3, ping_g}},
                         {{f35, ping_f}, {g3, ping_g}},
                         {{f36, ping_f}, {g3, ping_g}},
                         {{f37, ping_f}, {g3, ping_g}},
                         {{g3, pong_g}, {top, pong_top}}]}},
    {ok, Top} = een:spawn_config(Config),
    een_gen:cast(Top, {msg, ping_in, {undefined, undefined}, {}}), %% Fake
    receive pong_out -> ok end.

t3() ->
    Config =
        {#een_component_spec{id = top,
                             module = t3_top,
                             args = [self()],
                             node = make_node(w1)},
         #een_children_config{
             children = [{#een_component_spec{id = spawn,
                                              module = t3_spawn,
                                              node = make_node(w2)},
                          #een_children_config{
                              is_spawn = true,
                              children = [{#een_component_spec{id = child,
                                                               module = t3_child,
                                                               node = make_node(w3)},
                                           #een_children_config{}}],
                              bindings = [{{spawn, spawn}, {child, spawn}, spawn},
                                          {{spawn, ping_call}, {child, ping_call_child}},
                                          {{spawn, ping_call_single}, {child, ping_call_single_child}},
                                          {{spawn, ping_cast}, {child, ping_cast_child}},
                                          {{spawn, ping_cast_single}, {child, ping_cast_single_child}},
                                          {{child, pong_cast_child}, {spawn, pong_cast}},
                                          {{child, pong_cast_single_child}, {spawn, pong_cast_single}}],
                              spawn_min = 2,
                              spawn_max = 5,
                              spawn_init = 3}}],
             bindings = [{{top, ping_top}, {spawn, ping_spawn}},
                         {{top, spawn}, {spawn, spawn}}]}},
    {ok, Top} = een:spawn_config(Config),
    een_gen:cast(Top, {msg, ping_in, {undefined, undefined}, {}}), %% Fake
    receive pong_out -> ok end.

t4() ->
    Config =
        {#een_component_spec{id = top,
                             module = t4_top,
                             args = [self(), 3, 2, false],
                             node = make_node(w1)},
         #een_children_config{
             children = [{#een_component_spec{id = d,
                                              module = t4_d,
                                              node = make_node(w4)},
                          #een_children_config{}},
                         {#een_component_spec{id = top_in_top,
                                              module = t4_top,
                                              args = [none, 1, 1, false],
                                              node = make_node(w2)},
                          #een_children_config{
                              children = [{#een_component_spec{id = a,
                                                               module = t4_a,
                                                               node = make_node(w3)},
                                           #een_children_config{}},
                                          {#een_component_spec{id = b,
                                                               module = t4_b,
                                                               args = [1],
                                                               node = make_node(w5)},
                                           #een_children_config{}}],
                              bindings = [{{top_in_top, ping_top}, {a, ping_a}},
                                          {{a, pong_a}, {b, ping_b}},
                                          {{b, pong_b}, {top_in_top, pong_top}}]}},
                         {#een_component_spec{id = a,
                                              module = t4_a,
                                              node = make_node(w2)},
                          #een_children_config{}},
                         {#een_component_spec{id = b,
                                              module = t4_b,
                                              args = [2],
                                              node = make_node(w3)},
                          #een_children_config{}},
                         {#een_component_spec{id = sp,
                                              module = t4_sp,
                                              node = make_node(w5)},
                          #een_children_config{
                              children = [{#een_component_spec{id = a,
                                                               module = t4_a,
                                                               node = make_node(w1)},
                                           #een_children_config{}}],
                              bindings = [{{sp, ping_sp}, {a, ping_a}}]}}],
             bindings = [{{top, ping_top}, {d, ping_d}},
                         {{d, pong_d}, {top_in_top, ping_in}},
                         {{top_in_top, pong_out}, {top, pong_top}},
                         {{top, ping_top}, {a, ping_a}},
                         {{a, pong_a}, {b, ping_b}},
                         {{b, pong_b}, {top, pong_top}},
                         {{top, ping_top}, {sp, ping_in}}]}},
    {ok, Top} = een:spawn_config(Config),
    een_gen:cast(Top, {msg, ping_in, {undefined, undefined}, {}}), %% Fake
    receive pong_out -> ok end,
    Config2 =
        {#een_component_spec{id = top,
                             version = changed,
                             module = t4_top,
                             args = [self(), 2, 2, true],
                             node = make_node(w1)},
         #een_children_config{
             version = changed,
             children = [{#een_component_spec{id = d,
                                              version = unchanged},
                          #een_children_config{version = unchanged}},
                         {#een_component_spec{id = top_in_top,
                                              version = changed,
                                              module = t4_top2,
                                              node = make_node(w2)},
                          #een_children_config{
                              version = changed,
                              children = [{#een_component_spec{id = a,
                                                               version = unchanged,
                                                               module = t4_a,
                                                               node = make_node(w3)},
                                           #een_children_config{version = unchanged}},
                                          {#een_component_spec{id = b,
                                                               version = removed},
                                           #een_children_config{}},
                                          {#een_component_spec{id = sp,
                                                               version = new,
                                                               module = t4_sp,
                                                               node = make_node(w5)},
                                           #een_children_config{
                                               version = new,
                                               children = [{#een_component_spec{id = a,
                                                                                version = new,
                                                                                module = t4_a,
                                                                                node = make_node(w1)},
                                                            #een_children_config{version = new}}],
                                               bindings = [{{sp, ping_sp}, {a, ping_a}}]}}],
                              bindings = [{{top_in_top, ping_top1}, {a, ping_a}},
                                          {{a, pong_a}, {top_in_top, pong_top}},
                                          {{top_in_top, ping_top2}, {sp, ping_in}}]}},
                         {#een_component_spec{id = a,
                                              version = unchanged},
                          #een_children_config{version = unchanged}},
                         {#een_component_spec{id = b,
                                              version = unchanged},
                          #een_children_config{version = unchanged}},
                         {#een_component_spec{id = sp,
                                              version = removed},
                          #een_children_config{}},
                         {#een_component_spec{id = b2,
                                              version = new,
                                              module = t4_b,
                                              args = [1],
                                              node = make_node(w5)},
                          #een_children_config{version = new}}],
             bindings = [{{top, ping_top}, {d, ping_d}},
                         {{d, pong_d}, {top_in_top, ping_in}},
                         {{top_in_top, pong_out}, {b2, ping_b}},
                         {{b2, pong_b}, {top, pong_top}},
                         {{top, ping_top}, {a, ping_a}},
                         {{a, pong_a}, {b, ping_b}},
                         {{b, pong_b}, {top, pong_top}}]}},
    ok = een:reconfig(Top, Config2),
    een_gen:cast(Top, {msg, ping_in, {undefined, undefined}, {}}), %% Fake
    receive pong_out -> ok end.

t5() ->
    Config =
        {#een_component_spec{id = top,
                             module = t5_top,
                             args = [self()],
                             node = make_node(w1)},
         #een_children_config{
             children = [{#een_component_spec{id = sender,
                                              module = t5_sender,
                                              args = [fun t5_phase_msg/1,
                                                      fun t5_check_reply/2,
                                                      10,
                                                      basic],
                                              node = make_node(w2)},
                          #een_children_config{routes = orddict:from_list([{a, fun t5_route/2}])}},
                         {#een_component_spec{id = recv_a,
                                              module = t5_receiver,
                                              args = [fun t5_reply_a/1],
                                              node = make_node(w3)},
                          #een_children_config{}},
                         {#een_component_spec{id = recv_b,
                                              module = t5_receiver,
                                              args = [fun t5_reply_b/1],
                                              node = make_node(w4)},
                          #een_children_config{}},
                         {#een_component_spec{id = recv_c,
                                              module = t5_receiver,
                                              args = [fun t5_reply_c/1],
                                              node = make_node(w5)},
                          #een_children_config{}}],
             bindings = [{{top, start}, {sender, start}},
                         {{sender, route_out}, {recv_a, route_in}, {route, a}},
                         {{sender, route_out}, {recv_b, route_in}, {route, a}},
                         {{sender, route_out}, {recv_c, route_in}, {route, a}}]}},
    {ok, Top} = een:spawn_config(Config),
    een_gen:cast(Top, {msg, start, {undefined, undefined}, {}}), %% Fake
    receive pong_out -> ok end.
    
t5_phase_msg(0) -> {a};
t5_phase_msg(1) -> {b};
t5_phase_msg(2) -> {c};
t5_phase_msg(3) -> {d};
t5_phase_msg(4) -> {e};
t5_phase_msg(5) -> {f};
t5_phase_msg(6) -> {g};
t5_phase_msg(7) -> {h};
t5_phase_msg(8) -> {i};
t5_phase_msg(9) -> {j}.

t5_route({a}, DestList) -> find_in_dest_list([recv_a], DestList);
t5_route({b}, DestList) -> find_in_dest_list([recv_b], DestList);
t5_route({c}, DestList) -> find_in_dest_list([recv_c], DestList);
t5_route({d}, DestList) -> find_in_dest_list([recv_a, recv_b], DestList);
t5_route({e}, DestList) -> find_in_dest_list([recv_b, recv_c], DestList);
t5_route({f}, DestList) -> find_in_dest_list([recv_a, recv_c], DestList);
t5_route({g}, DestList) -> find_in_dest_list([recv_a, recv_b, recv_c], DestList);
t5_route({h}, DestList) -> find_in_dest_list([recv_c], DestList);
t5_route({i}, DestList) -> find_in_dest_list([recv_c], DestList);
t5_route({j}, DestList) -> find_in_dest_list([recv_b], DestList).

find_in_dest_list(Comps, DestList) ->
    CompsSet = ordsets:from_list(Comps),
    lists:filter(fun ({CompId, _, _}) -> ordsets:is_element(CompId, CompsSet) end,
                 DestList).

t5_reply_a({a}) -> reply_a_a;
t5_reply_a({d}) -> reply_a_d;
t5_reply_a({f}) -> reply_a_f;
t5_reply_a({g}) -> reply_a_g.

t5_reply_b({b}) -> reply_b_b;
t5_reply_b({d}) -> reply_b_d;
t5_reply_b({e}) -> reply_b_e;
t5_reply_b({g}) -> reply_b_g;
t5_reply_b({j}) -> reply_b_j.

t5_reply_c({c}) -> reply_c_c;
t5_reply_c({e}) -> reply_c_e;
t5_reply_c({f}) -> reply_c_f;
t5_reply_c({g}) -> reply_c_g;
t5_reply_c({h}) -> reply_c_h;
t5_reply_c({i}) -> reply_c_i.

t5_check_reply({reply, Reply}, Phase) ->
    t5_check_reply1(Reply, Phase).

t5_check_reply1(reply_a_a, 0) -> ok;
t5_check_reply1(reply_a_d, 3) -> ok;
t5_check_reply1(reply_a_f, 5) -> ok;
t5_check_reply1(reply_a_g, 6) -> ok;
t5_check_reply1(reply_b_b, 1) -> ok;
t5_check_reply1(reply_b_d, 3) -> ok;
t5_check_reply1(reply_b_e, 4) -> ok;
t5_check_reply1(reply_b_g, 6) -> ok;
t5_check_reply1(reply_b_j, 9) -> ok;
t5_check_reply1(reply_c_c, 2) -> ok;
t5_check_reply1(reply_c_e, 4) -> ok;
t5_check_reply1(reply_c_f, 5) -> ok;
t5_check_reply1(reply_c_g, 6) -> ok;
t5_check_reply1(reply_c_h, 7) -> ok;
t5_check_reply1(reply_c_i, 8) -> ok.

t6() ->
    {ok, Top} = twt:start(),
    twt2:reconfig(Top),
    een:shutdown(Top).

t7() ->
    {ok, Top} = twt:start(),
    erlang:yield(),
    twt2:reconfig(Top),
    een:shutdown(Top).

t8() ->
    {ok, Top} = twt:start(),
    twt2:reconfig(Top),
    erlang:yield(),
    een:shutdown(Top).

t9() ->
    {ok, Top} = twt:start(),
    erlang:yield(),
    twt2:reconfig(Top),
    erlang:yield(),
    een:shutdown(Top).

t10() ->
    {ok, Top} = twt:start(),
    timer:sleep(1000),
    twt2:reconfig(Top),
    timer:sleep(1000),
    een:shutdown(Top).

run_java() ->
    een_java_server:start(),
    %%io:format("Starting Erjeen...~n"),
    %%spawn(fun () -> Output = os:cmd("java -jar java/erleen/dist/erleen.jar"),
    %%                io:format("Erjeen finished. Output:~n~n~s~n", [Output])
    %%      end),
    io:format("Awaiting connection from Erjeen...~n"),
    een_java_server:wait_connection(make_node(een_java)),
    io:format("Erjeen connected~n"),
    ok.

repeat(Fun, Times) ->
    fun () -> [Fun() || _ <- lists:seq(1, Times)] end.

make_node(N) ->
    list_to_atom(atom_to_list(N) ++ "@" ++ net_adm:localhost()).
