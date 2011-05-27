
-module(een_SUITE).

-include_lib("eunit/include/eunit.hrl").

-include_lib("erleen.hrl").

-compile(export_all).

t1_test_() ->
    {timeout, 60, repeat(fun t1/0, 100)}.

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
             bindings = [#een_binding{from = #een_port{comp_id = top,
                                                       port_name = ping_top},
                                      to   = #een_port{comp_id = a,
                                                       port_name = ping_a}},
                         #een_binding{from = #een_port{comp_id = a,
                                                       port_name = ping_a},
                                      to   = #een_port{comp_id = b,
                                                       port_name = ping_b}},
                         #een_binding{from = #een_port{comp_id = b,
                                                       port_name = pong1_b},
                                      to   = #een_port{comp_id = top,
                                                       port_name = pong1_top}},
                         #een_binding{from = #een_port{comp_id = b,
                                                       port_name = pong2_b},
                                      to   = #een_port{comp_id = a,
                                                       port_name = pong2_a}}]}},
    {ok, Top} = een:spawn_config(Config),
    een_gen:cast(Top, {msg, ping_in, {}}), %% Fake
    receive pong_out -> ok end.

repeat(Fun, Times) ->
    fun () -> [Fun() || _ <- lists:seq(1, Times)] end.

make_node(N) ->
    list_to_atom(atom_to_list(N) ++ "@" ++ net_adm:localhost()).
