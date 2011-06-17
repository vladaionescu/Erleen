
-module(twt).

-compile(export_all).

-include_lib("erleen.hrl").

start() ->
    Config =
        {#een_component_spec{id = top,
                             module = twt_top,
                             node = make_node(w1)},
         #een_children_config{
             children = [{#een_component_spec{id = bob,
                                              module = twt_random_twtr,
                                              args = ["Bob"],
                                              node = make_node(w5)},
                          #een_children_config{}},
                         {#een_component_spec{id = twtr,
                                              module = twt_base,
                                              node = make_node(w1)},
                          #een_children_config{
                              children = [{#een_component_spec{id = store,
                                                               module = twt_store,
                                                               node = make_node(w2)},
                                           #een_children_config{}},
                                          {#een_component_spec{id = users,
                                                               module = twt_users,
                                                               node = make_node(w2)},
                                           #een_children_config{}},
                                          {#een_component_spec{id = call_split,
                                                               module = call_splitter,
                                                               args = [2],
                                                               node = make_node(w1)},
                                           #een_children_config{}},
                                          {#een_component_spec{id = stats,
                                                               module = twt_stats,
                                                               node = make_node(w1)},
                                           #een_children_config{}},
                                          {#een_component_spec{id = tracer,
                                                               module = twt_tracer,
                                                               args = [2, 100],
                                                               node = make_node(w1)},
                                           #een_children_config{}}],
                              bindings = [{{twtr, new_user}, {users, new_user}},
                                          {{twtr, follow}, {users, follow}},
                                          {{twtr, get_followed_tweets}, {store, get_followed_tweets}},
                                          {{twtr, tweet}, {call_split, in_call}},
                                          {{call_split, out_call}, {store, tweet}},
                                          {{call_split, out_cast}, {stats, tweet}},
                                          {{call_split, out_cast}, {tracer, msg}}]}}],
             bindings = [{{top, start}, {bob, start}},
                         {{bob, new_user}, {twtr, new_user}},
                         {{bob, tweet}, {twtr, tweet}}]}},
    {ok, Top} = een:spawn_config(Config),
    een_gen:cast(Top, {msg, start, {undefined, undefined}, {}}),
    {ok, Top}.

make_node(N) ->
    list_to_atom(atom_to_list(N) ++ "@" ++ net_adm:localhost()).
