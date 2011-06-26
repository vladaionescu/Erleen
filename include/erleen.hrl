
-record(een_component_spec, {id,
                             type, %% ?
                             module,
                             args = [],
                             node = node(),
                             version = new %% new | changed | removed | unchanged
                            }).
-record(een_children_config, {children = [],
                              bindings = [],
                              routes = [],
                              is_spawn = false,
                              spawn_min = 0,
                              spawn_max = 1,
                              spawn_init = 0,
                              version = new %% new | changed | unchanged
                             }).

%% Warning: preserve for Java bindings
-record(een_interface_spec, {ext_in = [],
                             ext_out = [],
                             int_in = [],
                             int_out = []}).
%% Warning: preserve for Java bindings
-record(een_port_spec, {name,
                        type = basic, %% basic | multi
                        msg_type,     %% cast | call
                        arrity}).

%% ----------------------------------------------------------------------------
%% Internal
%% ----------------------------------------------------------------------------

-record(een_state, {id = een_comp,
                    config = #een_children_config{},
                    mod = none,
                    mst = none,
                    if_spec = #een_interface_spec{},
                    spec = #een_component_spec{},
                    is_spawn = false,
                    spawn_binding,
                    spawn_index = 1,
                    spawn_current = 0,
                    map_comps = orddict:new(),
                    map_pid_compid,
                    multi_buf = een_multi_buffer:new(),
                    ext_in_binds,
                    ext_out_binds,
                    int_in_binds,
                    int_out_binds}).

-record(een_component, {pid,
                        in_binds = orddict:new(),
                        out_binds = orddict:new(),
                        spec,
                        children_config}).
