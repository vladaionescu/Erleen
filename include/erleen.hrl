
-record(een_children_config, {children = [],
                              bindings = []}).
-record(een_component_spec, {id,
                             type, %% ?
                             mfa,
                             node,
                             children_config = #een_children_config{}}).
-record(een_binding, {from, to}).
-record(een_port, {comp_id, port_name}).

-record(een_interface_spec, {ext_in = [],
                             ext_out = [],
                             int_in = [],
                             int_out = []}).
-record(een_port_spec, {name,
                        type = basic, %% basic | multi | route
                        msg_type,     %% cast | call
                        arrity}).

%% Allowed binding types:
%%
%% Sender  - Receiver            Sender        : Receiver          Sender     : Receiver
%% --------------------------------------------------------------------------------------
%% basic   - basic        (Msg = Params        : Params,   Reply = ReplyBit   : ReplyBit)
%% [basic] - basic        (Msg = Params        : Params,   Reply = ReplyBit   : ReplyBit)
%% basic   - [basic]      (Msg = Params        : Params,   Reply = ReplyBit   : ReplyBit)
%% [basic] - multi        (Msg = Params        : [Params], Reply = ReplyBit   : ReplyBit)
%% multi   - [basic]      (Msg = Params        : Params,   Reply = [ReplyBit] : ReplyBit)
%% route   - [basic]      (Msg = {Key, Params} : Params,   Reply = ReplyBit   : ReplyBit)
