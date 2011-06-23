
-module(twt_base).

-behaviour(een_comp).

-compile(export_all).

-include_lib("erleen.hrl").

-record(state, {calls = dict:new(),
                shutdown = false}).

reinit(_, _, []) ->
    {ok,
     #een_interface_spec{ext_in  = [#een_port_spec{name = tweet,
                                                   msg_type = call,
                                                   arrity = 2},
                                    #een_port_spec{name = get_followed_tweets,
                                                   msg_type = call,
                                                   arrity = 1},
                                    #een_port_spec{name = new_user,
                                                   msg_type = call,
                                                   arrity = 1},
                                    #een_port_spec{name = follow,
                                                   msg_type = call,
                                                   arrity = 1}],
                         int_out = [#een_port_spec{name = tweet,
                                                   msg_type = call,
                                                   arrity = 2},
                                    #een_port_spec{name = get_followed_tweets,
                                                   msg_type = call,
                                                   arrity = 1},
                                    #een_port_spec{name = new_user,
                                                   msg_type = call,
                                                   arrity = 1},
                                    #een_port_spec{name = follow,
                                                   msg_type = call,
                                                   arrity = 1}]},
     #state{}}.

handle_in(shutdown, {Reason}, _From, State = #state{shutdown = false}) ->
    {ok, MsgId} = een:out(shutdown, {Reason}),
    {ok, State#state{shutdown = {true, Reason, MsgId}}};
handle_in(Port, Params, From, State = #state{calls = Calls}) ->
    case een:out(Port, Params) of
        {ok, MsgId} -> {ok, State#state{calls = dict:store(MsgId, From, Calls)}};
        Error       -> een:reply(From, {error, Error}),
                       {ok, State}
    end.

handle_reply(MsgId, _Reply, State = #state{shutdown = {true, Reason, MsgId}}) ->
    {shutdown, Reason, State};
handle_reply(MsgId, Reply, State = #state{calls = Calls}) ->
    From = dict:fetch(MsgId, Calls),
    NewCalls = dict:erase(MsgId, Calls),
    case Reply of
        {reply, InReply} -> een:reply(From, InReply);
        {down, _}        -> een:reply(From, Reply)
    end,
    {ok, State#state{calls = NewCalls}}.

handle_child_exit(_Child, State) ->
    {restart, State}.

terminate(Reason, _State) ->
    Reason.
