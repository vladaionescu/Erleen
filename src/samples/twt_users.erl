
-module(twt_users).

-behaviour(een_comp).

-compile(export_all).

-include_lib("erleen.hrl").

-record(state, {users = dict:new(),
                shutdown = false}).

reinit(_, _, []) ->
    {ok,
     #een_interface_spec{ext_in  = [#een_port_spec{name = new_user,
                                                   msg_type = call,
                                                   arrity = 1},
                                    #een_port_spec{name = follow,
                                                   msg_type = call,
                                                   arrity = 2},
                                    #een_port_spec{name = query_follow,
                                                   msg_type = call,
                                                   arrity = 1}]},
     #state{}}.

handle_in(shutdown, {Reason}, _From, State = #state{shutdown = false}) ->
    {ok, MsgId} = een:out(shutdown, {Reason}),
    {ok, State#state{shutdown = {true, Reason, MsgId}}};
handle_in(new_user, {User}, From, State = #state{users = Users}) ->
    case dict:find(User, Users) of
        {ok, _} ->
            een:reply(From, {error, already_exists}),
            {ok, State};
        error ->
            een:reply(From, ok),
            {ok, State#state{users = dict:store(User, [], Users)}}
    end;
handle_in(follow, {User1, User2}, From, State = #state{users = Users}) ->
    case dict:find(User1, Users) of
        {ok, Following} ->
            een:reply(From, ok),
            {ok, State#state{users =
                     dict:store(User1, [User2 | Following], Users)}};
        error ->
            een:reply(From, {error, does_not_exist}),
            {ok, State}
    end;
handle_in(query_follow, User, From, State = #state{users = Users}) ->
    een:reply(From, dict:fetch(User, Users)),
    {ok, State}.

handle_reply(MsgId, _Reply, State = #state{shutdown = {true, Reason, MsgId}}) ->
    {shutdown, Reason, State}.

terminate(Reason, _State) ->
    Reason.
