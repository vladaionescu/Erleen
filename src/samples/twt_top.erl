
-module(twt_top).

-behaviour(een_comp).

-compile(export_all).

-include_lib("erleen.hrl").

-record(state, {shutdown = false}).

reinit(_, _, []) ->
    {ok,
     #een_interface_spec{ext_in  = [#een_port_spec{name = start,
                                                   msg_type = cast}],
                         ext_out = [#een_port_spec{name = start,
                                                   msg_type = cast,
                                                   type = multi}]},
     #state{}}.

handle_in(shutdown, {Reason}, _From, State = #state{shutdown = false}) ->
    {ok, MsgId} = een:out(shutdown, {Reason}),
    {ok, State#state{shutdown = {true, Reason, MsgId}}};
handle_in(start, {}, _From, State) ->
    een:out(start, {}),
    {ok, State}.

handle_reply(MsgId, _Reply, State = #state{shutdown = {true, Reason, MsgId}}) ->
    {shutdown, Reason, State}.

terminate(Reason, #state{}) ->
    Reason.
