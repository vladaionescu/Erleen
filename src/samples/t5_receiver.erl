
-module(t5_receiver).

-behaviour(een_comp).

-compile(export_all).

-include_lib("erleen.hrl").

-record(state, {reply_fun}).

reinit(_, _, [ReplyFun]) ->
    {ok, #een_interface_spec{ext_in = [#een_port_spec{name = route_in,
                                                      msg_type = call,
                                                      arrity = 1}]},
     #state{reply_fun = ReplyFun}}.

handle_in(route_in, Param, From, State = #state{reply_fun = ReplyFun}) ->
    een:reply(From, ReplyFun(Param)),
    {ok, State}.

handle_reply(_, _, _) ->
    unexpected.

terminate(Reason, _State) ->
    Reason.
