
-module(t2_f).

-behaviour(een_comp).

-include_lib("erleen.hrl").

-export([reinit/3, handle_in/4, handle_reply/3, terminate/2]).

-record(state, {sent_ping = false,
                got_reply = false}).

reinit(_, _, []) ->
    {ok,
     #een_interface_spec{ext_in = [#een_port_spec{name = ping_f,
                                                  msg_type = cast,
                                                  arrity = 0}],
                         ext_out = [#een_port_spec{name = ping_f,
                                                   msg_type = call,
                                                   arrity = 0},
                                    #een_port_spec{name = pong_f,
                                                   msg_type = cast,
                                                   arrity = 0}]},
     #state{}}.

handle_in(ping_f, {}, _From, State = #state{sent_ping = false}) ->
    {ok, MsgId} = een:out(ping_f, {}),
    {ok, State#state{sent_ping = {true, MsgId}}}.

handle_reply(MsgId, {reply, pong_g}, State = #state{sent_ping = {true, MsgId},
                                           got_reply = false}) ->
    een:out(pong_f, {}),
    {ok, State#state{got_reply = true}}.

terminate(Reason, #state{sent_ping = {true, _},
                         got_reply = true}) ->
    Reason;
terminate(_, State) ->
    {failed_state, State}.
