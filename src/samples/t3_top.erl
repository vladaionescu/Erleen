
-module(t3_top).

-behaviour(een_comp).

-include_lib("erleen.hrl").

-export([reinit/3, handle_in/4, handle_reply/3, terminate/2]).

-record(state, {sent_ping = false,
                got_reply = false,
                phase = 0,
                caller}).

reinit(_, _, [Caller]) ->
    {ok,
     #een_interface_spec{ext_in = [#een_port_spec{name = ping_in,
                                                  msg_type = cast,
                                                  arrity = 0}],
                         int_out = [#een_port_spec{name = ping_top,
                                                   msg_type = call,
                                                   arrity = 0},
                                    #een_port_spec{name = spawn,
                                                   msg_type = cast,
                                                   arrity = 0}]},
     #state{caller = Caller}}.

handle_in(ping_in, {}, _From, State = #state{sent_ping = false}) ->
    {ok, MsgId} = een:out(ping_top, {}),
    {ok, State#state{sent_ping = {true, MsgId},
                     phase = 1}}.

handle_reply(MsgId, {reply, pong}, State = #state{sent_ping = {true, MsgId},
                                                  got_reply = false,
                                                  phase = Phase}) when Phase < 3->
    een:out(spawn, {}),
    {ok, NewMsgId} = een:out(ping_top, {}),
    {ok, State#state{sent_ping = {true, NewMsgId},
                     got_reply = false,
                     phase = Phase + 1}};
handle_reply(MsgId, {reply, pong}, State = #state{sent_ping = {true, MsgId},
                                                  got_reply = false,
                                                  phase = 3,
                                                  caller = Caller}) ->
    Caller ! pong_out,
    {stop, normal, State#state{got_reply = true}}.

terminate(Reason, #state{phase = 3,
                         sent_ping = {true, _},
                         got_reply = true}) ->
    Reason;
terminate(_, State) ->
    {failed_state, State}.
