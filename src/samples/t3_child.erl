
-module(t3_child).

-behaviour(een_comp).

-include_lib("erleen.hrl").

-export([reinit/3, handle_in/4, handle_reply/3, terminate/2]).

-record(state, {}).

reinit(_, _, []) ->
    {ok,
     #een_interface_spec{ext_in = [#een_port_spec{name = ping_call_child,
                                                  msg_type = call,
                                                  arrity = 0},
                                   #een_port_spec{name = ping_call_single_child,
                                                  msg_type = call,
                                                  arrity = 0},
                                   #een_port_spec{name = ping_cast_child,
                                                  msg_type = cast,
                                                  arrity = 0},
                                   #een_port_spec{name = ping_cast_single_child,
                                                  msg_type = cast,
                                                  arrity = 0},
                                   #een_port_spec{name = spawn,
                                                  msg_type = cast,
                                                  arrity = 0}],
                         ext_out = [#een_port_spec{name = pong_cast_child,
                                                   msg_type = cast,
                                                   arrity = 0},
                                    #een_port_spec{name = pong_cast_single_child,
                                                   msg_type = cast,
                                                   arrity = 0}]},
     #state{}}.

handle_in(spawn, {}, _From, State) ->
    {ok, State};
handle_in(ping_call_child, {}, From, State) ->
    een:reply(From, pong),
    {ok, State};
handle_in(ping_call_single_child, {}, From, State) ->
    een:reply(From, pong),
    {ok, State};
handle_in(ping_cast_child, {}, _From, State) ->
    een:out(pong_cast_child, {}),
    {ok, State};
handle_in(ping_cast_single_child, {}, _From, State) ->
    een:out(pong_cast_single_child, {}),
    {ok, State}.

handle_reply(_, _, _) ->
    unexpected.

terminate(Reason, #state{}) ->
    Reason.
