
-module(call_splitter).

-behaviour(een_comp).

-compile(export_all).

-include_lib("erleen.hrl").

-record(state, {calls = dict:new()}).

reinit(_, _, [Arrity]) ->
    {ok,
     #een_interface_spec{ext_in  = [#een_port_spec{name = in_call,
                                                   msg_type = call,
                                                   arrity = Arrity}],
                         ext_out = [#een_port_spec{name = out_call,
                                                   msg_type = call,
                                                   arrity = Arrity},
                                    #een_port_spec{name = out_cast,
                                                   msg_type = cast,
                                                   type = multi,
                                                   arrity = Arrity}]},
     #state{}}.

handle_in(in_call, Params, From, State = #state{calls = Calls}) ->
    {ok, MsgId} = een:out(out_call, Params),
    een:out(out_cast, Params),
    {ok, State#state{calls = dict:store(MsgId, From, Calls)}}.

handle_reply(MsgId, Reply, State = #state{calls = Calls}) ->
    From = dict:fetch(MsgId, Calls),
    NewCalls = dict:erase(MsgId, Calls),
    een:reply(From, Reply),
    {ok, State#state{calls = NewCalls}}.

terminate(Reason, _State) ->
    Reason.
