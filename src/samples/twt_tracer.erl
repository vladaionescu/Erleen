
-module(twt_tracer).

-behaviour(een_comp).

-compile(export_all).

-include_lib("erleen.hrl").

-record(state, {count = 0,
                freq}).

reinit(_, _, [Arrity, Freq]) ->
    {ok,
     #een_interface_spec{ext_in  = [#een_port_spec{name = msg,
                                                   msg_type = cast,
                                                   arrity = Arrity}]},
     #state{freq = Freq}}.

handle_in(msg, Params, _From,
          State = #state{count = Count,
                         freq = Freq}) ->
    if
        (Count + 1) rem Freq =:= 0 ->
            io:format("Tracer: ~p~n", [Params]);
        true ->
            ok
    end,
    {ok, State#state{count = Count + 1}}.

handle_reply(_, _, _) ->
    unexpected.

terminate(Reason, _State) ->
    Reason.
