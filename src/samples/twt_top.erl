
-module(twt_top).

-behaviour(een_comp).

-compile(export_all).

-include_lib("erleen.hrl").

reinit(_, _, []) ->
    {ok,
     #een_interface_spec{ext_in  = [#een_port_spec{name = start,
                                                   msg_type = cast}],
                         ext_out = [#een_port_spec{name = start,
                                                   msg_type = cast,
                                                   type = multi}]},
     nostate}.

handle_in(start, {}, _From, nostate) ->
    een:out(start, {}),
    {ok, nostate}.

handle_reply(_, _, _) ->
    unexpected.

terminate(Reason, nostate) ->
    Reason.
