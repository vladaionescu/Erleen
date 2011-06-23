
-module(een_java).

-include_lib("erleen.hrl").

-export([reinit/3, handle_in/4, handle_reply/3, handle_child_exit/3,
         terminate/2]).

-record(state, {java_pid}).

reinit(OldModule, OldState,
       [JavaNode, JavaClass, JavaReinitParams]) ->
    {ok, JavaPid} = een_java_server:get_java_pid(JavaNode),
    {ok, OldJavaClass, OldJavaState} =
        case OldModule of
            een_java -> call_java(hot_swap_terminate, [], OldState);
            none     -> {ok, none, none};
            _        -> {ok, OldModule, OldState}
        end,
    State0 = #state{java_pid = JavaPid},
    {ok, InterfaceSpec} =
        call_java(reinit,
                  [OldJavaClass, OldJavaState, JavaClass, JavaReinitParams],
                  State0),
    {ok, InterfaceSpec, State0}.

handle_in(Port, Msg, From, State) ->
    ok = call_java(handle_in, [Port, Msg, From], State),
    {ok, State}.

handle_reply(MsgId, Reply, State) ->
    ok = call_java(handle_reply, [MsgId, Reply], State),
    {ok, State}.

handle_child_exit(Child, Reason, State) ->
    case call_java(handle_child_exit, [Child, Reason], State) of
        restart            -> {restart, State};
        {shutdown, Reason} -> {shutdown, Reason, State};
        ignore             -> {ignore, State}
    end.

terminate(Reason, State) ->
    call_java(terminate, [Reason], State).

call_java(Function, Args, #state{java_pid = Java}) ->
    java_msg(Java, {function, Function, Args}),
    receive_loop(Java).

receive_loop(Java) ->
    receive
        {'$een_java', rpc, Sender, {M, F, A}} ->
            java_msg(Sender, {rpc_reply, apply(M, F, A)}),
            receive_loop(Java);
        {'$een_java', reply, Reply} ->
            Reply
    end.

java_msg(Java, Msg) ->
    Java ! {'$een_java', self(), get('$een_component_id'), Msg}.
