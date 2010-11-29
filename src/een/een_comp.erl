
-module(een_comp).

-include("erleen.hrl").

-export([out/1, out_async/1, fwd/2, reply/2]).
-export([start/3, start/2, bind/2, get_params/1]).
-export([behaviour_info/1]).

-record(state, {mod,
                mod_state,
                params,
                bind_out}).

%% ----------------------------------------------------------------------------

out(Msg) ->
    To = to(Msg),
    case Msg of
        {call, Call, Params} -> do_call(To, Call, Params, self());
        {cast, Cast, Params} -> do_cast(To, Cast, Params), ok
    end.

out_async(Msg) ->
    To = to(Msg),
    case Msg of
        {call, Call, Params} -> do_call_async(To, Call, Params, self());
        {cast, Cast, Params} -> do_cast(To, Cast, Params), none
    end.

fwd(InMsg, OutMsg) ->
    To = to(OutMsg),
    case {InMsg, OutMsg} of
        {{call, _, _, ReplyTo}, {cast, Cast, Params}} ->
            do_cast(To, Cast, Params),
            reply(ReplyTo, ok);
        {{call, _, _, ReplyTo}, {call, Call, Params}} ->
            %% TODO: This should delegate the monitor. There may be no way
            %% in Erlang to do that
            do_call_fwd(To, Call, Params, ReplyTo);
        {{cast, _, _}, {cast, Cast, Params}} ->
            do_cast(To, Cast, Params);
        {{cast, _, _}, {call, Call, Params}} ->
            do_call_async(To, Call, Params, none)
    end,
    ok.

reply(none, _) ->
    ok;
reply({To, Monitor}, Reply) ->
    To ! {'$een_reply', Monitor, Reply},
    ok;
reply(To, Reply) ->
    To ! {'$een_reply', Reply},
    ok.

to(Msg) ->
    #state{bind_out = Out} = get_state(),
    Out(Msg).

%% ----------------------------------------------------------------------------

start(Node, Module, Args) ->
    Parent = self(),
    {Pid, Monitor} = spawn_opt(Node, ?MODULE, init, [Parent, Module, Args],
                               [monitor]),
    receive
        {Pid, ok} ->
            demonitor(Monitor, [flush]),
            {ok, Pid};
        {Pid, Error} ->
            demonitor(Monitor, [flush]),
            Error;
        {'DOWN', Monitor, process, Pid, Reason} ->
            {error, Reason}
    end.

start(Module, Args) ->
    start(node(), Module, Args).

bind(Pid, Out) ->
    case sync_reply_or_down(Pid, {'$een_bind', Out, self()},
                            '$een_bind_reply') of
        {reply, ok}    -> ok;
        {down, Reason} -> {error, Reason}
    end.

get_params(Pid) ->
    case sync_reply_or_down(Pid, {'$een_get_params', self()},
                            '$een_get_params_reply') of
        {reply, Params} -> {ok, Params};
        {down, Reason}  -> {error, Reason}
    end.

%% ----------------------------------------------------------------------------

behaviour_info(callback) ->
    [
        %% (OldModule, OldState, Args) ->
        %%     {ok, een_comp_params(), State} | {error, Error}
        {reinit, 3},

        %% (InMsg, State) ->
        %%     {reply, Reply, NewState} |
        %%     {noreply, NewState} |
        %%     {fwd, Msg, NewState} |
        %%     {stop, Reason, NewState}
        %% where
        %%     InMsg = Call | Cast
        %%     Call = {call, CallName, CallParams, From}
        %%     Cast = {cast, CastName, CastParams,
        %%     CallName = CastName = atom()
        %%     CallParams = CastParams = tuple()
        %%     From = reply_id()
        {handle, 2}
    ];
behaviour_info(Module) ->
    undefined.

%% ----------------------------------------------------------------------------

init(Parent, Mod, ModArgs) ->
    case Mod:reinit(undefined, undefined, ModArgs) of
        {ok, Params = #een_comp_params{}, MState} ->
            set_state(#state{mod = Mod, mod_state = MState, params = Params}),
            Parent ! {self(), ok},
            try loop() of _ -> exit(impossible)
            catch exit:Reason -> cbk_terminate(Reason)
            end;
        {error, _} = E ->
            Parent ! {self(), E}
    end.

loop() ->
    State = #state{mod = Mod,
                   mod_state = MState,
                   params = EenParams} = get_state(),
    receive
        {'$een_call', Call, Params, ReplyToM} ->
            cbk_handle({call, Call, Params, ReplyToM}, ReplyToM);
        {'$een_cast', Cast, Params} ->
            cbk_handle({cast, Cast, Params}, none);
        {'$een_reply', Monitor, Reply} ->
            Mod:handle({reply, Monitor, Reply});
        {'DOWN', Monitor, process, Pid, Reason} ->
            Mod:handle({reply_down, Monitor, Reason});
        {'$een_bind', BindOut, From} ->
            put('$een_comp_state', State#state{bind_out = BindOut}),
            From ! {'$een_bind_reply', ok};
        {'$een_get_params', From} ->
            From ! {'$een_get_params_reply', EenParams}
    end,
    loop().

cbk_handle(InMsg, ReplyToM) ->
    State = #state{mod = Mod, mod_state = MState} = get_state(),
    Ret = Mod:handle(InMsg, MState),
    NewMState = case Ret of {noreply, NMS}  -> NMS;
                            {reply, _, NMS} -> NMS;
                            {fwd, _, NMS}   -> NMS;
                            {stop, _, NMS}  -> NMS
                end,
    set_state(State#state{mod_state = NewMState}),
    case Ret of {noreply, _}      -> ok;
                {reply, Reply, _} -> reply(ReplyToM, Reply);
                {fwd, Msg, _}     -> fwd(InMsg, Msg);
                {stop, Reason, _} -> exit(Reason)
    end.

cbk_terminate(Reason) ->
    State = #state{mod = Mod, mod_state = MState} = get_state(),
    _ = Mod:terminate(Reason, MState).

%% ----------------------------------------------------------------------------

sync_reply_or_down(Pid, Msg, ReplyId) ->
    M = erlang:monitor(process, Pid),
    Pid ! Msg,
    receive
        {ReplyId, Reply} ->
            demonitor(M, [flush]),
            {reply, Reply};
        {'DOWN', M, process, Pid, Reason} ->
            {down, Reason}
    end.

do_call(To, Call, Params, ReplyTo) ->
    case sync_reply_or_down(To, {'$een_call', Call, Params, ReplyTo},
                            '$een_reply') of
        {reply, Reply} -> Reply;
        {down, Reason} -> throw({exit, Reason})
    end.

do_call_async(To, Call, Params, ReplyTo) ->
    M = erlang:monitor(process, To),
    To ! {'$een_call', Call, Params, {ReplyTo, M}},
    M.

do_call_fwd(To, Call, Params, ReplyTo) ->
    To ! {'$een_call', Call, Params, ReplyTo}.

do_cast(To, Cast, Params) ->
    To ! {'$een_cast', Cast, Params}.

get_state() ->
    get('$een_comp_state').

set_state(NewState) ->
    put('$een_comp_state', NewState).
