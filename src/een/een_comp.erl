
-module(een_comp).

-record(state, {mod,
                mod_state}).

%% ----------------------------------------------------------------------------

out_call() ->
    a.

out_cast() ->
    b.

%% ----------------------------------------------------------------------------

behaviour_info(callback) ->
    [
        %% (OldModule, OldState, Params) ->
        %%     {ok, een_comp_params(), State} || {error, Error}
        {reinit, 3},

        %% (Call, State) ->
        %%     {reply, Reply, NewState} |
        %%     {noreply, NewState} |
        %%     {spawn_child, ChildId, Params, NewState} |
        %%     {fwd_to_child, ChildId, FwdCall, NewState} | // ???
        %%     {fwd_out, FwdCall, NewState}                 // ???
        %% where
        %%     Call = FwdCall = {CallName, CallParams, From}
        %%     CallName = atom()
        %%     CallParams = tuple()
        %%     From = reply_id()
        %%     ChildId = ChildName | {ChildName, InstanceId = integer()}
        %%     ChildName = atom()
        {handle_call, 2},
        {outgoing, 0}
    ];
behaviour_info(Module) ->
    undefined.

%% ----------------------------------------------------------------------------

init(Mod, ModArgs) ->
    case Mod:init(ModArgs) of
        {ok, MState}   -> {ok, #state{mod = Mod, mod_state = MState}};
        {error, _} = E -> E
    end.

loop(State = #state{mod = Mod, mod_state = MState}) ->
    receive
        {'$een_call', Call} ->
            Mod:handle_in_call(Call, CallId, MState);
        {'$erleen_cast', Cast} ->
            Mod:handle_in_cast(Cast, MState);
        Other ->
            Mod:handle_info(Other, MState)
    end.

