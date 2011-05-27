
-module(een_comp).

-behaviour(een_gen).

-export([behaviour_info/1]).
-export([start/2, reply/2]).
-export([reinit/3, handle_cast/2, handle_call/3, handle_reply/3, terminate/2,
         handle_child_exit/3, handle_parent_exit/2]).

-record(state, {id = een_comp,
                mod,
                mst}).

%% ----------------------------------------------------------------------------
%% Interface
%% ----------------------------------------------------------------------------

start(Module, Args) ->
    een_gen:start_child(?MODULE, [Module, Args]).

reply(From, Msg) ->
    een_gen:reply(From, Msg).

%% ----------------------------------------------------------------------------
%% Behaviour spec
%% ----------------------------------------------------------------------------

behaviour_info(callback) ->
    [
        %% Configuration = Comp
        %% Comp = {CompId, CompType, MFA, Node, ChildrenConfig}
        %% ChildrenConfig = {Comps, Bindings}
        %% Comps = [Comp]
        %% Bindings = [Binding]
        %% Binding = {CompId1, IfId1, CompId2, IfId2}

        %% InBindings = OutBindings =
        %%     [{LocalIfId, {RemotePid, RemoteIfId}}]

        %% IfList = [{IfId, {call | cast, Arrity}}]
        %% IfId = any()
        %% Msg = {call | cast, Params}
        %% Params = tuple()
        %% TypeId = atom()
        %% HandleReturn = {ok, NewState} |
        %%                {reply, Reply, NewState} |
        %%                {stop, Reason, NewState}

        %% (OldModule, OldState, Args) -> {ok, State} | {error, Error}
        {reinit, 3},

        %% (State) -> IfList
        {in_if, 1},
        %% (State) -> IfList
        {out_if, 1},

        %% (ChildrenConfig, State) -> HandleReturn
        {handle_children_config, 2},

        %% (IfId, Msg, From, State) -> HandleReturn
        {handle_in, 4},

        %% (MsgId, Reply, State) -> HandleReturn
        {handle_reply, 3},

        %% (Child, Reason, State) -> HandleReturn
        {handle_child_exit, 3},

        %% (Reason, State) -> NewReason
        {terminate, 2}
    ];
behaviour_info(_) ->
    undefined.

%% ----------------------------------------------------------------------------
%% Gen callbacks
%% ----------------------------------------------------------------------------

reinit(_OldModule, _OldState, [Mod, Args]) ->
    een_out:reset(),
    case Mod:reinit(none, none, Args) of
        {ok, Mst0}     -> {ok, #state{mod = Mod, mst = Mst0}};
        {error, _} = E -> E
    end.

handle_cast({msg, LocalId, Cast}, State) ->
    do_handle_in(LocalId, {cast, Cast}, none, State).

handle_call(get_ifs, _From, State = #state{mod = Mod, mst = Mst}) ->
    {reply, {Mod:in_if(Mst), Mod:out_if(Mst)}, State};
handle_call({set_bindings, _InBindings, OutBindings}, _From, State) ->
    een_out:set(OutBindings, ext),
    {reply, ok, State};
handle_call({set_children_config, Config}, _From,
            State = #state{mod = Mod, mst = Mst}) ->
    io:format("Setting children config ~p in ~p~n", [Config, self()]),
    handle_return(Mod:handle_children_config(Config, Mst), State);
handle_call({msg, LocalId, Call}, From, State) ->
    do_handle_in(LocalId, {call, Call}, From, State).

%% TODO: 'DOWN' ?
handle_reply(MsgId, Reply, State = #state{mod = Mod, mst = Mst}) ->
    handle_return(Mod:handle_reply(MsgId, Reply, Mst), State).

handle_parent_exit(Reason, State) ->
    NewReason = case Reason of
                    {parent_death, _} -> Reason;
                    _                 -> {parent_death, Reason}
                end,
    {stop, NewReason, State}.

handle_child_exit(Child, Reason, State = #state{mod = Mod, mst = Mst}) ->
    handle_return(Mod:handle_child_exit(Child, Reason, Mst), State).

terminate(Reason, #state{mod = Mod, mst = Mst}) ->
    Mod:terminate(Reason, Mst).

%% ----------------------------------------------------------------------------
%% Internal
%% ----------------------------------------------------------------------------

do_handle_in(IfId, Msg, From, State = #state{mod = Mod, mst = Mst}) ->
    handle_return(Mod:handle_in(IfId, Msg, From, Mst), State).

handle_return({ok, NewMst}, State) ->
    {ok, State#state{mst = NewMst}};
handle_return({reply, Reply, NewMst}, State) ->
    {reply, Reply, State#state{mst = NewMst}};
handle_return({stop, Reason, NewMst}, State) ->
    {stop, Reason, State#state{mst = NewMst}}.
