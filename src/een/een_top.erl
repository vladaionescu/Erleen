
-module(een_top).

-behaviour(een_coord).

-export([start/2]).
-export([behaviour_info/1]).
-export([reinit/3, ext_in_if/1, ext_out_if/1, int_in_if/1, int_out_if/1,
         handle_in/4, handle_reply/3, terminate/2]).

-record(state, {id = een_top,
                mod,
                mst}).

%% ----------------------------------------------------------------------------
%% Interface
%% ----------------------------------------------------------------------------

start(Module, Args) ->
    een_coord:start(?MODULE, [Module, Args]).

%% ----------------------------------------------------------------------------
%% Behaviour spec
%% ----------------------------------------------------------------------------

behaviour_info(callback) ->
    [
        %% (OldModule, OldState, Args) -> {ok, State} | {error, Error}
        {reinit, 3},

        %% (State) -> IfList
        {int_in_if, 1},
        %% (State) -> IfList
        {int_out_if, 1},

        %% (IfId, Msg, From, State) -> HandleReturn
        {handle_in, 4},

        %% (MsgId, Reply, State) -> HandleReturn
        {handle_reply, 3},

        %% (Reason, State) -> NewReason
        {terminate, 2}
    ];
behaviour_info(_) ->
    undefined.

%% ----------------------------------------------------------------------------
%% Coord callbacks
%% ----------------------------------------------------------------------------

reinit(_OldModule, _OldState, [Mod, Args]) ->
    case Mod:reinit(none, none, Args) of
        {ok, Mst0}     -> {ok, #state{mod = Mod, mst = Mst0}};
        {error, _} = E -> E
    end.

ext_in_if(_) ->
    [].

ext_out_if(_) ->
    [].

int_in_if(#state{mod = Mod, mst = Mst}) ->
    Mod:int_in_if(Mst).

int_out_if(#state{mod = Mod, mst = Mst}) ->
    Mod:int_out_if(Mst).

handle_in(IfId, Msg, From, State = #state{mod = Mod, mst = Mst}) ->
    handle_return(Mod:handle_in(IfId, Msg, From, Mst), State).

handle_reply(MsgId, Reply, State = #state{mod = Mod, mst = Mst}) ->
    handle_return(Mod:handle_reply(MsgId, Reply, Mst), State).

terminate(Reason, #state{mod = Mod, mst = Mst}) ->
    Mod:terminate(Reason, Mst).

handle_return({ok, NewMst}, State) ->
    {ok, State#state{mst = NewMst}};
handle_return({reply, Reply, NewMst}, State) ->
    {reply, Reply, State#state{mst = NewMst}};
handle_return({stop, Reason, NewMst}, State) ->
    {stop, Reason, State#state{mst = NewMst}}.