
-module(een_comp).

-include("erleen.hrl").

-export([out_call/2, out_call_async/2, out_cast/2, reply/2]).
-export([start/3, start/2, bind/2, get_params/1, init/3]).
-export([behaviour_info/1]).

-record(state, {mod,
                mod_state,
                params}).

%% ----------------------------------------------------------------------------
%% Message I/O API
%% ----------------------------------------------------------------------------

out_call(Call, Params) ->
    {ok, ReplyId} = msg_out({call, Call, Params}),
    block_on_reply(ReplyId).

out_call_async(Call, Params) ->
    msg_out({call, Call, Params}),
    ok.

out_cast(Cast, Params) ->
    msg_out({cast, Cast, Params}),
    ok.

reply(_Msg, none) ->
    ok;
reply(Msg, From) ->
    msg_reply(Msg, From),
    ok.

%% ----------------------------------------------------------------------------
%% Message I/O internal
%% ----------------------------------------------------------------------------

handle_msg(Msg) ->
    case collect_in(Msg) of
        {in, MsgInSeries, ReplyTo} -> cbk_handle_msg(MsgInSeries, ReplyTo);
        noin                       -> ok
    end.

handle_reply(Reply) ->
    case collect_reply(Reply) of
        {reply, ReplyId, ReplySeries} -> cbk_handle_reply(ReplyId, ReplySeries);
        noreply                       -> ok
    end.

block_on_reply(ReplyId) ->
    Reply =
        receive
            {'$een_reply', ReplyId, ReplyBit} ->
                {ReplyId, {reply, ReplyBit}};
            {'DOWN', ReplyId, process, _, Reason} ->
                {ReplyId, {reply_down, Reason}}
        end,
    case collect_reply(Reply) of
        {reply, ReplyId, ReplySeries} -> ReplySeries;
        noreply                       -> block_on_reply(ReplyId)
    end.

%% (MsgOut) -> {ok, ReplyId} | ok
msg_out(MsgOut) ->
    #een_binding{msg_out = Out} = get_binding(),
    Out(MsgOut).

%% (Reply) -> {reply, ReplyId, ReplySeries} | noreply
collect_reply(Reply) ->
    #een_binding{collect_reply = Collect} = get_binding(),
    Collect(Reply).

%% (MsgIn) -> {in, MsgInSeries, ReplyTo} | noin
collect_in(MsgIn) ->
    #een_binding{collect_in = Collect} = get_binding(),
    Collect(MsgIn).

%% (Msg, ReplyTo) -> ok
msg_reply(Msg, From) ->
    #een_binding{msg_reply = Reply} = get_binding(),
    Reply(Msg, From).

%% ----------------------------------------------------------------------------
%% Internal startup API
%% ----------------------------------------------------------------------------

start(Node, Module, Args) ->
    Parent = self(),
    {Pid, Monitor} = spawn_opt(Node, ?MODULE, init, [Parent, Module, Args],
                               [monitor]),
    case recv_reply_or_down(Pid, Pid, Monitor) of
        {reply, ok}    -> {ok, Pid};
        {reply, Error} -> Error;
        {down, Reason} -> {error, Reason}
    end.

start(Module, Args) ->
    start(node(), Module, Args).

bind(Pid, Binding = #een_binding{}) ->
    case sync_reply_or_down(Pid, {'$een_bind', Binding, self()},
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

sync_reply_or_down(Pid, Msg, ReplyKey) ->
    M = erlang:monitor(process, Pid),
    Pid ! Msg,
    recv_reply_or_down(ReplyKey, Pid, M).

recv_reply_or_down(ReplyKey, Pid, Monitor) ->
    receive
        {ReplyKey, Reply} ->
            erlang:demonitor(Monitor, [flush]),
            {reply, Reply};
        {'DOWN', Monitor, process, Pid, Reason} ->
            {down, Reason}
    end.

%% ----------------------------------------------------------------------------
%% Behaviour spec
%% ----------------------------------------------------------------------------

behaviour_info(callback) ->
    [
        %% (OldModule, OldState, Args) ->
        %%     {ok, een_comp_params(), State} | {error, Error}
        {reinit, 3},

        %% (Name, Series, ReplyTo, State) -> HandleReturn
        %% where
        %%     Name = atom()
        %%     Series = [Params]
        %%     Params = tuple()
        %%     ReplyTo = reply_to()
        %%     HandleReturn = {reply, Reply, NewState} |
        %%                    {noreply, NewState} |
        %%                    {stop, Reason, NewState}
        {handle_call, 4},

        %% (Name, Series, State) -> HandleReturn
        {handle_cast, 3},

        %% (ReplyId, ReplySeries, State) -> HandleReturn
        %% where
        %%     ReplyId = reply_id()
        %%     ReplySeries = [Reply]
        %%     Reply = term()
        {handle_reply, 3},

        %% (Reason, State) -> _
        {terminate, 2}
    ];
behaviour_info(_) ->
    undefined.

%% ----------------------------------------------------------------------------
%% Main loop
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
    receive
        {'$een_msg', Msg} ->
            handle_msg(Msg);
        {'$een_reply', ReplyId, ReplyBit} ->
            handle_reply({ReplyId, {reply, ReplyBit}});
        {'DOWN', ReplyId, process, _Pid, Reason} ->
            handle_reply({ReplyId, {reply_down, Reason}});
        {'$een_bind', Binding, From} ->
            set_binding(Binding),
            From ! {'$een_bind_reply', ok};
        {'$een_get_params', From} ->
            #state{params = EenParams} = get_state(),
            From ! {'$een_get_params_reply', EenParams}
    end,
    loop().

%% ----------------------------------------------------------------------------
%% Callback util
%% ----------------------------------------------------------------------------

cbk_handle_msg(MsgInSeries, ReplyTo) ->
    State = #state{mod = Mod, mod_state = MState} = get_state(),
    Ret = case {MsgInSeries, ReplyTo} of
              {{cast, Cast, Series}, none} ->
                  Mod:handle_cast(Cast, Series, MState);
              {{call, Call, Series}, _} ->
                  Mod:handle_call(Call, Series, ReplyTo, MState)
          end,
    update_state_cbk(Ret, State),
    case Ret of {noreply, _}      -> ok;
                {reply, Reply, _} -> reply(ReplyTo, Reply);
                {stop, Reason, _} -> exit(Reason)
    end.

cbk_handle_reply(ReplyId, ReplySeries) ->
    State = #state{mod = Mod, mod_state = MState} = get_state(),
    Ret = Mod:handle_reply(ReplyId, ReplySeries, MState),
    update_state_cbk(Ret, State),
    case Ret of {noreply, _}      -> ok;
                {reply, Reply, _} -> reply(none, Reply);
                {stop, Reason, _} -> exit(Reason)
    end.

cbk_terminate(Reason) ->
    #state{mod = Mod, mod_state = MState} = get_state(),
    _ = Mod:terminate(Reason, MState).

update_state_cbk(Ret, State) ->
    NewMState = case Ret of {noreply, NMS}  -> NMS;
                            {reply, _, NMS} -> NMS;
                            {stop, _, NMS}  -> NMS
                end,
    set_state(State#state{mod_state = NewMState}).

%% ----------------------------------------------------------------------------
%% Misc
%% ----------------------------------------------------------------------------

get_binding() ->
    get('$een_comp_binding').

set_binding(NewBinding = #een_binding{}) ->
    put('$een_comp_binding', NewBinding).

get_state() ->
    get('$een_comp_state').

set_state(NewState = #state{}) ->
    put('$een_comp_state', NewState).
