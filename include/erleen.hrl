

-record(een_comp_params, {allowed_in = [],
                          allowed_out = []}).

-record(een_binding, {
    %% MsgOut = {call, Call, Params} | {cast, Cast, Params}
    %% Call = Cast = atom()
    %% Params = tuple()
    %% ReplyId = reply_id()
    %% ReplySeries = [ReplyBit]
    %% ReplyBit = {reply, Reply} | {reply_down, Reason}
    %% Reply = term()
    %% Reason = term()
    %% MsgIn = {call, Call, Params, ReplyToBit} | {cast, Cast, Params}
    %% ReplyToBit = pid()
    %% MsgInSeries = {call, Call, ParamsSeries, ReplyTo} |
    %%               {cast, Cast, ParamsSeries}
    %% ParamsSeries = [Params]
    %% ReplyTo = [ReplyToBit]

    %% (MsgOut) -> {ok, ReplyId} | ok
    msg_out,

    %% (Reply) -> {reply, ReplyId, ReplySeries} | noreply
    collect_reply,

    %% (MsgIn) -> {in, MsgInSeries, ReplyTo} | noin
    collect_in,

    %% (Reply, ReplyTo) -> ok
    msg_reply}).
