
-module(een_multi_reply_buffer).

-export([new/0, new_call/1, in/2]).

-record(buf, {map_msg_call = orddict:new(),
              map_calls = orddict:new()}).

-record(call, {size,
               occ = 0,
               replies = []}).

new() ->
    put('$een_multi_reply_buffer', #buf{}).

new_call(MsgIds) ->
    Buf = #buf{map_msg_call = MC,
               map_calls = Cs} = get('$een_multi_reply_buffer'),
    CallId = make_ref(),
    NewMC = lists:foldl(fun (MsgId, CurMC) ->
                                orddict:store(MsgId, CallId, CurMC)
                        end, MC, MsgIds),
    NewCs = orddict:store(CallId, #call{size = length(MsgIds)}, Cs),
    put('$een_multi_reply_buffer', Buf#buf{map_msg_call = NewMC,
                                           map_calls = NewCs}),
    CallId.

in(MsgId, Reply) ->
    Buf = #buf{map_msg_call = MC,
               map_calls = Cs} = get('$een_multi_reply_buffer'),
    case orddict:find(MsgId, MC) of
        {ok, CallId} ->
            NewMC = orddict:erase(MsgId, MC),
            Call = #call{occ = Occ,
                         replies = Replies} = orddict:fetch(CallId, Cs),
            NewCall = Call#call{occ = Occ + 1, replies = [Reply | Replies]},
            {Outcome, NewCs} =
                case NewCall of
                    #call{occ = Size, size = Size, replies = AllReplies} ->
                        {{out, CallId, AllReplies}, orddict:erase(CallId, Cs)};
                    _ ->
                        {noout, orddict:store(CallId, NewCall, Cs)}
                end,
            put('$een_multi_reply_buffer', Buf#buf{map_msg_call = NewMC,
                                                   map_calls = NewCs}),
            Outcome;
        error ->
            not_multi
    end.
