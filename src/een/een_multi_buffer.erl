
-module(een_multi_buffer).

-export([new/0, new_buffer/3, in/5]).

-record(buf, {size,
              occ = 0,
              store = orddict:new()}).

%% TODO: allow resize (not to lose messages)

new() ->
    orddict:new().

new_buffer(Port, Size, Bufs) ->
    orddict:store(Port, #buf{size = Size}, Bufs).

in(Port, SenderId, Msg, From, Bufs) ->
    Buf = #buf{store = Store, occ = Occ} = orddict:fetch(Port, Bufs),
    Outcome =
        case orddict:find(SenderId, Store) of
            {ok, L = [_|_]} -> {ok, L};
            {ok, []}        -> error;
            error           -> error
        end,
    Entry = {Msg, From},
    NewBuf =
        case Outcome of
            {ok, List} -> Buf#buf{store = orddict:store(SenderId, List ++ [Entry], Store)};
            error      -> Buf#buf{store = orddict:store(SenderId, [Entry], Store),
                                  occ = Occ + 1}
        end,
    {Out, NewNewBuf} = maybe_ship_it(NewBuf),
    {Out, orddict:store(Port, NewNewBuf, Bufs)}.
    
maybe_ship_it(Buf = #buf{size = Size, occ = Size, store = Store}) ->
    {NewStore, MsgList, FromList} =
        orddict:fold(
            fun (SenderId, [{Msg, From} | Rest], {CurStore, AccMsg, AccFrom}) ->
                    {orddict:store(SenderId, Rest, CurStore),
                     [Msg | AccMsg], [From | AccFrom]}
            end, {Store, [], []}, Store),
    {{out, MsgList, FromList}, Buf#buf{store = NewStore}};
maybe_ship_it(Buf) ->
    {noout, Buf}.
