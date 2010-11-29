
-module(sample01).

-behaviour(een_comp).

-include("erleen.hrl").

-export([reinit/3, handle_call/4, handle_cast/3, handle_reply/3, terminate/2]).



reinit(_OldModule, _OldState, _Args) ->
    {ok, #een_comp_params{allowed_in = [{call, some_call, 2},
                                        {call, some_cast, 4}],
                          allowed_out = [{cast, other_cast, 1}]},
     nostate}.

handle_call(some_call, _Series, _From, State) ->
    een_comp:out_cast(other_cast, {ok}),
    {reply, ok, State}.

handle_cast(some_cast, _Series, State) ->
    {noreply, State}.

handle_reply(_ReplyId, _Replies, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
