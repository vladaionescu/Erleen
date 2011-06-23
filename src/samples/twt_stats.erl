
-module(twt_stats).

-behaviour(een_comp).

-compile(export_all).

-include_lib("erleen.hrl").

-record(state, {total_tweets = 0,
                shutdown = false}).

reinit(_, _, []) ->
    {ok,
     #een_interface_spec{ext_in  = [#een_port_spec{name = tweet,
                                                   msg_type = cast,
                                                   arrity = 2}]},
     #state{}}.

handle_in(shutdown, {Reason}, _From, State = #state{shutdown = false}) ->
    {ok, MsgId} = een:out(shutdown, {Reason}),
    {ok, State#state{shutdown = {true, Reason, MsgId}}};
handle_in(tweet, {_User, _Tweet}, _From,
          State = #state{total_tweets = TotalTweets}) ->
    if
        (TotalTweets + 1) rem 1000 =:= 0 ->
            io:format("Total tweets: ~p~n", [TotalTweets + 1]);
        true ->
            ok
    end,
    {ok, State#state{total_tweets = TotalTweets + 1}}.

handle_reply(MsgId, _Reply, State = #state{shutdown = {true, Reason, MsgId}}) ->
    {shutdown, Reason, State}.

terminate(Reason, _State) ->
    Reason.
