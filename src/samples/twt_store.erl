
-module(twt_store).

-behaviour(een_comp).

-compile(export_all).

-include_lib("erleen.hrl").

-record(state, {tweets = dict:new(),
                get_tweets_calls = dict:new()}).

reinit(_, _, []) ->
    {ok,
     #een_interface_spec{ext_in  = [#een_port_spec{name = tweet,
                                                   msg_type = call,
                                                   arrity = 2},
                                    #een_port_spec{name = get_followed_tweets,
                                                   msg_type = call,
                                                   arrity = 1}],
                         ext_out = [#een_port_spec{name = query_follow,
                                                   msg_type = call,
                                                   arrity = 1}]},
     #state{}}.

handle_in(tweet, {User, Tweet}, From, State = #state{tweets = Tweets}) ->
    NewTweets = dict:update(User, fun (UserTweets) -> [Tweet | UserTweets] end,
                            [Tweet], Tweets),
    een:reply(From, ok),
    {ok, State#state{tweets = NewTweets}};
handle_in(get_followed_tweets, {User}, From,
          State = #state{get_tweets_calls = GetTweetsCalls}) ->
    {ok, MsgId} = een:out(query_follow, {User}),
    NewGetTweetsCalls = dict:store(MsgId, From, GetTweetsCalls),
    {ok, State#state{get_tweets_calls = NewGetTweetsCalls}}.

handle_reply(MsgId, {reply, FollowingUsers},
             State = #state{get_tweets_calls = GetTweetsCalls,
                            tweets = Tweets}) ->
    From = dict:fetch(MsgId, GetTweetsCalls),
    NewGetTweetsCalls = dict:erase(MsgId, GetTweetsCalls),
    Followed = lists:foldl(fun (User, FollowedTweets) ->
                                   dict:fetch(User, Tweets) ++ FollowedTweets
                           end, [], FollowingUsers),
    een:reply(From, Followed),
    {ok, State#state{get_tweets_calls = NewGetTweetsCalls}}.

terminate(Reason, _State) ->
    Reason.
