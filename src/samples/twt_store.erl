
-module(twt_store).

-behaviour(een_comp).

-compile(export_all).

-include_lib("erleen.hrl").

%% State definition
-record(state, {tweets = dict:new(),
                get_tweets_calls = dict:new()}).

%% Initialization
reinit(_, _, []) ->
    {ok,
     %% Interface declaration
     #een_interface_spec{%% External input ports
                         ext_in  = [#een_port_spec{name = tweet,
                                                   msg_type = call,
                                                   arrity = 2},
                                    #een_port_spec{name = get_followed_tweets,
                                                   msg_type = call,
                                                   arrity = 1}],
                         %% Internal input ports
                         ext_out = [#een_port_spec{name = query_follow,
                                                   msg_type = call,
                                                   arrity = 1}]},
     %% Initial state
     #state{}}.

%% Tweet message
handle_in(tweet, {User, Tweet}, From, State = #state{tweets = Tweets}) ->
    %% Add the tweet to the corresponding user
    NewTweets = dict:update(User, fun (UserTweets) -> [Tweet | UserTweets] end,
                            [Tweet], Tweets),
    %% Reply ok
    een:reply(From, ok),
    %% Return the updated state
    {ok, State#state{tweets = NewTweets}};
%% Get followed tweets request
handle_in(get_followed_tweets, {User}, From,
          State = #state{get_tweets_calls = GetTweetsCalls}) ->
    %% Query for followed users
    {ok, MsgId} = een:out(query_follow, {User}),
    %% Store the query request in a dictionary
    NewGetTweetsCalls = dict:store(MsgId, From, GetTweetsCalls),
    %% Return the updated state
    {ok, State#state{get_tweets_calls = NewGetTweetsCalls}}.

%% Reply from query_follow
handle_reply(MsgId, {reply, FollowingUsers},
             State = #state{get_tweets_calls = GetTweetsCalls,
                            tweets = Tweets}) ->
    %% Get the requester previously stored in the dictionary
    From = dict:fetch(MsgId, GetTweetsCalls),
    %% Erase the entry in the dictionary
    NewGetTweetsCalls = dict:erase(MsgId, GetTweetsCalls),
    %% For each followed user, get the tweets and accumulate them in a list
    Followed = lists:foldl(fun (User, FollowedTweets) ->
                                   dict:fetch(User, Tweets) ++ FollowedTweets
                           end, [], FollowingUsers),
    %% Reply with all the followed tweets
    een:reply(From, Followed),
    %% Return the updated state
    {ok, State#state{get_tweets_calls = NewGetTweetsCalls}}.

terminate(Reason, _State) ->
    Reason.
