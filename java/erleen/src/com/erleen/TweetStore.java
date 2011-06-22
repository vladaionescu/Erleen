
package com.erleen;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TweetStore implements Component
{
    private Map<String, List<OtpErlangString>> tweets;
    private Map<MessageId, From> getTweetsCalls;

    public InterfaceSpec reinit()
    {
        // Initialize members
        tweets = new HashMap<String, List<OtpErlangString>>();
        getTweetsCalls = new HashMap<MessageId, From>();

        //
        // Define interface spec

        InterfaceSpec ifSpec = new InterfaceSpec();

        PortSpec TweetPort =
                new PortSpec("tweet",
                        PortSpec.PortType.BASIC,
                        PortSpec.MessageType.CALL, 2);
        ifSpec.addExtInPort(TweetPort);
        
        PortSpec GetFollowedTweetsPort =
                new PortSpec("get_followed_tweets",
                        PortSpec.PortType.BASIC,
                        PortSpec.MessageType.CALL, 1);
        ifSpec.addExtInPort(GetFollowedTweetsPort);

        PortSpec QueryFollowPort =
                new PortSpec("query_follow",
                        PortSpec.PortType.BASIC,
                        PortSpec.MessageType.CALL, 1);
        ifSpec.addExtOutPort(QueryFollowPort);
        
        return ifSpec;
    }

    public void handleIn(Message msg, From from)
    {
        // Match on port names
        if(msg.getPortName().equals("tweet"))
            handleTweet(msg, from);
        else if(msg.getPortName().equals("get_followed_tweets"))
            handleGetFollowedTweets(msg, from);
    }

    private void handleTweet(Message msg, From from)
    {
        // Get user
        String user = msg.getArg(1).toString();
        // Get tweet text
        OtpErlangString tweet = (OtpErlangString) msg.getArg(2);

        // Store tweet in user's tweets list
        List<OtpErlangString> userTweets = tweets.get(user);
        if(userTweets == null)
        {
            userTweets = new ArrayList<OtpErlangString>();
            userTweets.add(tweet);
            tweets.put(user, userTweets);
        }
        else
        {
            userTweets.add(tweet);
        }

        // Reply ok
        from.reply(new OtpErlangAtom("ok"));
    }

    private void handleGetFollowedTweets(Message msg, From from)
    {
        // Get user from args
        final String user = msg.getArg(1).toString();

        // Out querry_follow
        Message querryFollow =
                new Message("query_follow",
                    new ArrayList<OtpErlangObject>()
                        {{add(new OtpErlangString(user));}});
        MessageId querryFollowMsgId = querryFollow.out();

        // Store from of the request
        getTweetsCalls.put(querryFollowMsgId, from);
    }

    public void handleReply(MessageId id, Reply reply)
    {
        // Get previously stored from
        From from = getTweetsCalls.remove(id);

        // Build up list of all tweets the user is following
        List<OtpErlangObject> followingTweets =
                new ArrayList<OtpErlangObject>();
        OtpErlangList followingList = (OtpErlangList) reply.getReply();
        OtpErlangObject[] followingListArray = followingList.elements();
        for(int i = 0; i < followingListArray.length; i++)
        {
            String user = followingListArray[i].toString();
            followingTweets.addAll(tweets.get(user));
        }

        // Reply with the tweets from all the users we are following
        OtpErlangObject[] followingTweetsArray =
                (OtpErlangObject[]) followingTweets.toArray();
        from.reply(new OtpErlangList(followingTweetsArray));
    }

    public ChildExitAction handleChildExit(String componentName, Reason reason)
    {
        return ChildExitAction.shutdown(reason);
    }

    public Reason terminate(Reason reason)
    {
        return reason;
    }
}
