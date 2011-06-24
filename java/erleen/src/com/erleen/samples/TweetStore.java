
package com.erleen.samples;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.erleen.ChildExitAction;
import com.erleen.Component;
import com.erleen.ErleenException;
import com.erleen.InterfaceSpec;
import com.erleen.Message;
import com.erleen.MessageId;
import com.erleen.PortSpec;
import com.erleen.Reply;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TweetStore extends Component
{
    private static final long serialVersionUID = 0L;

    private Map<String, List<OtpErlangString>> tweets;
    private Map<MessageId, Message> getTweetsCalls;

    public InterfaceSpec reinit(String OldClass, Component OldState,
            OtpErlangObject[] Params)
    {
        // Initialize members
        tweets = new HashMap<String, List<OtpErlangString>>();
        getTweetsCalls = new HashMap<MessageId, Message>();

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

    public void handleIn(Message msg) throws ErleenException, OtpErlangExit
    {
        // Match on port names
        if(msg.getPortName().equals("tweet"))
            handleTweet(msg);
        else if(msg.getPortName().equals("get_followed_tweets"))
            handleGetFollowedTweets(msg);
    }

    private void handleTweet(Message msg) throws ErleenException, OtpErlangExit
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
        reply(msg, new OtpErlangAtom("ok"));
    }

    private void handleGetFollowedTweets(Message msg)
            throws ErleenException, OtpErlangExit
    {
        // Get user from args
        final String user = msg.getArg(1).toString();

        // Out querry_follow
        Message querryFollow =
                new Message("query_follow",
                    new OtpErlangObject[] {new OtpErlangString(user)});
        MessageId querryFollowMsgId = out(querryFollow);

        // Store request
        getTweetsCalls.put(querryFollowMsgId, msg);
    }

    public void handleReply(Reply reply) throws ErleenException, OtpErlangExit
    {
        // Get previously stored request
        Message msg = getTweetsCalls.remove(reply.getMessageId());

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
        reply(msg, new OtpErlangList(followingTweetsArray));
    }

    public ChildExitAction handleChildExit(String componentName,
            OtpErlangObject reason) throws ErleenException, OtpErlangExit
    {
        return ChildExitAction.shutdown(reason);
    }

    public OtpErlangObject terminate(OtpErlangObject reason)
    {
        return reason;
    }
}
