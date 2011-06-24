
package com.erleen;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangTuple;
import java.io.Serializable;

public abstract class Component implements Serializable
{
    private OtpErlangPid pid;
    private String componentId;
    private Dispatcher dispatcher;

    public abstract InterfaceSpec reinit(String oldClassName,
            Component oldState, OtpErlangObject[] params);
    public abstract void handleIn(Message msg)
            throws ErleenException, OtpErlangExit;
    public abstract void handleReply(Reply reply)
            throws ErleenException, OtpErlangExit;
    public abstract ChildExitAction handleChildExit(String name,
            OtpErlangObject reason) throws ErleenException, OtpErlangExit;
    public abstract OtpErlangObject terminate(OtpErlangObject reason);

    public String getComponentId()
    {
        return componentId;
    }

    void setComponentId(String componentId)
    {
        this.componentId = componentId;
    }

    public OtpErlangPid getPid()
    {
        return pid;
    }

    void setPid(OtpErlangPid pid)
    {
        this.pid = pid;
    }

    public Dispatcher getDispatcher()
    {
        return dispatcher;
    }

    void setDispatcher(Dispatcher dispatcher)
    {
        this.dispatcher = dispatcher;
    }

    protected MessageId out(Message msg) throws ErleenException
    {
        OtpErlangList args =
                new OtpErlangList(new OtpErlangObject[]
                {
                    new OtpErlangAtom(msg.getPortName()),
                    msg.getMessage(),
                });
        OtpErlangObject reply = dispatcher.rpc(pid, "een", "out", args);
        if (reply instanceof OtpErlangAtom &&
                ((OtpErlangAtom) reply).atomValue().equals("ok"))
        {
            return null;
        }
        else if (reply instanceof OtpErlangTuple)
        {
            OtpErlangObject[] replyTuple = ((OtpErlangTuple) reply).elements();
            if (replyTuple.length != 2)
                throw new ErleenException("Invalid reply");
            if (!((OtpErlangAtom) replyTuple[0]).atomValue().equals("ok"))
                throw new ErleenException("Invalid reply");

            return new MessageId((OtpErlangRef) replyTuple[1]);
        }
        else
        {
            throw new ErleenException("Invalid reply");
        }
    }

    protected void reply(Message msg, OtpErlangObject reply) throws ErleenException
    {
        reply(msg.getFrom(), reply);
    }

    protected void reply(OtpErlangObject from, OtpErlangObject reply) throws ErleenException
    {
        if (from == null)
            throw new ErleenException("From not reply-able");

        OtpErlangList args =
                new OtpErlangList(new OtpErlangObject[]
                {
                    from,
                    reply,
                });
        OtpErlangObject response = dispatcher.rpc(pid, "een", "reply", args);
        if (!(response instanceof OtpErlangAtom) ||
                !((OtpErlangAtom) response).atomValue().equals("ok"))
        {
            // @#
            System.out.println("The response is: " + response.toString());
            throw new ErleenException("Invalid response");
        }
    }
}
