
package com.erleen.samples;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.erleen.ChildExitAction;
import com.erleen.Component;
import com.erleen.ErleenException;
import com.erleen.InterfaceSpec;
import com.erleen.Message;
import com.erleen.MessageId;
import com.erleen.PortSpec;
import com.erleen.Reply;

public class T2A extends Component
{
    private int n;
    private MessageId outId = null;
    private OtpErlangObject from = null;
    boolean gotReply = false;

    @Override
    public InterfaceSpec reinit(String oldClassName, Component oldState, OtpErlangObject[] params)
    {
        InterfaceSpec ifSpec = new InterfaceSpec();

        PortSpec pingA = new PortSpec(
                "ping_a",
                PortSpec.PortType.BASIC,
                PortSpec.MessageType.CALL,
                0);
        ifSpec.addExtInPort(pingA);

        PortSpec pingA2 = new PortSpec(
                "ping_a",
                PortSpec.PortType.MULTI,
                PortSpec.MessageType.CALL,
                0);
        ifSpec.addExtOutPort(pingA2);
        
        if (params.length != 1)
            throw new RuntimeException("Params invalid");

        try
        {
            n = ((OtpErlangLong) ((OtpErlangTuple) params[0]).elementAt(0)).intValue();
        }
        catch (OtpErlangRangeException ex)
        {
            throw new RuntimeException(ex);
        }

        return ifSpec;
    }

    @Override
    public void handleIn(Message msg) throws ErleenException, OtpErlangExit
    {
        if (!msg.getPortName().equals("ping_a"))
            throw new RuntimeException("Invalid port");

        if (outId != null || from != null)
            throw new RuntimeException("Invalid state");

        outId = out(new Message("ping_a", new OtpErlangObject[0]));
        from = msg.getFrom();
    }

    @Override
    public void handleReply(Reply reply) throws ErleenException, OtpErlangExit
    {
        if (gotReply)
            throw new RuntimeException("Invalid state");

        if (outId == null || from == null)
            throw new RuntimeException("Invalid state");

        OtpErlangObject[] replyList = ((OtpErlangList) reply.getReply()).elements();
        if (replyList.length != n)
            throw new RuntimeException("Invalid reply");

        gotReply = true;
        reply(from, new OtpErlangAtom("pong_a"));
    }

    @Override
    public ChildExitAction handleChildExit(String name, OtpErlangObject reason) throws ErleenException, OtpErlangExit
    {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public OtpErlangObject terminate(OtpErlangObject reason)
    {
        if (gotReply && outId != null && from != null)
            return reason;
        else
            return new OtpErlangAtom("fail_state");
    }

}
