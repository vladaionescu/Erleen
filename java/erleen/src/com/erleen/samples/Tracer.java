
package com.erleen.samples;

import com.ericsson.otp.erlang.OtpErlangExit;
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
import com.erleen.Shutdown;

public class Tracer extends Component
{
    private int freq;
    private long count;
    private OtpErlangObject shutdownReason;
    private MessageId shutdownMessageId;

    @Override
    public InterfaceSpec reinit(String oldClassName, Component oldState, OtpErlangObject[] params)
    {
        count = 0;

        OtpErlangObject[] paramsTuple = ((OtpErlangTuple) params[0]).elements();
        int arrity;
        try
        {
            arrity = ((OtpErlangLong) paramsTuple[0]).intValue();
            freq = ((OtpErlangLong) paramsTuple[1]).intValue();
        }
        catch (OtpErlangRangeException ex)
        {
            throw new RuntimeException("Invalid args");
        }

        InterfaceSpec ifSpec = new InterfaceSpec();

        PortSpec msg = new PortSpec(
                "msg",
                PortSpec.PortType.BASIC,
                PortSpec.MessageType.CAST,
                arrity);
        ifSpec.addExtInPort(msg);
        
        return ifSpec;
    }

    @Override
    public void handleIn(Message msg) throws ErleenException, OtpErlangExit
    {
        if (msg.getPortName().equals("msg"))
        {
            count++;
            if (count % freq == 0)
                System.out.println("Tracer: " + msg.getMessage().toString());
        }
        else if (msg.getPortName().equals("shutdown"))
        {
            shutdownReason = msg.getArg(0);
            shutdownMessageId =
                out(new Message("shutdown", new OtpErlangObject[]
                    {
                        shutdownReason,
                    }));
        }
        else
        {
            throw new RuntimeException("Invalid port name");
        }
    }

    @Override
    public void handleReply(Reply reply) throws ErleenException, OtpErlangExit
    {
        if (reply.getMessageId().equals(shutdownMessageId))
        {
            throw new Shutdown(shutdownReason);
        }
        else
        {
            throw new RuntimeException("Unexpected reply");
        }
    }

    @Override
    public ChildExitAction handleChildExit(String name, OtpErlangObject reason)
            throws ErleenException, OtpErlangExit
    {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public OtpErlangObject terminate(OtpErlangObject reason)
    {
        return reason;
    }
}
