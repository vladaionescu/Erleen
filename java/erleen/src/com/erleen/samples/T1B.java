
package com.erleen.samples;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.erleen.ChildExitAction;
import com.erleen.Component;
import com.erleen.ErleenException;
import com.erleen.InterfaceSpec;
import com.erleen.Message;
import com.erleen.PortSpec;
import com.erleen.Reply;

public class T1B extends Component
{
    boolean gotPing = false;

    @Override
    public InterfaceSpec reinit(String OldClassName, Component OldState, OtpErlangObject[] Params)
    {
        InterfaceSpec ifSpec = new InterfaceSpec();

        PortSpec pingB = new PortSpec(
                "ping_b",
                PortSpec.PortType.BASIC,
                PortSpec.MessageType.CALL,
                0);
        ifSpec.addExtInPort(pingB);

        PortSpec pong1B = new PortSpec(
                "pong1_b",
                PortSpec.PortType.BASIC,
                PortSpec.MessageType.CAST,
                0);
        ifSpec.addExtOutPort(pong1B);

        PortSpec pong2B = new PortSpec(
                "pong2_b",
                PortSpec.PortType.BASIC,
                PortSpec.MessageType.CAST,
                0);
        ifSpec.addExtOutPort(pong2B);

        return ifSpec;
    }

    @Override
    public void handleIn(Message msg) throws ErleenException, OtpErlangExit
    {
        if (!msg.getPortName().equals("ping_b"))
            throw new UnsupportedOperationException("Invalid port name");

        if (gotPing)
            throw new RuntimeException("Unexpected state");

        out(new Message("pong1_b", new OtpErlangObject[0]));
        out(new Message("pong2_b", new OtpErlangObject[0]));
        reply(msg, new OtpErlangAtom("pong_reply"));
        gotPing = true;
    }

    @Override
    public void handleReply(Reply reply) throws ErleenException, OtpErlangExit
    {
        throw new UnsupportedOperationException("Unexpected.");
    }

    @Override
    public ChildExitAction handleChildExit(String name, OtpErlangObject reason)
            throws ErleenException, OtpErlangExit
    {
        throw new UnsupportedOperationException("Unexpected");
    }

    @Override
    public OtpErlangObject terminate(OtpErlangObject reason)
    {
        if (!gotPing)
            return new OtpErlangAtom("fail_state");
        else
            return reason;
    }
}
