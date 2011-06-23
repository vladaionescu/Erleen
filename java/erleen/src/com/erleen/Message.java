
package com.erleen;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class Message
{
    private final String port;
    private final OtpErlangObject from;
    private final boolean isInward;
    private final OtpErlangObject msg;

    public Message(String port, OtpErlangObject[] msgTuple)
    {
        this.isInward = false;
        this.port = port;
        this.msg = new OtpErlangTuple(msgTuple);
        this.from = null;
    }

    Message(String port, OtpErlangObject msgTupleOrList, OtpErlangObject from)
    {
        this.isInward = true;
        this.port = port;
        this.msg = msgTupleOrList;
        this.from = from;
    }

    public String getPortName()
    {
        return port;
    }

    public OtpErlangObject getFrom()
    {
        return from;
    }

    public OtpErlangObject getArg(int i)
    {
        return ((OtpErlangTuple) msg).elementAt(i);
    }

    public OtpErlangObject getMessage()
    {
        return msg;
    }

    public boolean isIsInward()
    {
        return isInward;
    }
}
