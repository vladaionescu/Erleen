
package com.erleen;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class PortSpec
{
    private final OtpErlangObject portSpec;
    
    public PortSpec(String name, PortType type, MessageType msgType, int arrity)
    {
        portSpec =
                new OtpErlangTuple(new OtpErlangObject[]
                {
                    new OtpErlangAtom("een_port_spec"),
                    new OtpErlangAtom(name),
                    typeToErlang(type),
                    messageTypeToErlang(msgType),
                    new OtpErlangInt(arrity),
                });
    }

    OtpErlangObject toErlang()
    {
        return portSpec;
    }

    private static OtpErlangAtom typeToErlang(PortType portType)
    {
        switch (portType)
        {
            case BASIC:
                return new OtpErlangAtom("basic");
            case MULTI:
                return new OtpErlangAtom("multi");
            default:
                throw new UnsupportedOperationException("Unsupported port type");
        }
    }

    private static OtpErlangAtom messageTypeToErlang(MessageType messageType)
    {
        switch (messageType)
        {
            case CAST:
                return new OtpErlangAtom("cast");
            case CALL:
                return new OtpErlangAtom("call");
            default:
                throw new UnsupportedOperationException("Unsupported message type");
        }
    }

    public enum PortType
    {
        BASIC,
        MULTI,
    }

    public enum MessageType
    {
        CAST,
        CALL,
    }
}
