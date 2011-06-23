
package com.erleen;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ChildExitAction
{
    private final OtpErlangObject shutdownReason;
    private final Action action;

    private ChildExitAction(Action action, OtpErlangObject shutdownReason)
    {
        this.action = action;
        this.shutdownReason = shutdownReason;
    }

    public static ChildExitAction shutdown(OtpErlangObject reason)
    {
        return new ChildExitAction(Action.SHUTDOWN, reason);
    }
    public static ChildExitAction restart()
    {
        return new ChildExitAction(Action.RESTART, null);
    }

    public static ChildExitAction ignore()
    {
        return new ChildExitAction(Action.IGNORE, null);
    }

    OtpErlangObject toErlang()
    {
        switch (action)
        {
            case SHUTDOWN:
                return new OtpErlangTuple(new OtpErlangObject[]
                {
                    new OtpErlangAtom("shutdown"),
                    shutdownReason,
                });

            case RESTART:
                return new OtpErlangAtom("restart");

            case IGNORE:
                return new OtpErlangAtom("ignore");

            default:
                throw new UnsupportedOperationException("Unexpected action");
        }
    }

    private enum Action
    {
        SHUTDOWN,
        RESTART,
        IGNORE,
    }
}
