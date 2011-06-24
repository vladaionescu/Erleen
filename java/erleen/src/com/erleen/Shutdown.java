
package com.erleen;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;

public class Shutdown extends ErleenException
{
    private final OtpErlangObject reason;

    public Shutdown()
    {
        super();
        reason = new OtpErlangAtom("normal");
    }

    public Shutdown(OtpErlangObject reason)
    {
        super();
        this.reason = reason;
    }

    public OtpErlangObject getReason()
    {
        return reason;
    }
}
