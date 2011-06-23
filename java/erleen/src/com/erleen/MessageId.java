
package com.erleen;

import com.ericsson.otp.erlang.OtpErlangRef;

public class MessageId
{
    private final OtpErlangRef ref;
    
    MessageId(OtpErlangRef ref)
    {
        this.ref = ref;
    }

    @Override
    public boolean equals(Object obj)
    {
        if (obj == null)
        {
            return false;
        }
        if (getClass() != obj.getClass())
        {
            return false;
        }
        final MessageId other = (MessageId) obj;
        if (this.ref != other.ref && (this.ref == null || !this.ref.equals(other.ref)))
        {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode()
    {
        int hash = 7;
        hash = 29 * hash + (this.ref != null ? this.ref.hashCode() : 0);
        return hash;
    }    
}
