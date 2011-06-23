
package com.erleen;

import com.ericsson.otp.erlang.OtpErlangObject;

public class Reply
{
    private final MessageId messageId;
    private final OtpErlangObject reply;
    
    Reply(MessageId messageId, OtpErlangObject reply)
    {
        this.messageId = messageId;
        this.reply = reply;
    }

    public OtpErlangObject getReply()
    {
        return reply;
    }

    public MessageId getMessageId()
    {
        return messageId;
    }
}
