
package com.erleen;

import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import java.io.Serializable;

public abstract class Component implements Serializable
{
    private OtpErlangPid pid;
    private String componentId;
    private Dispatcher dispatcher;

    public abstract InterfaceSpec reinit(String OldClassName,
            Component OldState, OtpErlangObject[] Params);
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
}
