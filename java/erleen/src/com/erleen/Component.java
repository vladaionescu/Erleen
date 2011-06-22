
package com.erleen;

public interface Component
{
    public InterfaceSpec reinit();
    public void handleIn(Message msg, From from);
    public void handleReply(MessageId id, Reply reply);
    public ChildExitAction handleChildExit(String name, Reason reason);
    public Reason terminate(Reason reason);
}
