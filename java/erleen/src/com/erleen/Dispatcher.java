
package com.erleen;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class Dispatcher
{
    private OtpConnection erlangConnection = null;
    private final Object rpcLock = new Object();
    private OtpConnection erlangRpcConnection = null;
    private OtpSelf self;
    private OtpSelf selfRpc;
    private OtpPeer erleen;
    private Map<OtpErlangPid, Component> mapComponents =
            Collections.synchronizedMap(new HashMap<OtpErlangPid, Component>());

    public Dispatcher(String nodeName, String erleenNodeName, String cookie)
            throws IOException
    {
        self = new OtpSelf(nodeName, cookie);
        selfRpc = new OtpSelf(nodeName, cookie);
        erleen = new OtpPeer(erleenNodeName);
    }

    public void messageLoop() throws ErleenException
    {
        try
        {
            // Connect to Erleen node
            if (erlangConnection == null)
                erlangConnection = self.connect(erleen);
            if (erlangRpcConnection == null)
                erlangRpcConnection = selfRpc.connect(erleen);

            // Register Java node
            OtpErlangObject[] registerArgs = new OtpErlangObject[] {self.pid()};
            erlangConnection.sendRPC(
                    "een_java_server", "register_java_node", registerArgs);
            OtpErlangObject registerReply = erlangConnection.receiveRPC();
            if (!(registerReply instanceof OtpErlangAtom &&
                    ((OtpErlangAtom) registerReply).atomValue().equals("ok")))
                throw new ErleenException("Unable to register java node");

            while (true)
            {
                try
                {
                    // Receive message
                    OtpErlangObject request = erlangConnection.receive();
                    processRequest(request);
                }
                catch (OtpErlangExit e)
                {
                    // TODO remove stack trace
                    e.printStackTrace();
                    mapComponents.remove(e.pid());
                }
            }
        }
        catch (IOException e)
        {
            throw new ErleenException(e);
        }
        catch (OtpAuthException e)
        {
            throw new ErleenException(e);
        }
        catch (OtpErlangExit e)
        {
            throw new ErleenException(e);
        }
    }

    public MessageId out(Component component, Message msg)
            throws ErleenException, OtpErlangExit
    {
        OtpErlangList args =
                new OtpErlangList(new OtpErlangObject[]
                {
                    new OtpErlangString(msg.getPortName()),
                    msg.getMessage(),
                });
        OtpErlangObject reply = rpc(component.getPid(), "een", "out", args);
        if (reply instanceof OtpErlangAtom &&
                ((OtpErlangAtom) reply).atomValue().equals("ok"))
        {
            return null;
        }
        else if (reply instanceof OtpErlangTuple)
        {
            OtpErlangObject[] replyTuple = ((OtpErlangTuple) reply).elements();
            if (replyTuple.length != 2)
                throw new ErleenException("Invalid reply");
            if (!((OtpErlangAtom) replyTuple[0]).atomValue().equals("ok"))
                throw new ErleenException("Invalid reply");
            
            return new MessageId((OtpErlangRef) replyTuple[1]);
        }
        else
        {
            throw new ErleenException("Invalid reply");
        }
    }

    public void reply(Component component, OtpErlangObject from,
            OtpErlangObject reply) throws ErleenException, OtpErlangExit
    {
        if (from == null)
            throw new ErleenException("From not reply-able");
        
        OtpErlangList args =
                new OtpErlangList(new OtpErlangObject[]
                {
                    from,
                    reply,
                });
        OtpErlangObject response =
                rpc(component.getPid(), "een", "reply", args);
        if (!(response instanceof OtpErlangAtom) ||
                !((OtpErlangAtom) response).atomValue().equals("ok"))
        {
            throw new ErleenException("Invalid response");
        }
    }

    private OtpErlangObject rpc(OtpErlangPid pid,
            String module, String function, OtpErlangList args)
            throws ErleenException, OtpErlangExit
    {
        synchronized (rpcLock)
        {
            OtpErlangObject rpc =
                new OtpErlangTuple(new OtpErlangObject[]
                {
                    new OtpErlangAtom("$een_java"),
                    new OtpErlangAtom("rpc"),
                    selfRpc.pid(),
                    new OtpErlangTuple(new OtpErlangObject[]
                    {
                        new OtpErlangAtom(module),
                        new OtpErlangAtom(function),
                        args,
                    }),
                });
            OtpErlangObject[] replyTuple;
            try
            {
                erlangRpcConnection.send(pid, rpc);
                replyTuple =
                    ((OtpErlangTuple) erlangRpcConnection.receive()).elements();
            }
            catch (IOException e)
            {
                throw new ErleenException(e);
            }
            catch (OtpAuthException e)
            {
                throw new ErleenException(e);
            }

            if (replyTuple.length != 4)
                throw new ErleenException("Received invalid message");
            String identifierAtom = ((OtpErlangAtom) replyTuple[0]).atomValue();
            if (!identifierAtom.equals("$een_java"))
                throw new ErleenException("Received invalid message");

            OtpErlangPid peerPid = (OtpErlangPid) replyTuple[1];
            if (!peerPid.equals(pid))
                throw new ErleenException("Invalid peer");

            //String componentId = ((OtpErlangAtom) replyTuple[2]).atomValue();

            OtpErlangObject[] requestTuple =
                ((OtpErlangTuple) replyTuple[3]).elements();
            if (requestTuple.length != 2)
                throw new ErleenException("Received invalid request");
            if (!((OtpErlangAtom) requestTuple[0]).atomValue().equals("rpc_reply"))
                throw new ErleenException("Received invalid request");
            return requestTuple[1];
        }
    }

    private void processRequest(OtpErlangObject request)
            throws IOException, ErleenException, OtpErlangExit
    {
        OtpErlangObject[] rpcTuple = ((OtpErlangTuple) request).elements();
        if (rpcTuple.length != 4)
            throw new ErleenException("Received invalid message");
        String identifierAtom = ((OtpErlangAtom) rpcTuple[0]).atomValue();
        if (!identifierAtom.equals("$een_java"))
            throw new ErleenException("Received invalid message");

        OtpErlangPid pid = (OtpErlangPid) rpcTuple[1];
        String componentId = ((OtpErlangAtom) rpcTuple[2]).atomValue();

        OtpErlangObject[] requestTuple =
                ((OtpErlangTuple) rpcTuple[3]).elements();
        if (requestTuple.length != 3)
            throw new ErleenException("Received invalid request");
        if (!((OtpErlangAtom) requestTuple[0]).atomValue().equals("function"))
            throw new ErleenException("Received invalid request");
        String function = ((OtpErlangString) requestTuple[1]).stringValue();
        OtpErlangList args = (OtpErlangList) requestTuple[2];

        OtpErlangObject reply =
                dispatch(componentId, pid, function, args.elements());

        OtpErlangTuple replyTuple =
                new OtpErlangTuple(new OtpErlangObject[]
                {
                    new OtpErlangAtom("$een_java"),
                    new OtpErlangAtom("reply"),
                    reply,
                });
        erlangConnection.send(pid, replyTuple);
    }

    private OtpErlangObject dispatch(String componentId, OtpErlangPid pid,
            String function, OtpErlangObject[] args)
            throws ErleenException, OtpErlangExit
    {
        Component component = mapComponents.get(pid);
        
        if (function.equals("reinit") && args.length == 4)
        {
            String oldClass;
            Component oldState = null; // TODO
            if (args[0] instanceof OtpErlangAtom &&
                    ((OtpErlangAtom) args[0]).atomValue().equals("none"))
            {
                oldClass = null;
            }
            else
            {
                oldClass = ((OtpErlangString) args[0]).stringValue();
            }
            String className = ((OtpErlangString) args[2]).stringValue();
            OtpErlangObject[] params = ((OtpErlangList) args[3]).elements();

            try
            {
                Class componentClass = Class.forName(className);
                component = (Component) componentClass.newInstance();
            }
            catch (ClassNotFoundException e)
            {
                throw new ErleenException(e);
            }
            catch (IllegalAccessException e)
            {
                throw new ErleenException(e);
            }
            catch (InstantiationException e)
            {
                throw new ErleenException(e);
            }

            component.setComponentId(componentId);
            component.setPid(pid);
            component.setDispatcher(this);
            mapComponents.put(pid, component);
            try
            {
                erlangConnection.link(pid);
            }
            catch (IOException e)
            {
                throw new ErleenException(e);
            }

            InterfaceSpec ifSpec = component.reinit(oldClass, oldState, params);
            return new OtpErlangTuple(new OtpErlangObject[]
                    {new OtpErlangAtom("ok"), ifSpec.toErlang()});
        }
        else if (function.equals("handle_in") && args.length == 3)
        {
            String port = ((OtpErlangAtom) args[0]).atomValue();
            OtpErlangObject msgTupleOrList = args[1];
            OtpErlangObject from = args[2];

            component.handleIn(new Message(port, msgTupleOrList, from));
            return new OtpErlangAtom("ok");
        }
        else if (function.equals("handle_reply") && args.length == 2)
        {
            OtpErlangRef msgId = (OtpErlangRef) args[0];
            OtpErlangObject replyTupleOrList = args[1];

            component.handleReply(
                    new Reply(new MessageId(msgId), replyTupleOrList));
            return new OtpErlangAtom("ok");
        }
        else if (function.equals("handle_child_exit") && args.length == 2)
        {
            String child = ((OtpErlangAtom) args[0]).atomValue();
            OtpErlangObject reason = args[1];

            ChildExitAction cea =
                    component.handleChildExit(child, reason);
            return cea.toErlang();
        }
        else if (function.equals("terminate") && args.length == 1)
        {
            OtpErlangObject reason = args[0];

            return component.terminate(reason);
        }
        else
        {
            throw new UnsupportedOperationException(
                    "Function " + function + "/" + args.length +
                    " not implemented");
        }
    }
}
