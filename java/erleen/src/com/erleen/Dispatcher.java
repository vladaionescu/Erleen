
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
import java.util.concurrent.Exchanger;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Dispatcher
{
    private final int THREAD_POOL_SIZE = 4;
    private final int MAXIMUM_THREAD_POOL_SIZE = 16;
    private final long THREAD_KEEP_ALIVE_TIME = 5;
    private final TimeUnit THREAD_KEEP_ALIVE_TIME_UNIT = TimeUnit.MINUTES;

    private final OtpSelf self;
    private final OtpPeer erleen;
    private final Object connectionLock = new Object();
    private OtpConnection connection = null;

    private final Map<OtpErlangPid, Component> mapComponents =
            Collections.synchronizedMap(new HashMap<OtpErlangPid, Component>());

    private Map<OtpErlangPid, BlockingRpc> rpcMap =
            Collections.synchronizedMap(
                new HashMap<OtpErlangPid, BlockingRpc>());

    private Thread receiverThread = null;
    private ThreadPoolExecutor requestsExecutor =
            new ThreadPoolExecutor(
                THREAD_POOL_SIZE,
                MAXIMUM_THREAD_POOL_SIZE,
                THREAD_KEEP_ALIVE_TIME,
                THREAD_KEEP_ALIVE_TIME_UNIT,
                new LinkedBlockingDeque<Runnable>());

    public Dispatcher(String nodeName, String erleenNodeName, String cookie)
            throws IOException
    {
        self = new OtpSelf(nodeName, cookie);
        erleen = new OtpPeer(erleenNodeName);
    }

    public void run() throws ErleenException
    {
        if (connection != null)
            return;

        try
        {
            connection = self.connect(erleen);

            // Register Java node
            OtpErlangObject[] registerArgs = new OtpErlangObject[] {self.pid()};
            connection.sendRPC(
                    "een_java_server", "register_java_node", registerArgs);
            OtpErlangObject registerReply = connection.receiveRPC();
            if (!(registerReply instanceof OtpErlangAtom &&
                    ((OtpErlangAtom) registerReply).atomValue().equals("ok")))
                throw new ErleenException("Unable to register java node");

            while (true)
            {
                dispatch();
            }
        }
        catch (IOException ex)
        {
            throw new ErleenException(ex);
        }
        catch (OtpAuthException ex)
        {
            throw new ErleenException(ex);
        }
        catch (OtpErlangExit ex)
        {
            throw new ErleenException(ex);
        }
        catch (InterruptedException ex)
        {
            throw new ErleenException(ex);
        }
    }

    OtpErlangObject rpc(OtpErlangPid pid,
            String module, String function, OtpErlangList args)
            throws ErleenException
    {
        OtpErlangObject rpc =
            new OtpErlangTuple(new OtpErlangObject[]
            {
                new OtpErlangAtom("$een_java"),
                new OtpErlangAtom("rpc"),
                self.pid(),
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
            BlockingRpc pendingRpc = new BlockingRpc();
            rpcMap.put(pid, pendingRpc);
            
            synchronized (connectionLock)
            {
                connection.send(pid, rpc);
            }

            if (pendingRpc.get() == null)
                throw new ErleenException("Counterpart died");

            return pendingRpc.get();
        }
        catch (IOException e)
        {
            throw new ErleenException(e);
        }
        catch (InterruptedException e)
        {
            throw new ErleenException(e);
        }
    }

    private void killComponent(OtpErlangPid pid)
    {
        mapComponents.remove(pid);

        BlockingRpc pendingRpc = rpcMap.get(pid);
        if (pendingRpc != null)
        {
            try
            {
                pendingRpc.set(null);
            }
            catch (InterruptedException ex1)
            {
                Logger.getLogger(Dispatcher.class.getName()).log(Level.SEVERE, null, ex1);
            }
        }
    }

    private void killComponent(OtpErlangPid pid, Throwable ex)
    {
        OtpErlangTuple death =
                new OtpErlangTuple(new OtpErlangObject[]
                {
                    new OtpErlangAtom("java_exception"),
                    new OtpErlangString(ex.toString()),
                    new OtpErlangString(ex.getStackTrace().toString()),
                });
        try
        {
            synchronized (connectionLock)
            {
                connection.send(pid, death);
            }
        }
        catch (IOException ex1)
        {
            Logger.getLogger(Dispatcher.class.getName()).log(Level.SEVERE, null, ex1);
        }

        killComponent(pid);
    }

    private class Request implements Runnable
    {
        public final OtpErlangPid pid;
        public final String componentId;
        public final String function;
        public final OtpErlangObject[] args;

        Request(OtpErlangPid pid, String componentId, String function,
                OtpErlangList args)
        {
            this.pid = pid;
            this.componentId = componentId;
            this.function = function;
            this.args = args.elements();
        }

        public void run()
        {
            try
            {
                handleRequest();
            }
            catch (ErleenException ex)
            {
                killComponent(pid, ex);
            }
            catch (OtpErlangExit ex)
            {
                killComponent(ex.pid());
            }
            catch (IOException ex)
            {
                killComponent(pid, ex);
            }
        }

        private void handleRequest() throws ErleenException, IOException, OtpErlangExit
        {
            Component component = mapComponents.get(pid);

            if (component == null && !function.equals("reinit"))
            {
                // Component may have died. Ignore request
                return;
            }

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
                mapComponents.put(pid, component);
                try
                {
                    synchronized (connectionLock)
                    {
                        connection.link(pid);
                    }
                }
                catch (IOException e)
                {
                    throw new ErleenException(e);
                }

                InterfaceSpec ifSpec = component.reinit(oldClass, oldState, params);
                reply(new OtpErlangTuple(new OtpErlangObject[]
                        {new OtpErlangAtom("ok"), ifSpec.toErlang()}));
            }
            else if (function.equals("handle_in") && args.length == 3)
            {
                String port = ((OtpErlangAtom) args[0]).atomValue();
                OtpErlangObject msgTupleOrList = args[1];
                OtpErlangObject from = args[2];

                component.handleIn(new Message(port, msgTupleOrList, from));
                reply(new OtpErlangAtom("ok"));
            }
            else if (function.equals("handle_reply") && args.length == 2)
            {
                OtpErlangRef msgId = (OtpErlangRef) args[0];
                OtpErlangObject replyTupleOrList = args[1];

                component.handleReply(
                        new Reply(new MessageId(msgId), replyTupleOrList));
                reply(new OtpErlangAtom("ok"));
            }
            else if (function.equals("handle_child_exit") && args.length == 2)
            {
                String child = ((OtpErlangAtom) args[0]).atomValue();
                OtpErlangObject reason = args[1];

                ChildExitAction cea =
                        component.handleChildExit(child, reason);
                reply(cea.toErlang());
            }
            else if (function.equals("terminate") && args.length == 1)
            {
                OtpErlangObject reason = args[0];

                reply(component.terminate(reason));
            }
            else
            {
                throw new UnsupportedOperationException(
                        "Function " + function + "/" + args.length +
                        " not implemented");
            }
        }

        private void reply(OtpErlangObject reply) throws IOException
        {
            OtpErlangTuple replyTuple =
                new OtpErlangTuple(new OtpErlangObject[]
                {
                    new OtpErlangAtom("$een_java"),
                    new OtpErlangAtom("reply"),
                    reply,
                });
            
            synchronized (connectionLock)
            {
                connection.send(pid, replyTuple);
            }
        }
    }

    private void dispatch() throws ErleenException, InterruptedException, IOException, OtpAuthException
    {
        try
        {
            OtpErlangObject request;
            synchronized (connectionLock)
            {
                request = connection.receive();
            }

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
            if (requestTuple.length == 3 &&
                    ((OtpErlangAtom) requestTuple[0]).atomValue().equals("function"))
            {
                String function = ((OtpErlangString) requestTuple[1]).stringValue();
                OtpErlangList args = (OtpErlangList) requestTuple[2];

                requestsExecutor.execute(
                        new Request(pid, componentId, function, args));
            }
            else if (requestTuple.length == 2 &&
                    ((OtpErlangAtom) requestTuple[0]).atomValue().equals("rpc_reply"))
            {
                BlockingRpc pendingRpc = rpcMap.get(pid);
                if (pendingRpc == null)
                    throw new ErleenException("Unexpected RPC reply");

                pendingRpc.set(requestTuple[1]);
            }
            else
            {
                throw new ErleenException("Received invalid request");
            }
        }
        catch (OtpErlangExit ex)
        {
            killComponent(ex.pid());
        }
    }

    private class BlockingRpc extends Exchanger<OtpErlangObject>
    {
        private OtpErlangObject val;
        private boolean valSet = false;
        private boolean set = false;
        
        BlockingRpc()
        {
            super();
        }

        synchronized void set(OtpErlangObject val) throws InterruptedException
        {
            if (!set)
            {
                exchange(val);
                set = true;
            }
        }

        OtpErlangObject get() throws InterruptedException
        {
            if (!valSet)
            {
                valSet = true;
                val = exchange(null);
            }
            
            return val;
        }
    }
}
