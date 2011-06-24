
package com.erleen;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class Dispatcher
{
    private final int THREAD_POOL_SIZE = 4;
    private final int MAXIMUM_THREAD_POOL_SIZE = 16;
    private final long THREAD_KEEP_ALIVE_TIME = 5;
    private final TimeUnit THREAD_KEEP_ALIVE_TIME_UNIT = TimeUnit.MINUTES;

    private final String erleenNode;
    private final OtpNode node;

    private OtpMbox self = null;
    private final Object replyLock = new Object();
    private OtpMbox selfReply = null;
    private final Object rpcLock = new Object();
    private OtpMbox selfRpc = null;

    private final Object debugLock = new Object();

    private final Map<OtpErlangPid, Component> mapComponents =
            Collections.synchronizedMap(new HashMap<OtpErlangPid, Component>());

    private ThreadPoolExecutor requestsExecutor =
            new ThreadPoolExecutor(
                THREAD_POOL_SIZE,
                MAXIMUM_THREAD_POOL_SIZE,
                THREAD_KEEP_ALIVE_TIME,
                THREAD_KEEP_ALIVE_TIME_UNIT,
                new LinkedBlockingDeque<Runnable>());

    private final Map<OtpErlangPid, Request> mapRequests =
            new HashMap<OtpErlangPid, Request>();

    public Dispatcher(String nodeName, String erleenNode, String cookie)
            throws IOException
    {
        this.erleenNode = erleenNode;
        this.node = new OtpNode(nodeName, cookie);
    }

    private OtpErlangObject erlangRpc(String module, String function,
            OtpErlangObject[] args)
            throws OtpErlangExit, OtpErlangDecodeException
    {
        OtpErlangObject msg =
                new OtpErlangTuple(new OtpErlangObject[]
                {
                    self.self(),
                    new OtpErlangTuple(new OtpErlangObject[]
                    {
                        new OtpErlangAtom("call"),
                        new OtpErlangAtom(module),
                        new OtpErlangAtom(function),
                        new OtpErlangList(args),
                        new OtpErlangAtom("user"),
                    }),
                });
        self.send("rex", erleenNode, msg);
        OtpErlangObject[] replyTuple = ((OtpErlangTuple) self.receive()).elements();
        return replyTuple[1];
    }

    public void run() throws ErleenException
    {
        try
        {
            if(!node.ping(erleenNode, 10000))
                throw new ErleenException("Did not receive pong");

            self = node.createMbox();
            selfReply = node.createMbox();
            selfRpc = node.createMbox();

            // Register Java node
            OtpErlangObject[] registerArgs = new OtpErlangObject[] {self.self()};
            OtpErlangObject registerReply = erlangRpc(
                    "een_java_server", "register_java_node", registerArgs);
            if (!(registerReply instanceof OtpErlangAtom &&
                    ((OtpErlangAtom) registerReply).atomValue().equals("ok")))
                throw new ErleenException("Unable to register java node");

            while (true)
            {
                dispatch();
            }
        }
        catch (OtpAuthException ex)
        {
            ex.printStackTrace();
            throw new ErleenException(ex);
        }
        catch (OtpErlangExit ex)
        {
            ex.printStackTrace();
            throw new ErleenException(ex);
        }
        catch (InterruptedException ex)
        {
            ex.printStackTrace();
            throw new ErleenException(ex);
        }
        catch (OtpErlangDecodeException ex)
        {
            ex.printStackTrace();
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
                selfRpc.self(),
                new OtpErlangTuple(new OtpErlangObject[]
                {
                    new OtpErlangAtom(module),
                    new OtpErlangAtom(function),
                    args,
                }),
            });
        try
        {
            OtpErlangObject reply;
            synchronized (rpcLock)
            {
                selfRpc.link(pid);
                selfRpc.send(pid, rpc);
                reply = selfRpc.receive();
                selfRpc.unlink(pid);
            }

            OtpErlangObject[] rpcTuple = ((OtpErlangTuple) reply).elements();
            if (rpcTuple.length != 4)
                throw new ErleenException("Received invalid message");
            String identifierAtom = ((OtpErlangAtom) rpcTuple[0]).atomValue();
            if (!identifierAtom.equals("$een_java"))
                throw new ErleenException("Received invalid message");

            //OtpErlangPid pid = (OtpErlangPid) rpcTuple[1];
            //String componentId = ((OtpErlangAtom) rpcTuple[2]).atomValue();

            OtpErlangObject[] requestTuple =
                ((OtpErlangTuple) rpcTuple[3]).elements();

            if (!(requestTuple.length == 2 &&
                ((OtpErlangAtom) requestTuple[0]).atomValue().equals("rpc_reply")))
            {
                throw new ErleenException("Unexpected message");
            }

            return requestTuple[1];
        }
        catch (OtpErlangDecodeException e)
        {
            e.printStackTrace();
            throw new ErleenException(e);
        }
        catch (OtpErlangExit e)
        {
            e.printStackTrace();
            if (e.pid().equals(pid))
            {
                throw new ErleenException(e);
            }
            else
            {
                killComponent(e.pid());
                throw new ErleenException(e);
            }
        }
    }

    private void killComponent(OtpErlangPid pid)
    {
        mapComponents.remove(pid);
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

        synchronized (replyLock)
        {
            selfReply.send(pid, death);
        }

        killComponent(pid);
    }

    private static class RequestBit
    {
        public final String function;
        public final OtpErlangObject[] args;

        public RequestBit(String function, OtpErlangList args)
        {
            this.function = function;
            this.args = args.elements();
        }
    }

    private class Request implements Runnable
    {
        public final OtpErlangPid pid;
        public final String componentId;
        public final Queue<RequestBit> requestBits =
                new LinkedList<RequestBit>();
        public final Dispatcher dispatcher;

        Request(OtpErlangPid pid, String componentId, Dispatcher dispatcher)
        {
            this.pid = pid;
            this.componentId = componentId;
            this.dispatcher = dispatcher;
        }

        public void addRequest(RequestBit req)
        {
            synchronized (mapRequests)
            {
                requestBits.offer(req);
            }
        }

        private RequestBit takeRequest()
        {
            synchronized (mapRequests)
            {
                return requestBits.poll();
            }
        }

        private boolean eliminate()
        {
            synchronized (mapRequests)
            {
                if (!requestBits.isEmpty())
                    return false;

                mapRequests.remove(pid);
                return true;
            }
        }

        public void run()
        {
            try
            {
                do
                {
                    RequestBit req = takeRequest();
                    while (req != null)
                    {
                        handleRequest(req);
                        req = takeRequest();
                    }
                }
                while(!eliminate());
            }
            catch (ErleenException ex)
            {
                ex.printStackTrace();
                killComponent(pid, ex);
            }
            catch (OtpErlangExit ex)
            {
                ex.printStackTrace();
                if (ex.pid() == null)
                    killComponent(pid, ex);
                else
                    killComponent(ex.pid(), ex);
            }
        }

        private void handleRequest(RequestBit req)
                throws ErleenException, OtpErlangExit
        {
            // @#
            synchronized (debugLock)
            {
                System.out.println(
                        pid.toString() + ": handling request fun: " +
                        req.function + " args: " + req.args.toString());
            }

            Component component = mapComponents.get(pid);

            if (component == null && !req.function.equals("reinit"))
            {
                // Component may have died. Ignore reply
                return;
            }

            synchronized (replyLock)
            {
                selfReply.link(pid);
            }

            if (req.function.equals("reinit") && req.args.length == 4)
            {
                String oldClass;
                Component oldState = null; // TODO
                if (req.args[0] instanceof OtpErlangAtom &&
                        ((OtpErlangAtom) req.args[0]).atomValue().equals("none"))
                {
                    oldClass = null;
                }
                else
                {
                    oldClass = ((OtpErlangString) req.args[0]).stringValue();
                }
                String className = ((OtpErlangString) req.args[2]).stringValue();
                OtpErlangObject[] params = ((OtpErlangList) req.args[3]).elements();

                try
                {
                    Class componentClass = Class.forName(className);
                    component = (Component) componentClass.newInstance();
                }
                catch (ClassNotFoundException e)
                {
                    e.printStackTrace();
                    throw new ErleenException(e);
                }
                catch (IllegalAccessException e)
                {
                    e.printStackTrace();
                    throw new ErleenException(e);
                }
                catch (InstantiationException e)
                {
                    e.printStackTrace();
                    throw new ErleenException(e);
                }

                component.setComponentId(componentId);
                component.setPid(pid);
                component.setDispatcher(dispatcher);
                mapComponents.put(pid, component);

                InterfaceSpec ifSpec = component.reinit(oldClass, oldState, params);
                reply(new OtpErlangTuple(new OtpErlangObject[]
                        {new OtpErlangAtom("ok"), ifSpec.toErlang()}));
            }
            else if (req.function.equals("handle_in") && req.args.length == 3)
            {
                String port = ((OtpErlangAtom) req.args[0]).atomValue();
                OtpErlangObject msgTupleOrList = req.args[1];
                OtpErlangObject from = req.args[2];

                try
                {
                    component.handleIn(new Message(port, msgTupleOrList, from));
                    reply(new OtpErlangAtom("ok"));
                }
                catch (Shutdown sh)
                {
                    reply(new OtpErlangTuple(new OtpErlangObject[]
                            {
                                new OtpErlangAtom("shutdown"),
                                sh.getReason(),
                            }));
                }
            }
            else if (req.function.equals("handle_reply") && req.args.length == 2)
            {
                OtpErlangRef msgId = (OtpErlangRef) req.args[0];
                OtpErlangObject replyTupleOrList = req.args[1];

                try
                {
                    component.handleReply(
                            new Reply(new MessageId(msgId), replyTupleOrList));
                    reply(new OtpErlangAtom("ok"));
                }
                catch (Shutdown sh)
                {
                    reply(new OtpErlangTuple(new OtpErlangObject[]
                            {
                                new OtpErlangAtom("shutdown"),
                                sh.getReason(),
                            }));
                }
            }
            else if (req.function.equals("handle_child_exit") && req.args.length == 2)
            {
                String child = ((OtpErlangAtom) req.args[0]).atomValue();
                OtpErlangObject reason = req.args[1];

                ChildExitAction cea =
                        component.handleChildExit(child, reason);
                reply(cea.toErlang());
            }
            else if (req.function.equals("terminate") && req.args.length == 1)
            {
                OtpErlangObject reason = req.args[0];

                reply(component.terminate(reason));
            }
            else
            {
                throw new UnsupportedOperationException(
                        "Function " + req.function + "/" + req.args.length +
                        " not implemented");
            }

            synchronized (replyLock)
            {
                selfReply.unlink(pid);
            }
        }

        private void reply(OtpErlangObject reply) throws ErleenException
        {
            OtpErlangTuple replyTuple =
                new OtpErlangTuple(new OtpErlangObject[]
                {
                    new OtpErlangAtom("$een_java"),
                    new OtpErlangAtom("reply"),
                    reply,
                });

            synchronized (replyLock)
            {
                boolean repeat;
                do
                {
                    repeat = false;
                    
                    try
                    {
                        if (selfReply.receive(0) != null)
                            throw new ErleenException("Unexpected message");
                    }
                    catch (OtpErlangExit ex)
                    {
                        ex.printStackTrace();
                        if (ex.pid() == null)
                        {
                            throw new ErleenException(ex);
                        }
                        else
                        {
                            killComponent(ex.pid());
                            repeat = true;
                        }
                    }
                    catch (OtpErlangDecodeException ex)
                    {
                        ex.printStackTrace();
                        throw new ErleenException(ex);
                    }
                }
                while (repeat);

                selfReply.send(pid, replyTuple);
            }
        }
    }

    private void dispatch() throws ErleenException, InterruptedException, OtpAuthException, OtpErlangDecodeException
    {
        try
        {
            OtpErlangObject request = self.receive();

            // @#
            synchronized (debugLock)
            {
                System.out.println("received: " + request.toString());
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
                String function = ((OtpErlangAtom) requestTuple[1]).atomValue();
                OtpErlangList args = (OtpErlangList) requestTuple[2];

                // @#
                synchronized (debugLock)
                {
                    System.out.println("Dispatching fun: " + function + " args: " + args.toString());
                }

                synchronized (mapRequests)
                {
                    Request req = mapRequests.get(pid);
                    RequestBit reqBit = new RequestBit(function, args);
                    if (req == null)
                    {
                        req = new Request(pid, componentId, this);
                        mapRequests.put(pid, req);

                        req.addRequest(reqBit);

                        requestsExecutor.execute(req);
                    }
                    else
                    {
                        req.addRequest(reqBit);
                    }
                }
            }
            else
            {
                throw new ErleenException("Received invalid request");
            }
        }
        catch (OtpErlangExit ex)
        {
            ex.printStackTrace();
            throw new ErleenException(ex);
        }
    }
}
