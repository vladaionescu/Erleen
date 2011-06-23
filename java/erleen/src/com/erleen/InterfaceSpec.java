
package com.erleen;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import java.util.ArrayList;

public class InterfaceSpec
{
    private ArrayList<PortSpec> extInPorts = new ArrayList<PortSpec>();
    private ArrayList<PortSpec> extOutPorts = new ArrayList<PortSpec>();
    private ArrayList<PortSpec> intInPorts = new ArrayList<PortSpec>();
    private ArrayList<PortSpec> intOutPorts = new ArrayList<PortSpec>();

    public void addExtInPort(PortSpec port)
    {
        extInPorts.add(port);
    }

    public void addExtOutPort(PortSpec port)
    {
        extOutPorts.add(port);
    }

    public void addIntInPort(PortSpec port)
    {
        intInPorts.add(port);
    }

    public void addIntOutPort(PortSpec port)
    {
        intOutPorts.add(port);
    }

    OtpErlangObject toErlang()
    {
        return new OtpErlangTuple(new OtpErlangObject[]
        {
            new OtpErlangAtom("een_interface_spec"),
            portArrayToErlang(extInPorts),
            portArrayToErlang(extOutPorts),
            portArrayToErlang(intInPorts),
            portArrayToErlang(intOutPorts),
        });
    }

    private static OtpErlangList portArrayToErlang(ArrayList<PortSpec> list)
    {
        if (list.isEmpty())
            return new OtpErlangList();
        
        OtpErlangObject[] erlangList = new OtpErlangObject[list.size()];
        int i = 0;
        for (PortSpec port : list)
            erlangList[i++] = port.toErlang();

        return new OtpErlangList(erlangList);
    }
}
