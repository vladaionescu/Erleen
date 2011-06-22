/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.erleen;

/**
 *
 * @author aether
 */
public class PortSpec {

    public PortSpec()
    {
    }

    public PortSpec(String name, PortType type, MessageType msgType, int arrity)
    {
    }

    public enum PortType
    {
        BASIC, MULTI
    }

    public enum MessageType
    {
        CAST, CALL
    }
}
