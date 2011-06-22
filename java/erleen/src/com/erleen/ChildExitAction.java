/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.erleen;

/**
 *
 * @author aether
 */
public class ChildExitAction
{
    static ChildExitAction shutdown(Reason reason)
    {
        return new ChildExitAction();
    }
    static ChildExitAction restart()
    {
        return new ChildExitAction();
    }
}
