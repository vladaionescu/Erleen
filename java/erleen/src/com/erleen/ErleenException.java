
package com.erleen;

public class ErleenException extends Exception
{
    public ErleenException(String string)
    {
        super(string);
    }

    public ErleenException(Throwable e)
    {
        super(e);
    }
}
