
package com.erleen.samples;

import com.erleen.Dispatcher;
import com.erleen.ErleenException;
import java.io.IOException;

public class TestMain
{
    public static void main(String[] args)
    {
        Dispatcher disp;
        try
        {
            disp = new Dispatcher("een_java", "een", "KQQGRUUVFWOECMSFLUPL");
            disp.run();
        }
        catch (IOException ex)
        {
            ex.printStackTrace();
        }
        catch (ErleenException ex)
        {
            ex.printStackTrace();
        }

        System.exit(0);
    }
}
