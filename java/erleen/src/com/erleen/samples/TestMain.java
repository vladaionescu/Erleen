
package com.erleen.samples;

import com.erleen.Dispatcher;
import com.erleen.ErleenException;
import java.io.IOException;

public class TestMain
{
    public static void main(String[] args) throws IOException, ErleenException
    {
        System.out.println("Attempting connection to Erleen...");
        Dispatcher disp =
                new Dispatcher("een_java", "een", "KQQGRUUVFWOECMSFLUPL");
        disp.run();
    }
}
