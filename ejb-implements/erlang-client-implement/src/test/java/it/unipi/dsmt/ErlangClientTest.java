package it.unipi.dsmt;

import it.unipi.dsmt.librarink.ErlangClient;
import it.unipi.dsmt.librarink.ErlangClientEJB;
import it.unipi.dsmt.librarink.ErlangClientNodeEJB;
import org.junit.Test;

import java.io.IOException;

public class ErlangClientTest {
    public final int N_WORKER = 1;
    public final ErlangClientNodeEJB client;
    public final Thread[] workers;

    public ErlangClientTest(){
        client = new ErlangClientNodeEJB();
        client.init();
        workers = new Thread[N_WORKER];
    }

    public void test() {
       client.destroy();
        //for (int i = 0; i < N_WORKER; ++i) {
        //    workers[i] = new Thread(() -> System.out.println(client.read_all_copies("AAA")));
        //    workers[i].start();
        //}
//
        //for(Thread worker: workers) {
        //    try {
        //        worker.join();
        //    } catch (InterruptedException e) {
        //        e.printStackTrace();
        //    }
        //}
        //((ErlangClientEJB) client).destroy();
    }
}
