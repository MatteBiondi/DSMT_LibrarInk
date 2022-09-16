package it.unipi.dsmt.librarink;

import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.ejb.Stateless;
import java.io.IOException;
import java.net.InetAddress;
import java.util.Properties;
import java.util.logging.Logger;

@Stateless
public class ErlangClientNodeEJB implements ErlangClientNode {
    private OtpNode node;
    private static final Logger LOGGER = Logger.getLogger(ErlangClientNodeEJB.class.getName());

    @PostConstruct
    public void init(){
        try{
            Properties properties = new Properties();
            properties.load(this.getClass().getClassLoader().getResourceAsStream("erlang-client.properties"));//InetAddress.getLocalHost().getHostAddress();
            node = new OtpNode(
                    properties.getProperty("name", "client") + this.hashCode() + "@" +
                            InetAddress.getLocalHost().getHostName(),
                    properties.getProperty("cookie", "no-cookie")
            );

            LOGGER.info("Init ErlangClientNode");
        }
        catch (IOException ex){
            ex.printStackTrace();
        }
    }

    @Override
    public OtpMbox getMbox() throws NullPointerException {
        return node.createMbox();
    }

    @Override
    public OtpErlangRef makeRef() throws NullPointerException{
        return node.createRef();
    }

    @PreDestroy
    public void destroy(){
        node.close();
        LOGGER.info("Destroy ErlangClientNode");
    }

}
