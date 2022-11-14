package it.unipi.dsmt.librarink;

import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.ejb.Stateless;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Logger;

@Stateless
public class ErlangClientNodeEJB implements ErlangClientNode {
    private OtpNode node;
    private static final Logger LOGGER = Logger.getLogger(ErlangClientNodeEJB.class.getName());

    @PostConstruct
    public void init(){
        InputStream input = null;
        try{
            Properties properties = new Properties();
            input = this.getClass().getClassLoader().getResourceAsStream("erlang-client.properties");
            properties.load(input);
            node = new OtpNode(
                    properties.getProperty("name", "client") + "-" + this.hashCode(),
                    properties.getProperty("cookie", "no-cookie")
            );
            LOGGER.info("Init ErlangClientNode");
        }
        catch (IOException ex){
            ex.printStackTrace();
        }
        finally {
            try {
                if (input != null)
                    input.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
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
