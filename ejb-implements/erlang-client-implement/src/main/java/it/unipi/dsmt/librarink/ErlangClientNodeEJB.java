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
    private String clientName;
    private String cookie;
    private static final Logger LOGGER = Logger.getLogger(ErlangClientNodeEJB.class.getName());

    @PostConstruct
    public void init(){
        LOGGER.info("Init ErlangClientNode");

        InputStream input = null;
        try{
            Properties properties = new Properties();
            input = this.getClass().getClassLoader().getResourceAsStream("erlang-client.properties");
            properties.load(input);
            clientName = properties.getProperty("name", "client");
            cookie =  properties.getProperty("cookie", "no-cookie");
            node = getConnection();
        }
        catch (IOException ex){
            node = null;
            LOGGER.warning(String.format("Impossible establish connection to Erlang server: %s", ex.getMessage()));
        }
        finally {
            try {
                if (input != null)
                    input.close();
            } catch (IOException ex) {
                LOGGER.warning(ex.getMessage());
            }
        }
    }
    private OtpNode getConnection() throws IOException{
        if(node == null)
            return new OtpNode(clientName + "-" + this.hashCode(), cookie);
        else
            return node;
    }

    @Override
    public OtpMbox getMbox() throws ErlangClientException {
        try{
           return getConnection().createMbox();
        }
        catch (NullPointerException | IOException ex){
            node = null;
            throw new ErlangClientException("Unavailable server");
        }
    }

    @Override
    public OtpErlangRef makeRef() throws ErlangClientException {
        try{
            return getConnection().createRef();
        }
        catch (NullPointerException | IOException ex){
            node = null;
            throw new ErlangClientException("Unavailable server");
        }
    }

    @PreDestroy
    public void destroy() {
        LOGGER.info("Destroy ErlangClientNode");
        if(node != null)
            node.close();
    }

}
