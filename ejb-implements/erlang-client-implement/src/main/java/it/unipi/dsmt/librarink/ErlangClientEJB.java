package it.unipi.dsmt.librarink;

import com.ericsson.otp.erlang.*;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.ejb.*;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Properties;
import java.util.logging.Logger;

@Stateless
public class ErlangClientEJB implements ErlangClient {
    private Properties properties;
    private static final Logger LOGGER = Logger.getLogger(ErlangClientNodeEJB.class.getName());

    @EJB
    private ErlangClientNode node;

    @PostConstruct
    public void init() {
        properties = new Properties();
        try {
            properties.load(this.getClass().getClassLoader().getResourceAsStream("erlang-client.properties"));
        } catch (IOException e) {
            e.printStackTrace();
        }
        LOGGER.info("Init ErlangClient");
    }

    @PreDestroy
    public void destroy() {
        LOGGER.info("Destroy ErlangClientNode");
    }

    private String send_request(OtpErlangTuple request) {
        String result;
        try{

            // Create unique reference to identify request
            OtpErlangRef tag = node.makeRef();

            OtpMbox mbox = node.getMbox();

            // Send request to erlang server
            mbox.send(
                    properties.getProperty("server_name"),
                    properties.getProperty("remote_node"),
                    new OtpErlangTuple(new OtpErlangObject[]{mbox.self(),
                            tag, request}));

            // Receive request from erlang server
            OtpErlangObject response = mbox.receive(Integer.parseInt(
                    properties.getProperty("timeout", "30000"))
            );
            mbox.close();

            // Check and parse response
            if(response instanceof OtpErlangTuple && ((OtpErlangTuple) response).arity() == 2) {
                OtpErlangObject[] response_objs = ((OtpErlangTuple) response).elements();
                if (tag.equals(response_objs[0]) && response_objs[1] instanceof OtpErlangBinary)
                    result = OtpErlangString.newString(((OtpErlangBinary)response_objs[1]).binaryValue());
                else
                    result = properties.getProperty("unexpected_error");
            }
            else
                result = properties.getProperty("unavailable_server");

        }
        catch (OtpErlangDecodeException | OtpErlangExit | NullPointerException ex){
            ex.printStackTrace();
            result = properties.getProperty("unavailable_server");
        }

        return result + " @ " + hashCode();
    }

    @Override
    public String write_copy(String isbn, String id) {
        if (isbn == null || id == null)
            return "bad request";

        OtpErlangAtom request = new OtpErlangAtom("write_copy");
        OtpErlangMap args = new OtpErlangMap();
        args.put(new OtpErlangAtom("isbn"), new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        args.put(new OtpErlangAtom("id"), new OtpErlangBinary(id.getBytes(StandardCharsets.UTF_8)));

        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String write_loan(String user, String isbn) {
        if (user == null || isbn == null)
            return "bad request";

        OtpErlangAtom request = new OtpErlangAtom("write_loan");
        OtpErlangMap args = new OtpErlangMap();
        args.put(new OtpErlangAtom("user"),new OtpErlangBinary(user.getBytes(StandardCharsets.UTF_8)));
        args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));

        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String write_reservation(String user, String isbn) {
        if (user == null || isbn == null)
            return "bad request";

        OtpErlangAtom request = new OtpErlangAtom("write_reservation");
        OtpErlangMap args = new OtpErlangMap();
        args.put(new OtpErlangAtom("user"),new OtpErlangBinary(user.getBytes(StandardCharsets.UTF_8)));
        args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));

        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String delete_copy(String isbn, String id) {
        OtpErlangAtom request = new OtpErlangAtom("delete_copy");
        OtpErlangMap args = new OtpErlangMap();
        if (isbn != null && id != null){
            args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
            args.put(new OtpErlangAtom("id"),new OtpErlangBinary(id.getBytes(StandardCharsets.UTF_8)));
        }
        else if (id != null){
            args.put(new OtpErlangAtom("id"),new OtpErlangBinary(id.getBytes(StandardCharsets.UTF_8)));
        }
        else
            return "bad request";

        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request, args}));

    }

    @Override
    public String delete_loan(String user, String isbn, String id) {
        OtpErlangAtom request = new OtpErlangAtom("delete_loan");
        OtpErlangMap args = new OtpErlangMap();

        if (user != null && isbn != null && id != null){
            args.put(new OtpErlangAtom("user"),new OtpErlangBinary(user.getBytes(StandardCharsets.UTF_8)));
            args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
            args.put(new OtpErlangAtom("id"),new OtpErlangBinary(id.getBytes(StandardCharsets.UTF_8)));
        }
        else if(user != null && isbn == null && id == null){
            args.put(new OtpErlangAtom("user"),new OtpErlangBinary(user.getBytes(StandardCharsets.UTF_8)));
        }
        else if(user == null && isbn != null && id == null){
            args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        }
        else
            return "bad request";

        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String delete_reservation(String user, String isbn) {
        OtpErlangAtom request = new OtpErlangAtom("delete_reservation");
        OtpErlangMap args = new OtpErlangMap();

        if (user != null && isbn != null){
            args.put(new OtpErlangAtom("user"),new OtpErlangBinary(user.getBytes(StandardCharsets.UTF_8)));
            args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        }
        else if(user != null){
            args.put(new OtpErlangAtom("user"),new OtpErlangBinary(user.getBytes(StandardCharsets.UTF_8)));
        }
        else if(isbn != null){
            args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        }
        else
            return "bad request";

        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String archive_loans() {
        OtpErlangAtom request = new OtpErlangAtom("archive_loans");
        OtpErlangMap args = new OtpErlangMap();
        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request,args}));
    }

    @Override
    public String archive_reservations() {
        OtpErlangAtom request = new OtpErlangAtom("archive_reservations");
        OtpErlangMap args = new OtpErlangMap();
        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request,args}));
    }

    @Override
    public String read_all_copies(String isbn) {
        OtpErlangAtom request = new OtpErlangAtom("read_copies");
        OtpErlangMap args = new OtpErlangMap();

        args.put(new OtpErlangAtom("type"), new OtpErlangAtom("all"));
        if (isbn != null)
            args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));

        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String read_available_copies(String isbn) {
        OtpErlangAtom request = new OtpErlangAtom("read_copies");
        OtpErlangMap args = new OtpErlangMap();

        if (isbn != null){
            args.put(new OtpErlangAtom("type"), new OtpErlangAtom("available"));
            args.put(new OtpErlangAtom("operation"), new OtpErlangAtom("list"));
            args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        }
        else
            return "bad request";

        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String count_available_copies(String isbn) {
        OtpErlangAtom request = new OtpErlangAtom("read_copies");
        OtpErlangMap args = new OtpErlangMap();

        if (isbn != null){
            args.put(new OtpErlangAtom("type"), new OtpErlangAtom("available"));
            args.put(new OtpErlangAtom("operation"), new OtpErlangAtom("count"));
            args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        }
        else
            return "bad request";

        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String read_loans(String user, String isbn, String id) {
        OtpErlangAtom request = new OtpErlangAtom("read_loans");
        OtpErlangMap args = new OtpErlangMap();

        if (user == null && isbn != null && id != null){
            args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
            args.put(new OtpErlangAtom("id"),new OtpErlangBinary(id.getBytes(StandardCharsets.UTF_8)));
        }
        else if(user == null && isbn != null){
            args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        }
        else if(user != null && isbn == null && id == null){
            args.put(new OtpErlangAtom("user"),new OtpErlangBinary(user.getBytes(StandardCharsets.UTF_8)));
        }
        else if (user != null && isbn != null)
            return "bad request";

        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String read_ended_loans() {
        OtpErlangAtom request = new OtpErlangAtom("read_ended_loans");
        OtpErlangMap args = new OtpErlangMap();

        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String read_reservations(String user, String isbn) {
        OtpErlangAtom request = new OtpErlangAtom("read_reservations");
        OtpErlangMap args = new OtpErlangMap();

        if (user != null && isbn != null){
            args.put(new OtpErlangAtom("user"),new OtpErlangBinary(user.getBytes(StandardCharsets.UTF_8)));
            args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        }
        else if(user != null){
            args.put(new OtpErlangAtom("user"),new OtpErlangBinary(user.getBytes(StandardCharsets.UTF_8)));
        }
        else if(isbn != null){
            args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        }

        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String read_ended_reservations() {
        OtpErlangAtom request = new OtpErlangAtom("read_ended_reservations");
        OtpErlangMap args = new OtpErlangMap();

        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String terminate_loan(String isbn, String id) {
        OtpErlangAtom request = new OtpErlangAtom("update_loan");
        OtpErlangMap args = new OtpErlangMap();

        if (isbn == null || id == null){
            return "bad request";
        }
        args.put(new OtpErlangAtom("type"), new OtpErlangAtom("terminate"));
        args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        args.put(new OtpErlangAtom("id"),new OtpErlangBinary(id.getBytes(StandardCharsets.UTF_8)));


        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String renew_loan(String isbn, String id) {
        OtpErlangAtom request = new OtpErlangAtom("update_loan");
        OtpErlangMap args = new OtpErlangMap();

        if (isbn == null || id == null){
            return "bad request";
        }
        args.put(new OtpErlangAtom("type"), new OtpErlangAtom("renew"));
        args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        args.put(new OtpErlangAtom("id"),new OtpErlangBinary(id.getBytes(StandardCharsets.UTF_8)));


        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String cancel_reservation(String user, String isbn) {
        OtpErlangAtom request = new OtpErlangAtom("update_reservation");
        OtpErlangMap args = new OtpErlangMap();

        if (user == null || isbn == null){
            return "bad request";
        }
        args.put(new OtpErlangAtom("type"), new OtpErlangAtom("cancel"));
        args.put(new OtpErlangAtom("user"),new OtpErlangBinary(user.getBytes(StandardCharsets.UTF_8)));
        args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));


        return send_request(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }
}