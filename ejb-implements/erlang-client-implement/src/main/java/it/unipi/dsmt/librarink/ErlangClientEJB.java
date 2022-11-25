package it.unipi.dsmt.librarink;

import com.ericsson.otp.erlang.*;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.reflect.TypeToken;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.ejb.*;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Type;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;
import java.util.stream.Collectors;

@Stateless
public class ErlangClientEJB implements ErlangClient {
    private Properties properties;
    private static final Logger LOGGER = Logger.getLogger(ErlangClientNodeEJB.class.getName());

    @EJB
    private ErlangClientNode node;

    @PostConstruct
    public void init() {
        LOGGER.info("Init ErlangClient");

        // Load parameters
        InputStream input = null;
        properties = new Properties();
        try {
            input = this.getClass().getClassLoader().getResourceAsStream("erlang-client.properties");
            properties.load(input);
        } catch (IOException e) {
            LOGGER.warning(e.getMessage());
        }
        finally {
            try {
                if (input != null)
                    input.close();
            } catch (IOException e) {
                LOGGER.warning(e.getMessage());
            }
        }
    }

    @PreDestroy
    public void destroy() {
        LOGGER.info("Destroy ErlangClient");
    }

    @Override
    public String write_copy(String isbn, String id) throws ErlangClientException {
        if (isbn == null || id == null)
            throw new ErlangClientException(properties.getProperty("bad_request"));

        OtpErlangAtom request = new OtpErlangAtom("write_copy");
        OtpErlangMap args = new OtpErlangMap();
        args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        args.put(new OtpErlangAtom("id"),new OtpErlangBinary(id.getBytes(StandardCharsets.UTF_8)));

        return sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public LoanDTO write_loan(String user, String isbn, String id) throws ErlangClientException {
        if (user == null || isbn == null || id == null)
            throw new ErlangClientException(properties.getProperty("bad_request"));

        OtpErlangAtom request = new OtpErlangAtom("write_loan");
        OtpErlangMap args = new OtpErlangMap();
        args.put(new OtpErlangAtom("user"),new OtpErlangBinary(user.getBytes(StandardCharsets.UTF_8)));
        args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        args.put(new OtpErlangAtom("id"),new OtpErlangBinary(id.getBytes(StandardCharsets.UTF_8)));

        return parseLoan(sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request, args})));
    }

    @Override
    public String write_reservation(String user, String isbn) throws ErlangClientException {
        if (user == null || isbn == null)
            throw  new ErlangClientException(properties.getProperty("bad_request"));

        OtpErlangAtom request = new OtpErlangAtom("write_reservation");
        OtpErlangMap args = new OtpErlangMap();
        args.put(new OtpErlangAtom("user"),new OtpErlangBinary(user.getBytes(StandardCharsets.UTF_8)));
        args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));

        return sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String delete_copy(String isbn, String id) throws ErlangClientException {
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
            throw new ErlangClientException(properties.getProperty("bad_request"));

        return sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request, args}));

    }

    @Override
    public String delete_loan(String user, String isbn, String id) throws ErlangClientException {
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
            throw new ErlangClientException(properties.getProperty("bad_request"));

        return sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String delete_reservation(String user, String isbn) throws ErlangClientException {
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
            throw new ErlangClientException(properties.getProperty("bad_request"));

        return sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public List<LoanDTO> archive_loans() throws ErlangClientException {
        OtpErlangAtom request = new OtpErlangAtom("archive_loans");
        OtpErlangMap args = new OtpErlangMap();
        String json_result = sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request,args}));
        return parseLoans(json_result);
    }

    @Override
    public List<ReservationDTO> archive_reservations() throws ErlangClientException {
        OtpErlangAtom request = new OtpErlangAtom("archive_reservations");
        OtpErlangMap args = new OtpErlangMap();
        String json_result = sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request,args}));
        return parseReservations(json_result);
    }

    @Override
    public List<BookCopyDTO> read_all_copies(String isbn) throws ErlangClientException {
        OtpErlangAtom request = new OtpErlangAtom("read_copies");
        OtpErlangMap args = new OtpErlangMap();

        args.put(new OtpErlangAtom("type"), new OtpErlangAtom("all"));
        if (isbn != null)
            args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));

        String json_result = sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
        return parseBookCopies(json_result);
    }

    @Override
    public List<BookCopyDTO> read_available_copies(String isbn) throws ErlangClientException {
        OtpErlangAtom request = new OtpErlangAtom("read_copies");
        OtpErlangMap args = new OtpErlangMap();

        if (isbn != null){
            args.put(new OtpErlangAtom("type"), new OtpErlangAtom("available"));
            args.put(new OtpErlangAtom("operation"), new OtpErlangAtom("list"));
            args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        }
        else{
            throw new ErlangClientException(properties.getProperty("bad_request"));
        }

        String json_result = sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
        return parseBookCopies(json_result);
    }

    @Override
    public Integer count_available_copies(String isbn) throws ErlangClientException {
        OtpErlangAtom request = new OtpErlangAtom("read_copies");
        OtpErlangMap args = new OtpErlangMap();

        if (isbn != null){
            args.put(new OtpErlangAtom("type"), new OtpErlangAtom("available"));
            args.put(new OtpErlangAtom("operation"), new OtpErlangAtom("count"));
            args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        }
        else{
            throw new ErlangClientException(properties.getProperty("bad_request"));
        }

        String json_result = sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
        return parseSimpleInt(json_result);
    }

    @Override
    public List<LoanDTO> read_loans(String user, String isbn, String id) throws ErlangClientException {
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
           throw new ErlangClientException(properties.getProperty("bad_request"));

        String json_result = sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
        return parseLoans(json_result);
    }

    @Override
    public List<LoanDTO> read_ended_loans() throws ErlangClientException {
        OtpErlangAtom request = new OtpErlangAtom("read_ended_loans");
        OtpErlangMap args = new OtpErlangMap();

        String json_result = sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
        return parseLoans(json_result);
    }

    @Override
    public List<ReservationDTO> read_reservations(String user, String isbn) throws ErlangClientException {
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

        String json_result = sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request, args}));

        return parseReservations(json_result);
    }

    @Override
    public List<ReservationDTO> read_ended_reservations() throws ErlangClientException {
        OtpErlangAtom request = new OtpErlangAtom("read_ended_reservations");
        OtpErlangMap args = new OtpErlangMap();

        String json_result = sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
        return parseReservations(json_result);
    }

    @Override
    public String terminate_loan(String isbn, String id) throws ErlangClientException {
        OtpErlangAtom request = new OtpErlangAtom("update_loan");
        OtpErlangMap args = new OtpErlangMap();

        if (isbn == null || id == null){
            throw new ErlangClientException(properties.getProperty("bad_request"));
        }
        args.put(new OtpErlangAtom("type"), new OtpErlangAtom("terminate"));
        args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        args.put(new OtpErlangAtom("id"),new OtpErlangBinary(id.getBytes(StandardCharsets.UTF_8)));


        return sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String renew_loan(String isbn, String id) throws ErlangClientException {
        OtpErlangAtom request = new OtpErlangAtom("update_loan");
        OtpErlangMap args = new OtpErlangMap();

        if (isbn == null || id == null){
            throw new ErlangClientException(properties.getProperty("bad_request"));
        }
        args.put(new OtpErlangAtom("type"), new OtpErlangAtom("renew"));
        args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        args.put(new OtpErlangAtom("id"),new OtpErlangBinary(id.getBytes(StandardCharsets.UTF_8)));

        return sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    @Override
    public String cancel_reservation(String user, String isbn) throws ErlangClientException {
        OtpErlangAtom request = new OtpErlangAtom("update_reservation");
        OtpErlangMap args = new OtpErlangMap();

        if (user == null || isbn == null){
            throw new ErlangClientException(properties.getProperty("bad_request"));
        }
        args.put(new OtpErlangAtom("type"), new OtpErlangAtom("cancel"));
        args.put(new OtpErlangAtom("user"),new OtpErlangBinary(user.getBytes(StandardCharsets.UTF_8)));
        args.put(new OtpErlangAtom("isbn"),new OtpErlangBinary(isbn.getBytes(StandardCharsets.UTF_8)));
        
        return sendRequest(new OtpErlangTuple(new OtpErlangObject[]{request, args}));
    }

    private String sendRequest(OtpErlangTuple request) throws ErlangClientException{
        String result = "";
        try {

            // Create unique reference to identify request
            OtpErlangRef tag = node.makeRef();

            OtpMbox mbox = node.getMbox();

            // Send request to erlang server
            mbox.send(
                    properties.getProperty("server_name"),
                    properties.getProperty("remote_node"),
                    new OtpErlangTuple(new OtpErlangObject[]{mbox.self(), tag, request})
            );

            // Receive request from erlang server
            OtpErlangObject response = mbox.receive(Integer.parseInt(properties.getProperty("timeout", "10000")));
            mbox.close();

            // Check and parse response
            if(response instanceof OtpErlangTuple && ((OtpErlangTuple) response).arity() == 2) {
                OtpErlangObject[] response_objs = ((OtpErlangTuple) response).elements();
                if (tag.equals(response_objs[0]) && response_objs[1] instanceof OtpErlangBinary)
                    result = OtpErlangString.newString(((OtpErlangBinary)response_objs[1]).binaryValue());
                else
                    throw new ErlangClientException("unexpected_error");
            }
            else{
               throw new ErlangClientException("unavailable_server");
            }
        }
        catch (OtpErlangDecodeException | OtpErlangExit ex){
            ex.printStackTrace();
        }
        return result;
    }

    private List<BookCopyDTO> parseBookCopies(String json) throws ErlangClientException {
        return parseDTO(json, new TypeToken<List<BookCopyDTO>>(){}.getType())
                .stream()
                .map(element -> ((BookCopyDTO)element))
                .collect(Collectors.toList());
    }

    private List<ReservationDTO> parseReservations(String json) throws ErlangClientException {
        return parseDTO(json, new TypeToken<List<ReservationDTO>>(){}.getType())
                .stream()
                .map(element -> ((ReservationDTO)element))
                .collect(Collectors.toList());
    }

    private List<LoanDTO> parseLoans(String json) throws ErlangClientException {
        return parseDTO(json, new TypeToken<List<LoanDTO>>(){}.getType())
                .stream()
                .map(element -> ((LoanDTO)element))
                .collect(Collectors.toList());
    }

    private LoanDTO parseLoan(String json) throws ErlangClientException {
        List<DTO> loan = parseDTO(json, new TypeToken<LoanDTO>(){}.getType());
        return (LoanDTO) loan.get(0);
    }

    private List<DTO> parseDTO(String json, Type collectionType) throws ErlangClientException {
        JsonObject result = new Gson().fromJson(json, JsonObject.class);
        if (result.get("result").getAsString().equals("succeed")){
            JsonObject response = new Gson().fromJson(result.get("response").toString(), JsonObject.class);
            if(response.has("values"))
                return new Gson().fromJson(response.get("values").toString(), collectionType);
            else
                return Collections.singletonList(new Gson().fromJson(response, collectionType));
        }
        else {
           throw new ErlangClientException(properties.getProperty("unexpected_error"));
        }
    }

    private Integer parseSimpleInt(String json){
        JsonObject result = new Gson().fromJson(json, JsonObject.class);
        if (result.get("result").getAsString().equals("succeed")){
            return new Gson().fromJson(result.get("response").toString(), Integer.class);
        }
        else {
            return 0;
        }
    }
}