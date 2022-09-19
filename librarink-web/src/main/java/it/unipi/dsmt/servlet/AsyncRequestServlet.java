package it.unipi.dsmt.servlet;

import it.unipi.dsmt.librarink.ErlangClient;

import javax.ejb.EJB;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.logging.Logger;

@WebServlet(name = "AsyncRequestServlet", value = "/request/async", loadOnStartup = 0)
public class AsyncRequestServlet extends HttpServlet {
    private static final Logger LOGGER = Logger.getLogger(AsyncRequestServlet.class.getName());

    @EJB
    private ErlangClient erlang_client;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        LOGGER.info(String.format(
                "Request from session-id: %s\nOperation: %s\nParams: <user: %s, isbn: %s, id: %s>",
                request.getSession().getId(),
                request.getParameter("request"),
                request.getSession().getAttribute("user"),
                request.getParameter("isbn"),
                request.getParameter("id")
        ));

        String user = (String) request.getSession().getAttribute("user");
        String isbn = request.getParameter("isbn");
        String id = request.getParameter("id");

        PrintWriter writer = response.getWriter();
        response.setContentType("application/json");

        switch (request.getParameter("request")){
            case "write_copy":
                writer.write(erlang_client.write_copy(isbn, id));
                break;
            case "write_loan":
                writer.write(erlang_client.write_loan(user, isbn));
                break;
            case "write_reservation":
                writer.write(erlang_client.write_reservation(user, isbn));
                break;
            case "delete_copy":
                writer.write(erlang_client.delete_copy(isbn, id));
                break;
            case "delete_loan":
                writer.write(erlang_client.delete_loan(user, isbn, id));
                break;
            case "delete_reservation":
                writer.write(erlang_client.delete_reservation(user, isbn));
                break;
            case "archive_loans":
                writer.write(erlang_client.archive_loans());
                break;
            case "archive_reservations":
                writer.write(erlang_client.archive_reservations());
                break;
            case "read_all_copies":
                writer.write(erlang_client.read_all_copies(isbn));
                break;
            case "read_available_copies":
                writer.write(erlang_client.read_available_copies(isbn));
                break;
            case "count_available_copies":
                writer.write(erlang_client.count_available_copies(isbn));
                break;
            case "read_loans":
                writer.write(erlang_client.read_loans(user, isbn, id));
                break;
            case "read_ended_loans":
                writer.write(erlang_client.read_ended_loans());
                break;
            case "read_reservations":
                writer.write(erlang_client.read_reservations(user, isbn));
                break;
            case "read_ended_reservations":
                writer.write(erlang_client.read_ended_reservations());
                break;
            case "terminate_loan":
                writer.write(erlang_client.terminate_loan(isbn, id));
                break;
            case "renew_loan":
                writer.write(erlang_client.renew_loan(isbn, id));
                break;
            case "cancel_reservation":
                writer.write(erlang_client.cancel_reservation(user, isbn));
                break;
            case "load_wishlist":
                writer.write("[]");//TODO: load from mysql
                break;
            default:
                writer.write("{\"error\":\"unexpected request\"");
        }
    }
}
