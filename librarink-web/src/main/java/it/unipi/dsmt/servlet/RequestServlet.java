package it.unipi.dsmt.servlet;

import it.unipi.dsmt.librarink.ErlangClient;

import javax.ejb.EJB;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.logging.Logger;

@WebServlet(name = "RequestServlet", value = "/request", loadOnStartup = 0)
public class RequestServlet extends HttpServlet {
    private static final Logger LOGGER = Logger.getLogger(RequestServlet.class.getName());

    @EJB
    private ErlangClient erlang_client;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        LOGGER.info("Request from " + request.getRemoteAddr());

        StringBuilder sb = new StringBuilder();
        sb.append(erlang_client.write_copy("AAA","0002")).
                append(erlang_client.write_reservation("Federico", "AAA")).
                append(erlang_client.read_reservations("Federico", null)).
                append(erlang_client.read_all_copies("AAA"));

        response.setContentType("text/html");
        response.getWriter().write(sb.toString());
    }

}
