package it.unipi.dsmt.servlet;

import it.unipi.dsmt.librarink.*;

import javax.ejb.EJB;
import javax.ejb.EJBException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.logging.Logger;

/**
 * This class allow a user to see the detailed page for a selected book
 */
@WebServlet(name = "BookDetailServlet", value = "/book_detail", loadOnStartup = 0)
public class BookDetailServlet extends HttpServlet {
    private static final Logger LOGGER = Logger.getLogger(BookDetailServlet.class.getName());

    @EJB
    LibrarinkRemote remote;
    @EJB
    private ErlangClient erlang_client;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        PrintWriter writer = response.getWriter();
        String isbn = request.getParameter("isbn");
        try {
            // Load data from DB
            Integer available_copies = erlang_client.count_available_copies(isbn);
            BookDTO book = remote.findBooksByIsbn(isbn);
            Double rating = remote.computeRating(isbn);

            // Prepare data for JSP page
            response.setContentType("text/html");
            request.setAttribute("book", book);
            request.setAttribute("rating", rating);
            request.setAttribute("available_copies", available_copies);
            getServletContext().getRequestDispatcher("/pages/jsp/book_detail.jsp").forward(request, response);
        }
        catch (ErlangClientException | RemoteDBException ex){
            response.setContentType("application/json");
            writer.write(String.format("%s", ex.getMessage()));
        }
        catch (EJBException ex){
            LOGGER.warning(String.format("EJB exception %s", ex.getMessage()));
            response.setContentType("application/json");
            writer.write("{\"result\": \"error\", \"response\": \"server error\"}");
        }

    }
}
