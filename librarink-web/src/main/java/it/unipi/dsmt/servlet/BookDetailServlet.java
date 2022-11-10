package it.unipi.dsmt.servlet;

import it.unipi.dsmt.librarink.ErlangClient;
import it.unipi.dsmt.librarink.LibrarinkRemote;
import it.unipi.dsmt.librarink.Librarink_booksDTO;

import javax.ejb.EJB;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.logging.Logger;

@WebServlet(name = "BookDetailServlet", value = "/book_detail", loadOnStartup = 0)
public class BookDetailServlet extends HttpServlet {
    private static final Logger LOGGER = Logger.getLogger(BookDetailServlet.class.getName());
    @EJB
    LibrarinkRemote remote;
    @EJB
    private ErlangClient erlang_client;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        //LOGGER.info(String.format(
        //        "Request from session-id: %s\nParams: < user: %s, isbn: %s >",
        //        request.getSession().getId(),
        //        request.getSession().getAttribute("user"),
        //        request.getParameter("isbn")
        //));
        String isbn = request.getParameter("isbn");

        // Load data from DB
        Integer available_copies = erlang_client.count_available_copies(isbn);
        Librarink_booksDTO book = remote.findBooksByIsbn(isbn);
        Double rating = remote.computeRating(isbn);

        // Prepare data for JSP page
        response.setContentType("text/html");
        request.setAttribute("book", book);
        request.setAttribute("rating", rating);
        request.setAttribute("available_copies", available_copies);
        getServletContext().getRequestDispatcher("/pages/jsp/book_detail.jsp").forward(request, response);
    }
}
