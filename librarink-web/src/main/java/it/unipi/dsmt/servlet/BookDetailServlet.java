package it.unipi.dsmt.servlet;

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
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        LOGGER.info(String.format(
                "Request from session-id: %s\nParams: < user: %s, isbn: %s >",
                request.getSession().getId(),
                request.getSession().getAttribute("user"),
                request.getParameter("isbn")
        ));
        String isbn = request.getParameter("isbn");
        Librarink_booksDTO book = remote.findBooksByIsbn(isbn);
        LOGGER.info(book.toString());
        response.setContentType("text/html");
        request.setAttribute("book", book);
        getServletContext().getRequestDispatcher("/pages/jsp/book_detail.jsp").forward(request, response);
    }
}
