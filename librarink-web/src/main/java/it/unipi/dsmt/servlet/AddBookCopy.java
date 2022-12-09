package it.unipi.dsmt.servlet;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

@WebServlet(name = "AddBookCopy", value = "/adminAddBookCopy", loadOnStartup = 0)
public class AddBookCopy extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        String TargetJSP = "/pages/jsp/admin_book_copy.jsp";
        RequestDispatcher requestDispatcher = getServletContext().getRequestDispatcher(TargetJSP);
        requestDispatcher.forward(request, response);
    }
}
