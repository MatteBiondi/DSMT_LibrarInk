package it.unipi.dsmt.servlet;

import it.unipi.dsmt.librarink.*;

import javax.ejb.EJB;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
@WebServlet(name = "AddBookCopy", value = "/adminAddBookCopy", loadOnStartup = 0)
public class AddBookCopy extends HttpServlet {

    @EJB
    private ErlangClient erlang_client;
    @EJB
    private LibrarinkRemote remoteEJB;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        String TargetJSP = "/pages/jsp/admin_page_add_book_copy.jsp";
        RequestDispatcher requestDispatcher = getServletContext().getRequestDispatcher(TargetJSP);
        requestDispatcher.forward(request, response);
    }
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
    {
    }
}
