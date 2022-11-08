package it.unipi.dsmt.servlet;

import it.unipi.dsmt.librarink.ErlangClient;
import it.unipi.dsmt.librarink.LibrarinkRemote;
import it.unipi.dsmt.librarink.Librarink_history_loanDTO;
import it.unipi.dsmt.librarink.Librarink_history_reservationDTO;

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

public class AddLoanAdminServlet {
    @WebServlet(name = "AddLoanAdminServlet", value = "/adminAddLoan", loadOnStartup = 0)
    public class AdminPageHistoryServlet extends HttpServlet {
        //private static final Logger LOGGER = Logger.getLogger(AsyncRequestServlet.class.getName());

        @EJB
        private ErlangClient erlang_client;
        @EJB
        private LibrarinkRemote remoteEJB;

        @Override
        protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {

        }
        @Override
        protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
        {
            String[] user;
            String[] isbn;
            user= request.getParameterValues("User");
            isbn=request.getParameterValues("ISBN");
            erlang_client.write_loan(user[0],isbn[0]);
        }
    }
}
