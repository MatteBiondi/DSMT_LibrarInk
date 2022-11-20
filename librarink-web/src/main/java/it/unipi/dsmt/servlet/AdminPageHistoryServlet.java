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
import java.sql.Date;
import java.util.List;


@WebServlet(name = "AdminPageHistoryServlet", value = "/adminHistory", loadOnStartup = 0)
public class AdminPageHistoryServlet extends HttpServlet {
    //private static final Logger LOGGER = Logger.getLogger(AsyncRequestServlet.class.getName());

    @EJB
    private LibrarinkRemote remoteEJB;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        List<Librarink_history_loanDTO> history_loans;
        List<Librarink_history_reservationDTO> historyReservationDTOS;
        history_loans = remoteEJB.listHistoryLoan(new Librarink_history_loanDTO());
        request.setAttribute("loanHistoryList",history_loans);
        historyReservationDTOS= remoteEJB.listHistoryReservation(new Librarink_history_reservationDTO());
        request.setAttribute("reservationHistoryList",historyReservationDTOS);
        String TargetJSP ="/pages/jsp/admin_page_history.jsp";
        RequestDispatcher requestDispatcher=request.getRequestDispatcher(TargetJSP);
        requestDispatcher.forward(request,response);

    }
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {




    }
}

