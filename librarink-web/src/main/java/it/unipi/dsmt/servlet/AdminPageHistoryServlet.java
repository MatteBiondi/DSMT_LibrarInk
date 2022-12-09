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
import java.util.Date;
import java.text.SimpleDateFormat;
import java.util.List;

@WebServlet(name = "AdminPageHistoryServlet", value = "/adminHistory", loadOnStartup = 0)
public class AdminPageHistoryServlet extends HttpServlet {

    @EJB
    private LibrarinkRemote remoteEJB;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        try {
            List<HistoryLoanDTO> history_loans;
            List<HistoryReservationDTO> history_reservations;
            history_loans = remoteEJB.listHistoryLoans(new HistoryLoanDTO());
            history_reservations= remoteEJB.listHistoryReservations(new HistoryReservationDTO());
            request.setAttribute("reservationList",history_reservations);
            request.setAttribute("loanList", history_loans);
            request.setAttribute("table", "history");
            SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
            request.setAttribute("df", df );
            String TargetJSP ="/pages/jsp/admin_page.jsp";
            RequestDispatcher requestDispatcher = request.getRequestDispatcher(TargetJSP);
            requestDispatcher.forward(request,response);
        }
        catch (RemoteDBException ex){
            PrintWriter writer = response.getWriter();
            writer.write(String.format("%s", ex.getMessage()));
        }
    }
}