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
import java.util.logging.Logger;

@WebServlet(name = "admin_page_servlet", value = "", loadOnStartup = 0)
public class admin_page_servlet extends HttpServlet {
    //private static final Logger LOGGER = Logger.getLogger(AsyncRequestServlet.class.getName());

    @EJB
    private ErlangClient erlang_client;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        PrintWriter writer = response.getWriter();
        LibrarinkRemoteEJB librarinkRemoteEJB=new LibrarinkRemoteEJB();
        List<LoanDTO> loans;
        List<ReservationDTO> reservationDTOS;
        List<LoanDTO> loansResponse = null;
        List<ReservationDTO> reservationResponse = null;
        List<ReservationDTO> reservations;
        List<Librarink_usersDTO> usersDTOList=librarinkRemoteEJB.listUsers(new Librarink_usersDTO());
        for(Librarink_usersDTO user:usersDTOList)
        {
            loans = erlang_client.read_loans(user.getName(),null,null);
            loansResponse.addAll(loans);

        }
        request.setAttribute("loanList",loansResponse);
        for(Librarink_usersDTO user:usersDTOList)
        {
            reservationDTOS= erlang_client.read_reservations(user.getName(),null);
            reservationResponse.addAll(reservationDTOS);

        }
        request.setAttribute("reservationList",reservationResponse);
        String targetJPS ="librarink-web/src/main/webapp/pages/jsp/admin_page.jsp";

        RequestDispatcher requestDispatcher=request.getRequestDispatcher(targetJPS);
        try {
            requestDispatcher.forward(request,response);
        } catch (ServletException e) {
            throw new RuntimeException(e);
        }
    }

}

