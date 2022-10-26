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
import java.util.Locale;
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
        List<ReservationDTO> reservations;
        List<Librarink_usersDTO> usersDTOList=librarinkRemoteEJB.listUsers(new Librarink_usersDTO());
        loans = erlang_client.read_loans(null,null,null);
        request.setAttribute("loanList",loans);
        reservationDTOS= erlang_client.read_reservations(null,null);
        request.setAttribute("reservationList",reservationDTOS);
        String targetJPS ="librarink-web/src/main/webapp/pages/jsp/admin_page.jsp";

        RequestDispatcher requestDispatcher=request.getRequestDispatcher(targetJPS);
        try {
            requestDispatcher.forward(request,response);
        } catch (ServletException e) {
            throw new RuntimeException(e);
        }
    }
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String type_request= (String) request.getAttribute("type_request");
        switch (type_request) {
            case "loan_end":
                //do loan
                break;
            case "reservation_deleted":
                //do reservation
                break;
            default:
                break;
        }

    }
}

