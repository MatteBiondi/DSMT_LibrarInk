package it.unipi.dsmt.servlet;

import it.unipi.dsmt.librarink.*;
import it.unipi.dsmt.librarink.entities.History_reservation;

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


@WebServlet(name = "admin_page_servlet", value = "", loadOnStartup = 0)
public class admin_page_servlet extends HttpServlet {
    //private static final Logger LOGGER = Logger.getLogger(AsyncRequestServlet.class.getName());

    @EJB
    private ErlangClient erlang_client;
    private LibrarinkRemoteEJB remoteEJB;

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
        History_reservation history_reservation_filter;
        String[] reservation_parameter;
        String[] reservations_checkbox;
        String[] loan_parameter;
        String[] loan_checkbox;
        String type_request= (String) request.getAttribute("button");
        switch (type_request) {
            case "ConfirmReservation":
                reservations_checkbox = request.getParameterValues("reservation");
                if (reservations_checkbox != null && reservations_checkbox.length != 0) {

                    for (String reservationsCheckbox : reservations_checkbox) {
                        reservation_parameter = reservationsCheckbox.split(";");

                        //ToDo archive a reservation
                        erlang_client.write_loan(reservation_parameter[0], reservation_parameter[1]);
                        erlang_client.archive_reservations();
                    }
                }
                break;
            case "DeleteReservation":
                reservations_checkbox = request.getParameterValues("reservation");
                if (reservations_checkbox != null && reservations_checkbox.length != 0) {

                    for (String reservationsCheckbox : reservations_checkbox) {
                        reservation_parameter = reservationsCheckbox.split(";");
                        erlang_client.cancel_reservation(reservation_parameter[0], reservation_parameter[1]);
                    }
                }
                break;
            case "EndLoan":
                loan_checkbox = request.getParameterValues("loan");
                if (loan_checkbox != null && loan_checkbox.length != 0) {

                    for (String loanCheckbox : loan_checkbox) {
                        loan_parameter = loanCheckbox.split(";");
                        //ToDo archive a reservation
                        String s = erlang_client.terminate_loan(loan_parameter[0], loan_parameter[1]);
                    }
                }
                //end loan
                break;
            default:
                break;
        }

    }
}

