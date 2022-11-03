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


@WebServlet(name = "AdminPageServlet", value = "/admin", loadOnStartup = 0)
public class AdminPageServlet extends HttpServlet {
    //private static final Logger LOGGER = Logger.getLogger(AsyncRequestServlet.class.getName());

    @EJB
    private ErlangClient erlang_client;
    @EJB
    private LibrarinkRemote remoteEJB;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        PrintWriter writer = response.getWriter();
        //LibrarinkRemoteEJB librarinkRemoteEJB=new LibrarinkRemoteEJB();
        List<LoanDTO> loans;
        List<ReservationDTO> reservationDTOS;
        List<ReservationDTO> reservations;
        List<Librarink_usersDTO> usersDTOList=remoteEJB.listUsers(new Librarink_usersDTO());
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
                        erlang_client.write_loan(reservation_parameter[0], reservation_parameter[1]);
                        List<ReservationDTO> reservationDTO=
                                erlang_client.read_reservations(reservation_parameter[0],reservation_parameter[1]);
                        Librarink_history_reservationDTO history_reservationDTO = new Librarink_history_reservationDTO();
                        history_reservationDTO.setUser_email(reservationDTO.get(0).getUser());
                        history_reservationDTO.setIsbn(reservationDTO.get(0).getIsbn());
                        history_reservationDTO.setStart_date((Date) reservationDTO.get(0).getStartDate());
                        history_reservationDTO.setEnd_date((Date) reservationDTO.get(0).getStopDate());
                        history_reservationDTO.setDeleted(false);
                        remoteEJB.saveOrUpdateHistory_reservation(history_reservationDTO,false);
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
                        List<ReservationDTO> reservationDTO=
                                erlang_client.read_reservations(reservation_parameter[0],reservation_parameter[1]);
                        Librarink_history_reservationDTO history_reservationDTO = new Librarink_history_reservationDTO();
                        history_reservationDTO.setUser_email(reservationDTO.get(0).getUser());
                        history_reservationDTO.setIsbn(reservationDTO.get(0).getIsbn());
                        history_reservationDTO.setStart_date((Date) reservationDTO.get(0).getStartDate());
                        history_reservationDTO.setEnd_date((Date) reservationDTO.get(0).getStopDate());
                        history_reservationDTO.setDeleted(reservationDTO.get(0).getCancelled());
                        erlang_client.delete_reservation(reservation_parameter[0],reservation_parameter[1]);
                    }
                }
                break;
            case "EndLoan":
                loan_checkbox = request.getParameterValues("loan");
                if (loan_checkbox != null && loan_checkbox.length != 0) {

                    for (String loanCheckbox : loan_checkbox) {
                        loan_parameter = loanCheckbox.split(";");

                        String s = erlang_client.terminate_loan(loan_parameter[0], loan_parameter[1]);
                        List<LoanDTO> loanDTOList = erlang_client.read_loans(loan_parameter[2],loan_parameter[0],loan_parameter[1]);
                        LoanDTO loanDTO = loanDTOList.get(0);
                        Librarink_history_loanDTO librarink_history_loanDTO=new Librarink_history_loanDTO();
                        librarink_history_loanDTO.setIsbn(loanDTO.getIsbn());
                        librarink_history_loanDTO.setStart_date((Date) loanDTO.getStartDate());
                        librarink_history_loanDTO.setUser_email(loanDTO.getUser());
                        librarink_history_loanDTO.setId_copy(loanDTO.getId());
                        librarink_history_loanDTO.setEnd_date((Date) loanDTO.getStopDate());
                        remoteEJB.saveOrUpdateHistory_loan(librarink_history_loanDTO,false);
                        erlang_client.delete_loan(loan_parameter[2],loan_parameter[0],loan_parameter[1]);

                    }
                }
                //end loan
                break;
            default:
                break;
        }

    }
}

