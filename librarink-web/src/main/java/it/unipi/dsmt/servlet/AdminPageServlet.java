package it.unipi.dsmt.servlet;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
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
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;


@WebServlet(name = "AdminPageServlet", value = "/admin", loadOnStartup = 0)
public class AdminPageServlet extends HttpServlet {
    private static final Logger LOGGER = Logger.getLogger(AsyncRequestServlet.class.getName());

    @EJB
    private ErlangClient erlang_client;
    @EJB
    private LibrarinkRemote remoteEJB;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        try {
            List<LoanDTO> loans;
            List<ReservationDTO> reservations;
            loans = erlang_client.read_loans(null, null, null);
            reservations = erlang_client.read_reservations(null, null);
            request.setAttribute("reservationList", reservations);
            request.setAttribute("loanList", loans);
            request.setAttribute("table", "active");

            SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            request.setAttribute("df", df );

            String TargetJSP = "/pages/jsp/admin_page.jsp";
            RequestDispatcher requestDispatcher = getServletContext().getRequestDispatcher(TargetJSP);
            requestDispatcher.forward(request, response);
        } catch (ErlangClientException ex) {
            LOGGER.warning(String.format("EJB exception %s", ex.getMessage()));
            throw new ServletException();
        }
    }
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws  IOException {
        PrintWriter writer = response.getWriter();

        String reservationParam;
        String separator=request.getParameter("separator");;
        String[] reservation_parameter;
        String[] reservations_checkbox;
        String loanParam;
        String[] loan_parameter;
        String[] loan_checkbox;
        String type_request = request.getParameter("button");

        response.setContentType("application/json");
        try {
            switch (type_request) {
                case "ConfirmReservation":
                    reservationParam = request.getParameter("reservation");
                    reservations_checkbox=reservationParam.split(separator);
                    if (reservations_checkbox.length != 0 && !reservationParam.equals("")) {
                        ArrayList<LoanDTO> loans = new ArrayList<>();
                        LoanDTO newLoanDTO;
                        // Confirm reservations
                        for (String reservationsCheckbox : reservations_checkbox) {
                            reservation_parameter = reservationsCheckbox.split(";");
                            newLoanDTO = erlang_client.write_loan(
                                    reservation_parameter[0],
                                    reservation_parameter[1],
                                    reservation_parameter[2]
                            );
                            loans.add(newLoanDTO);
                        }

                        // Archive reservations
                        List<ReservationDTO> reservationDTOList = erlang_client.archive_reservations();
                        for (ReservationDTO reservationDTO : reservationDTOList) {
                            HistoryReservationDTO history_reservationDTO = new HistoryReservationDTO();
                            history_reservationDTO.setUser(reservationDTO.getUser());
                            history_reservationDTO.setIsbn(reservationDTO.getIsbn());
                            history_reservationDTO.setStartDate(new Timestamp(reservationDTO.getStartDate().getTime()));
                            history_reservationDTO.setEndDate(new Timestamp(reservationDTO.getStopDate().getTime()));
                            history_reservationDTO.setDeleted(reservationDTO.getCancelled());
                            remoteEJB.saveOrUpdateHistoryReservation(history_reservationDTO, false);
                        }
                        JsonObject jsonResponse = new JsonObject();
                        JsonArray loansJS = new JsonArray();
                        for(LoanDTO loan: loans){
                            loansJS.add(new Gson().toJsonTree(loan));
                        }
                        jsonResponse.addProperty("result", "success");
                        jsonResponse.addProperty("response", "Reservation(s) confirmed");
                        jsonResponse.add("loans", loansJS);
                        writer.write(jsonResponse.toString());
                    }
                    else{
                        writer.write("{\"result\": \"error\", \"response\": \"No reservation selected\"}");
                    }
                    break;
                case "DeleteReservation":
                    reservationParam = request.getParameter("reservation");
                    reservations_checkbox=reservationParam.split(separator);
                    if (reservations_checkbox.length != 0 && !reservationParam.equals("")) {
                        for (String reservationsCheckbox : reservations_checkbox) {
                            reservation_parameter = reservationsCheckbox.split(";");
                            erlang_client.cancel_reservation(reservation_parameter[0], reservation_parameter[1]);
                            List<ReservationDTO> reservationDTOList = erlang_client.archive_reservations();
                            for (ReservationDTO reservationDTO : reservationDTOList) {
                                HistoryReservationDTO history_reservationDTO = new HistoryReservationDTO();
                                history_reservationDTO.setUser(reservationDTO.getUser());
                                history_reservationDTO.setIsbn(reservationDTO.getIsbn());
                                history_reservationDTO.setStartDate(new Timestamp( reservationDTO.getStartDate().getTime()));
                                history_reservationDTO.setEndDate(new Timestamp(reservationDTO.getStopDate().getTime()));
                                history_reservationDTO.setDeleted(reservationDTO.getCancelled());
                                remoteEJB.saveOrUpdateHistoryReservation(history_reservationDTO, false);
                            }
                        }
                        writer.write("{\"result\": \"success\", \"response\": \"Reservation(s) deleted\"}");
                    }
                    else{
                        writer.write("{\"result\": \"error\", \"response\": \"No reservation selected\"}");
                    }
                    break;
                case "EndLoan":
                    loanParam = request.getParameter("loan");
                    loan_checkbox = loanParam.split(separator);
                    if (loan_checkbox.length != 0 && !loanParam.equals("")) {
                        for (String loanCheckbox : loan_checkbox) {
                            loan_parameter = loanCheckbox.split(";");
                            erlang_client.terminate_loan(loan_parameter[0], loan_parameter[1]);
                            List<LoanDTO> loanDTOList = erlang_client.archive_loans();
                            for (LoanDTO loanDTO : loanDTOList) {
                                HistoryLoanDTO librarink_history_loanDTO = new HistoryLoanDTO();
                                librarink_history_loanDTO.setIsbn(loanDTO.getIsbn());
                                librarink_history_loanDTO.setStartDate(new Timestamp(loanDTO.getStartDate().getTime()));
                                librarink_history_loanDTO.setUser(loanDTO.getUser());
                                librarink_history_loanDTO.setCopyId(loanDTO.getCopyId());
                                librarink_history_loanDTO.setEndDate(new Timestamp(loanDTO.getStopDate().getTime()));
                                remoteEJB.saveOrUpdateHistoryLoan(librarink_history_loanDTO, false);
                            }
                        }
                        writer.write("{\"result\": \"success\", \"response\": \"Loan(s) terminated\"}");
                    }
                    else{
                        writer.write("{\"result\": \"error\", \"response\": \"No loan selected\"}");
                    }
                    break;
                default:
                    break;
            }
        } catch (ErlangClientException | RemoteDBException ex) {
            LOGGER.warning(String.format("EJB exception %s", ex.getMessage()));
            writer.write("{\"result\": \"error\", \"response\": \"server error\"}");
        }
    }
}

