package it.unipi.dsmt.servlet;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.google.common.reflect.TypeToken;
import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
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
import java.util.ArrayList;
import java.util.List;


@WebServlet(name = "AdminPageServlet", value = "/admin", loadOnStartup = 0)
public class AdminPageServlet extends HttpServlet {
    //private static final Logger LOGGER = Logger.getLogger(AsyncRequestServlet.class.getName());

    @EJB
    private ErlangClient erlang_client;
    @EJB
    private LibrarinkRemote remoteEJB;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        PrintWriter writer = response.getWriter();
        try {
            List<LoanDTO> loans;
            List<ReservationDTO> reservationDTOS;
            loans = erlang_client.read_loans(null, null, null);
            request.setAttribute("loanList", loans);
            reservationDTOS = erlang_client.read_reservations(null, null);
            request.setAttribute("reservationList", reservationDTOS);
            String TargetJSP = "/pages/jsp/admin_page.jsp";
            RequestDispatcher requestDispatcher = getServletContext().getRequestDispatcher(TargetJSP);
            requestDispatcher.forward(request, response);
        } catch (ErlangClientException ex) {
            writer.write(String.format("{\"error\":\"%s\"}", ex.getMessage()));
        }

    }
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        PrintWriter writer = response.getWriter();


        String[] reservation_parameter;
        String[] reservations_checkbox;
        String[] loan_parameter;
        String[] loan_checkbox;
        String type_request= (String) request.getAttribute("button");
        try {
            switch (type_request) {
                case "ConfirmReservation":
                    reservations_checkbox = request.getParameterValues("reservation");
                    if (reservations_checkbox != null && reservations_checkbox.length != 0) {
                        JsonArray newloanList = new JsonArray();

                        for (String reservationsCheckbox : reservations_checkbox) {

                            reservation_parameter = reservationsCheckbox.split(";");
                            LoanDTO newloanDTO = erlang_client.write_loan(reservation_parameter[0], reservation_parameter[1], reservation_parameter[3]);

                            ObjectWriter ow = new ObjectMapper().writer().withDefaultPrettyPrinter();
                            String newloan = ow.writeValueAsString(newloanDTO);
                            JsonObject jsonObject = new Gson().fromJson(newloan, new TypeToken<LoanDTO>() {
                            }.getType());
                            List<ReservationDTO> reservationDTOList =
                                    erlang_client.archive_reservations();
                            newloanList.add(jsonObject);
                            for (ReservationDTO reservationDTO : reservationDTOList) {
                                Librarink_history_reservationDTO history_reservationDTO = new Librarink_history_reservationDTO();
                                history_reservationDTO.setUser_email(reservationDTO.getUser());
                                history_reservationDTO.setIsbn(reservationDTO.getIsbn());
                                history_reservationDTO.setStart_date((Date) reservationDTO.getStartDate());
                                history_reservationDTO.setEnd_date((Date) reservationDTO.getStopDate());
                                history_reservationDTO.setDeleted(false);
                                remoteEJB.saveOrUpdateHistory_reservation(history_reservationDTO, false);
                            }
                        }
                        writer.write(newloanList.toString());
                    }
                    break;
                case "DeleteReservation":
                    reservations_checkbox = request.getParameterValues("reservation");
                    if (reservations_checkbox != null && reservations_checkbox.length != 0) {

                        for (String reservationsCheckbox : reservations_checkbox) {
                            reservation_parameter = reservationsCheckbox.split(";");
                            erlang_client.cancel_reservation(reservation_parameter[0], reservation_parameter[1]);
                            List<ReservationDTO> reservationDTOList = erlang_client.archive_reservations();
                            for (ReservationDTO reservationDTO : reservationDTOList) {
                                Librarink_history_reservationDTO history_reservationDTO = new Librarink_history_reservationDTO();
                                history_reservationDTO.setUser_email(reservationDTO.getUser());
                                history_reservationDTO.setIsbn(reservationDTO.getIsbn());
                                history_reservationDTO.setStart_date((Date) reservationDTO.getStartDate());
                                history_reservationDTO.setEnd_date((Date) reservationDTO.getStopDate());
                                history_reservationDTO.setDeleted(true);
                                remoteEJB.saveOrUpdateHistory_reservation(history_reservationDTO, false);
                            }

                        }

                    }
                    break;
                case "EndLoan":
                    loan_checkbox = request.getParameterValues("loan");
                    if (loan_checkbox != null && loan_checkbox.length != 0) {

                        for (String loanCheckbox : loan_checkbox) {
                            loan_parameter = loanCheckbox.split(";");

                            String s = erlang_client.terminate_loan(loan_parameter[0], loan_parameter[1]);
                            List<LoanDTO> loanDTOList = erlang_client.archive_loans();
                            for (LoanDTO loanDTO : loanDTOList) {
                                Librarink_history_loanDTO librarink_history_loanDTO = new Librarink_history_loanDTO();
                                librarink_history_loanDTO.setIsbn(loanDTO.getIsbn());
                                librarink_history_loanDTO.setStart_date((Date) loanDTO.getStartDate());
                                librarink_history_loanDTO.setUser_email(loanDTO.getUser());
                                librarink_history_loanDTO.setId_copy(loanDTO.getId());
                                librarink_history_loanDTO.setEnd_date((Date) loanDTO.getStopDate());
                                remoteEJB.saveOrUpdateHistory_loan(librarink_history_loanDTO, false);
                            }

                        }
                    }
                    //end loan
                    break;
                default:
                    break;
            }
        } catch (ErlangClientException e) {
            writer.write(String.format("{\"error\":\"%s\"}", e.getMessage()));
        }

    }
}

