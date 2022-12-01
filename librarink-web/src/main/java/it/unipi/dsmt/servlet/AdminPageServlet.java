package it.unipi.dsmt.servlet;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
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
import java.util.logging.Logger;


@WebServlet(name = "AdminPageServlet", value = "/admin", loadOnStartup = 0)
public class AdminPageServlet extends HttpServlet {
    private static final Logger LOGGER = Logger.getLogger(AsyncRequestServlet.class.getName());
    private ObjectWriter ow = new ObjectMapper().writer().withDefaultPrettyPrinter();

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
            LOGGER.warning(String.format("EJB exception %s", ex.getMessage()));
            writer.write("{\"result\": \"error\", \"response\": \"server error\"}");
        }

    }
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        PrintWriter writer = response.getWriter();

        String reservationResponse;
        String separator=request.getParameter("separator");;
        String[] reservation_parameter;
        String[] reservations_checkbox;
        String loanResponse;
        String[] loan_parameter;
        String[] loan_checkbox;

        String type_request= (String) request.getParameter("button");
        if(type_request==null)
        {
            LOGGER.info("request is null");
        }
        LOGGER.info("type request:"+type_request);
        try {
            switch (type_request) {
                case "ConfirmReservation":
                    String sep ="";
                    reservationResponse = request.getParameter("reservation");
                    LOGGER.info("reservation value: "+reservationResponse);
                    LOGGER.info("separator value: "+separator);
                    reservations_checkbox=reservationResponse.split(separator);
                    if (reservations_checkbox != null && reservations_checkbox.length != 0) {
                        LOGGER.info("reservation list is not null");
                        String newloanList = "[";
                        for (String reservationsCheckbox : reservations_checkbox) {
                            LOGGER.info("reservation parameter checkbox:"+reservationsCheckbox);
                            reservation_parameter = reservationsCheckbox.split(";");
                            LoanDTO newloanDTO = erlang_client.write_loan(reservation_parameter[0], reservation_parameter[1], reservation_parameter[2]);
                            LOGGER.info("new LOAN:"+ newloanDTO.toString());
                            String newloan = ow.writeValueAsString(newloanDTO);
                            LOGGER.info("loan:"+newloan);
                            List<ReservationDTO> reservationDTOList =
                                    erlang_client.archive_reservations();
                            newloanList+= sep +newloan;
                            sep =",";
                            for (ReservationDTO reservationDTO : reservationDTOList) {
                                HistoryReservationDTO history_reservationDTO = new HistoryReservationDTO();
                                history_reservationDTO.setUser(reservationDTO.getUser());
                                history_reservationDTO.setIsbn(reservationDTO.getIsbn());
                                history_reservationDTO.setStartDate(new java.sql.Date(reservationDTO.getStartDate().getTime()));
                                history_reservationDTO.setEndDate(new java.sql.Date(reservationDTO.getStopDate().getTime()));
                                history_reservationDTO.setDeleted(false);
                                remoteEJB.saveOrUpdateHistoryReservation(history_reservationDTO, false);
                            }
                        }
                        newloanList+="]";
                        LOGGER.info("new loan list: "+newloanList.toString());
                        writer.write(newloanList.toString());
                    }
                    else{
                        writer.write("{\"result\": \"error\", \"response\": \"no row selected\"}");
                    }

                    break;
                case "DeleteReservation":
                    reservationResponse = (String) request.getParameter("reservation");
                    reservations_checkbox=reservationResponse.split(separator);
                    if (reservations_checkbox != null && reservations_checkbox.length != 0) {
                        LOGGER.info("reservation list is not null");
                        for (String reservationsCheckbox : reservations_checkbox) {
                            LOGGER.info("reservation parameter:"+reservationsCheckbox);
                            reservation_parameter = reservationsCheckbox.split(";");
                            erlang_client.cancel_reservation(reservation_parameter[0], reservation_parameter[1]);
                            List<ReservationDTO> reservationDTOList = erlang_client.archive_reservations();
                            for (ReservationDTO reservationDTO : reservationDTOList) {
                                HistoryReservationDTO history_reservationDTO = new HistoryReservationDTO();
                                history_reservationDTO.setUser(reservationDTO.getUser());
                                history_reservationDTO.setIsbn(reservationDTO.getIsbn());
                                history_reservationDTO.setStartDate(new java.sql.Date( reservationDTO.getStartDate().getTime()));
                                history_reservationDTO.setEndDate(new java.sql.Date(reservationDTO.getStopDate().getTime()));
                                history_reservationDTO.setDeleted(true);
                                remoteEJB.saveOrUpdateHistoryReservation(history_reservationDTO, false);
                            }

                        }
                        writer.write("{\"result\": \"correct\", \"response\": \"Delete reservation is executed correctly\"}");
                    }
                    else{
                        writer.write("{\"result\": \"error\", \"response\": \"no row selected\"}");
                    }
                    break;
                case "EndLoan":
                    loanResponse = (String) request.getParameter("loan");
                    loan_checkbox = loanResponse.split(separator);
                    if (loan_checkbox != null && loan_checkbox.length != 0) {
                        LOGGER.info("loan list is not null");
                        for (String loanCheckbox : loan_checkbox) {
                            LOGGER.info("reservation parameter:"+loanCheckbox);
                            loan_parameter = loanCheckbox.split(";");
                            String s = erlang_client.terminate_loan(loan_parameter[0], loan_parameter[1]);
                            List<LoanDTO> loanDTOList = erlang_client.archive_loans();
                            for (LoanDTO loanDTO : loanDTOList) {
                                HistoryLoanDTO librarink_history_loanDTO = new HistoryLoanDTO();
                                librarink_history_loanDTO.setIsbn(loanDTO.getIsbn());
                                librarink_history_loanDTO.setStartDate((Date) loanDTO.getStartDate());
                                librarink_history_loanDTO.setUser(loanDTO.getUser());
                                librarink_history_loanDTO.setCopyId(loanDTO.getCopyId());
                                librarink_history_loanDTO.setEndDate((Date) loanDTO.getStopDate());
                                remoteEJB.saveOrUpdateHistoryLoan(librarink_history_loanDTO, false);

                            }

                        }
                        writer.write("{\"result\": \"correct\", \"response\": \"EndLoan is executed correctly\"}");
                    }
                    else{
                        writer.write("{\"result\": \"error\", \"response\": \"no row selected\"}");
                    }

                    //end loan
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

