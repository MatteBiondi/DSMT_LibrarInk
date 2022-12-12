package it.unipi.dsmt.servlet;

import com.google.gson.Gson;
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
import java.util.List;
@WebServlet(name = "AddLoanAdminServlet", value = "/adminAddLoan", loadOnStartup = 0)
public class AddLoanAdminServlet extends HttpServlet{

        @EJB
        private ErlangClient erlang_client;
        @EJB
        private LibrarinkRemote remoteEJB;

        @Override
        protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
            String TargetJSP = "/pages/jsp/admin_add_loan.jsp";
            RequestDispatcher requestDispatcher = getServletContext().getRequestDispatcher(TargetJSP);
            requestDispatcher.forward(request, response);
        }
        @Override
        protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
            PrintWriter writer = response.getWriter();
            String user;
            String isbn;
            String idBook;
            response.setContentType("application/json");
            try {
                user = request.getParameter("User");
                isbn = request.getParameter("ISBN");
                idBook = request.getParameter("IDBook");
                erlang_client.write_reservation(user, isbn);
                LoanDTO loan = erlang_client.write_loan(user, isbn, idBook);
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
                jsonResponse.addProperty("result", "success");
                jsonResponse.addProperty("response", "Loan added");
                jsonResponse.add("loan", new Gson().toJsonTree(loan));
                writer.write(jsonResponse.toString());
            } catch (ErlangClientException | RemoteDBException ex) {
                writer.write("{\"result\": \"error\", \"response\": \"Something went wrong\"}");
            }
        }

}
