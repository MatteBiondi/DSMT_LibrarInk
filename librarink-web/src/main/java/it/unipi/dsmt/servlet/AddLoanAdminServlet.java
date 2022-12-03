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
@WebServlet(name = "AddLoanAdminServlet", value = "/adminAddLoan", loadOnStartup = 0)
public class AddLoanAdminServlet extends HttpServlet{

        @EJB
        private ErlangClient erlang_client;
        @EJB
        private LibrarinkRemote remoteEJB;

        @Override
        protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
            String TargetJSP = "/pages/jsp/admin_page_add_loan.jsp";
            RequestDispatcher requestDispatcher = getServletContext().getRequestDispatcher(TargetJSP);
            requestDispatcher.forward(request, response);
        }
        @Override
        protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
        {
            PrintWriter writer = response.getWriter();
            String user;
            String isbn;
            String idBook;
            try {
                user = request.getParameter("User");
                isbn = request.getParameter("ISBN");
                idBook = request.getParameter("IDBook");
                erlang_client.write_reservation(user, isbn);
                erlang_client.write_loan(user, isbn, idBook);
                List<ReservationDTO> reservationDTOList = erlang_client.archive_reservations();
                for (ReservationDTO reservationDTO : reservationDTOList) {
                    HistoryReservationDTO history_reservationDTO = new HistoryReservationDTO();
                    history_reservationDTO.setUser(reservationDTO.getUser());
                    history_reservationDTO.setIsbn(reservationDTO.getIsbn());
                    history_reservationDTO.setStartDate(new java.sql.Date(reservationDTO.getStartDate().getTime()));
                    history_reservationDTO.setEndDate(new java.sql.Date(reservationDTO.getStopDate().getTime()));
                    history_reservationDTO.setDeleted(false);
                    remoteEJB.saveOrUpdateHistoryReservation(history_reservationDTO, false);
                    writer.write("{\"result\": \"correct\", \"response\": \"Loan added correctly\"}");
                }
            } catch (ErlangClientException | RemoteDBException ex) {
                writer.write("{\"result\": \"error\", \"response\": \"server error\"}");
            }
        }

}
