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
        protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {

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
                    history_reservationDTO.setStartDate((Date) reservationDTO.getStartDate());
                    history_reservationDTO.setEndDate((Date) reservationDTO.getStopDate());
                    history_reservationDTO.setDeleted(false);
                    remoteEJB.saveOrUpdateHistoryReservation(history_reservationDTO, false);
                }
            } catch (ErlangClientException | RemoteDBException ex) {
                writer.write(String.format("%s", ex.getMessage()));
            }
        }

}
