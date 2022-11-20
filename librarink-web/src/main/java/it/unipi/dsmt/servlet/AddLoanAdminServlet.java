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

public class AddLoanAdminServlet {
    @WebServlet(name = "AddLoanAdminServlet", value = "/adminAddLoan", loadOnStartup = 0)
    public class AdminPageHistoryServlet extends HttpServlet {
        //private static final Logger LOGGER = Logger.getLogger(AsyncRequestServlet.class.getName());

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
            String user;
            String isbn;
            user= request.getParameter("User");
            isbn=request.getParameter("ISBN");
            erlang_client.write_reservation(user,isbn);
            erlang_client.write_loan(user,isbn,"id");//todo select a correct id
            List<ReservationDTO> reservationDTOList=
                    erlang_client.archive_reservations();
            for(ReservationDTO reservationDTO:reservationDTOList) {
                Librarink_history_reservationDTO history_reservationDTO = new Librarink_history_reservationDTO();
                history_reservationDTO.setUser_email(reservationDTO.getUser());
                history_reservationDTO.setIsbn(reservationDTO.getIsbn());
                history_reservationDTO.setStart_date((Date) reservationDTO.getStartDate());
                history_reservationDTO.setEnd_date((Date) reservationDTO.getStopDate());
                history_reservationDTO.setDeleted(false);
                remoteEJB.saveOrUpdateHistory_reservation(history_reservationDTO, false);
            }
        }
    }
}
