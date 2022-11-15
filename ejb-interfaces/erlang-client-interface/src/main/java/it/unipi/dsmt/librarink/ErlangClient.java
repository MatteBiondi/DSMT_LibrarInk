package it.unipi.dsmt.librarink;

import javax.ejb.Remote;
import java.util.List;

@Remote
public interface ErlangClient {
    String write_copy(String isbn, String id);
    LoanDTO write_loan(String user, String isbn, String id);
    String write_reservation(String user, String isbn);
    String delete_copy(String isbn, String id);
    String delete_loan(String user, String isbn, String id);
    String delete_reservation(String user, String isbn);
    List<LoanDTO> archive_loans();
    List<ReservationDTO> archive_reservations();
    List<BookCopyDTO> read_all_copies(String isbn);
    List<BookCopyDTO> read_available_copies(String isbn);
    Integer count_available_copies(String isbn);
    List<LoanDTO> read_loans(String user, String isbn, String id);
    List<LoanDTO> read_ended_loans();
    List<ReservationDTO> read_reservations(String user, String isbn);
    List<ReservationDTO> read_ended_reservations();
    String terminate_loan(String isbn, String id);
    String renew_loan(String isbn, String id);
    String cancel_reservation(String user, String isbn);
}
