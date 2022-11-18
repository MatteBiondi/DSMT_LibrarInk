package it.unipi.dsmt.librarink;

import javax.ejb.Remote;
import java.util.List;

@Remote
public interface ErlangClient {
    String write_copy(String isbn, String id) throws ErlangClientException;
    LoanDTO write_loan(String user, String isbn, String id) throws ErlangClientException;
    String write_reservation(String user, String isbn) throws ErlangClientException;
    String delete_copy(String isbn, String id) throws ErlangClientException;
    String delete_loan(String user, String isbn, String id) throws ErlangClientException;
    String delete_reservation(String user, String isbn) throws ErlangClientException;
    List<LoanDTO> archive_loans() throws ErlangClientException;
    List<ReservationDTO> archive_reservations() throws ErlangClientException;
    List<BookCopyDTO> read_all_copies(String isbn) throws ErlangClientException;
    List<BookCopyDTO> read_available_copies(String isbn) throws ErlangClientException;
    Integer count_available_copies(String isbn) throws ErlangClientException;
    List<LoanDTO> read_loans(String user, String isbn, String id) throws ErlangClientException;
    List<LoanDTO> read_ended_loans() throws ErlangClientException;
    List<ReservationDTO> read_reservations(String user, String isbn) throws ErlangClientException;
    List<ReservationDTO> read_ended_reservations() throws ErlangClientException;
    String terminate_loan(String isbn, String id) throws ErlangClientException;
    String renew_loan(String isbn, String id) throws ErlangClientException;
    String cancel_reservation(String user, String isbn) throws ErlangClientException;
}
