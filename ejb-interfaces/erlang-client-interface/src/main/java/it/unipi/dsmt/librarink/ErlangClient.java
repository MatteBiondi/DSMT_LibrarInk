package it.unipi.dsmt.librarink;

import javax.ejb.Remote;

@Remote
public interface ErlangClient {
    String write_copy(String isbn, String id);
    String write_loan(String user, String isbn);
    String write_reservation(String user, String isbn);
    String delete_copy(String isbn, String id);
    String delete_loan(String user, String isbn, String id);
    String delete_reservation(String user, String isbn);
    String archive_loans();
    String archive_reservations();
    String read_all_copies(String isbn);
    String read_available_copies(String isbn);
    String read_loans(String user, String isbn, String id);
    String read_ended_loans();
    String read_reservations(String user, String isbn);
    String read_ended_reservations();
    String update_loan(String type, String isbn, String id);
    String update_reservation(String type, String user, String isbn);
}
