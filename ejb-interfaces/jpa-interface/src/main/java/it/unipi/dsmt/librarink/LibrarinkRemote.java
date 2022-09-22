package it.unipi.dsmt.librarink;

import javax.ejb.Remote;
import java.sql.Date;
import java.util.List;
@Remote
public interface LibrarinkRemote {
    public List<Librarink_usersDTO> listUsers(Librarink_usersDTO userFilter);
    public List<Librarink_booksDTO> listBooks(Librarink_booksDTO booksFilter);
    public List<Librarink_history_loanDTO> listHistoryLoan(Librarink_history_loanDTO history_loanFilter);
    public List<Librarink_wishlistDTO> listWishlist(Librarink_wishlistDTO wishlistFilter);
    public List<Librarink_gradesDTO> listGrades(Librarink_gradesDTO gradesFilter);
    public List<Librarink_history_reservationDTO> listHistoryReservation(Librarink_history_reservationDTO history_reservationFilter);
    public Librarink_usersDTO findUsersByEmail(String email);
    public Librarink_booksDTO findBooksByIsbn(String isbn);
    public Librarink_gradesDTO findGradesByKey(String user_email, String isbn);
    public Librarink_wishlistDTO findWishlistByKey(String user_email, String isbn);
    public Librarink_history_loanDTO findHistoryLoanByKeys(String user_email, String isbn, String id_copy, Date start_date);
    public Librarink_history_reservationDTO findHistoryReservationByKeys(String user_email, String isbn, String id_copy, Date start_date);
    public boolean  deleteUserByEmail(String email);
    public boolean  deleteBookByIsbn(String isbn);
    public boolean deleteWishlistByKey(String user_email,String isbn);
    public boolean deleteGradeByKey(String user_email,String isbn);
    public boolean deleteHistoryLoanByKeys( String user_email,String isbn,String id_copy,Date start_date);
    public boolean deleteHistoryReservationByKeys( String user_email,String isbn,String id_copy,Date start_date);
    public Librarink_usersDTO saveOrUpdateUser(Librarink_usersDTO userDTO, boolean update);
    public Librarink_booksDTO saveOrUpdateBooks(Librarink_booksDTO bookDTO, boolean update);
    public Librarink_wishlistDTO saveOrUpdateWishlist(Librarink_wishlistDTO wishlistDTO, boolean update);
    public Librarink_gradesDTO saveOrUpdateGrade(Librarink_gradesDTO gradesDTO, boolean update);
    public Librarink_history_reservationDTO saveOrUpdateHistory_reservation(Librarink_history_reservationDTO history_reservationDTO, boolean update);
    public Librarink_history_loanDTO saveOrUpdateHistory_loan(Librarink_history_loanDTO history_loanDTO, boolean update);

}
