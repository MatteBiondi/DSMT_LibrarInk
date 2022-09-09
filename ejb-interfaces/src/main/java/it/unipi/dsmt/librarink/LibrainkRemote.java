package it.unipi.dsmt.librarink;

import javax.ejb.Remote;
import java.sql.Date;
import java.util.List;
@Remote
public interface LibrainkRemote {
    public List<libraink_usersDTO> listUsers(libraink_usersDTO userFilter);
    public List<libraink_booksDTO> listBooks(libraink_booksDTO booksFilter);
    public List<libraink_history_loanDTO> listHistoryLoan(libraink_history_loanDTO history_loanFilter);
    public List<libraink_wishlistDTO> listWishlist(libraink_wishlistDTO wishlistFilter);
    public List<libraink_history_reservationDTO> listHistoryReservation(libraink_history_reservationDTO history_reservationFilter);
    public libraink_usersDTO findUsersByEmail(String email);
    public libraink_booksDTO findBooksByIsbn(String isbn);
    public libraink_wishlistDTO findWishlistByKey(String user_email,String isbn);
    public libraink_history_loanDTO findHistoryLoanByKeys( String user_email,String isbn,String id_copy,Date start_date);
    public libraink_history_reservationDTO findHistoryReservationByKeys( String user_email,String isbn,String id_copy,Date start_date);
    public boolean  deleteUserByEmail(String email);
    public boolean  deleteBookByIsbn(String isbn);
    public boolean deleteWishlistByKey(String user_email,String isbn);
    public boolean deleteHistoryLoanByKeys( String user_email,String isbn,String id_copy,Date start_date);
    public boolean deleteHistoryReservationByKeys( String user_email,String isbn,String id_copy,Date start_date);
    public libraink_usersDTO saveOrUpdateUser(libraink_usersDTO userDTO,boolean update);
    public libraink_booksDTO saveOrUpdateBooks(libraink_booksDTO bookDTO,boolean update);
    public libraink_wishlistDTO saveOrUpdateWishlist(libraink_wishlistDTO wishlistDTO,boolean update);
    public libraink_history_reservationDTO saveOrUpdateHistory_reservation(libraink_history_reservationDTO history_reservationDTO,boolean update);
    public libraink_history_loanDTO saveOrUpdateHistory_loan(libraink_history_loanDTO history_loanDTO,boolean update);
}
