package it.unipi.dsmt.librarink;

import javax.ejb.Remote;
import java.sql.Timestamp;
import java.util.List;
@Remote
public interface LibrarinkRemote {
    List<UserDTO> listUsers(UserDTO userFilter) throws RemoteDBException;
    List<AdminDTO> listAdmins(AdminDTO adminFilter) throws RemoteDBException;
    List<BookDTO> listBook(BookDTO booksFilter) throws RemoteDBException;
    List<HistoryLoanDTO> listHistoryLoans(HistoryLoanDTO historyLoanFilter) throws RemoteDBException;
    List<WishlistDTO> listWishlist(WishlistDTO wishlistFilter) throws RemoteDBException;
    List<GradeDTO> listGrades(GradeDTO gradesFilter) throws RemoteDBException;
    List<HistoryReservationDTO> listHistoryReservations(HistoryReservationDTO historyReservationFilter) throws RemoteDBException;
    UserDTO findUsersByEmail(String email) throws RemoteDBException;
    BookDTO findBooksByIsbn(String isbn) throws RemoteDBException;
    GradeDTO findGradeByKey(String user_email, String isbn) throws RemoteDBException;
    WishlistDTO findWishlistByKey(String user_email, String isbn) throws RemoteDBException;
    HistoryLoanDTO findHistoryLoanByKey(String user_email, String isbn, String id_copy, Timestamp start_date) throws RemoteDBException;
    HistoryReservationDTO findHistoryReservationByKey(String user_email, String isbn, String id_copy,
                                                      Timestamp start_date) throws RemoteDBException;
    boolean  deleteUserByEmail(String email) throws RemoteDBException;
    boolean  deleteBookByIsbn(String isbn) throws RemoteDBException;
    boolean deleteWishlistByKey(String user_email,String isbn) throws RemoteDBException;
    boolean deleteGradeByKey(String user_email,String isbn) throws RemoteDBException;
    boolean deleteHistoryLoanByKey( String user_email,String isbn,String id_copy,Timestamp start_date) throws RemoteDBException;
    boolean deleteHistoryReservationByKey( String user_email,String isbn,String id_copy,Timestamp start_date) throws RemoteDBException;
    UserDTO saveOrUpdateUser(UserDTO userDTO, boolean update) throws RemoteDBException;
    BookDTO saveOrUpdateBook(BookDTO bookDTO, boolean update) throws RemoteDBException;
    WishlistDTO saveOrUpdateWishlist(WishlistDTO wishlistDTO, boolean update) throws RemoteDBException;
    GradeDTO saveOrUpdateGrade(GradeDTO gradesDTO) throws RemoteDBException;
    HistoryReservationDTO saveOrUpdateHistoryReservation(HistoryReservationDTO history_reservationDTO,
                                                         boolean update) throws RemoteDBException;
    HistoryLoanDTO saveOrUpdateHistoryLoan(HistoryLoanDTO history_loanDTO, boolean update) throws RemoteDBException;
    List<BookDTO> listPaginationBook(int offset, int page, BookDTO filter) throws RemoteDBException;
    Long countBooks(BookDTO filter) throws RemoteDBException;
    Double computeRating(String isbn) throws RemoteDBException;
}
