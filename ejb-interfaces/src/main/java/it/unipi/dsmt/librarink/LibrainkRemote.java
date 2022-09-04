package it.unipi.dsmt.librarink;

import javax.ejb.Remote;
import java.util.List;
@Remote
public interface LibrainkRemote {
    public List<libraink_usersDTO> listUsers();
    public List<libraink_booksDTO> listBooks();
    public List<libraink_history_loanDTO> listHistoryLoan();
    public List<libraink_wishlistDTO> listWishlist();
    public List<libraink_history_reservationDTO> listHistoryReservation();
}
