package it.unipi.dsmt.librarink;

import javax.ejb.Stateless;
import java.util.List;
@Stateless
public class librarinkRemoteEJB implements LibrainkRemote{
    @Override
    public List<libraink_usersDTO> listUsers() {
        return null;
    }

    @Override
    public List<libraink_booksDTO> listBooks() {
        return null;
    }

    @Override
    public List<libraink_history_loanDTO> listHistoryLoan() {
        return null;
    }

    @Override
    public List<libraink_wishlistDTO> listWishlist() {
        return null;
    }

    @Override
    public List<libraink_history_reservationDTO> listHistoryReservation() {
        return null;
    }
}
