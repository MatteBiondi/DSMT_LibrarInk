package it.unipi.dsmt.librarink;

import it.unipi.dsmt.librarink.entities.Books;
import it.unipi.dsmt.librarink.entities.Users;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import java.util.List;
@Stateless
public class librarinkRemoteEJB implements LibrainkRemote{
    @PersistenceContext
    private EntityManager entityManager;
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

    @Override
    public libraink_usersDTO findUsersByEmail(String email) {
        Users user = entityManager.find(Users.class,email);
        libraink_usersDTO userDTO = new libraink_usersDTO();
        userDTO.setEmail(user.getEmail());
        userDTO.setAddress(user.getAddress());
        userDTO.setBirthday(user.getBirthday());
        userDTO.setImage(user.getImage());
        userDTO.setName(user.getName());
        userDTO.setSurname(user.getSurname());
        userDTO.setPassword(user.getPassword());
        return userDTO;
    }

    @Override
    public libraink_booksDTO findBooksByIsbn(String isbn) {
        Books book = entityManager.find(Books.class,isbn);
        libraink_booksDTO bookDTO = new libraink_booksDTO();
        bookDTO.setIsbn(book.getIsbn());
        bookDTO.setBook_title(book.getBook_title());
        bookDTO.setGenre(book.getGenre());
        bookDTO.setPublisher(book.getPublisher());
        bookDTO.setImage_url_s(book.getImage_url_s());
        bookDTO.setImage_url_m(book.getImage_url_m());
        bookDTO.setImage_url_l(book.getImage_url_l());
        bookDTO.setDescription(book.getDescription());
        bookDTO.setYear_of_publication(book.getYear_of_publication());
        bookDTO.setSum_of_stars(book.getSum_of_stars());
        bookDTO.setNumber_of_review(book.getNumber_of_review());
        return bookDTO;
    }

    @Override
    public libraink_usersDTO saveOrUpdateUser(libraink_usersDTO userDTO,boolean update) {
        Users user = null;
        if (!update) {
            user = new Users();
        } else {
            user = entityManager.find(Users.class, userDTO.getEmail());
        }

        user.setEmail(userDTO.getEmail());
        user.setAddress(userDTO.getAddress());
        user.setBirthday(userDTO.getBirthday());
        user.setImage(userDTO.getImage());
        user.setName(userDTO.getName());
        user.setSurname(userDTO.getSurname());
        user.setPassword(userDTO.getPassword());
        if(!update)
        {
            entityManager.persist(user);
        }
        else
        {
            entityManager.merge(user);
        }
        return userDTO;
    }

    @Override
    public libraink_booksDTO saveOrUpdateBooks(libraink_booksDTO bookDTO, boolean update) {
        Books book = null;
        if(!update)
        {
            book = new Books();
        }
        else
        {
            book =  entityManager.find(Books.class,bookDTO.getIsbn());
        }
        book.setIsbn(bookDTO.getIsbn());
        book.setBook_title(bookDTO.getBook_title());
        book.setGenre(bookDTO.getGenre());
        book.setPublisher(bookDTO.getPublisher());
        book.setImage_url_s(bookDTO.getImage_url_s());
        book.setImage_url_m(bookDTO.getImage_url_m());
        book.setImage_url_l(bookDTO.getImage_url_l());
        book.setDescription(bookDTO.getDescription());
        book.setYear_of_publication(bookDTO.getYear_of_publication());
        book.setSum_of_stars(bookDTO.getSum_of_stars());
        book.setNumber_of_review(bookDTO.getNumber_of_review());
        if(!update)
        {
            entityManager.persist(book);
        }
        else
        {
            entityManager.merge(book);
        }
        return bookDTO;
    }
}
