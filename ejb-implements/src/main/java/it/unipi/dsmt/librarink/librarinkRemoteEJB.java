package it.unipi.dsmt.librarink;

import it.unipi.dsmt.librarink.entities.*;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.sql.Date;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Stateless
public class librarinkRemoteEJB implements LibrainkRemote{
    @PersistenceContext
    private EntityManager entityManager;
    @Override
    public List<libraink_usersDTO> listUsers(libraink_usersDTO usersFilter) {
        Map<String, Object> parameters = new HashMap<String, Object>();
        StringBuilder jpql = new StringBuilder();
        jpql.append("select u, coalesce(size(c.languages),0) from users u where 1 = 1 ");
        if (usersFilter.getName() != null && !usersFilter.getName().isEmpty()){
            jpql.append(" and lower(u.name) like concat('%', lower(:name), '%') ");
            parameters.put("name", usersFilter.getName());
        }
        if (usersFilter.getSurname() != null && !usersFilter.getSurname().isEmpty()){
            jpql.append(" and lower(u.surname) like concat('%', lower(:surname), '%') ");
            parameters.put("surname", usersFilter.getSurname());
        }
        if (usersFilter.getPassword() != null && !usersFilter.getPassword().isEmpty()){
            jpql.append(" and lower(u.password) like concat('%', lower(:password), '%') ");
            parameters.put("password", usersFilter.getSurname());
        }
        jpql.append(" group by u ");
        Query query = entityManager.createQuery(jpql.toString());
        for (Map.Entry<String, Object> paramKeyValue: parameters.entrySet()){
            query.setParameter(paramKeyValue.getKey(), paramKeyValue.getValue());
        }
        List<Object[]> usersList = query.getResultList();
        List<libraink_usersDTO> toReturnList = new ArrayList<libraink_usersDTO>();
        if (usersList != null && !usersList.isEmpty()) {
            for(Object[] usersInfo: usersList){
                Users user = (Users)usersInfo[0];
                Integer numLanguages = ((Number)usersInfo[1]).intValue();
                libraink_usersDTO usersdto = new libraink_usersDTO();
                usersdto.setPassword(user.getPassword());
                usersdto.setName(user.getName());
                usersdto.setSurname(user.getSurname());
                usersdto.setEmail(user.getEmail());
                usersdto.setAddress(user.getAddress());
                usersdto.setBirthday(user.getBirthday());
                usersdto.setImage(user.getImage());
                toReturnList.add(usersdto);
            }
        }
        return toReturnList;
    }

    @Override
    public List<libraink_booksDTO> listBooks(libraink_booksDTO booksFilter) {
        Map<String, Object> parameters = new HashMap<String, Object>();
        StringBuilder jpql = new StringBuilder();
        jpql.append("select b, coalesce(size(b.languages),0) from users b where 1 = 1 ");
        if (booksFilter.getBook_title() != null && !booksFilter.getBook_title().isEmpty()){
            jpql.append(" and lower(b.book_title) like concat('%', lower(:book_title), '%') ");
            parameters.put("book", booksFilter.getBook_title());
        }
        if (booksFilter.getPublisher() != null && !booksFilter.getPublisher().isEmpty()){
            jpql.append(" and lower(b.publisher) like concat('%', lower(:publisher), '%') ");
            parameters.put("publisher", booksFilter.getPublisher());
        }
        if (booksFilter.getBook_author() != null && !booksFilter.getBook_author().isEmpty()){
            jpql.append(" and lower(b.book_author) like concat('%', lower(:book_author), '%') ");
            parameters.put("book_author", booksFilter.getBook_author());
        }
        jpql.append(" group by b ");
        Query query = entityManager.createQuery(jpql.toString());
        for (Map.Entry<String, Object> paramKeyValue: parameters.entrySet()){
            query.setParameter(paramKeyValue.getKey(), paramKeyValue.getValue());
        }
        List<Object[]> booksList = query.getResultList();
        List<libraink_booksDTO> toReturnList = new ArrayList<libraink_booksDTO>();
        if (booksList != null && !booksList.isEmpty()) {
            for(Object[] booksInfo : booksList){
                Books book = (Books) booksInfo[0];
                Integer numLanguages = ((Number) booksInfo[1]).intValue();
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
                toReturnList.add(bookDTO);
            }
        }
        return toReturnList;
    }

    @Override
    public List<libraink_history_loanDTO> listHistoryLoan(libraink_history_loanDTO history_loanFilter) {
        return null;
    }

    @Override
    public List<libraink_wishlistDTO> listWishlist(libraink_wishlistDTO wishlistFilter) {
        Map<String, Object> parameters = new HashMap<String, Object>();
        StringBuilder jpql = new StringBuilder();
        jpql.append("select w, coalesce(size(w.languages),0) from users w where 1 = 1 ");
        if (wishlistFilter.getEmail_user() != null && !wishlistFilter.getEmail_user().isEmpty()){
            jpql.append(" and lower(w.email_user) like concat('%', lower(:email_user), '%') ");
            parameters.put("email_user", wishlistFilter.getEmail_user());
        }
        if (wishlistFilter.getIsbn() != null && !wishlistFilter.getIsbn().isEmpty()){
            jpql.append(" and lower(u.isbn) like concat('%', lower(:isbn), '%') ");
            parameters.put("isbn", wishlistFilter.getIsbn());
        }
        jpql.append(" group by w ");
        Query query = entityManager.createQuery(jpql.toString());
        for (Map.Entry<String, Object> paramKeyValue: parameters.entrySet()){
            query.setParameter(paramKeyValue.getKey(), paramKeyValue.getValue());
        }
        List<Object[]> wishlistList = query.getResultList();
        List<libraink_wishlistDTO> toReturnList = new ArrayList<libraink_wishlistDTO>();
        if (wishlistList != null && !wishlistList.isEmpty()) {
            for(Object[] wishlistInfo: wishlistList){
                Wishlist wishlist = (Wishlist) wishlistInfo[0];
                Integer numLanguages = ((Number)wishlistInfo[1]).intValue();
                libraink_wishlistDTO wishlistDTO = new libraink_wishlistDTO();
                wishlistDTO.setIsbn(wishlist.getIsbn());
                wishlistDTO.setEmail_user(wishlist.getEmail_user());
                toReturnList.add(wishlistDTO);
            }
        }
        return toReturnList;
    }

    @Override
    public List<libraink_history_reservationDTO> listHistoryReservation(libraink_history_reservationDTO history_reservationFilter) {
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
    public libraink_wishlistDTO findWishlistByKey(String user_email, String isbn) {
        WishListKey wishListKey=new WishListKey(user_email,isbn);
        Wishlist wishlist = entityManager.find(Wishlist.class,wishListKey);
        libraink_wishlistDTO wishlistDTO = new libraink_wishlistDTO();
        wishlistDTO.setIsbn(wishlist.getIsbn());
        wishlistDTO.setEmail_user(wishlist.getEmail_user());
        return wishlistDTO;
    }

    @Override
    public libraink_history_loanDTO findHistoryLoanByKeys(String user_email, String isbn, String id_copy, Date start_date) {
        History_loanKey history_loanKey=new History_loanKey(user_email,isbn,id_copy,start_date);
        History_loan history_loan = entityManager.find(History_loan.class,history_loanKey);
        libraink_history_loanDTO history_loanDTO = new libraink_history_loanDTO();
        history_loanDTO.setIsbn(history_loan.getIsbn());
        history_loanDTO.setEnd_date(history_loan.getEnd_date());
        history_loanDTO.setId_copy(history_loan.getId_copy());
        history_loanDTO.setUser_email(history_loan.getUser_email());
        history_loanDTO.setStart_date(history_loan.getStart_date());
        return history_loanDTO;
    }

    @Override
    public libraink_history_reservationDTO findHistoryReservationByKeys(String user_email, String isbn, String id_copy, Date start_date) {
        History_reservationKey history_reservationKey=new History_reservationKey(user_email,isbn,id_copy,start_date);
        History_reservation history_reservation = entityManager.find(History_reservation.class,history_reservationKey);
        libraink_history_reservationDTO history_reservationDTO = new libraink_history_reservationDTO();
        history_reservationDTO.setIsbn(history_reservation.getIsbn());
        history_reservationDTO.setEnd_date(history_reservation.getEnd_date());
        history_reservationDTO.setId_copy(history_reservation.getId_copy());
        history_reservationDTO.setUser_email(history_reservation.getUser_email());
        history_reservationDTO.setStart_date(history_reservation.getStart_date());
        history_reservationDTO.setDeleted(history_reservation.isDeleted());
        return history_reservationDTO;
    }

    @Override
    public boolean deleteUserByEmail(String email) {
        Users user = entityManager.find(Users.class,email);
        if (user!=null)
        {
            entityManager.remove(user);
            return true;
        }
        return false;
    }

    @Override
    public boolean deleteBookByIsbn(String isbn) {
        Books book = entityManager.find(Books.class,isbn);
        if (book!=null)
        {
            entityManager.remove(book);
            return true;
        }
        return false;

    }

    @Override
    public boolean deleteWishlistByKey(String user_email, String isbn) {
        WishListKey wishListKey = new WishListKey(user_email,isbn);
        Wishlist wishlist = entityManager.find(Wishlist.class,wishListKey);
        if(wishlist!=null)
        {
            entityManager.remove(wishlist);
            return true;
        }
        return false;
    }

    @Override
    public boolean deleteHistoryLoanByKeys(String user_email, String isbn, String id_copy, Date start_date) {
        History_loanKey history_loanKey = new History_loanKey(user_email,isbn,id_copy,start_date);
        History_loan history_loan = entityManager.find(History_loan.class,history_loanKey);
        if(history_loan!=null)
        {
            entityManager.remove(history_loan);
            return true;
        }
        return false;
    }

    @Override
    public boolean deleteHistoryReservationByKeys(String user_email, String isbn, String id_copy, Date start_date) {
        History_reservationKey history_reservationKey = new History_reservationKey(user_email,isbn,id_copy,start_date);
        History_reservation history_reservation = entityManager.find(History_reservation.class,history_reservationKey);
        if(history_reservation!=null)
        {
            entityManager.remove(history_reservation);
            return true;
        }
        return false;
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

    @Override
    public libraink_wishlistDTO saveOrUpdateWishlist(libraink_wishlistDTO wishlistDTO, boolean update) {
        Wishlist wishlist = null;
        if(!update)
        {
            wishlist=new Wishlist();
        }
        else
        {
            wishlist=entityManager.find(Wishlist.class,new WishListKey(wishlistDTO.getEmail_user(),wishlistDTO.getIsbn()));


        }
        wishlist.setIsbn(wishlistDTO.getIsbn());
        wishlist.setEmail_user(wishlistDTO.getEmail_user());
        if(!update)
        {
            entityManager.persist(wishlist);
        }
        else
        {
            entityManager.merge(wishlist);
        }
        return wishlistDTO;
    }

    @Override
    public libraink_history_reservationDTO saveOrUpdateHistory_reservation(libraink_history_reservationDTO history_reservationDTO, boolean update) {
        History_reservation history_reservation=null;
        if(!update)
        {
            history_reservation=new History_reservation();
        }
        else
        {
            history_reservation=entityManager.find(History_reservation.class,new History_reservationKey(
                    history_reservationDTO.getUser_email(), history_reservationDTO.getIsbn(),
                    history_reservationDTO.getId_copy(),history_reservationDTO.getStart_date()));
        }

        history_reservation.setIsbn(history_reservationDTO.getIsbn());
        history_reservation.setEnd_date(history_reservationDTO.getEnd_date());
        history_reservation.setId_copy(history_reservationDTO.getId_copy());
        history_reservation.setUser_email(history_reservationDTO.getUser_email());
        history_reservation.setStart_date(history_reservationDTO.getStart_date());
        history_reservation.setDeleted(history_reservationDTO.isDeleted());
        if(!update)
        {
            entityManager.persist(history_reservation);
        }
        else
        {
            entityManager.merge(history_reservation);
        }
        return history_reservationDTO;
    }

    @Override
    public libraink_history_loanDTO saveOrUpdateHistory_loan(libraink_history_loanDTO history_loanDTO, boolean update) {
        History_loan history_loan=null;
        if(!update)
        {
            history_loan=new History_loan();
        }
        else
        {
            history_loan=entityManager.find(History_loan.class,new History_loanKey(
                    history_loanDTO.getUser_email(), history_loanDTO.getIsbn(),
                    history_loanDTO.getId_copy(),history_loanDTO.getStart_date()));
        }

        history_loan.setIsbn(history_loanDTO.getIsbn());
        history_loan.setEnd_date(history_loanDTO.getEnd_date());
        history_loan.setId_copy(history_loanDTO.getId_copy());
        history_loan.setUser_email(history_loanDTO.getUser_email());
        history_loan.setStart_date(history_loanDTO.getStart_date());

        if(!update)
        {
            entityManager.persist(history_loan);
        }
        else
        {
            entityManager.merge(history_loan);
        }
        return history_loanDTO;
    }
}
