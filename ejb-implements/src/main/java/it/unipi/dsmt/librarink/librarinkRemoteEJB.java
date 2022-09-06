package it.unipi.dsmt.librarink;

import it.unipi.dsmt.librarink.entities.Books;
import it.unipi.dsmt.librarink.entities.Users;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
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
