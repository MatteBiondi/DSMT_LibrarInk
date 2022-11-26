package it.unipi.dsmt.librarink;

import it.unipi.dsmt.librarink.entities.*;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.sql.Date;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Stateless
public class LibrarinkRemoteEJB implements LibrarinkRemote {
    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public List<UserDTO> listUsers(UserDTO usersFilter) throws RemoteDBException {
        try{
            Map<String, Object> parameters = new HashMap<>();
            StringBuilder jpql = new StringBuilder();

            jpql.append("select u from User u where 1 = 1 ");
            if (usersFilter.getEmail() != null && !usersFilter.getEmail().isEmpty()){
                jpql.append(" and lower(u.email) like concat('%', lower(:email), '%') ");
                parameters.put("email", usersFilter.getEmail());
            }
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
                parameters.put("password", usersFilter.getPassword());
            }

            Query query = entityManager.createQuery(jpql.toString());
            for (Map.Entry<String, Object> paramKeyValue: parameters.entrySet()){
                query.setParameter(paramKeyValue.getKey(), paramKeyValue.getValue());
            }
            List<User> userList = query.getResultList();
            List<UserDTO> toReturnList = new ArrayList<>();
            if (userList != null && !userList.isEmpty()) {
                for(User user : userList){
                    UserDTO userDTO = new UserDTO();
                    userDTO.setPassword(user.getPassword());
                    userDTO.setName(user.getName());
                    userDTO.setSurname(user.getSurname());
                    userDTO.setEmail(user.getEmail());
                    userDTO.setAddress(user.getAddress());
                    userDTO.setBirthday(user.getBirthday());
                    userDTO.setImage(user.getImage());
                    toReturnList.add(userDTO);
                }
            }

            return toReturnList;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public List<AdminDTO> listAdmins(AdminDTO adminFilter) {
        Map<String, Object> parameters = new HashMap<String, Object>();
        StringBuilder jpql = new StringBuilder();
        jpql.append("select a from Admin a where 1 = 1 ");
        if (adminFilter.getEmail() != null && !adminFilter.getEmail().isEmpty()){
            jpql.append(" and lower(a.email) like concat('%', lower(:email), '%') ");
            parameters.put("email", adminFilter.getEmail());
        }
        if (adminFilter.getPassword() != null && !adminFilter.getPassword().isEmpty()){
            jpql.append(" and lower(a.password) like concat('%', lower(:password), '%') ");
            parameters.put("password", adminFilter.getPassword());
        }
        Query query = entityManager.createQuery(jpql.toString());
        for (Map.Entry<String, Object> paramKeyValue: parameters.entrySet()){
            query.setParameter(paramKeyValue.getKey(), paramKeyValue.getValue());
        }
        List<Admin> adminList = query.getResultList();
        List<AdminDTO> toReturnList = new ArrayList<AdminDTO>();
        if (adminList != null && !adminList.isEmpty()) {
            for(Admin admin : adminList){
                AdminDTO adminDto = new AdminDTO();
                adminDto.setPassword(admin.getPassword());
                adminDto.setEmail(admin.getEmail());
                toReturnList.add(adminDto);
            }
        }
        return toReturnList;
    }

    //@Override
    //public List<LibrarinkBookDTO> listBook(LibrarinkBookDTO booksFilter) throws RemoteDBException{
    //    Map<String, Object> parameters = new HashMap<String, Object>();
    //    StringBuilder jpql = new StringBuilder();
    //    //jpql.append("select b, coalesce(size(b.languages),0) from books b where 1 = 1 ");
    //    jpql.append("select b from Book b where 1 = 1 ");
    //    if (booksFilter.getTitle() != null && !booksFilter.getTitle().isEmpty()){
    //        jpql.append(" and lower(b.book_title) like concat('%', lower(:book_title), '%') ");
    //        parameters.put("book", booksFilter.getTitle());
    //    }
    //    if (booksFilter.getPublisher() != null && !booksFilter.getPublisher().isEmpty()){
    //        jpql.append(" and lower(b.publisher) like concat('%', lower(:publisher), '%') ");
    //        parameters.put("publisher", booksFilter.getPublisher());
    //    }
    //    if (booksFilter.getAuthor() != null && !booksFilter.getAuthor().isEmpty()){
    //        jpql.append(" and lower(b.book_author) like concat('%', lower(:book_author), '%') ");
    //        parameters.put("book_author", booksFilter.getAuthor());
    //    }
    //    Query query = entityManager.createQuery(jpql.toString());
    //    for (Map.Entry<String, Object> paramKeyValue: parameters.entrySet()){
    //        query.setParameter(paramKeyValue.getKey(), paramKeyValue.getValue());
    //    }
    //    List<Object[]> booksList = query.getResultList();
    //    List<LibrarinkBookDTO> toReturnList = new ArrayList<LibrarinkBookDTO>();
    //    if (booksList != null && !booksList.isEmpty()) {
    //        for(Object[] booksInfo : booksList){
    //            Book book = (Book) booksInfo[0];
    //            LibrarinkBookDTO bookDTO = buildBookDTO(book);
    //            toReturnList.add(bookDTO);
    //        }
    //    }
    //    return toReturnList;
    //}

    @Override
    public List<HistoryLoanDTO> listHistoryLoans(HistoryLoanDTO history_loanFilter)  throws RemoteDBException {
        try {
            Map<String, Object> parameters = new HashMap<>();
            StringBuilder jpql = new StringBuilder();

            jpql.append("select hl from HistoryLoan hl where 1 = 1 ");
            if (history_loanFilter.getUser() != null && !history_loanFilter.getUser().isEmpty()){
                jpql.append(" and lower(hl.user) like concat('%', lower(:user), '%') ");
                parameters.put("user", history_loanFilter.getUser());
            }
            if (history_loanFilter.getIsbn() != null && !history_loanFilter.getIsbn().isEmpty()){
                jpql.append(" and lower(hl.isbn) like concat('%', lower(:isbn), '%') ");
                parameters.put("isbn", history_loanFilter.getIsbn());
            }
            if (history_loanFilter.getId_copy() != null && !history_loanFilter.getId_copy().isEmpty()){
                jpql.append(" and lower(hl.id_copy) like concat('%', lower(:id_copy), '%') ");
                parameters.put("id_copy", history_loanFilter.getIsbn());
            }

            Query query = entityManager.createQuery(jpql.toString());
            for (Map.Entry<String, Object> paramKeyValue: parameters.entrySet()){
                query.setParameter(paramKeyValue.getKey(), paramKeyValue.getValue());
            }

            List<Object[]> history_loanList = query.getResultList();
            List<HistoryLoanDTO> toReturnList = new ArrayList<>();
            if (history_loanList != null && !history_loanList.isEmpty()) {
                for(Object[] history_loanInfo: history_loanList){
                    HistoryLoan history_loan = (HistoryLoan) history_loanInfo[0];
                    HistoryLoanDTO history_loanDTO = new HistoryLoanDTO();
                    history_loanDTO.setIsbn(history_loan.getIsbn());
                    history_loanDTO.setEnd_date(history_loan.getEnd_date());
                    history_loanDTO.setId_copy(history_loan.getId_copy());
                    history_loanDTO.setUser(history_loan.getUser());
                    history_loanDTO.setStart_date(history_loan.getStart_date());
                    toReturnList.add(history_loanDTO);
                }
            }
            return toReturnList;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public List<WishlistDTO> listWishlist(WishlistDTO wishlistFilter) throws RemoteDBException {
        try {
            Map<String, Object> parameters = new HashMap<>();
            StringBuilder jpql = new StringBuilder();

            jpql.append("select w from Wishlist w where 1 = 1 ");
            if (wishlistFilter.getUser() != null && !wishlistFilter.getUser().isEmpty()) {
                jpql.append(" and lower(w.user) like concat('%', lower(:user), '%') ");
                parameters.put("user", wishlistFilter.getUser());
            }
            if (wishlistFilter.getIsbn() != null && !wishlistFilter.getIsbn().isEmpty()) {
                jpql.append(" and lower(w.isbn) like concat('%', lower(:isbn), '%') ");
                parameters.put("isbn", wishlistFilter.getIsbn());
            }

            Query query = entityManager.createQuery(jpql.toString());
            for (Map.Entry<String, Object> paramKeyValue : parameters.entrySet()) {
                query.setParameter(paramKeyValue.getKey(), paramKeyValue.getValue());
            }
            List<Wishlist> wishlistList = query.getResultList();
            List<WishlistDTO> toReturnList = new ArrayList<>();
            if (wishlistList != null && !wishlistList.isEmpty()) {
                for (Wishlist wishlist : wishlistList) {
                    WishlistDTO wishlistDTO = new WishlistDTO();
                    wishlistDTO.setIsbn(wishlist.getIsbn());
                    wishlistDTO.setUser(wishlist.getUser());
                    toReturnList.add(wishlistDTO);
                }
            }
            return toReturnList;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public List<GradeDTO> listGrades(GradeDTO gradesFilter) throws RemoteDBException {
        try{
            Map<String, Object> parameters = new HashMap<>();
            StringBuilder jpql = new StringBuilder();

            jpql.append("select g from Grade g where 1 = 1 ");

            if (gradesFilter.getUser() != null && !gradesFilter.getUser().isEmpty()) {
                jpql.append(" and lower(g.user) like concat('%', lower(:user), '%') ");
                parameters.put("user", gradesFilter.getUser());
            }
            if (gradesFilter.getIsbn() != null && !gradesFilter.getIsbn().isEmpty()) {
                jpql.append(" and lower(g.isbn) like concat('%', lower(:isbn), '%') ");
                parameters.put("isbn", gradesFilter.getIsbn());
            }

            Query query = entityManager.createQuery(jpql.toString());
            for (Map.Entry<String, Object> paramKeyValue : parameters.entrySet()) {
                query.setParameter(paramKeyValue.getKey(), paramKeyValue.getValue());
            }

            List<Grade> gradeList = query.getResultList();
            List<GradeDTO> toReturnList = new ArrayList<>();
            if (gradeList != null && !gradeList.isEmpty()) {
                for (Grade grade : gradeList) {
                    GradeDTO gradesDTO = new GradeDTO();
                    gradesDTO.setIsbn(grade.getIsbn());
                    gradesDTO.setUser(grade.getUser());
                    gradesDTO.setStars(grade.getStars());
                    toReturnList.add(gradesDTO);
                }
            }
            return toReturnList;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public List<HistoryReservationDTO> listHistoryReservations(HistoryReservationDTO history_reservationFilter) throws RemoteDBException {
        try{
            Map<String, Object> parameters = new HashMap<>();
            StringBuilder jpql = new StringBuilder();

            jpql.append("select hr from HistoryReservation hr where 1 = 1 ");
            if (history_reservationFilter.getUser() != null && !history_reservationFilter.getUser().isEmpty()) {
                jpql.append(" and lower(hr.user) like concat('%', lower(:user), '%') ");
                parameters.put("user", history_reservationFilter.getUser());
            }
            if (history_reservationFilter.getIsbn() != null && !history_reservationFilter.getIsbn().isEmpty()) {
                jpql.append(" and lower(hr.isbn) like concat('%', lower(:isbn), '%') ");
                parameters.put("isbn", history_reservationFilter.getIsbn());
            }
            Query query = entityManager.createQuery(jpql.toString());
            for (Map.Entry<String, Object> paramKeyValue : parameters.entrySet()) {
                query.setParameter(paramKeyValue.getKey(), paramKeyValue.getValue());
            }
            List<Object[]> history_reservationList = query.getResultList();
            List<HistoryReservationDTO> toReturnList = new ArrayList<>();
            if (history_reservationList != null && !history_reservationList.isEmpty()) {
                for (Object[] history_reservationInfo : history_reservationList) {
                    HistoryReservation history_reservation = (HistoryReservation) history_reservationInfo[0];
                    HistoryReservationDTO history_reservationDTO = new HistoryReservationDTO();
                    history_reservationDTO.setIsbn(history_reservation.getIsbn());
                    history_reservationDTO.setEnd_date(history_reservation.getEnd_date());
                    history_reservationDTO.setUser(history_reservation.getUser());
                    history_reservationDTO.setStart_date(history_reservation.getStart_date());
                    history_reservationDTO.setDeleted(history_reservation.isDeleted());
                    toReturnList.add(history_reservationDTO);
                }
            }
            return toReturnList;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public UserDTO findUsersByEmail(String email) throws RemoteDBException {
        try {
            User user = entityManager.find(User.class, email);
            return buildUserDTO(user);
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public BookDTO findBooksByIsbn(String isbn) throws RemoteDBException {
        try {
            Book book = entityManager.find(Book.class,isbn);
            return buildBookDTO(book);
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public GradeDTO findGradeByKey(String user, String isbn) throws RemoteDBException {
        try {
            GradeKey gradeKey = new GradeKey(user, isbn);
            Grade grade = entityManager.find(Grade.class, gradeKey);

            if (grade == null)
                return null;

            GradeDTO gradeDTO = new GradeDTO();
            gradeDTO.setIsbn(grade.getIsbn());
            gradeDTO.setUser(grade.getUser());
            gradeDTO.setStars(grade.getStars());
            return gradeDTO;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public WishlistDTO findWishlistByKey(String user, String isbn) throws RemoteDBException {
        try {
            WishListKey wishListKey = new WishListKey(user, isbn);
            Wishlist wishlist = entityManager.find(Wishlist.class, wishListKey);
            WishlistDTO wishlistDTO = new WishlistDTO();
            wishlistDTO.setIsbn(wishlist.getIsbn());
            wishlistDTO.setUser(wishlist.getUser());
            return wishlistDTO;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public HistoryLoanDTO findHistoryLoanByKey(String user, String isbn, String id_copy, Date start_date) throws RemoteDBException {
        try {
            HistoryLoanKey history_loanKey = new HistoryLoanKey(user, isbn, id_copy, start_date);
            HistoryLoan history_loan = entityManager.find(HistoryLoan.class, history_loanKey);
            HistoryLoanDTO history_loanDTO = new HistoryLoanDTO();
            history_loanDTO.setIsbn(history_loan.getIsbn());
            history_loanDTO.setEnd_date(history_loan.getEnd_date());
            history_loanDTO.setId_copy(history_loan.getId_copy());
            history_loanDTO.setUser(history_loan.getUser());
            history_loanDTO.setStart_date(history_loan.getStart_date());
            return history_loanDTO;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public HistoryReservationDTO findHistoryReservationByKey(String user, String isbn, String id_copy,
                                                             Date start_date) throws RemoteDBException {
        try {
            HistoryReservationKey history_reservationKey = new HistoryReservationKey(user, isbn, start_date);
            HistoryReservation history_reservation = entityManager.find(HistoryReservation.class, history_reservationKey);
            HistoryReservationDTO history_reservationDTO = new HistoryReservationDTO();
            history_reservationDTO.setIsbn(history_reservation.getIsbn());
            history_reservationDTO.setEnd_date(history_reservation.getEnd_date());
            history_reservationDTO.setUser(history_reservation.getUser());
            history_reservationDTO.setStart_date(history_reservation.getStart_date());
            history_reservationDTO.setDeleted(history_reservation.isDeleted());
            return history_reservationDTO;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public boolean deleteUserByEmail(String email) throws RemoteDBException {
        try {
            User user = entityManager.find(User.class, email);
            if (user != null) {
                entityManager.remove(user);
                return true;
            }
            return false;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public boolean deleteBookByIsbn(String isbn) throws RemoteDBException {
        try {
            Book book = entityManager.find(Book.class, isbn);
            if (book != null) {
                entityManager.remove(book);
                return true;
            }
            return false;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public boolean deleteWishlistByKey(String user, String isbn) throws RemoteDBException {
        try {
            WishListKey wishListKey = new WishListKey(user, isbn);
            Wishlist wishlist = entityManager.find(Wishlist.class, wishListKey);
            if (wishlist != null) {
                entityManager.remove(wishlist);
                return true;
            }
            return false;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public boolean deleteGradeByKey(String user, String isbn) throws RemoteDBException {
        try {
            GradeKey gradeKey = new GradeKey(user, isbn);
            Grade grade = entityManager.find(Grade.class, gradeKey);
            if (grade != null) {
                entityManager.remove(grade);
                return true;
            }
            return false;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public boolean deleteHistoryLoanByKey(String user, String isbn, String id_copy, Date start_date) throws RemoteDBException {
        try {
            HistoryLoanKey history_loanKey = new HistoryLoanKey(user, isbn, id_copy, start_date);
            HistoryLoan history_loan = entityManager.find(HistoryLoan.class, history_loanKey);
            if (history_loan != null) {
                entityManager.remove(history_loan);
                return true;
            }
            return false;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public boolean deleteHistoryReservationByKey(String user, String isbn, String id_copy, Date start_date) throws RemoteDBException {
        try {
            HistoryReservationKey history_reservationKey = new HistoryReservationKey(user, isbn, start_date);
            HistoryReservation history_reservation = entityManager.find(HistoryReservation.class, history_reservationKey);
            if (history_reservation != null) {
                entityManager.remove(history_reservation);
                return true;
            }
            return false;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public UserDTO saveOrUpdateUser(UserDTO userDTO, boolean update) throws RemoteDBException {
        try {
            User user = entityManager.find(User.class, userDTO.getEmail());
            if (!update && user != null) { //Duplicate user
                return null;
            } else if (!update) {
                user = new User();
            }
            user.setEmail(userDTO.getEmail());
            user.setAddress(userDTO.getAddress());
            user.setBirthday(userDTO.getBirthday());
            user.setImage(userDTO.getImage());
            user.setName(userDTO.getName());
            user.setSurname(userDTO.getSurname());
            user.setPassword(userDTO.getPassword());
            if (!update) {
                entityManager.persist(user);
            } else {
                entityManager.merge(user);
            }
            return userDTO;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public BookDTO saveOrUpdateBook(BookDTO bookDTO, boolean update) throws RemoteDBException {
        try {
            Book book;
            if (!update) {
                book = new Book();
            } else {
                book = entityManager.find(Book.class, bookDTO.getIsbn());
            }
            book.setIsbn(bookDTO.getIsbn());
            book.setTitle(bookDTO.getTitle());
            book.setGenre(bookDTO.getGenre());
            book.setPublisher(bookDTO.getPublisher());
            book.setImageUrlS(bookDTO.getImageUrlS());
            book.setImageUrlM(bookDTO.getImageUrlM());
            book.setImageUrlL(bookDTO.getImageUrlL());
            book.setDescription(bookDTO.getDescription());
            book.setYearOfPublication(bookDTO.getYearOfPublication());
            book.setLanguage(bookDTO.getLanguage());
            if (!update) {
                entityManager.persist(book);
            } else {
                entityManager.merge(book);
            }
            return bookDTO;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public WishlistDTO saveOrUpdateWishlist(WishlistDTO wishlistDTO, boolean update) throws RemoteDBException {
        try {
            Wishlist wishlist;
            if (!update) {
                wishlist = new Wishlist();
            } else {
                wishlist = entityManager.find(Wishlist.class, new WishListKey(wishlistDTO.getUser(), wishlistDTO.getIsbn()));
            }
            wishlist.setIsbn(wishlistDTO.getIsbn());
            wishlist.setUser(wishlistDTO.getUser());
            if (!update) {
                entityManager.persist(wishlist);
            } else {
                entityManager.merge(wishlist);
            }
            return wishlistDTO;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public GradeDTO saveOrUpdateGrade(GradeDTO gradesDTO) throws RemoteDBException {
        try{
            boolean update = true;
            Grade grade = entityManager.find(
                    Grade.class,
                    new GradeKey(gradesDTO.getUser(), gradesDTO.getIsbn())
            );
            if (grade == null) {
                grade = new Grade();
                update = false;
            }
            grade.setIsbn(gradesDTO.getIsbn());
            grade.setUser(gradesDTO.getUser());
            grade.setStars(gradesDTO.getStars());
            if (!update) {
                entityManager.persist(grade);
            } else {
                entityManager.merge(grade);
            }
            return gradesDTO;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public HistoryReservationDTO saveOrUpdateHistoryReservation(HistoryReservationDTO history_reservationDTO, boolean update) throws RemoteDBException {
        try {
            HistoryReservation history_reservation = null;
            if (!update) {
                history_reservation = new HistoryReservation();
            } else {
                history_reservation = entityManager.find(HistoryReservation.class, new HistoryReservationKey(
                        history_reservationDTO.getUser(), history_reservationDTO.getIsbn(),
                        history_reservationDTO.getStart_date()));
            }

            history_reservation.setIsbn(history_reservationDTO.getIsbn());
            history_reservation.setEnd_date(history_reservationDTO.getEnd_date());
            history_reservation.setUser(history_reservationDTO.getUser());
            history_reservation.setStart_date(history_reservationDTO.getStart_date());
            history_reservation.setDeleted(history_reservationDTO.isDeleted());
            if (!update) {
                entityManager.persist(history_reservation);
            } else {
                entityManager.merge(history_reservation);
            }
            return history_reservationDTO;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public HistoryLoanDTO saveOrUpdateHistoryLoan(HistoryLoanDTO history_loanDTO, boolean update) throws RemoteDBException {
        try {
            HistoryLoan history_loan;
            if (!update) {
                history_loan = new HistoryLoan();
            } else {
                history_loan = entityManager.find(HistoryLoan.class, new HistoryLoanKey(
                        history_loanDTO.getUser(), history_loanDTO.getIsbn(),
                        history_loanDTO.getId_copy(), history_loanDTO.getStart_date()));
            }

            history_loan.setIsbn(history_loanDTO.getIsbn());
            history_loan.setEnd_date(history_loanDTO.getEnd_date());
            history_loan.setId_copy(history_loanDTO.getId_copy());
            history_loan.setUser(history_loanDTO.getUser());
            history_loan.setStart_date(history_loanDTO.getStart_date());

            if (!update) {
                entityManager.persist(history_loan);
            } else {
                entityManager.merge(history_loan);
            }
            return history_loanDTO;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public List<BookDTO> listPaginationBook(int offset, int page, BookDTO filter) throws RemoteDBException {
        try {
            Map<String, Object> parameters = new HashMap<>();
            StringBuilder jpql = new StringBuilder("select b from Book b where 1=1");

            applyBookFilter(jpql, filter, parameters);

            Query query = entityManager.createQuery(jpql.toString());
            for (Map.Entry<String, Object> paramKeyValue : parameters.entrySet()) {
                query.setParameter(paramKeyValue.getKey(), paramKeyValue.getValue());
            }
            query.setMaxResults(page);
            query.setFirstResult(page * offset);
            List<Book> booksList = query.getResultList();
            List<BookDTO> toReturnList = new ArrayList<>();

            if (booksList != null && !booksList.isEmpty()) {
                for (Book book : booksList) {
                    BookDTO bookDTO = buildBookDTO(book);
                    toReturnList.add(bookDTO);
                }
            }

            return toReturnList;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }

    @Override
    public Long countBooks(BookDTO filter) throws RemoteDBException {
        try{
            Map<String, Object> parameters = new HashMap<>();
            StringBuilder jpql = new StringBuilder("select count(b.isbn) from Book b where 1=1");

            applyBookFilter(jpql, filter, parameters);

            Query query = entityManager.createQuery(jpql.toString());
            for (Map.Entry<String, Object> paramKeyValue: parameters.entrySet()){
                query.setParameter(paramKeyValue.getKey(), paramKeyValue.getValue());
            }

            return (Long) query.getSingleResult();
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());
        }
    }
    @Override
    public Double computeRating(String isbn) throws RemoteDBException  {
        try {
            Query query = entityManager.createQuery(
                    "select avg(g.stars) from Grade g where g.isbn = :isbn group by g.isbn"
            );
            query.setParameter("isbn", isbn);
            return (Double) query.getSingleResult();
        }
        catch (NoResultException ex){
            return 0.0;
        }
        catch (Exception ex){
            throw new RemoteDBException(ex.getMessage());

        }
    }

    private void applyBookFilter(StringBuilder jpql, BookDTO filter, Map<String, Object> parameters) throws RemoteDBException {
        if (filter.getTitle() != null && !filter.getTitle().isEmpty()){
            jpql.append(" and lower(b.book_title) like concat('%', lower(:book_title), '%')");
            parameters.put("book_title", filter.getTitle());
        }
        if (filter.getAuthor() != null && !filter.getAuthor().isEmpty()){
            jpql.append(" and lower(b.book_author) like concat('%', lower(:book_author), '%') ");
            parameters.put("book_author", filter.getAuthor());
        }
        if (filter.getGenre() != null && !filter.getGenre().isEmpty()){
            jpql.append(" and lower(b.genre) like concat('%', lower(:genre), '%') ");
            parameters.put("genre", filter.getGenre());
        }
        if (filter.getIsbn() != null && !filter.getIsbn().isEmpty()){
            jpql.append(" and lower(b.isbn) like concat('%', lower(:isbn), '%') ");
            parameters.put("isbn", filter.getIsbn());
        }
    }
    
    private BookDTO buildBookDTO(Book book) throws RemoteDBException {
        BookDTO bookDTO = new BookDTO();
        bookDTO.setIsbn(book.getIsbn());
        bookDTO.setTitle(book.getTitle());
        bookDTO.setGenre(book.getGenre());
        bookDTO.setPublisher(book.getPublisher());
        bookDTO.setImageUrlS(book.getImageUrlS());
        bookDTO.setImageUrlM(book.getImageUrlM());
        bookDTO.setImageUrlL(book.getImageUrlL());
        bookDTO.setDescription(book.getDescription());
        bookDTO.setYearOfPublication(book.getYearOfPublication());
        bookDTO.setLanguage(book.getLanguage());
        return bookDTO;
    }

    private UserDTO buildUserDTO(User user) throws RemoteDBException {
        UserDTO userDTO = new UserDTO();
        userDTO.setEmail(user.getEmail());
        userDTO.setAddress(user.getAddress());
        userDTO.setBirthday(user.getBirthday());
        userDTO.setImage(user.getImage());
        userDTO.setName(user.getName());
        userDTO.setSurname(user.getSurname());
        userDTO.setPassword(user.getPassword());

        return userDTO;
    }
}

