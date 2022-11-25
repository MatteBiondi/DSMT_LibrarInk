package it.unipi.dsmt.servlet;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import it.unipi.dsmt.librarink.*;

import javax.ejb.EJB;
import javax.ejb.EJBException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.logging.Logger;

/**
 * This class is in charge of forward the received requests to the erlang client,
 * that allows manipulation of mnesia db.
 */
@WebServlet(name = "AsyncRequestServlet", value = "/request/async", loadOnStartup = 0)
public class AsyncRequestServlet extends HttpServlet {
    private static final Logger LOGGER = Logger.getLogger(AsyncRequestServlet.class.getName());

    @EJB
    private ErlangClient erlang_client;

    @EJB
    private LibrarinkRemote remote;

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {

        //Get each parameter and session attributes
        String user = (String) request.getSession().getAttribute("email");
        String isbn = request.getParameter("isbn");
        String id = request.getParameter("id");

        PrintWriter writer = response.getWriter();
        response.setContentType("application/json");

        List<LoanDTO> loans;
        List<ReservationDTO> reservations;

        try {
            // Identify the requested action, forward it to erlang client and send back a response to client
            switch (request.getParameter("request")){
                case "write_copy":
                    writer.write(erlang_client.write_copy(isbn, id));
                    return;
                //case "write_loan"://TODO: remove
                //    LoanDTO loan = erlang_client.write_loan(user, isbn, id);
                //    if (loan!=null)
                //        writer.write(String.format("{\"id\":\"%s\"}",loan.getId()));
                //    else
                //        writer.write("{\"error\":\"Unexpected error\"}");
                //    return;
                case "write_reservation":
                    writer.write(erlang_client.write_reservation(user, isbn));
                    return;
                //case "delete_copy"://TODO: remove
                //    writer.write(erlang_client.delete_copy(isbn, id));
                //    return;
                case "delete_loan":
                    writer.write(erlang_client.delete_loan(user, isbn, id));
                    return;
                case "delete_reservation":
                    writer.write(erlang_client.delete_reservation(user, isbn));
                    return;
                case "count_available_copies":
                    writer.write(erlang_client.count_available_copies(isbn));
                    return;
                case "read_loans":
                    loans = erlang_client.read_loans(user, isbn, id);
                    if (loans != null){
                        writer.write(loans.toString());
                        return;
                    }
                    break;
                case "read_reservations":
                    reservations = erlang_client.read_reservations(user, isbn);
                    if (reservations != null){
                        writer.write(reservations.toString());
                        return;
                    }
                    break;
                //case "terminate_loan"://TODO: remove
                //    writer.write(erlang_client.terminate_loan(isbn, id));
                //    return;
                //case "renew_loan"://TODO: remove
                //    writer.write(erlang_client.renew_loan(isbn, id));
                //    return;
                case "cancel_reservation":
                    writer.write(erlang_client.cancel_reservation(user, isbn));
                    return;
                case "add_wishlist":
                    Librarink_wishlistDTO new_item = new Librarink_wishlistDTO();
                    new_item.setEmail_user(user);
                    new_item.setIsbn(isbn);
                    Librarink_wishlistDTO result = remote.saveOrUpdateWishlist(new_item, false); // TODO: Should be boolean
                    writer.write("{\"result\": \"succeed\", \"response\": \"ok\"}");
                    return;
                case "remove_wishlist":
                    boolean deleted = remote.deleteWishlistByKey(user, isbn);
                    if(deleted){
                        writer.write("{\"result\": \"succeed\", \"response\": \"ok\"}");
                    }
                    else {
                        writer.write("{\"result\": \"error\"}");
                    }
                    return;
                case "load_wishlist":
                    Librarink_wishlistDTO filter_wishlist = new Librarink_wishlistDTO();
                    filter_wishlist.setEmail_user(user);
                    List<Librarink_wishlistDTO> wishlist = remote.listWishlist(filter_wishlist);

                    JsonArray wishlist_js = new JsonArray();
                    for (Librarink_wishlistDTO wishlist_item: wishlist){
                        wishlist_js.add(wishlist_item.getIsbn());
                    }
                    writer.write(wishlist_js.toString());
                    return;
                case "rate_book":
                    float stars;
                    try{
                        stars = Float.parseFloat(request.getParameter("grade"));
                    }
                    catch (NullPointerException | NumberFormatException ex){
                        writer.write("{\"error\":\"something went wrong\"}");
                        return;
                    }

                    Librarink_gradesDTO grade = new Librarink_gradesDTO();
                    grade.setUser_email(user);
                    grade.setIsbn(isbn);
                    grade.setStars(stars);

                    Librarink_gradesDTO grade_result =  remote.saveOrUpdateGrade(grade);
                    if(grade_result != null)
                        writer.write("{\"result\": \"succeed\", \"response\": \"ok\"}");
                    else
                        writer.write("{\"error\":\"something went wrong\"}");

                    return;
                case "load_grades":
                    Librarink_gradesDTO grade_filter = new Librarink_gradesDTO();
                    grade_filter.setUser_email(user);
                    List<Librarink_gradesDTO> grades = remote.listGrades(grade_filter);

                    JsonArray grades_js = new JsonArray();
                    for (Librarink_gradesDTO grade_item: grades){
                        JsonObject grade_obj = new JsonObject();
                        grade_obj.addProperty("isbn", grade_item.getIsbn());
                        grade_obj.addProperty("grade", grade_item.getStars());
                        grades_js.add(grade_obj);
                    }
                    writer.write(grades_js.toString());
                    return;
                case "compute_rating":
                    Double rating = remote.computeRating(isbn);
                    if (rating != null){
                        writer.write(String.format("{\"result\": \"succeed\", \"rating\": \"%f\"}", rating));
                    }
                    else {
                        writer.write("{\"error\":\"something went wrong\"}");
                    }
                    return;
                case "book_title":
                    Librarink_booksDTO book_title = remote.findBooksByIsbn(isbn);
                    if(book_title != null)
                        writer.write(String.format("{\"result\": \"succeed\", \"title\": \"%s\"}", book_title.getBook_title()));
                    else
                        writer.write("{\"error\":\"Book not found\"}");
                    return;
                case "load_image_url":
                    // Given an isbn, retrieve the book's cover url
                    Librarink_booksDTO book = remote.findBooksByIsbn(isbn);

                    JsonObject book_and_url_obj = new JsonObject();
                    book_and_url_obj.addProperty("isbn", book.getIsbn());
                    book_and_url_obj.addProperty("url", book.getImage_url_l());

                    writer.write(book_and_url_obj.toString());
                    return;
                case "available_copy_ids"://TODO check if admin
                    List<BookCopyDTO> copies = erlang_client.read_available_copies(isbn);
                    JsonArray copies_js = new JsonArray();
                    for(BookCopyDTO copy: copies){
                        copies_js.add(copy.getId());
                    }
                    writer.write(copies_js.toString());
                    return;
                default:
                    writer.write("{\"error\":\"Unexpected request\"}");
            }
        }
        catch (ErlangClientException ex){
            writer.write(String.format("{\"error\":\"%s\"}", ex.getMessage()));
        }
        catch (EJBException ex){
            LOGGER.warning(String.format("EJB exception %s", ex.getMessage()));
            writer.write("{\"error\":\"server error\"}");
        }

    }
}
