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
 * This class takes care of asynchronous operations requested by web client to retrieve/update data from databases
 **/
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

        PrintWriter writer = response.getWriter();
        response.setContentType("application/json");

        List<LoanDTO> loans;
        List<ReservationDTO> reservations;

        try {
            // Identify the requested action, forward it to erlang client and send back a response to client
            switch (request.getParameter("request")){
                case "write_reservation":
                    writer.write(erlang_client.write_reservation(user, isbn));
                    return;
                case "delete_reservation":
                    writer.write(erlang_client.delete_reservation(user, isbn));
                    return;
                case "count_available_copies":
                    writer.write(erlang_client.count_available_copies(isbn));
                    return;
                case "read_loans":
                    loans = erlang_client.read_loans(user, null, null);
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
                case "cancel_reservation":
                    writer.write(erlang_client.cancel_reservation(user, isbn));
                    return;
                case "add_wishlist":
                    WishlistDTO new_item = new WishlistDTO();
                    new_item.setUser(user);
                    new_item.setIsbn(isbn);
                    remote.saveOrUpdateWishlist(new_item, false);
                    writer.write("{\"result\": \"succeed\", \"response\": \"ok\"}");
                    return;
                case "remove_wishlist":
                    boolean deleted = remote.deleteWishlistByKey(user, isbn);
                    if(deleted){
                        writer.write("{\"result\": \"succeed\", \"response\": \"ok\"}");
                    }
                    else {
                        writer.write("{\"result\": \"error\", \"response\": \"something went wrong\"}");
                    }
                    return;
                case "load_wishlist":
                    WishlistDTO filter_wishlist = new WishlistDTO();
                    filter_wishlist.setUser(user);
                    List<WishlistDTO> wishlist = remote.listWishlist(filter_wishlist);

                    JsonArray wishlist_js = new JsonArray();
                    for (WishlistDTO wishlist_item: wishlist){
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
                        writer.write("{\"result\": \"error\", \"response\": \"something went wrong\"}");
                        return;
                    }

                    GradeDTO grade = new GradeDTO();
                    grade.setUser(user);
                    grade.setIsbn(isbn);
                    grade.setStars(stars);

                    GradeDTO grade_result =  remote.saveOrUpdateGrade(grade);
                    if(grade_result != null)
                        writer.write("{\"result\": \"succeed\", \"response\": \"ok\"}");
                    else
                        writer.write("{\"result\": \"error\", \"response\": \"something went wrong\"}");

                    return;
                case "load_grades":
                    GradeDTO grade_filter = new GradeDTO();
                    grade_filter.setUser(user);
                    List<GradeDTO> grades = remote.listGrades(grade_filter);

                    JsonArray grades_js = new JsonArray();
                    for (GradeDTO grade_item: grades){
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
                        writer.write("{\"result\": \"error\", \"response\": \"something went wrong\"}");
                    }
                    return;
                case "book_title":
                    BookDTO book_title = remote.findBooksByIsbn(isbn);
                    if(book_title != null)
                        writer.write(String.format("{\"result\": \"succeed\", \"title\": \"%s\"}",
                                book_title.getTitle()));
                    else
                        writer.write("{\"result\": \"error\", \"response\": \"book not found\"}");
                    return;
                case "load_image_url":
                    // Given an isbn, retrieve the book's cover url
                    BookDTO book = remote.findBooksByIsbn(isbn);

                    JsonObject book_and_url_obj = new JsonObject();
                    book_and_url_obj.addProperty("isbn", book.getIsbn());
                    book_and_url_obj.addProperty("url", book.getImageUrlL());

                    writer.write(book_and_url_obj.toString());
                    return;
            }

            // Admin only requests
            if(request.getSession().getAttribute("admin") != null){
                switch (request.getParameter("request")){
                    case "available_copy_ids":
                        List<BookCopyDTO> copies = erlang_client.read_available_copies(isbn);
                        JsonArray copies_js = new JsonArray();
                        for(BookCopyDTO copy: copies){
                            copies_js.add(copy.getId());
                        }
                        writer.write(copies_js.toString());
                        return;
                    case "write_copy":
                        writer.write(erlang_client.write_copy(isbn));
                        return;
                    case "delete_copy":
                        writer.write(erlang_client.delete_copy(isbn, request.getParameter("id")));
                        return;
                    case "write_loan":
                        LoanDTO loan = erlang_client.write_loan(user, isbn, request.getParameter("id"));
                        if (loan!=null)
                            writer.write(String.format("{\"id\":\"%s\"}",loan.getCopyId()));
                        else
                            writer.write("{\"error\":\"Unexpected error\"}");
                        return;
                    case "delete_reservation":
                        writer.write(erlang_client.delete_reservation(user, isbn));
                        return;
                    case "delete_loan":
                        writer.write(erlang_client.delete_loan(user, isbn, request.getParameter("id")));
                        return;
                    case "terminate_loan":
                        writer.write(erlang_client.terminate_loan(isbn, request.getParameter("id")));
                        return;
                    case "renew_loan":
                        writer.write(erlang_client.renew_loan(isbn, request.getParameter("id")));
                        return;
                }
            }
            writer.write("{\"result\": \"error\", \"response\": \"unexpected request\"}");
        }
        catch (ErlangClientException | RemoteDBException ex){
            writer.write(String.format("%s", ex.getMessage()));
        }
        catch (EJBException ex){
            LOGGER.warning(String.format("EJB exception %s", ex.getMessage()));
            writer.write("{\"result\": \"error\", \"response\": \"server error\"}");
        }
    }
}
