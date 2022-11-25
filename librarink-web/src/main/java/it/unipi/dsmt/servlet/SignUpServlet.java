package it.unipi.dsmt.servlet;

import com.google.common.hash.Hashing;
import it.unipi.dsmt.librarink.LibrarinkRemote;
import it.unipi.dsmt.librarink.UserDTO;
import it.unipi.dsmt.librarink.RemoteDBException;

import javax.ejb.EJB;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.Date;

/**
 * This class allow the user to create its own profile and to see the signup form page.
 */
@WebServlet(name = "SignUpServlet", value = "/signup")
public class SignUpServlet extends HttpServlet {
    @EJB
    private LibrarinkRemote librarinkRemote;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // Get request are handled to let user see the signup page in case of not logged-in user.
        // Otherwise, user is redirected to the homepage

        HttpSession session = request.getSession(false);
        if (session == null || session.getAttribute("email") == null) {
            // User is not logged in
            response.setContentType("text/html");
            String resourceURL = "/pages/jsp/signUp.jsp";
            RequestDispatcher rd = request.getRequestDispatcher(resourceURL);
            rd.forward(request, response);
        }
        else{
            // Already logged user
            response.setContentType("text/html");
            response.sendRedirect(request.getContextPath() + "/homepage");
        }
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // Post request are handled to let user create its own profile in the system.

        // Get signup form data
        String name = request.getParameter("name");
        String email = request.getParameter("email");
        String surname = request.getParameter("surname");
        String password = request.getParameter("password");
        Date birthday = Date.valueOf(request.getParameter("birthday"));
        String address = request.getParameter("address");

        // Hash user inserted password
        String hashedPsw = Hashing.sha256()
                .hashString(password, StandardCharsets.UTF_8)
                .toString();

        // Store the new user information into the database
        UserDTO new_user = new UserDTO();
        new_user.setPassword(hashedPsw);
        new_user.setBirthday(birthday);
        new_user.setEmail(email);
        new_user.setAddress(address);
        new_user.setName(name);
        new_user.setSurname(surname);
        new_user.setImage("https://cdn.onlinewebfonts.com/svg/img_335286.png");
        try {
            UserDTO user = librarinkRemote.saveOrUpdateUser(new_user, false);
            // Saving success. User is redirected to login page
            request.getSession().setAttribute("message", "User created");
            request.getSession().setAttribute("messageType", "success-message");
            response.setContentType("text/html");
            response.sendRedirect(request.getContextPath() + "/login");
        }
        catch (RemoteDBException ex){
            String resourceURL;
            // Saving failed
            response.setContentType("text/html");
            resourceURL = "/pages/jsp/signUp.jsp";
            request.setAttribute("message", "User already exists");
            RequestDispatcher rd = request.getRequestDispatcher(resourceURL);
            rd.forward(request, response);
        }
    }
}
