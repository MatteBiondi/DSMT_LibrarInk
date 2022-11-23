package it.unipi.dsmt.servlet;

import it.unipi.dsmt.librarink.LibrarinkRemote;
import it.unipi.dsmt.librarink.Librarink_usersDTO;

import javax.ejb.EJB;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * This class allow user to access its own profile page when it's logged-in
 */
@WebServlet(name = "UserProfileServlet", value = "/user")
public class UserProfileServlet extends HttpServlet {
    @EJB
    LibrarinkRemote remote;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        String email = (String) request.getSession().getAttribute("email");

        // Retrieve any user information from database
        Librarink_usersDTO user = remote.findUsersByEmail(email);

        // Set any information as parameter for JSP page
        request.setAttribute("name", user.getName());
        request.setAttribute("surname", user.getSurname());
        request.setAttribute("address", user.getAddress());
        request.setAttribute("birthday", user.getBirthday());
        request.setAttribute("email", email);
        request.setAttribute("image", user.getImage());

        // Redirect user to the profile page
        response.setContentType("text/html");
        getServletContext().getRequestDispatcher("/pages/jsp/userProfile.jsp").forward(request, response);
    }
}
