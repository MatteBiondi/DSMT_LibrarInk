package it.unipi.dsmt.servlet;

import com.google.common.hash.Hashing;
import it.unipi.dsmt.librarink.LibrarinkRemote;
import it.unipi.dsmt.librarink.Librarink_usersDTO;

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
import java.util.List;

/**
 * This class implement the login behind the user login and login page visualization
 */
@WebServlet(name = "loginServlet", value = "/login")
public class LoginServlet extends HttpServlet {
    @EJB
    private LibrarinkRemote librarinkRemote;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // Get request are handled to let user see the login page in case of not logged-in user.
        // Otherwise, user is redirected to the homepage

        HttpSession session = request.getSession(false);
        if (session == null || session.getAttribute("email") == null) {
            // Not logged in user
            response.setContentType("text/html");
            String TargetJSP = "/pages/jsp/login.jsp";
            RequestDispatcher requestDispatcher = request.getRequestDispatcher(TargetJSP);
            requestDispatcher.forward(request, response);
        }
        else{
            //Already logged user
            response.setContentType("text/html");
            response.sendRedirect(request.getContextPath() + "/homepage");
        }
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // Post request are handled to let user log-in the system.

        Librarink_usersDTO usersFilter = new Librarink_usersDTO();
        // Get user inserted credentials
        String email = request.getParameter("email");
        String password = request.getParameter("password");

        // Retrieve a user with that email
        usersFilter.setEmail(email);
        List<Librarink_usersDTO> usersDTO_list = librarinkRemote.listUsers(usersFilter);
        if (!usersDTO_list.isEmpty())
        {
            // Check password correctness

            Librarink_usersDTO usersDTO = usersDTO_list.get(0);
            // Hash inserted password
            String hashedPsw = Hashing.sha256()
                    .hashString(password, StandardCharsets.UTF_8)
                    .toString();
            String savedPsw = usersDTO.getPassword();
            if(hashedPsw.equals(savedPsw)) {
                // Credentials match
                // Create a session
                HttpSession session = request.getSession(true);
                // In a session we save the user email
                session.setAttribute("email", email);
                // Set session to expire in 30 mins
                session.setMaxInactiveInterval(30 * 60);

                response.setContentType("text/html");
                response.sendRedirect(request.getContextPath() + "/homepage");
                return;
            }
        }

        // User not exist or incorrect credentials
        response.setContentType("text/html");
        String TargetJSP ="/pages/jsp/login.jsp";
        request.setAttribute("message","Password or  email not valid");
        request.setAttribute("messageType","error-message");
        RequestDispatcher requestDispatcher=request.getRequestDispatcher(TargetJSP);
        requestDispatcher.forward(request,response);
    }
}
