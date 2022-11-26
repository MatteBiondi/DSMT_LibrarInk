package it.unipi.dsmt.servlet;

import com.google.common.hash.Hashing;
import it.unipi.dsmt.librarink.AdminDTO;
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
import java.util.List;
import java.util.logging.Logger;

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

        // Get user inserted credentials
        String email = request.getParameter("email");
        String password = request.getParameter("password");
        // Hash inserted password
        String hashedPsw = Hashing.sha256()
                .hashString(password, StandardCharsets.UTF_8)
                .toString();
        // Type of access
        String adminChecked = request.getParameter("adminCheck");

        try {
            if (adminChecked != null){
                AdminDTO adminFilter = new AdminDTO();
                // Retrieve admin by email
                adminFilter.setEmail(email);
                adminFilter.setPassword(hashedPsw);
                List<AdminDTO> adminDTO_list= librarinkRemote.listAdmins(adminFilter);
                if(!adminDTO_list.isEmpty()){
                    // Credentials match
                    // Create a session
                    HttpSession session = request.getSession(true);
                    // In a session we save the user email
                    session.setAttribute("email", email);
                    session.setAttribute("admin", true);
                    // Set session to expire in 30 mins
                    session.setMaxInactiveInterval(30 * 60);

                    response.setContentType("text/html");
                    response.sendRedirect(request.getContextPath() + "/admin");
                    return;
                }
            }
            else{
                UserDTO usersFilter = new UserDTO();
                // Retrieve a user with that email
                usersFilter.setEmail(email);
                usersFilter.setPassword(hashedPsw);
                List<UserDTO> usersDTO_list = librarinkRemote.listUsers(usersFilter);
                if (!usersDTO_list.isEmpty()) {
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
        }
        catch(RemoteDBException ex){
            Logger.getLogger(this.getClass().getName()).warning("Login error: " + ex.getMessage());
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
