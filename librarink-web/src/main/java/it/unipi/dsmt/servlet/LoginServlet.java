package it.unipi.dsmt.servlet;

import it.unipi.dsmt.librarink.LibrarinkRemote;
import it.unipi.dsmt.librarink.Librarink_usersDTO;
import org.springframework.security.crypto.argon2.Argon2PasswordEncoder;

import javax.ejb.EJB;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.http.Cookie;
import java.io.IOException;
import java.util.List;

@WebServlet(name = "loginServlet", value = "/login")
public class LoginServlet extends HttpServlet {
    @EJB
    private LibrarinkRemote librarinkRemote;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        response.setContentType("text/html");
        String TargetJSP ="/pages/jsp/login.jsp";
        RequestDispatcher requestDispatcher=request.getRequestDispatcher(TargetJSP);
        requestDispatcher.forward(request,response);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        Librarink_usersDTO usersDTO = null;
        Librarink_usersDTO usersFilter = new Librarink_usersDTO();
        String email = request.getParameter("email");
        String password = request.getParameter("password");
        usersFilter.setEmail(email);
        List<Librarink_usersDTO> usersDTO_list = librarinkRemote.listUsers(usersFilter);//todo potrebbe non funzionare
        usersDTO = usersDTO_list.get(0);
        if (usersDTO !=null)
        {
            //check psw
            Argon2PasswordEncoder encoder = new Argon2PasswordEncoder(32,64,1,15*1024,2);
            boolean validPassword = encoder.matches(password, usersDTO.getPassword());
            if(validPassword) {
                HttpSession session = request.getSession(true);
                //a user session is composed by email and password
                session.setAttribute("email", email);
                //setting session to expiry in 30 mins
                session.setMaxInactiveInterval(30 * 60);
                //todo controlla se serve o no cookie
                Cookie userEmail = new Cookie("email", email);
                userEmail.setMaxAge(30 * 60);
                response.addCookie(userEmail);
                response.setContentType("text/html");
                response.sendRedirect(request.getContextPath() + "/homepage");
                //todo to check
                //String TargetJSP = "/index.jsp";
                //todo serve? request.setAttribute("user", usersDTO);
                //RequestDispatcher requestDispatcher = request.getRequestDispatcher(TargetJSP);
                //requestDispatcher.forward(request, response);
            }
        }
        else
        {
            response.setContentType("text/html");
            String TargetJSP ="/pages/jsp/login.jsp";
            request.setAttribute("message","Password or  email not valid");
            RequestDispatcher requestDispatcher=request.getRequestDispatcher(TargetJSP);
            requestDispatcher.forward(request,response);
        }
    }
}
