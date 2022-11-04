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
        Librarink_usersDTO usersFilter = new Librarink_usersDTO();
        String email = request.getParameter("email");
        String password = request.getParameter("password");
        usersFilter.setEmail(email);
        List<Librarink_usersDTO> usersDTO_list = librarinkRemote.listUsers(usersFilter);
        if (!usersDTO_list.isEmpty())
        {
            Librarink_usersDTO usersDTO = usersDTO_list.get(0);
            //check psw
            String hashedPsw = Hashing.sha256()
                    .hashString(password, StandardCharsets.UTF_8)
                    .toString();
            String savedPsw = usersDTO.getPassword();
            if(hashedPsw.equals(savedPsw)) {
                HttpSession session = request.getSession(true);
                //a user session is composed by email and password
                session.setAttribute("email", email);
                //setting session to expiry in 30 mins
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
        RequestDispatcher requestDispatcher=request.getRequestDispatcher(TargetJSP);
        requestDispatcher.forward(request,response);
    }
}
