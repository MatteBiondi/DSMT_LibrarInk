package it.unipi.dsmt.servlet;

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

@WebServlet(name = "loginServlet", value = "/loginServlet")
public class LoginServlet extends HttpServlet {
    @EJB
    private LibrarinkRemote librarinkRemote;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String resourceUrl = null;
        String action = request.getParameter("action");
        HttpSession session = request.getSession(true);
        /*if(session.getAttribute("email")!=null)
        {
            String email = (String) session.getAttribute("email");
            users
        }*/
        if (action != null) {
            Librarink_usersDTO usersDTO = null;
            if ("login".equalsIgnoreCase(action)) {
                Librarink_usersDTO usersFilter = new Librarink_usersDTO();
                String email = request.getParameter("email");
                String password = request.getParameter("password");
                usersFilter.setEmail(email);
                usersFilter.setPassword(password);
                usersDTO = (Librarink_usersDTO) librarinkRemote.listUsers(usersFilter);//potrebbe non funzionare
                if (usersDTO !=null)
                {
                    //a user session is composed by email and password
                    session.setAttribute("email",email);
                    session.setAttribute("password",password);
                    String targetJPS ="librarink-web/src/main/webapp/index.jsp";
                    request.setAttribute("user",usersDTO);
                    RequestDispatcher requestDispatcher=request.getRequestDispatcher(targetJPS);
                    requestDispatcher.forward(request,response);

                }
                else
                {

                    String targetJPS ="librarink-web/src/main/webapp/pages/jsp/login.jsp";
                    request.setAttribute("message","password or  email not valid");
                    RequestDispatcher requestDispatcher=request.getRequestDispatcher(targetJPS);
                    requestDispatcher.forward(request,response);
                }
            }

        }
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

    }
}
