package it.unipi.dsmt.servlet;

import it.unipi.dsmt.librarink.libraink_usersDTO;
import it.unipi.dsmt.librarink.librarinkRemoteEJB;

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
public class loginServlet extends HttpServlet {
    @EJB
    private librarinkRemoteEJB librainkRemoteEJB;
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
            libraink_usersDTO usersDTO = null;
            if ("login".equalsIgnoreCase(action)) {
                libraink_usersDTO usersFilter = new libraink_usersDTO();
                String email = request.getParameter("email");
                String password = request.getParameter("password");
                usersFilter.setEmail(email);
                usersFilter.setPassword(password);
                usersDTO = (libraink_usersDTO) librainkRemoteEJB.listUsers(usersFilter);//potrebbe non funzionare
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
