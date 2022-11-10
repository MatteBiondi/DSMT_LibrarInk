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

@WebServlet(name = "UserProfileServlet", value = "/user")
public class UserProfileServlet extends HttpServlet {
    @EJB
    LibrarinkRemote remote;

    @Override
    public void init(){
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        String email = (String) request.getSession().getAttribute("email");
        Librarink_usersDTO user = remote.findUsersByEmail(email);

        request.setAttribute("name", user.getName());
        request.setAttribute("surname", user.getSurname());
        request.setAttribute("address", user.getAddress());
        request.setAttribute("birthday", user.getBirthday());
        //todo è necessario?
        request.setAttribute("email", email);
        //todo toglierei img da db e metterei semplicemente url in html
        request.setAttribute("image", user.getImage());
        response.setContentType("text/html");
        getServletContext().getRequestDispatcher("/pages/jsp/userProfile.jsp").forward(request, response);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    }
}
