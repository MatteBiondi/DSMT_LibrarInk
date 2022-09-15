package it.unipi.distributed_project.servlet;

import it.unipi.dsmt.librarink.libraink_usersDTO;
import it.unipi.dsmt.librarink.librarinkRemoteEJB;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.sql.Date;

@WebServlet(name = "SignUpServlet", value = "/SignUpServlet")
public class SignUpServlet extends HttpServlet {
    private librarinkRemoteEJB librainkRemoteEJB;
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String name = request.getParameter("name");
        String email = request.getParameter("email");
        String surname = request.getParameter("surname");
        String password = request.getParameter("password");
        Date birthday = Date.valueOf(request.getParameter("birthday"));
        String address = request.getParameter("address");
        libraink_usersDTO new_user = new libraink_usersDTO();
        new_user.setPassword(password);
        new_user.setBirthday(birthday);
        new_user.setEmail(email);
        new_user.setAddress(address);
        new_user.setName(name);
        new_user.setSurname(surname);
        new_user.setImage("https://cdn.onlinewebfonts.com/svg/img_335286.png");
        libraink_usersDTO user=librainkRemoteEJB.saveOrUpdateUser(new_user,false);
        String resourceURL;
        if (user==null)
        {
            resourceURL = "librarink-web/src/main/webapp/pages/jsp/signUp.jsp";
            request.setAttribute("message", "user alredy exists");
            RequestDispatcher rd = request.getRequestDispatcher(resourceURL);
            rd.forward(request, response);
        }
        else
        {
            resourceURL = "librarink-web/src/main/webapp/pages/jsp/login.jsp";
            request.setAttribute("message", "user created");
            RequestDispatcher rd = request.getRequestDispatcher(resourceURL);
            rd.forward(request, response);
            //notify user correctly registered
        }

    }
}
