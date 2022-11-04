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
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.Date;

@WebServlet(name = "SignUpServlet", value = "/signup")
public class SignUpServlet extends HttpServlet {
    @EJB
    private LibrarinkRemote librarinkRemote;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        response.setContentType("text/html");
        String resourceURL = "/pages/jsp/signUp.jsp";
        RequestDispatcher rd = request.getRequestDispatcher(resourceURL);
        rd.forward(request, response);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String name = request.getParameter("name");
        String email = request.getParameter("email");
        String surname = request.getParameter("surname");
        String password = request.getParameter("password");
        //hash psw
        String hashedPsw = Hashing.sha256()
                .hashString(password, StandardCharsets.UTF_8)
                .toString();
        Date birthday = Date.valueOf(request.getParameter("birthday"));
        String address = request.getParameter("address");
        Librarink_usersDTO new_user = new Librarink_usersDTO();
        new_user.setPassword(hashedPsw);
        new_user.setBirthday(birthday);
        new_user.setEmail(email);
        new_user.setAddress(address);
        new_user.setName(name);
        new_user.setSurname(surname);
        new_user.setImage("https://cdn.onlinewebfonts.com/svg/img_335286.png");
        Librarink_usersDTO user= librarinkRemote.saveOrUpdateUser(new_user,false);
        String resourceURL;
        if (user==null)
        {
            response.setContentType("text/html");
            resourceURL = "/pages/jsp/signUp.jsp";
            request.setAttribute("message", "User already exists");
            RequestDispatcher rd = request.getRequestDispatcher(resourceURL);
            rd.forward(request, response);
        }
        else
        {
            request.getSession().setAttribute("message", "User created");
            response.setContentType("text/html");
            response.sendRedirect(request.getContextPath() + "/login");
        }

    }
}
