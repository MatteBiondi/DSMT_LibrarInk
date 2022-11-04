package it.unipi.dsmt.servlet;

import javax.servlet.*;
import javax.servlet.annotation.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.util.logging.Logger;

@WebFilter(
        filterName = "AuthenticationFilter",
        urlPatterns = {"/*"},
        description = "Check if the user is logged or not. If no, redirect the user to login page"
)

public class AuthenticationFilter implements Filter {
    private static final Logger LOGGER = Logger.getLogger(HomepageServlet.class.getName());

    public void init(FilterConfig config) {
        LOGGER.info("AuthenticationFilter initialized");
    }

    public void destroy() {
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws ServletException, IOException {
        HttpServletRequest req = (HttpServletRequest) request;
        HttpServletResponse res = (HttpServletResponse) response;

        String uri = req.getRequestURI();
        LOGGER.info(String.format("Requested Resource:: %s", uri));

        HttpSession session = req.getSession(false);
        //todo se page non esiste -> errore senza filtro
        if(!uri.endsWith("login") && !uri.endsWith("signup") && (session == null || session.getAttribute("email") == null)){
            LOGGER.info("Unauthorized, access request");
            req.getSession().setAttribute("message", "Login is needed");
            res.sendRedirect(req.getContextPath() + "/login");
        }else{
            // pass the request along the filter chain
            chain.doFilter(request, response);
        }
    }
}
