package it.unipi.dsmt.servlet;

import javax.servlet.*;
import javax.servlet.annotation.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.util.logging.Logger;

/**
 * This class implements a Servlet Filter to check in advance if
 *      - Logged user is trying to access to admin pages
 *      - Admin user is trying to access to "normal" logged user pages
 * In that case user will be redirected to homepage.
 * Pay attention:
 * 1)This filter bases its logic on the existence of another filter that checks the user login execution
 * 2)Any files that the admin needs to access include "admin" in their name
 */

@WebFilter(
        filterName = "UnauthorizedAccessFilter.java",
        urlPatterns = {"/*"},
        description = "Check if the logged user is trying to access " +
                "pages for which they do not have authorisation. " +
                "If yes, redirect the user to the relative homepage"
)
public class UnauthorizedAccessFilter implements Filter {
    private static final Logger LOGGER = Logger.getLogger(HomepageServlet.class.getName());

    public void init(FilterConfig config) {
    }

    public void destroy() {
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws ServletException, IOException {

        HttpServletRequest req = (HttpServletRequest) request;
        HttpServletResponse res = (HttpServletResponse) response;

        // Get current session
        HttpSession session = req.getSession(false);

        // Check if the user is logged
        boolean logged = session.getAttribute("email") != null;
        if(!logged){
            chain.doFilter(request, response);
            return;
        }

        // Check if the user is the admin one
        boolean isAdmin = session.getAttribute("admin") != null;

        // Get last part of URI
        String uri = req.getRequestURI();
        String servletReq = uri.substring(uri.lastIndexOf('/') + 1);
        LOGGER.info("[Access user]: isAdmin=>"+isAdmin+"; ServletReq=>"+servletReq);

        if(!isAdmin && servletReq.toLowerCase().contains("admin")){
            // Not admin user is trying to access to admin page
            res.sendRedirect(req.getContextPath() + "/homepage");
        }
        else if(isAdmin && !servletReq.toLowerCase().contains("admin") && !servletReq.equals("async")){
            // Admin user is trying to access to non-admin page
            res.sendRedirect(req.getContextPath() + "/admin");
        }
        else
            chain.doFilter(request, response);

    }
}
