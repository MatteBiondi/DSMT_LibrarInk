package it.unipi.dsmt.servlet;

import javax.servlet.*;
import javax.servlet.annotation.*;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.Enumeration;
import java.util.logging.Logger;

@WebFilter(
        filterName = "RequestLoggingFilter",
        urlPatterns = {"/*"},
        dispatcherTypes = DispatcherType.REQUEST,
        description = "Log all the logging request received"
)
public class RequestLoggingFilter implements Filter {
    private static final Logger LOGGER = Logger.getLogger(HomepageServlet.class.getName());

    public void init(FilterConfig config){
        LOGGER.info("RequestLoggingFilter initialized");
    }

    public void destroy() {
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws ServletException, IOException {
        HttpServletRequest req = (HttpServletRequest) request;
        Enumeration<String> params = req.getParameterNames();
        while(params.hasMoreElements()){
            String name = params.nextElement();
            String value = request.getParameter(name);
            if(!name.equals("password"))
                LOGGER.info(String.format("%s::Request Params::{%s=%s}",req.getRemoteAddr(), name, value));
        }

        // pass the request along the filter chain
        chain.doFilter(request, response);
    }
}
