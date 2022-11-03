package it.unipi.dsmt.servlet;

import it.unipi.dsmt.librarink.LibrarinkRemote;
import it.unipi.dsmt.librarink.Librarink_booksDTO;

import javax.ejb.EJB;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;
import java.util.logging.Logger;

@WebServlet(name = "HomepageServlet", value = "/homepage", loadOnStartup = 0)
public class HomepageServlet extends HttpServlet {
    private static final Logger LOGGER = Logger.getLogger(HomepageServlet.class.getName());
    private int page_length;
    private int nav_items_length;
    @EJB
    LibrarinkRemote remote;

    @Override
    public void init(){
        InputStream input = null;
        Properties properties = new Properties();
        try {
            input = this.getClass().getClassLoader().getResourceAsStream("servlet.properties");
            properties.load(input);
        } catch (IOException e) {
            e.printStackTrace();
        }
        finally {
            try {
                if (input != null)
                    input.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        page_length = Integer.parseInt(properties.getProperty("page_length", "50"));
        nav_items_length = Integer.parseInt(properties.getProperty("nav_items_length", "11"));

        LOGGER.info("Init HomepageServlet");
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        LOGGER.info(String.format(
                "Request from session-id: %s\nParams: < user: %s, page: %s, search: %s => %s >",
                request.getSession().getId(),
                request.getSession().getAttribute("email"),
                request.getParameter("page"),
                request.getParameter("search"),
                request.getParameter("keyword")
        ));
        //String user = (String) request.getSession().getAttribute("email");

        // Get request parameters
        String filter_attribute = request.getParameter("search") == null ? "":request.getParameter("search");
        String filter_keyword = request.getParameter("keyword") == null ? "":request.getParameter("keyword");

        List<Librarink_booksDTO> books;
        Librarink_booksDTO filter = new Librarink_booksDTO();

        switch (filter_attribute.toLowerCase(Locale.ROOT)) {
            case "title":
                filter.setBook_title(filter_keyword);
                break;
            case "author":
                filter.setBook_author(filter_keyword);
                break;
            case "isbn":
                filter.setIsbn(filter_keyword);
                break;
            default: break;

        }
        int page_offset;
        int max_offset = Math.max(1,(int) Math.ceil((double) remote.count_book(filter) / page_length));
        try {
            page_offset = Integer.parseInt(request.getParameter("page"));
            page_offset = Math.max(page_offset, 1);
            page_offset = Math.min(page_offset, max_offset);
        }
        catch (NumberFormatException ignored){ page_offset = 1; }

        books = remote.list_pagination_book(page_offset - 1, page_length, filter);

        // Compute pagination indexes
        ArrayList<Integer> offsets  = new ArrayList<>();
        int before = page_offset - Math.max(1, page_offset - nav_items_length);
        int after = Math.min(max_offset, page_offset + nav_items_length) - page_offset;

        int index_before = before;
        int index_after = after;

        while ((after > 0 || before > 0) && offsets.size() <  Math.min(nav_items_length, max_offset)){
            if (index_before > 0)
                offsets.add(page_offset - (before - --index_before ));
            if (index_after >= 0 && offsets.size() < Math.min(nav_items_length, max_offset))
                offsets.add(page_offset + after - index_after-- );
        }
        Collections.sort(offsets);

        // Forward to jsp page
        response.setContentType("text/html");
        request.setAttribute("books", books);
        request.setAttribute("page_offset", page_offset);
        request.setAttribute("offsets", offsets.toArray());
        LOGGER.info(String.format("Filter attribute %s",filter_attribute));
        if (filter_attribute.equals("")) // Only if the homepage is directly requested by typing the link
            getServletContext().getRequestDispatcher("/index.jsp").forward(request, response);
        else
            getServletContext().getRequestDispatcher("/pages/common/book_list.jsp").forward(request, response);
    }
}
