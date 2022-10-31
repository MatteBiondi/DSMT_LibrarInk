package it.unipi.dsmt.librarink.entities;

import java.io.Serializable;
import java.sql.Date;

public class History_reservationKey implements Serializable {

    private String user_email;
    private String isbn;
    private Date start_date;

    public History_reservationKey(String user_email, String isbn, Date start_date) {
        this.user_email = user_email;
        this.isbn = isbn;
        this.start_date = start_date;
    }

    public String getUser_email() {
        return user_email;
    }

    public void setUser_email(String user_email) {
        this.user_email = user_email;
    }

    public String getIsbn() {
        return isbn;
    }

    public void setIsbn(String isbn) {
        this.isbn = isbn;
    }

    public Date getStart_date() {
        return start_date;
    }

    public void setStart_date(Date start_date) {
        this.start_date = start_date;
    }
}
