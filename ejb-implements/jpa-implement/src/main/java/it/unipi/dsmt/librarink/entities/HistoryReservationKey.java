package it.unipi.dsmt.librarink.entities;

import java.io.Serializable;
import java.sql.Timestamp;

public class HistoryReservationKey implements Serializable {

    private String user;
    private String isbn;
    private Timestamp start_date;

    public HistoryReservationKey(String user, String isbn, Timestamp start_date) {
        this.user = user;
        this.isbn = isbn;
        this.start_date = start_date;
    }

    public String getUser() {
        return user;
    }

    public void setUser(String user) {
        this.user = user;
    }

    public String getIsbn() {
        return isbn;
    }

    public void setIsbn(String isbn) {
        this.isbn = isbn;
    }

    public Timestamp getStart_date() {
        return start_date;
    }

    public void setStart_date(Timestamp start_date) {
        this.start_date = start_date;
    }
}
