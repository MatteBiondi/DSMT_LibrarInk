package it.unipi.dsmt.librarink.entities;

import java.io.Serializable;
import java.sql.Timestamp;

public class HistoryLoanKey implements Serializable {

    private String user;
    private String isbn;
    private String id_copy;
    private Timestamp start_date;

    public HistoryLoanKey(String user, String isbn, String id_copy, Timestamp start_date) {
        this.user = user;
        this.isbn = isbn;
        this.id_copy = id_copy;
        this.start_date = start_date;
    }

    public String getUser() {
        return user;
    }

    public void setUser(String email) {
        this.user = email;
    }

    public String getIsbn() {
        return isbn;
    }

    public void setIsbn(String isbn) {
        this.isbn = isbn;
    }

    public String getId_copy() {
        return id_copy;
    }

    public void setId_copy(String id_copy) {
        this.id_copy = id_copy;
    }

    public Timestamp getStart_date() {
        return start_date;
    }

    public void setStart_date(Timestamp start_date) {
        this.start_date = start_date;
    }
}
