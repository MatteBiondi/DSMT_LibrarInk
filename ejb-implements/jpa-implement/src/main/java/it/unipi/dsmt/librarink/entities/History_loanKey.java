package it.unipi.dsmt.librarink.entities;

import javax.persistence.Column;
import javax.persistence.Id;
import java.io.Serializable;
import java.sql.Date;

public class History_loanKey implements Serializable {

    private String user_email;
    private String isbn;
    private String id_copy;
    private Date start_date;

    public History_loanKey(String user_email, String isbn, String id_copy, Date start_date) {
        this.user_email = user_email;
        this.isbn = isbn;
        this.id_copy = id_copy;
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

    public String getId_copy() {
        return id_copy;
    }

    public void setId_copy(String id_copy) {
        this.id_copy = id_copy;
    }

    public Date getStart_date() {
        return start_date;
    }

    public void setStart_date(Date start_date) {
        this.start_date = start_date;
    }
}
