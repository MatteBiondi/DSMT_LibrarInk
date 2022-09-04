package it.unipi.dsmt.librarink;

import java.io.Serializable;
import java.sql.Date;

public class libraink_history_loanDTO implements Serializable {
    String user_email;
    String isbn;
    String id_copy;
    Date start_date;
    Date end_date;

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

    public Date getEnd_date() {
        return end_date;
    }

    public void setEnd_date(Date end_date) {
        this.end_date = end_date;
    }

    @Override
    public String toString() {
        return "libraink_history_loanDTO{" +
                "user_email='" + user_email + '\'' +
                ", isbn='" + isbn + '\'' +
                ", id_copy='" + id_copy + '\'' +
                ", start_date=" + start_date +
                ", end_date=" + end_date +
                '}';
    }
}
