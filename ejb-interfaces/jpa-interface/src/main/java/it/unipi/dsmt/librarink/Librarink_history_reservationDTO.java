package it.unipi.dsmt.librarink;

import java.io.Serializable;
import java.sql.Date;

public class Librarink_history_reservationDTO implements Serializable {
    String user_email;
    String id_copy;
    String isbn;
    Date start_date;
    Date end_date;
    boolean deleted;

    public String getUser_email() {
        return user_email;
    }

    public void setUser_email(String user_email) {
        this.user_email = user_email;
    }

    public String getId_copy() {
        return id_copy;
    }

    public void setId_copy(String id_copy) {
        this.id_copy = id_copy;
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

    public Date getEnd_date() {
        return end_date;
    }

    public void setEnd_date(Date end_date) {
        this.end_date = end_date;
    }

    public boolean isDeleted() {
        return deleted;
    }

    public void setDeleted(boolean deleted) {
        this.deleted = deleted;
    }

    @Override
    public String toString() {
        return "libraink_history_reservationDTO{" +
                "user_email='" + user_email + '\'' +
                ", id_copy='" + id_copy + '\'' +
                ", isbn='" + isbn + '\'' +
                ", start_date=" + start_date +
                ", end_date=" + end_date +
                ", deleted=" + deleted +
                '}';
    }
}
