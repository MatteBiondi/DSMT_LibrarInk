package it.unipi.dsmt.librarink;

import java.io.Serializable;
import java.sql.Date;

public class HistoryReservationDTO implements Serializable {
    String user;
    String isbn;
    Date start_date;
    Date end_date;
    boolean deleted;

    public String getUser() {
        return user;
    }

    public void setUser(String user_email) {
        this.user = user_email;
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
                "user_email='" + user + '\'' +
                ", isbn='" + isbn + '\'' +
                ", start_date=" + start_date +
                ", end_date=" + end_date +
                ", deleted=" + deleted +
                '}';
    }
}
