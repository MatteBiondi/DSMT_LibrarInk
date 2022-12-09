package it.unipi.dsmt.librarink;

import java.io.Serializable;
import java.sql.Timestamp;

public class HistoryReservationDTO implements Serializable {
    String user;
    String isbn;
    Timestamp startDate;
    Timestamp endDate;
    boolean deleted;

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

    public Timestamp getStartDate() {
        return startDate;
    }

    public void setStartDate(Timestamp startDate) {
        this.startDate = startDate;
    }

    public Timestamp getEndDate() {
        return endDate;
    }

    public void setEndDate(Timestamp endDate) {
        this.endDate = endDate;
    }

    public boolean isDeleted() {
        return deleted;
    }

    public void setDeleted(boolean deleted) {
        this.deleted = deleted;
    }

    @Override
    public String toString() {
        return "HistoryReservationDTO{" +
                "user_email='" + user + '\'' +
                ", isbn='" + isbn + '\'' +
                ", start_date=" + startDate +
                ", end_date=" + endDate +
                ", deleted=" + deleted +
                '}';
    }
}
