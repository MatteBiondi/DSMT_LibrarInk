package it.unipi.dsmt.librarink;

import java.io.Serializable;
import java.sql.Timestamp;

public class HistoryLoanDTO implements Serializable {
    String user;
    String isbn;
    String copyId;
    Timestamp startDate;
    Timestamp endDate;

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

    public String getCopyId() {
        return copyId;
    }

    public void setCopyId(String copyId) {
        this.copyId = copyId;
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

    @Override
    public String toString() {
        return "HistoryLoanDTO{" +
                "user='" + user + '\'' +
                ", isbn='" + isbn + '\'' +
                ", id_copy='" + copyId + '\'' +
                ", start_date=" + startDate +
                ", end_date=" + endDate +
                '}';
    }
}
