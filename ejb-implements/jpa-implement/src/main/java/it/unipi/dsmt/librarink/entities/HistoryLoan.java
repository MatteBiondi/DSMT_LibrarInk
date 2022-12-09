package it.unipi.dsmt.librarink.entities;

import javax.persistence.*;
import java.sql.Timestamp;

@Entity
@Table(name = "history_loan")
@IdClass(HistoryLoanKey.class)
public class HistoryLoan {
    @Id
    @Column(name="user")
    String user;
    @Id
    @Column(name="isbn")
    String isbn;
    @Id
    @Column(name="id_copy")
    String id_copy;
    @Id
    @Column(name="start_date")
    Timestamp start_date;
    @Column(name="end_date")
    Timestamp end_date;

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

    public Timestamp getEnd_date() {
        return end_date;
    }

    public void setEnd_date(Timestamp end_date) {
        this.end_date = end_date;
    }

    public HistoryLoanKey getHistory_loanKey()
    {
        return new HistoryLoanKey(user,isbn,id_copy,start_date);
    }
}
