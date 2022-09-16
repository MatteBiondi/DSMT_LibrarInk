package it.unipi.dsmt.librarink.entities;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.sql.Date;
@Entity
@Table(name = "history_loan")
public class History_loan {
    @Id
    @Column(name="user_email")
    String user_email;
    @Id
    @Column(name="isbn")
    String isbn;
    @Id
    @Column(name="id_copy")
    String id_copy;
    @Id
    @Column(name="start_date")
    Date start_date;
    @Column(name="end_date")
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
    public History_loanKey getHistory_loanKey()
    {
        return new History_loanKey(user_email,isbn,id_copy,start_date);
    }
}
