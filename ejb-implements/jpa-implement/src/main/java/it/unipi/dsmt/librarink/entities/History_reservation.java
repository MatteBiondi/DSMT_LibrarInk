package it.unipi.dsmt.librarink.entities;

import javax.persistence.*;
import java.sql.Date;

@Entity
@Table(name = "history_reservation")
@IdClass(History_reservationKey.class)
public class History_reservation {
    @Id
    @Column(name="user_email")
    String user_email;
    @Id
    @Column(name="isbn")
    String isbn;

    @Id
    @Column(name="start_date")
    Date start_date;
    @Column(name="end_date")
    Date end_date;
    @Column(name="deleted")
    boolean deleted;

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
    public History_reservationKey getHistory_reservationKey(){
        return new History_reservationKey(user_email,isbn, start_date);
    }
}
