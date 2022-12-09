package it.unipi.dsmt.librarink.entities;

import javax.persistence.*;
import java.sql.Timestamp;

@Entity
@Table(name = "history_reservation")
@IdClass(HistoryReservationKey.class)
public class HistoryReservation {
    @Id
    @Column(name="user")
    String user;
    @Id
    @Column(name="isbn")
    String isbn;

    @Id
    @Column(name="start_date")
    Timestamp start_date;
    @Column(name="end_date")
    Timestamp end_date;
    @Column(name="deleted")
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

    public boolean isDeleted() {
        return deleted;
    }

    public void setDeleted(boolean deleted) {
        this.deleted = deleted;
    }
    public HistoryReservationKey getHistory_reservationKey(){
        return new HistoryReservationKey(user,isbn, start_date);
    }
}
