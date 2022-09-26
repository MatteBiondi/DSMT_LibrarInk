package it.unipi.dsmt.librarink;

import java.io.Serializable;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class ReservationDTO implements Serializable {
    String user;
    String isbn;
    Date start_date;
    Date stop_date;
    Boolean cancelled;

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

    public Date getStartDate() {
        return start_date;
    }

    public void setStartDate(String startDate) {
        try {
            this.start_date =  new SimpleDateFormat("yyyy/MM/ddTHH:mm:ssZ").parse(startDate);
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }

    public Date getStopDate() {
        return stop_date;
    }

    public void setStopDate(String stopDate) {
        try {
            this.stop_date = new SimpleDateFormat("yyyy/MM/ddTHH:mm:ssZ").parse(stopDate);
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }

    public Boolean getCancelled() {
        return cancelled;
    }

    public void setCancelled(Boolean cancelled) {
        this.cancelled = cancelled;
    }

    @Override
    public String toString(){
        return String.format(
                "{\"user\":\"%s\",\"isbn\":\"%s\",\"start_date\":\"%s\",\"stop_date\":\"%s\",\"cancelled\":\"%s\"}",
                user, isbn, start_date, stop_date, cancelled
        );
    }
}
