package it.unipi.dsmt.librarink;

import java.io.Serializable;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class LoanDTO extends DTO implements Serializable {
    String user; 
    String isbn; 
    String id;
    Date start_date;
    Date stop_date;

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
        return id;
    }

    public void setCopyId(String copyId) {
        this.id = copyId;
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

    @Override
    public String toString(){
        return String.format(
                "{\"user\":\"%s\",\"isbn\":\"%s\",\"id\":\"%s\",\"start_date\":\"%s\",\"stop_date\":\"%s\"}",
                user, isbn, id, start_date, start_date
        );
    }
}
