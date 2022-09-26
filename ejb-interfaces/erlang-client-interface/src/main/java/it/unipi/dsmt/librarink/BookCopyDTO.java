package it.unipi.dsmt.librarink;

import java.io.Serializable;

public class BookCopyDTO implements Serializable {
    String isbn;
    String id;

    public String getIsbn() {
        return isbn;
    }

    public void setIsbn(String isbn) {
        this.isbn = isbn;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    @Override
    public String toString(){
        return String.format("{\"isbn\":\"%s\",\"copy_id\":\"%s\"}", isbn, id);
    }
}
