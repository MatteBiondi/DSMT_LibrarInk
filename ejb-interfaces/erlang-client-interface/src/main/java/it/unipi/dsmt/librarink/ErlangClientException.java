package it.unipi.dsmt.librarink;

import javax.ejb.ApplicationException;

@ApplicationException
public class ErlangClientException extends Exception {
    public ErlangClientException(String message){
        super(message);
    }
}
