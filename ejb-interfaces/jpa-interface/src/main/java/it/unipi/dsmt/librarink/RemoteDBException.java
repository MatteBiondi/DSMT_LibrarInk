package it.unipi.dsmt.librarink;

import javax.ejb.ApplicationException;

@ApplicationException
public class RemoteDBException extends Exception{
    public RemoteDBException(String message){
        super(message);
    }
}
