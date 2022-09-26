package it.unipi.dsmt;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.reflect.TypeToken;
import it.unipi.dsmt.librarink.ReservationDTO;
import org.junit.Test;
import java.lang.reflect.Type;
import java.util.List;
import java.util.Map;

public class ErlangClientTest {

    public void test() {
        String json = "{\"response\":{\"counter\":1,\"values\":[{\"cancelled\":false,\"isbn\":\"AAA\"," +
                "\"start_date\":\"2022-09-26T13:42:35Z\",\"stop_date\":null,\"user\":\"federico\"}]}," +
                "\"result\":\"succeed\"}";

        JsonObject result = new Gson().fromJson(json, JsonObject.class);

        if (result.get("result").getAsString().equals("succeed")){
            System.out.println(result.get("response").toString());
            JsonObject response = new Gson().fromJson(result.get("response").toString(), JsonObject.class);
            Type collectionType = new TypeToken<List<ReservationDTO>>(){}.getType();
            List list = new Gson().fromJson(response.get("values").toString(), collectionType);
            System.out.println(list);
        }
        else {
            System.out.println("null");
        }
    }
}
