const socket = new WebSocket('ws://' + location.hostname + ':5000/update');

socket.onopen = function (){
    console.log("Websocket opened");
    socket.send("Objs"); // TODO: send objects to track
}

socket.onmessage = function (event){
    console.log(event.data);
    // TODO: update interface
}
socket.onerror = function (event){
    console.log(event)
}
socket.onclose = function (){console.log("Websocket closed")}


// TODO: remove, test only
$("#ws_send").on('click', function (){
    socket.send($("#ws_msg").val());
})