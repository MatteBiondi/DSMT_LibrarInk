const socket = new WebSocket('ws://' + location.hostname + ':5000/update');
const intervalID = setInterval(function(){
    if (socket.readyState === 1) socket.send("keep-alive")
}, 90 * 1000);


socket.onopen = function (){
    console.log("Websocket opened");
    //socket.send("Objs"); // TODO: send objects to track
}

socket.onmessage = function (event){
    console.log(event.data);
    // TODO: update interface
}
socket.onerror = function (event){
   //alert("We are experiencing some troubles, the information may not be updated.\nWe apologize for the
    // inconvenience");
}
socket.onclose = function (){
    alert("We are experiencing some troubles, the information may not be updated.\nWe apologize for the inconvenience");
    console.log("Websocket closed")
}


// TODO: remove, test only
$("#ws_send").on('click', function (){
    socket.send($("#ws_msg").val());
})