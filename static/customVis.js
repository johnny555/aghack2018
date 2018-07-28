var video = document.getElementById('video');



function cameraStart() {
    navigator.mediaDevices.getUserMedia({
    audio: false,
    video: true
}).then(function ( stream) {
    video.src = stream;
    video.play();
}).catch(function (err) {
    console.log("An error occurred! " + err);
});
                       };

var canvas = document.getElementById('canvas');

photo_settings = {};
photo_settings.takepicture = function (width, height) {

    var context = canvas.getContext('2d');
    context.drawImage(video, 0, 0, width, height);

    var data = canvas.toDataURL('image/png');
    return data;
};

window.addEventListener("load", cameraStart, false);
