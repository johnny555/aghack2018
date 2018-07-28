globalMedia = navigator.mediaDevices.getUserMedia({
    audio: false,
    video: true
});
var video = document.getElementById('video');

globalMedia.then(function ( stream) {
    video.src = stream.srcObject;
    video.play();
});
