var video = document.getElementById('video');
navigator.mediaDevices.getUserMedia({
    audio: false,
    video: true
}).then(function ( stream) {
    video.src = stream;
    video.play();
});;
