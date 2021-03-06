var video = document.getElementById('video');
var output = document.getElementById('output');
//var photo = document.getElementById('photo');



function cameraStart() {
    console.log("starting camera...");
    navigator.mediaDevices.getUserMedia({

    audio: false,
        video: {
            facingMode : {ideal: 'environment'}
        }
}).then(function ( stream) {
    video.srcObject = stream;
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
    var img = canvas.toDataURL('image/png');
    var data = atob(img.split(',')[1]);
    var ia = new Uint8Array(data.length);
    for (var i = 0; i < data.length; i++) {
        ia[i] = data.charCodeAt(i);
    }
    blobData = new Blob([ia], {type: "mimeString"});

    canvas.getContext('2d').drawImage(video, 0,0,width,height);
    //canvas.width = width;
    //canvas.height = height;
    photo_settings.img = blobData;
    photo_settings.data = data;
    photo_settings.ia = ia;
    return blobData;
};

var albumBucketName = 'dex-hackathon-bucket-2';
var bucketRegion = 'us-east-1';
var IdentityPoolId = 'us-east-1:71e2f4ab-2db9-4039-abd5-35c66e86937c';


uploader = {};
uploader.init = function () {

    AWS.config.update({
        region: bucketRegion,
        credentials: new AWS.CognitoIdentityCredentials({
            IdentityPoolId: IdentityPoolId
        })
    });

    var s3 = new AWS.S3({
        apiVersion: '2006-03-01',
        params: {Bucket: albumBucketName}
    });

    uploader.s3 = s3;

};

uploader.upload = function () {
    var s3 = uploader.s3;
    output.innerText = "Uploading ...";
    s3.upload({
        Key:  'testfile.png',
        Body: photo_settings.img,
        ACL : 'public-read'
    }, function (err, data) {
        if (err) {
            return alert("There was an error uploading");
        }
        output.innerText = "Successfully uploaded. Getting result.";
        resulter.getResult();
    });
};



global_result = {};

function parseResult (e) {
    var res = JSON.parse(e);
    global_result = res;

    const max = res.predictions.reduce(function(prev, current) {
        return (prev.prob > current.prob) ? prev : current
    });

    var name = max.label;
    var prob = max.prob;

    //return JSON.stringify(max, null, 4);
    return [name, prob];
};




resulter = {};
resulter.listener = (e) => {
    var res = parseResult(e);
    output.innerText = res[0] + " " + res[1];

};
resulter.getResult = function () {
    var oReq = new XMLHttpRequest();
    var params = {image_url: "https://s3.amazonaws.com/dex-hackathon-bucket-2/testfile.png",
                  top_k: "3"};
    var queryString ="https://nwo22ki9il.execute-api.us-east-1.amazonaws.com/dev/v0.0.1/predict" +
        "?" + Object.keys(params).map((key) => {return key + "=" + encodeURIComponent(params[key])}).join("&");
    oReq.open("GET",
              queryString, true);

    oReq.onreadystatechange = function() {
        if (oReq.readyState == XMLHttpRequest.DONE) {
            resulter.listener(oReq.responseText);
        }
    }
    oReq.setRequestHeader("x-api-key", "rRF4H5rD0t2XdbAANZGDY6WUj2VfN1uU2iIEC2ki");

    oReq.send();

};



// cameraStart();
