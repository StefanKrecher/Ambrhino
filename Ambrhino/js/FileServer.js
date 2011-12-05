smalltalk.addPackage('FileServer', {});
smalltalk.addClass('FileServer', smalltalk.Object, ['path', 'http', 'fs', 'url', 'port', 'basePath', 'sys'], 'FileServer');
smalltalk.addMethod(
unescape('_basePath'),
smalltalk.method({
selector: unescape('basePath'),
category: 'accessing',
fn: function () {
    var self = this;
    return ($receiver = self['@basePath']) == nil || $receiver == undefined ? function () {return unescape("./");}() : $receiver;
    return self;
},
args: [],
source: unescape('basePath%0A%09%5EbasePath%20ifNil%3A%20%5B%27./%27%5D'),
messageSends: ["ifNil:"],
referencedClasses: []
}),
smalltalk.FileServer);

smalltalk.addMethod(
unescape('_basePath_'),
smalltalk.method({
selector: unescape('basePath%3A'),
category: 'accessing',
fn: function (aString) {
    var self = this;
    self['@basePath'] = aString;
    return self;
},
args: ["aString"],
source: unescape('basePath%3A%20aString%0A%09basePath%20%3A%3D%20aString'),
messageSends: [],
referencedClasses: []
}),
smalltalk.FileServer);

smalltalk.addMethod(
unescape('_port'),
smalltalk.method({
selector: unescape('port'),
category: 'accessing',
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_class", []), "_port", []);
    return self;
},
args: [],
source: unescape('port%0A%09%5Eself%20class%20port'),
messageSends: ["port", "class"],
referencedClasses: []
}),
smalltalk.FileServer);

smalltalk.addMethod(
unescape('_initialize'),
smalltalk.method({
selector: unescape('initialize'),
category: 'initialization',
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.Object);
    self['@path'] = smalltalk.send(self, "_require_", ["path"]);
    self['@http'] = smalltalk.send(self, "_require_", ["http"]);
    self['@fs'] = smalltalk.send(self, "_require_", ["fs"]);
    self['@sys'] = smalltalk.send(self, "_require_", ["sys"]);
    self['@url'] = smalltalk.send(self, "_require_", ["url"]);
    return self;
},
args: [],
source: unescape('initialize%0A%09super%20initialize.%0A%09path%20%3A%3D%20self%20require%3A%20%27path%27.%0A%09http%20%3A%3D%20self%20require%3A%20%27http%27.%0A%09fs%20%3A%3D%20self%20require%3A%20%27fs%27.%0A%09sys%20%3A%3D%20self%20require%3A%20%27sys%27.%0A%09url%20%3A%3D%20self%20require%3A%20%27url%27'),
messageSends: ["initialize", "require:"],
referencedClasses: []
}),
smalltalk.FileServer);

smalltalk.addMethod(
unescape('_require_'),
smalltalk.method({
selector: unescape('require%3A'),
category: 'private',
fn: function (aModuleString) {
    var self = this;
    return smalltalk.send(typeof require == "undefined" ? nil : require, "_value_", [aModuleString]);
    return self;
},
args: ["aModuleString"],
source: unescape('require%3A%20aModuleString%0A%09%22call%20to%20the%20require%20function%22%0A%09%5Erequire%20value%3A%20aModuleString'),
messageSends: ["value:"],
referencedClasses: []
}),
smalltalk.FileServer);

smalltalk.addMethod(
unescape('_writeData_toFileNamed_'),
smalltalk.method({
selector: unescape('writeData%3AtoFileNamed%3A'),
category: 'private',
fn: function (data, aFilename) {
    var self = this;
    smalltalk.send(typeof console == "undefined" ? nil : console, "_log_", [aFilename]);
    return self;
},
args: ["data", "aFilename"],
source: unescape('writeData%3A%20data%20toFileNamed%3A%20aFilename%0A%09console%20log%3A%20aFilename'),
messageSends: ["log:"],
referencedClasses: []
}),
smalltalk.FileServer);

smalltalk.addMethod(
unescape('_respondNotFoundTo_'),
smalltalk.method({
selector: unescape('respondNotFoundTo%3A'),
category: 'request handling',
fn: function (aResponse) {
    var self = this;
    (function ($rec) {smalltalk.send($rec, "_writeHead_options_", [404, smalltalk.HashedCollection._fromPairs_([smalltalk.send(unescape("Content-Type"), "__minus_gt", [unescape("text/plain")])])]);smalltalk.send($rec, "_write_", ["404 Not found"]);return smalltalk.send($rec, "_end", []);}(aResponse));
    return self;
},
args: ["aResponse"],
source: unescape('respondNotFoundTo%3A%20aResponse%0A%09aResponse%20%0A%09%09writeHead%3A%20404%20options%3A%20%23%7B%27Content-Type%27%20-%3E%20%27text/plain%27%7D%3B%0A%09%09write%3A%20%27404%20Not%20found%27%3B%0A%09%09end'),
messageSends: ["writeHead:options:", unescape("-%3E"), "write:", "end"],
referencedClasses: []
}),
smalltalk.FileServer);

smalltalk.addMethod(
unescape('_handleRequest_respondTo_'),
smalltalk.method({
selector: unescape('handleRequest%3ArespondTo%3A'),
category: 'request handling',
fn: function (aRequest, aResponse) {
    var self = this;
    ($receiver = smalltalk.send(smalltalk.send(aRequest, "_method", []), "__eq", ["PUT"])).klass === smalltalk.Boolean ? $receiver ? function () {return smalltalk.send(self, "_handlePUTRequest_respondTo_", [aRequest, aResponse]);}() : function () {return smalltalk.send(self, "_handleGETRequest_respondTo_", [aRequest, aResponse]);}() : smalltalk.send($receiver, "_ifTrue_ifFalse_", [function () {return smalltalk.send(self, "_handlePUTRequest_respondTo_", [aRequest, aResponse]);}, function () {return smalltalk.send(self, "_handleGETRequest_respondTo_", [aRequest, aResponse]);}]);
    return self;
},
args: ["aRequest", "aResponse"],
source: unescape('handleRequest%3A%20aRequest%20respondTo%3A%20aResponse%0A%09aRequest%20method%20%3D%20%27PUT%27%0A%09%09ifTrue%3A%20%5Bself%20handlePUTRequest%3A%20aRequest%20respondTo%3A%20aResponse%5D%0A%09%09ifFalse%3A%20%5Bself%20handleGETRequest%3A%20aRequest%20respondTo%3A%20aResponse%5D'),
messageSends: ["ifTrue:ifFalse:", unescape("%3D"), "method", "handlePUTRequest:respondTo:", "handleGETRequest:respondTo:"],
referencedClasses: []
}),
smalltalk.FileServer);

smalltalk.addMethod(
unescape('_handleGETRequest_respondTo_'),
smalltalk.method({
selector: unescape('handleGETRequest%3ArespondTo%3A'),
category: 'request handling',
fn: function (aRequest, aResponse) {
    var self = this;
    var uri = nil;
    var filename = nil;
    uri = smalltalk.send(smalltalk.send(self['@url'], "_parse_", [smalltalk.send(aRequest, "_url", [])]), "_pathname", []);
    filename = smalltalk.send(self['@path'], "_join_with_", [smalltalk.send(self, "_basePath", []), uri]);
    smalltalk.send(self['@path'], "_exists_do_", [filename, function (boolean) {return ($receiver = boolean).klass === smalltalk.Boolean ? !$receiver ? function () {return smalltalk.send(self, "_respondNotFoundTo_", [aResponse]);}() : function () {return smalltalk.send(self, "_respondFileNamed_to_", [filename, aResponse]);}() : smalltalk.send($receiver, "_ifFalse_ifTrue_", [function () {return smalltalk.send(self, "_respondNotFoundTo_", [aResponse]);}, function () {return smalltalk.send(self, "_respondFileNamed_to_", [filename, aResponse]);}]);}]);
    return self;
},
args: ["aRequest", "aResponse"],
source: unescape('handleGETRequest%3A%20aRequest%20respondTo%3A%20aResponse%0A%09%7C%20uri%20filename%20%7C%0A%09uri%20%3A%3D%20%28url%20parse%3A%20aRequest%20url%29%20pathname.%0A%09filename%20%3A%3D%20path%20join%3A%20self%20basePath%20with%3A%20uri.%0A%09path%20exists%3A%20filename%20do%3A%20%5B%3Aboolean%20%7C%20%0A%09%09boolean%20%0A%09%09%09ifFalse%3A%20%5Bself%20respondNotFoundTo%3A%20aResponse%5D%0A%09%09%09ifTrue%3A%20%5Bself%20respondFileNamed%3A%20filename%20to%3A%20aResponse%5D%5D'),
messageSends: ["pathname", "parse:", "url", "join:with:", "basePath", "exists:do:", "ifFalse:ifTrue:", "respondNotFoundTo:", "respondFileNamed:to:"],
referencedClasses: []
}),
smalltalk.FileServer);

smalltalk.addMethod(
unescape('_handlePUTRequest_respondTo_'),
smalltalk.method({
selector: unescape('handlePUTRequest%3ArespondTo%3A'),
category: 'request handling',
fn: function (aRequest, aResponse) {
    var self = this;
    var stream = nil;
    stream = smalltalk.send(self['@fs'], "_createWriteStream_", [smalltalk.send(".", "__comma", [smalltalk.send(aRequest, "_url", [])])]);
    smalltalk.send(aRequest, "_setEncoding_", ["utf8"]);
    smalltalk.send(aRequest, "_on_do_", ["data", function (data) {return smalltalk.send(stream, "_write_", [data]);}]);
    smalltalk.send(aRequest, "_on_do_", ["end", function () {smalltalk.send(stream, "_end", []);return smalltalk.send(self, "_respondOKTo_", [aResponse]);}]);
    return self;
},
args: ["aRequest", "aResponse"],
source: unescape('handlePUTRequest%3A%20aRequest%20respondTo%3A%20aResponse%0A%09%7C%20stream%20%7C%0A%09stream%20%3A%3D%20fs%20createWriteStream%3A%20%27.%27%2C%20aRequest%20url.%0A%0A%09aRequest%20setEncoding%3A%20%27utf8%27.%0A%09aRequest%20on%3A%20%23data%20do%3A%20%5B%3Adata%20%7C%20stream%20write%3A%20data%5D.%0A%0A%09aRequest%20on%3A%20%23end%20do%3A%20%5B%0A%09%09stream%20end.%0A%09%09self%20respondOKTo%3A%20aResponse%5D'),
messageSends: ["createWriteStream:", unescape("%2C"), "url", "setEncoding:", "on:do:", "write:", "end", "respondOKTo:"],
referencedClasses: []
}),
smalltalk.FileServer);

smalltalk.addMethod(
unescape('_respondFileNamed_to_'),
smalltalk.method({
selector: unescape('respondFileNamed%3Ato%3A'),
category: 'request handling',
fn: function (aFilename, aResponse) {
    var self = this;
    var type = nil;
    var filename = nil;
    filename = aFilename;
    ($receiver = smalltalk.send(smalltalk.send(self['@fs'], "_statSync_", [aFilename]), "_isDirectory", [])).klass === smalltalk.Boolean ? $receiver ? function () {return filename = smalltalk.send(filename, "__comma", ["index.html"]);}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return filename = smalltalk.send(filename, "__comma", ["index.html"]);}]);
    smalltalk.send(self['@fs'], "_readFile_do_", [filename, function (ex, file) {return ($receiver = smalltalk.send(ex, "_notNil", [])).klass === smalltalk.Boolean ? $receiver ? function () {return smalltalk.send(self, "_respondInternalErrorTo_", [aResponse]);}() : function () {type = smalltalk.send(smalltalk.send(self, "_class", []), "_mimeTypeFor_", [filename]);return function ($rec) {smalltalk.send($rec, "_writeHead_options_", [200, smalltalk.HashedCollection._fromPairs_([smalltalk.send(unescape("Content-Type"), "__minus_gt", [type])])]);smalltalk.send($rec, "_write_binary_", [file, "binary"]);return smalltalk.send($rec, "_end", []);}(aResponse);}() : smalltalk.send($receiver, "_ifTrue_ifFalse_", [function () {return smalltalk.send(self, "_respondInternalErrorTo_", [aResponse]);}, function () {type = smalltalk.send(smalltalk.send(self, "_class", []), "_mimeTypeFor_", [filename]);return function ($rec) {smalltalk.send($rec, "_writeHead_options_", [200, smalltalk.HashedCollection._fromPairs_([smalltalk.send(unescape("Content-Type"), "__minus_gt", [type])])]);smalltalk.send($rec, "_write_binary_", [file, "binary"]);return smalltalk.send($rec, "_end", []);}(aResponse);}]);}]);
    return self;
},
args: ["aFilename", "aResponse"],
source: unescape('respondFileNamed%3A%20aFilename%20to%3A%20aResponse%0A%09%7C%20type%20filename%20%7C%0A%0A%09filename%20%3A%3D%20aFilename.%0A%09%28fs%20statSync%3A%20aFilename%29%20isDirectory%20ifTrue%3A%20%5B%0A%20%20%20%20%20%20%20%20%09filename%20%3A%3D%20filename%2C%20%27index.html%27%5D.%0A%0A%09fs%20readFile%3A%20filename%20do%3A%20%5B%3Aex%20%3Afile%20%7C%0A%09%09ex%20notNil%20%0A%09%09%09ifTrue%3A%20%5Bself%20respondInternalErrorTo%3A%20aResponse%5D%0A%09%09%09ifFalse%3A%20%5B%0A%20%20%20%20%20%20%20%20%09%09%09type%20%3A%3D%20self%20class%20mimeTypeFor%3A%20filename.%0A%09%09%09%09aResponse%20%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%09%09writeHead%3A%20200%20options%3A%20%20%23%7B%27Content-Type%27%20-%3E%20type%7D%3B%0A%09%09%09%09%09write%3A%20file%20binary%3A%20%27binary%27%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%09%09end%5D%5D'),
messageSends: ["ifTrue:", "isDirectory", "statSync:", unescape("%2C"), "readFile:do:", "ifTrue:ifFalse:", "notNil", "respondInternalErrorTo:", "mimeTypeFor:", "class", "writeHead:options:", unescape("-%3E"), "write:binary:", "end"],
referencedClasses: []
}),
smalltalk.FileServer);

smalltalk.addMethod(
unescape('_respondInternalErrorTo_'),
smalltalk.method({
selector: unescape('respondInternalErrorTo%3A'),
category: 'request handling',
fn: function (aResponse) {
    var self = this;
    (function ($rec) {smalltalk.send($rec, "_writeHead_options_", [500, smalltalk.HashedCollection._fromPairs_([smalltalk.send(unescape("Content-Type"), "__minus_gt", [unescape("text/plain")])])]);smalltalk.send($rec, "_write_", ["500 Internal server error"]);return smalltalk.send($rec, "_end", []);}(aResponse));
    return self;
},
args: ["aResponse"],
source: unescape('respondInternalErrorTo%3A%20aResponse%0A%09aResponse%20%0A%09%09writeHead%3A%20500%20options%3A%20%23%7B%27Content-Type%27%20-%3E%20%27text/plain%27%7D%3B%0A%09%09write%3A%20%27500%20Internal%20server%20error%27%3B%0A%09%09end'),
messageSends: ["writeHead:options:", unescape("-%3E"), "write:", "end"],
referencedClasses: []
}),
smalltalk.FileServer);

smalltalk.addMethod(
unescape('_respondOKTo_'),
smalltalk.method({
selector: unescape('respondOKTo%3A'),
category: 'request handling',
fn: function (aResponse) {
    var self = this;
    (function ($rec) {smalltalk.send($rec, "_writeHead_options_", [200, smalltalk.HashedCollection._fromPairs_([smalltalk.send(unescape("Content-Type"), "__minus_gt", [unescape("text/plain")])])]);return smalltalk.send($rec, "_end", []);}(aResponse));
    return self;
},
args: ["aResponse"],
source: unescape('respondOKTo%3A%20aResponse%0A%09aResponse%20%0A%09%09writeHead%3A%20200%20options%3A%20%23%7B%27Content-Type%27%20-%3E%20%27text/plain%27%7D%3B%0A%09%09end'),
messageSends: ["writeHead:options:", unescape("-%3E"), "end"],
referencedClasses: []
}),
smalltalk.FileServer);

smalltalk.addMethod(
unescape('_startOn_'),
smalltalk.method({
selector: unescape('startOn%3A'),
category: 'starting',
fn: function (aPort) {
    var self = this;
    self['@port'] = aPort;
    smalltalk.send(self, "_start", []);
    return self;
},
args: ["aPort"],
source: unescape('startOn%3A%20aPort%0A%09port%20%3A%3D%20aPort.%0A%09self%20start'),
messageSends: ["start"],
referencedClasses: []
}),
smalltalk.FileServer);

smalltalk.addMethod(
unescape('_start'),
smalltalk.method({
selector: unescape('start'),
category: 'starting',
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(self['@http'], "_createServer_", [function (request, response) {return smalltalk.send(self, "_handleRequest_respondTo_", [request, response]);}]), "_listen_", [smalltalk.send(self, "_port", [])]);
    smalltalk.send(typeof console == "undefined" ? nil : console, "_log_", [smalltalk.send("Starting file server on port ", "__comma", [smalltalk.send(smalltalk.send(self, "_port", []), "_asString", [])])]);
    return self;
},
args: [],
source: unescape('start%0A%09%28http%20createServer%3A%20%5B%3Arequest%20%3Aresponse%20%7C%0A%09%20%09self%20handleRequest%3A%20request%20respondTo%3A%20response%5D%29%20listen%3A%20self%20port.%0A%09console%20log%3A%20%27Starting%20file%20server%20on%20port%20%27%2C%20self%20port%20asString'),
messageSends: ["listen:", "createServer:", "handleRequest:respondTo:", "port", "log:", unescape("%2C"), "asString"],
referencedClasses: []
}),
smalltalk.FileServer);


smalltalk.FileServer.klass.iVarNames = ['port','mimeTypes'];
smalltalk.addMethod(
unescape('_port'),
smalltalk.method({
selector: unescape('port'),
category: 'accessing',
fn: function () {
    var self = this;
    return ($receiver = self['@port']) == nil || $receiver == undefined ? function () {return 4000;}() : $receiver;
    return self;
},
args: [],
source: unescape('port%0A%09%5Eport%20ifNil%3A%20%5B4000%5D'),
messageSends: ["ifNil:"],
referencedClasses: []
}),
smalltalk.FileServer.klass);

smalltalk.addMethod(
unescape('_port_'),
smalltalk.method({
sel