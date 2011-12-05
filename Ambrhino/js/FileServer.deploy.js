smalltalk.addPackage('FileServer', {});
smalltalk.addClass('FileServer', smalltalk.Object, ['path', 'http', 'fs', 'url', 'port', 'basePath', 'sys'], 'FileServer');
smalltalk.addMethod(
'_basePath',
smalltalk.method({
selector: 'basePath',
fn: function () {
    var self = this;
    return ($receiver = self['@basePath']) == nil || $receiver == undefined ? function () {return unescape("./");}() : $receiver;
    return self;
}
}),
smalltalk.FileServer);

smalltalk.addMethod(
'_basePath_',
smalltalk.method({
selector: 'basePath:',
fn: function (aString) {
    var self = this;
    self['@basePath'] = aString;
    return self;
}
}),
smalltalk.FileServer);

smalltalk.addMethod(
'_port',
smalltalk.method({
selector: 'port',
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_class", []), "_port", []);
    return self;
}
}),
smalltalk.FileServer);

smalltalk.addMethod(
'_initialize',
smalltalk.method({
selector: 'initialize',
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.Object);
    self['@path'] = smalltalk.send(self, "_require_", ["path"]);
    self['@http'] = smalltalk.send(self, "_require_", ["http"]);
    self['@fs'] = smalltalk.send(self, "_require_", ["fs"]);
    self['@sys'] = smalltalk.send(self, "_require_", ["sys"]);
    self['@url'] = smalltalk.send(self, "_require_", ["url"]);
    return self;
}
}),
smalltalk.FileServer);

smalltalk.addMethod(
'_require_',
smalltalk.method({
selector: 'require:',
fn: function (aModuleString) {
    var self = this;
    return smalltalk.send(typeof require == "undefined" ? nil : require, "_value_", [aModuleString]);
    return self;
}
}),
smalltalk.FileServer);

smalltalk.addMethod(
'_writeData_toFileNamed_',
smalltalk.method({
selector: 'writeData:toFileNamed:',
fn: function (data, aFilename) {
    var self = this;
    smalltalk.send(typeof console == "undefined" ? nil : console, "_log_", [aFilename]);
    return self;
}
}),
smalltalk.FileServer);

smalltalk.addMethod(
'_respondNotFoundTo_',
smalltalk.method({
selector: 'respondNotFoundTo:',
fn: function (aResponse) {
    var self = this;
    (function ($rec) {smalltalk.send($rec, "_writeHead_options_", [404, smalltalk.HashedCollection._fromPairs_([smalltalk.send(unescape("Content-Type"), "__minus_gt", [unescape("text/plain")])])]);smalltalk.send($rec, "_write_", ["404 Not found"]);return smalltalk.send($rec, "_end", []);}(aResponse));
    return self;
}
}),
smalltalk.FileServer);

smalltalk.addMethod(
'_handleRequest_respondTo_',
smalltalk.method({
selector: 'handleRequest:respondTo:',
fn: function (aRequest, aResponse) {
    var self = this;
    ($receiver = smalltalk.send(smalltalk.send(aRequest, "_method", []), "__eq", ["PUT"])).klass === smalltalk.Boolean ? $receiver ? function () {return smalltalk.send(self, "_handlePUTRequest_respondTo_", [aRequest, aResponse]);}() : function () {return smalltalk.send(self, "_handleGETRequest_respondTo_", [aRequest, aResponse]);}() : smalltalk.send($receiver, "_ifTrue_ifFalse_", [function () {return smalltalk.send(self, "_handlePUTRequest_respondTo_", [aRequest, aResponse]);}, function () {return smalltalk.send(self, "_handleGETRequest_respondTo_", [aRequest, aResponse]);}]);
    return self;
}
}),
smalltalk.FileServer);

smalltalk.addMethod(
'_handleGETRequest_respondTo_',
smalltalk.method({
selector: 'handleGETRequest:respondTo:',
fn: function (aRequest, aResponse) {
    var self = this;
    var uri = nil;
    var filename = nil;
    uri = smalltalk.send(smalltalk.send(self['@url'], "_parse_", [smalltalk.send(aRequest, "_url", [])]), "_pathname", []);
    filename = smalltalk.send(self['@path'], "_join_with_", [smalltalk.send(self, "_basePath", []), uri]);
    smalltalk.send(self['@path'], "_exists_do_", [filename, function (boolean) {return ($receiver = boolean).klass === smalltalk.Boolean ? !$receiver ? function () {return smalltalk.send(self, "_respondNotFoundTo_", [aResponse]);}() : function () {return smalltalk.send(self, "_respondFileNamed_to_", [filename, aResponse]);}() : smalltalk.send($receiver, "_ifFalse_ifTrue_", [function () {return smalltalk.send(self, "_respondNotFoundTo_", [aResponse]);}, function () {return smalltalk.send(self, "_respondFileNamed_to_", [filename, aResponse]);}]);}]);
    return self;
}
}),
smalltalk.FileServer);

smalltalk.addMethod(
'_handlePUTRequest_respondTo_',
smalltalk.method({
selector: 'handlePUTRequest:respondTo:',
fn: function (aRequest, aResponse) {
    var self = this;
    var stream = nil;
    stream = smalltalk.send(self['@fs'], "_createWriteStream_", [smalltalk.send(".", "__comma", [smalltalk.send(aRequest, "_url", [])])]);
    smalltalk.send(aRequest, "_setEncoding_", ["utf8"]);
    smalltalk.send(aRequest, "_on_do_", ["data", function (data) {return smalltalk.send(stream, "_write_", [data]);}]);
    smalltalk.send(aRequest, "_on_do_", ["end", function () {smalltalk.send(stream, "_end", []);return smalltalk.send(self, "_respondOKTo_", [aResponse]);}]);
    return self;
}
}),
smalltalk.FileServer);

smalltalk.addMethod(
'_respondFileNamed_to_',
smalltalk.method({
selector: 'respondFileNamed:to:',
fn: function (aFilename, aResponse) {
    var self = this;
    var type = nil;
    var filename = nil;
    filename = aFilename;
    ($receiver = smalltalk.send(smalltalk.send(self['@fs'], "_statSync_", [aFilename]), "_isDirectory", [])).klass === smalltalk.Boolean ? $receiver ? function () {return filename = smalltalk.send(filename, "__comma", ["index.html"]);}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return filename = smalltalk.send(filename, "__comma", ["index.html"]);}]);
    smalltalk.send(self['@fs'], "_readFile_do_", [filename, function (ex, file) {return ($receiver = smalltalk.send(ex, "_notNil", [])).klass === smalltalk.Boolean ? $receiver ? function () {return smalltalk.send(self, "_respondInternalErrorTo_", [aResponse]);}() : function () {type = smalltalk.send(smalltalk.send(self, "_class", []), "_mimeTypeFor_", [filename]);return function ($rec) {smalltalk.send($rec, "_writeHead_options_", [200, smalltalk.HashedCollection._fromPairs_([smalltalk.send(unescape("Content-Type"), "__minus_gt", [type])])]);smalltalk.send($rec, "_write_binary_", [file, "binary"]);return smalltalk.send($rec, "_end", []);}(aResponse);}() : smalltalk.send($receiver, "_ifTrue_ifFalse_", [function () {return smalltalk.send(self, "_respondInternalErrorTo_", [aResponse]);}, function () {type = smalltalk.send(smalltalk.send(self, "_class", []), "_mimeTypeFor_", [filename]);return function ($rec) {smalltalk.send($rec, "_writeHead_options_", [200, smalltalk.HashedCollection._fromPairs_([smalltalk.send(unescape("Content-Type"), "__minus_gt", [type])])]);smalltalk.send($rec, "_write_binary_", [file, "binary"]);return smalltalk.send($rec, "_end", []);}(aResponse);}]);}]);
    return self;
}
}),
smalltalk.FileServer);

smalltalk.addMethod(
'_respondInternalErrorTo_',
smalltalk.method({
selector: 'respondInternalErrorTo:',
fn: function (aResponse) {
    var self = this;
    (function ($rec) {smalltalk.send($rec, "_writeHead_options_", [500, smalltalk.HashedCollection._fromPairs_([smalltalk.send(unescape("Content-Type"), "__minus_gt", [unescape("text/plain")])])]);smalltalk.send($rec, "_write_", ["500 Internal server error"]);return smalltalk.send($rec, "_end", []);}(aResponse));
    return self;
}
}),
smalltalk.FileServer);

smalltalk.addMethod(
'_respondOKTo_',
smalltalk.method({
selector: 'respondOKTo:',
fn: function (aResponse) {
    var self = this;
    (function ($rec) {smalltalk.send($rec, "_writeHead_options_", [200, smalltalk.HashedCollection._fromPairs_([smalltalk.send(unescape("Content-Type"), "__minus_gt", [unescape("text/plain")])])]);return smalltalk.send($rec, "_end", []);}(aResponse));
    return self;
}
}),
smalltalk.FileServer);

smalltalk.addMethod(
'_startOn_',
smalltalk.method({
selector: 'startOn:',
fn: function (aPort) {
    var self = this;
    self['@port'] = aPort;
    smalltalk.send(self, "_start", []);
    return self;
}
}),
smalltalk.FileServer);

smalltalk.addMethod(
'_start',
smalltalk.method({
selector: 'start',
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(self['@http'], "_createServer_", [function (request, response) {return smalltalk.send(self, "_handleRequest_respondTo_", [request, response]);}]), "_listen_", [smalltalk.send(self, "_port", [])]);
    smalltalk.send(typeof console == "undefined" ? nil : console, "_log_", [smalltalk.send("Starting file server on port ", "__comma", [smalltalk.send(smalltalk.send(self, "_port", []), "_asString", [])])]);
    return self;
}
}),
smalltalk.FileServer);


smalltalk.FileServer.klass.iVarNames = ['port','mimeTypes'];
smalltalk.addMethod(
'_port',
smalltalk.method({
selector: 'port',
fn: function () {
    var self = this;
    return ($receiver = self['@port']) == nil || $receiver == undefined ? function () {return 4000;}() : $receiver;
    return self;
}
}),
smalltalk.FileServer.klass);

smalltalk.addMethod(
'_port_',
smalltalk.method({
selector: 'port:',
fn: function (aNumber) {
    var self = this;
    self['@port'] = aNumber;
    return self;
}
}),
smalltalk.FileServer.klass);

smalltalk.addMethod(
'_defaultMimeTypes',
smalltalk.method({
selector: 'defaultMimeTypes',
fn: function () {
    var self = this;
    return smalltalk.HashedCollection._fromPairs_([smalltalk.send(unescape("%25"), "__minus_gt", [unescape("application/x-trash")]), smalltalk.send("323", "__minus_gt", [unescape("text/h323")]), smalltalk.send("abw", "__minus_gt", [unescape("application/x-abiword")]), smalltalk.send("ai", "__minus_gt", [unescape("application/postscript")]), smalltalk.send("aif", "__minus_gt", [unescape("audio/x-aiff")]), smalltalk.send("aifc", "__minus_gt", [unescape("audio/x-aiff")]), smalltalk.send("aiff", "__minus_gt", [unescape("audio/x-aiff")]), smalltalk.send("alc", "__minus_gt", [unescape("chemical/x-alchemy")]), smalltalk.send("art", "__minus_gt", [unescape("image/x-jg")]), smalltalk.send("asc", "__minus_gt", [unescape("text/plain")]), smalltalk.send("asf", "__minus_gt", [unescape("video/x-ms-asf")]), smalltalk.send("asn", "__minus_gt", [unescape("chemical/x-ncbi-asn1-spec")]), smalltalk.send("aso", "__minus_gt", [unescape("chemical/x-ncbi-asn1-binary")]), smalltalk.send("asx", "__minus_gt", [unescape("video/x-ms-asf")]), smalltalk.send("au", "__minus_gt", [unescape("audio/basic")]), smalltalk.send("avi", "__minus_gt", [unescape("video/x-msvideo")]), smalltalk.send("b", "__minus_gt", [unescape("chemical/x-molconn-Z")]), smalltalk.send("bak", "__minus_gt", [unescape("application/x-trash")]), smalltalk.send("bat", "__minus_gt", [unescape("application/x-msdos-program")]), smalltalk.send("bcpio", "__minus_gt", [unescape("application/x-bcpio")]), smalltalk.send("bib", "__minus_gt", [unescape("text/x-bibtex")]), smalltalk.send("bin", "__minus_gt", [unescape("application/octet-stream")]), smalltalk.send("bmp", "__minus_gt", [unescape("image/x-ms-bmp")]), smalltalk.send("book", "__minus_gt", [unescape("application/x-maker")]), smalltalk.send("bsd", "__minus_gt", [unescape("chemical/x-crossfire")]), smalltalk.send("c", "__minus_gt", [unescape("text/x-csrc")]), smalltalk.send(unescape("c++"), "__minus_gt", [unescape("text/x-c++src")]), smalltalk.send("c3d", "__minus_gt", [unescape("chemical/x-chem3d")]), smalltalk.send("cac", "__minus_gt", [unescape("chemical/x-cache")]), smalltalk.send("cache", "__minus_gt", [unescape("chemical/x-cache")]), smalltalk.send("cascii", "__minus_gt", [unescape("chemical/x-cactvs-binary")]), smalltalk.send("cat", "__minus_gt", [unescape("application/vnd.ms-pki.seccat")]), smalltalk.send("cbin", "__minus_gt", [unescape("chemical/x-cactvs-binary")]), smalltalk.send("cc", "__minus_gt", [unescape("text/x-c++src")]), smalltalk.send("cdf", "__minus_gt", [unescape("application/x-cdf")]), smalltalk.send("cdr", "__minus_gt", [unescape("image/x-coreldraw")]), smalltalk.send("cdt", "__minus_gt", [unescape("image/x-coreldrawtemplate")]), smalltalk.send("cdx", "__minus_gt", [unescape("chemical/x-cdx")]), smalltalk.send("cdy", "__minus_gt", [unescape("application/vnd.cinderella")]), smalltalk.send("cef", "__minus_gt", [unescape("chemical/x-cxf")]), smalltalk.send("cer", "__minus_gt", [unescape("chemical/x-cerius")]), smalltalk.send("chm", "__minus_gt", [unescape("chemical/x-chemdraw")]), smalltalk.send("chrt", "__minus_gt", [unescape("application/x-kchart")]), smalltalk.send("cif", "__minus_gt", [unescape("chemical/x-cif")]), smalltalk.send("class", "__minus_gt", [unescape("application/java-vm")]), smalltalk.send("cls", "__minus_gt", [unescape("text/x-tex")]), smalltalk.send("cmdf", "__minus_gt", [unescape("chemical/x-cmdf")]), smalltalk.send("cml", "__minus_gt", [unescape("chemical/x-cml")]), smalltalk.send("cod", "__minus_gt", [unescape("application/vnd.rim.cod")]), smalltalk.send("com", "__minus_gt", [unescape("application/x-msdos-program")]), smalltalk.send("cpa", "__minus_gt", [unescape("chemical/x-compass")]), smalltalk.send("cpio", "__minus_gt", [unescape("application/x-cpio")]), smalltalk.send("cpp", "__minus_gt", [unescape("text/x-c++src")]), smalltalk.send("cpt", "__minus_gt", [unescape("image/x-corelphotopaint")]), smalltalk.send("crl", "__minus_gt", [unescape("application/x-pkcs7-crl")]), smalltalk.send("crt", "__minus_gt", [unescape("application/x-x509-ca-cert")]), smalltalk.send("csf", "__minus_gt", [unescape("chemical/x-cache-csf")]), smalltalk.send("csh", "__minus_gt", [unescape("text/x-csh")]), smalltalk.send("csm", "__minus_gt", [unescape("chemical/x-csml")]), smalltalk.send("csml", "__minus_gt", [unescape("chemical/x-csml")]), smalltalk.send("css", "__minus_gt", [unescape("text/css")]), smalltalk.send("csv", "__minus_gt", [unescape("text/comma-separated-values")]), smalltalk.send("ctab", "__minus_gt", [unescape("chemical/x-cactvs-binary")]), smalltalk.send("ctx", "__minus_gt", [unescape("chemical/x-ctx")]), smalltalk.send("cu", "__minus_gt", [unescape("application/cu-seeme")]), smalltalk.send("cub", "__minus_gt", [unescape("chemical/x-gaussian-cube")]), smalltalk.send("cxf", "__minus_gt", [unescape("chemical/x-cxf")]), smalltalk.send("cxx", "__minus_gt", [unescape("text/x-c++src")]), smalltalk.send("dat", "__minus_gt", [unescape("chemical/x-mopac-input")]), smalltalk.send("dcr", "__minus_gt", [unescape("application/x-director")]), smalltalk.send("deb", "__minus_gt", [unescape("application/x-debian-package")]), smalltalk.send("dif", "__minus_gt", [unescape("video/dv")]), smalltalk.send("diff", "__minus_gt", [unescape("text/plain")]), smalltalk.send("dir", "__minus_gt", [unescape("application/x-director")]), smalltalk.send("djv", "__minus_gt", [unescape("image/vnd.djvu")]), smalltalk.send("djvu", "__minus_gt", [unescape("image/vnd.djvu")]), smalltalk.send("dl", "__minus_gt", [unescape("video/dl")]), smalltalk.send("dll", "__minus_gt", [unescape("application/x-msdos-program")]), smalltalk.send("dmg", "__minus_gt", [unescape("application/x-apple-diskimage")]), smalltalk.send("dms", "__minus_gt", [unescape("application/x-dms")]), smalltalk.send("doc", "__minus_gt", [unescape("application/msword")]), smalltalk.send("dot", "__minus_gt", [unescape("application/msword")]), smalltalk.send("dv", "__minus_gt", [unescape("video/dv")]), smalltalk.send("dvi", "__minus_gt", [unescape("application/x-dvi")]), smalltalk.send("dx", "__minus_gt", [unescape("chemical/x-jcamp-dx")]), smalltalk.send("dxr", "__minus_gt", [unescape("application/x-director")]), smalltalk.send("emb", "__minus_gt", [unescape("chemical/x-embl-dl-nucleotide")]), smalltalk.send("embl", "__minus_gt", [unescape("chemical/x-embl-dl-nucleotide")]), smalltalk.send("ent", "__minus_gt", [unescape("chemical/x-pdb")]), smalltalk.send("ep