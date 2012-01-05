smalltalk.addPackage('AmberServer', {});
smalltalk.addClass('AmberClient', smalltalk.Object, [], 'AmberServer');

smalltalk.addMethod(
'_createProgram_main_filename_onSuccess_onError_',
smalltalk.method({
selector: 'createProgram:main:filename:onSuccess:onError:',
fn: function (categoriesOC, mainClassName, aTargetFilename, onSuccessBlock, onErrorBlock) {
    var self = this;
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_options_", [smalltalk.send(smalltalk.send(smalltalk.send(unescape("createProgram%3F"), "__comma", [mainClassName]), "__comma", [unescape("%3F")]), "__comma", [aTargetFilename]), smalltalk.HashedCollection._fromPairs_([smalltalk.send("type", "__minus_gt", ["POST"]), smalltalk.send("success", "__minus_gt", [onSuccessBlock]), smalltalk.send("data", "__minus_gt", [smalltalk.send(categoriesOC, "_asJSON", [])]), smalltalk.send("error", "__minus_gt", [onErrorBlock])])]);
    return self;
}
}),
smalltalk.AmberClient.klass);

smalltalk.addMethod(
'_createExample',
smalltalk.method({
selector: 'createExample',
fn: function () {
    var self = this;
    var oc = nil;
    oc = smalltalk.send(smalltalk.OrderedCollection || OrderedCollection, "_new", []);
    smalltalk.send(oc, "_add_", ["Examples"]);
    smalltalk.send(smalltalk.AmberClient || AmberClient, "_createProgram_main_filename_onSuccess_onError_", [oc, "JavaCall", "java", function () {return nil;}, function () {return nil;}]);
    return self;
}
}),
smalltalk.AmberClient.klass);


