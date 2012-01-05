smalltalk.addPackage('AmberServer', {});
smalltalk.addClass('AmberClient', smalltalk.Object, [], 'AmberServer');

smalltalk.addMethod(
unescape('_createProgram_main_filename_onSuccess_onError_'),
smalltalk.method({
selector: unescape('createProgram%3Amain%3Afilename%3AonSuccess%3AonError%3A'),
category: 'not yet classified',
fn: function (categoriesOC, mainClassName, aTargetFilename, onSuccessBlock, onErrorBlock) {
    var self = this;
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_options_", [smalltalk.send(smalltalk.send(smalltalk.send(unescape("createProgram%3F"), "__comma", [mainClassName]), "__comma", [unescape("%3F")]), "__comma", [aTargetFilename]), smalltalk.HashedCollection._fromPairs_([smalltalk.send("type", "__minus_gt", ["POST"]), smalltalk.send("success", "__minus_gt", [onSuccessBlock]), smalltalk.send("data", "__minus_gt", [smalltalk.send(categoriesOC, "_asJSON", [])]), smalltalk.send("error", "__minus_gt", [onErrorBlock])])]);
    return self;
},
args: ["categoriesOC", "mainClassName", "aTargetFilename", "onSuccessBlock", "onErrorBlock"],
source: unescape('createProgram%3A%20categoriesOC%20main%3A%20mainClassName%20filename%3A%20aTargetFilename%20onSuccess%3A%20onSuccessBlock%20onError%3A%20onErrorBlock%0A%0AjQuery%20%0A%09%09%09ajax%3A%20%28%27createProgram%3F%27%2CmainClassName%2C%20%27%3F%27%2C%20aTargetFilename%29%0A%09%09%09options%3A%20%23%7B%0A%09%09%09%09%27type%27%20-%3E%20%27POST%27.%0A%09%09%09%09%27success%27%20-%3E%20onSuccessBlock.%0A%09%09%09%09%27data%27%20-%3E%20categoriesOC%20asJSON.%0A%09%09%09%09%27error%27%20-%3E%20onErrorBlock%0A%09%09%09%7D.'),
messageSends: ["ajax:options:", unescape("%2C"), unescape("-%3E"), "asJSON"],
referencedClasses: []
}),
smalltalk.AmberClient.klass);

smalltalk.addMethod(
unescape('_createExample'),
smalltalk.method({
selector: unescape('createExample'),
category: 'not yet classified',
fn: function () {
    var self = this;
    var oc = nil;
    oc = smalltalk.send(smalltalk.OrderedCollection || OrderedCollection, "_new", []);
    smalltalk.send(oc, "_add_", ["Examples"]);
    smalltalk.send(smalltalk.AmberClient || AmberClient, "_createProgram_main_filename_onSuccess_onError_", [oc, "JavaCall", "java", function () {return nil;}, function () {return nil;}]);
    return self;
},
args: [],
source: unescape('createExample%0A%7C%20oc%20%7C%0Aoc%20%3A%3D%20OrderedCollection%20new.%0Aoc%20add%3A%20%27Examples%27.%0A%0AAmberClient%20createProgram%3A%20oc%20main%3A%20%27JavaCall%27%20filename%3A%20%27java%27%20onSuccess%3A%20%5B%5D%20onError%3A%20%5B%5D'),
messageSends: ["new", "add:", "createProgram:main:filename:onSuccess:onError:"],
referencedClasses: ["OrderedCollection", "AmberClient"]
}),
smalltalk.AmberClient.klass);


