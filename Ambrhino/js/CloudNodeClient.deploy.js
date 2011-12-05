smalltalk.addPackage('CloudNodeClient', {"version":"2.23"});
smalltalk.addClass('UserContainer', smalltalk.Object, ['username', 'password'], 'CloudNodeClient');
smalltalk.addMethod(
'_createAccount',
smalltalk.method({
selector: 'createAccount',
fn: function () {
    var self = this;
    var dict = nil;
    dict = smalltalk.send(smalltalk.Dictionary || Dictionary, "_new", []);
    smalltalk.send(dict, "_at_put_", ["_id", smalltalk.send(smalltalk.send(unescape("%23createUsername"), "_asJQuery", []), "_val", [])]);
    smalltalk.send(dict, "_at_put_", ["email", smalltalk.send(smalltalk.send(unescape("%23createEmail"), "_asJQuery", []), "_val", [])]);
    smalltalk.send(dict, "_at_put_", ["password", smalltalk.send(smalltalk.send(unescape("%23createPassword"), "_asJQuery", []), "_val", [])]);
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_options_", [unescape("FileServer%3FcreateAccount%3A"), smalltalk.Dictionary._fromPairs_([smalltalk.send("type", "__minus_gt", ["POST"]), smalltalk.send("success", "__minus_gt", [function (message) {return smalltalk.send(typeof window == "undefined" ? nil : window, "_alert_", [message]);}]), smalltalk.send("error", "__minus_gt", [function (ex) {return smalltalk.send(typeof window == "undefined" ? nil : window, "_alert_", [smalltalk.send("error: ", "__comma", [smalltalk.send(ex, "_responseText", [])])]);}]), smalltalk.send("data", "__minus_gt", [smalltalk.send(dict, "_asJSON", [])])])]);
    return self;
}
}),
smalltalk.UserContainer);

smalltalk.addMethod(
'_showLogin',
smalltalk.method({
selector: 'showLogin',
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(unescape("%23createAccount"), "_asJQuery", []), "_hide", []);
    smalltalk.send(smalltalk.send(unescape("%23login"), "_asJQuery", []), "_show", []);
    smalltalk.send(smalltalk.send(unescape("%23browse"), "_asJQuery", []), "_hide", []);
    return self;
}
}),
smalltalk.UserContainer);

smalltalk.addMethod(
'_showCreateAccount',
smalltalk.method({
selector: 'showCreateAccount',
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(unescape("%23createAccount"), "_asJQuery", []), "_show", []);
    smalltalk.send(smalltalk.send(unescape("%23login"), "_asJQuery", []), "_hide", []);
    smalltalk.send(smalltalk.send(unescape("%23browse"), "_asJQuery", []), "_hide", []);
    return self;
}
}),
smalltalk.UserContainer);

smalltalk.addMethod(
'_showBrowse',
smalltalk.method({
selector: 'showBrowse',
fn: function () {
    var self = this;
    var canvas = nil;
    var tb = nil;
    var div = nil;
    smalltalk.send(smalltalk.send(unescape("%23createAccount"), "_asJQuery", []), "_hide", []);
    smalltalk.send(smalltalk.send(unescape("%23login"), "_asJQuery", []), "_hide", []);
    smalltalk.send(smalltalk.send(unescape("%23browse"), "_asJQuery", []), "_show", []);
    div = smalltalk.send(unescape("%23packages"), "_asJQuery", []);
    smalltalk.send(div, "_empty", []);
    smalltalk.send(smalltalk.send(smalltalk.PackageHandler || PackageHandler, "_new", []), "_browsePublic_onError_", [function (message) {return smalltalk.send(smalltalk.send(smalltalk.JSON || JSON, "_parse_", [message]), "_do_", [function (entry) {var url = nil;var a = nil;var name = nil;var version = nil;name = smalltalk.send(entry, "_key", []);version = smalltalk.send(entry, "_value", []);url = smalltalk.send(smalltalk.send(smalltalk.send(unescape("http%3A//localhost%3A8080/packages/"), "__comma", [name]), "__comma", [unescape(".st%3Fversion%3D")]), "__comma", [version]);(function ($rec) {smalltalk.send($rec, "_show_", [url]);return smalltalk.send($rec, "_cr", []);}(smalltalk.Transcript || Transcript));a = smalltalk.send(smalltalk.send(smalltalk.send(smalltalk.HTMLCanvas || HTMLCanvas, "_new", []), "_a", []), "_href_", [url]);smalltalk.send(a, "_append_", [smalltalk.send(smalltalk.send(smalltalk.send("Package: ", "__comma", [name]), "__comma", [" Version: "]), "__comma", [version])]);smalltalk.send(div, "_append_", [smalltalk.send(a, "_asJQuery", [])]);return smalltalk.send(div, "_append_", [smalltalk.send(smalltalk.send(smalltalk.send(smalltalk.HTMLCanvas || HTMLCanvas, "_new", []), "_br", []), "_asJQuery", [])]);}]);}, function (ex) {return smalltalk.send(typeof window == "undefined" ? nil : window, "_alert_", [smalltalk.send(ex, "_responseText", [])]);}]);
    return self;
}
}),
smalltalk.UserContainer);

smalltalk.addMethod(
'_login',
smalltalk.method({
selector: 'login',
fn: function () {
    var self = this;
    var dict = nil;
    dict = smalltalk.send(smalltalk.Dictionary || Dictionary, "_new", []);
    smalltalk.send(dict, "_at_put_", ["_id", smalltalk.send(smalltalk.send(unescape("%23loginUsername"), "_asJQuery", []), "_val", [])]);
    smalltalk.send(dict, "_at_put_", ["password", smalltalk.send(smalltalk.send(unescape("%23loginPassword"), "_asJQuery", []), "_val", [])]);
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_options_", [unescape("FileServer%3Flogin%3A"), smalltalk.HashedCollection._fromPairs_([smalltalk.send("type", "__minus_gt", ["POST"]), smalltalk.send("success", "__minus_gt", [function (message) {smalltalk.send(typeof window == "undefined" ? nil : window, "_alert_", [message]);smalltalk.send(self, "_username_", [smalltalk.send(dict, "_at_", ["_id"])]);return smalltalk.send(self, "_password_", [smalltalk.send(dict, "_at_", ["password"])]);}]), smalltalk.send("error", "__minus_gt", [function (ex) {return smalltalk.send(typeof window == "undefined" ? nil : window, "_alert_", [smalltalk.send(ex, "_responseText", [])]);}]), smalltalk.send("data", "__minus_gt", [smalltalk.send(dict, "_asJSON", [])])])]);
    return self;
}
}),
smalltalk.UserContainer);

smalltalk.addMethod(
'_username',
smalltalk.method({
selector: 'username',
fn: function () {
    var self = this;
    return self['@username'];
    return self;
}
}),
smalltalk.UserContainer);

smalltalk.addMethod(
'_username_',
smalltalk.method({
selector: 'username:',
fn: function (aUsername) {
    var self = this;
    self['@username'] = aUsername;
    return self;
}
}),
smalltalk.UserContainer);

smalltalk.addMethod(
'_password',
smalltalk.method({
selector: 'password',
fn: function () {
    var self = this;
    return self['@password'];
    return self;
}
}),
smalltalk.UserContainer);

smalltalk.addMethod(
'_password_',
smalltalk.method({
selector: 'password:',
fn: function (aPassword) {
    var self = this;
    self['@password'] = aPassword;
    return self;
}
}),
smalltalk.UserContainer);



smalltalk.addClass('PackageHandler', smalltalk.Object, [], 'CloudNodeClient');
smalltalk.addMethod(
'_browsePublic_onError_',
smalltalk.method({
selector: 'browsePublic:onError:',
fn: function (onSuccessBlock, onErrorBlock) {
    var self = this;
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_options_", [unescape("FileServer%3Fbrowse%3A"), smalltalk.Dictionary._fromPairs_([smalltalk.send("type", "__minus_gt", ["POST"]), smalltalk.send("success", "__minus_gt", [onSuccessBlock]), smalltalk.send("data", "__minus_gt", ["bla"]), smalltalk.send("error", "__minus_gt", [onErrorBlock])])]);
    return self;
}
}),
smalltalk.PackageHandler);

smalltalk.addMethod(
'_commit_user_password_',
smalltalk.method({
selector: 'commit:user:password:',
fn: function (aPackage, aUser, aPassword) {
    var self = this;
    try {
        var dict = nil;
        dict = smalltalk.send(smalltalk.Dictionary || Dictionary, "_new", []);
        ($receiver = smalltalk.send(smalltalk.send(smalltalk.send(smalltalk.Smalltalk || Smalltalk, "_current", []), "_packageAt_", [aPackage]), "_isNil", [])).klass === smalltalk.Boolean ? $receiver ? function () {return function () {throw {name: "stReturn", selector: "_commit_user_password_", fn: function () {return smalltalk.send(typeof window == "undefined" ? nil : window, "_alert_", ["Package not found"]);}};}();}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return function () {throw {name: "stReturn", selector: "_commit_user_password_", fn: function () {return smalltalk.send(typeof window == "undefined" ? nil : window, "_alert_", ["Package not found"]);}};}();}]);
        smalltalk.send(dict, "_at_put_", ["packageName", aPackage]);
        smalltalk.send(dict, "_at_put_", ["packageMeta", smalltalk.send(smalltalk.send(smalltalk.send(smalltalk.Smalltalk || Smalltalk, "_current", []), "_packageAt_", [aPackage]), "_propertiesAsJSON", [])]);
        smalltalk.send(dict, "_at_put_", ["jsdeploy", smalltalk.send(smalltalk.send(smalltalk.StrippedExporter || StrippedExporter, "_new", []), "_exportPackage_", [aPackage])]);
        smalltalk.send(dict, "_at_put_", ["st", smalltalk.send(smalltalk.send(smalltalk.ChunkExporter || ChunkExporter, "_new", []), "_exportPackage_", [aPackage])]);
        smalltalk.send(dict, "_at_put_", ["user", aUser]);
        smalltalk.send(dict, "_at_put_", ["password", aPassword]);
        smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_options_", [unescape("FileServer%3FcommitPackage%3A"), smalltalk.Dictionary._fromPairs_([smalltalk.send("type", "__minus_gt", ["POST"]), smalltalk.send("success", "__minus_gt", [function (message) {return smalltalk.send(typeof window == "undefined" ? nil : window, "_alert_", [message]);}]), smalltalk.send("data", "__minus_gt", [smalltalk.send(dict, "_asJSON", [])]), smalltalk.send("error", "__minus_gt", [function (ex) {return smalltalk.send(typeof window == "undefined" ? nil : window, "_alert_", [smalltalk.send(ex, "_responseText", [])]);}])])]);
        return self;
    } catch (e) {
        if (e.name === "stReturn" &&
            e.selector === "_commit_user_password_") {
            return e.fn();
        }
        throw e;
    }
}
}),
smalltalk.PackageHandler);



