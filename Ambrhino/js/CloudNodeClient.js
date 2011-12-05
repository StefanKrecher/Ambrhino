smalltalk.addPackage('CloudNodeClient', {"version":"2.23"});
smalltalk.addClass('UserContainer', smalltalk.Object, ['username', 'password'], 'CloudNodeClient');
smalltalk.addMethod(
unescape('_createAccount'),
smalltalk.method({
selector: unescape('createAccount'),
category: 'not yet classified',
fn: function () {
    var self = this;
    var dict = nil;
    dict = smalltalk.send(smalltalk.Dictionary || Dictionary, "_new", []);
    smalltalk.send(dict, "_at_put_", ["_id", smalltalk.send(smalltalk.send(unescape("%23createUsername"), "_asJQuery", []), "_val", [])]);
    smalltalk.send(dict, "_at_put_", ["email", smalltalk.send(smalltalk.send(unescape("%23createEmail"), "_asJQuery", []), "_val", [])]);
    smalltalk.send(dict, "_at_put_", ["password", smalltalk.send(smalltalk.send(unescape("%23createPassword"), "_asJQuery", []), "_val", [])]);
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_options_", [unescape("FileServer%3FcreateAccount%3A"), smalltalk.Dictionary._fromPairs_([smalltalk.send("type", "__minus_gt", ["POST"]), smalltalk.send("success", "__minus_gt", [function (message) {return smalltalk.send(typeof window == "undefined" ? nil : window, "_alert_", [message]);}]), smalltalk.send("error", "__minus_gt", [function (ex) {return smalltalk.send(typeof window == "undefined" ? nil : window, "_alert_", [smalltalk.send("error: ", "__comma", [smalltalk.send(ex, "_responseText", [])])]);}]), smalltalk.send("data", "__minus_gt", [smalltalk.send(dict, "_asJSON", [])])])]);
    return self;
},
args: [],
source: unescape('createAccount%0A%7C%20dict%20%7C%0Adict%20%3A%3D%20Dictionary%20new.%20%0Adict%20at%3A%20%27_id%27%20put%3A%20%28%27%23createUsername%27%20asJQuery%20val%29.%0Adict%20at%3A%20%27email%27%20put%3A%20%28%27%23createEmail%27%20asJQuery%20val%29.%0Adict%20at%3A%20%27password%27%20put%3A%20%28%27%23createPassword%27%20asJQuery%20val%29.%20%0A%0A%20%20jQuery%20%0A%09%09%09ajax%3A%20%27FileServer%3FcreateAccount%3A%27%0A%09%09%09options%3A%20%23%7B%0A%09%09%09%09%27type%27%20-%3E%20%27POST%27.%0A%09%09%09%09%27success%27%20-%3E%20%5B%20%3Amessage%20%7C%20window%20alert%3A%20message%20%5D.%0A%09%09%09%09%27error%27%20-%3E%20%5B%20%3Aex%20%7C%20window%20alert%3A%20%27error%3A%20%27%2C%20ex%20responseText%20%5D.%0A%09%09%09%09%22%27dataType%27%20-%3E%20%27jsonp%27.%22%0A%09%09%09%09%27data%27%20-%3E%20dict%20asJSON%0A%09%09%09%7D.'),
messageSends: ["new", "at:put:", "val", "asJQuery", "ajax:options:", unescape("-%3E"), "alert:", unescape("%2C"), "responseText", "asJSON"],
referencedClasses: ["Dictionary"]
}),
smalltalk.UserContainer);

smalltalk.addMethod(
unescape('_showLogin'),
smalltalk.method({
selector: unescape('showLogin'),
category: 'not yet classified',
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(unescape("%23createAccount"), "_asJQuery", []), "_hide", []);
    smalltalk.send(smalltalk.send(unescape("%23login"), "_asJQuery", []), "_show", []);
    smalltalk.send(smalltalk.send(unescape("%23browse"), "_asJQuery", []), "_hide", []);
    return self;
},
args: [],
source: unescape('showLogin%0A%27%23createAccount%27%20asJQuery%20hide.%0A%27%23login%27%20asJQuery%20show.%0A%27%23browse%27%20asJQuery%20hide.'),
messageSends: ["hide", "asJQuery", "show"],
referencedClasses: []
}),
smalltalk.UserContainer);

smalltalk.addMethod(
unescape('_showCreateAccount'),
smalltalk.method({
selector: unescape('showCreateAccount'),
category: 'not yet classified',
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(unescape("%23createAccount"), "_asJQuery", []), "_show", []);
    smalltalk.send(smalltalk.send(unescape("%23login"), "_asJQuery", []), "_hide", []);
    smalltalk.send(smalltalk.send(unescape("%23browse"), "_asJQuery", []), "_hide", []);
    return self;
},
args: [],
source: unescape('showCreateAccount%0A%27%23createAccount%27%20asJQuery%20show.%0A%27%23login%27%20asJQuery%20hide.%0A%27%23browse%27%20asJQuery%20hide.'),
messageSends: ["show", "asJQuery", "hide"],
referencedClasses: []
}),
smalltalk.UserContainer);

smalltalk.addMethod(
unescape('_showBrowse'),
smalltalk.method({
selector: unescape('showBrowse'),
category: 'not yet classified',
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
},
args: [],
source: unescape('showBrowse%0A%7C%20canvas%20tb%20div%20%7C%20%0A%27%23createAccount%27%20asJQuery%20hide.%0A%27%23login%27%20asJQuery%20hide.%0A%27%23browse%27%20asJQuery%20show.%0A%0Adiv%20%3A%3D%20%27%23packages%27%20asJQuery.%0Adiv%20empty.%0A%0APackageHandler%20new%20browsePublic%3A%20%5B%0A%20%20%09%3Amessage%20%7C%20%0A%20%20%09%09%28JSON%20parse%3A%20message%29%20do%3A%20%5B%20%3A%20entry%20%7C%0A%09%09%09%7C%20url%20a%20name%20version%20%7C%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20name%20%3A%3D%20entry%20key.%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20version%20%3A%3D%20entry%20value.%0A%09%09%09url%20%3A%3D%20%27http%3A//localhost%3A8080/packages/%27%2C%20name%2C%20%27.st%3Fversion%3D%27%2C%20version.%0A%20%20%09%09%09Transcript%20show%3A%20url%3B%20cr.%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20a%20%3A%3D%20%28HTMLCanvas%20new%20a%20href%3A%20url%29.%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20a%20append%3A%20%27Package%3A%20%27%2C%20name%2C%20%27%20Version%3A%20%27%2C%20version.%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%09div%20append%3A%20%28a%20asJQuery%29.%0A%09%09%09div%20append%3A%20%28HTMLCanvas%20new%20br%20asJQuery%29.%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%5D%0A%09%5D%20%0A%0A%09onError%3A%20%20%5B%20%3Aex%20%7C%20window%20alert%3A%20%28ex%20responseText%29%20%5D.'),
messageSends: ["hide", "asJQuery", "show", "empty", "browsePublic:onError:", "new", "do:", "parse:", "key", "value", unescape("%2C"), "show:", "cr", "href:", "a", "append:", "br", "alert:", "responseText"],
referencedClasses: ["PackageHandler", "JSON", "Transcript", "HTMLCanvas"]
}),
smalltalk.UserContainer);

smalltalk.addMethod(
unescape('_login'),
smalltalk.method({
selector: unescape('login'),
category: 'not yet classified',
fn: function () {
    var self = this;
    var dict = nil;
    dict = smalltalk.send(smalltalk.Dictionary || Dictionary, "_new", []);
    smalltalk.send(dict, "_at_put_", ["_id", smalltalk.send(smalltalk.send(unescape("%23loginUsername"), "_asJQuery", []), "_val", [])]);
    smalltalk.send(dict, "_at_put_", ["password", smalltalk.send(smalltalk.send(unescape("%23loginPassword"), "_asJQuery", []), "_val", [])]);
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_options_", [unescape("FileServer%3Flogin%3A"), smalltalk.HashedCollection._fromPairs_([smalltalk.send("type", "__minus_gt", ["POST"]), smalltalk.send("success", "__minus_gt", [function (message) {smalltalk.send(typeof window == "undefined" ? nil : window, "_alert_", [message]);smalltalk.send(self, "_username_", [smalltalk.send(dict, "_at_", ["_id"])]);return smalltalk.send(self, "_password_", [smalltalk.send(dict, "_at_", ["password"])]);}]), smalltalk.send("error", "__minus_gt", [function (ex) {return smalltalk.send(typeof window == "undefined" ? nil : window, "_alert_", [smalltalk.send(ex, "_responseText", [])]);}]), smalltalk.send("data", "__minus_gt", [smalltalk.send(dict, "_asJSON", [])])])]);
    return self;
},
args: [],
source: unescape('login%0A%7C%20dict%20%7C%0Adict%20%3A%3D%20Dictionary%20new.%20%0Adict%20at%3A%20%27_id%27%20put%3A%20%28%27%23loginUsername%27%20asJQuery%20val%29.%0Adict%20at%3A%20%27password%27%20put%3A%20%28%27%23loginPassword%27%20asJQuery%20val%29.%20%0A%0A%20%20jQuery%20%0A%09%09%09ajax%3A%20%27FileServer%3Flogin%3A%27%0A%09%09%09options%3A%20%23%7B%0A%09%09%09%09%27type%27%20-%3E%20%27POST%27.%0A%09%09%09%09%27success%27%20-%3E%20%5B%20%3Amessage%20%7C%20window%20alert%3A%20message.%20self%20username%3A%20%28dict%20at%3A%20%27_id%27%29.%20self%20password%3A%20%28dict%20at%3A%20%27password%27%20%29%20%5D.%0A%09%09%09%09%27error%27%20-%3E%20%5B%20%3Aex%20%7C%20window%20alert%3A%20%28ex%20responseText%29%20%5D.%0A%09%09%09%09%22%27dataType%27%20-%3E%20%27jsonp%27.%22%0A%09%09%09%09%27data%27%20-%3E%20dict%20asJSON%0A%09%09%09%7D.'),
messageSends: ["new", "at:put:", "val", "asJQuery", "ajax:options:", unescape("-%3E"), "alert:", "username:", "at:", "password:", "responseText", "asJSON"],
referencedClasses: ["Dictionary"]
}),
smalltalk.UserContainer);

smalltalk.addMethod(
unescape('_username'),
smalltalk.method({
selector: unescape('username'),
category: 'not yet classified',
fn: function () {
    var self = this;
    return self['@username'];
    return self;
},
args: [],
source: unescape('username%0A%5Eusername'),
messageSends: [],
referencedClasses: []
}),
smalltalk.UserContainer);

smalltalk.addMethod(
unescape('_username_'),
smalltalk.method({
selector: unescape('username%3A'),
category: 'not yet classified',
fn: function (aUsername) {
    var self = this;
    self['@username'] = aUsername;
    return self;
},
args: ["aUsername"],
source: unescape('username%3A%20aUsername%0Ausername%20%3A%3D%20aUsername'),
messageSends: [],
referencedClasses: []
}),
smalltalk.UserContainer);

smalltalk.addMethod(
unescape('_password'),
smalltalk.method({
selector: unescape('password'),
category: 'not yet classified',
fn: function () {
    var self = this;
    return self['@password'];
    return self;
},
args: [],
source: unescape('password%0A%5Epassword'),
messageSends: [],
referencedClasses: []
}),
smalltalk.UserContainer);

smalltalk.addMethod(
unescape('_password_'),
smalltalk.method({
selector: unescape('password%3A'),
category: 'not yet classified',
fn: function (aPassword) {
    var self = this;
    self['@password'] = aPassword;
    return self;
},
args: ["aPassword"],
source: unescape('password%3A%20aPassword%0Apassword%20%3A%3D%20aPassword'),
messageSends: [],
referencedClasses: []
}),
smalltalk.UserContainer);



smalltalk.addClass('PackageHandler', smalltalk.Object, [], 'CloudNodeClient');
smalltalk.addMethod(
unescape('_browsePublic_onError_'),
smalltalk.method({
selector: unescape('browsePublic%3AonError%3A'),
category: 'not yet classified',
fn: function (onSuccessBlock, onErrorBlock) {
    var self = this;
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_options_", [unescape("FileServer%3Fbrowse%3A"), smalltalk.Dictionary._fromPairs_([smalltalk.send("type", "__minus_gt", ["POST"]), smalltalk.send("success", "__minus_gt", [onSuccessBlock]), smalltalk.send("data", "__minus_gt", ["bla"]), smalltalk.send("error", "__minus_gt", [onErrorBlock])])]);
    return self;
},
args: ["onSuccessBlock", "onErrorBlock"],
source: unescape('browsePublic%3A%20onSuccessBlock%20onError%3A%20onErrorBlock%0A%0AjQuery%20%0A%09%09%09ajax%3A%20%28%27FileServer%3Fbrowse%3A%27%29%0A%09%09%09options%3A%20%23%7B%0A%09%09%09%09%27type%27%20-%3E%20%27POST%27.%0A%09%09%09%09%27success%27%20-%3E%20onSuccessBlock.%0A%09%09%09%09%27data%27%20-%3E%20%27bla%27.%0A%09%09%09%09%27error%27%20-%3E%20onErrorBlock%0A%09%09%09%7D.'),
messageSends: ["ajax:options:", unescape("-%3E")],
referencedClasses: []
}),
smalltalk.PackageHandler);

smalltalk.addMethod(
unescape('_commit_user_password_'),
smalltalk.method({
selector: unescape('commit%3Auser%3Apassword%3A'),
category: 'not yet classified',
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
},
args: ["aPackage", "aUser", "aPassword"],
source: unescape('commit%3A%20aPackage%20user%3A%20aUser%20password%3A%20aPassword%0A%7C%20dict%20%7C%0Adict%20%3A%3D%20Dictionary%20new.%0A%0A%28Smalltalk%20current%20packageAt%3A%20aPackage%29%0A%09isNil%20ifTrue%3A%20%5B%20%5Ewindow%20alert%3A%20%27Package%20not%20found%27%20%5D.%0A%0Adict%20at%3A%20%27packageName%27%20put%3A%20aPackage.%0Adict%20at%3A%20%27packageMeta%27%20put%3A%20%28%28Smalltalk%20current%20packageAt%3A%20aPackage%29%20propertiesAsJSON%29.%0A%22dict%20at%3A%20%27js%27%20put%3A%20%28Exporter%20new%20exportPackage%3A%20aPackage%29.%22%0Adict%20at%3A%20%27jsdeploy%27%20put%3A%20%28StrippedExporter%20new%20exportPackage%3A%20aPackage%29.%0Adict%20at%3A%20%27st%27%20put%3A%20%28ChunkExporter%20new%20exportPackage%3A%20aPackage%29.%0Adict%20at%3A%20%27user%27%20put%3A%20aUser.%0Adict%20at%3A%20%27password%27%20put%3A%20aPassword.%0A%0AjQuery%20%0A%09%09%09ajax%3A%20%28%27FileServer%3FcommitPackage%3A%27%29%0A%09%09%09options%3A%20%23%7B%0A%09%09%09%09%27type%27%20-%3E%20%27POST%27.%0A%09%09%09%09%27success%27%20-%3E%20%5B%20%3Amessage%20%7C%20window%20alert%3A%20message.%20%5D.%0A%09%09%09%09%27data%27%20-%3E%20%28dict%20asJSON%29.%0A%09%09%09%09%27error%27%20-%3E%20%5B%20%3Aex%20%7C%20window%20alert%3A%20%28ex%20responseText%29%20%5D%0A%09%09%09%7D.'),
messageSends: ["new", "ifTrue:", "isNil", "packageAt:", "current", "alert:", "at:put:", "propertiesAsJSON", "exportPackage:", "ajax:options:", unescape("-%3E"), "asJSON", "responseText"],
referencedClasses: ["Dictionary", "Smalltalk", "StrippedExporter", "ChunkExporter"]
}),
smalltalk.PackageHandler);



