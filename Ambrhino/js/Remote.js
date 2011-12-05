smalltalk.addClass('HTTPRequest', smalltalk.Object, [], 'Remote');
smalltalk.addMethod(
unescape('_post'),
smalltalk.method({
selector: unescape('post'),
category: 'not yet classified',
fn: function () {
    var self = this;
    var result = nil;
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_options_", [unescape("http%3A//localhost%3A4000/bla"), smalltalk.Dictionary._fromPairs_([smalltalk.send("type", "__minus_gt", ["POST"]), smalltalk.send("success", "__minus_gt", [function (tmp) {return smalltalk.send(typeof window == "undefined" ? nil : window, "_alert_", [smalltalk.send("success ", "__comma", [tmp])]);}]), smalltalk.send("error", "__minus_gt", [function () {return smalltalk.send(typeof window == "undefined" ? nil : window, "_alert_", ["error"]);}]), smalltalk.send("data", "__minus_gt", ["Moin Moin"])])]);
    return self;
},
args: [],
source: unescape('post%0A%7C%20result%20%7C%0A%20jQuery%20%0A%09%09%09ajax%3A%20%27http%3A//localhost%3A4000/bla%27%0A%09%09%09options%3A%20%23%7B%0A%09%09%09%09%27type%27%20-%3E%20%27POST%27.%0A%09%09%09%09%27success%27%20-%3E%20%5B%20%3Atmp%20%7C%20window%20alert%3A%20%27success%20%27%2C%20tmp%5D.%0A%09%09%09%09%27error%27%20-%3E%20%5Bwindow%20alert%3A%20%27error%27%5D.%0A%09%09%09%09%22%27dataType%27%20-%3E%20%27jsonp%27.%22%0A%09%09%09%09%27data%27%20-%3E%20%27Moin%20Moin%27%0A%09%09%09%7D.%20'),
messageSends: ["ajax:options:", unescape("-%3E"), "alert:", unescape("%2C")],
referencedClasses: []
}),
smalltalk.HTTPRequest);



