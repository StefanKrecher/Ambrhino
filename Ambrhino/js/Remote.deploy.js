smalltalk.addClass('HTTPRequest', smalltalk.Object, [], 'Remote');
smalltalk.addMethod(
'_post',
smalltalk.method({
selector: 'post',
fn: function () {
    var self = this;
    var result = nil;
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_options_", [unescape("http%3A//localhost%3A4000/bla"), smalltalk.Dictionary._fromPairs_([smalltalk.send("type", "__minus_gt", ["POST"]), smalltalk.send("success", "__minus_gt", [function (tmp) {return smalltalk.send(typeof window == "undefined" ? nil : window, "_alert_", [smalltalk.send("success ", "__comma", [tmp])]);}]), smalltalk.send("error", "__minus_gt", [function () {return smalltalk.send(typeof window == "undefined" ? nil : window, "_alert_", ["error"]);}]), smalltalk.send("data", "__minus_gt", ["Moin Moin"])])]);
    return self;
}
}),
smalltalk.HTTPRequest);



