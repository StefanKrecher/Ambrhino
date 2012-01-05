smalltalk.addPackage('Examples', {});
smalltalk.addClass('JavaCall', smalltalk.Object, [], 'Examples');
smalltalk.addMethod(
'_initialize',
smalltalk.method({
selector: 'initialize',
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.Object);
    return self;
}
}),
smalltalk.JavaCall);


smalltalk.addMethod(
'_main',
smalltalk.method({
selector: 'main',
fn: function () {
    var self = this;
    var frame = nil;
    var textArea = nil;
    var scrollPane = nil;
    var mainPanel = nil;
    var buttonPanel = nil;
    var buttonDoit = nil;
    var buttonPrintit = nil;
    var aNull = nil;
    var borderLayout = nil;
    importPackage(javax.swing);
    importPackage(java.awt);
    importClass(java.awt.Window);
    frame = new JFrame("a Workspace");
    borderLayout = new BorderLayout;
    textArea = smalltalk.send(smalltalk.JTextArea || JTextArea, "_new", []);
    smalltalk.send(textArea, "_initialize", []);
    mainPanel = new JPanel;
    buttonPanel = new JPanel;
    buttonDoit = new JButton("do it");
    buttonPrintit = new JButton("print it");
    scrollPane = new JScrollPane;
    smalltalk.send(frame, "_setDefaultCloseOperation_", [3]);
    smalltalk.send(buttonPrintit, "_addActionListener_", [function () {var result = nil;var pos = nil;result = smalltalk.send(self, "_eval_", [smalltalk.send(smalltalk.send(textArea, "_javaObject", []), "_getSelectedText", [])]);pos = smalltalk.send(smalltalk.send(textArea, "_javaObject", []), "_getSelectionEnd", []);smalltalk.send(textArea, "_insert_pos_", [result, pos]);return smalltalk.send(textArea, "_select_size_", [pos, smalltalk.send(smalltalk.send(result, "_asString", []), "_size", [])]);}]);
    smalltalk.send(buttonDoit, "_addActionListener_", [function () {return smalltalk.send(self, "_eval_", [smalltalk.send(smalltalk.send(textArea, "_javaObject", []), "_getSelectedText", [])]);}]);
    aNull = null;
    smalltalk.send(frame, "_setLocationRelativeTo_", [aNull]);
    smalltalk.send(frame, "_setLayout_", [borderLayout]);
    smalltalk.send(scrollPane, "_setPreferredSize_", [smalltalk.send(self, "_dimension_", [[400, 300]])]);
    smalltalk.send(scrollPane, "_setViewportView_", [smalltalk.send(textArea, "_javaObject", [])]);
    smalltalk.send(scrollPane, "_setVerticalScrollBarPolicy_", [20]);
    smalltalk.send(mainPanel, "_add_", [scrollPane]);
    smalltalk.send(buttonPanel, "_add_", [buttonDoit]);
    smalltalk.send(buttonPanel, "_add_", [buttonPrintit]);
    smalltalk.send(self, "_frame_add_orientation_", [frame, mainPanel, "Center"]);
    smalltalk.send(self, "_frame_add_orientation_", [frame, buttonPanel, "South"]);
    smalltalk.send(frame, "_pack", []);
    smalltalk.send(frame, "_setVisible_", [true]);
    return "Return from Amber";
    return self;
}
}),
smalltalk.JavaCall.klass);

smalltalk.addMethod(
'_dimension_height_',
smalltalk.method({
selector: 'dimension:height:',
fn: function (width, height) {
    var self = this;
    var dim = nil;
    dim = new Dimension(width, height);
    return dim;
    return self;
}
}),
smalltalk.JavaCall.klass);

smalltalk.addMethod(
'_dimension_',
smalltalk.method({
selector: 'dimension:',
fn: function (col) {
    var self = this;
    var dim = nil;
    var w = nil;
    var h = nil;
    w = smalltalk.send(col, "_at_", [1]);
    h = smalltalk.send(col, "_at_", [2]);
    dim = new Dimension(w, h);
    return dim;
    return self;
}
}),
smalltalk.JavaCall.klass);

smalltalk.addMethod(
'_messageBox_',
smalltalk.method({
selector: 'messageBox:',
fn: function (aString) {
    var self = this;
    importPackage(javax.swing);
    JOptionPane.showMessageDialog(null, aString);
    return self;
}
}),
smalltalk.JavaCall.klass);

smalltalk.addMethod(
'_eval_',
smalltalk.method({
selector: 'eval:',
fn: function (aString) {
    var self = this;
    try {
        var compiler = nil;
        var compiled = nil;
        var cm = nil;
        compiler = smalltalk.send(smalltalk.Compiler || Compiler, "_new", []);
        smalltalk.send(function () {return smalltalk.send(compiler, "_parseExpression_", [aString]);}, "_on_do_", [smalltalk.Error || Error, function (ex) {return function () {throw {name: "stReturn", selector: "_eval_", fn: function () {return smalltalk.send(self, "_messageBox_", [smalltalk.send(ex, "_messageText", [])]);}};}();}]);
        (function () {throw {name: "stReturn", selector: "_eval_", fn: function () {return smalltalk.send(smalltalk.send(smalltalk.send(compiler, "_load_forClass_", [smalltalk.send(smalltalk.send(unescape("doIt%20%5E%5B"), "__comma", [aString]), "__comma", [unescape("%5D%20value")]), smalltalk.DoIt || DoIt]), "_fn", []), "_value", []);}};}());
        return self;
    } catch (e) {
        if (e.name === "stReturn" && e.selector === "_eval_") {
            return e.fn();
        }
        throw e;
    }
}
}),
smalltalk.JavaCall.klass);

smalltalk.addMethod(
'_frame_add_orientation_',
smalltalk.method({
selector: 'frame:add:orientation:',
fn: function (aFrame, aComponent, anOrientation) {
    var self = this;
    aFrame.add(aComponent, anOrientation);
    return self;
}
}),
smalltalk.JavaCall.klass);


smalltalk.addClass('MessageBox', smalltalk.Object, ['joptionspane'], 'Examples');

smalltalk.addMethod(
'_initialize',
smalltalk.method({
selector: 'initialize',
fn: function () {
    var self = this;
    importPackage(javax.swing);
    return self;
}
}),
smalltalk.MessageBox.klass);


smalltalk.addClass('JTextArea', smalltalk.Object, ['textArea'], 'Examples');
smalltalk.addMethod(
'_javaObject',
smalltalk.method({
selector: 'javaObject',
fn: function () {
    var self = this;
    return self['@textArea'];
    return self;
}
}),
smalltalk.JTextArea);

smalltalk.addMethod(
'_insert_pos_',
smalltalk.method({
selector: 'insert:pos:',
fn: function (aString, aPos) {
    var self = this;
    var ta = nil;
    ta = smalltalk.send(self, "_javaObject", []);
    ta.insert(aString, aPos);
    return self;
}
}),
smalltalk.JTextArea);

smalltalk.addMethod(
'_initialize',
smalltalk.method({
selector: 'initialize',
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.Object);
    self['@textArea'] = new JTextArea;
    return self;
}
}),
smalltalk.JTextArea);

smalltalk.addMethod(
'_select_size_',
smalltalk.method({
selector: 'select:size:',
fn: function (start, size) {
    var self = this;
    var ta = nil;
    var end = nil;
    ta = smalltalk.send(self, "_javaObject", []);
    end = ($receiver = start).klass === smalltalk.Number ? $receiver + size : smalltalk.send($receiver, "__plus", [size]);
    ta.select(start, end);
    smalltalk.send(ta, "_requestFocus", []);
    return self;
}
}),
smalltalk.JTextArea);



