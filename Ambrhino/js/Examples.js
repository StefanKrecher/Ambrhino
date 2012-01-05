smalltalk.addPackage('Examples', {});
smalltalk.addClass('JavaCall', smalltalk.Object, [], 'Examples');
smalltalk.addMethod(
unescape('_initialize'),
smalltalk.method({
selector: unescape('initialize'),
category: 'not yet classified',
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.Object);
    return self;
},
args: [],
source: unescape('initialize%0Asuper%20initialize'),
messageSends: ["initialize"],
referencedClasses: []
}),
smalltalk.JavaCall);


smalltalk.addMethod(
unescape('_main'),
smalltalk.method({
selector: unescape('main'),
category: 'not yet classified',
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
},
args: [],
source: unescape('main%0A%7C%20frame%20textArea%20scrollPane%20mainPanel%20buttonPanel%20buttonDoit%20buttonPrintit%20aNull%20borderLayout%20%7C%0A%3CimportPackage%20%28javax.swing%29%3E.%0A%3CimportPackage%20%28java.awt%29%3E.%0A%3CimportClass%28java.awt.Window%29%3E.%0A%0A%3Cframe%20%3D%20new%20JFrame%28%27a%20Workspace%27%29%3B%3E.%0A%3CborderLayout%20%3D%20new%20BorderLayout%28%29%3B%3E.%0AtextArea%20%3A%3D%20JTextArea%20new.%0AtextArea%20initialize.%0A%0A%3CmainPanel%20%3D%20new%20JPanel%28%29%3B%3E.%0A%3CbuttonPanel%20%3D%20new%20JPanel%28%29%3B%3E.%0A%0A%3CbuttonDoit%20%3D%20new%20JButton%28%27do%20it%27%29%3B%3E.%0A%3CbuttonPrintit%20%3D%20new%20JButton%28%27print%20it%27%29%3B%3E.%0A%0A%3CscrollPane%20%3D%20new%20JScrollPane%28%29%3E.%0A%0Aframe%20setDefaultCloseOperation%3A%203.%20%22JFrame.EXIT_ON_CLOSE%22%0A%0AbuttonPrintit%20addActionListener%3A%20%5B%20%0A%09%7C%20result%20pos%20%7C%0A%09result%20%3A%3D%20self%20eval%3A%20%28textArea%20javaObject%20getSelectedText%29.%0A%09pos%20%3A%3D%20textArea%20javaObject%20getSelectionEnd.%0A%09textArea%20insert%3A%20result%20pos%3A%20pos.%0A%09textArea%20select%3A%20pos%20size%3A%20%28result%20asString%20size%29.%0A%5D.%0A%0AbuttonDoit%20addActionListener%3A%20%5B%20%0A%09self%20eval%3A%20%28textArea%20javaObject%20getSelectedText%29.%0A%5D.%0A%0A%3CaNull%20%3D%20null%3E.%0Aframe%20setLocationRelativeTo%3A%20aNull.%0Aframe%20setLayout%3A%20borderLayout.%0AscrollPane%20setPreferredSize%3A%20%28self%20dimension%3A%20%23%28400%20300%29%29.%0AscrollPane%20setViewportView%3A%20%28textArea%20javaObject%29.%0AscrollPane%20setVerticalScrollBarPolicy%3A%2020.%20%22VERTICAL_SCROLLBAR_AS_NEEDED%20%3D%2020%3B%22%20%0AmainPanel%20add%3A%20scrollPane.%0A%09%09%09%09%0AbuttonPanel%20add%3A%20buttonDoit.%0AbuttonPanel%20add%3A%20buttonPrintit.%0A%0Aself%20frame%3A%20frame%20add%3A%20mainPanel%20orientation%3A%20%27Center%27.%0Aself%20frame%3A%20frame%20add%3A%20buttonPanel%20orientation%3A%20%27South%27.%0Aframe%20pack.%0Aframe%20setVisible%3A%20true.%0A%0A%5E%27Return%20from%20Amber%27'),
messageSends: ["new", "initialize", "setDefaultCloseOperation:", "addActionListener:", "eval:", "getSelectedText", "javaObject", "getSelectionEnd", "insert:pos:", "select:size:", "size", "asString", "setLocationRelativeTo:", "setLayout:", "setPreferredSize:", "dimension:", "setViewportView:", "setVerticalScrollBarPolicy:", "add:", "frame:add:orientation:", "pack", "setVisible:"],
referencedClasses: ["JTextArea"]
}),
smalltalk.JavaCall.klass);

smalltalk.addMethod(
unescape('_dimension_height_'),
smalltalk.method({
selector: unescape('dimension%3Aheight%3A'),
category: 'not yet classified',
fn: function (width, height) {
    var self = this;
    var dim = nil;
    dim = new Dimension(width, height);
    return dim;
    return self;
},
args: ["width", "height"],
source: unescape('dimension%3A%20width%20height%3A%20height%0A%7C%20dim%20%7C%0A%3Cdim%20%3D%20new%20Dimension%28width%2C%20height%29%3E.%0A%5Edim'),
messageSends: [],
referencedClasses: []
}),
smalltalk.JavaCall.klass);

smalltalk.addMethod(
unescape('_dimension_'),
smalltalk.method({
selector: unescape('dimension%3A'),
category: 'not yet classified',
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
},
args: ["col"],
source: unescape('dimension%3A%20col%0A%7C%20dim%20w%20h%20%7C%0Aw%20%3A%3D%20col%20at%3A%201.%0Ah%20%3A%3D%20col%20at%3A%202.%0A%3Cdim%20%3D%20new%20Dimension%28w%2C%20h%29%3E.%0A%5Edim'),
messageSends: ["at:"],
referencedClasses: []
}),
smalltalk.JavaCall.klass);

smalltalk.addMethod(
unescape('_messageBox_'),
smalltalk.method({
selector: unescape('messageBox%3A'),
category: 'not yet classified',
fn: function (aString) {
    var self = this;
    importPackage(javax.swing);
    JOptionPane.showMessageDialog(null, aString);
    return self;
},
args: ["aString"],
source: unescape('messageBox%3A%20aString%0A%3CimportPackage%20%28javax.swing%29%3E.%0A%3CJOptionPane.showMessageDialog%28null%2C%20%20aString%29%3E.'),
messageSends: [],
referencedClasses: []
}),
smalltalk.JavaCall.klass);

smalltalk.addMethod(
unescape('_eval_'),
smalltalk.method({
selector: unescape('eval%3A'),
category: 'not yet classified',
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
},
args: ["aString"],
source: unescape('eval%3A%20aString%0A%7C%20compiler%20compiled%20cm%20%7C%0Acompiler%20%3A%3D%20Compiler%20new.%0A%5Bcompiler%20parseExpression%3A%20aString%5D%20on%3A%20Error%20do%3A%20%5B%3Aex%20%7C%0A%09%5Eself%20messageBox%3A%20ex%20messageText%5D.%0A%5E%28compiler%20load%3A%20%27doIt%20%5E%5B%27%2C%20aString%2C%20%27%5D%20value%27%20forClass%3A%20DoIt%29%20fn%20value'),
messageSends: ["new", "on:do:", "parseExpression:", "messageBox:", "messageText", "value", "fn", "load:forClass:", unescape("%2C")],
referencedClasses: ["Compiler", "Error", "DoIt"]
}),
smalltalk.JavaCall.klass);

smalltalk.addMethod(
unescape('_frame_add_orientation_'),
smalltalk.method({
selector: unescape('frame%3Aadd%3Aorientation%3A'),
category: 'not yet classified',
fn: function (aFrame, aComponent, anOrientation) {
    var self = this;
    aFrame.add(aComponent, anOrientation);
    return self;
},
args: ["aFrame", "aComponent", "anOrientation"],
source: unescape('frame%3A%20aFrame%20add%3A%20aComponent%20orientation%3A%20anOrientation%0A%3CaFrame.add%28aComponent%2C%20anOrientation%29%3E.'),
messageSends: [],
referencedClasses: []
}),
smalltalk.JavaCall.klass);


smalltalk.addClass('MessageBox', smalltalk.Object, ['joptionspane'], 'Examples');

smalltalk.addMethod(
unescape('_initialize'),
smalltalk.method({
selector: unescape('initialize'),
category: 'not yet classified',
fn: function () {
    var self = this;
    importPackage(javax.swing);
    return self;
},
args: [],
source: unescape('initialize%0A%3CimportPackage%20%28javax.swing%29%3E.'),
messageSends: [],
referencedClasses: []
}),
smalltalk.MessageBox.klass);


smalltalk.addClass('JTextArea', smalltalk.Object, ['textArea'], 'Examples');
smalltalk.addMethod(
unescape('_javaObject'),
smalltalk.method({
selector: unescape('javaObject'),
category: 'not yet classified',
fn: function () {
    var self = this;
    return self['@textArea'];
    return self;
},
args: [],
source: unescape('javaObject%0A%5EtextArea'),
messageSends: [],
referencedClasses: []
}),
smalltalk.JTextArea);

smalltalk.addMethod(
unescape('_insert_pos_'),
smalltalk.method({
selector: unescape('insert%3Apos%3A'),
category: 'not yet classified',
fn: function (aString, aPos) {
    var self = this;
    var ta = nil;
    ta = smalltalk.send(self, "_javaObject", []);
    ta.insert(aString, aPos);
    return self;
},
args: ["aString", "aPos"],
source: unescape('insert%3A%20aString%20pos%3A%20aPos%0A%7C%20ta%20%7C%0Ata%20%3A%3D%20self%20javaObject.%0A%3Cta.insert%28aString%2C%20aPos%29%3E.'),
messageSends: ["javaObject"],
referencedClasses: []
}),
smalltalk.JTextArea);

smalltalk.addMethod(
unescape('_initialize'),
smalltalk.method({
selector: unescape('initialize'),
category: 'not yet classified',
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.Object);
    self['@textArea'] = new JTextArea;
    return self;
},
args: [],
source: unescape('initialize%0Asuper%20initialize.%0AtextArea%20%3A%3D%20%3Cnew%20JTextArea%28%29%3E.'),
messageSends: ["initialize"],
referencedClasses: []
}),
smalltalk.JTextArea);

smalltalk.addMethod(
unescape('_select_size_'),
smalltalk.method({
selector: unescape('select%3Asize%3A'),
category: 'not yet classified',
fn: function (start, size) {
    var self = this;
    var ta = nil;
    var end = nil;
    ta = smalltalk.send(self, "_javaObject", []);
    end = ($receiver = start).klass === smalltalk.Number ? $receiver + size : smalltalk.send($receiver, "__plus", [size]);
    ta.select(start, end);
    smalltalk.send(ta, "_requestFocus", []);
    return self;
},
args: ["start", "size"],
source: unescape('select%3A%20start%20size%3A%20size%0A%7C%20ta%20end%20%7C%0Ata%20%3A%3D%20self%20javaObject.%0Aend%20%3A%3D%20start%20+%20size.%0A%3Cta.select%28start%2C%20end%29%3E.%0Ata%20requestFocus.'),
messageSends: ["javaObject", unescape("+"), "requestFocus"],
referencedClasses: []
}),
smalltalk.JTextArea);



