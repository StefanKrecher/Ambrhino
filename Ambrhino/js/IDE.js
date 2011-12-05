smalltalk.addPackage('IDE', {});
smalltalk.addClass('TabManager', smalltalk.Widget, ['selectedTab', 'tabs', 'opened', 'ul', 'input'], 'IDE');
smalltalk.addMethod(
unescape('_tabs'),
smalltalk.method({
selector: unescape('tabs'),
category: 'accessing',
fn: function () {
    var self = this;
    return ($receiver = self['@tabs']) == nil || $receiver == undefined ? function () {return self['@tabs'] = smalltalk.send(smalltalk.Array || Array, "_new", []);}() : $receiver;
    return self;
},
args: [],
source: unescape('tabs%0A%20%20%20%20%5Etabs%20ifNil%3A%20%5Btabs%20%3A%3D%20Array%20new%5D'),
messageSends: ["ifNil:", "new"],
referencedClasses: ["Array"]
}),
smalltalk.TabManager);

smalltalk.addMethod(
unescape('_labelFor_'),
smalltalk.method({
selector: unescape('labelFor%3A'),
category: 'accessing',
fn: function (aWidget) {
    var self = this;
    var label = nil;
    var maxSize = nil;
    maxSize = 15;
    label = smalltalk.send(smalltalk.send(aWidget, "_label", []), "_copyFrom_to_", [0, smalltalk.send(smalltalk.send(smalltalk.send(aWidget, "_label", []), "_size", []), "_min_", [maxSize])]);
    ($receiver = ($receiver = smalltalk.send(smalltalk.send(aWidget, "_label", []), "_size", [])).klass === smalltalk.Number ? $receiver > maxSize : smalltalk.send($receiver, "__gt", [maxSize])).klass === smalltalk.Boolean ? $receiver ? function () {return label = smalltalk.send(label, "__comma", ["..."]);}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return label = smalltalk.send(label, "__comma", ["..."]);}]);
    return label;
    return self;
},
args: ["aWidget"],
source: unescape('labelFor%3A%20aWidget%0A%09%7C%20label%20maxSize%20%7C%0A%09maxSize%20%3A%3D%2015.%0A%09label%20%3A%3D%20aWidget%20label%20copyFrom%3A%200%20to%3A%20%28aWidget%20label%20size%20min%3A%20maxSize%29.%0A%09aWidget%20label%20size%20%3E%20maxSize%20ifTrue%3A%20%5B%0A%09%09label%20%3A%3D%20label%2C%20%27...%27%5D.%0A%09%5Elabel'),
messageSends: ["copyFrom:to:", "label", "min:", "size", "ifTrue:", unescape("%3E"), unescape("%2C")],
referencedClasses: []
}),
smalltalk.TabManager);

smalltalk.addMethod(
unescape('_updateBodyMargin'),
smalltalk.method({
selector: unescape('updateBodyMargin'),
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_setBodyMargin_", [smalltalk.send(smalltalk.send(unescape("%23jtalk"), "_asJQuery", []), "_height", [])]);
    return self;
},
args: [],
source: unescape('updateBodyMargin%0A%20%20%20%20self%20setBodyMargin%3A%20%27%23jtalk%27%20asJQuery%20height'),
messageSends: ["setBodyMargin:", "height", "asJQuery"],
referencedClasses: []
}),
smalltalk.TabManager);

smalltalk.addMethod(
unescape('_updatePosition'),
smalltalk.method({
selector: unescape('updatePosition'),
category: 'actions',
fn: function () {
    var self = this;
    jQuery("#jtalk").css("top", "").css("bottom", "0px");
    return self;
},
args: [],
source: unescape('updatePosition%0A%20%20%20%20%3CjQuery%28%27%23jtalk%27%29.css%28%27top%27%2C%20%27%27%29.css%28%27bottom%27%2C%20%270px%27%29%3E'),
messageSends: [],
referencedClasses: []
}),
smalltalk.TabManager);

smalltalk.addMethod(
unescape('_removeBodyMargin'),
smalltalk.method({
selector: unescape('removeBodyMargin'),
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_setBodyMargin_", [0]);
    return self;
},
args: [],
source: unescape('removeBodyMargin%0A%20%20%20%20self%20setBodyMargin%3A%200'),
messageSends: ["setBodyMargin:"],
referencedClasses: []
}),
smalltalk.TabManager);

smalltalk.addMethod(
unescape('_setBodyMargin_'),
smalltalk.method({
selector: unescape('setBodyMargin%3A'),
category: 'actions',
fn: function (anInteger) {
    var self = this;
    smalltalk.send(smalltalk.send(".jtalkBody", "_asJQuery", []), "_css_put_", [unescape("margin-bottom"), smalltalk.send(smalltalk.send(anInteger, "_asString", []), "__comma", ["px"])]);
    return self;
},
args: ["anInteger"],
source: unescape('setBodyMargin%3A%20anInteger%0A%20%20%20%20%27.jtalkBody%27%20asJQuery%20css%3A%20%27margin-bottom%27%20put%3A%20anInteger%20asString%2C%20%27px%27'),
messageSends: ["css:put:", "asJQuery", unescape("%2C"), "asString"],
referencedClasses: []
}),
smalltalk.TabManager);

smalltalk.addMethod(
unescape('_onResize_'),
smalltalk.method({
selector: unescape('onResize%3A'),
category: 'actions',
fn: function (aBlock) {
    var self = this;
    jQuery("#jtalk").resizable({handles: "n", resize: aBlock, minHeight: 230});
    return self;
},
args: ["aBlock"],
source: unescape('onResize%3A%20aBlock%0A%20%20%20%20%3CjQuery%28%27%23jtalk%27%29.resizable%28%7B%0A%09handles%3A%20%27n%27%2C%20%0A%09resize%3A%20aBlock%2C%0A%09minHeight%3A%20230%0A%7D%29%3E'),
messageSends: [],
referencedClasses: []
}),
smalltalk.TabManager);

smalltalk.addMethod(
unescape('_onWindowResize_'),
smalltalk.method({
selector: unescape('onWindowResize%3A'),
category: 'actions',
fn: function (aBlock) {
    var self = this;
    jQuery(window).resize(aBlock);
    return self;
},
args: ["aBlock"],
source: unescape('onWindowResize%3A%20aBlock%0A%20%20%20%20%3CjQuery%28window%29.resize%28aBlock%29%3E'),
messageSends: [],
referencedClasses: []
}),
smalltalk.TabManager);

smalltalk.addMethod(
unescape('_open'),
smalltalk.method({
selector: unescape('open'),
category: 'actions',
fn: function () {
    var self = this;
    ($receiver = self['@opened']).klass === smalltalk.Boolean ? !$receiver ? function () {smalltalk.send(smalltalk.send("body", "_asJQuery", []), "_addClass_", ["jtalkBody"]);smalltalk.send(smalltalk.send(unescape("%23jtalk"), "_asJQuery", []), "_show", []);smalltalk.send(smalltalk.send(self['@ul'], "_asJQuery", []), "_show", []);smalltalk.send(self, "_updateBodyMargin", []);smalltalk.send(self['@selectedTab'], "_show", []);return self['@opened'] = true;}() : nil : smalltalk.send($receiver, "_ifFalse_", [function () {smalltalk.send(smalltalk.send("body", "_asJQuery", []), "_addClass_", ["jtalkBody"]);smalltalk.send(smalltalk.send(unescape("%23jtalk"), "_asJQuery", []), "_show", []);smalltalk.send(smalltalk.send(self['@ul'], "_asJQuery", []), "_show", []);smalltalk.send(self, "_updateBodyMargin", []);smalltalk.send(self['@selectedTab'], "_show", []);return self['@opened'] = true;}]);
    return self;
},
args: [],
source: unescape('open%0A%20%20%20%20opened%20ifFalse%3A%20%5B%0A%09%27body%27%20asJQuery%20addClass%3A%20%27jtalkBody%27.%0A%09%27%23jtalk%27%20asJQuery%20show.%0A%09ul%20asJQuery%20show.%0A%09self%20updateBodyMargin.%0A%09selectedTab%20show.%0A%09opened%20%3A%3D%20true%5D'),
messageSends: ["ifFalse:", "addClass:", "asJQuery", "show", "updateBodyMargin"],
referencedClasses: []
}),
smalltalk.TabManager);

smalltalk.addMethod(
unescape('_close'),
smalltalk.method({
selector: unescape('close'),
category: 'actions',
fn: function () {
    var self = this;
    ($receiver = self['@opened']).klass === smalltalk.Boolean ? $receiver ? function () {smalltalk.send(smalltalk.send(unescape("%23jtalk"), "_asJQuery", []), "_hide", []);smalltalk.send(smalltalk.send(self['@ul'], "_asJQuery", []), "_hide", []);smalltalk.send(self['@selectedTab'], "_hide", []);smalltalk.send(self, "_removeBodyMargin", []);smalltalk.send(smalltalk.send("body", "_asJQuery", []), "_removeClass_", ["jtalkBody"]);return self['@opened'] = false;}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {smalltalk.send(smalltalk.send(unescape("%23jtalk"), "_asJQuery", []), "_hide", []);smalltalk.send(smalltalk.send(self['@ul'], "_asJQuery", []), "_hide", []);smalltalk.send(self['@selectedTab'], "_hide", []);smalltalk.send(self, "_removeBodyMargin", []);smalltalk.send(smalltalk.send("body", "_asJQuery", []), "_removeClass_", ["jtalkBody"]);return self['@opened'] = false;}]);
    return self;
},
args: [],
source: unescape('close%0A%20%20%20%20opened%20ifTrue%3A%20%5B%0A%09%27%23jtalk%27%20asJQuery%20hide.%0A%09ul%20asJQuery%20hide.%0A%09selectedTab%20hide.%0A%09self%20removeBodyMargin.%0A%09%27body%27%20asJQuery%20removeClass%3A%20%27jtalkBody%27.%0A%09opened%20%3A%3D%20false%5D'),
messageSends: ["ifTrue:", "hide", "asJQuery", "removeBodyMargin", "removeClass:"],
referencedClasses: []
}),
smalltalk.TabManager);

smalltalk.addMethod(
unescape('_newBrowserTab'),
smalltalk.method({
selector: unescape('newBrowserTab'),
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.Browser || Browser, "_open", []);
    return self;
},
args: [],
source: unescape('newBrowserTab%0A%20%20%20%20Browser%20open'),
messageSends: ["open"],
referencedClasses: ["Browser"]
}),
smalltalk.TabManager);

smalltalk.addMethod(
unescape('_selectTab_'),
smalltalk.method({
selector: unescape('selectTab%3A'),
category: 'actions',
fn: function (aWidget) {
    var self = this;
    smalltalk.send(self, "_open", []);
    self['@selectedTab'] = aWidget;
    smalltalk.send(smalltalk.send(self, "_tabs", []), "_do_", [function (each) {return smalltalk.send(each, "_hide", []);}]);
    smalltalk.send(aWidget, "_show", []);
    smalltalk.send(self, "_update", []);
    return self;
},
args: ["aWidget"],
source: unescape('selectTab%3A%20aWidget%0A%20%20%20%20self%20open.%0A%20%20%20%20selectedTab%20%3A%3D%20aWidget.%0A%20%20%20%20self%20tabs%20do%3A%20%5B%3Aeach%20%7C%0A%09each%20hide%5D.%0A%20%20%20%20aWidget%20show.%0A%09%0A%20%20%20%20self%20update'),
messageSends: ["open", "do:", "tabs", "hide", "show", "update"],
referencedClasses: []
}),
smalltalk.TabManager);

smalltalk.addMethod(
unescape('_closeTab_'),
smalltalk.method({
selector: unescape('closeTab%3A'),
category: 'actions',
fn: function (aWidget) {
    var self = this;
    smalltalk.send(self, "_removeTab_", [aWidget]);
    smalltalk.send(self, "_selectTab_", [smalltalk.send(smalltalk.send(self, "_tabs", []), "_last", [])]);
    smalltalk.send(aWidget, "_remove", []);
    smalltalk.send(self, "_update", []);
    return self;
},
args: ["aWidget"],
source: unescape('closeTab%3A%20aWidget%0A%20%20%20%20self%20removeTab%3A%20aWidget.%0A%20%20%20%20self%20selectTab%3A%20self%20tabs%20last.%0A%20%20%20%20aWidget%20remove.%0A%20%20%20%20self%20update'),
messageSends: ["removeTab:", "selectTab:", "last", "tabs", "remove", "update"],
referencedClasses: []
}),
smalltalk.TabManager);

smalltalk.addMethod(
unescape('_search_'),
smalltalk.method({
selector: unescape('search%3A'),
category: 'actions',
fn: function (aString) {
    var self = this;
    var searchedClass = nil;
    searchedClass = smalltalk.send(smalltalk.send(smalltalk.Smalltalk || Smalltalk, "_current", []), "_at_", [aString]);
    ($receiver = smalltalk.send(searchedClass, "_isClass", [])).klass === smalltalk.Boolean ? $receiver ? function () {return smalltalk.send(smalltalk.Browser || Browser, "_openOn_", [searchedClass]);}() : function () {return smalltalk.send(smalltalk.ReferencesBrowser || ReferencesBrowser, "_search_", [aString]);}() : smalltalk.send($receiver, "_ifTrue_ifFalse_", [function () {return smalltalk.send(smalltalk.Browser || Browser, "_openOn_", [searchedClass]);}, function () {return smalltalk.send(smalltalk.ReferencesBrowser || ReferencesBrowser, "_search_", [aString]);}]);
    return self;
},
args: ["aString"],
source: unescape('search%3A%20aString%0A%09%7C%20searchedClass%20%7C%0A%09searchedClass%20%3A%3D%20Smalltalk%20current%20at%3A%20aString.%0A%09%09searchedClass%20isClass%0A%09%09%09ifTrue%3A%20%5BBrowser%20openOn%3A%20searchedClass%5D%0A%09%09%09ifFalse%3A%20%5BReferencesBrowser%20search%3A%20aString%5D'),
messageSends: ["at:", "current", "ifTrue:ifFalse:", "isClass", "openOn:", "search:"],
referencedClasses: ["Smalltalk", "Browser", "ReferencesBrowser"]
}),
smalltalk.TabManager);

smalltalk.addMethod(
unescape('_addTab_'),
smalltalk.method({
selector: unescape('addTab%3A'),
category: 'adding/Removing',
fn: function (aWidget) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_tabs", []), "_add_", [aWidget]);
    smalltalk.send(aWidget, "_appendToJQuery_", [smalltalk.send(unescape("%23jtalk"), "_asJQuery", [])]);
    smalltalk.send(aWidget, "_hide", []);
    return self;
},
args: ["aWidget"],
source: unescape('addTab%3A%20aWidget%0A%20%20%20%20self%20tabs%20add%3A%20aWidget.%0A%20%20%20%20aWidget%20appendToJQuery%3A%20%27%23jtalk%27%20asJQuery.%0A%20%20%20%20aWidget%20hide'),
messageSends: ["add:", "tabs", "appendToJQuery:", "asJQuery", "hide"],
referencedClasses: []
}),
smalltalk.TabManager);

smalltalk.addMethod(
unescape('_removeTab_'),
smalltalk.method({
selector: unescape('removeTab%3A'),
category: 'adding/Removing',
fn: function (aWidget) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_tabs", []), "_remove_", [aWidget]);
    smalltalk.send(self, "_update", []);
    return self;
},
args: ["aWidget"],
source: unescape('removeTab%3A%20aWidget%0A%20%20%20%20self%20tabs%20remove%3A%20aWidget.%0A%20%20%20%20self%20update'),
messageSends: ["remove:", "tabs", "update"],
referencedClasses: []
}),
smalltalk.TabManager);

smalltalk.addMethod(
unescape('_initialize'),
smalltalk.method({
selector: unescape('initialize'),
category: 'initialization',
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.Widget);
    self['@opened'] = true;
    smalltalk.send(function (html) {return smalltalk.send(smalltalk.send(html, "_div", []), "_id_", ["jtalk"]);}, "_appendToJQuery_", [smalltalk.send("body", "_asJQuery", [])]);
    smalltalk.send(smalltalk.send("body", "_asJQuery", []), "_addClass_", ["jtalkBody"]);
    smalltalk.send(self, "_appendToJQuery_", [smalltalk.send(unescape("%23jtalk"), "_asJQuery", [])]);
    (function ($rec) {smalltalk.send($rec, "_addTab_", [smalltalk.send(smalltalk.IDETranscript || IDETranscript, "_current", [])]);smalltalk.send($rec, "_addTab_", [smalltalk.send(smalltalk.Workspace || Workspace, "_new", [])]);return smalltalk.send($rec, "_addTab_", [smalltalk.send(smalltalk.TestRunner || TestRunner, "_new", [])]);}(self));
    smalltalk.send(self, "_selectTab_", [smalltalk.send(smalltalk.send(self, "_tabs", []), "_last", [])]);
    (function ($rec) {smalltalk.send($rec, "_onResize_", [function () {return function ($rec) {smalltalk.send($rec, "_updateBodyMargin", []);return smalltalk.send($rec, "_updatePosition", []);}(self);}]);return smalltalk.send($rec, "_onWindowResize_", [function () {return smalltalk.send(self, "_updatePosition", []);}]);}(self));
    return self;
},
args: [],
source: unescape('initialize%0A%20%20%20%20super%20initialize.%0A%20%20%20%20opened%20%3A%3D%20true.%0A%20%20%20%20%5B%3Ahtml%20%7C%20html%20div%20id%3A%20%27jtalk%27%5D%20appendToJQuery%3A%20%27body%27%20asJQuery.%0A%20%20%20%20%27body%27%20asJQuery%20%0A%09addClass%3A%20%27jtalkBody%27.%0A%20%20%20%20self%20appendToJQuery%3A%20%27%23jtalk%27%20asJQuery.%0A%20%20%20%20self%20%0A%09addTab%3A%20IDETranscript%20current%3B%0A%09addTab%3A%20Workspace%20new%3B%0A%09addTab%3A%20TestRunner%20new.%0A%20%20%20%20self%20selectTab%3A%20self%20tabs%20last.%0A%20%20%20%20self%20%0A%09onResize%3A%20%5Bself%20updateBodyMargin%3B%20updatePosition%5D%3B%0A%09onWindowResize%3A%20%5Bself%20updatePosition%5D'),
messageSends: ["initialize", "appendToJQuery:", "id:", "div", "asJQuery", "addClass:", "addTab:", "current", "new", "selectTab:", "last", "tabs", "onResize:", "updateBodyMargin", "updatePosition", "onWindowResize:"],
referencedClasses: ["IDETranscript", "Workspace", "TestRunner"]
}),
smalltalk.TabManager);

smalltalk.addMethod(
unescape('_renderOn_'),
smalltalk.method({
selector: unescape('renderOn%3A'),
category: 'rendering',
fn: function (html) {
    var self = this;
    smalltalk.send(smalltalk.send(html, "_div", []), "_id_", ["logo"]);
    smalltalk.send(self, "_renderToolbarOn_", [html]);
    self['@ul'] = function ($rec) {smalltalk.send($rec, "_id_", ["jtalkTabs"]);re