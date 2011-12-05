Smalltalk current createPackage: 'IDE' properties: #{}!
Widget subclass: #TabManager
	instanceVariableNames: 'selectedTab tabs opened ul input'
	category: 'IDE'!

!TabManager methodsFor: 'accessing'!

tabs
    ^tabs ifNil: [tabs := Array new]
!

labelFor: aWidget
	| label maxSize |
	maxSize := 15.
	label := aWidget label copyFrom: 0 to: (aWidget label size min: maxSize).
	aWidget label size > maxSize ifTrue: [
		label := label, '...'].
	^label
! !

!TabManager methodsFor: 'actions'!

updateBodyMargin
    self setBodyMargin: '#jtalk' asJQuery height
!

updatePosition
    <jQuery('#jtalk').css('top', '').css('bottom', '0px')>
!

removeBodyMargin
    self setBodyMargin: 0
!

setBodyMargin: anInteger
    '.jtalkBody' asJQuery css: 'margin-bottom' put: anInteger asString, 'px'
!

onResize: aBlock
    <jQuery('#jtalk').resizable({
	handles: 'n', 
	resize: aBlock,
	minHeight: 230
})>
!

onWindowResize: aBlock
    <jQuery(window).resize(aBlock)>
!

open
    opened ifFalse: [
	'body' asJQuery addClass: 'jtalkBody'.
	'#jtalk' asJQuery show.
	ul asJQuery show.
	self updateBodyMargin.
	selectedTab show.
	opened := true]
!

close
    opened ifTrue: [
	'#jtalk' asJQuery hide.
	ul asJQuery hide.
	selectedTab hide.
	self removeBodyMargin.
	'body' asJQuery removeClass: 'jtalkBody'.
	opened := false]
!

newBrowserTab
    Browser open
!

selectTab: aWidget
    self open.
    selectedTab := aWidget.
    self tabs do: [:each |
	each hide].
    aWidget show.
	
    self update
!

closeTab: aWidget
    self removeTab: aWidget.
    self selectTab: self tabs last.
    aWidget remove.
    self update
!

search: aString
	| searchedClass |
	searchedClass := Smalltalk current at: aString.
		searchedClass isClass
			ifTrue: [Browser openOn: searchedClass]
			ifFalse: [ReferencesBrowser search: aString]
! !

!TabManager methodsFor: 'adding/Removing'!

addTab: aWidget
    self tabs add: aWidget.
    aWidget appendToJQuery: '#jtalk' asJQuery.
    aWidget hide
!

removeTab: aWidget
    self tabs remove: aWidget.
    self update
! !

!TabManager methodsFor: 'initialization'!

initialize
    super initialize.
    opened := true.
    [:html | html div id: 'jtalk'] appendToJQuery: 'body' asJQuery.
    'body' asJQuery 
	addClass: 'jtalkBody'.
    self appendToJQuery: '#jtalk' asJQuery.
    self 
	addTab: IDETranscript current;
	addTab: Workspace new;
	addTab: TestRunner new.
    self selectTab: self tabs last.
    self 
	onResize: [self updateBodyMargin; updatePosition];
	onWindowResize: [self updatePosition]
! !

!TabManager methodsFor: 'rendering'!

renderOn: html
	html div id: 'logo'.
	self renderToolbarOn: html.
	ul := html ul
		id: 'jtalkTabs';
		yourself.
	self renderTabs
!

renderTabFor: aWidget on: html
	| li |
	li := html li.
	selectedTab = aWidget ifTrue: [
	li class: 'selected'].
	li with: [
		html span class: 'ltab'.
		html span
			class: 'mtab';
			with: [
				aWidget canBeClosed ifTrue: [
					html span 
						class: 'close';
						with: 'x';
					onClick: [self closeTab: aWidget]].
			html span with: (self labelFor: aWidget)].
		html span class: 'rtab'];
	onClick: [self selectTab: aWidget]
!

renderTabs
	ul contents: [:html |
	    self tabs do: [:each |
		self renderTabFor: each on: html].
	    html li
		class: 'newtab';
		with: [
			html span class: 'ltab'.
			html span class: 'mtab'; with: ' + '.
			html span class: 'rtab'];
		onClick: [self newBrowserTab]]
!

renderToolbarOn: html
	html div 
		id: 'jt_toolbar';
		with: [
			input := html input 
				class: 'implementors';
				yourself.
			input onKeyPress: [:event |
				event keyCode = 13 ifTrue: [
				self search: input asJQuery val]].
			html div id: 'jt_close'; onClick: [self close]]
! !

!TabManager methodsFor: 'updating'!

update
	self renderTabs
! !

TabManager class instanceVariableNames: 'current'!

!TabManager class methodsFor: 'instance creation'!

current
    ^current ifNil: [current := super new]
!

new
    self shouldNotImplement
! !

Widget subclass: #TabWidget
	instanceVariableNames: 'div'
	category: 'IDE'!

!TabWidget methodsFor: 'accessing'!

label
    self subclassResponsibility
! !

!TabWidget methodsFor: 'actions'!

open
    TabManager current addTab: self.
    TabManager current selectTab: self
!

show
	div asJQuery show
!

hide
	div asJQuery hide
!

remove
	div asJQuery remove
!

close
    TabManager current closeTab: self
! !

!TabWidget methodsFor: 'rendering'!

renderOn: html
	div := html div
		class: 'jtalkTool';
		yourself.
	self renderTab
!

renderBoxOn: html
!

renderButtonsOn: html
!

update
	self renderTab
!

renderTab
	div contents: [:html |
	    html div
		class: 'jt_box';
		with: [self renderBoxOn: html].
	    html div
		class: 'jt_buttons';
		with: [self renderButtonsOn: html]]
! !

!TabWidget methodsFor: 'testing'!

canBeClosed
    ^false
! !

!TabWidget class methodsFor: 'instance creation'!

open
    ^self new open
! !

TabWidget subclass: #Workspace
	instanceVariableNames: 'sourceArea'
	category: 'IDE'!

!Workspace methodsFor: 'accessing'!

label
    ^'Workspace'
! !

!Workspace methodsFor: 'actions'!

clearWorkspace
    sourceArea clear
!

doIt
   sourceArea doIt
!

printIt
	sourceArea printIt
!

inspectIt
    sourceArea inspectIt
!

fileIn
    sourceArea fileIn
! !

!Workspace methodsFor: 'rendering'!

renderBoxOn: html
    sourceArea := SourceArea new.
    sourceArea renderOn: html
!

renderButtonsOn: html
    html button
	with: 'DoIt';
	title: 'ctrl+d';
	onClick: [self doIt].
    html button
	with: 'PrintIt';
	title: 'ctrl+p';
	onClick: [self printIt].
    html button
	with: 'InspectIt';
	title: 'ctrl+i';
	onClick: [self inspectIt].
    html button
	with: 'FileIn';
	title: 'ctrl+f';
	onClick: [self fileIn].
    html button
	with: 'Clear workspace';
	onClick: [self clearWorkspace]
! !

TabWidget subclass: #Browser
	instanceVariableNames: 'selectedPackage selectedClass selectedProtocol selectedMethod packagesList classesList protocolsList methodsList sourceArea tabsList selectedTab saveButton classButtons methodButtons unsavedChanges'
	category: 'IDE'!

!Browser methodsFor: 'accessing'!

label
    ^selectedClass 
	ifNil: ['Browser (nil)']
	ifNotNil: ['Browser: ', selectedClass name]
!

packages
    | packages |
    packages := Array new.
    Smalltalk current classes do: [:each |
	(packages includes: each category) ifFalse: [
	    packages add: each category]].
    ^packages sort
!

classes
    ^(Smalltalk current classes 
	select: [:each | each category = selectedPackage])
	sort: [:a :b | a name < b name]
!

protocols
    | klass |
    selectedClass ifNotNil: [
	selectedTab = #comment ifTrue: [^#()].
	klass := selectedTab = #instance
	    ifTrue: [selectedClass]
	    ifFalse: [selectedClass class].
	klass methodDictionary isEmpty ifTrue: [
	    ^Array with: 'not yet classified'].
	^klass protocols].
    ^Array new
!

methods
    | klass |
    selectedTab = #comment ifTrue: [^#()].
    selectedClass ifNotNil: [
	klass := selectedTab = #instance
	    ifTrue: [selectedClass]
	    ifFalse: [selectedClass class]].
    ^(selectedProtocol 
	ifNil: [
	    klass 
		ifNil: [#()] 
		ifNotNil: [klass methodDictionary values]]
	ifNotNil: [
	    klass methodDictionary values select: [:each |
		each category = selectedProtocol]]) sort: [:a :b | a selector < b selector]
!

source
    selectedTab = #comment ifFalse: [
	^(selectedProtocol notNil or: [selectedMethod notNil])
	    ifFalse: [self declarationSource]
	    ifTrue: [self methodSource]].
    ^selectedClass
	ifNil: ['']
	ifNotNil: [self classCommentSource]
!

methodSource
    ^selectedMethod
	ifNil: [self dummyMethodSource]
	ifNotNil: [selectedMethod source]
!

dummyMethodSource
    ^'messageSelectorAndArgumentNames
	"comment stating purpose of message"

	| temporary variable names |
	statements'
!

declarationSource
    ^selectedTab = #instance
	ifTrue: [self classDeclarationSource]
	ifFalse: [self metaclassDeclarationSource]
!

classDeclarationSource
	| stream |
	stream := '' writeStream.
	selectedClass ifNil: [^self classDeclarationTemplate].
	stream 
	    nextPutAll: selectedClass superclass asString;
	    nextPutAll: ' subclass: #';
	    nextPutAll: selectedClass name;
	    nextPutAll: String lf, String tab;
	    nextPutAll: 'instanceVariableNames: '''.
	selectedClass instanceVariableNames 
	    do: [:each | stream nextPutAll: each] 
	    separatedBy: [stream nextPutAll: ' '].
	stream
	    nextPutAll: '''', String lf, String tab;
	    nextPutAll: 'package: ''';
	    nextPutAll: selectedClass category;
	    nextPutAll: ''''.
	^stream contents
!

metaclassDeclarationSource
    | stream |
    stream := '' writeStream.
    selectedClass ifNotNil: [
	stream 
	    nextPutAll: selectedClass asString;
	    nextPutAll: ' class ';
	    nextPutAll: 'instanceVariableNames: '''.
	selectedClass class instanceVariableNames
	    do: [:each | stream nextPutAll: each]
	    separatedBy: [stream nextPutAll: ' '].
	stream nextPutAll: ''''].
    ^stream contents
!

classCommentSource
    ^selectedClass comment
!

selectedClass
	^selectedClass
!

classDeclarationTemplate
	^'Object subclass: #NameOfSubclass
	instanceVariableNames: ''''
	package: ''', self selectedPackage, ''''
!

selectedPackage
	^selectedPackage
!

reloadOnServer
selectedPackage ifNotNil: [
  jQuery 
			ajax: 'FileServer?reload:'
			options: #{
				'type' -> 'POST'.
				'success' -> [ :message | window alert: message].
				'error' -> [window alert: 'error'].
				"'dataType' -> 'jsonp'."
				'data' -> selectedPackage
			}.
  ]
! !

!Browser methodsFor: 'actions'!

disableSaveButton
    saveButton ifNotNil: [
	saveButton at: 'disabled' put: true].
    unsavedChanges := false
!

hideClassButtons
    classButtons asJQuery hide
!

showClassButtons
    classButtons asJQuery show
!

hideMethodButtons
    methodButtons asJQuery hide
!

showMethodButtons
    methodButtons asJQuery show
!

compile
    self disableSaveButton.
    selectedTab = #comment 
	ifTrue: [
			selectedClass ifNotNil: [
				self compileClassComment]]
	ifFalse: [
			(selectedProtocol notNil or: [selectedMethod notNil])
				ifFalse: [self compileDefinition]
				ifTrue: [self compileMethodDefinition]]
!

compileClassComment
    selectedClass comment: sourceArea val
!

compileMethodDefinition
    selectedTab = #instance
	ifTrue: [self compileMethodDefinitionFor: selectedClass]
	ifFalse: [self compileMethodDefinitionFor: selectedClass class]
!

compileMethodDefinitionFor: aClass
    | compiler method source node | 
    source := sourceArea val.
    selectedProtocol ifNil: [selectedProtocol := selectedMethod category].
    compiler := Compiler new.
    compiler source: source.
    node := compiler parse: source.
    node isParseFailure ifTrue: [
	^window alert: 'PARSE ERROR: ', node reason, ', position: ', node position asString].
    compiler currentClass: aClass.
    method := compiler eval: (compiler compileNode: node).
    method category: selectedProtocol.
    compiler unknownVariables do: [:each |
         "Do not try to redeclare javascript's objects"
         (window at: each) ifNil: [
	 	(window confirm: 'Declare ''', each, ''' as instance variable?') ifTrue: [
			self addInstanceVariableNamed: each toClass: aClass.
			^self compileMethodDefinitionFor: aClass]]].
    aClass addCompiledMethod: method.
    compiler setupClass: aClass.
    self updateMethodsList.
    self selectMethod: method
!

compileDefinition
    | newClass |
    newClass := Compiler new loadExpression: sourceArea val.
    self 
	resetClassesList;
	updateCategoriesList;
	updateClassesList.
    self selectClass: newClass
!

cancelChanges
    ^unsavedChanges 
	ifTrue: [window confirm: 'Cancel changes?']
	ifFalse: [true]
!

removeClass
    (window confirm: 'Do you really want to remove ', selectedClass name, '?')
	ifTrue: [
	    Smalltalk current removeClass: selectedClass.
	    self resetClassesList.
	    self selectClass: nil]
!

removeMethod
    self cancelChanges ifTrue: [
	(window confirm: 'Do you really want to remove #', selectedMethod selector, '?')
	    ifTrue: [
		selectedTab = #instance 
			ifTrue: [selectedClass removeCompiledMethod: selectedMethod]
			ifFalse: [selectedClass class removeCompiledMethod: selectedMethod].
		self selectMethod: nil]]
!

setMethodProtocol: aString
    self cancelChanges ifTrue: [
	(self protocols includes: aString)
	    ifFalse: [self addNewProtocol]
	    ifTrue: [
		selectedMethod category: aString.
		selectedProtocol := aString.
		selectedMethod := selectedMethod.
		self 
		    updateProtocolsList;
		    updateMethodsList;
		    updateSourceAndButtons]]
!

addNewProtocol
    | newProtocol |
    newProtocol := window prompt: 'New method protocol'.
    (newProtocol notNil and: [newProtocol notEmpty]) ifTrue: [
	selectedMethod category: newProtocol.
	self setMethodProtocol: newProtocol]
!

selectCategory: aCategory
    self cancelChanges ifTrue: [
	selectedPackage := aCategory.
	selectedClass := selectedProtocol := selectedMethod :=  nil.
	self resetClassesList.
	self 
	    updateCategoriesList;
	    updateClassesList;
	    updateProtocolsList;
	    updateMethodsList;
	    updateSourceAndButtons]
!

selectClass: aClass
    self cancelChanges ifTrue: [
	selectedClass := aClass.
	selectedProtocol := selectedMethod := nil.
	self 
	    updateClassesList;
	    updateProtocolsList;
	    updateMethodsList;
	    updateSourceAndButtons]
!

selectProtocol: aString
    self cancelChanges ifTrue: [
	selectedProtocol := aString.
	selectedMethod := nil.
	self 
	    updateProtocolsList;
	    updateMethodsList;
	    updateSourceAndButtons]
!

selectMethod: aMethod
    self cancelChanges ifTrue: [
	selectedMethod := aMethod.
	self 
	    updateProtocolsList;
	    updateMethodsList;
	    updateSourceAndButtons]
!

selectTab: aString
    self cancelChanges ifTrue: [
	selectedTab := aString.
	self selectProtocol: nil.
	self updateTabsList]
!

renameClass
    | newName |
    newName := window prompt: 'Rename class ', selectedClass name.
    (newName notNil and: [newName notEmpty]) ifTrue: [
	selectedClass rename: newName.
	self 
		updateClassesList;
		updateSourceAndButtons]
!

addInstanceVariableNamed: aString toClass: aClass
	ClassBuilder new
		addSubclassOf: aClass superclass 
		named: aClass name 
		instanceVariableNames: (aClass instanceVariableNames copy add: aString; yourself)
		package: aClass package name
!

searchReferencesOf: aString
	ReferencesBrowser search: aString
!

searchClassReferences
	ReferencesBrowser search: selectedClass name
!

search: aString
	self cancelChanges ifTrue: [| searchedClass |
		searchedClass := Smalltalk current at: aString.
		searchedClass isClass
			ifTrue: [self class openOn: searchedClass]
			ifFalse: [self searchReferencesOf: aString]]
!

handleSourceAreaKeyDown: anEvent
	 <if(anEvent.ctrlKey) {
		if(anEvent.keyCode === 83) { //ctrl+s
			self._compile();
			anEvent.preventDefault();
			return false;
		}
	}
	>
!

commitPackage
	selectedPackage ifNotNil: [
		jQuery 
			ajax: self class commitPathJs, '/', selectedPackage, '.js'
			options: #{
				'type' -> 'PUT'.
				'data' -> (Exporter new exportPackage: selectedPackage).
				'error' -> [ window alert: 'Commit failed!!']
			}.
		jQuery 
			ajax: self class commitPathJs, '/', selectedPackage, '.deploy.js'
			options: #{
				'type' -> 'PUT'.
				'data' -> (StrippedExporter new exportPackage: selectedPackage).
				'error' -> [window alert: 'Commit failed!!']
			}.
		jQuery 
			ajax: self class commitPathSt, '/', selectedPackage, '.st'
			options: #{
				'type' -> 'PUT'.
				'data' -> (ChunkExporter new exportPackage: selectedPackage).
				'error' -> [window alert: 'Commit failed!!']
			}]
!

renamePackage

  | newName |
  newName := window prompt: 'Rename package ', selectedPackage.
  newName ifNotNil: [
    newName notEmpty ifTrue: [
	Smalltalk current renamePacka