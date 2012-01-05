Smalltalk current createPackage: 'Examples' properties: #{}!
Object subclass: #JavaCall
	instanceVariableNames: ''
	category: 'Examples'!

!JavaCall methodsFor: 'not yet classified'!

initialize
super initialize
! !

!JavaCall class methodsFor: 'not yet classified'!

main
| frame textArea scrollPane mainPanel buttonPanel buttonDoit buttonPrintit aNull borderLayout |
<importPackage (javax.swing)>.
<importPackage (java.awt)>.
<importClass(java.awt.Window)>.

<frame = new JFrame('a Workspace');>.
<borderLayout = new BorderLayout();>.
textArea := JTextArea new.
textArea initialize.

<mainPanel = new JPanel();>.
<buttonPanel = new JPanel();>.

<buttonDoit = new JButton('do it');>.
<buttonPrintit = new JButton('print it');>.

<scrollPane = new JScrollPane()>.

frame setDefaultCloseOperation: 3. "JFrame.EXIT_ON_CLOSE"

buttonPrintit addActionListener: [ 
	| result pos |
	result := self eval: (textArea javaObject getSelectedText).
	pos := textArea javaObject getSelectionEnd.
	textArea insert: result pos: pos.
	textArea select: pos size: (result asString size).
].

buttonDoit addActionListener: [ 
	self eval: (textArea javaObject getSelectedText).
].

<aNull = null>.
frame setLocationRelativeTo: aNull.
frame setLayout: borderLayout.
scrollPane setPreferredSize: (self dimension: #(400 300)).
scrollPane setViewportView: (textArea javaObject).
scrollPane setVerticalScrollBarPolicy: 20. "VERTICAL_SCROLLBAR_AS_NEEDED = 20;" 
mainPanel add: scrollPane.
				
buttonPanel add: buttonDoit.
buttonPanel add: buttonPrintit.

self frame: frame add: mainPanel orientation: 'Center'.
self frame: frame add: buttonPanel orientation: 'South'.
frame pack.
frame setVisible: true.

^'Return from Amber'
!

dimension: width height: height
| dim |
<dim = new Dimension(width, height)>.
^dim
!

dimension: col
| dim w h |
w := col at: 1.
h := col at: 2.
<dim = new Dimension(w, h)>.
^dim
!

messageBox: aString
<importPackage (javax.swing)>.
<JOptionPane.showMessageDialog(null,  aString)>.
!

eval: aString
| compiler compiled cm |
compiler := Compiler new.
[compiler parseExpression: aString] on: Error do: [:ex |
	^self messageBox: ex messageText].
^(compiler load: 'doIt ^[', aString, '] value' forClass: DoIt) fn value
!

frame: aFrame add: aComponent orientation: anOrientation
<aFrame.add(aComponent, anOrientation)>.
! !

Object subclass: #MessageBox
	instanceVariableNames: 'joptionspane'
	category: 'Examples'!

!MessageBox class methodsFor: 'not yet classified'!

initialize
<importPackage (javax.swing)>.
! !

Object subclass: #JTextArea
	instanceVariableNames: 'textArea'
	category: 'Examples'!

!JTextArea methodsFor: 'not yet classified'!

javaObject
^textArea
!

insert: aString pos: aPos
| ta |
ta := self javaObject.
<ta.insert(aString, aPos)>.
!

initialize
super initialize.
textArea := <new JTextArea()>.
!

select: start size: size
| ta end |
ta := self javaObject.
end := start + size.
<ta.select(start, end)>.
ta requestFocus.
! !

