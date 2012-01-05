Smalltalk current createPackage: 'AmberServer' properties: #{}!
Object subclass: #AmberClient
	instanceVariableNames: ''
	category: 'AmberServer'!

!AmberClient class methodsFor: 'not yet classified'!

createProgram: categoriesOC main: mainClassName filename: aTargetFilename onSuccess: onSuccessBlock onError: onErrorBlock

jQuery 
			ajax: ('createProgram?',mainClassName, '?', aTargetFilename)
			options: #{
				'type' -> 'POST'.
				'success' -> onSuccessBlock.
				'data' -> categoriesOC asJSON.
				'error' -> onErrorBlock
			}.
!

createExample
| oc |
oc := OrderedCollection new.
oc add: 'Examples'.

AmberClient createProgram: oc main: 'JavaCall' filename: 'java' onSuccess: [] onError: []
! !

