Object subclass: #HTTPRequest
	instanceVariableNames: ''
	category: 'Remote'!

!HTTPRequest methodsFor: 'not yet classified'!

post
| result |
 jQuery 
			ajax: 'http://localhost:4000/bla'
			options: #{
				'type' -> 'POST'.
				'success' -> [ :tmp | window alert: 'success ', tmp].
				'error' -> [window alert: 'error'].
				"'dataType' -> 'jsonp'."
				'data' -> 'Moin Moin'
			}.
! !

