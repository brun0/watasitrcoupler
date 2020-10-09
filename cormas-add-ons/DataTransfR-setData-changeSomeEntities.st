<?xml version="1.0"?>

<st-source>
<time-stamp>From VisualWorks® NonCommercial, 7.6 of lundi 3 mars 2008 on vendredi 9 octobre 2020 at 0:03:05</time-stamp>


<methods>
<class-id>CormasNS.Kernel.DataTransfR</class-id> <category>transfer</category>

<body package="Cormas" selector="setData">setData
	
	| sortedEntities nb args |
	sortedEntities := (self myCormas cormasModel
		perform: ('the' , self entityType , 's') asSymbol) asOrderedCollection
		asSortedCollection: [:e1 :e2 | e1 idRTransfer &lt; e2 idRTransfer].
	nb := 1.
	sortedEntities
		do:
			[:entity | 
			entity idRTransfer = (self ids at: nb)
				ifTrue:
					[args := Array new: 1.
					args
						at: 1
						put: (self values at: nb).
					entity
						perform: (self attribute , ':') asSymbol
						withArguments: args.
					nb := nb + 1.
					nb &gt; self ids size ifTrue: [^self]]
				ifFalse: [entity idRTransfer &gt; (self ids at: nb) ifTrue: [self halt]]]</body>
</methods>

</st-source>
