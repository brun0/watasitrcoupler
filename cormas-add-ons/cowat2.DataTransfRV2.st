<?xml version="1.0"?>

<st-source>
<time-stamp>From VisualWorks® NonCommercial, 7.6 of lundi 3 mars 2008 on jeudi 28 mai 2020 at 8:26:42</time-stamp>


<class>
<name>DataTransfR</name>
<environment>CormasNS.Kernel</environment>
<super>Core.Object</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>myCormas entityType attribute ids values </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category></category>
<attributes>
<package>Cormas</package>
</attributes>
</class>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>CormasNS.Kernel.DataTransfR class</class-id> <category>instance creation</category>

<body package="Cormas" selector="new">new
	"Answer a newly created and initialized instance."

	^super new initialize</body>

<body package="Cormas" selector="newData:cormas:">newData: classAttIdsValues cormas: aCormasInstance
	"Answer a newly created and initialized instance."
| newBe idsValues data|
	newBe:= self new.
	newBe myCormas: aCormasInstance.
	data := classAttIdsValues asArrayOfSubstringsSeparatedBy: $;.
	newBe entityType: data first.
	newBe attribute: (data at: 2).
	idsValues:= (data at: 3)  asArrayOfSubstringsSeparatedBy: $,.
	idsValues size &gt; 0 ifTrue: [
		newBe ids: (idsValues asOrderedCollection collect: [: string | string  asInteger]).
		newBe values: (	( (data at: 4)  asArrayOfSubstringsSeparatedBy: $,) asOrderedCollection collect: [: string | string asNumber asFloat]).
	].
 ^newBe</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>CormasNS.Kernel.DataTransfR</class-id> <category>initialize-release</category>

<body package="Cormas" selector="initialize">initialize
	"Initialize a newly created instance. This method must answer the receiver."

	" *** Replace this comment with the appropriate initialization code *** "
	^self</body>
</methods>

<methods>
<class-id>CormasNS.Kernel.DataTransfR</class-id> <category>accessing</category>

<body package="Cormas" selector="attribute">attribute
	^attribute</body>

<body package="Cormas" selector="attribute:">attribute: anObject
	attribute := anObject</body>

<body package="Cormas" selector="entityType">entityType
	^entityType</body>

<body package="Cormas" selector="entityType:">entityType: anObject
	entityType := anObject</body>

<body package="Cormas" selector="ids">ids
	^ids</body>

<body package="Cormas" selector="ids:">ids: anObject
	ids := anObject</body>

<body package="Cormas" selector="myCormas">myCormas
	^myCormas</body>

<body package="Cormas" selector="myCormas:">myCormas: anObject
	myCormas := anObject</body>

<body package="Cormas" selector="values">values
	^values</body>

<body package="Cormas" selector="values:">values: anObject
	values := anObject</body>
</methods>

<methods>
<class-id>CormasNS.Kernel.DataTransfR</class-id> <category>transfer</category>

<body package="Cormas" selector="getData">getData
	
	| sortedEntities |
	sortedEntities := (self myCormas cormasModel
		perform: ('the' , self entityType , 's') asSymbol) asOrderedCollection
		asSortedCollection: [:e1 :e2 | e1 idRTransfer &lt; e2 idRTransfer].
	self ids: ''.
	self values: ''.
	sortedEntities
		do:
			[:entity | 
			self ids: self ids, entity idRTransfer asString , ','.
			self values: self values, (entity perform: self attribute asSymbol) asString, ','].
^self ids,';', self values</body>

<body package="Cormas" selector="setData">setData
	
	| sortedEntities setedEntity nb args |
	sortedEntities := Dictionary new.
	(self myCormas cormasModel
		perform: ('the' , self entityType , 's') asSymbol) asOrderedCollection
		do:
			[:e | 
			sortedEntities
				at: e idRTransfer
				put: e].
	nb := 1.
	self ids
		do:
			[:idRT | 
			setedEntity := sortedEntities
				at: idRT
				ifAbsent: [self halt].
			args := Array new: 1.
			args
				at: 1
				put: (self values at: nb).
			setedEntity
				perform: (self attribute , ':') asSymbol
				withArguments: args.
			nb := nb + 1]</body>
</methods>

</st-source>
