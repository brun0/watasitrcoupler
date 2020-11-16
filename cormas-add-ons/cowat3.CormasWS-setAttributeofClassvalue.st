<?xml version="1.0"?>

<st-source>
<time-stamp>From VisualWorks® NonCommercial, 7.6 of lundi 3 mars 2008 on mardi 15 octobre 2019 at 6:45:34</time-stamp>


<methods>
<class-id>CormasNS.Kernel.CormasWS</class-id> <category>web-service</category>

<body package="Cormas" selector="setAttribute:ofClass:value:">setAttribute: attName ofClass: aClassName value: aValue
	
	| notThere aCAV aClass transferer|
	"aClassName = symbol du nom de la class"
	"self setAttribute: 'infectiousPeriod' ofClass: Host value: 40"
	&lt;operationName: #SetAttributeOfClassValue &gt;
	&lt;addParameter: #attName type: #String &gt;
	&lt;addParameter: #className type: #ByteString &gt;
	&lt;addParameter: #value type: #Float &gt;
	&lt;result: #String &gt; 
	"In data transfer mode, all data is sent in the attName argument since it is a string. aValue argument is not used"
	aClassName asSymbol = #DataTransfRSet ifTrue: [
	"exemple: self setAttribute: 'Efarmer;age;1,2,3,4,5,6,7,8,9,10;20,20,20,20,20,50,50,50,50,50' ofClass: 'DataTransfRSet' value: 0"
	transferer := DataTransfR newData: attName cormas: myCormas.
	transferer setData.
	^'done'].
	aClassName asSymbol = #DataTransfRGet ifTrue: [
	"example: self setAttribute: 'Efarmer;age; ; ' ofClass: 'DataTransfRGet' value: 0"
	transferer := DataTransfR newData: attName cormas: myCormas.
	^transferer getData].
	notThere := true.
	aClass:=  myCormas cormasModel classFromSymbol: aClassName .
	(DefaultAttributesSetter attributsFromClass: aClass)
		do: [:n | n = attName ifTrue: [notThere := false]].
	notThere ifTrue: [^attName , 'is not attribute of class ' , aClass name].
	aCAV := ClassAttributeValue newClass: aClass attribute: attName value:
			aValue.
	self myCormas cormasModel defaultAttributesSetter applyNewValue: aCAV.
	^'done'</body>
</methods>

</st-source>
