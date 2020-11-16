<?xml version="1.0"?>

<st-source>
<time-stamp>From VisualWorks® NonCommercial, 7.6 of lundi 3 mars 2008 on mercredi 23 septembre 2020 at 4:00:19</time-stamp>


<methods>
<class-id>CormasNS.Kernel.CormasWS</class-id> <category>web-service</category>

<body package="Cormas" selector="getProbe:ofClass:">getProbe: probeName ofClass: className
	
	&lt;operationName: #GetProbeOfClass&gt;
	&lt;addParameter: #probName type: #ByteSymbol&gt;
	&lt;addParameter: #className type: #ByteSymbol&gt;
	&lt;result: #(#Collection #Float)&gt;
	
	| res datas flatDatas |
	(self myCormas cormasModel data at: className)
		at: probeName
		ifAbsent:
			[res := (self myCormas cormasModel class classFromSymbol: className)
				perform: probeName.
			(res isKindOf: Collection)
				ifTrue: [^OrderedCollection withAll: res]
				ifFalse: [^OrderedCollection with: res]].
	datas := ((self myCormas cormasModel data at: className) at: probeName)
		asOrderedCollection.
	datas first class name = #OrderedCollection
		ifTrue:
			[flatDatas := OrderedCollection new.
			datas do: [:collec | flatDatas addAll: collec].
			^flatDatas]
		ifFalse: [^datas]</body>
</methods>

</st-source>
