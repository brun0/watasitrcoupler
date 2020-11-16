<?xml version="1.0"?>

<st-source>
<time-stamp>From VisualWorks® NonCommercial, 7.6 of lundi 3 mars 2008 on jeudi 1 octobre 2020 at 3:55:10</time-stamp>


<methods>
<class-id>CormasNS.Kernel.CormasWS</class-id> <category>web-service</category>

<body package="Cormas" selector="sayHello">sayHello
&lt;operationName: #SayHello &gt;
	&lt;result: #String &gt;
	self myCormas useExternalVisualization: false.
	self myCormas exportPlaybackLog: false.
	self myCormas simManager enableBackward: false.
	self myCormas cormasModel class setActiveProbes: self activeProbes.
	self activeProbes: nil.
	self myCormas cormasModel initDataStructures.
	self myCormas simManager enableBackward: false.
	self myCormas cormasModel initSimulation.
^ 'hello!'</body>
</methods>

</st-source>
