<?xml version="1.0"?>

<st-source>
<time-stamp>From VisualWorks® NonCommercial, 7.6 of lundi 3 mars 2008 on lundi 25 mars 2019 at 7:28:49</time-stamp>


<methods>
<class-id>CormasNS.Kernel.CormasWS</class-id> <category>web-service</category>

<body package="Cormas" selector="runSimu:">runSimu: duration
&lt;operationName: #RunSimu &gt;
	&lt;addParameter: #duration type: #SmallInteger &gt;
	&lt;result: #Boolean &gt;
	duration timesRepeat:[self myCormas cormasModel runStep].
^true</body>
</methods>

</st-source>
