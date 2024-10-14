Global Roads Open Access Data Set, Version 1 (gROADSv1)

February 7, 2013

DESCRIPTION

This archive contains the Global Roads Open Access Data Set, Version 1 (gROADSv1), developed under the auspices of the CODATA Global Roads Data Development Task Group. The data set combines the best available public domain roads data (i.e., “open data”) by country into a global roads coverage, using the UN Spatial Data Infrastructure Transport (UNSDI-T) version 2 as a common data model. Documentation and metadata for this data set are available on the SEDAC gROADSv1 web site:
http://sedac.ciesin.columbia.edu/data/set/groads-global-roads-open-access-v1


ACCESSING THE DATA

The data may be downloaded via the World Wide Web at http://sedac.ciesin.columbia.edu/data/set/groads-global-roads-open-access-v1

The data are available as compressed zipfiles of Esri file geodatabases (global and regional) or shapefiles (regional only). Downloaded files need to be uncompressed in a single folder using either WinZip (Windows file compression utility) or similar application before they can be accessed by your GIS software package. Users should expect an increase in the size of downloaded data after decompression. 

Extents of Regional tiles:					
					
Region		lonW	lonE	latN	latS
Africa		 -32	  72	  42	 -52
Americas	-180	 -32	  86	 -58
Asia (multipart polygon)					
AsiaWest	-180	-168.5	  75	  62
AsiaEast	  24	 180	  86	 -13
Europe		 -74	  48	  86	  27
OceaniaWest	-180	-128	  29	 -58
OceaniaEast	  90	 180	  29	 -58


The data are stored in geographic coordinates of decimal degrees based on the World Geodetic System spheroid of 1984 (WGS84).

Users should consult the gROADSv1 documentation for complete information on the data set.


CODEBOOK

gROADSv1 used the UN Spatial Data Infrastructure Transport (UNSDI-T) version 2 as a common data model. The following are the field codes and full names, with sample coding. It should be mentioned that owing to the variability in source data, many of the fields could not be properly coded.

UNSDITv2
Field		Full Name				Coding

OBJECTID		
Shape		
SourceID	Source ID	
Exs		Existence Category			1=Definite, 2=Doubtful, 0=Unspecified
Notes		Notes	
RoadID		Road ID	
ONme		Official Road Name	
RteNme		Route Name	
NtlClass	National Inventory Road Class	
FClass		Functional Class			1=Highway, 2=Primary, 3=Secondary, 4=Tertiary, 5=Local/ Urban, 6=Trail, 7=Private, 0=Unspecified
Crgway		Carriageways 				1=Single, 2=Dual, 0=Unspecified
NumLanes	Number of lanes	
LneWidthM	Lane Width (m)	
RdWidthM	Road Width (m)	
AxleLoadMT	Maximum Axle Loading (MT)	
TotLoadMT	Maxium Total Loading (MT)	
SrfTpe		Surface Type				1=Paved, 2=Gravel, 3=Dirt/Sand, 4=Steel, 5=Wood, 6=Grass, 0=Unspecified
SrfCond		Surface Condition			1=Rough (<40kph), 2=Smooth (>40kph), 3=Snow/Ice, 4=Mud, 0=Unspecified
SrfPrep		Surface Preparation			1=Natural Compaction, 2=Traffic Compaction, 3=Engineered Compaction, 4=Uncompacted, 0=Unspecified
IsSeasonal	Affected by Season			1=Yes, 2=No, 0=Unspecified"
CurntPrac	Current Road Practicability		1=Non-motorized, 2=Motorbike, 3=4WD <3.5MT, 4=Light Truck <10MT, 5=Heavy Truck <20MT, 6=Truck + Trailer >20MT, 0=Unspecified
GdWthrPrac	Good Weather Road Practicability	1=Non-motorized, 2=Motorbike, 3=4WD <3.5MT, 4=Light Truck <10MT, 5=Heavy Truck <20MT, 6=Truck + Trailer >20MT, 0=Unspecified
BdWthrPrac	Bad Weather Road Practicability		1=Non-motorized, 2=Motorbike, 3=4WD <3.5MT, 4=Light Truck <10MT, 5=Heavy Truck <20MT, 6=Truck + Trailer >20MT, 0=Unspecified
SpeedLimit	Speed Limit (Km/hr)	
CurntSpeed	Current Average Speed	
GnralSpeed	General Average Speed	
IsUndrCstr	Is under Construction / Repairs		1=Yes, 2=No, 0=Unspecified
CstWrkETC	Construction Work Est Completion Date	
GradDeg		Gradient (degrees)	
Sec		Road Security Category			1=Category A (low risk), 2=Category B (low to medium risk), 3=Category C (medium to high risk), 4=Category D (high risk), 5=Category E (critical risk), 0=Unspecified
HasShouldr	Has Shoulder				1=Yes, 2=No, 0=Unspecified
HasSidewalk	Has Sidewalk				1=Yes, 2=No, 0=Unspecified
DrivSide	Driving Side				1=Left, 2=Right, 0=Unspecified
IsElevated	Is elevated/suspended above ground or water surface		1=Yes, 2=No, 0=Unspecified
HasMedian	Has Median				1=Yes, 2=No, 0=Unspecified
OpStatus	Operational Status			1=Open, 2=Restricted, 3=Closed, 4=Abandoned/Disused, 0=Unspecified
Shape_Length	Length of segment	
Length_KM	Length of segment in kilometers


CREDIT AND USE CONSTRAINTS

Users should consult the gROADSv1 documentation for information on additional credit and use restrictions for the following countries: Andorra, Belize, Canada, Costa Rica, Gibraltar, Guatemala, Honduras, Japan, Jersey Island, Nicaragua, Panama, Pitcairn Island, Seychelles, Tuvalu, United Kingdom, and United States.


DATA ERRORS, CORRECTIONS AND QUALITY ASSESSMENT

CIESIN follows procedures designed to ensure that data disseminated by CIESIN are of reasonable quality. If, despite these procedures, users encounter apparent errors or misstatements in the data, they should contact us at ciesin.info@ciesin.columbia.edu. 


NO WARRANTY OR LIABILITY

CIESIN and the creators provide these data without any warranty of any kind whatsoever either express or implied. Neither CIESIN nor the creators shall be liable for incidental, consequential, or special damages arising out of the use of any downloaded data.


CITATION

The recommended citation for this dataset is:

Center for International Earth Science Information Network (CIESIN)/Columbia University, and Information Technology Outreach Services (ITOS)/University of Georgia. 2013. Global Roads Open Access Data Set, Version 1 (gROADSv1). Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). Downloaded from http://sedac.ciesin.columbia.edu/data/set/groads-global-roads-open-access-v1 [DATE DOWNLOADED]



