Groundwater Conditions Map

The goal is to develop a contour map of groundwater level monthly percentiles, which can be updated easily on a daily or weekly basis. We will use the same method to calculate percentiles that USGS uses for groundwater levels. The percentile tables can be updated every year or two. We will bring together well level data from the USGS network and the DNR real-time network, and also baseflow levels calculated for selected gages in the USGS stream flow monitoring network. We will use the same method for baseflow separation that North Carolina uses for their groundwater contour maps. Baseflow and well levels from Georgia and North Carolina will be included where appropriate and available.

Advanced features:
	Create separate contour maps for each source dataset.
	Create separate contour maps for different baseflow separation methods.
	Compare contour results with the North Carolina map.
	Leave-one-out cross-validation of the contour results.

DNR real-time groundwater network - the data is stored on a virtual desktop, and it is not clear what will be the best method of retrieving the real-time data. There are only 6 wells that we would want to use (?), so it would be feasible for the user to copy the data over from the virtual desktop as needed, at least until we can implement an automated method to access the real-time data. The real-time data is provisional, and it will be necessary to run quality assurance checks on the water level and barometric pressure data. If real-time data from nearby areas of North Carolina or Georgia is publicly available from the respective state monitoring networks, and the wells have at least 10 years of data and represent shallow aquifers, then include them.

3 files per well, 7 wells.
  provisional data from access database on groundwater server.
  long term monthly medians
  approved historical daily max, min, and median (not needed)


USGS groundwater network - can be accessed using the DataRetrieval R package. Includes 20-ish wells. AND-326 and SU-355 were both in the DNR network for a period. USGS took them over, but didn't take the old data. So, we want to use the USGS plus the DNR data for those two wells. We will use all of the circle dots on here: https://groundwaterwatch.usgs.gov/StateMap.asp?sa=SC&sc=45
Except for:
  CTF 189 (not enough data),
  BRK 431 (very deep),
  CHN 14 (very deep),
  BFT 1810 (planning to move),
  and AK 430 (planning to abandon).

In SC, that leaves:
  OC-233    345051083041800
  SP-1581   345145081502900
  AND-326   343714082285600 (additional records in DNR database)
  CTR-21    344000081250011
  YRK-3295  345830081033100
  LAN-497   344333080503600
  MCK-52    335336082214600
  KER-433   342440080443900
  SU-355    340059080240709 (additional records in DNR database)

GA sites:
  16MM03    344314083433201 400 ft depth. Data back to 1988.
  20GG41    340204083132601 743 ft depth. Data back to 2008.
  21BB04    332808083010201 497 ft depth. Data back to 1987.
  23X027    325848082480901 750 ft depth. Data back to 1985.
  31U009    323123081511602 210 ft depth. Data back to 1982.
  35P094    315950081161201  15 ft depth. Data back to 1942. (Coastal)
  39Q029    320127080511205  37 ft depth. Data back to 1998. (Coastal)
  30AA04    331711081573701 455 ft depth. Data back to 1979. (SRS)
  29AA42    331944082025501 509 ft depth. Data back to 2010. (SRS)
  30AA37    332221081584601 200 ft depth. Data back to 2009. (SRS)
  29AA09    332131082013401 213 ft depth. Data back to 1990. (SRS)
  29BB67    332528082003301  82 ft depth. Data back to 2011. (SRS)

NC sites:
  CE-029    351121083545002  24 ft depth. Data back to 1989.
  HW-047    352315082484401  18 ft depth. Data back to 1955.
  TR-066    351709082434101  23 ft depth. Data back to 1985.
  TR-065    351808082374302  58 ft depth. Data back to 1981.
  GS-290    352012081154302 123 ft depth. Data back to 2010.
  UN-148    345609080415103 301 ft depth. Data back to 2011.
  SC-080    345812079313401  39 ft depth. Data back to 1987.
  SC-040    344520079281001 240 ft depth. Data back to 2000.
  SC-142    344544079263701 161 ft depth. Data back to 2007.
  BR-381    335335078351901  39 ft depth. Data back to 2013. (Short record)
  BR-079    335629078115406 102 ft depth. Data back to 1987.
  BR-100    335849078054301 158 ft depth. Data back to 1999.
  BR-082    335631078003605  74 ft depth. Data back to 1999.

Wells in the USGS network in North Carolina and Georgia (but near the South Carolina border) can be included if they have more than 10 years of record, but wells tapping very deep aquifers should not be included. (How deep? Surficial only?)

USGS streamflow network - can be accessed using the DataRetrieval R package. Exclude gages with less than 10 years of record or with significant impacts from reservoirs operations. Consider nearby stream gages in North Carolina and Georgia as well.
