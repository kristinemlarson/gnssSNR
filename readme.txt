January 8, 2018
This is a new version of RinexSNR. The original used navigation
messages and was restricted to L1 and L2 GPS signals. This version
allows all GNSS constellations (GPS, GLonass, Galileo, and Beidou)
and L1, L2, and L5.  To accommodate this change, I am now using
sp3 files.  Another change is that you can modify the a priori receiver
location using the knut.txt file.  An input station position and velocity
can be provided. If it does not exist, it uses the RINEX header location.
This option is particularly useful for Antarctica and Greenland.

To test that you are getting the correct answers, use
             input        output    sp3file    option
gnssSNR.e smm32960.17o  out-smm3A gbm19721.sp3 98
gnssSNR.e smm32970.17o  out-smm3B gbm19722.sp3 98

option 98 and 99 are data between 5-30 degrees
option 88 is all data above 5 degrees
option 50 is all data below 10 degrees
an old option (77) for L2C only satellites is not currently set.

To accommodate reflector height corrections which need d(elevation angle)/ dt,
the 5th column now has this value, in degrees/sec.
6th column is not meaningful

7,8,9 are L1, L2, and L5

Satellites are defined as:
GPS 1-99
Glonass 101-199
Galileo 201-299
Beidou 301-399

Please let me know if you find bugs.

Sincerely,
Kristine M. Larson


-----------------------------------OLD CODE -------------------------------


June 5, 2015

RinexSNRv2 extracts SNR data from RINEX files (version 2.11)
It computes a simple azimuth and elevation angle
for easy reading into other programs, i.e. Matlab.

Output format:
satellite number, elevation angle, azimuth angle, secondsOfday 
east and north multipath reflection points (in meters) for a 
2 meter antenna and S1 S2 values in the original RINEX units, usually db-Hz

The options are easy to see in the code. Option 77 is all data for L2C only satellites.
As new satellites are launched, you need to add their names to the code.
Option 99 is for all satellites, but only below 30 degrees. Option 88 is all
satellites, all data.


I cut off at 5 degrees because I have found the data can have 
large outliers below this value, but you are welcome to modify the 
code for your own interests.

To run:
 
RinexSNRv2.e inputRinex outputSNRfileName navfile program-option

----

1. This version replaces the previous one that had a bug in the 
ephemeris calculation.  In fixing that, I took the opportunity to rewrite the code
in a more modular form. I hope this makes it easier for others to 
understand. 

2. This code ignores everything except GPS data.  If someone is willing
to give me fortran code that will calculate the orbit for a GLONASS
satellite, I am willing to modify it.

3. I do not think this code allows more than 24 satellites at a 
given epoch. I don't think that will be hard to fix, but currently the 
program exits if it finds more than 24 satellites.

4. There is a limit to the number of observables. You can use teqc
to reduce the number of observations if you run into that limitation.

5. I'm sorry but I do not speak PC. I cannot make a PC executable for you.

I would be very grateful for bug reports.

Sincerely
Kristine M. Larson
University of Colorado
