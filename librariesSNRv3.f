        subroutine azel(azimuth, elev, StaXYZ, East, North,
     .                  Staup, SatPos)
        implicit none
c      returns azimuth and elevation angle in degrees
c      station is cartesian coordinates of station (meters)
c      X, Y, Z is satellite coordinates (meters)
c ----------------------------------------------------
      double precision elev, azimuth
      double precision StaXYZ(3), X, Y, Z, East(3),
     . North(3), Staup(3), sta2sat(3), Mstation, Msat,
     . sta2sat_N, sta2sat_E, UdotS, zenith,SatPos(3)
      double precision pi, Lat,Long,Ht, range

c ----------------------------------------------------

c ... find azimuth/elevation of satellite relative
c ... to the reference station (station(3))
        pi = 4.0*datan(1.d0)
        sta2sat(1) = SatPos(1) - StaXYZ(1)
        sta2sat(2) = SatPos(2) - StaXYZ(2)
        sta2sat(3) = SatPos(3) - StaXYZ(3)
        range = sqrt(sta2sat(1)**2+ sta2sat(2)**2 +sta2sat(3)**2)

c    ... azimuth of satellite from station:
c  ... get components of 'sta2sat' in ENV frame:
         sta2sat_E = East(1)*sta2sat(1) + East(2)*sta2sat(2)
     .        + East(3)*sta2sat(3)
         sta2sat_N = North(1)*sta2sat(1) + North(2)*sta2sat(2)
     .        + North(3)*sta2sat(3)

c atan2(X,Y) == tan-1(X/Y)
         azimuth = datan2(sta2sat_E, sta2sat_N)*180/pi
         if (azimuth .lt. 0) then
            azimuth = 360 + azimuth
         endif

c    ... elevation angle calculation:
c         Mstation = dsqrt(Staup(1)**2 + Staup(2)**2 +
c    .          Staup(3)**2)
         Mstation = 1 
         Msat = dsqrt(sta2sat(1)**2 + sta2sat(2)**2 +
     .          sta2sat(3)**2)
         UdotS = Staup(1)*sta2sat(1) + Staup(2)*sta2sat(2) +
     .           Staup(3)*sta2sat(3)
         zenith = dacos(UdotS/(Mstation*Msat))*180/pi
         elev = 90 - zenith

       return
       end


c---------------------------------------------------------------------
      subroutine geoxyz2(alat,along,hght,x,y,z,iflag)
c
c Purpose:
c     Convert geodetic curvilinear coordinates to geocentric Cartesian
c        coordinates and vice versa
c
c Input:
c     iflag = 1  convert geodetic coordinates to cartesian coordinates
c           = 2  convert cartesian coordinates to geodetic coordinates
c
c Input/Output:
c     alat,along : geodetic latitude and longitude (radians)
c     hght       : height above the reference ellipsiod (meters)
c     x,y,z      : geocentric Cartesian coordinates (meters)
c
c Notes:
c     Currently assumes the WGS84 reference ellipsoid;
c     Clarke 1866 ellipsoid with approximate translation parameters
c        for NAD27 are commented.
c     Cartesian to geodetic conversion uses an iterative scheme
c        valid to the millimeter level.
c
      integer iflag
      real*8 alat,along,hght,x,y,z, b
      real*8 semi,finv
      real*8 twopi,f,e2,curvn,sqr,alat0,cutoff
      real*8 sinlat,coslat,sinlon,coslon

      semi = 6378137.d0
      finv = 298.257223563d0
      twopi= 8.d0*datan(1.d0)
      f= 1.d0/finv
      b = semi*(1.d0 - f)
      e2= 2.d0*f - f*f
      if( iflag.eq.1) then
         sinlat= dsin(alat)
         coslat= dcos(alat)
         sinlon= dsin(along)
         coslon= dcos(along)
         curvn= semi/(dsqrt(1.d0-e2*sinlat*sinlat))
     
         x= (curvn+hght)*coslat*coslon 
         y= (curvn+hght)*coslat*sinlon 
         z= (curvn*(1.d0-e2)+hght)*sinlat 
      else
         along= datan2(y,x)
         if( along.lt.0d0 ) along=along + twopi
c        starting value for latitude iteration
         sqr= dsqrt(x*x+y*y)
         alat0= datan2(z/sqr,1.d0-e2)
         alat= alat0
   40    sinlat= dsin(alat)
         curvn= semi/(dsqrt(1.d0-e2*sinlat*sinlat))
         alat= datan2((z+e2*curvn*sinlat),sqr)
c        iterate to millimeter level
         if( dabs(alat-alat0).lt.1.d-10) goto 30
         alat0= alat
         goto 40
   30    continue
         cutoff= 80.d0*twopi/360.d0
         if(alat.le.cutoff) then
            hght= (sqr/dcos(alat))-curvn
         else
            hght= z/dsin(alat)-curvn+e2*curvn
         endif
      endif
      return
      end
      subroutine julday (itimes, stime, utc)
c
c...coded by: j mcmillan - university of texas - july 1973
c   julday entry added by brian cuthbertson - 9/29/1979.
c
c...purpose:  to convert between calendar day and julian date (utc).
c
c...formal parameter definitions:
c
c   kalday input  (julday output):
c      utc       the julian date in (double precision) utc form.
c
c      itimes    integer array containing month, day, year, hour, minute
c      stime     floating point seconds
c
      implicit double precision (a-h,o-z)
      dimension itimes(5)
      jd = itimes(2) - 32075 +
     .     1461 * (itimes(3) + 4800 + (itimes(1)-14)/12) /4  +
     .     367 * (itimes(1) - 2 - (itimes(1)-14)/12*12) /12  -
     .     3 * ((itimes(3) + 4900 + (itimes(1)-14)/12)/100) /4
c
      utc= dfloat(jd-2400000) - 1.0d+0  +
     .     dfloat(itimes(4)) / 24.0d+0  +
     .     dfloat(itimes(5)) / 1440.0d+0  +
     .     stime / 86400.0d+0
c
      return
c
c...end kalday/julday
      end
      subroutine mjdgps(tjul,isec,nweek)
c      save
c*
cc name       : mjdgps
cc
cc      call mjdgps(tjul,second,nweek)
cc
cc purpose    : compute number of seconds past midnight of last
cc              saturday/sunday and gps week number of current
cc              date given in modified julian date
cc
cc parameters :
cc         in : tjul  : modified julian date                      r*8
cc        out : second: number of seconds past midnight of last   r*8
cc                      weekend (saturday/sunday)
cc              nweek : gps week number of current week           i*4
cc
cc sr called  : none
cc
cc author     : w. gurtner
cc              mjdgps is a member of gpslib
cc              astronomical institute, university of berne
cc              switzerland
cc##	no unit is used in this subroutine
cc
        implicit real*8 (a-h,o-z)
c
c
c  days since starting epoch of gps weeks (sunday 06-jan-80)
      deltat=tjul-44244.d0
c  current gps week
      nweek=deltat/7.d0
c  seconds past midnight of last weekend
      second=(deltat-(nweek)*7.d0)*86400.d0 
      isec = second  + 0.5d0
c
      return
      end




      subroutine read_header(fileID,rawf,xrec,yrec,zrec,
     .  iobs,nobs,iymd,station)
      implicit none
      include 'local.inc'
      integer  i, fileID
      character*80 rawf
      character*80 line, outline, dynfmt, dynfmt2
      logical  endofheader 
      integer nobs,iobs(maxsat), iymd(3), ios
      character*2 key(maxsat)
      character*4 station
      real*8 xrec, yrec, zrec
c     returns receiver coordinates
c     station name is empty to start with
c     returned to main code
      station = '    ' 
      endofheader = .false.

      open(fileID,file=rawf, status='old',iostat=ios)
      if (ios.ne.0) then
        print*, 'problem opening RINEX file' 
        print*, 'name:', rawf
        call exit(0)
      endif
      do while (.not.endofheader)
c     KL 18mar05, fixed bug on nobs
        read (fileID,'(a80)') line
        if (line(61:80).eq.'# / TYPES OF OBSERV') then
          read(line, fmt='(I6)') nobs
c         exit if more than 20 observables
          if (nobs.gt.20) then
             print*,'this code only supports <=20 observ types'
             call exit
          endif
c   KL 19jan09 allowing more lines of OBS types
c         first line has up to 9 OBS types
          if (nobs .lt. 10) then
            write(dynfmt, fmt='(A, I3.3, A)')
     +                      "(6X,", nobs, "(4X,A2))"
            read(line, fmt=dynfmt) (key(i), i=1,nobs)
c         between 10-18 OBS types
          elseif (nobs.gt.9.and.nobs.le.18) then
            write(dynfmt, fmt='(A, I3.3, A)')
     +                      "(6X,", 9, "(4X,A2))"
            read(line, fmt=dynfmt) (key(i), i=1,9)
c           read the next line
            read (fileID,'(a80)') line
            write(dynfmt, fmt='(A, I3.3, A)')
     +                      "(6X,", nobs-9, "(4X,A2))"
            read(line, fmt=dynfmt) (key(i), i=10,nobs)
c           this is more than 18 OBS types
          else
c           first line
            write(dynfmt, fmt='(A, I3.3, A)')
     +                      "(6X,", 9, "(4X,A2))"
            read(line, fmt=dynfmt) (key(i), i=1,9)

c           read the next line
            read (fileID,'(a80)') line
c           reassign this second line
            write(dynfmt, fmt='(A, I3.3, A)')
     +                      "(6X,", 9, "(4X,A2))"
            read(line, fmt=dynfmt) (key(i), i=10,18)

c           read the third line
            read (fileID,'(a80)') line
            write(dynfmt, fmt='(A, I3.3, A)')
     +                      "(6X,", nobs-18, "(4X,A2))"
            read(line, fmt=dynfmt) (key(i), i=19,nobs)

          endif
          print*, 'NUMBER OF OBSERVABLES ', nobs
        else if (line(61:80).eq.'APPROX POSITION XYZ') then
          read(line, fmt= '(3f14.4)') xrec, yrec, zrec
           print*, 'XYZ coordinates ', xrec, yrec, zrec
          if (xrec.eq.0) then
            print*, 'I cannot compute satellite elevation angles'
            print*, 'without apriori receiver coordinates - exiting'
            call exit
          endif
        else if (line(61:77).eq.'TIME OF FIRST OBS') then
          read(line, fmt= '(3i6)') iymd(1), iymd(2), iymd(3)
          print*, 'Time of first Obs: ', iymd
        else if (line(61:71).eq.'MARKER NAME') then
          read(line(1:4), fmt= '(a4)')  station
          print*, 'Station name ', station
        endif
        if (line(61:73).eq.'END OF HEADER'.or.
     +       line(61:73).eq.'end of header'.or.
     +       line(61:73).eq.' ') endofheader = .true.
      enddo
      print*, 'FOUND END OF HEADER'
      do i = 1,maxsat
          iobs(i) = 0
      enddo
      do i = 1, nobs
          if (key(i).eq.'l1' .or. key(i).eq.'L1') iobs(1) = i
          if (key(i).eq.'l2' .or. key(i).eq.'L2') iobs(2) = i
          if (key(i).eq.'c1' .or. key(i).eq.'C1') iobs(3) = i
          if (key(i).eq.'p1' .or. key(i).eq.'P1') iobs(4) = i
          if (key(i).eq.'p2' .or. key(i).eq.'P2') iobs(5) = i
          if (key(i).eq.'s1' .or. key(i).eq.'S1') iobs(6) = i
          if (key(i).eq.'s2' .or. key(i).eq.'S2') iobs(7) = i
          if (key(i).eq.'s5' .or. key(i).eq.'S5') iobs(8) = i
          if (key(i).eq.'s6' .or. key(i).eq.'S6') iobs(9) = i
          if (key(i).eq.'s7' .or. key(i).eq.'S7') iobs(10) = i
          if (key(i).eq.'s8' .or. key(i).eq.'S8') iobs(11) = i
      enddo
      end

      subroutine pickup_blockIIRM(blockIIRM)
      implicit none
      include 'local.inc'
      integer blockIIRM(maxsat)
c     define the blockIIRM satellites
c     this should be modified so that it is time dependent,
c     i.e. PRN 25 was only blockIIRM after August 2010.
      blockIIRM(1) = 1
      blockIIRM(5) = 1
      blockIIRM(7) = 1
      blockIIRM(12) = 1
      blockIIRM(17) = 1
      blockIIRM(15) = 1
      blockIIRM(29) = 1
      blockIIRM(24) = 1
      blockIIRM(31) = 1
      blockIIRM(25) = 1
      blockIIRM(27) = 1
c     launched jday 052 in 2014
      blockIIRM(30) = 1
c     launched jDAY 137 in 2014
      blockIIRM(6) = 1
c     launched August 1, 2014
      blockIIRM(9) = 1
c     launched October 29?, 2014
      blockIIRM(3) = 1
c     launched March 2015
      blockIIRM(26) = 1
c     launched July 2015
      blockIIRM(8) = 1
      blockIIRM(10) = 1
      blockIIRM(32) = 1

      end

      subroutine envTrans(xrec,yrec,zrec,stationXYZ, Lat,Long, 
     .  Height, North, East,Up)
c     inputs are receiver coordinates in meters apprently
c     returns a 3 vector (also in meters)
c     Lat and Long are in radians, height is in meters
      implicit none
      real*8 stationXYZ(3), xrec, yrec, zrec, Lat,Long,Height
      real*8 North(3), East(3), Up(3)
      real*8 eflat, pi
      eflat = .33528919d-02
      pi = 4.0*datan(1.d0)
c     XYZ in meters
      stationXYZ(1) = xrec
      stationXYZ(2) = yrec
      stationXYZ(3) = zrec
      call geoxyz2(Lat,Long,Height,xrec,yrec,zrec,2)
      write(6,'(2F15.9,1x,f12.4)') 180*Lat/pi,180*Long/pi,Height
      Up(1) = dcos(Lat)*dcos(Long)
      Up(2) = dcos(Lat)*dsin(Long)
      Up(3) = dsin(Lat)
c ... also define local east/north for station:
      North(1) = -dsin(Lat)*dcos(Long)
      North(2) = -dsin(Lat)*dsin(Long)
      North(3) = dcos(Lat)
      East(1) = -dsin(Long)
      East(2) = dcos(Long)
      East(3) = 0
      end
      subroutine convert_time(itime,sec, msec, gpsweek, tc)
      implicit none
c     takes as input itime (who can remember what the inputs are,
c     but i think it is RINEX, i.e. YY MM DD HH MM
c     seconds is its own
c     returns gpsweek and tc, which is time in gps seconds (real*8)
      integer itime(5), jtime(5), gpsweek, gpsseconds, msec, sec
      real*8 tjul, rho, tc
c     change 2 char to 4 char
      if (itime(1).lt.80) then
          jtime(3) = itime(1) + 2000
      else
          jtime(3) = itime(1) + 1900
      endif
c     rearrange
      jtime(1) = itime(2)
      jtime(2) = itime(3)
      jtime(4) = itime(4)
      jtime(5) = itime(5)
      rho = 1.0*sec
      call julday(jtime,rho,tjul)
      call mjdgps(tjul,gpsseconds,gpsweek)
c     gps seconds, including non integer parts
      tc = dble(gpsseconds) + msec/1000.0
      end
      subroutine pickup_snr(obs, iobs, itrack, s1, s2, s5,s6,s7,s8)
      implicit none
c     make sure SNR data exist before trying
c     to assign as index to a variable
c     send it the entire variable information
c     returns s1,s2,s5 etc
      include 'local.inc'
      real*8  obs(maxob,maxsat) 
      integer iobs(maxsat), itrack
      real*8 s1, s2, s5,s6,s7,s8
      s1 = 0
      s2 = 0
      s5 = 0
      s6 = 0
      s7 = 0
      s8 = 0
      if (iobs(6).ne.0) then
        s1 = obs(iobs(6),itrack)
      endif
      if (iobs(7).ne.0) then
        s2 = obs(iobs(7),itrack)
      endif
      if (iobs(8).ne.0) then
        s5 = obs(iobs(8),itrack)
      endif
      if (iobs(9).ne.0) then
        s6 = obs(iobs(9),itrack)
      endif
      if (iobs(10).ne.0) then
        s7 = obs(iobs(10),itrack)
      endif
      if (iobs(11).ne.0) then
        s8 = obs(iobs(11),itrack)
      endif
      end
