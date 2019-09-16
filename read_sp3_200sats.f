      subroutine read_sp3_200sats(inputfile, gps_weeks, 
     .  gps_seconds, nsat, satnames, XYZ,haveorbit,
     .  ipointer,nepochs,relTime)
      implicit none
c     kristine larson, september 15,2015
c     the input is a 15 minute sp3file name
c     17nov02 tried to extend to 5 minute spfiles
c     KL 19feb01, allow sp3 files that are longer than 23 hours and 45 minute
c     times will now be in gps seconds but a second day will no longer go back to 
c     zero on a day for new week.  this will make it easier to interpolate using a 
c     common time frame, which will be called relTime
c     previous behavior assumed all data were from the same day
c
c     returns satellite names, 
c     gps weeks and 
c     gps seconds of the week, XYZ (in km) 
c     17oct12, KL
c     changed to allow multiple GNSS and 200 satellites
c     INPUT 
c     inputfile is sp3 filename
c     OUTPUT
c     gps_weeks and gps_seconds are arrays? of times on the sp3file
c     nsat - number of satellites
c     satnames - integers
c     1-99 for GPS
c     101-199 for GLONASS
c     201-299 for GALILEO
c     301-399 for BDS
c     default nsat value of 0
c     added pointer array for orbits
c
c     17nov03 returns number of time epochs now
c     18may14 read the header date and ensure that only
c     data from that date are used (sp3 files from CODE had
c     had two midnites in them).
c     19mar25 changed filename of sp3 to be really really long
      include 'local.inc'
      character*128 inputfile
      character*80 line, outputfile
      character*2 chr
      integer satnames(maxsat),nsat, gpsweek, prn, i, j, k, 
     .  time, itime(5), gps_weeks(np), msec, sec, ios,newname,
     .  hdr_yy, hdr_mm, hdr_dd, hdr_hour, hdr_minute, FirstWeek
      real*8 x,y,z, XYZ(maxsat,np,3), gps_seconds(np), gpssecond,
     .  FirstSecond, relTime(np), rt
      character*1 duh/'+'/
      character*1 satID(maxsat), constell
      logical haveorbit(maxGNSS) 
      integer ipointer(maxGNSS), nepochs, s1, s2
      print*, 'Enter sp3 file reading code'
      nsat = 0
      do i=1,maxGNSS
        haveorbit(i) = .false.
        ipointer(i) = 0
      enddo
      do i=1,maxsat
        satnames(i) = 0
      enddo
c     define everything as zero to start 
      do i=1,np
        gps_seconds(i) = 0
        gps_weeks(i) = 0
        do j=1,maxsat
          do k=1,3
            XYZ(j,i,k) = 0.d0
          enddo
        enddo
      enddo
      sec= 0
      msec = 0
c     open the sp3file
c
      open(12, file=inputfile,status='old',iostat=ios)    
      if (ios .ne. 0) then
        print*, 'the sp3 file does not exist '
        print*, inputfile(1:80)
        call exit
      endif
c     #cP2015 12 30  0  0  0.00000000      97 d+D   IGb08 FIT AIUB
c     skip first two lines of the header-  
c     now save the month and day of the file
      read(12,'(a80)') line
      print*, 'First epoch of SP3 file ', line
      print*, 'Number of epochs', line(37:39)
c     removed the commas that are not compliant with new fortran?
      READ (line(37:39), '(I3)') nepochs
      if (nepochs.gt.np) then 
        print*,'there are more epochs in this file than the code'
        print*,'is dimensioned for. Exiting.'
        call exit
      endif
      READ (line(6:7), '(I2)') hdr_yy 
      READ (line(9:10), '(I2)') hdr_mm 
      READ (line(12:13), '(I2)') hdr_dd 
      READ (line(15:16), '(I2)') hdr_hour 
      READ (line(18:19), '(I2)') hdr_minute
      print*, 'num epochs in header', nepochs  
      print*, 'header time:', hdr_yy, hdr_mm, hdr_dd,hdr_hour,hdr_minute
      itime(1) = hdr_yy
      itime(2) = hdr_mm
      itime(3) = hdr_dd 
      itime(4) = hdr_hour
      itime(5) = hdr_minute
      sec = 0
      msec = 0
      call convert_time(itime,sec, msec, FirstWeek, FirstSecond)
      print*, 'first week/sec', FirstWeek, FirstSecond

      read(12,'(a80)') line
      read(12,'(1x,i5,3x, 17(a1,i2) )')nsat, 
     .     (satID(i), satnames(i), i=1,17)
      read(12,'(9x, 17(a1,i2))')(satID(i),satnames(i), i=18,34)
      read(12,'(9x, 17(a1,i2))')(satID(i),satnames(i), i=35,51)
      read(12,'(9x, 17(a1,i2))')(satID(i),satnames(i), i=52,68)
      read(12,'(9x, 17(a1,i2))')(satID(i),satnames(i), i=69,85)
      s1 = 86
      s2 = 102
113   read(12,'(a80)') line
c     print*, line
      if (line(1:2) .eq. '+ ') then
        print*,'found another sat line'
        read(line,'(9x, 17(a1,i2))')(satID(i),satnames(i), i=s1,s2)
c       increment the counters.  this has only been tested up to 102
        s1 = s1 +17
        s2 = s2 + 17
      else if (line(1:2) .eq. '/*') then 
c       print*,'comment line i think'
      else if (line(1:2) .eq. '++') then 
c       print*,'qual flag'
      endif

      if (line(1:1).ne.'*') goto 113

      call fill_pointer(nsat,satID,satnames,haveorbit,ipointer)
c      start your counter for number of epochs
c     I think this also means you have read the header
      time = 1
15    continue
      if (line(1:1).eq.'*') then
c       decode your time tag
        read(line(6:7), '(i2)') itime(1)
        read(line(9:10), '(i2)') itime(2)
        read(line(12:13), '(i2)') itime(3)
        read(line(15:16), '(i2)') itime(4)
        read(line(18:19), '(i2)') itime(5)
c       trying to read two day sp3 file, so changes wrt previous file
        if (.true.) then
          call convert_time(itime,sec, msec, gpsweek, gpssecond)
c         now need to read nsat lines to get the coordinates of the satellite
          do i=1,nsat
            read(12,'(a80)') line
            read(line(2:2),'(a1)') constell
            read(line(3:46),*)prn,x,y,z
c           change prn to new system
            call newSat(constell,prn,newname)
            prn = newname
c           now using index i instead of PRN number to store data
            XYZ(i,time,1) = x
            XYZ(i,time,2) = y
            XYZ(i,time,3) = z 
            gps_weeks(time) = gpsweek
            gps_seconds(time) = gpssecond
            call rel_time(gpsweek, gpssecond, 
     .           FirstWeek, FirstSecond,rt)
c           save seconds since first epoch
            relTime(time) = rt
c           print*, gpsweek,gpssecond, rt
          enddo
          time = time + 1
        endif
c       read the next line - it should be a time tag
        read(12,'(a80)') line
c       increment the time variable
        if (line(1:3).eq.'EOF') goto 55
        if (time >np) then
          print*,'your sp3 file exceeds max number ', np, ' values'
          print*,'this is bad - exiting the subroutine'
          goto 55
        endif
      endif
      goto 15
55    continue
c     subtract one because of the CODE midnite issue
      nepochs = time - 1
      print*, 'RETURNING epochs: ', nepochs
c     you are done reading the file
      close(12)
      print*, 'exiting the sp3 reading code'
56    continue
      end
      subroutine newSat(constell, satnum,nsatnum)
      implicit none
c     takes constellation ID and satellite number
c     and returns a new satellite number, offset by
c     100 for glonass, 200 for galileo etc
c     unknown satellites are all assigned to 400
c     author: kristine larson 17oct15
c     old rinex files do not use a G for GPS
c     so allow blank
c     17nov05 added Q satellites 381, 382, etc
      integer satnum,nsatnum
      character constell
      if (constell .eq. 'G') then
         nsatnum = satnum
      elseif (constell .eq. ' ') then
         nsatnum = satnum
      elseif (constell .eq. 'R') then
         nsatnum = 100+satnum
      elseif (constell .eq. 'E') then
         nsatnum = 200+satnum
      elseif (constell .eq. 'C') then
         nsatnum = 300+satnum
      elseif (constell .eq. 'J') then
c       Japanese satellites
         nsatnum = 380+satnum
      else
         nsatnum = 400
      endif

      end
      subroutine fill_pointer(nsat,satID,satnames,haveorbit,ipointer)
c     author: kristine larson 17nov03
c     purpose: change the satellite names to integers from R??, E??, etc
c     and fill the pointer array
c     inputs: nsat is number of satellites, satID is one character constellation ID
c     outputs: satnames uses our 100,200,300 convention for naming satellites
c             ipointer tells you where it is in the sp3 file
      implicit none
      include 'local.inc'
      integer satnames(maxsat), ipointer(maxGNSS), i, nsat, newname
      character*1 satID(maxsat)
      logical haveorbit(maxGNSS)
      do i =1, nsat
        call newSat(satID(i), satnames(i),newname)
c       was mostly for debugging
c       write(6,'(a1, i2, 1x, i3)') satID(i), satnames(i), newname
        satnames(i) = newname
        haveorbit(newname) = .true.
        ipointer(newname) = i
      enddo
      end

      subroutine rel_time(gps_week,gps_second,epochWeek,epochSec,rt)
c     send times (week,secs) and epoch times (epochWeek,epochSec)
c     return relative time, rt
      integer gps_week, epochWeek
      real*8 gps_second, epochSec, rt
      if (gps_week.eq.epochWeek) then
        rt = gps_second - epochSec
      else
c       add a week of seconds
        rt = 7*86400 + gps_second - epochSec
      endif
c     print*, gps_second, rt
      end
