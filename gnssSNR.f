      program gnssSNR
      implicit none
c     Kristine Larson 15jul27
c     version updated on 18jan08
c     uses sp3 file to compute azimuth and elevation angle
c     all satellite signals
c     17oct15 added orbit pointer
c     18may14 checked to make sure sp3 file doesn't have data
c     from two different days
c      
c     18oct01 added S6,S7,S8, currently storing S6 in column 6
c     and S7 and S8 are in new columns: 10 and 11
c     This helps with Galileo
c     column 5 is still edot
c     column 9 is still S5
c     column 7 and 8 are S1 and S2 
c 
c     18oct16 increased number of satellites allowed at any epoch to 48
c     this allows multiple constellations without having to make separate files
c     19feb04
c     allow sp3 files that are longer than 23 hr 45 minutes
      include 'local.inc'
      integer stderr
      parameter (stderr=6)
      character*80 inline, line 
      character*128 sp3file
      character*80 rawfilename, outfilename, broadfile
      character*4 station
      character*2  key(maxsat), prn_pickc
      character*1 char, satID(maxsat)
      integer  nobs, itime(5), prn(maxsat), numsat, flag, sec,
     +   msec, lli(maxob,maxsat), iprn,
     .   ios, itrack, L2Ctracking,j, i, iobs(maxsat), isec,
     .  gpsweek, ierr,  prn_pick, current_hour, fileIN, fileOUT,
     .  iymd(3), iyear, idoy, nepochs, FirstWeek  
      real*8  obs(maxob,maxsat), tod,North(3),
     .   East(3), Up(3), azimuth, elev, staXYZ(3), tod_save,
     .   pi,s1,s2,s5,xrec, yrec, zrec, tc, l1,l2, Lat,Long,Ht,
     .   edot,elev1,elev2, nxrec,nyrec, nzrec, s6, s7, s8,rt,
     .   rt_lastEpoch, FirstSecond, tod2
      logical eof, bad_point,useit, help, simon
      integer sp3_gps_weeks(np),sp3_nsat,sp3_satnames(maxsat)
      real*8 sp3_XYZ(maxsat,np,3), sp3_gps_seconds(np),
     .  t9(9), x9(9), y9(9), z9(9), sp3_rel_secs(np)
      integer ipointer(maxGNSS)
      logical haveorbit(maxGNSS), fsite, debug
      debug = .true.
      debug = .false.
c     set some defaults
c     if you want edot, set this to true
      simon = .true.
      fileIN = 12
      fileOUT = 55
      tod_save = 0.0
      bad_point = .false.
      pi = 4.0*datan(1.d0)
      help = .false.
c
c     read input files - rinex and output
      call getarg (1,rawfilename)
      call getarg (2,outfilename)
      call getarg (3,sp3file)
      print*,sp3file
      call getarg (4,prn_pickc)
      write(stderr,*) '>>>>> OUTPUT GOES HERE:', outfilename
c     comment out for now
c     figure out which option is being requested
      READ (prn_pickc, '(I2)')  prn_pick
      write(stderr, *) 'Selection ', prn_pick
c     read in the sp3file
      call read_sp3_200sats(sp3file, sp3_gps_weeks, sp3_gps_seconds,
     .   sp3_nsat, sp3_satnames, sp3_XYZ,haveorbit,
     .   ipointer,nepochs,sp3_rel_secs)
      FirstWeek = sp3_gps_weeks(1)
      FirstSecond = sp3_gps_seconds(1)
      print*, 'first and last',FirstWeek, FirstSecond
      print*, sp3_gps_weeks(nepochs), sp3_gps_seconds(nepochs)
c     figure out the time tag of the last sp3 point.  this way you don't
c     interpolate (much) beyond your last point
      print*, 'Last epoch, rel sense', sp3_rel_secs(nepochs)
      rt_lastEpoch = sp3_rel_secs(nepochs)
      if (sp3_nsat .eq. 0) then
        print*, 'problem reading sp3file'
        call exit(0)
      endif
c     read the header of the RINEX file, returning station coordinates
c     and an observable array and nobs, number of observables
      call read_header_25obs(fileIN,rawfilename, xrec,yrec,zrec,
     .  iobs,nobs,iymd, station)
      print*,'number of obs main code', nobs
c     comment out for now.  should read station name
c     from the receiver
      call moving_sites(station, iymd(1), iymd(2), iymd(3),
     .   nxrec,nyrec,nzrec,fsite)
      if (fsite) then
        print*, 'use variable model station coordinates'
        xrec = nxrec
        yrec = nyrec
        zrec = nzrec
        print*, xrec, yrec, zrec
      endif
      if (nobs .gt. 25 .or. nobs .eq. 0) then
        print*, 'This code currently only works for <= 20 obs types'
        print*, 'Something is wrong'
        call exit(0)
      endif
      print*, 'S1 location:', iobs(6)
      print*, 'S2 location:', iobs(7)
      print*, 'S5 location:', iobs(8)
      print*, 'S6 location:', iobs(9)
      print*, 'S7 location:', iobs(10)
      print*, 'S8 location:', iobs(11)


      call envTrans(xrec,yrec,zrec,staXYZ,Lat,Long,Ht,North,East,Up)
c     open output file
      open(fileOUT,file=outfilename, status='unknown')
      eof = .false.
      do while (.not.eof)
        inline = ' '
        read(fileIN,'(A80)', iostat=ios) inline
        if (ios.ne.0) goto 99
        read(inline(1:32),'(5I3,X,I2,X,I3,4X,2I3)')
     +         (itime(i), i=1,5), sec, msec, flag, numsat
        if (debug) then
          print*, 'number of satellites ' ,numsat
          print*, inline
        endif
c       seconds in the day
        tod = itime(4)*3600.0 + 60.0*itime(5) + sec
c       print*, tod, tod_save
        tod2 = tod + msec/1000.d0
        if (tod.lt.tod_save) then
          print*, 'Time is going backwards.'
          bad_point = .true.
        else
          bad_point = .false.
        endif
        tod_save = tod
c       read the observation block
        call read_block_gnss(fileIN, flag,inline,numsat,nobs,satID,
     .    prn,obs,lli)
c       flag 4 means it is a comment block, so that gets skipped
c       added that the point is good
        if (flag .ne. 4 .and. .not.bad_point) then
c         find out gpsweek and gpsseconds (tc)
          call convert_time(itime,sec, msec, gpsweek, tc)
c         then convert it to relative time to first sp3 point 
          call rel_time(gpsweek, tc, FirstWeek,FirstSecond, rt)
202       format(a10,i6,f10.0)
c         check that it is within 20 minutes of the first and last sp3 points
          if ( rt.gt. (rt_lastEpoch+20*60) .or. 
     .          (rt.lt. -20*60))  then
             write(stderr,202) 'Your epoch', gpsweek, tc
             write(stderr,202) 'First  sp3',FirstWeek,FirstSecond
             write(stderr,202) 'Last   sp3',sp3_gps_weeks(nepochs), 
     .           sp3_gps_seconds(nepochs)
             print*, 'Your epoch is beyond a reasonable sp3 point,'
             print*, 'so I am exiting now.'
             call exit
          endif
          do itrack = 1, numsat
            iprn = prn(itrack)
c           400 is for satellites that do not recognize
c            haveorbit is for whether we have a sp3 orbit for that satellite
            if (iprn .ne. 400 .and. haveorbit(iprn) ) then
c             x9,y9,z9 are in meters
              edot = 0.0
c             pick 9 points closest to observation time
c             need to use the pointer
c             print*, gpsweek,tc,rt
              call pick_9points(sp3_nsat, sp3_satnames, sp3_gps_weeks,
     .          sp3_gps_seconds, sp3_XYZ, iprn, gpsweek,tc,itime(4),
     .          t9,x9, y9,z9,ipointer,nepochs,sp3_rel_secs,rt)
              if (simon) then
c               if simon variable is true, calculate edot
c               call get_azel_sp3(tc+0.5, iprn, staXYZ,East,North,Up,
c    .           Lat,Long,Ht, azimuth,elev2,t9,x9,y9,z9)
                call get_azel_sp3(rt+0.5, iprn, staXYZ,East,North,Up,
     .           Lat,Long,Ht, azimuth,elev2,t9,x9,y9,z9)
              endif
c             call get_azel_sp3(tc, iprn, staXYZ,East,North,Up,
c    .           Lat,Long,Ht, azimuth,elev,t9,x9,y9,z9)
              call get_azel_sp3(rt, iprn, staXYZ,East,North,Up,
     .           Lat,Long,Ht, azimuth,elev,t9,x9,y9,z9)
              if (simon) then
c               since i did time values 0.5 seconds apart, multiply by 2
                edot =  2.d0*(elev2-elev)
              endif
c             assign the SNR values to variables
              call pickup_snr(obs, iobs, itrack, s1, s2, s5,s6,s7,s8)
c             write out to a file
              call write_gnss_to_file(fileOUT, iprn, tod,
     .          s1,s2,s5,azimuth, elev,edot,prn_pick,s6,s7,s8,tod2)
            else
c             this can be commented out - kept as debugging, sanity check
c             write(72,*)'no orbit for satellite', iprn, ' gpssec ',tc
            endif
          enddo
        endif
      enddo
99    continue
c     close input and output files
      close(fileIN)
      close(fileOUT)
      end
