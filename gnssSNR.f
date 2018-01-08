      program gnssSNR
      implicit none
c     Kristine Larson 15jul27
c     version updated on 18jan08
c     uses sp3 file to compute azimuth and elevation angle
c     all satellite signals
c     17oct15 added orbit pointer
      include 'local.inc'
      integer stderr 
      parameter (stderr=6)
      character*80 inline, line, sp3file
      character*60 rawfilename, outfilename, broadfile
      character*4 station
      character*2  key(maxsat), prn_pickc
      character*1 char, satID(maxsat)
      integer  nobs, itime(5), prn(maxsat), numsat, flag, sec,
     +   msec, lli(maxob,maxsat), snr(maxob,maxsat), iprn,
     .   ios, itrack, L2Ctracking,j, i, iobs(maxsat), isec, 
     .  gpsweek, ierr,  prn_pick, current_hour, fileIN, fileOUT,
     .  iymd(3), iyear, idoy, nepochs
      real*8  obs(maxob,maxsat), tod,North(3), 
     .   East(3), Up(3), azimuth, elev, staXYZ(3), tod_save, 
     .   pi,s1,s2,s5,xrec, yrec, zrec, tc, l1,l2, Lat,Long,Ht,
     .   edot,elev1,elev2, nxrec,nyrec, nzrec
      logical eof, bad_point,useit, help, simon
c     we assume there are only 96 temporal values in the sp3 file
      integer sp3_gps_weeks(np),sp3_nsat,sp3_satnames(maxsat) 
      real*8 sp3_XYZ(maxsat,np,3), sp3_gps_seconds(np),
     .  t9(9), x9(9), y9(9), z9(9)
      integer ipointer(maxGNSS)
      logical haveorbit(maxGNSS), fsite
      nepochs = 96
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
      call getarg (4,prn_pickc)
c     comment out for now
c     call getarg (5,station)
c     figure out which option is being requested
      READ (prn_pickc, '(I2)'), prn_pick
      write(stderr, *) 'Selection ', prn_pick
c    read in the sp3file
      call read_sp3_200sats(sp3file, sp3_gps_weeks, sp3_gps_seconds, 
     .   sp3_nsat, sp3_satnames, sp3_XYZ,haveorbit,ipointer,nepochs)
      if (sp3_nsat .eq. 0) then
        print*, 'problem reading sp3file'
        call exit(0)
      endif

c     read the header of the RINEX file, returning station coordinates
c     and an observable array and nobs, number of observables
      call read_header(fileIN,rawfilename, xrec,yrec,zrec, 
     .  iobs,nobs,iymd)
c     comment out for now.  should read station name
c     from the receiver
c     call moving_sites(station, iymd(1), iymd(2), iymd(3),
c    .   nxrec,nyrec,nzrec,fsite)
      if (fsite) then
        print*, 'use these new station coordinates'
        xrec = nxrec
        yrec = nyrec
        zrec = nzrec
        print*, xrec, yrec, zrec
      endif
      if (iobs(7) .eq. 0 .and. iobs(6) .eq. 0) then
        print*, 'no L1 and L2 SNR data - exiting '
        call exit(0)
      endif
      if (nobs .gt. 15) then
        print*, 'This code currently only works for <= 15 obs types'
        call exit(0)
      endif

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
c       seconds in the day
        tod = itime(4)*3600.0 + 60.0*itime(5) + sec
        tod_save = tod
        if (tod.lt.tod_save) then
c           'Time as written to this file is going backwards.'
          bad_point = .true.
        else
          bad_point = .false.
        endif
c       read the observation block
        call read_block(fileIN, flag,inline,numsat,nobs,satID, 
     .    prn,obs,snr,lli)
c       flag 4 means it is a comment block, so that gets skipped
        if (flag .ne. 4) then
c         find out gpsweek and gpstime (tc)
          call convert_time(itime,sec, msec, gpsweek, tc)
          do itrack = 1, numsat
            iprn = prn(itrack)
c           400 is for satellites that do not recognize
c            haveorbit is for whether we have a sp3 orbit for that satellite
            if (iprn .ne. 400 .and. haveorbit(iprn) ) then
c             x9,y9,z9 are in meters
              edot = 0.0
c             pick 9 points closest to observation time
c             need to use the pointer
              call pick_9points(sp3_nsat, sp3_satnames, sp3_gps_weeks,
     .        sp3_gps_seconds, sp3_XYZ, iprn, gpsweek,tc,itime(4),
     .        t9,x9, y9,z9,ipointer,nepochs)
              if (simon) then
c               if simon variable is true, calculate edot
                call get_azel_sp3(tc+0.5, iprn, staXYZ,East,North,Up,
     .           Lat,Long,Ht, azimuth,elev2,t9,x9,y9,z9)
              endif
              call get_azel_sp3(tc, iprn, staXYZ,East,North,Up,
     .           Lat,Long,Ht, azimuth,elev,t9,x9,y9,z9)
              if (simon) then
c               since i did time values 0.5 seconds apart, multiply by 2
                edot =  2.d0*(elev2-elev)
              endif
c             assign the SNR values to variables
              call pickup_snr(obs, iobs, itrack, s1, s2, s5)
c             write out to a file
              call write_gnss_to_file(fileOUT, iprn, tod,
     .          s1,s2,s5,azimuth, elev,edot,prn_pick)
            else
c             this can be comented out - kept as debugging, sanity check
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

