      subroutine pick_9points(sp3_nsat, sp3_satnames, sp3_gps_weeks,
     . sp3_gps_seconds, sp3_XYZ, ichoice, gpsweek,gpstime,ihr,
     . t9,x9,y9,z9,ipointer,nepochs,sp3_rel_seconds,relTime)
c    Author: kristine m. larson originally
c    added pointer for orbits october 2017
c    17nov03
c    sent nepochs to the code
c    changed 96 to 288 values -but now some of this doesn't quite work ... hmmm.
c    KL, 19feb04
c    sent time relative to sp3 epoch time. this allows the code to use sp3 files
c    that cross midnite, i.e. have more than one day in them.
c    this means that the relative time is sent as well for the epoch you are 
c    considering.
c
c    inputs: 
c      sp3 - entire sp3 file contents, with names, weeks, etc
c      ichoice is the satellite number (?) you want 
c      gpsweek and gpstime is the time (week and seconds of the week) you want 
c      ihr is hour of the day - I do not know why this is sent as it is not used.
c      ipointer is something telling you which satellite you are choosing.. do not remember
c      the details
c
c      nepochs is how many sp3 time blocks there are
c      sp3_rel_seconds are seconds with respect to the sp3 epoch time.  used instead
c      of GPS seconds so that you can interpolate over a GPS week change.
c      relTime is the epoch time in time relative to sp3 epoch time.
c    outputs:
c      t9,x9,y9,z9 are the time and cartesian coordinates for the 9 points 
c      you will use for doing the fit for the satellite position.  units are 
c      seconds (relative to sp3 first epoch) and position in meters
c
c
c    19feb04 - I am assuming we should use sp3_rel_seconds and relTime now
      implicit none
      include 'local.inc'
      integer sp3_gps_weeks(np), sp3_nsat, sp3_satnames(maxsat),gpsweek,
     .  ihr,i,isat,iv,k, i1,i2, trigpt,ivnew
      real*8 sp3_XYZ(maxsat,np,3), sp3_gps_seconds(np), gpstime, t9(9), 
     .  x9(9), y9(9), z9(9), firstGPS, sp3_rel_seconds(np), relTime,
     .  delta
      integer ipointer(maxGNSS), ichoice, nepochs
c     returns x9,y9,z9 in meters for glonass satellites
c     as of 19feb04, t9 is in seconds relative to sp3 epoch time
c
c     set output arrays to zero
      do i=1,9
        t9(i) = 0
        x9(i) = 0
        y9(i) = 0
        z9(i) = 0
      enddo
c     17oct15 need to change isat to the  pointer value
      isat = ipointer(ichoice)
c     so isat will be a smaller number than ichoice, typically 
c     assume for now that the gpsweek is right (hah!)
c     firstGPS = sp3_gps_seconds(1)
c     change the logic to 9 closest, period.  used to hardwire
c     midnite time periods for 15 minute sp3 files

      delta = sp3_rel_seconds(2) - sp3_rel_seconds(1)
      ivnew = 1 + relTime /delta
c     this is the old way of picking indices
c       pick nine closest
c     if (nepochs.eq.96) then
c        iv = 1 + (gpstime - firstGPS)/(15*60)
c     elseif (nepochs.eq.288) then
c        iv = 1 + (gpstime - firstGPS)/(5*60)
c     endif
c     try using ivnew instead of iv (previous logic was for hardwired
c     sp3 files
      trigpt = nepochs -5
      iv = ivnew
      if (iv.lt.5) then
c       use first 9
        i1 = 1
        i2 = 9
      elseif (iv.gt.trigpt) then
c       use last 9
        i1=nepochs-8
        i2=nepochs
      else
        i1 = iv-4 
        i2 = iv+4 
      endif
      k=0
105   format(a15,f10.0, i3, i5, i5,i5,i5)
c     for debugging
c     write(17,105) 'T H i1 i2 iv N ', gpstime, ihr, i1, 
c    . i2, ivnew, nepochs
      do i=i1, i2
        k=k+1
c       change to gpstime to relative seconds
        t9(k) = sp3_rel_seconds(i)
c       t9(k) = sp3_gps_seconds(i)
        x9(k) = sp3_XYZ(isat,i,1)*1000
        y9(k) = sp3_XYZ(isat,i,2)*1000
        z9(k) = sp3_XYZ(isat,i,3)*1000
      enddo
      end
