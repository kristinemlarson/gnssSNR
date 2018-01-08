      subroutine pick_9points(sp3_nsat, sp3_satnames, sp3_gps_weeks,
     . sp3_gps_seconds, sp3_XYZ, ichoice, gpsweek,gpstime,ihr,
     . t9,x9,y9,z9,ipointer,nepochs )
c    kristine larson originally
c    added pointer for orbits october 2017
c    17nov03
c    changed 96 to 288 values -but now some of this doesn't quite work ... hmmm.
c    sent nepochs to the code
c    inputs are
c      sp3 - entire sp3 file contents, with names, weeks, etc
c      ichoice is the satellite you want 
c      gpsweek and seconds is the time you want as well
      implicit none
      include 'local.inc'
      integer sp3_gps_weeks(np), sp3_nsat, sp3_satnames(maxsat),gpsweek,
     .  ihr,i,isat,iv,k, i1,i2, trigpt
      real*8 sp3_XYZ(maxsat,np,3), sp3_gps_seconds(np), gpstime, t9(9), 
     .  x9(9), y9(9), z9(9), firstGPS
      integer ipointer(maxGNSS), ichoice, nepochs
c     returns x9,y9,z9 in meters for glonass satellites
c     t9 is in gps seconds 
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
      firstGPS = sp3_gps_seconds(1)
c     change the logic to 9 closest, period.  used to hardwire
c     midnite time periods for 15 minute sp3 files
c       pick nine closest
      if (nepochs.eq.96) then
         iv = 1 + (gpstime - firstGPS)/(15*60)
      elseif (nepochs.eq.288) then
         iv = 1 + (gpstime - firstGPS)/(5*60)
      endif
      trigpt = nepochs -5
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
c     write(17,*)  gpstime, ihr,iv, i1, i2
      do i=i1, i2
        k=k+1
        t9(k) = sp3_gps_seconds(i)
        x9(k) = sp3_XYZ(isat,i,1)*1000
        y9(k) = sp3_XYZ(isat,i,2)*1000
        z9(k) = sp3_XYZ(isat,i,3)*1000
      enddo
      end
