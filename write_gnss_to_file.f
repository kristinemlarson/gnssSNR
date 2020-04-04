      subroutine write_gnss_to_file(outID, prn, tod, s1,s2,s5,az,
     .   elev,edot, prn_pick,s6,s7,s8,tod2)
c     inputs fileID, prn number, time of day (seconds), 
c     s1,s2,s5, azimuth, elevation angles
c     edot, in degrees/second
c     output selection
c     18ocdt01 added galileo frequencies
c     added option 66, which is all data < 30 degrees
c     20mar02 added msec to the output, sent from main code
      implicit none
      include 'local.inc'
      real*8 s1, s2, s5, tod, az, elev, edot,tod2
      real*8 s6, s7, s8 
      integer prn_pick,outID,prn,msec
      logical galileo
c     asked for all, but < 30 degrees elevation
c     i made 98 and 99 do the same thing so i would 
c     not interfere with my existing file structures at CU
c
c     2020 mar 02
c     try to use actual time, not the integer time
c     print*, msec
      msec = 0
c      tod = tod + msec/1000.d0
c      try this ...
      tod=tod2
      if (prn_pick.eq.99.or.prn_pick.eq.98) then
        if (elev .ge.5.and.elev.le.30) then
          write(outID,112)
     .      prn, elev, az, tod, edot,s6, s1, s2,s5,s7,s8
        endif
c         asked for all data > 5
      elseif (prn_pick.eq.88) then
        if (elev.ge.5) then
          write(outID, 112)
     .        prn, elev, az, tod, edot,s6, s1, s2,s5,s7,s8
        endif
c     all data below 30
c     KL 2019Sep25 fixed bug found by YNakashima 
      elseif (prn_pick.eq.66.and.elev.le.30) then
          write(outID, 112) prn, elev, az, tod, 
     .   edot,s6, s1, s2,s5,s7,s8
c     all data < 10
      elseif (prn_pick.eq.50) then
        if (elev.le.10) then
          write(outID, 112)
     .        prn, elev, az, tod, edot,s6, s1, s2,s5,s7,s8
        endif
      endif
c this format statement gives space for edot and S5
111   format(i3,  2f10.4, f10.0, f10.6, f7.2, 3f7.2)
c this format allows galileo
112   format(i3,  2f10.4, f10.1, f10.6, f7.2, 5f7.2)
      end
