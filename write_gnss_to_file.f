      subroutine write_gnss_to_file(outID, prn, tod, s1,s2,s5,az,
     .   elev,edot, prn_pick)
c     inputs fileID, prn number, time of day (seconds), 
c     s1,s2,s5, azimuth, elevation angles
c     edot, in degrees/second
c     output selection
      implicit none
      include 'local.inc'
      real*8 s1, s2, s5, tod, az, elev, edot
      integer prn_pick,outID,prn
c     asked for all, but < 30 degrees elevation
c     i made 98 and 99 do the same thing so i would 
c     not interfere with my existing file structures at CU
      if (prn_pick.eq.99.or.prn_pick.eq.98) then
        if (elev .ge.5.and.elev.le.30) then
          write(outID,111)
     .      prn, elev, az, tod, edot,0.0, s1, s2,s5
        endif
c     asked for all data > 5
      elseif (prn_pick.eq.88) then
        if (elev.ge.5) then
          write(outID, 111)
     .      prn, elev, az, tod, edot,-99.d0, s1, s2,s5
        endif
c     all data < 10
      elseif (prn_pick.eq.50) then
        if (elev.le.10) then
          write(outID, 111)
     .      prn, elev, az, tod, edot,-99.d0, s1, s2,s5
        endif
      endif
c this format statement gives space for edot and S5
111   format(i3,  2f10.4, f10.0, f10.6, f7.2, 3f7.2)
      end
