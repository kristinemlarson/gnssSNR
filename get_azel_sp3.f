      subroutine get_azel_sp3(tc, prn, stationXYZ,East,North,Up,
     .  Lat,Long,Ht, azimuth,elev,t9,x9,y9,z9)
      implicit none
      include 'local.inc'
      integer jj, ierr,prn, ihr, t(9),polyorder
      real*8 tc, SatPos(3), toffset, omeg, ghadot,
     . xrec, yrec, zrec,xnew, ynew, znew, tod,
     . azimuth, elev, North(3), East(3), Up(3),stationXYZ(3), range2,
     . Lat,Long,Ht, t9(9), x9(9),y9(9), z9(9), outX,outY,outZ,dy
c    this code requests the azel for a time (tc) and satellite (prn)
c    t,x9,y9,and z9 are vectors of precise Cartesian GLONASS coordinates (time in 
c     GPS seconds, xyz in meters
c     stationXYZ is in meters
c     returns azimuth and elev in radians (likely)
c     dy is a sigma from the interpolator
c     t9 is real*8 !!!!!
c
c     KL 19feb04 - need to make changes for multi-day sp3 files.
c     t9 is now relative to sp3 epoch time. which means tc should not be used,
c     but rather tc relative to sp3 epoch time

c     starting value for transmit time
      toffset = 0.07
c     Earth Rotation
      ghadot = 7.2921151467d-5 ! rad/sec, using same one as ICD200
      polyorder = 9
c     do this three times - should converge
      do jj = 1, 3
c       interpolate for X,Y, and Z
        call polint(t9, x9, polyorder, tc-toffset, outX, dy)
        call polint(t9, y9, polyorder, tc-toffset, outY, dy)
        call polint(t9, z9, polyorder, tc-toffset, outZ, dy)
        omeg = -ghadot*toffset
        xnew = outX*dcos(omeg) - outY*dsin(omeg)
        ynew = outX*dsin(omeg) + outY*dcos(omeg)
        znew = outZ
        range2= (xnew - stationXYZ(1))**2 + (ynew 
     . - stationXYZ(2))**2 + (znew-stationXYZ(3))**2
c       new time is the geometric range divided by the speed of light
        toffset = dsqrt(range2)/c
      enddo
      SatPos(1) =  xnew
      SatPos(2) =  ynew
      SatPos(3) =  znew
      call azel(azimuth, elev, stationXYZ, East, North, Up, SatPos)
      end

