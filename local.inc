      integer maxsat, maxob, maxGNSS, np
      parameter (maxsat = 200)
      parameter (maxob = 25)
      parameter (maxGNSS = 400)
c     parameter (np= 288)
C try to accommodate a 48 hour sp3 file
      parameter (np= 576)
      real*8 c, f1, f2
c     parameter    (f1=1575.42d+06)
c     parameter    (f2=1227.60d+06)
      parameter (c = 0.299792458D+09)       ! speed of light (m/sec)

