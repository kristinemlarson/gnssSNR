      subroutine moving_sites(station, year, month, day, 
     .     xrec, yrec,zrec,fsite)
      implicit none
      character*4 station, cstation
      integer y, m, d, ios, year, month, day,jtime(5)
      logical eof, fsite
      real*8 xx,yy,zz,dx,dy,dz, xrec, yrec,zrec
      real*8 tjul0, trecord, rho, tjul
      character*80 tmp, filename
c     default outputs to zero and false
c     a priori coordinates and velocity file
c     filename = '/gipsy/source/RinexGNSSv2/knut.txt'
c     filename = 'knut.txt'
      call getlogical ('COORDS', filename)

      print*, year, month, day
      eof = .false.
      fsite = .false.
      xrec = 0.d0
      yrec = 0.d0
      zrec = 0.d0
      rho = 0.d0
      jtime(4) = 0
      jtime(5) = 0
      jtime(1) = month
      jtime(2) = day
      jtime(3) = year
      call julday(jtime,rho,tjul)
      print*, station, year, month, day, 'MJD ', tjul
c     requesting XYZ for a station at time t, in years
c     not meant to be super precise cause it is not needed for reflectometry

      open(17,file=filename,status='old',iostat = ios)
      if (ios.ne.0) then
        print*, 'did not find input file of coordinates'
        goto 102
      endif
c     skip two header lines
      read(17,'(a80)') tmp
      read(17,'(a80)') tmp
      do while (.not.eof)
        read(17,'(A4, 3F16.3, 3F11.3,i6,i3,i3)',iostat=ios )
     .      cstation, xx,yy,zz,dx,dy,dz,y,m,d
        jtime(1) = m
        jtime(2) = d
        jtime(3) = y
c       reference mJD is in tjul0
        call julday(jtime,rho,tjul0)
        if (ios.ne.0) goto 101
c       time of record
c       change time from days to years by div 365.25
        if (station .eq. cstation) then
          write(6,*) 'POS at epoch', xx, yy, zz
          print*, 'velocities', dx, dy, dz
          print*,'DELtime', (tjul-tjul0)/365.25
          xrec = xx +dx*(tjul-tjul0)/365.25
          yrec = yy +dy*(tjul-tjul0)/365.25
          zrec = zz +dz*(tjul-tjul0)/365.25
c         write(6,*)' now', xrec, yrec, zrec
c         flag that you found the site
          fsite = .true. 
          goto 101
        endif
      enddo
101   continue
      close(17)
102   continue
      end
