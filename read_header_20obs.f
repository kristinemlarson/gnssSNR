      subroutine read_header(fileID,rawf,xrec,yrec,zrec,
     .  iobs,nobs,iymd,station)
      implicit none
      include 'local.inc'
      integer  i, fileID
      character*80 rawf
      character*80 line, outline, dynfmt, dynfmt2
      logical  endofheader 
      integer nobs,iobs(maxsat), iymd(3), ios
      character*2 key(maxsat)
      character*4 station
      real*8 xrec, yrec, zrec
c     returns receiver coordinates
c     station name is empty to start with
c     returned to main code
      station = '    ' 
      endofheader = .false.

      open(fileID,file=rawf, status='old',iostat=ios)
      if (ios.ne.0) then
        print*, 'problem opening RINEX file' 
        print*, 'name:', rawf
        call exit(0)
      endif
      do while (.not.endofheader)
c     KL 18mar05, fixed bug on nobs
        read (fileID,'(a80)') line
        if (line(61:80).eq.'# / TYPES OF OBSERV') then
          read(line, fmt='(I6)') nobs
c         exit if more than 20 observables
          if (nobs.gt.20) then
             print*,'this code only supports <=20 observ types'
             call exit
          endif
c   KL 19jan09 allowing more lines of OBS types
c         first line has up to 9 OBS types
          if (nobs .lt. 10) then
            write(dynfmt, fmt='(A, I3.3, A)')
     +                      "(6X,", nobs, "(4X,A2))"
            read(line, fmt=dynfmt) (key(i), i=1,nobs)
c         between 10-18 OBS types
          elseif (nobs.gt.9.and.nobs.le.18) then
            write(dynfmt, fmt='(A, I3.3, A)')
     +                      "(6X,", 9, "(4X,A2))"
            read(line, fmt=dynfmt) (key(i), i=1,9)
c           read the next line
            read (fileID,'(a80)') line
            write(dynfmt, fmt='(A, I3.3, A)')
     +                      "(6X,", nobs-9, "(4X,A2))"
            read(line, fmt=dynfmt) (key(i), i=10,nobs)
c           this is more than 18 OBS types
          else
c           first line
            write(dynfmt, fmt='(A, I3.3, A)')
     +                      "(6X,", 9, "(4X,A2))"
            read(line, fmt=dynfmt) (key(i), i=1,9)

c           read the next line
            read (fileID,'(a80)') line
c           reassign this second line
            write(dynfmt, fmt='(A, I3.3, A)')
     +                      "(6X,", 9, "(4X,A2))"
            read(line, fmt=dynfmt) (key(i), i=10,18)

c           read the third line
            read (fileID,'(a80)') line
            write(dynfmt, fmt='(A, I3.3, A)')
     +                      "(6X,", nobs-18, "(4X,A2))"
            read(line, fmt=dynfmt) (key(i), i=19,nobs)

          endif
          print*, 'NUMBER OF OBSERVABLES ', nobs
        else if (line(61:80).eq.'APPROX POSITION XYZ') then
          read(line, fmt= '(3f14.4)') xrec, yrec, zrec
           print*, 'XYZ coordinates ', xrec, yrec, zrec
          if (xrec.eq.0) then
            print*, 'I cannot compute satellite elevation angles'
            print*, 'without apriori receiver coordinates - exiting'
            call exit
          endif
        else if (line(61:77).eq.'TIME OF FIRST OBS') then
          read(line, fmt= '(3i6)') iymd(1), iymd(2), iymd(3)
          print*, 'Time of first Obs: ', iymd
        else if (line(61:71).eq.'MARKER NAME') then
          read(line(1:4), fmt= '(a4)')  station
          print*, 'Station name ', station
        endif
        if (line(61:73).eq.'END OF HEADER'.or.
     +       line(61:73).eq.'end of header'.or.
     +       line(61:73).eq.' ') endofheader = .true.
      enddo
      print*, 'FOUND END OF HEADER'
      do i = 1,maxsat
          iobs(i) = 0
      enddo
      do i = 1, nobs
          if (key(i).eq.'l1' .or. key(i).eq.'L1') iobs(1) = i
          if (key(i).eq.'l2' .or. key(i).eq.'L2') iobs(2) = i
          if (key(i).eq.'c1' .or. key(i).eq.'C1') iobs(3) = i
          if (key(i).eq.'p1' .or. key(i).eq.'P1') iobs(4) = i
          if (key(i).eq.'p2' .or. key(i).eq.'P2') iobs(5) = i
          if (key(i).eq.'s1' .or. key(i).eq.'S1') iobs(6) = i
          if (key(i).eq.'s2' .or. key(i).eq.'S2') iobs(7) = i
          if (key(i).eq.'s5' .or. key(i).eq.'S5') iobs(8) = i
          if (key(i).eq.'s6' .or. key(i).eq.'S6') iobs(9) = i
          if (key(i).eq.'s7' .or. key(i).eq.'S7') iobs(10) = i
          if (key(i).eq.'s8' .or. key(i).eq.'S8') iobs(11) = i
      enddo
      end
