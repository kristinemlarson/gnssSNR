      subroutine read_block(fileID, flag,inline,numsat,nobs,satID,
     .  prn,obs,snr,lli)
      implicit none
      include 'local.inc'
      character*1 char, satID(maxsat)
      integer fileID, i, itrack, flag, nobs, numsat, sec
      integer prn(maxsat), ios, nsat
      character*80 inline, dynfmt, dynfmt2, anotherline,dynfmt3
      real*8  obs(maxob,maxsat) 
      integer  lli(maxob,maxsat), snr(maxob,maxsat) 
c     KL remove clockerr - was not using it - perhaps making blocks crash 
c     for certain compilers
c     KL - put lli and snr into integer arrays, previously misdefined
c     kl allow up to 15 observables now
c         and 36 satellites
      if (flag.le.1 .or. flag.eq.6) then
        read(inline(33:80),'(12(A1,I2))')
     +         (char, prn(i),i=1,12)
        anotherline = inline
        read(inline(33:80),'(12(A1,2x))') (satID(i),i=1,12) 
c       print*, 'need to read extra lines'
        if (numsat > 12 .and. numsat < 25) then
          read(fileID,'(A80)', iostat=ios) inline
          read(inline(33:80),'(12(A1,I2))') (char, prn(i),i=13,numsat) 
          read(inline(33:80),'(12(A1,2x))') (satID(i),i=13,numsat)
        elseif (numsat > 24 .and. numsat <= 36) then
          read(fileID,'(A80)', iostat=ios) inline
          read(inline(33:80),'(12(A1,I2))') (char, prn(i),i=13,24)
          read(inline(33:80),'(12(A1,2x))') (satID(i),i=13,24)
          read(fileID,'(A80)', iostat=ios) inline
          read(inline(33:80),'(12(A1,I2))') (char, prn(i),i=25,numsat) 
          read(inline(33:80),'(12(A1,2x))') (satID(i),i=25,numsat)
        endif
        if (numsat > 36) then
          print*, 'I cannot read more than 36 satellites'
          call exit
        endif
c       I need to rename the satellites now
        do i =1, numsat
          call newSat(satID(i), prn(i),nsat)
          prn(i) = nsat
        enddo
        if (flag .le. 1) then
          do itrack = 1, numsat
            if (nobs.gt.5 .and. nobs .le. 10) then
              write(dynfmt, fmt='(A, I3.3, A)')
     +           "(", 5, "(F14.3, 2I1))"
              write(dynfmt2, fmt='(A, I3.3, A)')
     +            "(5(F14.3, 2I1),/,'//'", nobs-5, "(F14.3, 2I1))"
              read(fileID, fmt=dynfmt, iostat=ios)
     +           (obs(i,itrack),lli(i,itrack),snr(i,itrack), i=1,5)
              read(fileID, fmt=dynfmt2, iostat=ios)
     +             (obs(i,itrack),lli(i,itrack),snr(i,itrack), i=6,nobs)
            elseif (nobs.gt.10) then

              write(dynfmt, fmt='(A, I3.3, A)')
     +           "(", 5, "(F14.3, 2I1))"
              write(dynfmt2, fmt='(A, I3.3, A)')
     +           "(", 5, "(F14.3, 2I1))"
              write(dynfmt3, fmt='(A, I3.3, A)')
     +            "(5(F14.3, 2I1),/,'//'", nobs-5, "(F14.3, 2I1))"

              read(fileID, fmt=dynfmt, iostat=ios)
     +           (obs(i,itrack),lli(i,itrack),snr(i,itrack), i=1,5)
              read(fileID, fmt=dynfmt2, iostat=ios)
     +           (obs(i,itrack),lli(i,itrack),snr(i,itrack), i=6,10)
              read(fileID, fmt=dynfmt3, iostat=ios)
     +           (obs(i,itrack),lli(i,itrack),snr(i,itrack), i=11,nobs)

            else
              write(dynfmt, fmt='(A, I3.3, A)')
     +           "(", nobs, "(F14.3, 2I1))"
              read(fileID, fmt=dynfmt, iostat=ios)
     +           (obs(i,itrack),lli(i,itrack),snr(i,itrack), i=1,nobs)
            endif
          enddo
        endif
      else
        do itrack = 1, numsat
          read(fileID, fmt='(A80)', iostat=ios) inline
        enddo
      endif
      end
