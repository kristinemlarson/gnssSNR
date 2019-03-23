c
c
c
      subroutine getlogical(logname, filename)
      implicit none
c
      character*60 SccsID
      data SccsID /'@(#) getlogical.f   1.1    3/6/94 \0'/
 
      character*(*)    logname, filename
c
      filename = ' '
      call getenv(logname, filename)
c
      return
      end
c
c
c
      subroutine timedate(string)
      implicit none
      character*60 SccsID
      data SccsID /'@(#) timedate.f   1.1    3/6/94 \0'/
 
c
      character*25    string
c
c
      call fdate(string)
c
      return
      end

