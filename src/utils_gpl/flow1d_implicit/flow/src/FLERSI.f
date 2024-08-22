      subroutine FLERSI (branch ,x      ,nexres ,exres  ,juer   ,ker  )
c
c     Declaration of Parameters:
c
      integer nexres, juer, ker, branch(4,*)
      real    x(*), exres(3,*)
c
c     Declaration of local variables:
c
      integer i1, i2, ibr, j, igr, lbrnam
      character*40   branam
c
c     Include sobek constants and error codes
c
      include '..\include\errcod.i'
c
      do 20 j = 1, nexres
c
c        get branch number -ibr-
c
         ibr = int(exres(1,j))
         i1  = branch (3,ibr)
         i2  = branch (4,ibr)
c
         if ( exres(2,j) .lt. x(i1) .or.
     +        exres(2,j) .gt. x(i2) ) then
            call getbrn (ibr,branam,lbrnam)
            ker = fatal
            call error (juer,'FLERSI Coordinate of head loss not in bran
     &ch @'//branam(:lbrnam)//'@', eflnib, ker)
         else
c
c           Loop over grid points in branch
c
            do 10 igr = i1, i2-1
               if ( exres(2,j) .ge. x(igr) .and.
     +              exres(2,j) .le. x(igr+1) ) then
                  exres(2,j) = igr
               endif
   10       continue
         endif
   20 continue
c
      end
