subroutine FLERSI (branch ,x      ,nexres ,exres  ,juer   ,ker  )
!
!     Declaration of Parameters:
!
   integer nexres, juer, ker, branch(4,*)
   real    x(*), exres(3,*)
!
!     Declaration of local variables:
!
   integer i1, i2, ibr, j, igr, lbrnam
   character(len=40) branam
!
!     Include sobek constants and error codes
!
   include '../include/errcod.i'
!
   do 20 j = 1, nexres
!
!        get branch number -ibr-
!
      ibr = int(exres(1,j))
      i1  = branch (3,ibr)
      i2  = branch (4,ibr)
!
      if ( exres(2,j) .lt. x(i1) .or.&
      &exres(2,j) .gt. x(i2) ) then
         call getbrn (ibr,branam,lbrnam)
         ker = fatal
         call sre_error (juer,'FLERSI Coordinate of head loss not in bran&
         &ch @'//branam(:lbrnam)//'@', eflnib, ker)
      else
!
!           Loop over grid points in branch
!
         do 10 igr = i1, i2-1
            if ( exres(2,j) .ge. x(igr) .and.&
            &exres(2,j) .le. x(igr+1) ) then
               exres(2,j) = igr
            endif
10       continue
      endif
20 continue
!
end
