subroutine motrgp(ngrid  ,x      ,branch ,bno    ,x1     ,x2     ,&
&gp1    ,gp2    ,juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOTRGP (MOrphology TRajectory in GridPoints)
!
! Module description: Convert trajectory to gridpoints
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 bno               I  Branch number.
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  7 gp1               O  Gridpoint at begin of trajectory.
!  8 gp2               O  Gridpoint at end of trajectory.
!  9 juer              P  -
! 10 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  1 ngrid             I  Number of grid points in network.
!  5 x1                I  X-coordinate at begin of trajectory.
!  2 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!  6 x2                I  X-coordinate at end of trajectory.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! epsequ  EQUal test with interval EPSilon
! error   write an ERROR to the error file.
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: motrgp.pf,v $
! Revision 1.4  1999/06/01  13:42:33  kuipe_j
! names in messages substituted + message template
!
! Revision 1.3  1999/03/15  15:53:07  kuipe_j
! tabs removed
!
! Revision 1.2  1997/06/17  11:27:00  kuipe_j
! output in history format
!
! Revision 1.1  1995/10/18  09:00:07  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer        ngrid, bno, gp1, gp2, juer, ker
   integer        branch(4,*)
   real           x1, x2
   real           x(ngrid)
!
!     Declaration of local variables:
!
   character      txt*4
   integer        i
   logical        bfnd, efnd
   logical        epsequ
   external       epsequ
!
!     Include Sobek error codes
!
   include '..\include\errcod.i'
!
!     Find corresponding grid points
!
   bfnd = .false.
   efnd = .false.

   do 20 i = branch(3,bno), branch(4,bno)
      if (x(i) .le. x1) then
         bfnd = .true.
         gp1  = i
      elseif (.not. bfnd) then
         bfnd = epsequ (x(i),x1,.01)
      endif
      if (.not. efnd) then
         gp2 = i
         if (x(i) .ge. x2) then
            efnd = .true.
         elseif (.not. efnd) then
            efnd = epsequ (x(i),x2,.01)
         endif
      endif
20 continue

   if (.not. bfnd) then
      ker = fatal
      write (txt,'(f12.0)') x1
      call error (juer,'MORP Begin coordinate not found@'//txt//'@',&
      &embeco,ker)
   elseif (.not. efnd) then
      ker = fatal
      write (txt,'(f12.0)') x2
      call error (juer,'MORP End coordinate not found@'//txt//'@',&
      &emenco,ker)
   endif

end
