      subroutine motrgp(ngrid  ,x      ,branch ,bno    ,x1     ,x2     ,
     &                  gp1    ,gp2    ,juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOTRGP (MOrphology TRajectory in GridPoints)
c
c Module description: Convert trajectory to gridpoints
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 bno               I  Branch number.
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  7 gp1               O  Gridpoint at begin of trajectory.
c  8 gp2               O  Gridpoint at end of trajectory.
c  9 juer              P  -
c 10 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  1 ngrid             I  Number of grid points in network.
c  5 x1                I  X-coordinate at begin of trajectory.
c  2 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c  6 x2                I  X-coordinate at end of trajectory.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c epsequ  EQUal test with interval EPSilon
c error   write an ERROR to the error file.
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: motrgp.pf,v $
c Revision 1.4  1999/06/01  13:42:33  kuipe_j
c names in messages substituted + message template
c
c Revision 1.3  1999/03/15  15:53:07  kuipe_j
c tabs removed
c
c Revision 1.2  1997/06/17  11:27:00  kuipe_j
c output in history format
c
c Revision 1.1  1995/10/18  09:00:07  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer        ngrid, bno, gp1, gp2, juer, ker
      integer        branch(4,*)
      real           x1, x2
      real           x(ngrid)
c
c     Declaration of local variables:
c
      character      txt*4
      integer        i
      logical        bfnd, efnd
      logical        epsequ
      external       epsequ
c
c     Include Sobek error codes
c
      include '..\include\errcod.i'
c
c     Find corresponding grid points
c
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
 20   continue

      if (.not. bfnd) then
         ker = fatal
         write (txt,'(f12.0)') x1
         call error (juer,'MORP Begin coordinate not found@'//txt//'@',
     &               embeco,ker)
      elseif (.not. efnd) then
         ker = fatal
         write (txt,'(f12.0)') x2
         call error (juer,'MORP End coordinate not found@'//txt//'@',
     &               emenco,ker)
      endif

      end
