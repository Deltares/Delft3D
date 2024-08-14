      subroutine getloc (igr, ibr ,coord)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Kuipers
c
c Module:             GETLOC (GET LOCation of gridpoint)
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 coord             P  -
c  2 ibr               P  -
c  1 igr               P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c getlc1  GET LoCation of gridpoint
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: getloc.pf,v $
c Revision 1.2  1999/03/15  15:51:18  kuipe_j
c tabs removed
c
c Revision 1.1  1995/10/18  08:59:40  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    igr    ,ibr
      real       coord
c
c     Declaration of local variables
c
      integer    nbran  ,branch ,x
c
c     External functions
c
      integer    gtrpnt, gtipnt
      external   gtrpnt, gtipnt
c
c     Include memory pool
c
      include '..\include\mempool.i'

      nbran  = ip(gtipnt('NBRAN' ))
      branch =    gtipnt('BRANCH')
      x      =    gtrpnt('X'     )

      call getlc1 (igr ,nbran ,ip(branch) ,rp(x) ,ibr ,coord)

      end

      subroutine getlc1 (igr    ,nbran ,branch ,x ,ibr ,coord)
c
c     Declaration of parameters
c
      integer    igr    ,nbran  ,ibr
      integer    branch (4,nbran)
      real       coord
      real       x      (*)
c
c     Declaration of local variables
c
      integer    i1  ,i2
      logical    again

      again = .true.
      ibr   = 1
 10   continue
      if (again) then
         i1 = branch(3,ibr)
         i2 = branch(4,ibr)
         if (igr .ge. i1 .and. igr .le. i2) then
            again = .false.
            coord = x(igr)
         else if (ibr .eq. nbran) then
            again = .false.
            ibr   = 0
            coord = 0.
         else
           ibr = ibr + 1
         endif
         goto 10
      endif

      end
