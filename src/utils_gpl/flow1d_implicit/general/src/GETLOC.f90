subroutine getloc (igr, ibr ,coord)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Kuipers
!
! Module:             GETLOC (GET LOCation of gridpoint)
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 coord             P  -
!  2 ibr               P  -
!  1 igr               P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! getlc1  GET LoCation of gridpoint
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: getloc.pf,v $
! Revision 1.2  1999/03/15  15:51:18  kuipe_j
! tabs removed
!
! Revision 1.1  1995/10/18  08:59:40  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    igr    ,ibr
   real       coord
!
!     Declaration of local variables
!
   integer    nbran  ,branch ,x
!
!     External functions
!
   integer    gtrpnt, gtipnt
   external   gtrpnt, gtipnt
!
!     Include memory pool
!
   include '..\include\mempool.i'

   nbran  = ip(gtipnt('NBRAN' ))
   branch =    gtipnt('BRANCH')
   x      =    gtrpnt('X'     )

   call getlc1 (igr ,nbran ,ip(branch) ,rp(x) ,ibr ,coord)

end

subroutine getlc1 (igr    ,nbran ,branch ,x ,ibr ,coord)
!
!     Declaration of parameters
!
   integer    igr    ,nbran  ,ibr
   integer    branch (4,nbran)
   real       coord
   real       x      (*)
!
!     Declaration of local variables
!
   integer    i1  ,i2
   logical    again

   again = .true.
   ibr   = 1
10 continue
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
