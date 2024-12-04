subroutine saforf (nbran ,ngrid ,ceps ,branch ,csa2  ,filc )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SAFORF (SAlt FORester Filter)
!
! Module description: Execute the filter on the calculated
!                     concentration field (was in older versions a
!                     Forrester filter) Now negative concentrations are
!                     simply made equal to EPSILON.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  3 ceps              I  Minimum allowed concentration.
!  5 csa2(ngrid)       IO Salt concentration in every grid point at time
!                         t(n+1).
!  6 filc(ngrid)       O  1  = negative concentration is filterred away
!                         -1 = calculated concentration is positive
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: saforf.pf,v $
! Revision 1.5  1995/10/18  09:00:22  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.4  1995/09/22  10:03:18  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/08/23  14:29:40  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.2  1995/05/30  07:06:03  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:45  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:13  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    nbran  ,ngrid
   integer    branch(4,nbran)
   real       ceps
   real       csa2(ngrid),filc(ngrid)
!
!     Declaration of local variables
!
   integer    i1    ,i2    ,ibr  ,igr
!
!     Filter in branches (only at inner points)
!
   do 40 ibr = 1,nbran
      i1     = branch(3,ibr)
      i2     = branch(4,ibr)
!
!        Determine filter flag in every grid point (also at
!        grid ends).
!
      do 10 igr = i1,i2
         if (csa2(igr) .lt. abs(ceps)) then
            csa2(igr) =    abs(ceps)
            filc(igr) =    1.0
         else
            filc(igr) =    -1.0
         endif
10    continue
!
40 continue
!
end
