subroutine moprld ( igp,&
&hws,&
&k,&
&ngrid,&
&maxlev,&
&hlev,&
&deltaz&
&)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOPRLD (MORPHology erosion/sedimentation PRoportional to Local Depth)
!
! Module description: Calculate new cross section
!
!                     This routine calculates new cross section dimensi-
!                     ons. The algoritm used will distribute the erosion
!                     and and sedimentation proportional to the local
!                     water depth in the cross section. The delta bed
!                     level is used to adapt the levels in the cross
!                     section below the found highest bed level.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  7 deltaz            I  Calculated change in cross section level
!  6 hlev(ngrid,       IO (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  2 hws               I  Water level for sediment transporting width
!  1 igp               I  Gridpoint number
!  3 k                 I  Cross section level which is morphodynamic
!                         active
!  5 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  4 ngrid             I  Number of grid points in network.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: moprld.pf,v $
! Revision 1.4  1999/03/15  15:53:02  kuipe_j
! tabs removed
!
! Revision 1.3  1996/05/31  12:57:03  kuipe_j
! keep mimimum level for proportional distr.
!
! Revision 1.2  1995/05/30  07:04:56  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:23  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:32:54  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:09  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!       Parameters
!
   integer   igp,&
   &k,&
   &maxlev,&
   &ngrid
!
   double precision hws,&
   &hlev (ngrid,maxlev),&
   &deltaz

!
!     Variables
!
   double precision denum, alpha
!
   integer   i

!
!     Denumerator fixed in loop
!
   denum = dmax1(hws - hlev(igp,1) , 1d-6 )
!
!     Calculate adapted bed levels
!
   do 100 i = 1, k
      alpha = (hws - hlev(igp,i)) / denum
      hlev(igp,i) = hlev(igp,i) - (alpha * deltaz)
100 continue

   return
end
