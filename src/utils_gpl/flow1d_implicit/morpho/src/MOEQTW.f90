subroutine moeqtw ( igp,&
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
! Module:             MOEQTW (MORPHology erosion/sedimentation spread EQually over Transport Width)
!
!
!
! Module description: This routine calculates new cross section dimensi-
!                     ons. The algoritm used will spread out the erosion
!                     and sedimentation equally over the transport width
!                     of the cross section. The delta bed level is used
!                     to adapt the levels in the cross section below the
!                     found highest bed level.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 deltaz            I  Calculated change in cross section level
!  5 hlev(ngrid,       IO (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  1 igp               I  Gridpoint number
!  2 k                 I  Cross section level which is morphodynamic
!                         active
!  4 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  3 ngrid             I  Number of grid points in network.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: moeqtw.pf,v $
! Revision 1.2  1995/05/30  07:04:44  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:12  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:32:39  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:07  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer   igp,&
   &k,&
   &maxlev,&
   &ngrid

   double precision hlev (ngrid,maxlev),&
   &deltaz

!
!     Variables
!
   integer   i

   do 100 i = 1, k
      hlev(igp,i) = hlev(igp,i) - deltaz
100 continue

   return
end
