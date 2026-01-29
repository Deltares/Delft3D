subroutine moeqdz ( igp,&
&k,&
&ngrid,&
&maxlev,&
&nlev,&
&wft,&
&deltaa,&
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
! Module:             MOEQDZ (MORPHology EQually over Transport Width Delta Z calculation)
!
! Module description: Calculate delta z if morphodynamic option is equ-
!                     ally over transport width.
!
!                     With the level found (MOMLEV) a delta bed level is
!                     calculated. The delta bed level is used to adapt
!                     the levels in the cross section below the found
!                     highest bed level.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  7 deltaa            I  Calculated change in area
!  8 deltaz            O  Calculated change in cross section level
!  1 igp               I  Gridpoint number
!  2 k                 I  Cross section level which is morphodynamic
!                         active
!  4 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  3 ngrid             I  Number of grid points in network.
!  5 nlev(ngrid)       I  Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
!  6 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
!                                 point i.
!                         - For a circle cross section:
!                         (i,1) = Radius of the circle.
!                         - For a sedredge cross section:
!                         (i,1) = Width of main section (i.e. left chan-
!                                 nel).
!                         (i,2) = Width of sub section 1 (i.e. right
!                                 channel).
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: moeqdz.pf,v $
! Revision 1.2  1995/05/30  07:04:43  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:12  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:32:38  kuipe_j
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
   &ngrid,&
   &nlev (ngrid)

   real      wft  (ngrid,maxlev)

   double precision deltaa, deltaz


!
!     Determine delta z
!
   if (k .eq. nlev(igp)) then
      deltaz = deltaa / wft(igp,k)
   else
      deltaz = (2. * deltaa) / (wft(igp,k) + wft(igp,k+1))
   endif

   return
end
