subroutine moprdz ( igp,&
&hws,&
&k,&
&ngrid,&
&maxlev,&
&nlev,&
&hlev,&
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
! Module:             MOPRDZ (MORPHology PRoportional Delta Z calculation)
!
! Module description: Calculate delta z if morphodynamic option is pro-
!                     portional to local depth.
!
!                     With the level found (MOMLEV) a delta bed level is
!                     calculated. The delta bed level is used to adapt
!                     the levels in the cross section below the found
!                     highest bed level.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  9 deltaa            I  Calculated change in area
! 10 deltaz            O  Calculated change in cross section level
!  7 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
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
!  6 nlev(ngrid)       I  Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
!  8 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
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
! $Log: moprdz.pf,v $
! Revision 1.4  1999/03/15  15:53:00  kuipe_j
! tabs removed
!
! Revision 1.3  1996/05/31  12:57:01  kuipe_j
! keep mimimum level for proportional distr.
!
! Revision 1.2  1995/05/30  07:04:54  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:22  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:32:53  kuipe_j
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
   &ngrid,&
   &nlev (ngrid)

   real      wft  (ngrid,maxlev)

   double precision hlev (ngrid,maxlev),&
   &hws, deltaa, deltaz
!
!       Variables
!
   integer   i
   double precision alpha1,&
   &alpha2,&
   &sum1,&
   &sum2,&
   &denum

!
!     Initialise counters
!
   alpha1 = 1d0
   sum1   = 0d0
   sum2   = 0d0

!
!     Denumerator fixed in loop
!
   denum = dmax1(hws - hlev(igp,1) , 1.0d-6 )

!
!     Calculate sum1 and sum2
!
   do 100 i = 1, k-1
      alpha2 = (hws - hlev(igp,i+1)) / denum
      sum1 = sum1 + (alpha1 * wft(igp,i+1))
      sum2 = sum2 + (alpha2 * wft(igp,i))

      alpha1 = alpha2
100 continue

!
!     Calculate last contribution sum1
!
   if (k .eq. nlev(igp)) then
      sum1 = sum1 + (alpha1 * wft(igp,k))
   else
      sum1 = sum1 + (alpha1 * wft(igp,k+1))
   endif

!
!     Calculate delta z
!
   deltaz = (2. * deltaa) / (wft(igp,1) + sum1 - sum2)

   return
end
