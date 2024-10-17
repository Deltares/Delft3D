subroutine gmabnd ( igp,   k,     ngrid, maxlev, nlev,  wft,&
&deltaa,deltaz)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         J.Kuipers
!
! Module:             GMABND (Graded Morphology Area for z-BouND)
!
! Module description: Calculate delta A if morphodynamic boundary
!                     condition is z=f(t). Deltaz-Z  is given.
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
! $Log: gmabnd.F,v $
! Revision 1.2  1996/06/07  11:55:04  kuipe_j
! multi  +  fixed layer
!
! Revision 1.1  1996/01/08  13:29:27  kuipe_j
! Multi layer option for under layer added
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
   double precision  deltaa, deltaz


!
!     Determine delta a
!
   if (k .eq. nlev(igp)) then
      deltaa = deltaz * wft(igp,k)
   else
      deltaa = deltaz * (wft(igp,k)+wft(igp,k+1)) * .5
   endif
end
