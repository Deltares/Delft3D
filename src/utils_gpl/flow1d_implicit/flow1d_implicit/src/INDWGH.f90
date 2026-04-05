subroutine INDWGH (ngrid , i     ,&
&maxlev, nlev  , hlev  ,&
&hact  , inter , wght  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Brouwer
!
! Module:             INDWGH (Compute INDex and WeiGHt factor)
!
! Module description: Computing of the index in array hlev and a weight
!                     factor.
!                     Explanation:
!                     hlev(i,j) : j-th water level in table for grid-
!                     point i.
!                     wtab(i,j) : j-th width/area in table for gridpoint
!                     i
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
!  5 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  2 i                 I  Gridpoint index in branch.
!  7 inter             O  Level in table hlev(i,j).
!  3 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  1 ngrid             I  Number of grid points in network.
!  4 nlev(ngrid)       I  Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
!  8 wght              O  Weight factor.
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
! $Log: indwgh.pf,v $
! Revision 1.3  1999/03/15  15:51:19  kuipe_j
! tabs removed
!
! Revision 1.2  1995/05/30  07:02:25  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:29  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:32:01  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:59  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     declaration of Parameters:
!
   integer ngrid, i, maxlev, inter
   integer nlev(ngrid)
   double precision hlev(ngrid,maxlev), wght, hact
!
!     declaration of local variables:
!
   integer j
   double precision h1, h2
   logical inner
!
   if ( hact .gt. hlev(i,nlev(i)) ) then
      inter = -1
   else
!
!        1. Find interval inter (j = current interval)
!
      j = 0
      inner = .false.
10    continue
      if (.not. inner .and. j .lt. (nlev(i)-1) ) then

         j     = j + 1

         h1    = hlev(i,j)
         h2    = hlev(i,j+1)
         inner = hact.ge.h1 .and. hact.le.h2

         go to 10
      endif
!
      if (inner) then
         inter = j
!
!           2. Compute weight factor
!
!           >> Definition weight factor :
!              w(h) := (1-wght) * w(h1) + wght * w(h2)
!
         wght = (hact-h1) / (h2-h1)

      else

!           write(*,*) 'Internal failure: h<bodem', i, j,h, hlev(i,1)

         inter = -1
      endif

   endif
end
