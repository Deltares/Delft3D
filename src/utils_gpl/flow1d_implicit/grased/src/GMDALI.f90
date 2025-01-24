subroutine GMDALI ( igpm1  ,igp    ,igpp1  ,ngrid ,nfrac  ,dtm   ,&
&intiph ,intimh ,source ,x     ,deltaa )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             GMDALI (Graded Morphology Delta Area for
!                             Last Internal point)
!
! Module description: This subroutine calculates the change in area for
!                     the last internal point. This point uses the cal-
!                     culated integral values Ii+1/2 (last value of
!                     MODAIP) and In-1/2 (value from MODAEP). This is
!                     shown below:
!
!                     |         |         |
!                     -----x---------x-----
!                     |         |         |
!                        i+1/2 n-1  n-1/2 n
!                         /     |     \
!                        /      |      \
!                     MODAIP    MODALI   MODAEP
!
!                     The lateral sediment on point n-1/2 will be assig-
!                     ned to the point n-1.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 11 deltaa            O  Calculated change in area
!  6 dtm               I  Morphology time step
!  2 igp               I  Gridpoint number
!  1 igpm1             I  Gridpoint number - 1
!  3 igpp1             I  Gridpoint number + 1
!  7 intiph            I  Calculated integral value on i + 1/2
!  8 intnmh            I  Calculated integral value on n - 1/2
!  4 isec              I  Section number (1 or 2)
!  5 ngrid             I  Number of grid points in network.
!  9 slat(ngrid,*)     I  Actual lateral sediment transport in grid
!              1|2        point i+1/2 for:
!                         (i,1) = Main or Left channel.
!                         (i,2) = Right channel.
! 10 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gmdali.F,v $
! Revision 1.2  1995/09/27  10:11:31  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!     Parameters
!
   integer  igpm1  ,igp    ,igpp1  ,ngrid  ,nfrac
   real     source (ngrid,nfrac+2) ,&
   &x      (ngrid)         ,&
   &intiph (nfrac)         ,intimh (nfrac)
   double precision dtm, deltaa (ngrid,nfrac+1)
!
!     Local variables
!
   integer  jf
   real     rdx    ,ili    ,sum  ,dtm2
!
!     Calculate dx
!
   rdx  = 2. / (x(igpp1) - x(igpm1))
   dtm2 = sngl(dtm) * .5

   do 10 jf=1,nfrac
!
      ili    = (source(igpm1,jf)+source(igp,jf)) * dtm2
!
!        Calculate delta A
!
      deltaa(igp,jf) = ( intimh(jf)-intiph(jf) - ili ) * rdx
10 continue
!
   sum = 0.
   do 20 jf=1,nfrac
      sum = sum + deltaa(igp,jf)
20 continue
   deltaa(igp,nfrac+1) = sum

end
