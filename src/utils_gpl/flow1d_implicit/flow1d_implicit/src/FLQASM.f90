subroutine flqasm (nbran  ,ngrid  ,branch ,grid ,hq1  ,hq2)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLQASM (FLow QqA will be SMoothed)
!
! Module description: Smoothing of parameter Q * Q/A in all interior
!                     grid points, structure cells excluded
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  4 grid(ngrid)       I  Grid cell type definition:
!                         cgrdcl (1) : Normal grid cell
!                         cstrcl (2) : Structure cell
!  5 hq1(ngrid)        IO (to be) smoothed array
!  6 hq2(ngrid)        IO scratch array
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flqasm.pf,v $
! Revision 1.3  1999/03/15  15:50:29  kuipe_j
! tabs removed
!
! Revision 1.2  1995/10/18  08:59:24  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.1  1995/09/29  10:36:16  kuipe_j
! Improvement of autostart and simple weir
!
!
!***********************************************************************
!
!
!     Declaration of parameters
!
   integer    nbran ,ngrid
   integer    branch(4,nbran), grid(ngrid)
   double precision hq1(ngrid)     ,hq2(ngrid)
!
!     Declaration of local variables
!
   integer    i1  ,i2  ,ibr   ,igr

   do 30 ibr = 1, nbran
      i1 = branch(3,ibr)
      i2 = branch(4,ibr)
      do 10 igr = i1+1, i2-1
         if (grid(igr) .eq. 1 .and. grid(igr-1) .eq. 1) then
            hq2(igr) = .25*hq1(igr-1) + .5*hq1(igr) + .25*hq1(igr+1)
         else
            hq2(igr) = hq1(igr)
         endif
10    continue
      do 20 igr = i1+1, i2-1
         hq1(igr) = hq2(igr)
20    continue
30 continue

end

