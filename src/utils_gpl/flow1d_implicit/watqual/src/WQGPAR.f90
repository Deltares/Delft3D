subroutine wqgpar ( ngrid ,aft   ,afs   ,igp   ,isecwq  ,carea )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQGPAR (Water Quality GridPoint ARea)
!
! Module description: This routine calculates the area in a particular
!                     gridpoint/section.
!
!                     The total area for the requested gridpoint/section
!                     is extracted. When the whole channel is indicated,
!                     including parallel sections, all areas are summed
!                     to one area.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 aft(ngrid)        I  Flow ot total area at every grid point
!  3 afs(ngrid,2)      I  Actual flow area per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
!  6 carea             O  Calculated area.
!  4 igp               I  Gridpoint.
!  5 isecwq            I  Section:
!                         0 = Channel including parallel sections
!                         1 = Main channel
!                         2 = Sub section 1
!                         3 = Sub section 2
!  1 ngrid             I  Number of grid points in network.
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
! $Log: wqgpar.pf,v $
! Revision 1.3  1999/03/12  12:42:22  kuipe_j
! parallel segments added
!
! Revision 1.2  1995/05/30  07:08:34  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:56  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:28  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!       Parameters
!
   integer  igp,&
   &isecwq,&
   &ngrid

   real     carea

   real     aft  (ngrid),&
   &afs (ngrid,2)

   if     (isecwq .eq. 0) then
      carea = aft(igp)
   elseif (isecwq .eq. 1) then
      carea = afs(igp,1)
   elseif (isecwq .eq. 2) then
      carea = afs(igp,2)
   else
      carea = aft(igp) - afs(igp,1) - afs(igp,2)
   endif
   carea = max(carea,0.0)

   return
end
