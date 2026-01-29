subroutine wqgpwd ( ngrid ,wf    ,wfs   ,igp   ,isecwq  ,cwidth )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQGPWD (Water Quality GridPoint WiDth)
!
! Module description: This routine calculates the width in a particular
!                     gridpoint/section.
!
!                     The total width for the requested gridpoint/secti-
!                     on is extracted. When the whole channel is indica-
!                     ted, including parallel sections, all widths are
!                     summed to one width.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 cwidth            O  Calculated widths.
!  4 igp               I  Gridpoint.
!  5 isecwq            I  Section:
!                         0 = Channel including parallel sections
!                         1 = Main channel
!                         2 = Sub section 1
!                         3 = Sub section 2
!  1 ngrid             I  Number of grid points in network.
!  2 wf(ngrid)         I  Actual flow width at every grid point.
!  3 wfs(ngrid,2)      I  Actual flow width per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
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
! $Log: wqgpwd.pf,v $
! Revision 1.3  1999/03/15  15:54:02  kuipe_j
! tabs removed
!
! Revision 1.2  1995/05/30  07:08:37  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:58  hoeks_a
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

   real     cwidth

   real     wf  (ngrid),&
   &wfs (ngrid,2)

   if (isecwq .eq. 0) then
      cwidth = wf(igp)
   elseif (isecwq .eq. 1) then
      cwidth = wfs(igp,1)
   elseif (isecwq .eq. 2) then
      cwidth = wfs(igp,2)
   else
      cwidth = wf(igp) - wfs(igp,1) - wfs(igp,2)
   endif

   return
end
