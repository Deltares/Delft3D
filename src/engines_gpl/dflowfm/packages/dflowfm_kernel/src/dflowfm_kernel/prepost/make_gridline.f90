!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

!> generate a gridline on a spline with a prescribed maximum mesh width
subroutine make_gridline(num, xsp, ysp, dwidth, mfacmax, mfac, hmax, xg, yg, sc, jacurv)

   use m_missing
   use m_alloc
   use geometry_module, only: dbdistance
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   integer,                                             intent(in)    :: num          !< number of spline control points
   double precision,              dimension(num),       intent(in)    :: xsp, ysp     !< coordinates of spline control points

   double precision,                                    intent(in)    :: dwidth       !< maximum mesh width
   integer,                                             intent(in)    :: mfacmax      !< maximum allowed number of mesh intervals
   double precision,                                    intent(in)    :: hmax         !< maximum grid height for this spline (both sides)

   integer,                                             intent(out)   :: mfac         !< number of mesh intervals
   double precision,              dimension(mfacmax+1), intent(out)   :: xg, yg       !< coordinates of grid points
   double precision,              dimension(mfacmax+1), intent(inout) :: sc           !< spline-coordinates of grid points

   integer,                                             intent(in)    :: jacurv       !< curvature adapted grid spacing (1) or not (0)

   double precision,              dimension(num)                      :: xsp2, ysp2   ! second order derivatives of spline coordinates

   double precision                                                   :: dmaxwidth    ! current maximum mesh width
   double precision                                                   :: dspllength   ! spline length

   integer                                                            :: i, mfac_loc

   double precision, external                                         :: splinelength

!  test
!  copy spline nodes to grid points
!   mfac = min(num-1, mfacmax)
!   do i=1,mfac+1
!      xg(i) = xsp(i)
!      yg(i) = ysp(i)
!   end do
!   return
!  end test



!  compute second order derivates of spline coordinates
   call spline(xsp,num,xsp2)
   call spline(ysp,num,ysp2)

!  make a gridline on the spline
   dmaxwidth = huge(1d0)

   dspllength = splinelength(num, xsp, ysp)
   mfac_loc   = int(0.9999d0+dspllength/dwidth)
   mfac       = min(mfac_loc,mfacmax)

   do while ( dmaxwidth.gt.dwidth)
!     make the gridline
      if ( jacurv.eq.1 ) then
         call spline2gridline(mfac+1, num, xsp, ysp, xsp2, ysp2, xg, yg, sc, hmax)
      else
         call spline2gridline(mfac+1, num, xsp, ysp, xsp2, ysp2, xg, yg, sc, -hmax)
      end if

!     determine maximum mesh width
      dmaxwidth = 0d0
      do i=1,mfac
         if ( xg(i).eq.DMISS .or. xg(i+1).eq.DMISS ) cycle
         dmaxwidth = max( dbdistance(xg(i),yg(i),xg(i+1),yg(i+1), jsferic, jasfer3D, dmiss), dmaxwidth)
      end do

!     compute and update the number of mesh intervals
      if ( dmaxwidth.le.dwidth .or. mfac.eq.mfacmax ) then
         exit
      else
         mfac = min(max(int(dmaxwidth/dwidth*mfac), mfac+1), mfacmax)  ! add at least one grid point
      end if
   end do

   return
end subroutine make_gridline
