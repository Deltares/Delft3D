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

!> copy the spline to a polyline
subroutine spline2poly()
   use m_splines
   use m_spline2curvi
   use m_gridsettings
   use m_polygon
   use m_missing

   implicit none

   double precision, allocatable, dimension(:) :: sc  !  spline-coordinates of grid points, not used

   integer                                     :: ispline, num, numpoints, kmax, mfacmax

   double precision                            :: hmax

   call savepol()
   call delpol()

   mfacmax = mfac

   allocate(sc(mfacmax+1))

   numpoints = 0
   do ispline=1,mcs
!     determine the number of control points in the spline
      call nump(ispline,num)

      if ( splineprops(ispline)%id .eq. 0 ) then    ! center splines only
         if ( numpoints.gt.0 ) then   ! add to existing polygon
!           add DMISS
!            numpoints = numpoints+mfac_loc(ispline)+1+1
            call increasepol(numpoints+mfacmax+2, 0 )
            npl = npl+1
            xpl(npl) = DMISS
            ypl(npl) = DMISS
         else  ! no existing polygon
!            numpoints = numpoints+mfac_loc(ispline)+1
            call increasepol(numpoints+mfacmax+1, 0)
         end if

         mfac = splineprops(ispline)%mfac
         hmax = splineprops(ispline)%hmax
         call make_gridline(num, xsp(ispline,1:num), ysp(ispline,1:num), dwidth, mfacmax, mfac, hmax, xpl(npl+1:numpoints), ypl(npl+1:numpoints), sc, jacurv)
         numpoints = numpoints+mfac+1
         npl = numpoints
      end if
   end do

   deallocate(sc)

!  restore
   mfac = mfacmax

   return
end subroutine spline2poly
