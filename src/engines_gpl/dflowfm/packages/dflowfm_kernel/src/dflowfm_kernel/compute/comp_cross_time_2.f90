!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_comp_cross_time_2

   implicit none

   private

   public :: comp_cross_time_2

contains

   real(kind=dp) function comp_cross_time_2(x1, x3, x4, v1, v3, v4, dclear)
      use precision, only: dp

      use m_missing, only: dmiss
      use m_comp_roots4, only: comp_roots4
      use geometry_module, only: dlinedis
      use m_sferic, only: jsferic, jasfer3D
      use m_comp_cross_time_1, only: comp_cross_time_1
      use m_lcrossgridline, only: Lcrossgridline

      implicit none

      real(kind=dp), dimension(2) :: x1, x3, x4 !< coordinates
      real(kind=dp), dimension(2) :: v1, v3, v4 !< velocities
      real(kind=dp) :: dclear !< clearance

      real(kind=dp), dimension(2) :: xdum1, xdum2
      real(kind=dp), dimension(4) :: x
      real(kind=dp), dimension(5) :: coeffs

      real(kind=dp) :: a, b, c, dnow, xc, yc, dteps, deps

      real(kind=dp) :: t1, t2, DdDt

      integer :: i, ja

      comp_cross_time_2 = 1.0e99_dp

      call dlinedis(x1(1), x1(2), x3(1), x3(2), x4(1), x4(2), ja, dnow, xc, yc, jsferic, jasfer3D, dmiss)

      t2 = 1.0e99_dp

!  only take nodes into account that are at the right-hand-side of the edge
      if (-(x1(1) - x3(1)) * (x4(2) - x3(2)) + (x1(2) - x3(2)) * (x4(1) - x3(1)) < 0.0_dp) return

      if (dnow <= dclear .and. dclear > 0.0_dp) then
         t2 = comp_cross_time_1(x1, x3, x4, v1, v3, v4, 0.0_dp)

         if (t2 < 1.0e99_dp) then
!        check if distance is increasing
            dteps = 1.0e-2_dp
            call dlinedis(x1(1) + v1(1) * dteps, x1(2) + v1(2) * dteps, x3(1) + v3(1) * dteps, x3(2) + v3(2) * dteps, x4(1) + v4(1) * dteps, x4(2) + v4(2) * dteps, ja, deps, xc, yc, jsferic, jasfer3D, dmiss)
            DdDt = (deps - dnow) / dteps
            if (DdDt < -1.0e-4_dp) then
!            t2 = comp_cross_time_1(x1,x3,x4,v1,v3,v4,0d0)
               t2 = 0.0_dp
            else
               t2 = comp_cross_time_1(x1, x3, x4, v1, v3, v4, 0.0_dp)
            end if
         end if

         comp_cross_time_2 = t2
         return
      end if

      t1 = comp_cross_time_1(x1, x3, x4, v1, v3, v4, dclear)

      if (t1 == DMISS .or. t1 <= 0.0_dp) t1 = 1.0e99_dp

!   if ( dbdistance(x1(1),x1(2),x3(1),x3(2)).gt.dclear ) then
      a = dot_product(v1 - v3, v1 - v3)
      b = 2.0_dp * dot_product(v1 - v3, x1 - x3)
      c = dot_product(x1 - x3, x1 - x3)

      coeffs = [0.0_dp, 0.0_dp, a, b, c - dclear * dclear]
      call comp_roots4(coeffs, x)
      do i = 1, 4
         if (x(i) == DMISS .or. x(i) <= 0.0_dp .or. x(i) > t1) cycle
!        check if intersection is in the right regime
         if (dot_product(x1 - x3 + (v1 - v3) * x(i), x4 - x3 + (v4 - v3) * x(i)) > 0.0_dp) then
            cycle
         end if

!        check if distance is decreasing
         DdDt = 1.0e99_dp
         if (dclear > 0.0_dp .and. x(i) > 0.0_dp) then
!           check if the new connecting line does not cross the center spline gridline
            xdum1 = x1 + v1 * x(i)
            xdum2 = x3 + v3 * x(i)
            if (.not. Lcrossgridline(xdum1, xdum2, 1)) then
               DdDt = (2.0_dp * a * x(i) + b) / (2.0_dp * dclear)
            end if
         end if
!        take minimum time
         if (x(i) /= DMISS .and. x(i) > 0.0_dp .and. DdDt < 0.0_dp) t1 = min(t1, x(i))
      end do
!   end if
!
!   if ( dbdistance(x1(1),x1(2),x4(1),x4(2)).gt.dclear ) then
      a = dot_product(v1 - v4, v1 - v4)
      b = 2.0_dp * dot_product(v1 - v4, x1 - x4)
      c = dot_product(x1 - x4, x1 - x4)

      coeffs = [0.0_dp, 0.0_dp, a, b, c - dclear * dclear]
      call comp_roots4(coeffs, x)
      do i = 1, 4
         if (x(i) == DMISS .or. x(i) <= 0.0_dp .or. x(i) > t1) cycle
!        check if intersection is in the right regime
         if (dot_product(x1 - x4 + (v1 - v4) * x(i), x3 - x4 + (v3 - v4) * x(i)) > 0.0_dp) then
            cycle
         end if
!        check if distance is decreasing
         DdDt = 1.0e99_dp
         if (dclear > 0.0_dp .and. x(i) > 0.0_dp) then
!           check if the new connecting line does not cross the center spline gridline
            xdum1 = x1 + v1 * x(i)
            xdum2 = x4 + v4 * x(i)
            if (.not. Lcrossgridline(xdum1, xdum2, 1)) then
               DdDt = (2.0_dp * a * x(i) + b) / (2.0_dp * dclear)
            end if
         end if
!        take minimum time
         if (x(i) /= DMISS .and. x(i) > 0.0_dp .and. DdDt < 0.0_dp) t1 = min(t1, x(i))
      end do
!   end if

      comp_cross_time_2 = min(t1, t2)

      return
   end function comp_cross_time_2

end module m_comp_cross_time_2
