   !----- AGPL --------------------------------------------------------------------
   !
   !  Copyright (C)  Stichting Deltares, 2017-2023.
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

   Module fm_manhole_losses
   implicit none
   public calculate_manhole_losses
   private
   
   contains
   
!>    gives link angle between 0 and Pi
      double precision function absdLinkangle(L)
         use m_sferic, only: jsferic
         use geometry_module, only: getdxdy 
         use network_data, only: kn, xk, yk
         
         implicit none
         
         integer,          intent(in) :: L  !< link number
         double precision              :: dx, dy       
         integer                       :: k1, k2
         
         if ( L.gt.0 ) then
            k1 = kn(1,L)
            k2 = kn(2,L)
         else
            k1 = kn(2,-L)
            k2 = kn(1,-L)
         end if
         
         call getdxdy(xk(k1), yk(k1), xk(k2), yk(k2),dx,dy,jsferic)
         
         absdLinkangle = abs(atan2(dy,dx))
         
         return
   end function absdLinkangle
   
   !> calculate Manhole losses entrance, expansion and bend losses for all manholes and apply losses to advi(L)
   subroutine calculate_manhole_losses(storS, advi)

   use m_flowgeom, only: nd, dxi
   use m_flow, only: u1, au, q1
   use m_storage, only: t_storage_set, t_storage
   use m_tables, only: interpolate
   
   type(t_storage_set),           intent(in   ) :: storS     !<  set of storage nodes that contain manhole parameters
   double precision, allocatable, intent(inout) :: advi  (:) !<  advection implicit part (1/s), energy losses are applied here.

   ! Manhole Losses
   integer                  :: iL, nstor, nod, L, i
   double precision         :: reference_angle, sum_1, sum_2, Kavg, total_outflow_area, Kexp, relative_angle, q_temp, q_sgn
   type(t_storage), pointer :: pstor
   
   double precision, parameter :: PI=4.D0*DATAN(1.D0) 
      
   nstor = storS%count

   !$OMP PARALLEL DO                       &
   !$OMP PRIVATE(i,iL,sum_1,sum_2,L,Kavg)
   do i = 1,nstor
      pstor => storS%stor(i)
      nod = pstor%node_index
      
      q_temp = 0 
      do iL = 1, nd(nod)%lnx
         L = nd(nod)%ln(iL)
         q_sgn = sign(1,L)*q1(L)
         L = abs(L)
         if (q_sgn > 0 .and. q_sgn > q_temp) then !we want the link with the biggest discharge as reference_angle
            q_temp = q_sgn
            reference_angle = absdLinkangle(L)
         endif
      enddo

      !calculate average bend loss K value
      sum_1 = 0
      sum_2 = 0
      do iL = 1, nd(nod)%lnx
         L = abs(nd(nod)%ln(iL))
         sum_1 = sum_1 + q1(L)
         sum_2 = sum_2 + q1(L)*interpolate(pstor%angle_loss,(absdLinkangle(L)-reference_angle)/90*pi) ! angle table is in degrees, dlinkangle is in radians
      enddo
      Kavg = sum_2/sum_1

      !calculate average output area
      do iL = 1, nd(nod)%lnx
         L = nd(nod)%ln(iL)
         if (sign(1.,q1(L)) > 0) then
            total_outflow_area = total_outflow_area + au(L)
         endif
      enddo

      !apply losses to advi
      do iL = 1, nd(nod)%lnx
         L = nd(nod)%ln(iL)

         Kexp = 0
         if (abs(au(L) - total_outflow_area) > 1e-16) then !there is expansion or contraction
            Kexp = -pstor%expansion_loss ! Negative Kexp to be consistent with formulation in "Delft3D Urban Modification"
         endif

         if (sign(1.,q1(L)) > 0) then
            advi(L) = (advi(L) + 0.5*(Kavg-Kexp)*u1(L) + pstor%exit_loss)*dxi(L)
         else
            advi(L) = (advi(L) + 0.5*(Kexp)*u1(L)  + pstor%entrance_loss)*dxi(L)
         endif
      enddo
   enddo
   !$OMP END PARALLEL DO
   end subroutine
end module


