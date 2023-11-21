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
      use precision, only: comparereal
      use m_sferic, only : pi
      
      type(t_storage_set),           intent(in   ) :: storS     !<  set of storage nodes that contain manhole parameters
      double precision, allocatable, intent(inout) :: advi  (:) !<  advection implicit part (1/s), energy losses are applied here.
   
      ! Manhole Losses
      integer                  :: iL, nstor, nod, L, i
      double precision         :: reference_angle, sum_1, sum_2, k_avg, total_outflow_area, total_inflow_area, k_exp, q_temp, q_outflow, angle
      type(t_storage), pointer :: pstor
         
      nstor = storS%count
   
      !$OMP PARALLEL DO                       &
      !$OMP PRIVATE(i,iL,sum_1,sum_2,L,k_avg)
      do i = 1,nstor
         pstor => storS%stor(i)
         nod = pstor%node_index
            
         if (pstor%angle_loss%length > 0) then
            q_temp = 0 
            do iL = 1, nd(nod)%lnx
               L = nd(nod)%ln(iL)
               q_outflow = -sign(1,L)
               L = abs(L)
               q_outflow = q_outflow*q1(L)
               reference_angle = 0d0
               if (q_outflow > 0 .and. q_outflow > q_temp) then !we want the link with the biggest discharge as reference_angle
                  q_temp = q_outflow
                  reference_angle = absdLinkangle(L)
               endif
            enddo
   
            !calculate average bend loss K value
            sum_1 = 0
            sum_2 = 0
            do iL = 1, nd(nod)%lnx
               L = nd(nod)%ln(iL)
               q_outflow = -sign(1,L)
               L = abs(L)
               q_outflow = q_outflow*q1(L)
               if (q_outflow < 0) then
                  sum_1 = sum_1 + q1(L)
                  angle = abs(absdLinkangle(L)-reference_angle)*180/pi
                  if (angle> 180d0) then
                     angle = 360d0-angle
                  endif
                  sum_2 = sum_2 + q1(L)*interpolate(pstor%angle_loss,angle) ! angle table is in degrees, dlinkangle is in radians
               endif
            enddo
            k_avg = sum_2/sum_1
         else 
            k_avg = 0d0
         endif
   
         if (pstor%expansion_loss /= 0) then 
            !calculate average output area
            total_outflow_area = 0d0
            total_inflow_area = 0d0
            do iL = 1, nd(nod)%lnx
               L = nd(nod)%ln(iL)
               q_outflow = -sign(1,L)
               L = abs(L)
               q_outflow = q_outflow*q1(L)
               if (q_outflow > 0d0) then
                  total_outflow_area = total_outflow_area + au(L)
               else
                  total_inflow_area = total_inflow_area + au(L)
               endif
            enddo
            
            select case (comparereal(total_inflow_area, total_outflow_area)) 
            case (0)
               ! then no expansion or contraction losses
               k_exp = 0
            case(-1)
               ! expansion loss -> outflow side gets negative contribution
               k_exp = -pstor%expansion_loss ! Negative Kexp to be consistent with formulation in "Delft3D Urban Modification"
            case(1)
               ! contraction loss -> outflow side gets positive contribution
               k_exp = pstor%expansion_loss ! Negative Kexp to be consistent with formulation in "Delft3D Urban Modification"
            end select
         else 
            k_exp = 0d0
         endif
   
         !apply losses to advi
         if ( k_exp /= 0 .or. k_avg /= 0d0 .or.  &
              pstor%entrance_loss == 0d0 .or. pstor%exit_loss == 0d0 ) then
            do iL = 1, nd(nod)%lnx
               L = nd(nod)%ln(iL)
               q_outflow = -sign(1,L)
               L = abs(L)
               q_outflow = q_outflow*q1(L)
   
   
               if (q_outflow > 0) then
                  advi(L) = (advi(L) + 0.5*(k_avg+k_exp + pstor%entrance_loss)*u1(L))*dxi(L)
               else
                  advi(L) = (advi(L) + 0.5*(-k_exp+ pstor%exit_loss)*u1(L)  )*dxi(L)
               endif
            enddo
         endif
      enddo
      !$OMP END PARALLEL DO
      end subroutine
   end module
   
   
   