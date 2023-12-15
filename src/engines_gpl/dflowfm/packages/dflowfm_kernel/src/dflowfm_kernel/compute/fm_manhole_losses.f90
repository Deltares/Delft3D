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
      
   double precision, allocatable, dimension(:,:) :: k_bend
   double precision, allocatable, dimension(:)   :: reference_angle
   contains

   pure subroutine calc_q_manhole_to_pipe(nod,iL,L,q_manhole_to_pipe)

   use m_flowgeom, only: nd
   use m_flow, only: q1
   
   integer,          intent(in ) :: iL, nod
   integer,          intent(out) :: L
   double precision, intent(out) :: q_manhole_to_pipe

   L = nd(nod)%ln(iL)
   q_manhole_to_pipe = -sign(1,L)
   L = abs(L)
   q_manhole_to_pipe = q_manhole_to_pipe*q1(L)

   end subroutine calc_q_manhole_to_pipe
   
   !> calculate Manhole losses entrance, expansion and bend losses for all manholes and apply losses to advi(L)
   subroutine calculate_manhole_losses(storS, advi)

   use m_alloc
   use m_flowgeom, only: nd, dxi
   use m_flow, only: u1, au
   use m_storage, only: t_storage_set, t_storage
   use m_tables, only: hasTableData, interpolate
   use precision, only: comparereal
   use m_sferic, only : pi
   use m_physcoef, only : ag
   use gridoperations, only: dlinkangle
   
   type(t_storage_set),           intent(in   ) :: storS     !<  set of storage nodes that contain manhole parameters
   double precision, allocatable, intent(inout) :: advi  (:) !<  advection implicit part (1/s), energy losses are applied here.

   ! Manhole Losses
   integer                  :: iL, nstor, nod, L, i
   double precision         :: ref_angle_local, total_m2p_area, total_p2m_area, k_exp, q_temp, q_manhole_to_pipe, angle
   double precision         :: energy_loss_total, v2_m2p, v2_p2m, k_correction
   type(t_storage), pointer :: pstor
   integer                  :: count
   
   nstor = storS%count
   count = 0
   do i = 1,nstor
      pstor => storS%stor(i)
      nod = pstor%node_index
     count = max(count,nd(nod)%lnx)
   enddo
   
   allocate(k_bend(count,nstor), reference_angle(nstor))
   
   !$OMP PARALLEL DO                       &
   !$OMP PRIVATE(i,iL,sum_1,sum_2,L,ref_angle_local,angle,count,q_temp,pstor,nod,q_manhole_to_pipe,total_m2p_area,total_p2m_area,v2_m2p,v2_p2m,energy_loss_total)
   do i = 1,nstor                                                                                                                
      pstor => storS%stor(i)
      nod = pstor%node_index

      if (hasTableData(pstor%angle_loss)) then
         q_temp = 0
         do iL = 1, nd(nod)%lnx
            call calc_q_manhole_to_pipe(nod,iL,L,q_manhole_to_pipe)
            ref_angle_local = 0d0
            if (q_manhole_to_pipe > 0 .and. q_manhole_to_pipe > q_temp) then !we want the link with the biggest discharge as reference_angle
               q_temp = q_manhole_to_pipe
               ref_angle_local = dlinkangle(nd(nod)%ln(iL))
            endif
         enddo
         if (ref_angle_local /= reference_angle(i)) then ! only recalculate k_bend if refangle has changed
            reference_angle(i) = ref_angle_local
            !Calculate bend loss K value
            count = 0
            do iL = 1, nd(nod)%lnx
               call calc_q_manhole_to_pipe(nod,iL,L,q_manhole_to_pipe)
               if (q_manhole_to_pipe < 0) then
                  angle = abs(dlinkangle(nd(nod)%ln(iL))-reference_angle(i))*180/pi
                  if (angle> 180d0) then
                     angle = 360d0-angle
                  endif
                  ! By definition: the angle to be used in the angle loss table is 0 when the two pipes are inline with each other.
                  ! In the previous part of the calculation, the inner angle between the two pipes are calculated.
                  ! This requires the following correction:
                  angle = 180d0 - angle

                  count = count +1
                  k_bend(count,i) = interpolate(pstor%angle_loss,angle) ! angle table is in degrees, dlinkangle is in radians
               endif
            enddo
         endif
      else
         k_bend(count,i) = 0d0
      endif

      if (pstor%expansion_loss /= 0) then 
         !calculate average output area
         total_m2p_area = 0d0
         total_p2m_area = 0d0
         do iL = 1, nd(nod)%lnx
            call calc_q_manhole_to_pipe(nod,iL,L,q_manhole_to_pipe)
            if (q_manhole_to_pipe > 0d0) then
               total_m2p_area = total_m2p_area + au(L)
            else
               total_p2m_area = total_p2m_area + au(L)
            endif
         enddo
         
         select case (comparereal(total_p2m_area, total_m2p_area)) 
         case (0)
            ! then no expansion or contraction losses
            k_exp = 0
         case(-1)
            ! expansion loss -> manhole to pipe flow side gets negative contribution
            k_exp = -pstor%expansion_loss ! Negative Kexp to be consistent with formulation in "Delft3D Urban Modification"
         case(1)
            ! contraction loss -> manhole to pipe flow side gets positive contribution
            k_exp = pstor%expansion_loss ! Negative Kexp to be consistent with formulation in "Delft3D Urban Modification"
         end select

      else 
         k_exp = 0d0
      endif

      !apply losses to advi
      if ( k_exp /= 0 .or. hasTableData(pstor%angle_loss) .or.  &
           pstor%entrance_loss /= 0d0 .or. pstor%exit_loss /= 0d0 ) then
         ! compute the total energy loss
         energy_loss_total = 0d0
         v2_m2p = 0d0
         v2_p2m = 0d0
         count = 0
         do iL = 1, nd(nod)%lnx
            call calc_q_manhole_to_pipe(nod,iL,L,q_manhole_to_pipe)
            if (q_manhole_to_pipe > 0) then
               energy_loss_total = energy_loss_total + 0.5*(k_exp + pstor%entrance_loss)*u1(L)**2/ag
               v2_m2p = max(v2_m2p, u1(L)**2)
            else
               count = count+1
               energy_loss_total = energy_loss_total + 0.5*(k_bend(count,i)-k_exp+ pstor%exit_loss)*u1(L)**2 /ag
               v2_p2m = max(v2_p2m, u1(L)**2)
            endif
         enddo
         if (energy_loss_total < 0.05*v2_m2p/(2d0*ag)) then
            k_correction = (0.05*v2_m2p/(2d0*ag) - energy_loss_total)*2*ag/v2_p2m
         else if (energy_loss_total > (v2_p2m+0.5*v2_m2p)/(2d0*ag) ) then
            k_correction = ((v2_p2m+0.5*v2_m2p)/(2d0*ag) - energy_loss_total)*2*ag/v2_p2m
         else
            k_correction = 0d0
         endif
            
         !Apply losses to ADVI
         do iL = 1, nd(nod)%lnx
            call calc_q_manhole_to_pipe(nod,iL,L,q_manhole_to_pipe)
            if (q_manhole_to_pipe > 0) then
               advi(L) = advi(L) + 0.5*(k_exp + pstor%exit_loss)*u1(L)*dxi(L)
            else
               advi(L) = advi(L) + 0.5*(k_correction + k_bend(count,i)-k_exp+ pstor%entrance_loss)*u1(L)  *dxi(L)
            endif
         enddo
      endif
   enddo
   !$OMP END PARALLEL DO
   end subroutine
end module
   
   
   


