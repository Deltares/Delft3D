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
   
!>    gives link angle, changes sign when link has negative number      
      double precision function dLinkangle(L)
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
         
         dLinkangle = atan2(dy,dx)
         
         return
      end function dLinkangle
   
   subroutine calculate_manhole_losses(storS, advi)

   use m_flowgeom, only: nd, dxi
   use m_flow, only: u1, au, q1
   use m_storage, only: t_storage_set, t_storage
   use m_tables, only: interpolate
   
   type(t_storage_set),           intent(in   ) :: storS
   double precision, allocatable, intent(inout) :: advi  (:)

   ! Manhole Losses
   integer                  :: iL, nstor, nod, L, i, sgn
   double precision         :: refangle, Ksum1, Ksum2, Kavg, effective_output_area, Kexp
   type(t_storage), pointer :: pstor
   nstor = storS%count

   !$OMP PARALLEL DO                       &
   !$OMP PRIVATE(i,iL,Ksum1,Ksum2,L,Kavg)
   do i = 1,nstor
      pstor => storS%stor(i)
      nod = pstor%node_index
      do iL = 1, nd(nod)%lnx
         L = nd(nod)%ln(iL)
         sgn = sign(1,L)
         L = abs(L)
         if (q1(abs(L))*sign(1,-L) > 0) then !
            refangle = dLinkangle(L)
            exit
         endif
      enddo

      !calculate average bend loss K value
      Ksum1 = 0
      Ksum2 = 0
      do iL = 1, nd(nod)%lnx
         L = nd(nod)%ln(iL)
         Ksum1 = Ksum1 + q1(L)
         Ksum2 = Ksum2 + q1(L)*interpolate(pstor%angle_loss,dLinkangle(L)-refangle)
      enddo
      Kavg = Ksum2/Ksum1

      !calculate average output area
      Ksum1 = 0
      Ksum2 = 0
      do iL = 1, nd(nod)%lnx
         L = nd(nod)%ln(iL)
         if (sign(1.,q1(L)) > 0) then
            effective_output_area = effective_output_area + au(L)
         endif
      enddo

      !apply losses to advi
      do iL = 1, nd(nod)%lnx
         L = nd(nod)%ln(iL)

         Kexp = 0
         if (abs(au(L) - effective_output_area) > 1e-16) then !there is expansion or contraction
            Kexp = pstor%expansion_loss
         endif

         if (sign(1.,q1(L)) > 0) then
            advi(L) = (advi(L) + 0.5*(Kavg+Kexp)*u1(L) + pstor%exit_loss)*dxi(L)
         else
            advi(L) = (advi(L) + 0.5*(-Kexp)*u1(L) + pstor%entrance_loss)*dxi(L)
         endif
      enddo
   enddo
   !$OMP END PARALLEL DO
   end subroutine
end module


