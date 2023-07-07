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

! 
! 

!> compute concentrations of particles (parts per unit volume) in flownodes
subroutine comp_concentration(s, nconst, iconst, c)
   use m_particles
   use m_partmesh
   use m_flowgeom, only : Ndx, ba, bl
   use m_flowparameters, only: epshs
   use m_flow, only: Ndkx
   implicit none

   double precision, dimension(Ndx),        intent(in)  :: s      !< water level
   integer,                                 intent(in)  :: nconst !< number of constituents
   integer,                                 intent(in)  :: iconst !< particle tracer constituent number
   double precision, dimension(Nconst,Ndx), intent(out) :: c      !< constituents

   integer :: i, k

   do i=1,Ndx
      c(iconst,i) = 0d0
   end do

!  count number of particles per cell
   do i=1,Npart
      k = kpart(i)
      if ( k.eq.0 ) cycle

      k = iabs(cell2nod(k))

      c(iconst,k) = c(iconst,k) + 1
   end do

!  compute concentration (parts per unit volume)
   do k=1,Ndx
      if ( s(k)-bl(k) .gt. epshs ) then
         c(iconst,k) = c(iconst,k) / (ba(k)*(s(k)-bl(k)))
      else
         c(iconst,k) = 0d0
      end if
   end do

   return
end subroutine comp_concentration
