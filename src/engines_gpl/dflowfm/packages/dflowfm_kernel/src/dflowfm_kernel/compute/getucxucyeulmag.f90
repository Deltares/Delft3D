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

!> Computes/gets cell centered horizontal x/y velocities, either Eulerian or Lagrangian, and when requested also magnitude.
!! Centralized routine for multiple uses in output files.
subroutine getucxucyeulmag(N, ucxeulg, ucyeulg, ucmago, jaeulervel, jaucmag)
   use m_flowgeom
   use m_flow, only: ndkx, ucx, ucy
   use m_flowparameters, only: jawave, flowWithoutWaves
   use m_waves, only: ustokes            ! available for all wave models

   implicit none

   integer,          intent(in   ) :: N          !< Length of cell arrays (probably ndkx)
   double precision, intent(  out) :: ucxeulg(N) !< Target array in which to store x-velocities.
   double precision, intent(  out) :: ucyeulg(N) !< Target array in which to store y-velocities.
   double precision, intent(  out) :: ucmago(N)  !< Target array in which to store velocity magnitudes. May be undefined when jaucmag==0.
   integer,          intent(in   ) :: jaeulervel !< Whether or not (1/0) to compute Eulerian velocities (i.e., substract Stokes drift)
   integer,          intent(in   ) :: jaucmag    !< Whether or not (1/0) to compute velocity magnitudes.

   ! Copy ucx/ucy to ucxeulg/ucyeulg
   ! They will optionally be transformed into Eulerian velocities
   ucxeulg(1:ndkx) = ucx(1:ndkx) ; ucyeulg(1:ndkx) = ucy(1:ndkx)

   ! Transform uxy/ucy into Eulerian velocities
   if (jaeulervel==1 .and. jawave>0 .and. .not. flowWithoutWaves) then
      call getucxucyeuler(N, ucxeulg, ucyeulg)
   endif

   ! Compute magnitude for vel.vectors (either Lagr. or Eul.)
   if (jaucmag == 1) then
      call getucmag(N, ucxeulg, ucyeulg, ucmago)
   end if

end subroutine getucxucyeulmag
