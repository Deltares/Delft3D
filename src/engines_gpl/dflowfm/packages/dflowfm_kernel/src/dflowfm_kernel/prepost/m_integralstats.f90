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

!> Module for maintaining (time-integral) statistics on flow quantities.
!! NOTE: could be the successor of Fourier analysis. Just maintain some first max/avg quantities for now.
module m_integralstats

integer            :: is_numndvals !< Number of variables on flow nodes for which statistics are recorded.
integer, parameter :: IDX_TAUS     = 1 !< Index for mean bed shear stress
integer, parameter :: IDX_UCM      = 2 !< Index for avg cell center velocity magnitude
integer, parameter :: IDX_HS       = 3 !< Index for avg water depth

double precision, allocatable, target :: is_sumvalsnd(:,:) !< [-] Integral values on flow nodes. {"location": "face", "shape": ["is_numndvals", "ndx"]}
double precision, allocatable, target :: is_maxvalsnd(:,:) !< [-] Integral values on flow nodes. {"location": "face", "shape": ["is_numndvals", "ndx"]}

character(len=1024), allocatable, target :: is_valnamesnd(:) !NOBMI [-] Names of the variables for which statistics are maintained {"shape": ["is_numndvals"]}
double precision, target                 :: is_dtint !< [s] total time interval since last statistics reset.  {"rank": 0}

contains

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_integralstats() instead.
subroutine default_integralstats()

   is_numndvals = 0

   ! Remaining of variables is handled in reset_integralstats()
   call reset_integralstats()
end subroutine default_integralstats

!> Resets only integralstats variables intended for a restart of flow simulation.
!! Upon loading of new model/MDU, call default_integralstats() instead.
subroutine reset_integralstats()
! node related
    is_sumvalsnd(1:is_numndvals,:) = 0d0
    is_maxvalsnd(1:is_numndvals,:) = -huge(1d0)
    is_valnamesnd(:) = ''
    is_valnamesnd(1) = 'taus'
    is_valnamesnd(2) = 'ucm'
    is_valnamesnd(3) = 'hs'

    is_dtint = 0d0
end subroutine reset_integralstats


!> Update the (time-)integral statistics for all flow nodes, typically after each time step.
subroutine update_integralstats()
   use m_flowtimes
   use m_flow
   use m_flowgeom

   integer :: k

   if (is_numndvals <= 0) then
      return
   end if

   if (jawave==0 .or. flowWithoutWaves) then   
      call gettaus(1,1)
   else
      call gettauswave(jawaveswartdelwaq)
   endif      
   
   do k=1,ndxi
      is_sumvalsnd(IDX_TAUS, k) =     is_sumvalsnd(IDX_TAUS, k) + dts * taus(k)
      is_sumvalsnd(IDX_UCM,  k) =     is_sumvalsnd(IDX_UCM,  k) + dts * sqrt(ucx(k)*ucx(k) + ucy(k)*ucy(k))
      is_sumvalsnd(IDX_HS, k)   =     is_sumvalsnd(IDX_HS,   k) + dts * hs(k)

      is_maxvalsnd(IDX_TAUS, k) = max(is_maxvalsnd(IDX_TAUS, k), taus(k))
      is_maxvalsnd(IDX_UCM,  k) = max(is_maxvalsnd(IDX_UCM,  k), sqrt(ucx(k)*ucx(k) + ucy(k)*ucy(k)))
      is_maxvalsnd(IDX_HS, k)   = max(is_maxvalsnd(IDX_HS,   k), hs(k))
   end do

   is_dtint = is_dtint + dts

end subroutine update_integralstats

end module m_integralstats
