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

! $Id$
! $HeadURL$

   subroutine fm_flocculate()
   !--description-----------------------------------------------------------------
   !
   !    Function: Exchange sediment mass due to flocculation 
   !
   !
   !--pseudo code and references--------------------------------------------------
   ! NONE
   !--declarations----------------------------------------------------------------
   use precision
   use m_flowtimes, only: dts
   use flocculation, only: FLOC_NONE, flocculate
   use m_flowgeom, only: ndx, bl
   use m_flow    , only: kmx, s1
   use m_flowparameters, only: epshs
   use m_transport, only: constituents, ised1
   use m_fm_erosed, only: floclist, flocmod, nflocpop, nflocsizes, tbreakup, tfloc
   implicit none
   
   !
   ! Local variables
   !
   integer                               :: istat          !< Memory (de)allocation status [-]
   integer                               :: i              !< Clay population index
   integer                               :: j              !< Floc size index
   integer                               :: k              !< Horizontal loop index
   integer                               :: kb             !< Index of bottommost cell
   integer                               :: kk             !< 3D cell index
   integer                               :: kt             !< Index of topmost cell
   integer                               :: ll             !< Sediment fraction index
   real(fp)                              :: breakdt        !< Relaxation factor towards equilibrium with less macro flocs [-]
   real(fp)                              :: flocdt         !< Relaxation factor towards equilibrium with more macro flocs [-]
   real(fp), dimension(:,:), allocatable :: cfloc          !< Concentration split per clay fraction and floc size [g/m3]

   !
   ! if no flocculation is active, skip this routine
   !
   if (flocmod == FLOC_NONE) return
   !
   ! flocculation happens in the water column and hence it evolves at the flow time scale (no morfac)
   !
   flocdt  = 1.0_fp - exp(-dts/tfloc)
   breakdt = 1.0_fp - exp(-dts/tbreakup)
   !
   allocate(cfloc(nflocpop, nflocsizes), stat = istat)
   
   do k = 1, ndx
      if (s1(k) - bl(k) < epshs) cycle
      !
      ! loop over the layers in the vertical
      !
      if (kmx > 0) then ! 3D
         !
         ! loop over the layers in the vertical
         !
         call getkbotktop(k, kb, kt)
      else ! 2D
         kb = k
         kt = k
      endif
      do kk = kb, kt
         !
         ! collect clay floc concentrations, convert to g/m3
         !
         do j = 1, nflocsizes
            do i = 1, nflocpop
                ll = floclist(i,j)
                cfloc(i,j) = constituents(ised1 - 1 + ll, kk) * 1000.0_fp
            enddo
         enddo
         !
         ! apply flocculation model
         !
         call flocculate(cfloc, flocdt, breakdt, flocmod)
         !
         ! update clay floc concentrations, convert back to kg/m3
         !
         do j = 1, nflocsizes
            do i = 1, nflocpop
                ll = floclist(i,j)
                constituents(ised1 - 1 + ll, kk)  = cfloc(i,j) * 0.001_fp
            enddo
         enddo
      enddo
   enddo   
   deallocate(cfloc, stat = istat)
   
   end subroutine fm_flocculate
