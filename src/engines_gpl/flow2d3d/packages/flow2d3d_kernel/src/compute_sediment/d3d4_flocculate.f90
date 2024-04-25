subroutine d3d4_flocculate(nmmax, kmax, lstsci, lsal, ltem, zmodel, r0, kfs, kfsmn0, kfsmx0, dts, gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!
!
!!--declarations----------------------------------------------------------------
    use flocculation, only: flocculate, FLOC_NONE
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer, pointer :: flocmod                    !  flocculation model applied to clay fractions
    integer, pointer :: nflocpop                   !  number of floc populations (groups of clay fractions that exchange mass)
    integer, pointer :: nflocsizes                 !  number of floc sizes distinguished in the flocculation model
    integer, dimension(:,:), pointer :: floclist   !  Table of groups of clay fractions that belong together (flocculation)
    real(fp), pointer :: tbreakup                  !  relaxation time scale for break-up of flocs [s]
    real(fp), pointer :: tfloc                     !  relaxation time scale for flocculation [s]

!
! Global variables
!
    integer                                                    , intent(in)   :: nmmax   !< number of internal cells
    integer                                                    , intent(in)   :: kmax    !< number of layers
    integer                                                    , intent(in)   :: lstsci  !< number of concentrations
    integer                                                    , intent(in)   :: lsal    !< salinity index
    integer                                                    , intent(in)   :: ltem    !< temperature index
    logical                                                    , intent(in)   :: zmodel  !< flag whether it's a z- or sigma-model
    real(fp)                                                   , intent(in)   :: dts     !< time step [s]
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)   :: kfs     !< dry/set flag
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)   :: kfsmn0  !< bottom most layer index
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)   :: kfsmx0 !!< top most layer index
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci) , intent(inout):: r0      !< concentration array
!
! Local variables
!
   integer                               :: istat          !< Memory (de)allocation status [-]
   integer                               :: i              !< Clay population index
   integer                               :: j              !< Floc size index
   integer                               :: nm             !< Horizontal loop index
   integer                               :: kb             !< Index of bottommost cell
   integer                               :: kk             !< 3D cell index
   integer                               :: kt             !< Index of topmost cell
   integer                               :: ll             !< Sediment fraction index
   integer                               :: lst            !< Maximum index of salinity/temperature, offset for sediment
   real(fp)                              :: breakdt        !< Relaxation factor towards equilibrium with less macroflocs [-]
   real(fp)                              :: flocdt         !< Relaxation factor towards equilibrium with more macroflocs [-]
   real(fp), dimension(:,:), allocatable :: cfloc          !< Concentration split per clay fraction and floc size [kg/m3]

!
!! executable statements -------------------------------------------------------
!
   flocmod             => gdp%gdsedpar%flocmod
   !
   ! if no flocculation is active, skip this routine
   !
   if (flocmod == FLOC_NONE) return

   nflocpop            => gdp%gdsedpar%nflocpop
   nflocsizes          => gdp%gdsedpar%nflocsizes
   floclist            => gdp%gdsedpar%floclist
   tfloc               => gdp%gdsedpar%tfloc
   
   lst   = max(lsal, ltem)
   !
   ! flocculation happens in the water column and hence it evolves at the flow time scale (no morfac)
   !
   flocdt  = 1.0_fp - exp(-dts/tfloc)
   breakdt = 1.0_fp - exp(-dts/tbreakup)
   !
   allocate(cfloc(nflocpop, nflocsizes), stat = istat)
   
   do nm = 1, nmmax
      if (kfs(nm) == 0) cycle
      !
      ! loop over the layers in the vertical
      !
      if (zmodel) then
         kb = kfsmn0(nm)
         kt = kfsmx0(nm)
      else
         kb = 1
         kt = kmax
      endif
      do kk = kb, kt
         !
         ! collect clay floc concentrations, convert to g/m3
         !
         do j = 1, nflocsizes
            do i = 1, nflocpop
                ll = lst + floclist(i,j)
                cfloc(i,j) = r0(nm, kk, ll) * 1000.0_fp
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
                ll = lst + floclist(i,j)
                r0(nm, kk, ll)  = cfloc(i,j) * 0.001_fp
            enddo
         enddo
      enddo
   enddo   
   deallocate(cfloc, stat = istat)
   
end subroutine d3d4_flocculate
