subroutine discha_nf(kmax      ,lstsci    ,nmmax     ,kfs       ,sour      ,sink      , &
                   & volum1    ,volum0    ,r0        ,thick     ,disnf     ,sournf    , &
                   & gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: discha_nf.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/discha_nf.f90 $
!!--description-----------------------------------------------------------------
!
!   Function: The discharges resulting from near field simulation (jet3d/nrfield etc) are
!             added to the sink and source terms of the continuity equation.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer                                                 , intent(in)  :: kmax
    integer,  dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfs    ! Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: lstsci ! Description and declaration in dimens.igs
    integer                                                               :: nmmax  ! Description and declaration in dimens.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in)  :: r0     ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: sink   ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: sour   ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: disnf  ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: sournf ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                               , intent(in)  :: thick  ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: volum0 ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: volum1 ! Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                               :: nm
    integer                               :: lcon
    integer                               :: k
    real(fp) , dimension(:) , allocatable :: total_mass
!
!! executable statements -------------------------------------------------------
!
    !
    ! Fill sinks for difu
    ! Determine total subtracted mass for negative discharges
    !
    allocate (total_mass(lstsci))
    !
    total_mass = 0.0_fp
    !
    do lcon = 1,lstsci
       do k = 1, kmax
          do nm = 1, nmmax
             if (disnf(nm,k) < 0.0_fp) then
                sink(nm, k, lcon) = sink(nm, k, lcon) -             &
                                  & disnf(nm,k)/volum1(nm, k)
                total_mass(lcon)  = total_mass(lcon)  - disnf(nm,k)*r0(nm,k,lcon)
             endif
          enddo
       enddo
    enddo
    !
    ! Fill sour array for difu
    ! Add total subtracted mass and initial discharge amount (sournf)
    !
    do lcon = 1,lstsci
       do k = 1, kmax
          do nm = 1, nmmax
             if (disnf(nm,k) > 0.0_fp) then
                sour(nm, k, lcon) = sour(nm, k, lcon)  +                       &
                                  & (sournf(nm,k,lcon) + total_mass(lcon))/    &
                                  & volum0(nm, k)
             endif
          enddo
       enddo
    enddo
    !
    deallocate (total_mass)
end subroutine discha_nf
