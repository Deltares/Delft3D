recursive subroutine init_mom_output(gdp       )
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
!!--description-----------------------------------------------------------------
!
! This subroutine initializes and resets the momentum output variables.
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
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                , pointer :: lundia
!
! Global variables
!
!   NONE
!
! Local variables
!
    integer :: istat
!
!! executable statements -------------------------------------------------------
!
    lundia     => gdp%gdinout%lundia
    !
    if (gdp%gdflwpar%flwoutput%momentum) then
       if (.not. associated(gdp%gdflwpar%mom_m_velchange)) then
          istat = 0
          if (istat==0) allocate (gdp%gdflwpar%mom_m_velchange(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_m_densforce(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_m_flowresist(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_m_corioforce(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_m_visco(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_m_pressure(gdp%d%nmlb:gdp%d%nmub), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_m_tidegforce(gdp%d%nmlb:gdp%d%nmub), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_m_windforce(gdp%d%nmlb:gdp%d%nmub), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_m_bedforce(gdp%d%nmlb:gdp%d%nmub), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_m_waveforce(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_m_convec(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_m_xadvec(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), stat = istat)
          !
          if (istat==0) allocate (gdp%gdflwpar%mom_n_velchange(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_n_densforce(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_n_flowresist(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_n_corioforce(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_n_visco(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_n_pressure(gdp%d%nmlb:gdp%d%nmub), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_n_tidegforce(gdp%d%nmlb:gdp%d%nmub), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_n_windforce(gdp%d%nmlb:gdp%d%nmub), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_n_bedforce(gdp%d%nmlb:gdp%d%nmub), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_n_waveforce(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_n_convec(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%mom_n_xadvec(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), stat = istat)
          !
          if (istat /= 0) then
             call prterr(lundia, 'U021', 'MOM_OUTPUT_INIT: memory alloc error')
             call d3stop(1, gdp)
          endif
       endif
       !
       gdp%gdflwpar%mom_accum = 0.0_fp
       !
       gdp%gdflwpar%mom_m_velchange  = 0.0_fp
       gdp%gdflwpar%mom_m_densforce  = 0.0_fp
       gdp%gdflwpar%mom_m_flowresist = 0.0_fp
       gdp%gdflwpar%mom_m_corioforce = 0.0_fp
       gdp%gdflwpar%mom_m_visco      = 0.0_fp
       gdp%gdflwpar%mom_m_pressure   = 0.0_fp
       gdp%gdflwpar%mom_m_tidegforce = 0.0_fp
       gdp%gdflwpar%mom_m_windforce  = 0.0_fp
       gdp%gdflwpar%mom_m_bedforce   = 0.0_fp
       gdp%gdflwpar%mom_m_waveforce  = 0.0_fp
       gdp%gdflwpar%mom_m_convec     = 0.0_fp
       gdp%gdflwpar%mom_m_xadvec     = 0.0_fp
       !
       gdp%gdflwpar%mom_n_velchange  = 0.0_fp
       gdp%gdflwpar%mom_n_densforce  = 0.0_fp
       gdp%gdflwpar%mom_n_flowresist = 0.0_fp
       gdp%gdflwpar%mom_n_corioforce = 0.0_fp
       gdp%gdflwpar%mom_n_visco      = 0.0_fp
       gdp%gdflwpar%mom_n_pressure   = 0.0_fp
       gdp%gdflwpar%mom_n_tidegforce = 0.0_fp
       gdp%gdflwpar%mom_n_windforce  = 0.0_fp
       gdp%gdflwpar%mom_n_bedforce   = 0.0_fp
       gdp%gdflwpar%mom_n_waveforce  = 0.0_fp
       gdp%gdflwpar%mom_n_convec     = 0.0_fp
       gdp%gdflwpar%mom_n_xadvec     = 0.0_fp
    endif
end subroutine init_mom_output
