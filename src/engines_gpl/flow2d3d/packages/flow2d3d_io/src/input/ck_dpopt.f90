subroutine ck_dpopt(lundia    ,lsedtot   ,zmodel    ,bedupd    ,dpsopt    , &
                  & dpuopt    ,gdp       )
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
! - Determines the value of DPSOPT and DPUOPT and
!   checks whether it fulfills the conditions
!   intended and stated in the User manual
! - It is called by READMD after reading the
!   Morphology input (RDMOR) because then all
!   relevant info is available for checking
!
! DPSOPT options: [MEAN/MAX/MIN/DP]
!   MAX  is default (keyword not found or value does not match available options)
! DPUOPT options: [MEAN/UPW/MIN/MOR/MEAN_DPS]
!   sigma model:  
!      MEAN is default (keyword not found or value does not match available options)
!      MOR  if dpuopt was not specified and bedupd = .TRUE.  (forced)
!   z model: 
!      only UPW and MIN are allowed
!      MIN  in case DPUOPT unequal to UPW or MIN
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
    character(6)   , pointer :: momsol
    logical        , pointer :: rst_dp
!
! Global variables
!
    integer, intent(in) :: lsedtot !  Description and declaration in esm_alloc_int.f90
    integer             :: lundia  !  Description and declaration in inout.igs
    logical, intent(in) :: bedupd  !  Description and declaration in morpar.igs
    logical, intent(in) :: zmodel  !  Description and declaration in procs.igs
    character(8)        :: dpsopt  !  Description and declaration in numeco.igs
    character(8)        :: dpuopt
    character(256)      :: message !  Message string
!
!! executable statements -------------------------------------------------------
!
    momsol             => gdp%gdnumeco%momsol
    rst_dp             => gdp%gdrestart%rst_dp
    !
    ! dpuopt
    !
    if (dpuopt == 'MIN_DPS' ) dpuopt = 'MIN'
    if (dpuopt == 'UPW_DPS' ) dpuopt = 'UPW'
    if (dpuopt == 'MEAN_DPD') dpuopt = 'MEAN'
    !
    ! If dpuopt does not contain one of the allowed values: stop with an error
    !
    if ( dpuopt /= ' '         .and. &
       & dpuopt /= 'MIN'       .and. &
       & dpuopt /= 'MOR'       .and. &
       & dpuopt /= 'UPW'       .and. &
       & dpuopt /= 'MEAN'      .and. &
       & dpuopt /= 'MEAN_DPS') then
       message = 'Invalid value found in MD-file for DPUOPT: '//dpuopt
       call prterr(lundia, 'P004', message)
       call d3stop(1, gdp)
    endif
    !
    ! Different functionalities require different settings of dpuopt
    !
    ! Morphological run
    !
    if (lsedtot > 0 .and. bedupd) then
       if (dpuopt == ' ') then
          !
          ! If dpuopt is not prescribed it is set to MOR (=MIN) for morphological runs 
          !
          dpuopt = 'MOR'
          call prterr(lundia, 'G051', 'DPUOPT not prescribed, set to MOR for morphological run')
       elseif (dpuopt /= 'MIN' .and. dpuopt /= 'MOR') then
          message = 'The combination of DPUOPT = '//dpuopt//' and morphology has not been tested'
          call prterr(lundia, 'Z013', message)
       else
          !
          ! Dpuopt has correct value (MIN or MOR)
          !
       endif
    endif
    !
    ! Dpuopt = 'MOR' means Dpuopt = 'MIN'
    !
    if (dpuopt == 'MOR') dpuopt = 'MIN'
    !
    ! Flooding momentum solver
    !   
    if (momsol == 'flood') then
       if (dpuopt == ' ') then
          !
          ! If dpuopt is not prescribed it is set to MIN for the Flooding solver 
          !
          dpuopt = 'MIN'
          call prterr(lundia, 'G051', 'DPUOPT not prescribed, set to MIN for the Flooding solver')
       elseif (dpuopt /= 'MIN') then
          message = 'The combination of DPUOPT = '//dpuopt//' and Flooding solver is not allowed'
          call prterr(lundia, 'P004', message)
          call d3stop(1, gdp)
       else
          !
          ! Dpuopt has correct value (MIN)
          !
       endif
    endif
    !
    ! Z-model run
    !
    if (zmodel) then
       if (dpuopt == ' ') then
          !
          ! If dpuopt is not prescribed it is set to MIN for Z-model runs
          !
          dpuopt = 'MIN'
          call prterr(lundia, 'G051', 'DPUOPT not prescribed, set to MIN for Z-model runs')
       elseif (dpuopt /= 'MIN' .and. dpuopt /= 'UPW') then
          message = 'The combination of DPUOPT = '//dpuopt//' and Z-model is not allowed'
          call prterr(lundia, 'P004', message)
          call d3stop(1, gdp)
       else
          !
          ! Dpuopt has correct value (MIN or UPW)
          !
       endif
    endif
    !
    ! Final check whether dpuopt has been set
    !
    if (dpuopt == ' ') then
       !
       ! If dpuopt is not prescribed it is set to MEAN
       !
       dpuopt = 'MEAN'
       call prterr(lundia, 'Z013', 'DPUOPT not prescribed, set to MEAN')
    endif
    !
    ! dpsopt
    !
    if (dpsopt == 'MIN_DPD' ) dpsopt = 'MIN'
    if (dpsopt == 'MAX_DPD' ) dpsopt = 'MAX'
    if (dpsopt == 'MEAN_DPD') dpsopt = 'MEAN'
    !
    ! If dpsopt does not contain one of the allowed values: stop with an error
    !
    if ( dpsopt /= ' '    .and. &
       & dpsopt /= 'MEAN' .and. &
       & dpsopt /= 'DP'   .and. &
       & dpsopt /= 'MAX'  .and. &
       & dpsopt /= 'MIN') then
       message = 'Invalid value found in MD-file for DPSOPT: '//dpsopt
       call prterr(lundia, 'P004', message)
       call d3stop(1, gdp)
    endif
    !
    ! For a restart run dpsopt should be set to 'DP'
    !
    if (rst_dp) then
       if (dpsopt == ' ') then
          dpsopt = 'DP'
          call prterr(lundia, 'Z013', 'DPSOPT not prescribed, set to DP because of restart')
       elseif (dpsopt /= 'DP') then
          dpsopt = 'DP'
          call prterr(lundia, 'Z013', 'DPSOPT value reset to DP because of restart')
       else
          !
          ! Dpsopt has correct value (DP)
          !
       endif
    endif
    !
    ! Final check whether dpsopt has been set
    !
    if (dpsopt == ' ') then
       !
       ! If dpsopt is not prescribed it is set to MAX
       !
       dpsopt = 'MAX'
       call prterr(lundia, 'Z013', 'DPSOPT not prescribed; set to MAX')
    endif
end subroutine ck_dpopt
