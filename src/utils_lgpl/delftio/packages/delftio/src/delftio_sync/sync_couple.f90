module sync_flowcouple
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
! Organizes the communication between the FLOW
! executable and the COUPLE executable.
! This module is the interface between DelftIO and
! Delft3D-FLOW.
! Method used: Handled by DelftIO
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dio_plt_rw
    implicit none
!
! Module variables
!
    logical,save :: firstinit
    data firstinit  /.true./
contains
!
!
!
!==============================================================================
subroutine syncflowcouple_quit(runid, flowtocouple, pars, locs, putvalue)
!
! Shut down Couple in case of 'early' error
!
    implicit none
!
! Global variables
!
    real(sp), dimension(1, 1)          :: putvalue
    character(20), dimension(1)        :: locs
    character(20), dimension(1)        :: pars
    character(*)                       :: runid
    type (dioplttype)                  :: flowtocouple
!
! Local variables
!
    character(300) :: channelname
!
!! executable statements -------------------------------------------------------
!
    ! Init DelftIO
    call dioinit
    !
    ! Signal from Flow to Couple
    ! To send the shutdown message
    channelname  = 'FlowToCouple_' // trim(runid)
    flowtocouple = diopltdefine(trim(channelname), dio_plt_real, pars, locs)
    ! Send shutdown code to Couple
    putvalue(1, 1) = -1.0
    !
    call diopltput(flowtocouple, putvalue)
    ! Close stream
    call diopltdestroy(flowtocouple)
    !
end subroutine syncflowcouple_quit
!
!
!
!==============================================================================
subroutine syncflowcouple_init(error, runid, flowtocouple, pars, locs)
! Initialise communication between Flow and Couple
    implicit none
!
! Global variables
!
    character(20), dimension(1)        :: locs
    character(20), dimension(1)        :: pars
    character(*)                       :: runid
    type (dioplttype)                  :: flowtocouple
    logical              , intent(out) :: error
!
! Local variables
!
    character(300)              :: channelname
!
!! executable statements -------------------------------------------------------
!
    error = .false.
    ! Init DelftIO
    if (firstinit) then
       call dioinit
       firstinit = .false.
    endif
    ! Signal from Flow to Couple
    channelname  = 'FlowToCouple_' // trim(runid)
    flowtocouple = diopltdefine(trim(channelname), dio_plt_real, pars, locs)
    !
    !TO DO: if not OK then error = .true.
    !
end subroutine syncflowcouple_init
!
!
!
!==============================================================================
subroutine syncflowcouple_send(flowstatus, flowtocouple, putvalue)
! Gives permission to DIO to proceed one step (synchronised mode)
    implicit none
!
! Global variables
!
    integer               , intent(in) :: flowstatus
    type (dioplttype)                  :: flowtocouple
    real(sp), dimension(1, 1)          :: putvalue
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    ! Status, < 0 tells Couple to quit
    putvalue(1, 1) = real(flowstatus, sp)
    ! Send to Couple
    call diopltput(flowtocouple, putvalue)
    !
end subroutine syncflowcouple_send
!
!
!
!==============================================================================
subroutine syncflowcouple_get(couplestatus, coupletoflow, getvalue, runid, firstget)
! Gets permission from DIO to proceed one step (synchronised mode)
    implicit none
!
! Global variables
!
    integer              , intent(out) :: couplestatus
    real(sp), pointer, dimension(:, :) :: getvalue
    logical                            :: firstget
    character(*)                       :: runid
    type (dioplttype)                  :: coupletoflow
!
! Local variables
!
    character(300)              :: channelname
!
!! executable statements -------------------------------------------------------
!
    if (firstget) then      ! <0: FLOW may quit
       ! Signal from Couple to Flow
       channelname  = 'CoupleToFlow_' // trim(runid)
       coupletoflow = diopltgetdataset(trim(channelname))
       firstget     = .false.
    endif
    ! Get status from Couple
    if (diopltget(coupletoflow, getvalue)) then
       couplestatus = nint(getvalue(1, 1))
    else
       couplestatus = -5
    endif
    !
end subroutine syncflowcouple_get
!
!
!
!==============================================================================
subroutine syncflowcouple_close(flowtocouple, coupletoflow)
! Close open streams
    implicit none
!
! Global variables
!
    type (dioplttype)                  :: flowtocouple
    type (dioplttype)                  :: coupletoflow
!
!! executable statements -------------------------------------------------------
!
    call diopltdestroy(coupletoflow)
    call diopltdestroy(flowtocouple)
end subroutine syncflowcouple_close



end module sync_flowcouple
