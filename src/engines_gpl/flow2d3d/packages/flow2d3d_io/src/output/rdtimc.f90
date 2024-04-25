subroutine rdtimc(comfil    ,lundia    ,error     ,commrd    ,itlen     , &
                & tscale    ,gdp       )
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
!    Function: - Read time parameters from communication file
!                if the file does not exist then commrd = .false.
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer      , intent(out) :: itlen  !  Description and declaration in esm_alloc_int.f90
    integer                    :: lundia !  Description and declaration in inout.igs
    logical      , intent(out) :: commrd !!  Flag = TRUE if communication file exists and can be read from
    logical      , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)     , intent(out) :: tscale !  Description and declaration in esm_alloc_real.f90
    character(*)               :: comfil !!  First part of file name
!
!
! Local variables
!
    integer                                       :: fds
    integer                                       :: ierror ! error flag
    integer                                       :: lfil   ! Actual length of name file COMFIL 
    integer                                       :: luntmp
    integer                                       :: newlun
    integer       , dimension(1)                  :: idummy ! Help array to write integers
    integer      , dimension(3,5)                 :: uindex
    integer                        , external     :: clsnef
    integer                        , external     :: getelt
    integer                        , external     :: open_datdef
    integer                        , external     :: neferr
    character(16)                                 :: grpnam ! Data-group name defined for the COM-files 
    character(256)                                :: fixcom ! fixed size version of comfil, needed for character concatenation 
    character(5)                                  :: cdummy ! Character string containing text "dummy" 
    character(256)                                :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
!
! Data statements
!
    data grpnam/'PARAMS'/
!
!! executable statements -------------------------------------------------------
!
    ierror = open_datdef(comfil, fds, .true.)
    if (ierror /= 0) then
       error  = .false.
       commrd = .false.
       return
    endif
    !
    ! initialize group index
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    !-----Read TSCALE and ITLEN from communication file
    !     read in dummy variables because in case ierror <> 0, the original
    !     values still exist
    !
    call sbuff_checksize(1)
    sbuff(1) = 0.
    ierror = getelt(fds, grpnam, 'TSCALE', uindex, 1, 4, sbuff)
    if (ierror/=0) goto 9999
    !
    idummy(1) = 0
    ierror = getelt(fds, grpnam, 'ITLEN', uindex, 1, 4, idummy)
    if (ierror/=0) goto 9999
    !
    ierror = clsnef(fds)
    if (ierror/= 0) goto 9999
    !
    tscale = sbuff(1)
    itlen = idummy(1)
    !
 9999 continue
    if (ierror/=0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine rdtimc
