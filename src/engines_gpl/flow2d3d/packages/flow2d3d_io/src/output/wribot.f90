subroutine wribot(comfil    ,lundia    ,error     ,mmax      ,nmax      , &
                & nmaxus    ,dp        ,dps       ,rbuff     ,gdp       )
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
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    use datagroups
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical                  , pointer :: first
    type (datagroup)         , pointer :: group
!
! Global variables
!
    integer                                                                        :: lundia !  Description and declaration in inout.igs
    integer                                                                        :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                        :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                        :: nmaxus !  Description and declaration in esm_alloc_int.f90
    logical                                                          , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: dp     !  Description and declaration in esm_alloc_real.f90
    real(prec)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(nmaxus, mmax)                                          :: rbuff  !  Description and declaration in r-i-ch.igs
    character(*)                                                                   :: comfil !!  Name for communication file
                                                                                             !!  com-<case><label>
!
! Local variables
!
    integer                                       :: fds
    integer                                       :: i
    integer                                       :: ierror ! Flag for error when writing to Communication file 
    integer                                       :: m
    integer                                       :: n
    integer      , dimension(1)                   :: idummy ! Help array to write integers
    integer      , dimension(3,5)                 :: uindex
    integer                        , external     :: putelt
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    integer                        , external     :: neferr
    character(16)                                 :: grpnam ! Data-group name defined for the COM-files 
    character(256)                                :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
!
! Data statements
!
    data grpnam/'INITBOT'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_COM, grpnam, group)
    first   => group%first
    !
    if (first) then
       !
       ! Set up the element chracteristics
       !
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'DP0', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Initial bottom depth in bottom points (positive down)', '[   M   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'DPFIX', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Depth of fixed layer (positive down)', '[   M   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'DPS', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Initial bottom depth in zeta points (positive down)', '[   M   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'NVASTI', ' ', IO_INT4, 1, (/1/), ' ', 'Fixed layer present (1) or fixed layer absent (0)', '[   -   ]')
    endif
    !
    ierror = open_datdef(comfil, fds, .false.)
    if (ierror /= 0) goto 9999
    !
    if (first) then
       call defnewgrp(fds, FILOUT_COM, grpnam, gdp, comfil, errlog=ERRLOG_NONE)
       first = .false.
    endif
    !
    ! initialize group index
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    ! element  'DP0'
    !
    call sbuff_checksize(nmaxus*mmax)
    i = 0
    do m = 1, mmax
       do n = 1, nmaxus
          i = i+1
          sbuff(i) = real(dp(n, m),sp)
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'DP0', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'DPS'
    !
    i = 0
    do m = 1, mmax
       do n = 1, nmaxus
          i = i+1
          sbuff(i) = real(dps(n, m),sp)
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'DPS', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'NVASTI'
    !
    idummy(1) = 0
    ierror = putelt(fds, grpnam, 'NVASTI', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ierror = clsnef(fds)
    !
    ! write error message if error occured and set error= .true.
    !
9999   continue
    if (ierror /= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error= .true.
    endif
end subroutine wribot
