subroutine wrcomwind(error     ,comfil    ,itcur    ,itimc     , &
                   & mmax      ,nmaxus    , &
                   & windu     ,windv     ,gdp       )
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
    logical              , pointer :: first
    integer              , pointer :: celidt
    type (datagroup)     , pointer :: group
    integer              , pointer :: lundia
!
! Global variables
!
    integer                                                      , intent(in)  :: itcur  ! Current time counter for the COM file
    integer                                                      , intent(in)  :: itimc  ! Current time step counter
    integer                                                                    :: mmax   ! Description and declaration in esm_alloc_int.f90
    integer                                                                    :: nmaxus ! Description and declaration in esm_alloc_int.f90
    logical                                                      , intent(out) :: error  ! Flag=TRUE if an error is encountered
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: windu  ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: windv  ! Description and declaration in esm_alloc_real.f90
    character(60)                                                , intent(in)  :: comfil ! COM file name
!
! Local variables
!
    integer                 :: ierror       ! Local error flag
    integer                 :: fds
    integer                 :: i
    integer                 :: m            ! Help var. 
    integer                 :: n            ! Help var. 
    integer, dimension(1)   :: idummy       ! Help array to write integers
    integer, dimension(3,5) :: uindex
    integer, external       :: putelt
    integer, external       :: clsnef
    integer, external       :: open_datdef
    integer, external       :: neferr
    character(16)           :: grnam
    character(256)          :: errmsg       ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(1024)         :: error_string
!
! Data statements
!
    data grnam/'WIND'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_COM, grnam, group)
    first   => group%first
    celidt  => group%celidt
    lundia  => gdp%gdinout%lundia
    !
    ! Initialize local variables
    !
    errmsg = ' '
    !
    ! initialize group index time dependent data
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    ierror = open_datdef(comfil, fds, .false.)
    if (ierror/= 0) goto 9999
    !
    if (first) then
       call addelm(gdp, lundia, FILOUT_COM, grnam, 'TIMCUR', ' ', IO_INT4, 1, (/1/), ' ', 'Time of current field rel.to reference date/time', '[ TSCALE]')
       call addelm(gdp, lundia, FILOUT_COM, grnam, 'WINDU', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Wind-velocity in zeta-point in x-direction at end of time interval', '[  M/S  ]')
       call addelm(gdp, lundia, FILOUT_COM, grnam, 'WINDV', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Wind-velocity in zeta-point in y-direction at end of time interval', '[  M/S  ]')
       !
       call defnewgrp(fds, FILOUT_COM, grnam, gdp, comfil, errlog=ERRLOG_NONE)
       first = .false.
    endif
    !
    ! Writing of output on every itcur
    !
    celidt = itcur
    uindex(1,1) = celidt
    uindex(2,1) = celidt
    !
    ! TIMCUR
    !
    idummy(1)   = itimc
    ierror     = putelt(fds, grnam, 'TIMCUR', uindex, 1, idummy)
    if (ierror/=0) goto 9999
    !
    ! WINDU
    !
    call sbuff_checksize(mmax*nmaxus)
    i = 0
    do m = 1, mmax
       do n = 1, nmaxus
          i        = i+1
          sbuff(i) = real(windu(n, m),sp)
       enddo
    enddo
    ierror = putelt(fds, grnam, 'WINDU', uindex, 1, sbuff)
    if (ierror/=0) goto 9999
    !
    ! WINDV
    !
    call sbuff_checksize(mmax*nmaxus)
    i = 0
    do m = 1, mmax
       do n = 1, nmaxus
          i        = i+1
          sbuff(i) = real(windv(n, m),sp)
       enddo
    enddo
    ierror = putelt(fds, grnam, 'WINDV', uindex, 1, sbuff)
    if (ierror/=0) goto 9999
    !
    ierror = clsnef(fds)
    !
    ! write errormessage if error occurred and set error = .true.
    !
 9999 continue
    if (ierror/= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrcomwind
