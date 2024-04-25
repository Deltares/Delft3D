subroutine wrsedwaqm( lundia , error     , trifil    , itmapc    , &
                    & mmax   , nmaxus    , &
                    & dps    , gdp      )
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
!    Function: Writes the time varying data for sediment (4 & 5)
!              to the FLOW MAP file
!              Output is performed conform the times of the map
!              file and only in case waqol == .true.
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
    logical              , pointer :: first
    integer              , pointer :: celidt
    type (datagroup)     , pointer :: group4
    type (datagroup)     , pointer :: group5
!
! Global variables
!
    integer                                                              , intent(in)  :: itmapc !!  Current time counter for the MAP data file
    integer                                                                            :: lundia !  Description and declaration in inout.igs
    integer                                                                            :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                            :: nmaxus !  Description and declaration in esm_alloc_int.f90
    logical                                                              , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(prec)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)  , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    character(60)                                                        , intent(in)  :: trifil !!  File name for FLOW output
                                                                                                 !!  files (tri"h/m"-"casl""labl".dat/def)
!
! Local variables
!
    integer                 :: ierror     ! Local error flag
    integer                 :: fds
    integer                 :: i
    integer                 :: m          ! Help var. 
    integer                 :: n          ! Help var. 
    integer, dimension(1)   :: idummy     ! Help array to write integers
    integer, dimension(3,5) :: uindex
    integer, external       :: clsnef
    integer, external       :: putelt
    integer, external       :: inqmxi
    integer, external       :: open_datdef
    integer, external       :: neferr
    character(10)           :: transpunit
    character(16)           :: grnam4
    character(16)           :: grnam5
    character(256)          :: errmsg      ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(60)           :: filnam      ! Help var. for FLOW file name 
!
! Data statements
!
    data grnam4/'map-infsed-serie'/
    data grnam5/'map-sed-series'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_MAP, grnam4, group4)
    first   => group4%first
    celidt  => group4%celidt
    !
    !
    ! Initialize local variables
    !
    filnam = trifil(1:3) // 'm' // trifil(5:)
    errmsg = ' '
    !
    ! initialize group index time dependent data
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    if (first) then
       !
       ! map-infsed-serie
       !
       call addelm(gdp, lundia, FILOUT_MAP, grnam4, 'ITMAPS', ' ', IO_INT4, 1, (/1/), ' ', 'timestep number (ITMAPC*DT*TUNIT := time in sec from ITDATE)', '[   -   ]')
       call defnewgrp(filnam, FILOUT_MAP, grnam4, gdp)
       !
       ! map-sed-series
       !
       call addelm(gdp, lundia, FILOUT_MAP, grnam5, 'DPS', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Bottom depth (zeta point)', '[   M   ]')
       call defnewgrp(filnam, FILOUT_MAP, grnam5, gdp)
    endif
    !
    ierror = open_datdef(filnam   ,fds      , .false.)
    if (ierror/= 0) goto 9999
    if (first) then
       !
       ! end of initialization, don't come here again
       !
       ierror = inqmxi(fds, grnam4, celidt)
       first = .false.
    endif
    !
    ! Writing of output on every itmapc
    !
    celidt = celidt + 1
    !
    ! element 'ITMAPS'
    !
    idummy(1)   = itmapc
    uindex(1,1) = celidt
    uindex(2,1) = celidt
    !
    ! Group map-sed-series, identified with grnam5, must use the same
    ! value for celidt.
    ! Easy solution:
    call getdatagroup(gdp, FILOUT_MAP, grnam5, group5)
    group5%celidt = celidt
    ! Neat solution in pseudo code:
    ! subroutine wrsedwaqm
    !    integer :: celidt
    !    call wrsedminfsed(celidt)
    !    call wrsedmsed(celidt)
    ! end subroutine
    !
    ierror     = putelt(fds, grnam4, 'ITMAPS', uindex, 1, idummy)
    if (ierror/=0) goto 9999
    !
    ! element 'DPS'
    !
    call sbuff_checksize(mmax*nmaxus)
    i = 0
    do m = 1, mmax
       do n = 1, nmaxus
          i        = i+1
          sbuff(i) = real(dps(n, m),sp)
       enddo
    enddo
    ierror = putelt(fds, grnam5, 'DPS', uindex, 1, sbuff)
    if (ierror/=0) goto 9999
    !
    ierror = clsnef(fds)
    !
    ! write errormessage if erroroccurred and set error = .true.
    !
 9999 continue
    if (ierror/= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrsedwaqm
