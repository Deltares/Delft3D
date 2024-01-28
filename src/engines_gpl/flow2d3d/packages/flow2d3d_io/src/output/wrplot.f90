subroutine wrplot(filnam    ,lundia    ,error     ,mmax      ,nmax      , &
                & nmaxus    ,kcs       ,ibuff     ,xz        ,yz        , &
                & sferic    ,gdp       )
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
    integer     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(nmaxus, mmax)                                          :: ibuff  !  Description and declaration in esm_alloc_int.f90
    logical                                                          , intent(in)  :: sferic !  Description and declaration in tricom.igs
    logical                                                          , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: xz     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: yz     !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                   :: filnam !!  Name for output file
                                                                                             !!  Comm. file: com-<case><label>
                                                                                             !!  Map file: trim-<case><label>
!
! Local variables
!
    integer                                       :: IO_FIL
    integer                                       :: fds
    integer                                       :: ierror ! Flag for error when writing to Communication file 
    integer                                       :: m
    integer                                       :: md
    integer                                       :: n
    integer                                       :: nd
    integer      , dimension(3,5)                 :: uindex
    integer                        , external     :: putelt
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    integer                        , external     :: neferr
    character(9)                                  :: spunit ! spatial unit m or deg
    character(16)                                 :: grpnam ! Data-group name defined for the COM-files 
    character(256)                                :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
!
! Data statements
!
    data grpnam/'TEMPOUT'/
!
!! executable statements -------------------------------------------------------
!
    if (filnam(1:3) == 'com') then
       IO_FIL = FILOUT_COM
    else
       IO_FIL = FILOUT_MAP
    endif
    call getdatagroup(gdp, IO_FIL, grpnam, group)
    first   => group%first
    !
    if (first) then
       !
       ! Set up the element chracteristics
       !
       spunit = '[   M   ]'
       if (sferic) spunit = '[  DEG ]'
       call addelm(gdp, lundia, IO_FIL, grpnam, 'XWAT', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'X-coord. water level point in local system', spunit)
       call addelm(gdp, lundia, IO_FIL, grpnam, 'YWAT', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Y-coord. water level point in local system', spunit)
       call addelm(gdp, lundia, IO_FIL, grpnam, 'CODB', ' ', IO_INT4, 2, (/nmaxus, mmax/), ' ', '1/-1 Active/Non-active bottom point ( w.r.t. coordinates )', '[   -   ]')
       call addelm(gdp, lundia, IO_FIL, grpnam, 'CODW', ' ', IO_INT4, 2, (/nmaxus, mmax/), ' ', '1/-1 Active/Non-active water level point (w.r.t. coordinates )', '[   -   ]')
    endif
    !
    ierror = open_datdef(filnam, fds, .false.)
    if (ierror /= 0) goto 9999
    !
    if (first) then
       call defnewgrp(fds, IO_FIL, grpnam, gdp, filnam, errlog=ERRLOG_NONE)
       first = .false.
    endif
    !
    ! initialize group index
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    ! element 'XWAT'
    !
    call sbuff_checksize(nmaxus*mmax)
    do m = 1, mmax
       do n = 1, nmaxus
          sbuff(n + (m-1)*nmaxus) = xz(n, m)
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'XWAT', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'YWAT'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          sbuff(n + (m-1)*nmaxus) = yz(n, m)
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'YWAT', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'CODB'
    !
    do m = 1, mmax
       ibuff(1, m) = -1
    enddo
    do n = 1, nmaxus
       ibuff(n, 1) = -1
    enddo
    !
    do m = 2, mmax
       md = m - 1
       do n = 2, nmaxus
          nd = n - 1
          ibuff(n, m) = -1
          if (kcs(n, m)==1) then
             ibuff(n, m) = 1
             ibuff(n, md) = 1
             ibuff(nd, m) = 1
             ibuff(nd, md) = 1
          endif
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'CODB', uindex, 1, ibuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'CODW'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          ibuff(n, m) = -1
          if (kcs(n, m)==1) ibuff(n, m) = 1
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'CODW', uindex, 1, ibuff)
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
end subroutine wrplot
