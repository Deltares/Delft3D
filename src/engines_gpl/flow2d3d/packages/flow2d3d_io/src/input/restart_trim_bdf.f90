subroutine restart_trim_bdf(lundia    ,nmaxus    ,mmax      ,bdfh      , &
                          & bdfhread  ,bdfl      ,bdflread  ,rdum      , &
                          & gdp       )
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
! Reads initial field condition records from a trim-file
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use rdarray, only: rdarray_nm
    use nan_check_module
    use netcdf, only: nf90_close, nf90_open, nf90_sync, NF90_NOWRITE
    use dfparall, only: inode, master, dfint
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer       , dimension(:,:)       , pointer :: iarrc
    integer       , dimension(:)         , pointer :: mf
    integer       , dimension(:)         , pointer :: ml
    integer       , dimension(:)         , pointer :: nf
    integer       , dimension(:)         , pointer :: nl
    !
    integer                              , pointer :: i_restart
    integer                              , pointer :: filetype
    character(256)                       , pointer :: filename
    character(256)                       , pointer :: restid
    !
    ! Global variables
    !
    integer                                                                    , intent(in)  :: lundia
    integer                                                                    , intent(in)  :: mmax
    integer                                                                    , intent(in)  :: nmaxus
    logical                                                                    , intent(out) :: bdfhread
    logical                                                                    , intent(out) :: bdflread
    real(fp)                                                                   , intent(in)  :: rdum    !< value used for initialization of array
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: bdfh
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: bdfl
    !
    ! Local variables
    !
    integer, external                         :: clsnef
    integer, external                         :: crenef
    !
    integer                                   :: fds
    integer                                   :: ierror
    character(len=16)                         :: grnam
    character(len=256)                        :: dat_file
    character(len=256)                        :: def_file
    !
    !! executable statements -------------------------------------------------------
    !
    mf                  => gdp%gdparall%mf
    ml                  => gdp%gdparall%ml
    nf                  => gdp%gdparall%nf
    nl                  => gdp%gdparall%nl
    iarrc               => gdp%gdparall%iarrc
    !
    i_restart           => gdp%gdrestart%i_restart
    filetype            => gdp%gdrestart%filetype
    filename            => gdp%gdrestart%filename
    restid              => gdp%gdrestart%restid
    !
    bdfhread = .false.
    bdflread = .false.
    !
    if (filetype == -999) return
    if (inode == master) then
        if (filetype == FTYPE_NEFIS) then
            dat_file = trim(restid)//'.dat'
            def_file = trim(restid)//'.def'
            ierror   = crenef(fds, dat_file, def_file, ' ', 'r')
        elseif (filetype == FTYPE_NETCDF) then
            ierror   = nf90_open(filename, NF90_NOWRITE, fds)
        else
            ierror = -999
        endif
    endif
    call dfbroadc_gdp ( ierror  , 1, dfint, gdp )
    if (ierror /= 0) return
    !
    ! element 'DUNEHEIGHT'
    !
    grnam = 'map-sed-series'
    call rdarray_nm(fds, filename, filetype, grnam, i_restart, &
                 & nf, nl, mf, ml, iarrc, gdp, &
                 & ierror, lundia, bdfh, 'DUNEHEIGHT', rdum)
    if (ierror /= 0) then
       !
       ! In the research version, DUNEHEIGHT was stored in the map-series group.
       ! For the time being remain compatible with that version.
       !
       grnam = 'map-series'
       call rdarray_nm(fds, filename, filetype, grnam, i_restart, &
                    & nf, nl, mf, ml, iarrc, gdp, &
                    & ierror, lundia, bdfh, 'DUNEHEIGHT', rdum)
    endif
    if (ierror == 0) then
       write(lundia, '(a)') 'Bed form height read from restart file.'
       bdfhread = .true.
    endif
    !
    ! element 'DUNELENGTH'
    !
    call rdarray_nm(fds, filename, filetype, grnam, i_restart, &
                 & nf, nl, mf, ml, iarrc, gdp, &
                 & ierror, lundia, bdfl, 'DUNELENGTH', rdum)
    if (ierror == 0) then
       write(lundia, '(a)') 'Bed form length read from restart file.'
       bdflread = .true.
    endif
    !
    if (inode == master) then
       if (filetype == FTYPE_NETCDF) then
          ierror = nf90_sync(fds); call nc_check_err(lundia, ierror, "sync file", filename)
          ierror = nf90_close(fds); call nc_check_err(lundia, ierror, "closing file", filename)
       elseif (filetype == FTYPE_NEFIS) then
          ierror = clsnef(fds)
       endif
    endif
end subroutine restart_trim_bdf
