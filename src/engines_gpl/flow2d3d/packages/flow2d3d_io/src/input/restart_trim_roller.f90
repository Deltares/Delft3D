subroutine restart_trim_roller(lundia    ,error     ,fds       ,filename  ,filetype  , &
                             & i_restart ,ewave1    ,eroll1    ,qxkr      , &
                             & qykr      ,qxkw      ,qykw      ,fxw       ,fyw       , &
                             & wsu       ,wsv       ,guu       ,gvv       , &
                             & hrms      ,rdum      ,gdp       )
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
    use dfparall, only: dfint, dfmax
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
! Global variables
!
    integer                                                      , intent(in)  :: i_restart
    integer                                                                    :: lundia    !  Description and declaration in inout.igs
    logical                                                      , intent(out) :: error
    real(fp)                                                     , intent(in)  :: rdum    !< value used for initialization of array
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: eroll1    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: ewave1    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: fxw       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: fyw       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: guu       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: gvv       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: hrms      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: qxkr      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: qxkw      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: qykr      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: qykw      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: wsu       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: wsv       !  Description and declaration in esm_alloc_real.f90
    integer                                                      , intent(in)  :: fds
    character(256)                                               , intent(in)  :: filename
    integer                                                      , intent(in)  :: filetype
!
! Local variables
!
    integer                                   :: ierror
    character(16)                             :: grnam4
!
! Data statements
!
    data grnam4/'map-rol-series'/
!
!! executable statements -------------------------------------------------------
!
    mf                  => gdp%gdparall%mf
    ml                  => gdp%gdparall%ml
    nf                  => gdp%gdparall%nf
    nl                  => gdp%gdparall%nl
    iarrc               => gdp%gdparall%iarrc
    !
    error        = .false.
    !
    write(lundia, '(a)') 'Roller: restarting from trim-file'
    !
    ! element 'HS'
    !
    call rdarray_nm(fds, filename, filetype, grnam4, i_restart, &
                 & nf, nl, mf, ml, iarrc, gdp, &
                 & ierror, lundia, hrms, 'HS', rdum)
    if (ierror /= 0) goto 9999
    hrms = hrms / sqrt(2.0_fp) 
    !
    ! element 'EWAVE1'
    !
    call rdarray_nm(fds, filename, filetype, grnam4, i_restart, &
                 & nf, nl, mf, ml, iarrc, gdp, &
                 & ierror, lundia, ewave1, 'EWAVE1', rdum)
    if (ierror /= 0) goto 9999
    !
    ! element 'EROLL1'
    !
    call rdarray_nm(fds, filename, filetype, grnam4, i_restart, &
                 & nf, nl, mf, ml, iarrc, gdp, &
                 & ierror, lundia, eroll1, 'EROLL1', rdum)
    if (ierror /= 0) goto 9999
    !
    ! element 'QXKR'
    !
    call rdarray_nm(fds, filename, filetype, grnam4, i_restart, &
                 & nf, nl, mf, ml, iarrc, gdp, &
                 & ierror, lundia, qxkr, 'QXKR', rdum)
    if (ierror /= 0) goto 9999
    !
    ! element 'QYKR'
    !
    call rdarray_nm(fds, filename, filetype, grnam4, i_restart, &
                 & nf, nl, mf, ml, iarrc, gdp, &
                 & ierror, lundia, qykr, 'QYKR', rdum)
    if (ierror /= 0) goto 9999
    !
    ! element 'QXKW'
    !
    call rdarray_nm(fds, filename, filetype, grnam4, i_restart, &
                 & nf, nl, mf, ml, iarrc, gdp, &
                 & ierror, lundia, qxkw, 'QXKW', rdum)
    if (ierror /= 0) goto 9999
    !
    ! element 'QYKW'
    !
    call rdarray_nm(fds, filename, filetype, grnam4, i_restart, &
                 & nf, nl, mf, ml, iarrc, gdp, &
                 & ierror, lundia, qykw, 'QYKW', rdum)
    if (ierror /= 0) goto 9999
    !
    ! element 'FXW'
    !
    call rdarray_nm(fds, filename, filetype, grnam4, i_restart, &
                 & nf, nl, mf, ml, iarrc, gdp, &
                 & ierror, lundia, fxw, 'FXW', rdum)
    if (ierror /= 0) goto 9999
    !
    ! element 'FYW'
    !
    call rdarray_nm(fds, filename, filetype, grnam4, i_restart, &
                 & nf, nl, mf, ml, iarrc, gdp, &
                 & ierror, lundia, fyw, 'FYW', rdum)
    if (ierror /= 0) goto 9999
    !
    ! element 'WSU'
    !
    call rdarray_nm(fds, filename, filetype, grnam4, i_restart, &
                 & nf, nl, mf, ml, iarrc, gdp, &
                 & ierror, lundia, wsu, 'WSU', rdum)
    if (ierror /= 0) goto 9999
    !
    ! element 'WSV'
    !
    call rdarray_nm(fds, filename, filetype, grnam4, i_restart, &
                 & nf, nl, mf, ml, iarrc, gdp, &
                 & ierror, lundia, wsv, 'WSV', rdum)
    if (ierror /= 0) goto 9999
    !
    ! Check restart data for not-a-numbers
    !
    if (.not. nan_check(hrms  , 'HS (restart-file)'    , lundia, gdp%d%nlb, gdp%d%mlb)) ierror = 1
    if (.not. nan_check(eroll1, 'EROLL1 (restart-file)', lundia, gdp%d%nlb, gdp%d%mlb)) ierror = 1
    if (.not. nan_check(ewave1, 'EWAVE1 (restart-file)', lundia, gdp%d%nlb, gdp%d%mlb)) ierror = 1
    if (.not. nan_check(qxkr  , 'QXKR (restart-file)'  , lundia, gdp%d%nlb, gdp%d%mlb)) ierror = 1
    if (.not. nan_check(qykr  , 'QYKR (restart-file)'  , lundia, gdp%d%nlb, gdp%d%mlb)) ierror = 1
    if (.not. nan_check(qxkw  , 'QXKW (restart-file)'  , lundia, gdp%d%nlb, gdp%d%mlb)) ierror = 1
    if (.not. nan_check(qykw  , 'QYKW (restart-file)'  , lundia, gdp%d%nlb, gdp%d%mlb)) ierror = 1
    if (.not. nan_check(fxw   , 'FXW (restart-file)'   , lundia, gdp%d%nlb, gdp%d%mlb)) ierror = 1
    if (.not. nan_check(fyw   , 'FYW (restart-file)'   , lundia, gdp%d%nlb, gdp%d%mlb)) ierror = 1
    if (.not. nan_check(wsu   , 'WSU (restart-file)'   , lundia, gdp%d%nlb, gdp%d%mlb)) ierror = 1
    if (.not. nan_check(wsv   , 'WSV (restart-file)'   , lundia, gdp%d%nlb, gdp%d%mlb)) ierror = 1
    call dfreduce_gdp( ierror, 1, dfint, dfmax, gdp )
    !
9999 continue
    if (ierror /= 0) error = .true.
    !
end subroutine restart_trim_roller
