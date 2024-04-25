subroutine wrgrid(comfil    ,lundia    ,error     ,mmax      ,nmax      , &
                & kmax      ,nmaxus    , &
                & xcor      ,ycor      ,guu       ,gvv       ,guv       , &
                & gvu       ,gsqs      ,gsqd      ,alfas     ,thick     , &
                & rbuff     ,rbuffz    ,sferic    ,zmodel    ,zbot      , &
                & zk        ,gdp       )
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
! Write group GRID to the COM-file
!
! (Nefis-)errors may occur when trying to write new elements (SPHERIC, ZK,
! ZMODEL) to an already existing COM-file of older type (e.g. without the new
! elements).
! - In this subroutine, wrgrid, all lines
!   "if (ierr/=0) goto 9999"
!   are replaced by writing a warning to the tri-diag file and continue
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
    real(fp)                 , pointer :: amiss
    logical                  , pointer :: first
    type (datagroup)         , pointer :: group
!
! Global variables
!
    integer                                                                    :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    :: lundia !  Description and declaration in inout.igs
    integer                                                                    :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    :: nmaxus !  Description and declaration in esm_alloc_int.f90
    logical                                                      , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                                                     , intent(in)  :: zbot   !  Description and declaration in zmodel.igs
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: alfas  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: gsqd   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: ycor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                                  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                                  , intent(in)  :: zk     !!  Vertical coordinates of cell
                                                                                         !!  interfaces
                                                                                         !!  Flag for activation of Z-MODEL
    real(fp), dimension(kmax+1)                                                :: rbuffz
    real(fp), dimension(nmaxus, mmax)                                          :: rbuff  !  Description and declaration in r-i-ch.igs
    logical                                                      , intent(in)  :: sferic
    logical                                                      , intent(in)  :: zmodel !  Description and declaration in procs.igs
    character(*)                                                               :: comfil !  Name for communication file
!
! Local variables
!
    integer                                       :: fds
    integer                                       :: i
    integer                                       :: ierror ! Flag for error when writing to Communication file 
    integer                                       :: k
    integer                                       :: m
    integer                                       :: n
    integer      , dimension(1)                   :: idummy ! Help array to write integers
    character(16), dimension(1)                   :: cdummy ! Help array to write strings
    integer      , dimension(3,5)                 :: uindex
    integer                        , external     :: putels
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
    data grpnam/'GRID'/
!
!! executable statements -------------------------------------------------------
!
    amiss     => gdp%gdconst%amiss
    call getdatagroup(gdp, FILOUT_COM, grpnam, group)
    first     => group%first
    !
    if (first) then
       !
       ! Set up the element chracteristics
       !
       spunit = '[   M   ]'
       if (sferic) spunit = '[  DEG ]'
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'MMAX', ' ', IO_INT4, 1, (/1/), ' ', 'Number of cells in ksi-direction', '[   -   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'NMAX', ' ', IO_INT4, 1, (/1/), ' ', 'Number of cells in eta-direction', '[   -   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'XORI', ' ', IO_REAL4, 1, (/1/), ' ', 'X-position of local grid origin        (DO NOT USE)', '[   M   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'YORI', ' ', IO_REAL4, 1, (/1/), ' ', 'Y-position of local grid origin        (DO NOT USE)', '[   M   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'ALFORI', ' ', IO_REAL4, 1, (/1/), ' ', 'Orientation of local grid w.r.t. east  (DO NOT USE)', '[  DEG  ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'XCOR', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'X-coord. bottom point in local system', spunit)
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'YCOR', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Y-coord. bottom point in local system', spunit)
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'GUU', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'GUU grid distance in eta-direction at u-point', '[   M   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'GVV', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'GVV grid distance in ksi-direction at v-point', '[   M   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'GUV', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'GUV grid distance in eta-direction at v-point', '[   M   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'GVU', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'GVU grid distance in ksi-direction at u-point', '[   M   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'GSQS', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Cell area around water level point', '[   M2  ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'GSQD', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Cell area around bottom point', '[   M2  ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'ALFAS', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Orientation ksi-axis w.r.t. pos.x-axis at water level point', '[  DEG  ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'KMAX', ' ', IO_INT4, 1, (/1/), ' ', 'Number of layers in sigma-direction', '[   -   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'THICK', ' ', IO_REAL4, 1, (/kmax/), ' ', 'Relative layer thickness (fraction)', '[.01 * %]')
       if (zmodel) then
          call addelm(gdp, lundia, FILOUT_COM, grpnam, 'ZK', ' ', IO_REAL4, 1, (/kmax+1/), ' ', 'Vertical coordinates of cell interfaces', '[   M   ]')
       endif
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'COORDINATES', ' ', 16, 1, (/1/), ' ', 'Cartesian or Spherical coordinates', '[   -   ]') !CHARACTER
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'LAYER_MODEL', ' ', 16, 1, (/1/), ' ', 'Sigma-model or Z-model', '[   -   ]') !CHARACTER
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
    ! element 'MMAX'
    !
    idummy(1) = mmax
    ierror = putelt(fds, grpnam, 'MMAX', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ! element 'NMAX(US)'
    !
    idummy(1) = nmaxus
    ierror = putelt(fds, grpnam, 'NMAX', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ! element 'XORI'
    ! Not used anymore
    !
    call sbuff_checksize(1)
    sbuff(1) = amiss
    ierror = putelt(fds, grpnam, 'XORI', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'YORI'
    ! Not used anymore
    !
    sbuff(1) = amiss
    ierror = putelt(fds, grpnam, 'YORI', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'ALFORI'
    ! Not used anymore
    !
    sbuff(1) = amiss
    ierror = putelt(fds, grpnam, 'ALFORI', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'XCOR'
    !
    call sbuff_checksize(nmaxus*mmax)
    do m = 1, mmax
       do n = 1, nmaxus
          sbuff(n + (m-1)*nmaxus) = xcor(n, m)
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'XCOR', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'YCOR'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          sbuff(n + (m-1)*nmaxus) = ycor(n, m)
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'YCOR', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'GUU'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          sbuff(n + (m-1)*nmaxus) = guu(n, m)
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'GUU', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'GVV'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          sbuff(n + (m-1)*nmaxus) = gvv(n, m)
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'GVV', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'GUV'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          sbuff(n + (m-1)*nmaxus) = guv(n, m)
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'GUV', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'GVU'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          sbuff(n + (m-1)*nmaxus) = gvu(n, m)
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'GVU', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'GSQS'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          sbuff(n + (m-1)*nmaxus) = gsqs(n, m)
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'GSQS', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'GSQD'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          sbuff(n + (m-1)*nmaxus) = gsqd(n, m)
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'GSQD', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'ALFAS'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          sbuff(n + (m-1)*nmaxus) = alfas(n, m)
       enddo
    enddo
    ierror = putelt(fds, grpnam, 'ALFAS', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'KMAX'
    !
    idummy(1) = kmax
    ierror = putelt(fds, grpnam, 'KMAX', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ! element 'THICK'
    !
    call sbuff_checksize(kmax+1)
    do k = 1, kmax
       sbuff(k) = thick(k)
    enddo
    ierror = putelt(fds, grpnam, 'THICK', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    if (zmodel) then
       !
       ! element 'ZK'
       !
       do k = 1, kmax
          sbuff(k+1) = zk(k)
       enddo
       rbuffz(1) = zbot
       ierror = putelt(fds, grpnam, 'ZK', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
    endif
    !
    ! element 'COORDINATES'
    !
    if (sferic) then
       cdummy(1) = 'SPHERICAL'
    else
       cdummy(1) = 'CARTESIAN'
    endif
    ierror = putels(fds, grpnam, 'COORDINATES', uindex, 1, cdummy)
    if (ierror==9002) ierror=0 ! COORDINATES didn't exist on old files. Accept error for backward compatibility.
    if (ierror/= 0) goto 9999
    !
    ! element 'ZMODEL'
    !
    if (zmodel) then
       cdummy(1) = 'Z-MODEL'
    else
       cdummy(1) = 'SIGMA-MODEL'
    endif
    ierror = putels(fds, grpnam, 'LAYER_MODEL', uindex, 1, cdummy)
    if (ierror==9002) ierror=0 ! LAYER_MODEL didn't exist on old files. Accept error for backward compatibility.
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
end subroutine wrgrid
