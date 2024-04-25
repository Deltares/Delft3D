subroutine wrcurt(comfil    ,lundia    ,error     ,itcur     ,ntcur     , &
                & itimc     ,mmax      ,nmax      ,kmax      ,nmaxus    , &
                & lstsci    ,lsecfl    ,s1        ,u1        ,v1        , &
                & r1        ,qu        ,qv        ,dzu1      ,dzv1      , &
                & kmaxz     ,hu        ,hv        ,thick     ,gdp       )
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
! NONE
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
    logical                  , pointer :: only_distot_from_com
    logical                  , pointer :: zmodel
    logical                  , pointer :: first
    integer                  , pointer :: celidt
    type (datagroup)         , pointer :: group
!
! Global variables
!
    integer                                                                    , intent(in)  :: itcur  !!  Current time counter for the com-
                                                                                                       !!  munication file, where starting
                                                                                                       !!  point depend on CYCLIC
    integer                                                                    , intent(in)  :: itimc  !!  Current time step counter for 2D
                                                                                                       !!  system
    integer                                                                                  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                  :: kmaxz  !!  = KMAX for Z-model, = 0 for sigma-model
                                                                                                       !!  Needed for correct dimensioning of DZU1 and DZV1
    integer                                                                    , intent(in)  :: lsecfl !  Description and declaration in dimens.igs
    integer                                                                    , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                                                  :: lundia !  Description and declaration in inout.igs
    integer                                                                                  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: ntcur  !!  Total number of timesteps on com-
                                                                                                       !!  munication file (to write to)
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmaxz)                     :: dzu1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmaxz)                     :: dzv1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                            :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                            :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: qu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: qv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci), intent(in)  :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                                                :: thick  !  Description and declaration in esm_alloc_real.f90
    logical                                                                    , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    character(*)                                                                             :: comfil !!  Name for communication file
                                                                                                       !!  com-<case><label>
!
! Local variables
!
    integer                                       :: fds
    integer                                       :: i
    integer                                       :: ierror ! Flag for error when writing to Communication file 
    integer                                       :: k
    integer                                       :: m
    integer                                       :: n
    integer                                       :: nelmx1
    integer                                       :: nelmx2
    integer      , dimension(1)                   :: idummy ! Help array to read/write Nefis files 
    integer      , dimension(3,5)                 :: uindex
    integer                        , external     :: putelt
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    integer                        , external     :: neferr
    character(256)                                :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(16)                                 :: grnam1 ! Data-group name defined for the COM-files (CURNT) 
    character(16)                                 :: grnam2 ! Data-group name defined for the COM-files (CURTIM) 
!
! Data statements
!
    data grnam1/'CURNT'/
    data grnam2/'CURTIM'/
!
!! executable statements -------------------------------------------------------
!
    only_distot_from_com => gdp%gdprocs%only_distot_from_com
    zmodel               => gdp%gdprocs%zmodel
    call getdatagroup(gdp, FILOUT_COM, grnam1, group)
    first   => group%first
    celidt  => group%celidt
    !
    ! Set up the element dimensions
    ! different element dimensions for 2d and 3d applications
    ! if kmax =1 (2d) then only 2 dimensions
    !
    if (first) then
       !
       ! Set up the element chracteristics
       !
       call addelm(gdp, lundia, FILOUT_COM, grnam1, 'NTCUR', ' ', IO_INT4, 1, (/1/), ' ', 'Number of current fields in groups CURTIM and KENMTIM', '[   -   ]')
       !
       call addelm(gdp, lundia, FILOUT_COM, grnam2, 'TIMCUR', ' ', IO_REAL4, 1, (/1/), ' ', 'Time of current field rel.to reference date/time', '[ TSCALE]')
       if (kmax>1) then
          call addelm(gdp, lundia, FILOUT_COM, grnam2, 'QU', ' ', IO_REAL4, 3, (/nmaxus, mmax, kmax/), ' ', 'Time-average over latest interval of discharge in u-point', '[ M3/S  ]')
          call addelm(gdp, lundia, FILOUT_COM, grnam2, 'QV', ' ', IO_REAL4, 3, (/nmaxus, mmax, kmax/), ' ', 'Time-average over latest interval of discharge in v-point', '[ M3/S  ]')
       else
          call addelm(gdp, lundia, FILOUT_COM, grnam2, 'QU', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Time-average over latest interval of discharge in u-point', '[ M3/S  ]')
          call addelm(gdp, lundia, FILOUT_COM, grnam2, 'QV', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Time-average over latest interval of discharge in v-point', '[ M3/S  ]')
       endif
       call addelm(gdp, lundia, FILOUT_COM, grnam2, 'S1', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Water level in zeta point at end of time interval', '[   M   ]')
       if (kmax>1) then
          call addelm(gdp, lundia, FILOUT_COM, grnam2, 'U1', ' ', IO_REAL4, 3, (/nmaxus, mmax, kmax/), ' ', 'Velocity in u-point at end of time interval', '[  M/S  ]')
          call addelm(gdp, lundia, FILOUT_COM, grnam2, 'V1', ' ', IO_REAL4, 3, (/nmaxus, mmax, kmax/), ' ', 'Velocity in v-point at end of time interval', '[  M/S  ]')
       else
          call addelm(gdp, lundia, FILOUT_COM, grnam2, 'U1', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Velocity in u-point at end of time interval', '[  M/S  ]')
          call addelm(gdp, lundia, FILOUT_COM, grnam2, 'V1', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Velocity in v-point at end of time interval', '[  M/S  ]')
       endif
       call addelm(gdp, lundia, FILOUT_COM, grnam2, 'RSP', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Spiral flow intensity', '[  M/S  ]')
       if (kmax>1) then
          call addelm(gdp, lundia, FILOUT_COM, grnam2, 'DZU1', ' ', IO_REAL4, 3, (/nmaxus, mmax, kmax/), ' ', 'Layer thickness in u-point at end of time interval', '[   M   ]')
          call addelm(gdp, lundia, FILOUT_COM, grnam2, 'DZV1', ' ', IO_REAL4, 3, (/nmaxus, mmax, kmax/), ' ', 'Layer thickness in v-point at end of time interval', '[   M   ]')
       else
          call addelm(gdp, lundia, FILOUT_COM, grnam2, 'DZU1', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Layer thickness in u-point at end of time interval', '[   M   ]')
          call addelm(gdp, lundia, FILOUT_COM, grnam2, 'DZV1', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Layer thickness in v-point at end of time interval', '[   M   ]')
       endif
    endif
    !
    ierror = open_datdef(comfil, fds, .false.)
    if (ierror /= 0) goto 9999
    !
    if (first) then
       call defnewgrp(fds, FILOUT_COM, grnam1, gdp, comfil, errlog=ERRLOG_NONE)
       call defnewgrp(fds, FILOUT_COM, grnam2, gdp, comfil, errlog=ERRLOG_NONE)
       first = .false.
    endif
    !
    ! initialize group index
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    ! element 'NTCUR'
    !
    idummy(1) = ntcur
    ierror = putelt(fds, grnam1, 'NTCUR', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    uindex (1,1) = itcur ! start index
    uindex (2,1) = itcur ! end index
    uindex (3,1) = 1 ! increment in time
    !
    ! element 'TIMCUR'
    !
    celidt    = itcur
    idummy(1) = itimc
    ierror = putelt(fds, grnam2, 'TIMCUR', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ! element 'QU'
    !
    call sbuff_checksize(nmaxus*mmax*kmax)
    i = 0
    do k = 1, kmax
       do m = 1, mmax
          do n = 1, nmaxus
             i = i+1
             sbuff(i) = qu(n, m, k)
          enddo
       enddo
    enddo
    ierror = putelt(fds, grnam2, 'QU', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'QV'
    !
    i = 0
    do k = 1, kmax
       do m = 1, mmax
          do n = 1, nmaxus
             i = i+1
             sbuff(i) = qv(n, m, k)
          enddo
       enddo
    enddo
    ierror = putelt(fds, grnam2, 'QV', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'S1'
    !
    i = 0
    do m = 1, mmax
       do n = 1, nmaxus
          i = i+1
          sbuff(i) = s1(n, m)
       enddo
    enddo
    ierror = putelt(fds, grnam2, 'S1', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'U1'
    !
    i = 0
    do k = 1, kmax
       do m = 1, mmax
          do n = 1, nmaxus
             i = i+1
             sbuff(i) = u1(n, m, k)
          enddo
       enddo
    enddo
    ierror = putelt(fds, grnam2, 'U1', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'V1'
    !
    i = 0
    do k = 1, kmax
       do m = 1, mmax
          do n = 1, nmaxus
             i = i+1
             sbuff(i) = v1(n, m, k)
          enddo
       enddo
    enddo
    ierror = putelt(fds, grnam2, 'V1', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'RSP'
    ! if secondary flow is defined then r1(n,m,1,lsecfl) else 0.
    !
    i = 0
    if (lsecfl/=0) then
       do m = 1, mmax
          do n = 1, nmaxus
             i = i+1
             sbuff(i) = r1(n, m, 1, lsecfl)
          enddo
       enddo
    else
       sbuff = 0.0_sp
    endif
    ierror = putelt(fds, grnam2, 'RSP', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    if (.not. only_distot_from_com) then
       !
       ! element 'DZU1'
       !
       i = 0
       sbuff = 0.0_fp
       if (zmodel) then
          do k = 1, kmax
             do m = 1, mmax
                do n = 1, nmaxus
                   i = i+1
                   sbuff(i) = dzu1(n, m, k)
                enddo
             enddo
          enddo
       else
          do k = 1, kmax
             do m = 1, mmax
                do n = 1, nmaxus
                   i = i+1
                   sbuff(i) = hu(n, m)*thick(k)
                enddo
             enddo
          enddo
       endif
       ierror = putelt(fds, grnam2, 'DZU1', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ! element 'DZV1'
       !
       i = 0
       sbuff = 0.0_fp
       if (zmodel) then
          do k = 1, kmax
             do m = 1, mmax
                do n = 1, nmaxus
                   i = i+1
                   sbuff(i) = dzv1(n, m, k)
                enddo
             enddo
          enddo
       else
          do k = 1, kmax
             do m = 1, mmax
                do n = 1, nmaxus
                   i = i+1
                   sbuff(i) = hv(n, m)*thick(k)
                enddo
             enddo
          enddo
       endif
       ierror = putelt(fds, grnam2, 'DZV1', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
    endif
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
end subroutine wrcurt
