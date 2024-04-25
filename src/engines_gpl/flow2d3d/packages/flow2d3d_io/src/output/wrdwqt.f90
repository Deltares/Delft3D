subroutine wrdwqt(comfil    ,lundia    ,error     ,itcur     ,itimc     , &
                & nsrc      ,mnksrc    ,mmax      ,nmax      ,kmax      , &
                & nmaxus    ,lstsci    ,lsal      ,ltem      ,r1        , &
                & dicuv     ,dicww     ,discum    ,taubmx    ,gdp       )
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
    logical                  , pointer :: salin
    logical                  , pointer :: temp
    logical                  , pointer :: htur2d
    logical                  , pointer :: first
    integer                  , pointer :: celidt
    type (datagroup)         , pointer :: group
!
! Global variables
!
    integer                                                                        , intent(in)  :: itcur  !!  Current time counter for the communication file,
                                                                                                           !!  where startingpoint depends on CYCLIC
    integer                                                                        , intent(in)  :: itimc  !!  Current time step counter for 2D system
    integer                                                                                      :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                        , intent(in)  :: lsal   !  Description and declaration in dimens.igs
    integer                                                                        , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                                        , intent(in)  :: ltem   !  Description and declaration in dimens.igs
    integer                                                                                      :: lundia !  Description and declaration in inout.igs
    integer                                                                                      :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                      :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                      :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                                                      :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(7, nsrc)                                                             :: mnksrc !  Description and declaration in esm_alloc_int.f90
    logical                                                                        , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: taubmx !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)      , intent(in)  :: dicww  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax + 2)    , intent(in)  :: dicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci), intent(in)  :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(nsrc)                                                                :: discum !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                                 :: comfil !!  Name for communication file
                                                                                                           !!  com-<case><label>
!
!
! Local variables
!
    integer                                       :: fds
    integer                                       :: i
    integer                                       :: ierror ! Flag for error when writing to Communication file 
    integer                                       :: k
    integer                                       :: m
    integer                                       :: n
    integer                                       :: nelmx1 ! Number of elements for group GRPNM1 
    integer                                       :: nelmx2 ! Number of elements for group GRPNM2 
    integer       , dimension(1)                  :: idummy ! Help array to write integers
    integer      , dimension(3,5)                 :: uindex
    integer                        , external     :: putelt
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    integer                        , external     :: neferr
    character(16)                                 :: grpnm1 ! Data-group name defined for the COM-files (coupled with CURNT) 
    character(16)                                 :: grpnm2 ! Data-group name defined for the COM-files (coupled with CURNT) 
    character(256)                                :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
!
! Data statements
!
    data grpnm1/'DWQTIM'/
    data grpnm2/'TAUTIM'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_COM, grpnm1, group)
    !
    !-----Initialize local variables
    !
    salin      => gdp%gdprocs%salin
    temp       => gdp%gdprocs%temp
    htur2d     => gdp%gdprocs%htur2d
    first      => group%first
    celidt     => group%celidt
    !
    if (first) then
       !
       ! Set up the element chracteristics
       !
       call addelm(gdp, lundia, FILOUT_COM, grpnm1, 'TIMCUR', ' ', IO_INT4, 1, (/1/), ' ', 'Time of FLOW field rel.to reference date/time', '[ TSCALE]')
       !
       if (lsal>0) then
          if (kmax>1) then
             call addelm(gdp, lundia, FILOUT_COM, grpnm1, 'RSAL', ' ', IO_REAL4, 3, (/nmaxus, mmax, kmax/), ' ', 'Concentrations of salinity in zeta point', '[ PPT   ]')
          else
             call addelm(gdp, lundia, FILOUT_COM, grpnm1, 'RSAL', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Concentrations of salinity in zeta point', '[ PPT   ]')
          endif
       endif
       if (ltem>0) then
          if (kmax>1) then
             call addelm(gdp, lundia, FILOUT_COM, grpnm1, 'RTEM', ' ', IO_REAL4, 3, (/nmaxus, mmax, kmax/), ' ', 'Concentrations of temperature in zeta point', '[ DEG   ]')
          else
             call addelm(gdp, lundia, FILOUT_COM, grpnm1, 'RTEM', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Concentrations of temperature in zeta point', '[ DEG   ]')
          endif
       endif
       if (lstsci>0 .and. (kmax>1 .or. htur2d)) then
          call addelm(gdp, lundia, FILOUT_COM, grpnm1, 'DICUV', ' ', IO_REAL4, 3, (/nmaxus, mmax, kmax/), ' ', 'Horizontal eddy diffusivity in zeta point', '[  M2/S ]')
       endif
       if (kmax>1) then
          call addelm(gdp, lundia, FILOUT_COM, grpnm1, 'DICWW', ' ', IO_REAL4, 3, (/nmaxus, mmax, kmax/), ' ', 'Vertical eddy diffusivity-3D in zeta point', '[  M2/S ]')
       endif
       if (nsrc>0) then
          call addelm(gdp, lundia, FILOUT_COM, grpnm1, 'DISCUM', ' ', IO_REAL4, 1, (/nsrc/), ' ', 'Cummulative discharge original FLOW input', '[  M3   ]')
          call addelm(gdp, lundia, FILOUT_COM, grpnm1, 'MNKSRC', ' ', IO_INT4, 2, (/7, nsrc/), ' ', '(M,N,K) indices of discharge sources and time dep. location', '[   -   ]')
       endif
       !
       call addelm(gdp, lundia, FILOUT_COM, grpnm2, 'TAUMAX', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Tau_max in zeta points (scalar)', '[   -   ]')
    endif
    !
    celidt = itcur
    !
    ierror = open_datdef(comfil, fds, .false.)
    if (ierror /= 0) goto 9999
    !
    if (first) then
       call defnewgrp(fds, FILOUT_COM, grpnm1, gdp, comfil, errlog=ERRLOG_NONE)
       call defnewgrp(fds, FILOUT_COM, grpnm2, gdp, comfil, errlog=ERRLOG_NONE)
       first = .false.
    endif
    !
    ! initialize group index
    !
    uindex (1,1) = celidt ! start index
    uindex (2,1) = celidt ! end index
    uindex (3,1) = 1 ! increment in time
    !
    ! element 'TIMCUR'
    !
    idummy(1) = itimc
    ierror = putelt(fds, grpnm1, 'TIMCUR', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ! element 'RSAL'
    !
    call sbuff_checksize(nmaxus*mmax*kmax)
    if (lsal>0) then
       i = 0
       do k = 1, kmax
          do m = 1, mmax
             do n = 1, nmaxus
                i = i+1
                sbuff(i) = r1(n, m, k, lsal)
             enddo
          enddo
       enddo
       ierror = putelt(fds, grpnm1, 'RSAL', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
    endif
    !
    ! element 'RTEM'
    !
    if (ltem>0) then
       i = 0
       do k = 1, kmax
          do m = 1, mmax
             do n = 1, nmaxus
                i = i+1
                sbuff(i) = r1(n, m, k, ltem)
             enddo
          enddo
       enddo
       ierror = putelt(fds, grpnm1, 'RTEM', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
    endif
    !
    ! element 'DICUV'
    !     kmax > 1      : dicuv depends on dicww
    !     htur2d = true : dicuv is calculated (HLES) (AND depends on dicww)
    !     kmax+1 contains initial values and should not be writte
    !
    if (lstsci>0 .and. (kmax>1 .or. htur2d)) then
       i = 0
       do k = 1, kmax
          do m = 1, mmax
             do n = 1, nmaxus
                i = i+1
                sbuff(i) = dicuv(n, m, k)
             enddo
          enddo
       enddo
       ierror = putelt(fds, grpnm1, 'DICUV', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
    endif
    !
    ! element 'DICWW'
    !
    if (kmax>1) then
       i = 0
       do k = 1, kmax
          do m = 1, mmax
             do n = 1, nmaxus
                i = i+1
                sbuff(i) = dicww(n, m, k)
             enddo
          enddo
       enddo
       ierror = putelt(fds, grpnm1, 'DICWW', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
    endif
    !
    if (nsrc>0) then
       !
       ! element 'DISCUM'
       !
       do i = 1, nsrc
          sbuff(i) = discum(i)
       enddo
       ierror = putelt(fds, grpnm1, 'DISCUM', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ! element 'MNKSRC'
       !
       ierror = putelt(fds, grpnm1, 'MNKSRC', uindex, 1, mnksrc)
       if (ierror/= 0) goto 9999
    endif
    !
    ! element 'TAUMAX'
    !
    i = 0
    do m = 1, mmax
       do n = 1, nmaxus
          i = i+1
          sbuff(i) = taubmx(n, m)
       enddo
    enddo
    ierror = putelt(fds, grpnm2, 'TAUMAX', uindex, 1, sbuff)
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
end subroutine wrdwqt
