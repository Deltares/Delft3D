subroutine rstcom(comfil    ,lundia    ,error     ,mmax      ,nmax      , &
                & kmax      ,nmaxus    ,lstsci    ,lsecfl    ,lsec      , &
                & timrst    ,itlen     ,timcur    ,maxtim    ,s1        , &
                & u1        ,v1        ,r1        ,rbuff     ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: rstcom.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/output/rstcom.f90 $
!!--description-----------------------------------------------------------------
!
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
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
    integer, intent(in)                                                         :: itlen  !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)                                                         :: lsec   !  Description and declaration in dimens.igs
    integer, intent(in)                                                         :: lsecfl !  Description and declaration in dimens.igs
    integer, intent(in)                                                         :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: lundia !  Description and declaration in inout.igs
    integer, intent(in)                                                         :: maxtim !!  Max.nr. of timesteps for the communi-
                                                                                          !!  cation file
    integer                                                                     :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)                                                         :: timrst !!  Restart time in combination with
                                                                                          !!  restart option from comm. file
    integer, dimension(maxtim)                                                  :: timcur !!  Array with time steps on comm. file
                                                                                          !!  for restart option
    logical                                                                     :: error  !!  Flag=TRUE if an error is encountered
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci) :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmaxus, mmax, kmax, 2)                                  :: rbuff  !  Description and declaration in r-i-ch.igs
    character(*)                                                                :: comfil !!  Name for communication file
                                                                                          !!  com-<case><label>
!
! Local variables
!
    integer                                       :: fds
    integer                                       :: i      ! Hulp var. 
    integer                                       :: ierror
    integer                                       :: kmaxk
    integer                                       :: nhulp  ! Hulp var. 
    integer                                       :: nready ! Flag for determination of inter- polation coefficient 
    integer                                       :: ntcurr ! Total number of timesteps on com- munication file (to read from) 
    integer                                       :: ntimwa ! Time index of first function 
    integer                                       :: ntimwb ! Time index of second function 
    integer                                       :: tact   ! Actual time step number 
    integer      , dimension(1)                   :: idummy ! Help array to write integer
    integer      , dimension(2)                   :: ifcore ! Time indices (cell id's) of the wave functions which are in core available 
    integer      , dimension(3,5)                 :: uindex
    integer                        , external     :: getelt
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    integer                        , external     :: neferr
    real(fp)                                      :: atimw  ! Interpolation factor for first function 
    real(fp)                                      :: btimw  ! Interpolation factor for second function 
    character(16)                                 :: funam  ! Name of element which has to be read 
    character(16)                                 :: grnam1 ! Data-group name defined for the COM-files (CURNT) 
    character(16)                                 :: grnam2 ! Data-group name defined for the COM-files (CURTIM) 
    character(256)                                :: errmsg
!
! Data statements
!
    data grnam1/'CURNT'/
    data grnam2/'CURTIM'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_COM, grnam1, group)
    first   => group%first
    !
    ifcore(1) = 0
    ifcore(2) = 0
    !
    if (first) then
       !
       ! Set up the element chracteristics
       !
       call addelm(gdp, lundia, FILOUT_COM, grnam1, 'NTCUR', ' ', IO_INT4, 1, (/1/), ' ', 'Number of current fields in group CURTIM', '[   -   ]')
       !
       !call defnewgrp(comfil, grnam1, gdp)
       !
       call addelm(gdp, lundia, FILOUT_COM, grnam2, 'TIMCUR', ' ', IO_INT4, 1, (/1/), ' ', 'Time of current field rel.to reference date/time', '[ TSCALE]')
       if (kmax > 1) then
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
       call addelm(gdp, lundia, FILOUT_COM, grnam2, 'RSP', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Spiral flow intensity', '[   -   ]')
       !
       !call defnewgrp(comfil, grnam2, gdp)
       first = .false.
    endif
    !
    ierror = open_datdef(comfil, fds, .false.)
    if (ierror /= 0) goto 9999
    !
    ! initialize group index
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    ! element 'NTCUR'
    !
    idummy(1) = 0
    ierror = getelt(fds, grnam1, 'NTCUR', uindex, 1, 4, idummy)
    if (ierror/= 0) goto 9999
    ntcurr = idummy(1)
    !
    ! Test if number of time steps on file are inside defined array
    ! boundary maxtim If error occurred then write errormessage to
    ! diagnostic file
    !
    if (ntcurr > maxtim) then
       call prterr(lundia    ,'D008'    ,' '       )
       error = .true.
       goto 9999
    endif
    !
    ! Read the array with time information.
    !
    do i = 1, ntcurr
       ierror = getelt(fds, grnam2, 'TIMCUR', uindex, 1, 4, timcur(i))
       if (ierror/= 0) goto 9999
    enddo
    !
    ! Determination of reduced time and extrapolation coefficient
    !
    nready = 0
    if (itlen>0) then
       !
       ! Periodic functions
       !
       tact = timrst
       if (tact >= timcur(ntcurr)) then
          nhulp = (tact - timcur(ntcurr))/itlen + 1
          tact  = tact - nhulp*itlen
          if (tact < timcur(1)) then
             ntimwb = 1
             ntimwa = ntcurr
             btimw  = real(tact - timcur(ntcurr) + itlen,fp)      &
                    & /real(timcur(1) - timcur(ntcurr) + itlen,fp)
             nready = 1
          endif
       elseif (tact <= timcur(1)) then
          nhulp = (timcur(1) - tact)/itlen + 1
          tact  = tact + nhulp*itlen
          if (tact > timcur(ntcurr)) then
             ntimwb = 1
             ntimwa = ntcurr
             btimw  = real(tact - timcur(ntcurr),fp)              &
                    & /real(timcur(1) - timcur(ntcurr) + itlen,fp)
             nready = 1
          endif
       else
       endif
    else
       !
       ! A-periodic functions
       !
       tact = timrst
       if (tact < timcur(1)) then
          ntimwa = 1
          ntimwb = 2
          tact   = timcur(1)
          btimw  = 0.0
          nready = 1
       elseif (tact > timcur(ntcurr)) then
          ntimwa = ntcurr - 1
          ntimwb = ntcurr
          tact   = timcur(ntcurr)
          btimw  = 1.0
          nready = 1
       else
       endif
    endif
    if (nready == 0) then
       !
       ! Determination of interpolation coefficient
       !
       do i = 2, ntcurr
          if (tact <= timcur(i)) then
             ntimwb = i
             ntimwa = i - 1
             btimw  = real(tact - timcur(i - 1),fp)/real(timcur(i) - timcur(i - 1),fp)
             exit
          endif
       enddo
    endif
    atimw = 1.0 - btimw
    !
    ! Read the two selected timesteps of the functions and perform
    ! the interpolation.
    !
    funam = 'S1'
    kmaxk = 1
    call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
              & nmax      ,kmaxk     ,nmaxus    ,grnam2    , &
              & funam     ,ntimwa    ,ntimwb    , &
              & atimw     ,btimw     ,s1        ,rbuff     ,gdp       )
    if (error) goto 9999
    !
    funam = 'U1'
    kmaxk = kmax
    call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
              & nmax      ,kmaxk     ,nmaxus    ,grnam2    , &
              & funam     ,ntimwa    ,ntimwb    , &
              & atimw     ,btimw     ,u1        ,rbuff     ,gdp       )
    if (error) goto 9999
    !
    funam = 'V1'
    kmaxk = kmax
    call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
              & nmax      ,kmaxk     ,nmaxus    ,grnam2    , &
              & funam     ,ntimwa    ,ntimwb    , &
              & atimw     ,btimw     ,v1        ,rbuff     ,gdp       )
    if (error) goto 9999
    !
    if (lsec>0) then
       funam = 'RSP'
       kmaxk = 1
       call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
                 & nmax      ,kmaxk     ,nmaxus    ,grnam2    , &
                 & funam     ,ntimwa    ,ntimwb    , &
                 & atimw     ,btimw     ,r1(1, -1, 1, lsecfl) ,rbuff     ,gdp       )
       if (error) then
       endif
    endif
 9999 continue
    if (ierror /= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine rstcom
