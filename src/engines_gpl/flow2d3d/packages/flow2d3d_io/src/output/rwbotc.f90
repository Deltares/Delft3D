subroutine rwbotc(comfil    ,lundia    ,error     ,itima     , &
                & itcomi    ,mmax      ,nmax      ,nmaxus    ,dp        , &
                & rbuff     ,gdp       )
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
!    Function: - Write dp array to communication file
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
    integer                  , pointer :: celidt
    type (datagroup)         , pointer :: group
!
! Global variables
!
    integer                                                , intent(in)  :: itcomi !  Description and declaration in inttim.igs
    integer                                                , intent(in)  :: itima  !!  Time to start simulation (N * tscale)
                                                                                   !!  according to DELFT3D conventions
    integer                                                              :: lundia !  Description and declaration in inout.igs
    integer                                                              :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                              :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                              :: nmaxus !  Description and declaration in esm_alloc_int.f90
    logical                                                , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        :: dp     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmaxus, mmax)                                    :: rbuff  !  Description and declaration in r-i-ch.igs
    character(*)                                                         :: comfil !!  First part of file name
!
! Local variables
!
    integer                                       :: fds
    integer                                       :: ierror
    integer                                       :: m
    integer                                       :: n
    integer      , dimension(1)                   :: idummy ! Help array to write integers
    integer      , dimension(3,5)                 :: uindex
    integer                        , external     :: putelt
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    integer                        , external     :: neferr
    character(16)                                 :: grnam1
    character(16)                                 :: grnam2
    character(256)                                :: errmsg
!
! Data statements
!
    data grnam1/'BOTNT'/
    data grnam2/'BOTTIM'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_COM, grnam1, group)
    first   => group%first
    celidt  => group%celidt
    !
    ierror = 0
    if (first) then
       !
       ! Set up the element chracteristics
       !
       call addelm(gdp, lundia, FILOUT_COM, grnam1, 'NTBOT', ' ', IO_INT4, 1, (/1/), ' ', 'Number of bottom fields in group BOTTIM', '[   -   ]')
       !
       call addelm(gdp, lundia, FILOUT_COM, grnam2, 'TIMBOT', ' ', IO_INT4, 1, (/1/), ' ', 'Communication times bottom fields rel. to reference date/time', '[ TSCALE]')
       call addelm(gdp, lundia, FILOUT_COM, grnam2, 'DP', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Bottom depth in bottom points, positive downwards', '[   M   ]')
       !
       first  = .false.
       celidt = 1
    else
       celidt = celidt + 1
    endif
    !
    !-----write nrcel, dp and itstrt to communication file if itcomi > 0
    !
    if (itcomi>0) then
       ierror = open_datdef(comfil, fds, .false.)
       if (ierror /= 0) goto 9999
       !
       if (celidt==1) then
          call defnewgrp(fds, FILOUT_COM, grnam1, gdp, comfil, errlog=ERRLOG_NONE)
          call defnewgrp(fds, FILOUT_COM, grnam2, gdp, comfil, errlog=ERRLOG_NONE)
       endif
       !
       ! initialize group index
       !
       uindex (1,1) = 1 ! start index
       uindex (2,1) = 1 ! end index
       uindex (3,1) = 1 ! increment in time
       !
       idummy(1) = celidt
       ierror = putelt(fds, grnam1, 'NTBOT', uindex, 1, idummy)
       if (ierror/= 0) goto 9999
       !
       uindex (1,1) = celidt ! start index
       uindex (2,1) = celidt ! end index
       uindex (3,1) = 1 ! increment in time
       !
       idummy(1) = itima
       ierror = putelt(fds, grnam2, 'TIMBOT', uindex, 1, idummy)
       if (ierror/= 0) goto 9999
       !
       call sbuff_checksize(nmaxus*mmax)
       do m = 1, mmax
          do n = 1, nmaxus
             sbuff(n + (m-1)*nmaxus) = real(dp(n, m),sp)
          enddo
       enddo
       ierror = putelt(fds, grnam2, 'DP', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ierror = clsnef(fds)
       if (ierror/= 0) goto 9999
    endif
    !
    ! write error message if error occured and set error= .true.
    !
9999   continue
    if (ierror /= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error= .true.
    endif
end subroutine rwbotc
