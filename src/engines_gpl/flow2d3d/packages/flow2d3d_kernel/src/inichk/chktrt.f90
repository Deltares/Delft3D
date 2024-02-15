subroutine chktrt(lundia    ,error     ,nmax      ,mmax      ,nmaxus    , &
                & kcu       ,kcv       , gdp)
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
! - Checks if trachytope definitions are valid and
! if used trachytopes are available in trachytope
! definitions
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                    , pointer :: nttaru
    integer                    , pointer :: nttarv
    integer                    , pointer :: ntrt
    integer , dimension(:,:)   , pointer :: ittaru
    integer , dimension(:,:)   , pointer :: ittarv
    integer , dimension(:,:)   , pointer :: ittdef
    real(fp), dimension(:,:)   , pointer :: rgcalu
    real(fp), dimension(:,:)   , pointer :: rgcalv
    logical                    , pointer :: flsedprop_rqrd
    character(256)             , pointer :: flnmD50
    character(256)             , pointer :: flnmD90
!
! Global variables
!
    integer                                                                   :: lundia
    integer                                                                   :: mmax
    integer                                                                   :: nmax
    integer                                                     , intent(in)  :: nmaxus
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcu
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcv
    logical                                                                   :: error
!
! Local variables
!
    integer       :: i
    integer       :: id
    integer       :: m
    integer       :: n
    integer       :: numlen
    character(12) :: cnum
!
!! executable statements -------------------------------------------------------
!
    nttaru        => gdp%gdtrachy%nttaru
    nttarv        => gdp%gdtrachy%nttarv
    ntrt          => gdp%gdtrachy%ntrt
    ittaru        => gdp%gdtrachy%ittaru
    ittarv        => gdp%gdtrachy%ittarv
    ittdef        => gdp%gdtrachy%ittdef
    rgcalu        => gdp%gdtrachy%rgcalu
    rgcalv        => gdp%gdtrachy%rgcalv
    flsedprop_rqrd=> gdp%gdtrachy%flsedprop_rqrd
    flnmD50       => gdp%gdbedformpar%flnmD50
    flnmD90       => gdp%gdbedformpar%flnmD90
    !
    ! Initialise flags
    !
    flsedprop_rqrd = .false.
    !
    ! Check trachytope definitions
    !
    error = .false.
    !
    ! Check on negative values
    !
    do i = 1, ntrt
       if (ittdef(i, 1) <= 0) then
          cnum = ' '
          write (cnum, '(i12)') ittdef(i, 1)
          call remove_leading_spaces(cnum      ,numlen    )
          call prterr(lundia    ,'J008'    ,cnum(1:numlen)       )
          error = .true.
       endif
       if (ittdef(i, 2) <= 0) then
          cnum = ' '
          write (cnum, '(i12)') ittdef(i, 2)
          call remove_leading_spaces(cnum      ,numlen    )
          call prterr(lundia    ,'J009'    ,cnum(1:numlen)       )
          error = .true.
       endif
       !
       ! Check on double definitions
       !
       do id = 1, i - 1
          if (ittdef(i, 1) == ittdef(id, 1)) then
             cnum = ' '
             write (cnum, '(i12)') ittdef(i, 1)
             call remove_leading_spaces(cnum      ,numlen    )
             call prterr(lundia    ,'J012'    ,cnum(1:numlen)       )
             error = .true.
          endif
       enddo
    enddo
    !
    ! Check if used trachytopes have been defined
    !
    call chktra(lundia    ,error     ,nmax      ,mmax      ,ittdef    , &
              & ntrt      ,ittaru    ,nttaru    ,'U'       , &
              & flsedprop_rqrd       ,gdp      )
    call chktra(lundia    ,error     ,nmax      ,mmax      ,ittdef    , &
              & ntrt      ,ittarv    ,nttarv    ,'V'       , &
              & flsedprop_rqrd       ,gdp       )
    !
    if (flsedprop_rqrd) then
       !
       if (gdp%gdprocs%sedim) then
          !
          ! Use D50, D90, rhosol of simulated sediment
          !
          call prterr(lundia    ,'G051'    , &
            & 'Alluvial roughness predictor uses properties of simulated sediment fractions')
       else
          !
          ! Use BedformD50 and BedformD90
          !
          call prterr(lundia    ,'G051'    , &
            & 'Alluvial roughness predictor uses the following sediment properties')
          write(lundia,'(12x,2a)') 'D50 (keyword BdfD50) :', trim(flnmD50)
          write(lundia,'(12x,2a)') 'D90 (keyword BdfD90) :', trim(flnmD90)
       endif
       !
    endif
    !
    ! Check on negative values in RGCALU and RGCALV
    !
    do m = 1, mmax
       do n = 1, nmaxus
          if (kcu(n, m) > 0 .and. rgcalu(n, m)<=0.0) then
             call prterr(lundia    ,'J013'    ,'U'       )
             error = .true.
             goto 220
          endif
       enddo
    enddo
  220 continue
    do m = 1, mmax
       do n = 1, nmaxus
          if (kcv(n, m) > 0 .and. rgcalv(n, m)<=0.0) then
             call prterr(lundia    ,'J013'    ,'V'       )
             error = .true.
             goto 320
          endif
       enddo
    enddo
  320 continue
end subroutine chktrt
