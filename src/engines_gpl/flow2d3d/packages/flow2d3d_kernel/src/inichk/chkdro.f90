subroutine chkdro(lundia    ,itstrt    ,itfinish  ,drogue    ,itdrof    , &
                & itdrol    ,itdroi    ,ndro      ,nmax      ,mmax      , &
                & nmaxus    ,namdro    ,mndro     ,itdro     ,kcs       , &
                & dxydro    ,xydro     ,xcor      ,ycor      ,gdp       )
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
!    Function: - checks if release time  and stop time of
!                a drogue is inside simulation interval
!              - checks if release place of drogue is an
!                active point (kcs = 1)
!              - if for every drogue is outside simulation
!                period and will not calculated then
!                drogue will be set .false.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                                                                         :: itdrof   !  Description and declaration in inttim.igs
    integer                                                           , intent(out) :: itdroi   !  Description and declaration in inttim.igs
    integer                                                                         :: itdrol   !  Description and declaration in inttim.igs
    integer                                                           , intent(in)  :: itfinish !  Description and declaration in inttim.igs
    integer                                                           , intent(in)  :: itstrt   !  Description and declaration in inttim.igs
    integer                                                                         :: lundia   !  Description and declaration in inout.igs
    integer                                                           , intent(in)  :: mmax     !  Description and declaration in esm_alloc_int.f90
    integer                                                           , intent(in)  :: ndro     !  Description and declaration in dimens.igs
    integer                                                                         :: nmax     !  Description and declaration in esm_alloc_int.f90
    integer                                                           , intent(in)  :: nmaxus   !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(2, ndro)                                               :: itdro    !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(2, ndro)                                               :: mndro    !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcs      !  Description and declaration in esm_alloc_int.f90
    logical                                                           , intent(out) :: drogue   !  Description and declaration in procs.igs
    real(fp)     , dimension(2, ndro)                                 , intent(in)  :: dxydro   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(2, ndro)                                 , intent(out) :: xydro    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: xcor     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: ycor     !  Description and declaration in esm_alloc_real.f90
    character(20), dimension(ndro)                                                  :: namdro   !  Description and declaration in esm_alloc_char.f90
!
!
! Local variables
!
    integer                        :: i                    ! Loop counter for loop over NDRO 
    integer                        :: m                    ! Local M coordinate of drogue 
    integer                        :: md
    integer                        :: merrs                ! Local error count 
    integer                        :: n                    ! Local N coordinate of drogue 
    integer                        :: nd
    real(fp)                       :: xr
    real(fp)                       :: yr
    character(35)                  :: errmsg ! Error message 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----initialisation of interval to calculate drogues (to keep the
    !     trid-<runid>.dat as small as possible)
    !
    itdrof = itfinish + 1
    itdrol = itstrt - 1
    itdroi = 1
    !
    !-----test for all drogues if time interval is inside simulation
    !     interval (itstrt - itfinish) and if drogue is released on an
    !     active point (kcs = 1)
    !
    do i = 1, ndro
       merrs = 0
       !
       !--------itdro (1,i) < itstrt then warning and correction to itstrt
       !
       if (itdro(1, i)<itstrt) then
          errmsg = 'Drogue start time (<)'
          call prterr(lundia    ,'V008'    ,errmsg(:23)          )
          !
          write (lundia, '(12x,a,a)') 'for Drogue: ', namdro(i)
          itdro(1, i) = itstrt
       endif
       !
       !--------itdro (1,i) > itfinish then warning (no drogue calculated)
       !
       if (itdro(1, i)>itfinish) then
          errmsg = 'Drogue start time (>)'
          call prterr(lundia    ,'V017'    ,errmsg(:23)          )
          !
          write (lundia, '(12x,a,a)') 'for Drogue: ', namdro(i)
          merrs = merrs + 1
       endif
       !
       !--------itdro (2,i) < itstrt then warning (no drogue calculated)
       !
       if (itdro(2, i)<itstrt) then
          errmsg = 'Drogue stop time (<) '
          call prterr(lundia    ,'V017'    ,errmsg(:22)          )
          !
          write (lundia, '(12x,a,a)') 'for Drogue: ', namdro(i)
          merrs = merrs + 1
       endif
       !
       !--------itdro (2,i) > itfinish then warning and correction to itfinish
       !
       if (itdro(2, i)>itfinish) then
          errmsg = 'Drogue stop time (>) '
          call prterr(lundia    ,'V008'    ,errmsg(:22)          )
          !
          write (lundia, '(12x,a,a)') 'for Drogue: ', namdro(i)
          itdro(2, i) = itfinish
       endif
       !
       !--------itdro (1,i) > itdro(2,i) then warning (no drogue calculated)
       !
       if (itdro(1, i)>itdro(2, i)) then
          call prterr(lundia    ,'V018'    ,namdro(i) )
          !
          merrs = merrs + 1
       endif
       !
       !--------correction m,n input values to FLOW m,n values
       !
       mndro(1, i) = mndro(1, i) + 1
       mndro(2, i) = mndro(2, i) + 1
       !
       !--------check position of drogue. when outside boundaries or
       !        kcs (n,m) <> 1 then warning (no drogue track calculated)
       !
       m = mndro(1, i)
       n = mndro(2, i)
       if (m<1 .or. m>mmax .or. n<1 .or. n>nmaxus) then
          errmsg = 'Drogue ' // namdro(i)
          call prterr(lundia    ,'U007'    ,errmsg    )
          !
          write (lundia, '(20x,'' (m,n) = '',2i4)') m - 1, n - 1
          merrs = merrs + 1
       elseif (kcs(n, m)/=1) then
          call prterr(lundia    ,'V019'    ,namdro(i) )
          !
          merrs = merrs + 1
       else
       endif
       !
       !--------if merrs > 0 then reset itdro times to uncalculatable values
       !        (outside computation)
       !
       if (merrs>0) then
          itdro(1, i) = itfinish + 1
          itdro(2, i) = itstrt - 1
       else
          !
          !--------else calculate release coordinates in actual grid
          !
          md = m - 1
          nd = n - 1
          xr = dxydro(1, i)
          yr = dxydro(2, i)
          xydro(1, i) = (1. - xr)*(1. - yr)*xcor(nd, md) + xr*(1. - yr)         &
                      & *xcor(nd, m) + (1. - xr)*yr*xcor(n, md)                 &
                      & + xr*yr*xcor(n, m)
          xydro(2, i) = (1. - xr)*(1. - yr)*ycor(nd, md) + xr*(1. - yr)         &
                      & *ycor(nd, m) + (1. - xr)*yr*ycor(n, md)                 &
                      & + xr*yr*ycor(n, m)
       endif
       !
       !--------calculate first time to plot drogues for creating dro file
       !
       itdrof = min(itdrof, itdro(1, i))
       !
       !--------calculate last time to plot drogues for creating dro file
       !
       itdrol = max(itdrol, itdro(2, i))
    !
    enddo
    !
    !-----if itdrof > itdrol and then no drogues at all
    !
    if (itdrof>itdrol) then
       drogue = .false.
    endif
end subroutine chkdro
