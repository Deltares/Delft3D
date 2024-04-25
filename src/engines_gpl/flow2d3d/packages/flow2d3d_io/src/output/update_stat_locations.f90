subroutine update_stat_locations(nostat    ,ndro      ,mndro     ,xydro     ,timhr     , &
                               & julday    ,lundia    ,gdp       )
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
!    Function: - Updates the monitoring locations
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use flow_tables
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    type (handletype)                , pointer :: moving_stat_file
    integer        , dimension(:)    , pointer :: stat_type
    integer        , dimension(:)    , pointer :: stat_drogue
    integer        , dimension(:)    , pointer :: stat_table
    integer        , dimension(:)    , pointer :: stat_par
    integer        , dimension(:)    , pointer :: stat_tabidx
    integer        , dimension(:, :) , pointer :: mnstat
    real(fp)       , dimension(:, :) , pointer :: xystat
    logical                          , pointer :: spheric
!
! Global variables
!
    integer                     , intent(in)  :: nostat !  Description and declaration in dimens.igs
    integer                     , intent(in)  :: ndro   !  Description and declaration in dimens.igs
    integer                     , intent(in)  :: julday !  Julian date
    integer                     , intent(in)  :: lundia !  Diagnostics file
    integer , dimension(2, ndro)              :: mndro  !  Description and declaration in esm_alloc_int.f90
    real(fp)                    , intent(in)  :: timhr  !  Time in hours
    real(fp), dimension(2, ndro)              :: xydro  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer  :: i
    logical  :: inside
    real(fp) :: rmp
    real(fp) :: rnp
!
!! executable statements -------------------------------------------------------
!
    moving_stat_file => gdp%gdstations%moving_stat_file
    stat_type        => gdp%gdstations%stat_type 
    stat_drogue      => gdp%gdstations%stat_drogue
    stat_table       => gdp%gdstations%stat_table
    stat_par         => gdp%gdstations%stat_par
    stat_tabidx      => gdp%gdstations%stat_tabidx
    mnstat           => gdp%gdstations%mnstat
    xystat           => gdp%gdstations%xystat
    spheric          => gdp%gdtricom%sferic
    !
    do i = 1,nostat
       if (stat_type(i) == 0) then
          !
          ! fixed position: nothing to do
          !
       elseif (stat_type(i) == 1) then
          !
          ! movement as prescribed by table
          !
          call flw_gettabledata(moving_stat_file  , stat_table(i) , &
                      & stat_par(i) , 2 , stat_tabidx(i), xystat(:,i), &
                      & timhr       ,julday  , gdp        )
          !
          ! if previous cell index is valid, then use it is starting point
          !
          inside = mnstat(1,i)>0
          call findnm_kcs_flowwrapper(xystat(1,i), xystat(2,i), &
                                    & mnstat(1,i), mnstat(2,i), &
                                    & rmp        , rnp        , &
                                    & inside     , spheric    , gdp)
          if (.not.inside) then
             mnstat(1,i) = -999
             mnstat(2,i) = -999
          endif
       elseif (stat_type(i) == 2) then
          !
          ! move with drogue
          !
          xystat(:,i) = xydro(:,stat_drogue(i))
          mnstat(:,i) = mndro(:,stat_drogue(i))
       else
          call prterr(lundia    ,'P004'    ,'Unsupported station type encountered in update_stat_locations.'  )
          call d3stop(1         ,gdp       )
       endif
    enddo
end subroutine update_stat_locations
