subroutine initrestart(gdp)
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
!  $Id: initrestart.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/data/src/gdp/initrestart.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer        , pointer :: i_restart
    logical        , pointer :: dp_from_map_file
    logical        , pointer :: kfuv_from_restart
    logical        , pointer :: rst_dp
    character(256) , pointer :: restid
    character(16)  , pointer :: rst_layer_model
!
!! executable statements -------------------------------------------------------
!
    i_restart          => gdp%gdrestart%i_restart
    dp_from_map_file   => gdp%gdrestart%dp_from_map_file
    kfuv_from_restart  => gdp%gdrestart%kfuv_from_restart
    rst_dp             => gdp%gdrestart%rst_dp
    restid             => gdp%gdrestart%restid
    rst_layer_model    => gdp%gdrestart%rst_layer_model
    !
    dp_from_map_file  = .true.
    kfuv_from_restart = .false.
    rst_dp            = .false.
    i_restart         = 1
    restid            = ' '
    rst_layer_model   = ' '
end subroutine initrestart
