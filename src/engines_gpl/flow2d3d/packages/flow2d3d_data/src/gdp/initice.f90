subroutine initice(gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
!  $Id: initice.f90 61892 2018-09-24 06:54:21Z goede $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160210_12924_ice_coverage/src/engines_gpl/flow2d3d/packages/data/src/gdp/initice.f90 $
!!--description-----------------------------------------------------------------
!
! NONE
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
    real(fp) , pointer :: albedo_snow
    real(fp) , pointer :: albedo_ice
    real(fp) , pointer :: albedo_wat
    real(fp) , pointer :: cp_snow         !! specific heat for snow
    real(fp) , pointer :: cp_ice          !! specific heat for ice
    real(fp) , pointer :: k_snow          !! conductivity  for snow
    real(fp) , pointer :: k_ice           !! conductivity  for ice
    real(fp) , pointer :: lh_snow         !! latent heat for snow
    real(fp) , pointer :: lh_ice          !! latent heat for ice
    real(fp) , pointer :: rho_ice         !! density of ice
    real(fp) , pointer :: rho_snow        !! density of snow
    character(10) , pointer :: ice_model  !! type of ice model
    logical  , pointer :: dyn_ice         !! flag for horizontal transport of ice 
    logical  , pointer :: prs_ice         !! flag for ice pressure on water
    logical  , pointer :: his_ice         !! ice output on history file
!
! Global variables
!
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !-----Initialize statics for ice
    !
    albedo_snow  => gdp%gdice%albedo_snow
    albedo_ice   => gdp%gdice%albedo_ice
    albedo_wat   => gdp%gdice%albedo_wat
    cp_snow      => gdp%gdice%cp_snow
    cp_ice       => gdp%gdice%cp_ice
    k_snow       => gdp%gdice%k_snow
    k_ice        => gdp%gdice%k_ice
    lh_snow      => gdp%gdice%lh_snow
    lh_ice       => gdp%gdice%lh_ice
    rho_snow     => gdp%gdice%rho_snow
    rho_ice      => gdp%gdice%rho_ice
    ice_model    => gdp%gdice%ice_model
    dyn_ice      => gdp%gdice%dyn_ice
    prs_ice      => gdp%gdice%prs_ice
    his_ice      => gdp%gdice%his_ice
    !
    albedo_snow = 0.9_fp
    albedo_ice  = 0.75_fp
    albedo_wat  = 0.06_fp
    cp_snow     = 1.11   ! not used, because latent heat is used in computations  
    cp_ice      = 1994    ! cp_water= 3987
    k_snow      = 0.31_fp
    k_ice       = 2.04_fp
    lh_snow     = 110.0_fp * 10**6_fp
    lh_ice      = 302.0_fp * 10**6_fp
    rho_snow    = 300.0_fp  ! estimate; varies heavily
    rho_ice     = 910.0_fp   ! cp rho_ice = 1.89 10**9 j/m3 C
    ice_model   = 'dummy'
    dyn_ice     = 'N'
    prs_ice     = 'Y'
    his_ice     = 'N'
    !
end subroutine initice
