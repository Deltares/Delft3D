subroutine init_ice(restid,mmax      ,nmaxus    ,kmax      ,lstsci    ,  &
              & kfsice    ,kfssnw    ,toth_i    ,toth_w    ,f_w       , &
              & r1        ,h_ice     ,h_snow    ,a_ice     , &
              & t_ice     ,t_snow    , &
              & u_ice     ,v_ice     ,ut_ice    ,vt_ice    ,icknmi    , &
              & icestr    ,sxice     ,sxsn      ,sxa       ,gdp )
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
!  $Id: init_ice.f90 64423 2019-07-24 11:30:09Z goede $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20190705_ice_modelling/src/engines_gpl/flow2d3d/packages/kernel/src/inichk/init_ice.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Fills ice arrays: h_ice, h_snow, t_ice, t_snow
!              anad initializes toth_i, toth_i and f_w
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
    integer                 , pointer :: lundia
    logical                 , pointer :: dyn_ice
    character(10)           , pointer :: ice_model
!
! Global variables
!
integer                                         , intent(in) :: kmax
integer                                         , intent(in) :: lstsci
integer                                         , intent(in) :: mmax
integer                                         , intent(in) :: nmaxus
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci):: r1
integer,  dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: kfsice
integer,  dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: kfssnw
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: a_ice
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: h_ice
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: h_snow
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: t_ice
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: t_snow
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: toth_i
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: toth_w
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: f_w
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: u_ice
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: v_ice
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: ut_ice
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: vt_ice
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub,3)            :: icestr
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub,3)            :: icknmi
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub,5)            :: sxice
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub,5)            :: sxsn
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub,5)            :: sxa
character(*)                                                 , intent(in)  :: restid !!  Runid of the restart file
!

!
! Local variables
!
    integer :: k
    integer :: l
    integer :: m
    integer :: n
!
!! executable statements -------------------------------------------------------
!
    lundia      => gdp%gdinout%lundia
    dyn_ice     => gdp%gdice%dyn_ice
    ice_model   => gdp%gdice%ice_model
    !
    ! Initialize to zero
    !
    if (restid == ' ') then
       h_ice  = 0.0
       h_snow = 0.0
       if (dyn_ice) then
           a_ice  = 0.0
       else
           a_ice  = 1.0
       endif    
       u_ice  = 0.0_fp
       v_ice  = 0.0_fp
    endif
   !   
    ut_ice = 0.0_fp
    vt_ice = 0.0_fp
    t_ice  = 0.0_fp
    t_snow = 0.0_fp
    toth_i = 0.0_fp
    toth_w = 0.0_fp
    f_w    = 0.0_fp
    ut_ice = 0.0_fp
    vt_ice = 0.0_fp
    icestr = 0.0_fp
    sxice  = 0.0_fp
    sxsn   = 0.0_fp
    sxa    = 0.0_fp
    kfsice = 0
    kfssnw = 0
    !
    ! Recompute KFSICE and KFSSNW arrays
    !
    do n = 1, nmaxus
       do m = 1, mmax
          if (h_ice(n,m) .ge. 5e-2)  then
             kfsice(n,m) = 1
          endif  
          if (h_snow(n,m) .ge. 1e-3)  then
             kfssnw(n,m) = 1
          endif  
       enddo
    enddo
    !
    ! Set initial values for KNMI model
    !
    if (ice_model == 'knmi') then
       icknmi(:,:,1) = 3.3_fp
       icknmi(:,:,2) = 0.6_fp
       icknmi(:,:,3) = 4.0_fp
    endif
    !
end subroutine init_ice
