subroutine init_iceberg(j ,icx       ,icy       , &
              & nmmaxj    ,nmmax     ,kmax      ,lstsci    , &
              & r1        ,kspu      ,kspv      ,s1        ,pship     , &
              & dps       ,volum1    ,gsqs      ,thick     , &
              & h_ice     ,a_ice     ,kfsice    ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                     
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
!  $Id: init_iceberg.f90 64423 2019-07-24 11:30:09Z goede $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20190705_ice_modelling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_ice/init_iceberg.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes initialization of an ice berg
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use timers
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                , pointer :: ag
    real(fp)                , pointer :: rhow
!
! Global variables
!
integer                                                               :: icx
integer                                                               :: icy
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: a_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: h_ice
integer                                                               :: j
integer                                                               :: kmax
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfsice
integer, dimension(gdp%d%nmlb:gdp%d%nmub,0:kmax)                      :: kspu
integer, dimension(gdp%d%nmlb:gdp%d%nmub,0:kmax)                      :: kspv
integer                                                               :: lstsci
integer                                                               :: nmmax
integer                                                               :: nmmaxj
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: pship
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: r1
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: s1
real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)    , intent(in)          :: dps 
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)          :: gsqs
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out)         :: volum1
real(fp), dimension(kmax)                       , intent(in)          :: thick
!
! Local variables
!
    integer            :: k, nm, m, n
    real(fp)           :: ice_berg
!
!! executable statements -------------------------------------------------------
!
    ag          => gdp%gdphysco%ag
    rhow        => gdp%gdphysco%rhow
    !
    !  INITIALIZATION of ICE BERG
    !
    ice_berg = 0.5_fp !! 2.5_fp
    ! ice berg in x-direction
!      do m=4,5 !! 4,6 !!
!      do m=10,11 !! from right to left
!        do n=4,5 !!2,7 !! 
!           nm = (m-1)*9 +n
   ! ice berg in y-direction
!     do m=4,5
!       do n=4,5
!          nm = (m-1)*15 +n
    ! ice berg in diagonal direction
!    do m=4,8
!       do n=4,8
!          nm = (m-1)*15 +n
    ! ice berg in diagonal direction
!     do m=45,54
!       do n=316,325
!          nm = (m-1)*359 +n
    ! ice berg in North Sea
     do m=31,36
       do n=55,60
          nm = (m-1)*135 +n
          h_ice(nm) = ice_berg
          pship(nm) = 0.93_fp * h_ice(nm) * ag * rhow
          a_ice(nm) = 1.0_fp
          s1(nm) = s1(nm) - ice_berg
          do k = 1, kmax
             volum1(nm, k) = thick(k)*(s1(nm) + real(dps(nm),fp))*gsqs(nm)
          enddo
          kfsice(nm) = 1
          kspu(nm,0) = -2
          kspu(nm-icx,0) = -2
          kspv(nm,0) = -2
          kspv(nm-icy,0) = -2
      enddo
    enddo
   !
end subroutine init_iceberg
