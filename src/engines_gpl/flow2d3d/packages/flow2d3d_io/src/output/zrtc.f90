subroutine zrtc(mlb, mub, nlb, nub, kfs, kfsmin, kfsmax, sig, zk, s1, dps, kmax, gdp)
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
!  $Id: zrtc.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/output/zrtc.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: - Determines the elevation of the cell centres used in the output 
!                of salinity to RTC (subroutine datatortc in module sync_rtc.f90)
! Method used:
!
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
    logical                       , pointer :: zmodel
    real(fp)     , dimension(:,:) , pointer :: zrtcsta
    real(fp)     , dimension(:)   , pointer :: s1rtcsta
    integer                       , pointer :: stacnt
    integer      , dimension(:,:) , pointer :: mnrtcsta
!
! Global variables
!
    integer                               , intent(in) :: mlb
    integer                               , intent(in) :: mub
    integer                               , intent(in) :: nlb
    integer                               , intent(in) :: nub
    integer   , dimension(nlb:nub,mlb:mub), intent(in) :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(nlb:nub,mlb:mub), intent(in) :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(nlb:nub,mlb:mub), intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer                               , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(nlb:nub,mlb:mub), intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nlb:nub,mlb:mub), intent(in) :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(  kmax)         , intent(in) :: sig
    real(fp)  , dimension(0:kmax)         , intent(in) :: zk
!
! Local variables
!
    integer   :: i
    integer   :: m
    integer   :: n
    integer   :: k
!
!! executable statements -------------------------------------------------------
!
    zmodel     => gdp%gdprocs%zmodel
    zrtcsta    => gdp%gdrtc%zrtcsta
    s1rtcsta   => gdp%gdrtc%s1rtcsta
    stacnt     => gdp%gdrtc%stacnt
    mnrtcsta   => gdp%gdrtc%mnrtcsta
    !
    ! Determine the elevation of the cell centres for all RTC stations
    !
    do i = 1, stacnt
       m = mnrtcsta(1, i)
       n = mnrtcsta(2, i)
       if (zmodel) then
          do k = 1, kmax
             if (k>kfsmin(n, m) .and. k<kfsmax(n, m)) then
                zrtcsta(k, i) = 0.5_fp * (zk(k)+zk(k-1))
             elseif (k == kfsmin(n,m)) then
                zrtcsta(k, i) = 0.5_fp * (zk(k)-dps(n,m))
             elseif (k == kfsmax(n,m)) then
                zrtcsta(k, i) = 0.5_fp * (s1(n,m)+zk(k-1))
             else
                zrtcsta(k, i) = -999.0_sp
             endif
          enddo
       else
          do k = 1, kmax
             if (kfs(n, m) == 1) then
                zrtcsta(k, i) = sig(k) * (dps(n,m)+s1(n,m)) + s1(n,m)
             else
                zrtcsta(k, i) = -999.0_sp
             endif
          enddo
       endif
       s1rtcsta(i) = s1(n,m)
    enddo
end subroutine zrtc
