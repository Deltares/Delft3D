subroutine zrtc(mlb, mub, nlb, nub, kfs, kfsmin, kfsmax, sig, zk, s1, dps, r0, kmax, lstsci, gdp)
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
    logical                        , pointer :: zmodel
    real(fp)     , dimension(:,:,:), pointer :: r0rtcsta
    real(fp)     , dimension(:)    , pointer :: s1rtcsta
    real(fp)     , dimension(:,:)  , pointer :: zrtcsta
    integer                        , pointer :: stacnt
    integer      , dimension(:,:)  , pointer :: mnrtcsta
!
! Global variables
!
    integer                                           , intent(in) :: mlb
    integer                                           , intent(in) :: mub
    integer                                           , intent(in) :: nlb
    integer                                           , intent(in) :: nub
    integer   , dimension(nlb:nub,mlb:mub)            , intent(in) :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(nlb:nub,mlb:mub)            , intent(in) :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(nlb:nub,mlb:mub)            , intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer                                           , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                           , intent(in) :: lstsci !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(nlb:nub,mlb:mub)            , intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nlb:nub,mlb:mub,kmax,lstsci), intent(in) :: r0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nlb:nub,mlb:mub)            , intent(in) :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(  kmax)                     , intent(in) :: sig
    real(fp)  , dimension(0:kmax)                     , intent(in) :: zk
!
! Local variables
!
    integer   :: i
    integer   :: m
    integer   :: n
    integer   :: k
    integer   :: l
!
!! executable statements -------------------------------------------------------
!
    zmodel     => gdp%gdprocs%zmodel
    r0rtcsta   => gdp%gdrtc%r0rtcsta
    s1rtcsta   => gdp%gdrtc%s1rtcsta
    zrtcsta    => gdp%gdrtc%zrtcsta
    stacnt     => gdp%gdrtc%stacnt
    mnrtcsta   => gdp%gdrtc%mnrtcsta
    !
    ! Determine the elevation of the cell centres for all RTC stations
    !
    do i = 1, stacnt
       m = mnrtcsta(1, i)
       n = mnrtcsta(2, i)
       if (m==0) then
          ! value determined by other partition
          s1rtcsta(i)      = 0.0_fp
          zrtcsta(:, i)    = 0.0_fp
          r0rtcsta(:,:, i) = 0.0_fp
       elseif (zmodel) then
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
             do l = 1,lstsci
                r0rtcsta(l, k, i) = r0(n, m, k, l)
             enddo
          enddo
       else
          do k = 1, kmax
             if (kfs(n, m) == 1) then
                zrtcsta(k, i) = sig(k) * (dps(n,m)+s1(n,m)) + s1(n,m)
             else
                zrtcsta(k, i) = -999.0_sp
             endif
             do l = 1,lstsci
                r0rtcsta(l, k, i) = r0(n, m, k, l)
             enddo
          enddo
       endif
       s1rtcsta(i) = s1(n,m)
    enddo
end subroutine zrtc
