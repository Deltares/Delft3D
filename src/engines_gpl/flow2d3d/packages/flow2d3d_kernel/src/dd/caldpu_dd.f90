subroutine caldpu_dd(nmaxus ,mmax   ,kcs    ,kcu    ,kcv    , &
                   & umean  ,vmean  ,dp     ,dps    ,dpu    , &
                   & dpv    ,gdp    ) 
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
!     Correction for coupling points. Apply this only at the subdomain
!     interface:
!                       u s u s u s   u   s u
!     KCU at Domain  I: 1 + 1 + 1 + [1/3] + 3
!     KCU at Domain II:         3 + [3/1] + 1 + 1 + 1
!                               u s   u   s u s u s u
!                                     ^
!                                     | subdomain interface
!
!                       u s u s u s   u   s u
!     KCS at Domain  I: - 1 - 1 - 1   -   3 -
!     KCS at Domain II:         - 3   -   1 - 1 - 1 -
!                               u s   u   s u s u s u
!                                     ^
!                                     | subdomain interface
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
    character(8) , pointer :: dpuopt
!
! Global variables
!
    integer                                                        , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                        , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcv    !  Description and declaration in esm_alloc_int.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: dp     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: dpv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: vmean  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: ddb
    integer :: m   ! Help var. do loop MMAX 
    integer :: md  ! Help var. M-1 
    integer :: mu
    integer :: n   ! Help var. do loop NMAX 
    integer :: nd  ! Help var. N-1 
    integer :: nu
!
!! executable statements -------------------------------------------------------
!
    dpuopt  => gdp%gdnumeco%dpuopt
    !
    ddb = gdp%d%ddbound
    !
    do n = 1 - ddb, nmaxus
       do m = 1 - ddb, mmax
          md = max(m - 1, 1 - ddb)
          mu = min(m + 1, mmax)
          nd = max(n - 1, 1 - ddb)
          nu = min(n + 1, nmaxus)
          !
          if (kcu(n, m)==3) then
             if (kcs(n, m)*kcs(n, mu)==3) then
                !
                ! at subdomain interface
                !
                if (dpuopt=='UPW') then
                   if (umean(n, m)>=0.001_fp) then
                      dpu(n, m) = real(dps(n, m),fp)
                   elseif (umean(n, m)<= - 0.001_fp) then
                      dpu(n, m) = real(dps(n, mu),fp)
                   else
                      dpu(n, m) = min(real(dps(n, mu),fp), real(dps(n, m),fp))
                   endif
                elseif (dpuopt=='MIN') then
                   dpu(n, m) = min(real(dps(n, mu),fp), real(dps(n, m),fp))
                elseif (dpuopt=='MEAN') then
                   dpu(n, m) = 0.5_fp*(dp(n, m) + dp(nd, m))
                elseif (dpuopt=='MEAN_DPS') then
                   dpu(n, m) = 0.5_fp*(real(dps(n, mu) + dps(n, m),fp))
                else
                endif
             elseif (kcs(n, m)==0 .and. kcs(n, mu)==3) then
                !
                ! cell is left of the interface
                !
                dpu(n, m) = real(dps(n, mu),fp)
             elseif (kcs(n, md)==1 .and. kcs(n, m)==3) then
                !
                ! cell is right of the interface
                !
                dpu(n, m) = real(dps(n, m),fp)
             else
                dpu(n, m) = real(dps(n, m),fp)
             endif
          endif
          !
          if (kcv(n, m)==3) then
             if (kcs(n, m)*kcs(nu, m)==3) then
                !
                ! at subdomain interface
                !
                if (dpuopt=='UPW') then
                   if (vmean(n, m)>=0.001_fp) then
                      dpv(n, m) = real(dps(n, m),fp)
                   elseif (vmean(n, m)<= - 0.001_fp) then
                      dpv(n, m) = real(dps(nu, m),fp)
                   else
                      dpv(n, m) = min(real(dps(nu, m),fp), real(dps(n, m),fp))
                   endif
                elseif (dpuopt=='MIN') then
                   dpv(n, m) = min(real(dps(nu, m),fp), real(dps(n, m),fp))
                elseif (dpuopt=='MEAN') then
                   dpv(n, m) = 0.5_fp*(dp(n, m) + dp(n, md))
                elseif (dpuopt=='MEAN_DPS') then
                   dpv(n, m) = 0.5_fp*(real(dps(nu, m) + dps(n, m),fp))
                else
                endif
             elseif (kcs(n, m)==0 .and. kcs(nu, m)==3) then
                !
                ! cell is below the interface
                !
                dpv(n, m) = real(dps(nu, m),fp)
             elseif (kcs(nd, m)==1 .and. kcs(n, m)==3) then
                !
                ! cell is above the interface
                !
                dpv(n, m) = real(dps(n, m),fp)
             else
                dpv(n, m) = real(dps(n, m),fp)
             endif
          endif
       enddo
    enddo
end subroutine caldpu_dd
