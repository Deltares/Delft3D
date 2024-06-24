subroutine determine_densprof(kmax       ,thick      ,s0       ,dps    ,rho       , &
                            & ha         ,hd         ,stype1   ,stype2 ,rhoam     , &
                            & rhoas      ,rhoab      ,hint     ,drohj  , &
                            & kfsmin_amb ,kfsmax_amb ,dzs0_amb ,zmodel )
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
!  $Id: determine_densprof.f90 5916 2016-03-02 15:36:18Z platzek $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/determine_densprof.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Determines density profile following CORMIX definition
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Global variables
!
    integer                                                     , intent(in)  :: kmax
    integer                                                     , intent(in)  :: kfsmin_amb
    integer                                                     , intent(in)  :: kfsmax_amb 
    real(fp)   , dimension(kmax)                                , intent(in)  :: dzs0_amb
    real(fp)                                                    , intent(in)  :: s0
    real(fp)                                                    , intent(in)  :: dps
    real(fp)                                                    , intent(in)  :: ha
    real(fp)                                                    , intent(in)  :: hd
    real(fp)   , dimension(kmax)                                , intent(in)  :: rho
    real(fp)   , dimension(kmax)                                , intent(in)  :: thick
    real(fp)                                                    , intent(out) :: rhoam
    real(fp)                                                    , intent(out) :: rhoas
    real(fp)                                                    , intent(out) :: rhoab
    real(fp)                                                    , intent(out) :: hint
    real(fp)                                                    , intent(out) :: drohj
    logical                                                     , intent(in)  :: zmodel
    
    character*1                                                 , intent(out) :: stype1
    character*1                                                 , intent(out) :: stype2
!
! Local variables
!
    integer                                :: ierror
    integer                                :: k
    integer                                :: k0
    integer                                :: k1
    integer                                :: kgrad
    real(fp)                               :: dengra
    real(fp)                               :: d_diff
    real(fp)                               :: maxgrad
    real(fp)                               :: thck
    real(fp) , dimension(:) , allocatable  :: h1
    real(fp) , dimension(:) , allocatable  :: rhoa
!
!! executable statements -------------------------------------------------------
!
    !
    allocate (h1   (kmax), stat=ierror)
    allocate (rhoa (kmax), stat=ierror)
    h1   = 0.0_fp
    rhoa = 0.0_fp
    !
    ! Compute parameters density profile C,
    ! start with computing heigths and densities at the cell centers
    ! of the position where the ambient conditions are taken from
    ! (switch positive direction from positive downward to positive upward)
    !
    if (.not. zmodel) then
        k0 = 1
        k1 = kmax
        !
        h1(k0)   = 0.5_fp * thick(kmax) * (s0+dps)
        rhoa(k0) = rho(kmax)
        !
        do k = 2, kmax
           thck     = 0.5_fp * (thick(kmax-k+2) + thick(kmax-k+1))
           h1 (k)   = h1(k-1) + thck*(s0 + dps)
           rhoa (k) = rho(kmax-k+1)
        enddo
    else
        k0 = kfsmin_amb
        k1 = kfsmax_amb
        !
        h1(k0)   = 0.5_fp * dzs0_amb(k0)
        rhoa(k0) = rho(k0)
        !
        do k = k0+1, k1
           thck     = 0.5_fp * (dzs0_amb(k) + dzs0_amb(k-1))
           h1 (k)   = h1(k-1) + thck
           rhoa (k) = rho(k)
        enddo
    endif
    !
    ! Determine the density gradients,
    ! determine maxmimum density gradient and its location, but first,
    ! ensure stable density profile
    !
    do k = k0, k1 - 1
       if (rhoa(k) < rhoa(k+1)) then
          rhoa(k+1) = rhoa(k)
        endif
    enddo
    !
    maxgrad = -1.0e36_fp
    do k = k0, k1 - 1
       dengra = (rhoa(k) - rhoa(k+1))
       if (dengra > maxgrad) then
          maxgrad = dengra
          kgrad   = k
       endif
    enddo
    !
    ! Determine Profile type C parameters
    !
    rhoab = rhoa(k0)
    rhoas = 0.0_fp
    !
    do k = kgrad + 1, k1
       rhoas = rhoas + rhoa(k)/(k1 - kgrad)
    enddo
    !
    hint  = 0.5_fp*(h1(kgrad + 1) + h1 (kgrad))
    drohj = min(rhoa(kgrad) - rhoa(kgrad + 1), (rhoab - rhoas)-0.01_fp)
    !
    ! Adjust hint such that it is accepted by cormix
    !
    if (hint > 0.89_fp*ha .or. hint > 0.89_fp*hd) then
       hint = min(0.85_fp*ha,0.85_fp*hd)
    endif
    !
    if (hint < 0.41_fp*ha .or. hint < 0.41_fp*hd) then
       hint = max(0.45_fp*ha,0.45_fp*hd)
    endif
    !
    ! Determine profile type
    ! IMPORTANT: below the original density RHO is (partly) used, not the reversed and modified RHOA!
    !
    d_diff = rhoa(k0) - rhoa(k1)
    if (.not. zmodel) then
       if (d_diff < 0.2_fp) then
          stype1 = 'U'
          rhoam = 0.0_fp
          do k = k0, k1
             rhoam = rhoam + rho(k)*thick(k)
          enddo
       else
          stype1 = 'S'
          if (maxgrad < 0.5_fp*d_diff) then
             rhoas  = rho(k1)
             rhoab  = rho(k0)
             stype2 = 'A'
          else
             stype2 = 'C'
          endif
       endif
    else
       !
       ! Z-model
       !
       d_diff = rhoa(k0) - rhoa(k1)
       if (d_diff < 0.2_fp) then
          stype1 = 'U'
          rhoam = 0.0_fp
          do k = k0, k1
             rhoam = rhoam + rho(k)*dzs0_amb(k)/max(s0 + dps,0.01_fp)
          enddo
       else
          stype1 = 'S'
          if (maxgrad < 0.5_fp*d_diff) then
             rhoas  = rho(k1)
             rhoab  = rho(k0)
             stype2 = 'A'
          else
             stype2 = 'C'
          endif
       endif
    endif
    !
    deallocate (h1  , stat=ierror)
    deallocate (rhoa, stat=ierror)

end subroutine determine_densprof
