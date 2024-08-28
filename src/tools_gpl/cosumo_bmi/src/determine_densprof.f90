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
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
!
module m_densprof

contains
!
!
!==============================================================================
!> Determines density profile following CORMIX definition
subroutine determine_densprof(kbot   , ktop   , ha    , hd     , &
                            & stype1 , stype2 , rhoam , &
                            & rhoas  , rhoab  , hint  , drohj  )
    use precision
    use m_nfl_data
    !
    implicit none
    !
    ! Global variables
    !
    integer       , intent(in)  :: kbot
    integer       , intent(in)  :: ktop
    real(hp)      , intent(in)  :: ha            !< Water depth at ambient point
    real(hp)      , intent(in)  :: hd            !< Water depth at diffuser
    real(hp)      , intent(out) :: rhoam
    real(hp)      , intent(out) :: rhoas         !< Density at surface at ambient point
    real(hp)      , intent(out) :: rhoab         !< Density at bottom  at ambient point
    real(hp)      , intent(out) :: hint
    real(hp)      , intent(out) :: drohj
    character(len=1), intent(out) :: stype1
    character(len=1), intent(out) :: stype2
    !
    ! Local variables
    !
    integer                                :: ierror
    integer                                :: k
    integer                                :: kmax
    integer                                :: kgrad
    real(hp)                               :: dengra
    real(hp)                               :: d_diff
    real(hp)                               :: maxgrad
    real(hp)                               :: thck
    real(hp) , dimension(:) , allocatable  :: h1
    real(hp) , dimension(:) , allocatable  :: rhoa
    !
    !! executable statements -------------------------------------------------------
    !
    kmax = ktop-kbot+1
    allocate (h1   (kmax), stat=ierror)
    allocate (rhoa (kmax), stat=ierror)
    h1   = 0.0_hp
    rhoa = 0.0_hp
    !
    ! Compute parameters density profile C,
    ! start with computing heigths and densities at the cell centers
    ! of the position where the ambient conditions are taken from
    !
    h1  (1)  = 0.5_hp * (fm_z_level(kbot) - fm_z_level(kbot-1))
    rhoa(1)  = fm_rho(kbot)
    !
    do k = 2, kmax
        thck     = 0.5_hp * (fm_z_level(kbot+k-1) - fm_z_level(kbot+k-3))
        h1   (k) = h1(k-1) + thck
        rhoa (k) = fm_rho(kbot+k-1)
    enddo
    !
    ! Determine the density gradients,
    ! determine maxmimum density gradient and its location, but first,
    ! ensure stable density profile
    !
    do k = 1, kmax-1
        if (rhoa(k) < rhoa(k+1)) then
            rhoa(k+1) = rhoa(k)
        endif
    enddo
    !
    maxgrad = -1.0e36_hp
    do k = 1, kmax-1
        dengra = (rhoa(k) - rhoa(k+1))
        if (dengra > maxgrad) then
            maxgrad = dengra
            kgrad   = k
        endif
    enddo
    !
    ! Determine Profile type C parameters
    !
    rhoab = rhoa(1)
    rhoas = 0.0_hp
    !
    do k = kgrad + 1, kmax
        rhoas = rhoas + rhoa(k)/(kmax - kgrad)
    enddo
    !
    hint  = 0.5_hp*(h1(kgrad + 1) + h1 (kgrad))
    drohj = min(rhoa(kgrad) - rhoa(kgrad + 1), (rhoab - rhoas)-0.01_hp)
    !
    ! Adjust hint such that it is accepted by cormix
    !
    if (hint > 0.89_hp*ha .or. hint > 0.89_hp*hd) then
        hint = min(0.85_hp*ha,0.85_hp*hd)
    endif
    !
    if (hint < 0.41_hp*ha .or. hint < 0.41_hp*hd) then
        hint = max(0.45_hp*ha,0.45_hp*hd)
    endif
    !
    ! Determine profile type
    ! IMPORTANT: below the original density RHO is (partly) used, not the reversed and modified RHOA!
    !
    d_diff = rhoa(1) - rhoa(kmax)
    if (d_diff < 0.2_hp) then
        stype1 = 'U'
        rhoam = 0.0_hp
        do k = 1, kmax
            rhoam = rhoam + fm_rho(kbot+k-1)*(fm_z_level(kbot+k-1) - fm_z_level(kbot+k-2))/max(ha,0.01_hp)
        enddo
    else
        stype1 = 'S'
        if (maxgrad < 0.5_hp*d_diff) then
            rhoas  = fm_rho(ktop)
            rhoab  = fm_rho(kbot)
            stype2 = 'A'
        else
            stype2 = 'C'
        endif
    endif
    !
    deallocate (h1  , stat=ierror)
    deallocate (rhoa, stat=ierror)
end subroutine determine_densprof


end module m_densprof
