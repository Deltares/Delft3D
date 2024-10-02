subroutine findnmk(nlb    ,nub    ,mlb    ,mub    ,xz     , &
                 & yz     ,dps    ,s1     ,kcs    ,thick  , &
                 & kmax   ,x_jet  ,y_jet  ,z_jet  ,n_jet  , &
                 &  m_jet ,k_jet  ,kfsmn0 ,kfsmx0 ,dzs0   , &
                 & zmodel ,in_col ,gdp    )
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
!    Function: Finds n,m and k coordinates of "jet" points
!
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
! Global variables
!
    integer                                             , intent(in)  :: nlb
    integer                                             , intent(in)  :: nub
    integer                                             , intent(in)  :: mlb
    integer                                             , intent(in)  :: mub
    integer                                       , intent(in)  :: kmax   !  Description and declaration in tricom.igs
    integer                                             , intent(out) :: n_jet
    integer                                             , intent(out) :: m_jet
    integer                                       , intent(out) :: k_jet
    integer    , dimension(nlb:nub,mlb:mub)             , intent(in)  :: kcs    ! Description and declaration in esm_alloc_int.f90
    integer    , dimension(nlb:nub,mlb:mub)             , intent(in)  :: kfsmn0 ! Description and declaration in esm_alloc_int.f90
    integer    , dimension(nlb:nub,mlb:mub)             , intent(in)  :: kfsmx0 ! Description and declaration in esm_alloc_int.f90
    real(fp)                                      , intent(in)  :: x_jet
    real(fp)                                      , intent(in)  :: y_jet
    real(fp)                                      , intent(in)  :: z_jet
    real(fp)   , dimension(nlb:nub,mlb:mub)             , intent(in)  :: s1     ! Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(nlb:nub,mlb:mub)             , intent(in)  :: xz     ! Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(nlb:nub,mlb:mub)             , intent(in)  :: yz     ! Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(kmax)                        , intent(in)  :: thick  ! Description and declaration in esm_alloc_real.f90
    real(prec) , dimension(nlb:nub,mlb:mub)             , intent(in)  :: dps    ! Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(nlb:nub,mlb:mub, kmax)       , intent(in)  :: dzs0   ! Description and declaration in esm_alloc_real.f90
    logical                                             , intent(in)  :: zmodel
    logical                                             , intent(out) :: in_col ! false: above or under water column
!
! Local variables
!
    integer       :: k
    integer       :: n
    integer       :: m
    real(fp)      :: dist
    real(fp)      :: distmin
    real(fp)      :: r_below
    real(fp)      :: r_above
!
!! executable statements -------------------------------------------------------
!
    !
    ! Find the horizontal nm coordinate of the "Jet" trajectory point
    !
    n_jet   = 0
    m_jet   = 0
    distmin = 1.0e+30_fp
    do m = mlb, mub
       do n = nlb, nub
          if (kcs(n,m) == 1) then
             dist = sqrt((xz(n,m) - x_jet)**2 + (yz(n,m) - y_jet)**2)
          if (dist < distmin) then
                n_jet   = n
                m_jet   = m
             distmin = dist
          endif
       endif
    enddo
    enddo
    !
    ! Find the vertical position
    !
    k_jet  = 0
    in_col = .true.
    !
    ! k_ket=0 has a special meaning (full vertical).
    ! Unfortunately, z_jet=0.0 is a valid value.
    ! Use z_jet=999 or -999 to indicate k_jet=0
    !
    if (abs(z_jet)>998.999_fp .and. abs(z_jet)<999.999_fp) return
    !
    ! Note that for sigma-models the layer k is from the top downwards,
    ! whereas for z-models, it is from the bottom upwards
    ! This results in reversed loops at the end of desa.f90
    ! i.e stil from k_end_top to k_end_down, but with a -1 step size    
    !
    r_above = -1.0_fp * s1(n_jet,m_jet)
    if (z_jet < r_above) then
       in_col = .false.
    endif
    if (.not. zmodel) then
       do k = 1, kmax - 1
          r_below = r_above + thick(k)*(real(dps(n_jet,m_jet),fp) + s1(n_jet,m_jet))
          if (z_jet < r_below) then
          k_jet = k
          exit
       endif
          r_above = r_below
    enddo
       if (k_jet == 0) k_jet = kmax
    else
       do k = kfsmx0(n_jet,m_jet), kfsmn0(n_jet,m_jet) + 1,-1
          r_below = r_above + dzs0(n_jet,m_jet,k)
          if (z_jet < r_below) then
             k_jet = k
             exit
          endif
          r_above = r_below
       enddo
       if (k_jet == 0) k_jet = kfsmn0(n_jet,m_jet)
    endif
    if (z_jet > real(dps(n_jet,m_jet),fp)) then
       in_col = .false.
    endif
end subroutine findnmk
