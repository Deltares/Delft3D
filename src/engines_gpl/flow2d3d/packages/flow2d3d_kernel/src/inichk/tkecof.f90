subroutine tkecof(lturi     ,vonkar    ,sigdif    ,sigmol    , &
                & rtur1     ,rtu2d1    ,gdp       )
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
!    Function: initialises RTUR1 and RTU2D1
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
!
! Local pointers to GDP-data
!
! turcoe.igd:
!
    type (gd_turcoe) , pointer::gdturcoe 
    real(fp), pointer :: cde !  Description and declaration in turcoe.igs
    real(fp), pointer :: cmukl !  Description and declaration in turcoe.igs
    real(fp), pointer :: cmukep !  Description and declaration in turcoe.igs
    real(fp), pointer :: cep1 !  Description and declaration in turcoe.igs
    real(fp), pointer :: cep2 !  Description and declaration in turcoe.igs
    real(fp), pointer :: cep3 !  Description and declaration in turcoe.igs
    real(fp), pointer :: sigrho !  Description and declaration in turcoe.igs
    real(fp), pointer :: cewall !  Description and declaration in turcoe.igs
    real(fp), pointer :: ceta
    real(fp), pointer :: ck
    real(fp), pointer :: sigq2e
    real(fp), pointer :: lrdamp_fac
!
! dimens.igd:
!
    type (gd_dimens) , pointer::gddimens !
    integer, pointer :: nmax !  Description and declaration in esm_alloc_int.f90
    integer, pointer :: mmax !  Description and declaration in esm_alloc_int.f90
    integer, pointer :: kmax !  Description and declaration in esm_alloc_int.f90
    integer, pointer :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer, pointer :: lsal !  Description and declaration in dimens.igs
    integer, pointer :: lsed !  Number of suspended sediment fractions
    integer, pointer :: ltem !  Description and declaration in dimens.igs
    integer, pointer :: ltur !  Description and declaration in esm_alloc_int.f90
    integer, pointer :: ltur2d !  Description and declaration in dimens.igs
!
    real(fp)         , dimension(:)      , pointer :: tpsnumber ! turbulent Prandtl-Schmidt number for sediment fractions
!
! Global variables
!
    integer                , intent(in)  :: lturi !  Description and declaration in tricom.igs
    real(fp)               , intent(in)  :: vonkar !  Description and declaration in physco.igs
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:gdp%d%kmax, gdp%d%ltur), intent(out) :: rtur1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 2)                       , intent(out) :: rtu2d1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%lstsci), intent(out) :: sigdif !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%lstsci), intent(out) :: sigmol !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: k
    integer :: l
    integer :: lsedmin
    integer :: lsedmax
    integer :: m
    integer :: n
!
!! executable statements -------------------------------------------------------
!
    ! Local pointers refering to global data
    !
    gdturcoe   => gdp%gdturcoe
    cde        => gdturcoe%cde
    cmukl      => gdturcoe%cmukl
    cmukep     => gdturcoe%cmukep
    cep1       => gdturcoe%cep1
    cep2       => gdturcoe%cep2
    cep3       => gdturcoe%cep3
    sigrho     => gdturcoe%sigrho
    cewall     => gdturcoe%cewall
    ceta       => gdturcoe%ceta
    ck         => gdturcoe%ck
    sigq2e     => gdturcoe%sigq2e
    lrdamp_fac => gdturcoe%lrdamp_fac
    !
    gddimens => gdp%d
    nmax     => gddimens%nmax
    mmax     => gddimens%mmax
    kmax     => gddimens%kmax
    lstsci   => gddimens%lstsci
    lsal     => gddimens%lsal
    lsed     => gddimens%lsed
    ltem     => gddimens%ltem
    ltur     => gddimens%ltur
    ltur2d   => gddimens%ltur2d
    !
    tpsnumber => gdp%gdsedpar%tpsnumber
    !
    ! Initialisation
    !
    cmukep     = 0.09
    cmukl      = cmukep**0.25
    cde        = cmukep**0.75
    cep1       = 1.44
    cep2       = 1.92
    cep3       = 1.
    sigrho     = 0.7
    cewall     = cmukep**.75/vonkar
    !
    ! lrdamp_fac is used when low Reynolds number damping is switched on
    ! See subroutine redvic
    !
    lrdamp_fac = 1.0_fp / (cmukep * gdp%gdphysco%vicmol * 100.0_fp)
    !
    ! Initialisation coefficients for 2D turbulence coefficients
    !
    ceta = 1.
    sigq2e = 1.
    ck = 1.
    !
    ! Prandtl/Schmidt- and Moleculair Prandtl numbers for constituents,
    ! salt and temperature
    !
    lsedmin = max(lsal, ltem) + 1
    lsedmax = max(lsal, ltem) + lsed
    do l = 1, lstsci
       if (l == lsal) then
          sigmol(l) = 700.0
       elseif (l == ltem) then
          sigmol(l) = 6.7
       else ! constituents
          sigmol(l) = 1.0
       endif
       !
       if (l >= lsedmin .and. l <= lsedmax) then
           sigdif(l) = tpsnumber(l - lsedmin + 1)
       else
           sigdif(l) = 0.7
       endif
    enddo
    !
    ! Initialization of Prandtl/Schmidt-number for turbulence in RTUR1
    ! for K and EPS (LTUR=2)
    !
    if (lturi>0) then
       do l = 1, ltur
          do k = 0, kmax
             do m = -1, mmax + 2
                do n = 1, nmax
                   rtur1(n, m, k, l) = 1.E-7
                enddo
             enddo
          enddo
       enddo
    !
    ! Initialization of Prandtl/Schmidt-number for turbulence in RTUR1
    ! for EPS (LTUR=2)
    !
    elseif (lturi<0) then
       do l = 2, ltur
          do k = 0, kmax
             do m = -1, mmax + 2
                do n = 1, nmax
                   rtur1(n, m, k, l) = 1.E-7
                enddo
             enddo
          enddo
       enddo
    else
    endif
    !
    ! Initialization of turbulence in RTU2D1
    ! for TKE and Enstrophy
    !
    if (ltur2d>0) then
       do m = -1, mmax + 2
          do n = 1, nmax
             rtu2d1(n, m, 1) = 1.E-7
             rtu2d1(n, m, 2) = 1.E-7
          enddo
       enddo
    endif
end subroutine tkecof
