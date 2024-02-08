subroutine waveu(nmmax     ,kfs       ,sourw     , &
               & sinkw     ,ewave0    ,dps       ,s0        ,tp        , &
               & c         ,hbd       ,ewabr1    ,df        ,gdp       )
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
! Computes wave energy dissipation terms due to breaking and bottom friction
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                , pointer :: ndis
    integer                , pointer :: lundia
    real(fp)               , pointer :: alfarol
    real(fp)               , pointer :: gamdis
    real(fp)               , pointer :: f_lam
    real(fp)               , pointer :: fwee
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: timmin    
    logical                , pointer :: wave
    logical                , pointer :: wavcmp
    logical                , pointer :: cnstwv
    logical                , pointer :: snelli
    character(5)           , pointer :: disform
!
! Global variables
!
    integer                                     , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: c      !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: ewabr1 !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: ewave0 !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: hbd    !  breaker delay depth
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: df     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: sinkw  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: sourw  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: tp     !  Description and declaration in esm_alloc_real.f90
    !
    ! Global variables for Gamma according to Battjes & Stive
    !
!
! Local variables
!
    integer :: nm
    real(fp):: afp
    real(fp):: dw
    real(fp):: hb2
    real(fp):: hdis
    real(fp):: hrms2
    real(fp):: kwav
    real(fp):: zdis
    real(fp):: whdis
    real(fp):: gamdisi
    !
    ! Local variables for Gamma according to Battjes & Stive
    !
    integer                :: nwav
    integer, dimension(7)  :: isdir
    real(fp)               :: hrms0
    real(fp)               :: gamBaSti
    real(fp)               :: rlabda0
    real(fp)               :: tp0
    real(fp), dimension(7) :: wavcon
    character(37)          :: wavnam
!
!! executable statements -------------------------------------------------------
!
    data isdir/0, 0, 1, 0, 0, 0, 1/ 
    !
    ndis       => gdp%gdbetaro%ndis
    lundia     => gdp%gdinout%lundia
    alfarol    => gdp%gdbetaro%alfarol
    gamdis     => gdp%gdbetaro%gamdis
    f_lam      => gdp%gdbetaro%f_lam
    disform    => gdp%gdbetaro%disform
    fwee       => gdp%gdbetaro%fwee
    rhow       => gdp%gdphysco%rhow
    ag         => gdp%gdphysco%ag
    wave       => gdp%gdprocs%wave
    wavcmp     => gdp%gdprocs%wavcmp
    cnstwv     => gdp%gdprocs%cnstwv
    snelli     => gdp%gdprocs%snelli
    timmin     => gdp%gdinttim%timmin
    !
    if (wavcmp) then
       if (disform == 'R2004') then
          !
          ! Formulation Roelvink (2004)
          !
          do nm = 1, nmmax
             sinkw(nm)  = 0.0_fp
             sourw(nm)  = 0.0_fp
             ewabr1(nm) = min(ewabr1(nm),1.0_fp)
             ewabr1(nm) = max(ewabr1(nm),0.0_fp)
             if (kfs(nm) /= 0) then
                hdis = max (0.1_fp , s0(nm)+real(dps(nm),fp))
                if (tp(nm) < 0.02_fp) then
                   afp = 1.0_fp
                else
                   afp = alfarol/tp(nm)
                endif
                if (ewave0(nm) < 1.0e-6_fp) then
                   whdis = 1.0e-6_fp
                else
                   whdis = sqrt(8.0_fp * ewave0(nm) / (rhow*ag))
                endif
                sinkw(nm) = 2.0_fp * afp * whdis / hdis * ewabr1(nm)
                if (whdis > gamdis*hdis) then
                   ewabr1(nm)=1.0_fp
                endif
                if (whdis < 0.3_fp*hdis) then
                   ewabr1(nm)=0.0_fp
                endif
             endif
          enddo
       else
          !
          ! Formulation Roelvink (1993)
          !
          do nm = 1, nmmax
             sinkw(nm) = 0.0_fp
             sourw(nm) = 0.0_fp
             if (kfs(nm) /= 0) then
                hdis = max (0.1_fp , s0(nm)+real(dps(nm),fp))
                if (tp(nm) < 0.02_fp) then
                   afp = 1.0_fp
                else
                   afp = alfarol / tp(nm)
                endif
                if (ewave0(nm) < 0.0_fp) then
                   zdis = 0.0_fp
                else
                   zdis = (sqrt(8.0_fp*ewave0(nm)/(rhow*ag)) / (gamdis*hdis))**ndis
                endif
                sinkw(nm) = 2.0_fp * afp * (1.0_fp-exp(-zdis))
             endif
          enddo
       endif
    else
       if (disform == 'R2004') then
          !
          ! Formulation Roelvink (2004)
          !
          do nm = 1, nmmax
             sinkw(nm)  = 0.0_fp
             sourw(nm)  = 0.0_fp
             ewabr1(nm) = min(ewabr1(nm),1.0_fp)
             ewabr1(nm) = max(ewabr1(nm),0.0_fp)
             if (kfs(nm) /= 0) then
                hdis = max (0.01_fp , s0(nm)+real(dps(nm),fp))
                if (tp(nm) < 0.02_fp) then
                   afp = 1.0_fp
                else
                   afp = alfarol/tp(nm)
                endif
                if (ewave0(nm) < 1.0e-6_fp) then
                   whdis = 1.0e-6_fp
                else
                   whdis = sqrt(8.0_fp * ewave0(nm) / (rhow*ag))
                endif
                sinkw(nm) = 2.0_fp * afp * whdis / hdis * ewabr1(nm)
                df(nm)  = sinkw(nm)*ewave0(nm)
              if (whdis > gamdis*hdis) then
                   ewabr1(nm)=1.0_fp
                endif
                if (whdis < 0.3_fp*hdis) then
                   ewabr1(nm)=0.0_fp
                endif
             endif
          enddo
       else
          !
          ! Formulation Baldock(1998)
          !      
          if (comparereal(gamdis,-2.0_fp) == 0) then
              !
              ! Gamma according to Battjes & Stive (1985)
              ! Initialize hrms0 & rlabda0
              ! reading of wavecon file for time series of waves
              ! name is built using runid as extension
              !
              if (cnstwv .or. snelli) then
                 write(wavnam,'(a,a)') 'wavecon.', trim(gdp%runid)
                 nwav   = 7
                 call varcon(wavnam, timmin, wavcon, isdir, nwav, gdp)
                 hrms0  = wavcon(1) / sqrt(2.0_fp)
                 tp0    = wavcon(2)
                 !
                 ! Compute the wave length. considering deep water at the boundary 
                 !
                 rlabda0 = ag * (tp0**2) / (2*pi)
                 !
                 ! Compute Gamma
                 !
                 gamBaSti = 0.5_fp + 0.4_fp*tanh(33.0_fp*hrms0/max(rlabda0,1.0e-12_fp))
              else
                 call prterr(lundia, 'U021', 'Battjes & Stive formulation can not be used without wavecon file')
                 call d3stop(1, gdp)
              endif   
          endif
          !
          do nm = 1, nmmax
             sourw(nm) = 0.0_fp
             if (kfs(nm)/=0 .and. tp(nm)>0.02_fp .and. ewave0(nm)>0.01_fp .and. c(nm)>0.01_fp) then
                hdis = max (0.1_fp , s0(nm)+real(dps(nm),fp))           
                afp  = alfarol / tp(nm)
                kwav = 2.0_fp* pi / (c(nm)*tp(nm))
                if (comparereal(gamdis,-1.0_fp) == 0) then
                   !
                   ! gamma according to Ruessink et al (2003) 
                   !
                   gamdisi = 0.76_fp*kwav*hdis + 0.29_fp
                elseif (comparereal(gamdis,-2.0_fp) == 0) then
                   !
                   ! gamma according to Battjes & Stive (1985) 
                   !
                   gamdisi = gamBaSti
                else
                   gamdisi = gamdis
                endif
                if (f_lam < 0.0_fp) then
                   hdis = hbd(nm)
                endif
                hb2       = (0.88_fp / kwav * tanh(gamdisi/0.88_fp*kwav*hdis) )**2
                hrms2     = ewave0(nm) * 8.0_fp/ rhow / ag
                dw        = 0.25_fp * afp * rhow * ag * exp(-hb2/hrms2) * (hb2+hrms2)
                df(nm)    = rhow * fwee * (pi**2.5_fp) * ( sqrt(hrms2)/(tp(nm)*sinh(kwav*hdis)) )**3
                sinkw(nm) = (dw+df(nm)) / ewave0(nm)
             else
                df(nm)    = 0.0_fp
                sinkw(nm) = 0.0_fp
             endif
          enddo
       endif
    endif
end subroutine waveu
