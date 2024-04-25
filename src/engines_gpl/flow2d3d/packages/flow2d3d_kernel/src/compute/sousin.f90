subroutine sousin(j         ,nmmaxj    ,nmmax     ,kmax      ,lstsci    , &
                & lstsc     ,lsal      ,ktemp     ,ltem      ,lsts      , &
                & kfs       ,kfsmin    ,kfsmax    ,gsqs      ,thick     , &
                & s0        ,dps       ,volum0    ,sour      ,sink      , &
                & evap      ,precip    ,decay     ,kcs       ,gdp       )
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
!    Function: Initialisation of the arrays SOUR and SINK.
!              Rain temperature added as source term, evaporation
!              is added as sink term when KTEMP = 0
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
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: train
    logical                , pointer :: temp
    logical                , pointer :: const
    logical                , pointer :: sedim
    logical                , pointer :: zmodel
!
! Global variables
!
    integer                                                           :: j      !!  Begin pointer for arrays which have
                                                                                !!  been transformed into 1D arrays.
                                                                                !!  Due to the shift in the 2nd (M-)
                                                                                !!  index, J = -2*NMAX + 1
    integer                                             , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                             , intent(in)  :: ktemp  !  Description and declaration in tricom.igs
    integer                                                           :: lsal   !  Description and declaration in dimens.igs
    integer                                             , intent(in)  :: lsts   !  Description and declaration in dimens.igs
    integer                                             , intent(in)  :: lstsc  !  Description and declaration in dimens.igs
    integer                                             , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                             , intent(in)  :: ltem   !  Description and declaration in dimens.igs
    integer                                             , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                           :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)           , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)           , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)           , intent(in)  :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)           , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: evap   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: precip !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: volum0 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: sink   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(out) :: sour   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                               , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lstsc)                              , intent(in)  :: decay  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: k     ! Loop counter for KMAX 
    integer :: k0
    integer :: l     ! Loop counter for LSTSCI 
    integer :: nm    ! Loop counter for NMMAX 
    real(fp):: dz1
    real(fp):: sourt
!
!! executable statements -------------------------------------------------------
!
    temp       => gdp%gdprocs%temp
    const      => gdp%gdprocs%const
    sedim      => gdp%gdprocs%sedim
    zmodel     => gdp%gdprocs%zmodel
    train      => gdp%gdheat%train
    rhow       => gdp%gdphysco%rhow
    !
    do l = 1, lstsci
       do k = 1, kmax
          do nm = 1, nmmax
             sour(nm, k, l) = 0.0
             sink(nm, k, l) = 0.0
          enddo
       enddo
    enddo
    !
    ! INITIALIZE for LTEM the RAIN / EVAPORATION is taken into acount
    !       EVAPOR is INPUT value for KEVA > 0. For KTEMP <> 0 will be
    !       calculated in HEATU so will not added to SINK here
    !
    if (ltem/=0) then
       !
       ! Calculate the uniform source term containing the temperature
       ! of the rainfall (all terms are in SI-units)
       ! EVAP is already multiplied with RHOW and has to be
       ! divided.
       !
       do nm = 1, nmmax                 
          if (kcs(nm)>0) then
             if (zmodel) then
                k0 = max(kfsmin(nm), kfsmax(nm))
                sour(nm, k0, ltem) = precip(nm)*train*gsqs(nm)
                if (ktemp==0 .and. kfs(nm)/=0) then
                   sink(nm, k0, ltem) = evap(nm)*gsqs(nm)/rhow
                endif
             else
                k0 = 1
                dz1 = max(0.01_fp, s0(nm) + real(dps(nm),fp))*thick(k0)
                sour(nm, k0, ltem) = precip(nm)*train/dz1
                if (ktemp==0 .and. kfs(nm)/=0) then
                   sink(nm, k0, ltem) = evap(nm)/(rhow*dz1)
                endif
             endif
          endif
       enddo
    endif
    !
    ! INITIALIZE DECAY for conservative constituents, excluding the sediment
    !     where E (-Kt) is used for Decay and Decay rate K is added
    !     to SINK (=> BBKL) as positive value.
    !
    if (zmodel) then
       do l = lsts + 1, lstsc
          do nm = 1, nmmax
             do k = kfsmin(nm), kfsmax(nm)
                sink(nm, k, l) = sink(nm, k, l) + decay(l)*volum0(nm,k)
             enddo
          enddo
       enddo
    else
       do l = lsts + 1, lstsc
          do k = 1, kmax
             do nm = 1, nmmax
                sink(nm, k, l) = sink(nm, k, l) + decay(l)
             enddo
          enddo
       enddo
    endif
end subroutine sousin
