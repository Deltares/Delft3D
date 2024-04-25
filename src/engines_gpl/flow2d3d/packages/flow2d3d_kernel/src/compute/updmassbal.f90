subroutine updmassbal(imode     ,qxk       ,qyk       ,kcs       ,r1        , &
                    & volum0    ,volum1    ,sbuu      ,sbvv      ,disch     , &
                    & mnksrc    ,sink      ,sour      ,gsqs      ,guu       , &
                    & gvv       ,dps       ,rintsm    ,dt        ,gdp       )
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
!

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
    integer                        , pointer :: kmax
    integer                        , pointer :: nmmax
    integer                        , pointer :: lsed
    integer                        , pointer :: lsedtot
    integer                        , pointer :: lstsc
    integer                        , pointer :: lstsci
    logical                        , pointer :: massbal
    integer                        , pointer :: nbalpol
    integer                        , pointer :: nneighb
    logical                        , pointer :: resetfluxes
    integer      , dimension(:)    , pointer :: volnr
    integer      , dimension(:,:)  , pointer :: exchnr
    integer      , dimension(:,:)  , pointer :: neighb
    real(fp)     , dimension(:)    , pointer :: accdps
    real(fp)     , dimension(:)    , pointer :: volumes
    real(fp)     , dimension(:,:)  , pointer :: mass_r1
    real(fp)     , dimension(:,:)  , pointer :: fluxes
    real(fp)     , dimension(:,:,:), pointer :: fluxes_r1
    real(fp)     , dimension(:,:,:), pointer :: fluxes_sd
    !
    real(fp)                       , pointer :: morfac
    real(fp)     , dimension(:,:,:), pointer :: fluxu
    real(fp)     , dimension(:,:,:), pointer :: fluxv
    real(fp)     , dimension(:,:)  , pointer :: ssuu
    real(fp)     , dimension(:,:)  , pointer :: ssvv
!
! Global variables
!
    integer                                                                 , intent(in) :: imode ! 1 = initialize volumes (don't update fluxes), 2 = update fluxes, 3 = update fluxes and volumes
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                            , intent(in) :: kcs
    integer   , dimension(7, gdp%d%nsrc)                                    , intent(in) :: mnksrc
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,1:gdp%d%kmax,1:gdp%d%lstsci), intent(in) :: r1
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,1:gdp%d%kmax,1:gdp%d%lstsci), intent(in) :: sink
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,1:gdp%d%kmax,1:gdp%d%lstsci), intent(in) :: sour
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,1:gdp%d%kmax)               , intent(in) :: volum0
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,1:gdp%d%kmax)               , intent(in) :: volum1
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,1:gdp%d%kmax)               , intent(in) :: qxk
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,1:gdp%d%kmax)               , intent(in) :: qyk
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,gdp%d%lsedtot)              , intent(in) :: sbuu
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,gdp%d%lsedtot)              , intent(in) :: sbvv
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            , intent(in) :: gsqs
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            , intent(in) :: guu
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            , intent(in) :: gvv
    real(fp)  , dimension(gdp%d%lstsc,gdp%d%nsrc)                           , intent(in) :: rintsm
    real(fp)  , dimension(gdp%d%nsrc)                                       , intent(in) :: disch
    real(fp)                                                                             :: dt
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                            , intent(in) :: dps
!
! Local variables
!
    integer                                 :: ex
    integer                                 :: ex1
    integer                                 :: isrc
    integer                                 :: j
    integer                                 :: ivol
    integer                                 :: jvol
    integer                                 :: k
    integer                                 :: k1
    integer                                 :: l
    integer                                 :: nm
    integer                                 :: nm1
    integer                                 :: nm2
    integer                                 :: nmaxddb
    real(fp)                                :: dtmor
    real(fp)                                :: q
    real(fp)                                :: vol
!
!! executable statements -------------------------------------------------------
!
    massbal     => gdp%gdmassbal%massbal
    if (.not. massbal) return
    !
    kmax           => gdp%d%kmax
    nmmax          => gdp%d%nmmax
    lsed           => gdp%d%lsed
    lsedtot        => gdp%d%lsedtot
    lstsc          => gdp%d%lstsc
    lstsci         => gdp%d%lstsci
    !
    nbalpol        => gdp%gdmassbal%nbalpol
    nneighb        => gdp%gdmassbal%nneighb
    resetfluxes    => gdp%gdmassbal%resetfluxes
    volnr          => gdp%gdmassbal%volnr
    exchnr         => gdp%gdmassbal%exchnr
    neighb         => gdp%gdmassbal%neighb
    accdps         => gdp%gdmassbal%accdps
    volumes        => gdp%gdmassbal%volumes
    mass_r1        => gdp%gdmassbal%mass_r1
    fluxes         => gdp%gdmassbal%fluxes
    fluxes_r1      => gdp%gdmassbal%fluxes_r1
    fluxes_sd      => gdp%gdmassbal%fluxes_sd
    !
    morfac         => gdp%gdmorpar%morfac
    fluxu          => gdp%gdflwpar%fluxu
    fluxv          => gdp%gdflwpar%fluxv
    ssuu           => gdp%gderosed%e_ssn
    ssvv           => gdp%gderosed%e_sst
    !
    nmaxddb = gdp%d%nmax + 2*gdp%d%ddbound
    !
    ! If volumes were determined during previous call, then the accumulation of
    ! the fluxes should start over again.
    !
    if (resetfluxes) then
       fluxes = 0.0_fp
       fluxes_r1 = 0.0_fp
       fluxes_sd = 0.0_fp
       resetfluxes = .false.
    endif
    !
    if (imode > 1) then
       !
       ! Accumulate water fluxes
       !
       do k = 1,kmax
          do nm = 1, nmmax
             ivol = volnr(nm)
             if (ivol==0) cycle
             !
             do j=1,2
                if (j==1) then
                   nm2 = nm+nmaxddb
                   q = qxk(nm,k)*dt
                else
                   nm2 = nm+1
                   q = qyk(nm,k)*dt
                endif
                !
                jvol = volnr(nm2)
                if (jvol==0) cycle
                ex = exchnr(j,nm)
                if (ex==0) cycle
                !
                if (ivol==neighb(2,ex)) q = -q
                if (q>0.0_fp) then
                   fluxes(1,ex) = fluxes(1,ex) + q
                else
                   fluxes(2,ex) = fluxes(2,ex) - q
                endif
             enddo
          enddo
       enddo
       !
       do isrc = 1,gdp%d%nsrc
          k = mnksrc(6,isrc)
          ! skip this discharge if it is outside this partition
          if (k == -1) cycle
          call n_and_m_to_nm(mnksrc(5,isrc), mnksrc(4,isrc), nm, gdp)
          !
          ! all discharge exchanges are defined positive INTO the model, they are the last "nbalpol" exchanges
          !
          ex = nneighb - nbalpol + volnr(nm)
          if (mnksrc(7,isrc)>=2) then
             call n_and_m_to_nm(mnksrc(2,isrc), mnksrc(1,isrc), nm1, gdp)
             k1 = mnksrc(3,isrc)
             ex1 = nneighb - nbalpol + volnr(nm1)
          else
             nm1 = 0
             k1  = 0
             ex1 = 0
          endif
          !
          if (disch(isrc)>0.0_fp) then
              ! outflow at outfall or single point discharge
              fluxes(1,ex) = fluxes(1,ex) + disch(isrc)*dt
              do l = 1,lstsc
                  fluxes_r1(1,ex,l) = fluxes_r1(1,ex,l) + disch(isrc)*rintsm(l,isrc)*dt
              enddo
              !
              if (ex1>0) then
                 ! equivalent inflow at intake
                 fluxes(2,ex1) = fluxes(2,ex1) + disch(isrc)*dt
                 do l = 1,lstsc
                    if (k==0) then
                       ! determine "depth averaged concentration" r1avg
                       ! fluxes_r1(2,ex1,l) = fluxes_r1(2,ex1,l) + disch(isrc)*r1avg*dt
                    else
                       fluxes_r1(2,ex1,l) = fluxes_r1(2,ex1,l) + disch(isrc)*r1(nm1,k,l)*dt
                    endif
                 enddo
              endif
          else
              ! inflow at outfall or single point discharge
              fluxes(2,ex) = fluxes(2,ex) - disch(isrc)*dt
              do l = 1,lstsc
                  if (k==0) then
                     ! determine "depth averaged concentration" r1avg
                     ! fluxes_r1(2,ex,l) = fluxes_r1(2,ex,l) - disch(isrc)*r1avg*dt
                  else
                     fluxes_r1(2,ex,l) = fluxes_r1(2,ex,l) - disch(isrc)*r1(nm,k,l)*dt
                  endif
              enddo
              !
              if (ex1>0) then
                 ! equivalent outflow at intake
                 fluxes(2,ex1) = fluxes(2,ex1) - disch(isrc)*dt
                 do l = 1,lstsc
                    fluxes_r1(2,ex1,l) = fluxes_r1(2,ex1,l) - disch(isrc)*rintsm(l,isrc)*dt
                 enddo
              endif
          endif
       enddo
       !
       ! Accumulate constituent fluxes; the fluxu array is not yet available upon
       ! the first call from INCHKR (tested by means of associated). However,
       ! that's no problem since no fluxes have to be accumulated at that time.
       !
       if (associated(fluxu)) then
          do l = 1,lstsci
             do k = 1,kmax
                do nm = 1, nmmax
                   ivol = volnr(nm)
                   if (ivol==0) cycle
                   !
                   do j = 1,2
                      if (j==1) then
                         nm2 = nm+nmaxddb
                         q = fluxu(nm,k,l)*dt
                      else
                         nm2 = nm+1
                         q = fluxv(nm,k,l)*dt
                      endif
                      !
                      jvol = volnr(nm2)
                      if (jvol==0) cycle
                      ex = exchnr(j,nm)
                      if (ex==0) cycle
                      !
                      if (ivol==neighb(2,ex)) q = -q
                      if (q>0.0_fp) then
                         fluxes_r1(1,ex,l) = fluxes_r1(1,ex,l) + q
                      else
                         fluxes_r1(2,ex,l) = fluxes_r1(2,ex,l) - q
                      endif
                   enddo
                enddo
             enddo
          enddo
          !
          ! TODO: accumulate 2D fluxes (e.g. anticreep)
          !
       endif
       !
       ! Accumulate bed load and suspended sediment fluxes.
       ! Why not accumulate bed load only since the suspended part is also
       ! included in the FLUX_R1 part? Main reason: SSUU includes SUCOR whereas
       ! the FLUX_R1 computed above doesn't include the SUCOR part. It would be
       ! inconsistent if we were to include SUCOR here in the SBUU accumulation
       ! but in SSUU on the map-file.
       !
       if (lsedtot>0) then
          dtmor  = dt*morfac
          !
          do l = 1,lsedtot
             do nm = 1, nmmax
                ivol = volnr(nm)
                if (ivol==0) cycle
                !
                do j = 1,2
                   if (j==1) then
                      nm2 = nm+nmaxddb
                      q = sbuu(nm,l)
                      if (l<=lsed) q = q+ssuu(nm,l)
                      q = q*guu(nm)*dtmor
                   else
                      nm2 = nm+1
                      q = sbvv(nm,l)
                      if (l<=lsed) q = q+ssvv(nm,l)
                      q = q*gvv(nm)*dtmor
                   endif
                   !
                   jvol = volnr(nm2)
                   if (jvol==0) cycle
                   ex = exchnr(j,nm)
                   if (ex==0) cycle
                   !
                   if (ivol==neighb(2,ex)) q = -q
                   if (q>0.0_fp) then
                      fluxes_sd(1,ex,l) = fluxes_sd(1,ex,l) + q
                   else
                      fluxes_sd(2,ex,l) = fluxes_sd(2,ex,l) - q
                   endif
                enddo
             enddo
          enddo
       endif
    endif
    !
    ! If requested: compute total volume and mass
    !
    if (imode /= 2) then
       volumes = 0.0_fp
       mass_r1 = 0.0_fp
       accdps  = 0.0_fp
       !
       ! Determine volumes
       !
       do k = 1,kmax
          do nm = 1, nmmax
             ivol = volnr(nm)
             if (ivol>0 .and. kcs(nm)==1) then
                volumes(ivol)   = volumes(ivol) + volum1(nm,k)
             endif
          enddo
       enddo
       !
       ! Determine "mass" (i.e. volume*concentration)
       !
       do l = 1, lstsci
          do k = 1,kmax
             do nm = 1, nmmax
                ivol = volnr(nm)
                if (ivol>0 .and. kcs(nm)==1) then
                   mass_r1(ivol,l) = mass_r1(ivol,l) + volum1(nm,k)*r1(nm,k,l)
                endif
             enddo
          enddo
       enddo
       !
       ! Determine cumulative depth
       !
       if (lsedtot>0) then
          do nm = 1, nmmax
             ivol = volnr(nm)
             if (ivol>0 .and. kcs(nm)==1) then
                accdps(ivol) = accdps(ivol) + gsqs(nm)*real(dps(nm),fp)
             endif
          enddo
       endif
       !
       ! Delft3D outputs cumulative fluxes, so we don't reset the fluxes after each output.
       ! This could easily be changed by activating the following line.
       !
       !resetfluxes = .true.
    endif
end subroutine updmassbal
