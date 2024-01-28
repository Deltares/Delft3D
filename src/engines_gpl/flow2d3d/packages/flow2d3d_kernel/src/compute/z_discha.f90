subroutine z_discha(kmax      ,nsrc      ,nbub      ,lstsci    ,lstsc     ,j         , &
                  & nmmaxj    ,icx       ,icy       ,namsrc    ,mnksrc    , &
                  & kfs       ,kcs       ,kfsmin    ,kfsmx0    ,sour      , &
                  & sink      ,dps       ,s0        ,dzs0      ,r0        , &
                  & disch     ,rint      ,rintsm    ,relthkin  ,relthkout ,bubble    , &
                  & gdp       )
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
!    Function: The discharges are added to the right hand side of
!              the continuity equation.
!              NB. Only for normal dischages; for bubble screens
!              this is done in Z_DISBUB 
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: ltem
    integer , pointer :: lundia
    real(fp), pointer :: maxTOutlet
    logical , dimension(:) , pointer :: flbcktemp
!
! Global variables
!
    integer                                                 , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                                    !!  then computation proceeds in the X-
                                                                                    !!  dir. If icx=1 then computation pro-
                                                                                    !!  ceeds in the Y-dir.
    integer                                                 , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                               :: j      !!  Begin pointer for arrays which have
                                                                                    !!  been transformed into 1D arrays.
                                                                                    !!  Due to the shift in the 2nd (M-)
                                                                                    !!  index, J = -2*NMAX + 1
    integer                                                 , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: lstsc  !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: nbub   !  Description and declaration in esm_alloc_int.f90
    integer                                                               :: nmmaxj !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(7, nsrc)                             , intent(in)  :: mnksrc !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    logical                                                 , intent(in)  :: bubble !  Description and declaration in procs.igs
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzs0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in)  :: r0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: sink   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: sour   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                             :: relthkin
    real(fp), dimension(kmax)                                             :: relthkout
    real(fp), dimension(lstsc, nsrc)                        , intent(in)  :: rint   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lstsc, nsrc)                        , intent(out) :: rintsm !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                               , intent(in)  :: disch  !  Description and declaration in esm_alloc_real.f90
    character(20), dimension(nsrc)                                        :: namsrc !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer,external :: newlun
    integer          :: dfil
    integer          :: ddb
    integer          :: icxy
    integer          :: isrc
    integer          :: k
    integer          :: k2
    integer          :: kkout
    integer          :: kkin
    integer          :: lcon
    integer          :: nmout
    integer          :: nmin
    integer          :: iin
    integer          :: iout
    integer          :: kk
    integer          :: offset
    real(fp)         :: concin
    real(fp)         :: sumrelthk
    character(20)    :: chulp       ! Help character string
    character(200)   :: filename
!
!! executable statements -------------------------------------------------------
!
    ltem       => gdp%d%ltem
    lundia     => gdp%gdinout%lundia
    maxTOutlet => gdp%gddischarge%maxTOutlet
    flbcktemp  => gdp%gdheat%flbcktemp
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    ! Loop over lstsc and nsrc
    !
    if (bubble) then
       offset = - nbub
    else
       offset = 0
    endif
    do isrc = 1, nsrc + offset
       !
       ! (nmout,kkout) are the coordinates of the outfall location
       ! whereas (nmin,kkin) correspond to the intake
       !
       nmout = (mnksrc(5, isrc) + ddb) + ((mnksrc(4, isrc) - 1) + ddb)*icxy
       kkout = mnksrc(6, isrc)
       !
       ! skip this point when it is outside this partition
       !
       if (kkout == -1) cycle
       !
       ! calculate relative thickness of the active layers in outfall location
       !
       do lcon = 1, lstsc
          if (kfs(nmout) == 1) then
             if (kkout == 0) then
                sumrelthk = 0.0_fp
                do k = kfsmin(nmout), kfsmx0(nmout)
                   relthkout(k) = dzs0(nmout, k)
                   sumrelthk    = sumrelthk + relthkout(k)
                enddo
                relthkout = relthkout / sumrelthk
             else
                relthkout(kkout) = 1.0_fp
             endif
          endif
          !
          ! determine concentration at outfall (only for discharges of type >0)
          !
          if (mnksrc(7, isrc) <= 1) then
             !
             ! discharge without intake (regular, momentum, walking)
             ! concentration at outfall is prescribed in rint
             !
             concin = rint(lcon, isrc)
             !
             ! For background temperatures the current temperature is used
             !
             if ( flbcktemp(lcon) ) then
                nmin = (mnksrc(2, isrc) + ddb) + ((mnksrc(1, isrc) - 1) + ddb)*icxy
                kkin = mnksrc(3, isrc)
                if (kkin == 0) then
                   concin = 0.0_fp
                   do k2 = kfsmin(nmin), kfsmx0(nmin)
                      concin = concin + r0(nmin, k2, lcon)*relthkin(k2)
                   enddo
                else
                   if (kkin > kfsmx0(nmin)) kkin = max(kfsmx0(nmin), kfsmin(nmin))
                   if (kkin < kfsmin(nmin)) kkin = kfsmin(nmin)
                   concin = r0(nmin, kkin, lcon)
                endif
             endif
          else
             !
             ! discharge with intake (culverts, power station)
             !
             nmin = (mnksrc(2, isrc) + ddb) + ((mnksrc(1, isrc) - 1) + ddb)*icxy
             kkin = mnksrc(3, isrc)
             !
             ! calculate relative thickness of the active layers in intake location
             !
             if (kfs(nmin) == 1) then
                if (kkin == 0) then
                   sumrelthk = 0.0_fp
                   do k = kfsmin(nmin), kfsmx0(nmin)
                      relthkin(k) = dzs0(nmin, k)
                      sumrelthk   = sumrelthk + relthkin(k)
                   enddo
                   relthkin = relthkin / sumrelthk
                else
                   relthkin(kkin) = 1.0_fp
                endif
             endif
             !
             ! get concentration at intake
             !
             if (kfs(nmin) == 1) then
                if (kkin == 0) then
                   concin = 0.0_fp
                   do k2 = kfsmin(nmin), kfsmx0(nmin)
                      concin = concin + r0(nmin, k2, lcon)*relthkin(k2)
                   enddo
                else
                   if (kkin > kfsmx0(nmin)) kkin = max(kfsmx0(nmin), kfsmin(nmin))
                   if (kkin < kfsmin(nmin)) kkin = kfsmin(nmin)
                   concin = r0(nmin, kkin, lcon)
                endif
                if (mnksrc(7,isrc)==6 .and. lcon==ltem) then
                   !
                   ! Q-type power station and this is constituent 'temperature'
                   ! The outlet temperature is prescribed
                   !
                   concin = min(concin+rint(lcon,isrc), maxTOutlet)
                elseif (mnksrc(7,isrc)==2 .or. mnksrc(7,isrc)==6) then
                   !
                   ! for powerstations: add prescribed concentrations
                   ! They are interpreted as changes to the concentrations
                   ! Compare with regular/momentum/walking discharges,
                   ! where rint prescribes the absolute concentrations
                   !
                   concin = concin + rint(lcon, isrc)
                   if (concin < 0.0_fp) then
                      !
                      ! If negative concentration then withdrawal not allowed
                      !
                      concin = 0.0_fp
                      call prterr(lundia, 'S100', namsrc(isrc))
                   endif
                endif
             endif
          endif
          !
          ! source/sink addition at outfall
          !
          rintsm(lcon, isrc) = concin
          if (disch(isrc) > 0.0_fp) then
             !
             ! positive discharge; addition to sour
             ! Allowed in temporary but not in permanently dry cells,
             ! If KKOUT .ne. 0 redistribute discharge accross vertical cells
             !
             if (kcs(nmout) == 1) then
                if (kkout == 0) then
                   do k = kfsmin(nmout), kfsmx0(nmout)
                      sour(nmout, k, lcon) = sour(nmout, k, lcon) + disch(isrc)*concin*relthkout(k)
                   enddo
                else
                   if (kkout > kfsmx0(nmout)) kkout = max(kfsmx0(nmout), kfsmin(nmout))
                   if (kkout < kfsmin(nmout)) kkout = kfsmin(nmout)
                   sour(nmout, kkout, lcon) = sour(nmout, kkout, lcon) + disch(isrc)*concin
                endif
             else
                !
                ! Discharge at permanent dry cell not allowed
                !
                call prterr(lundia, 'S101', namsrc(isrc))
             endif
          !
          ! Negative discharge; substract from sinks
          ! If KKOUT .ne. 0 redistribute intake accross vertical cells
          !
          elseif (disch(isrc) < 0.0_fp ) then
             if (kfs(nmout) == 1) then
                if (kkout == 0) then
                   do k = kfsmin(nmout), kfsmx0(nmout)
                      sink(nmout, k, lcon) = sink(nmout, k, lcon) - disch(isrc)*relthkout(k)
                   enddo
                else
                   if (kkout > kfsmx0(nmout)) kkout = kfsmx0(nmout)
                   if (kkout < kfsmin(nmout)) kkout = kfsmin(nmout)
                   sink(nmout, kkout, lcon) = sink(nmout, kkout, lcon) - disch(isrc)
                endif
             else
                !
                ! If Q < 0 withdrawal from dry cell not allowed
                !
                call prterr(lundia, 'S102', namsrc(isrc))
             endif
          endif
          !
          ! sink addition at intake (for power stations and culverts)
          ! disch(isrc) may be negative or positive
          ! If KKIN .ne. 0 redistribute intake accross vertical cells
          !
          if (mnksrc(7, isrc) >= 2) then
             if (kfs(nmin) > 0) then
                if (kkin == 0) then
                   do k = kfsmin(nmin), kfsmx0(nmin)
                      sink(nmin, k, lcon) = sink(nmin, k, lcon) + disch(isrc)*relthkin(k)
                   enddo
                else
                   if (kkin > kfsmx0(nmin)) kkin = max(kfsmx0(nmin), kfsmin(nmin))
                   if (kkin < kfsmin(nmin)) kkin = kfsmin(nmin)
                   sink(nmin, kkin, lcon) = sink(nmin, kkin, lcon) + disch(isrc)
                endif
             else
                !
                ! If Q < 0 withdrawal from dry cell not allowed
                !
                call prterr(lundia, 'S103', namsrc(isrc))
             endif
          endif
       enddo
    enddo
end subroutine z_discha
