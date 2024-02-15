subroutine discha(kmax      ,nsrc      ,nbub      ,lstsci    ,lstsc     ,j         , &
                & nmmaxj    ,icx       ,icy       ,namsrc    ,mnksrc    , &
                & kfs       ,kcs       ,sour      ,sink      ,volum1    , &
                & volum0    ,r0        ,disch     ,rint      ,rintsm    ,thick     , &
                & bubble    ,gdp       )
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
! The discharges are added to the sink and source terms of
! the continuity equation.
!              NB. Only for normal dischages; for bubble screens
!              this is done in DISBUB 
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
    integer                                                 , intent(in)  :: icx    ! Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir. 
    integer                                                 , intent(in)  :: icy    ! Increment in the Y-dir. (see ICX)
    integer                                                               :: j      ! Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1                                                     
    integer                                                 , intent(in)  :: kmax
    integer,  dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfs    ! Description and declaration in esm_alloc_int.f90
    integer,  dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs    ! Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: lstsc  ! Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: lstsci ! Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: nbub   !  Description and declaration in esm_alloc_int.f90
    integer                                                               :: nmmaxj ! Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: nsrc   ! Description and declaration in dimens.igs
    integer,  dimension(7, nsrc)                            , intent(in)  :: mnksrc ! Description and declaration in r-i-ch.igs
    logical                                                 , intent(in)  :: bubble !  Description and declaration in procs.igs
    real(fp), dimension(nsrc)                               , intent(in)  :: disch  ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in)  :: r0     ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lstsc, nsrc)                        , intent(in)  :: rint   ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lstsc, nsrc)                        , intent(out) :: rintsm ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: sink   ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: sour   ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                               , intent(in)  :: thick  ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: volum0 ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: volum1 ! Description and declaration in esm_alloc_real.f90
    character(20), dimension(nsrc)                                        :: namsrc
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
    integer          :: offset
    real(fp)         :: concin
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
       do lcon = 1, lstsc
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
             ! For background temperatures the current temperature is used (Temprature at the outfall location)
             !
             if ( flbcktemp(lcon) ) then
                if (kkout == 0) then
                   concin = 0.0_fp
                   do k2 = 1, kmax
                      concin = concin + r0(nmout,k2, lcon)*thick(k2)
                   enddo
                else
                  concin = r0(nmout, kkout, lcon)
                endif
             endif
          else
             !
             ! discharge with intake (culverts, power station)
             !
             nmin = (mnksrc(2, isrc) + ddb) + ((mnksrc(1, isrc) - 1) + ddb)*icxy
             kkin = mnksrc(3, isrc)
             !
             ! get concentration at intake
             !
             if (disch(isrc) >= 0.0_fp ) then
                if (kkin==0) then
                   concin = 0.0_fp
                   do k2 = 1, kmax
                      concin = concin + r0(nmin, k2, lcon)*thick(k2)
                   enddo
                else
                   concin = r0(nmin, kkin, lcon)
                endif
             else
                if (kkout == 0) then
                   concin = 0.0_fp
                   do k2 = 1, kmax
                      concin = concin + r0(nmout, k2, lcon)*thick(k2)
                   enddo
                else
                   concin = r0(nmout, kkout, lcon)
                endif
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
          !
          ! source/sink addition at outfall
          !
          rintsm(lcon, isrc) = concin
          if (kcs(nmout)/=-1) then
             if (disch(isrc) > 0.0_fp) then
                !
                ! positive discharge; addition to sour
                ! Allowed in dry cells,
                ! as long as all cell volumes > 0.
                ! testing for volume(k=1) is enough
                !
                if (volum0(nmout, 1) > 0.0_fp) then
                   if (kkout==0) then
                      do k = 1, kmax
                         sour(nmout, k, lcon) = sour(nmout, k, lcon) + disch(isrc)             &
                                        & *concin*thick(k)/volum0(nmout, k)
                      enddo
                   else
                      sour(nmout, kkout, lcon) = sour(nmout, kkout, lcon) + disch(isrc)              &
                                      & *concin/volum0(nmout, kkout)
                   endif
                else
                   call prterr(lundia, 'S101', namsrc(isrc))
                endif
                !
             elseif ( disch(isrc) < 0.0_fp) then
                if (kfs(nmout) > 0) then
                   if (kkout == 0) then
                      do k = 1, kmax
                         sink(nmout, k, lcon) = sink(nmout, k, lcon)                           &
                                        & - disch(isrc)*thick(k)/volum1(nmout, k)
                      enddo
                   else
                      sink(nmout, kkout, lcon) = sink(nmout, kkout, lcon)                            &
                                      & - disch(isrc)/volum1(nmout, kkout)
                   endif
                else
                   !
                   ! negative discharge; addition to sink
                   ! NOT allowed in dry cells
                   !
                   call prterr(lundia, 'S102', namsrc(isrc))
                endif
             endif
          endif
          !
          ! sink addition at intake (for power stations and culverts)
          ! disch(isrc) may be negative or positive
          !
          if (mnksrc(7, isrc) >= 2) then
             !
             ! source/sink addition at intake
             !
             if (disch(isrc) < 0.0_fp) then
                !
                ! negative discharge; addition to sour
                ! Allowed in dry cells,
                ! as long as all cell volumes > 0.
                ! testing for volume(k=1) is enough
                !
                if (volum0(nmin, 1) > 0.0_fp) then
                   if (kkin == 0) then
                      do k = 1, kmax
                         sour(nmin, k, lcon) = sour(nmin, k, lcon) - disch(isrc) &
                                          & *concin*thick(k)/volum0(nmin, k)
                      enddo
                   else
                      sour(nmin, kkin, lcon) = sour(nmin, kkin, lcon) - disch(isrc)  &
                                        & *concin/volum0(nmin, kkin)
                   endif
                else
                   call prterr(lundia, 'S101', namsrc(isrc))
                endif
             elseif (disch(isrc) > 0.0_fp) then
                if (kfs(nmin) > 0) then
                   if (kkin == 0) then
                      do k = 1, kmax
                         sink(nmin, k, lcon) = sink(nmin, k, lcon) + &
                                          & disch(isrc)*thick(k) / volum1(nmin, k)
                      enddo
                   else
                      sink(nmin, kkin, lcon) = sink(nmin, kkin, lcon) + &
                                          & disch(isrc) / volum1(nmin, kkin)
                   endif
                else
                   !
                   ! negative discharge in dry cells NOT possible
                   !
                   call prterr(lundia, 'S103', namsrc(isrc))
                endif
             endif
          endif
       enddo
    enddo
end subroutine discha
