subroutine z_disbub(kmax    ,nsrcd     ,nsrc      ,nxbub    , &
                  & lstsci  ,lstsc     ,icx       ,icy      , &
                  & namsrc  ,mnksrc    , &
                  & kfsmin  ,kfsmax    ,gsqs      ,disinp   , &
                  & sour    ,sink      ,xcor      ,ycor     , &
                  & r1      ,disch     ,rint      ,zk       , &
                  & s1      ,dps       ,ifirst    ,gdp      )
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
! This routine determines mid-field circulation due to the bubble screens.
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
    real(fp), dimension(:)   , pointer :: hsink
    real(fp), dimension(:)   , pointer :: hsour
    real(fp), dimension(:)   , pointer :: xlbub
    real(fp), dimension(:)   , pointer :: zbubl
    real(fp), dimension(:)   , pointer :: zvelo
    logical , dimension(:)   , pointer :: flbub
    integer                  , pointer :: lundia
    character(256)           , pointer :: restid
!
! Global variables
!
    integer                                                 , intent(in)  :: icx    ! Increment in the X-dir., if ICX= NMAX
                                                                                    ! then computation proceeds in the X-
                                                                                    ! dir. If icx=1 then computation pro-
                                                                                    ! ceeds in the Y-dir. 
    integer                                                 , intent(in)  :: icy    ! Increment in the Y-dir. (see ICX)
                                                                                    ! been transformed into 1D arrays.
                                                                                    ! Due to the shift in the 2nd (M-)
                                                                                    ! index, J = -2*NMAX + 1                                                     
    integer                                                 , intent(in)  :: ifirst ! ifirst = 1 when first time in z_disbub                                                                                    
    integer                                                 , intent(in)  :: kmax
    integer                                                 , intent(in)  :: lstsc  ! Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: lstsci ! Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: nsrc   ! Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: nsrcd  ! Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: nxbub  ! Description and declaration in dimens.igs
    integer , dimension(7, nsrc)                            , intent(in)  :: mnksrc !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmax !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(nsrc)                                             :: disch  ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                               , intent(out) :: disinp ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in)  :: r1     ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lstsc, nsrc)                        , intent(out) :: rint   ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: sink   ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: sour   ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: ycor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                             , intent(in)  :: zk
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: dps
    character(20), dimension(nsrc)                          , intent(in)  :: namsrc !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer        :: ddb
    integer        :: m
    integer        :: n
    integer        :: icxy
    integer        :: ibub
    integer        :: inam
    integer        :: k
    integer        :: kbub
    integer        :: kbubx
    integer        :: kk
    integer        :: l
    integer        :: nm
    integer        :: nstbub
    real(fp)       :: dw
    real(fp)       :: ratio
    real(fp)       :: xstart
    real(fp)       :: xend
    real(fp)       :: ystart
    real(fp)       :: yend
    real(fp)       :: xlen
    real(fp)       :: ylen
    real(fp)       :: wrecmx
    real(fp)       :: recflx
    real(fp)       :: volum
    real(fp)       :: zb
    real(fp)       :: zincr
    real(fp)       :: zmax
    real(fp)       :: zkd
    real(fp)       :: hsum
    real(fp)       :: zku
    real(fp)       :: zsurf
    real(fp)       :: zloc
    logical        :: stopOnError
    character(20)  :: chulp   ! Help charatcter string
    character(256) :: error
    character(256) :: message
!
!! executable statements -------------------------------------------------------
!
    hsink   => gdp%gdbubble%hsink
    hsour   => gdp%gdbubble%hsour
    xlbub   => gdp%gdbubble%xlbub
    zbubl   => gdp%gdbubble%zbubl
    zvelo   => gdp%gdbubble%zvelo
    flbub   => gdp%gdbubble%flbub
    lundia  => gdp%gdinout%lundia
    restid  => gdp%gdrestart%restid
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    ! Stop on error when this is the first time and when not restarting from file
    !
    stopOnError = (ifirst == 1) .and. (restid == ' ')
    !
    ! Determine first bubble screen point in DISCH array
    !
    nstbub = nsrcd - nxbub + 1
    inam   = nstbub-1
    chulp  = 'dummy'
    do ibub = nstbub, nsrc, kmax
       !
       ! (nm,kk) are the coordinates of the bubble screen location
       !
       nm = (mnksrc(2, ibub) + ddb) + ((mnksrc(1, ibub) - 1) + ddb)*icxy
       kk = mnksrc(3, ibub)
       !
       ! skip this point when it is outside this partition
       !
       if (kk == -1) cycle
       !
       ! check for enough layers (3 or more) for bubblescreen
       !
       if (kfsmax(nm) - kfsmin(nm) < 3) then
          if (stopOnError) then
             write (error,'(a,i0,a)') 'Not enough layers at bubble screen ',ibub , &
                  & '. Increase the number of layers.'
             call prterr(lundia, 'P004', trim(error))
             call d3stop(1, gdp)
          else
             write (message,'(a,i0,a)') 'Not enough layers at bubble screen ',ibub , &
                  & '. Increase the number of layers.'
             call prterr(lundia, 'U190', trim(message))
             !
             ! This bubble screen is not included. Jump to next ibub.
             !
             cycle
          endif
       endif
       !   
       if (namsrc(ibub) /= chulp) then
          inam        = inam + 1
          chulp       = namsrc(ibub)
          xstart      = xcor(nm)
          xend        = xcor(nm)
          ystart      = ycor(nm)
          yend        = ycor(nm)
          xlbub(inam) = sqrt(gsqs(nm))  ! to have an inital value
       else
          xend        = xcor(nm)
          yend        = ycor(nm)
          xlen        = abs(xend - xstart)
          ylen        = abs(yend - ystart)
          xlbub(inam) = sqrt(xlen**2 + ylen**2)
       endif
    enddo
    inam   = nstbub-1
    chulp  = 'dummy'
    do ibub = nstbub, nsrc, kmax
       !
       ! (nm,kk) are the coordinates of the bubble screen location
       !
       nm = (mnksrc(2, ibub) + ddb) + ((mnksrc(1, ibub) - 1) + ddb)*icxy
       kk = mnksrc(3, ibub)
       if (namsrc(ibub) /= chulp) then
          inam  = inam + 1
          chulp = namsrc(ibub)
       endif
       !
       ! skip this point when it is outside this partition
       !
       if (kk == -1) cycle
       !
       ! Determine layer number of injection point
       !
       zb   = zbubl(ibub)
       kbub = 0
       do k = kfsmin(nm),kfsmax(nm)
          if (k == kfsmin(nm)) then
             zkd = -dps(nm)
          else
             zkd = zk(k - 1)
          endif
          zku = zk(k)
          if (zku > zb .and. zkd <= zb ) then
             kbub = k
          endif
       enddo
       if (kbub == 0 ) then
          kbub = kfsmin(nm)
          !
          ! adaptation if ZBUBL is below the bottom:
          !
          zbubl(ibub) = -dps(nm)
          zb          = -dps(nm)
          write (message,'(a,i0,a)') 'Bubble screen ',ibub ,' is moved from below bottom to bottom.'
          call prterr(lundia, 'U190', trim(message))
       endif
       !
       ! Copy discharge from input 
       !
       if (kfsmax(nm) - kbub < 3) then
          if (stopOnError) then
             write (error,'(2a,i0,2a)') 'Not enough layers above injection point', &
                  & ' at bubble screen ',ibub , '. Increase the number of layers.'
             call prterr(lundia, 'P004', trim(error))
             call d3stop(1, gdp)
          else
             write (message,'(2a,i0,2a)') 'Not enough layers above injection point', &
                  & ' at bubble screen ',ibub , '. Increase the number of layers.'
             call prterr(lundia, 'U190', trim(message))
             !
             ! Lower kbub one layer
             !
             kbub = kbub - 1
          endif
       endif
       !
       ! Copy discharge from input 
       !
       if (flbub(inam)) then
          disinp(ibub) = disch(ibub)
       endif
       !
       ! Compute horizontal extent of the bubble screen 
       !
       recflx = disinp(ibub)
       wrecmx = -recflx / gsqs(nm)
       !
       ! Determine layer number of maximum downward vertical 
       ! velocity WRECMX (at 20% of air injection point) and
       ! compute the vertical velocity ZVELO
       !
       ! and determine corresponding discharge
       !
       do k = 1,kmax
          zvelo(k)            = 0.0_fp
          disch(ibub+k-1)     = 0.0_fp
          do l = 1, lstsci
             rint(l,ibub+k-1) = 0.0_fp
          enddo
       enddo
       !
       ! determine 1/5th from the free surface to the air inlet location
       !
       zsurf = s1(nm)
       zloc  = zsurf - 0.2_fp*(zsurf-zb)
       kbubx = 0
       do k = kfsmin(nm),kfsmax(nm)
          if (k == kfsmin(nm)) then
             zkd = -dps(nm)
          else
             zkd = zk(k - 1)
          endif
          zku = zk(k)
          if (zku>zloc .and. zkd<=zloc) then
             kbubx = k
          endif
       enddo
       !
       ! check for layers (1 or more) above kbubx
       !
       if (kfsmax(nm) - kbubx < 1) then
          if (stopOnError) then
             write (error,'(a,i0)') 'Top layer too thick at bubblescreen ',ibub
             call prterr(lundia, 'P004', trim(error))
             call d3stop(1, gdp)
          else
             write (message,'(a,i0,a)') 'Top layer too thick at bubblescreen ',ibub, &
                  & '. Adapting bubblescreen schematisation locally.'
             call prterr(lundia, 'U190', trim(message))
             !
             ! Lower kbubx one layer
             !
             kbubx = kbubx - 1
          endif
       endif       
       !
       do k = kfsmax(nm), kbubx, -1
          zincr    = zk(kfsmax(nm)) - zk(k)
          zmax     = zk(kfsmax(nm)) - zk(kbubx)
          ratio    = zincr / zmax
          zvelo(k) = ratio * wrecmx
       enddo 
       do k = kbubx-1, kbub, -1
          zincr    = zk(kbubx) - zk(k)
          zmax     = zk(kbubx) - zk(kbub)
          ratio    = 1.0_fp - zincr/zmax
          zvelo(k) = ratio * wrecmx
       enddo
       !
       ! Determine sinks from point of maximum vertical velocity
       ! to the air injection point
       !
       !------------------------------------------------------------------
       ! HSINK is the integral of recirculated heat in [W/m^2] induced
       ! by the bubble screen but NOT by the recirculation for cooling.
       !
       ! The latter is excluded because WSIN is defined here for the
       ! recirculation by the bubble-screen only.
       !
       ! Note that HEATU computes temperature in Kelvin.
       ! Here HSINK is computed by the first-order upwind flux
       ! at the level of maximum downward recirculation-velocity:
       !-----------------------------------------------------------------
       !
       hsink = 0.0_fp
       hsour = 0.0_fp
       do k = kbubx, kbub+1, -1
          dw              = zvelo(k) - zvelo(k-1)
          disch(ibub+k-1) = dw * gsqs(nm)
          do l = 1,lstsci
             sink(nm,k,l) = sink(nm,k,l)  - disch(ibub+k-1) 
             hsink(l)     = hsink(l) - disch(ibub+k-1)*r1(nm,k,l)
          enddo
       enddo 
       !
       !------------------------------------------------------------------
       ! Return HSINK, dragged upward by the bubble screen is released
       ! over the top part of the bubble-induced recirculation.
       ! This is HSINK/KBLMX added to the SOUR-term due to the
       ! heat-influx by cooling:
       !------------------------------------------------------------------
       !
       do k = kfsmax(nm), kbubx+1, -1
          dw              = zvelo(k) - zvelo(k-1)
          disch(ibub+k-1) = dw * gsqs(nm)
       enddo
       !
       ! determine RINT for all substances
       !
       volum = 0.0_fp
       do k = kfsmax(nm), kbubx+1, -1
          volum = volum + disch(ibub+k-1)
       enddo
       do l = 1,lstsci
          hsour(l) = hsink(l) / max(1.0e-10_fp,volum)
       enddo
       do k = kfsmax(nm), kbubx+1, -1
          do l = 1,lstsci
             sour(nm,k,l)      = sour(nm,k,l) + disch(ibub+k-1)*hsour(l)
             rint(l, ibub+k-1) = hsour(l)
          enddo
       enddo
       !
       ! if (flbub(inam)) then
       !    m = int (nm / icxy ) + 1 - ddb
       !    n = nm - icxy * (m - 1 + ddb )- ddb
       !    do k=kmax,1,-1
       !       write (lundia,'(a,5i4,a,4i4,a,5e12.3)') 'z_disbub: (',ibub,ibub+k-1,m,n,k, &
       !           & ') (',kfsmin(nm),kfsmax(nm),kbubx,kbub,')',zvelo(k), &
       !           & disch(ibub+k-1),sink(nm,k,1),sour(nm,k,1),rint(1,ibub+k-1)
       !    enddo
       ! endif
       hsum = 0.0_fp
       do k = kfsmax(nm),kfsmin(nm), -1
          hsum = hsum + disch(ibub+k-1)
       enddo
       if (abs(hsum) > 0.0001_fp ) then
          call nm_to_n_and_m(nm, n, m, gdp)
          write (message,'(a,i0,a,e12.4,a,i0,a,i0,a)') 'Vertical sum of bubble_discharges ', &
                 & ibub, ' is not zero (',hsum, ') at (m,n)=(', n, ',',m,')'
          call prterr(lundia, 'U190', trim(message))
       endif
    enddo
end subroutine z_disbub
