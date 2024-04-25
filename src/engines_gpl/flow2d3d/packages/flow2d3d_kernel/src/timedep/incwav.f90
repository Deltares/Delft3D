subroutine incwav(timsec    ,j         ,nmmaxj    ,norow     ,icx       , &
                & icy       ,irocol    ,zetaif    ,ctif      ,stif      , &
                & zetabf    ,ctbf      ,stbf      ,zbmnf     ,wenf      , &
                & wenfm     ,wenlm     ,zetail    ,ctil      ,stil      , &
                & zetabl    ,ctbl      ,stbl      ,zbmnl     ,wenl      , &
                & cgdghf    ,cgdghl    ,zmeanf    ,umeanf    ,zmeanl    , &
                & umeanl    ,urf       ,dpu       ,s0        ,umean     , &
                & xcor      ,ycor      ,hu        ,ncmax     ,ampbc     , &
                & ombc      ,phibc     ,thetbc    ,first     ,gdp       )
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
! NONE
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
    integer                , pointer :: iro
    integer                , pointer :: ncomp
    integer                , pointer :: nsplit
    real(fp)               , pointer :: timtap
    real(fp)               , pointer :: depbnd
    real(fp)               , pointer :: gamdis
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
!
! Global variables
!
    integer                                :: icx
    integer                                :: icy
    integer                                :: j
    integer                                :: ncmax
    integer                                :: nmmaxj !  Description and declaration in dimens.igs
    integer                                :: norow  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, norow)           :: irocol !  Description and declaration in esm_alloc_int.f90
    real(fp)                               :: timsec !  Description and declaration in inttim.igs
    real(fp)                               :: urf
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: ycor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(ncmax), intent (in)    :: ampbc  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(ncmax)                 :: ombc   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(ncmax)                 :: phibc  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(ncmax)                 :: thetbc !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: cgdghf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: cgdghl !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: ctbf   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: ctbl   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: ctif   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: ctil   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: stbf   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: stbl   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: stif   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: stil   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: umeanf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: umeanl !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: wenf   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: wenfm  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: wenl   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: wenlm  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: zetaif !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: zetail !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: zbmnf  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: zbmnl  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: zetabf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: zetabl !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: zmeanf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: zmeanl !  Description and declaration in esm_alloc_real.f90
    logical                                    :: first
!
! Local variables
!
    integer                :: bnd
    integer                :: ddb
    integer                :: i
    integer                :: ib
    integer                :: ic
    integer                :: icx_in
    integer                :: icxy
    integer                :: jj
    integer                :: m
    integer                :: n
    integer                :: njj
    integer                :: nm
    integer                :: nmbu
    integer                :: nmbs
    logical                :: nolock
    real(fp)               :: amptap
    real(fp)               :: arg
    real(fp)               :: cg
    real(fp)               :: cgdgh
    real(fp)               :: ctb
    real(fp)               :: cti
    real(fp)               :: ctj
    real(fp)               :: ctn
    real(fp)               :: depth
    real(fp)               :: encg
    real(fp)               :: epsf
    real(fp)               :: reskx
    real(fp)               :: resky
    real(fp)               :: resom
    real(fp)               :: ripzc
    real(fp)               :: ripzct
    real(fp)               :: ripzcx
    real(fp)               :: ripzcy
    real(fp)               :: rkc
    real(fp)               :: rlength
    real(fp)               :: rrpuix
    real(fp)               :: rrpuiy
    real(fp)               :: rrpzc
    real(fp)               :: rrpzct
    real(fp)               :: rrpzcx
    real(fp)               :: rrpzcy
    real(fp)               :: sqrtk
    real(fp)               :: ss0
    real(fp)               :: stb
    real(fp)               :: sti
    real(fp)               :: stj
    real(fp)               :: stn
    real(fp)               :: su0
    real(fp)               :: taper
    real(fp), external     :: tapf
    real(fp)               :: th
    real(fp)               :: toten
    real(fp)               :: um
    real(fp)               :: wen
    real(fp)               :: wenm
    real(fp), external     :: wnr
    real(fp)               :: xb
    real(fp)               :: yb
    real(fp)               :: zb
    real(fp)               :: zbmn
    real(fp)               :: zc2
    real(fp)               :: zetab
    real(fp)               :: zetai
    real(fp)               :: zm
    real(fp), dimension(ncmax) :: co
    real(fp), dimension(ncmax) :: rin
    real(fp), dimension(ncmax) :: rk
    real(fp), dimension(ncmax) :: si
!
!! executable statements -------------------------------------------------------
!
    ncomp    => gdp%gdbcdat%ncomp
    nsplit   => gdp%gdbcdat%nsplit
    timtap   => gdp%gdbcdat%timtap
    depbnd   => gdp%gdbcdat%depbnd
    gamdis   => gdp%gdbetaro%gamdis
    rhow     => gdp%gdphysco%rhow
    ag       => gdp%gdphysco%ag
    iro      => gdp%gdphysco%iro
    !
    if (first) then
       ctbf  = 1.0_fp
       ctbl  = 1.0_fp
       stbf  = 0.0_fp
       stbl  = 0.0_fp
    endif
    icxy = max(icx, icy)
    ddb  = gdp%d%ddbound
    !
    ! Make taper with taper length of timtap seconds
    !
    arg = timsec/timtap
    taper = tapf(arg)
    !
    do ic = 1, norow
       !
       ! For first (left) and second (right) boundary
       !
       do bnd = 1, 2
          if (bnd == 1) then
             !
             ! First (left) boundary
             !
             n      = irocol(1, ic)
             m      = irocol(2, ic) - 1
             nmbu   = (n + ddb)*icy + (m + ddb)*icx - icxy
             nmbs   = nmbu
             !
             ib     = irocol(4, ic)
             ctb    = ctbf(ic)
             stb    = stbf(ic)
             zm     = zmeanf(ic)
             um     = umeanf(ic)
             wenm   = wenfm(ic)
             zbmn   = zbmnf(ic)
             icx_in = icx
          else
             !
             ! Second (right) boundary
             !
             n      = irocol(1, ic)
             m      = irocol(3, ic)
             nmbu   = (n + ddb)*icy + (m + ddb)*icx - icxy
             nmbs   = nmbu + icx
             !
             ib     = irocol(5, ic)
             ctb    = ctbl(ic)
             stb    = stbl(ic)
             zm     = zmeanl(ic)
             um     = umeanl(ic)
             wenm   = wenlm(ic)
             zbmn   = zbmnl(ic)
             icx_in = -icx
          endif
          !
          if (ib==6 .or. ib==2) then
             !
             ! Determine the direction of the grid normal pointing inward
             ! xb,yb = coordinates of velocity point at the open boundary
             ! ctn,stn = distance in x,y direction to first velocity point inside domain
             !
             xb      = 0.5_fp*(xcor(nmbu) + xcor(nmbu - icy))
             yb      = 0.5_fp*(ycor(nmbu) + ycor(nmbu - icy))
             ctn     = 0.5_fp*(xcor(nmbu + icx_in) + xcor(nmbu + icx_in - icy)) - xb
             stn     = 0.5_fp*(ycor(nmbu + icx_in) + ycor(nmbu + icx_in - icy)) - yb
             !
             ! Convert distances into cosine and sine
             !
             rlength = sqrt(ctn**2 + stn**2)
             ctn     = ctn/rlength
             stn     = stn/rlength
             !
             ! Do not use the actual, varying depth, but a constant reference depth.
             ! Otherwise the phases and lengths calculated for the waves will vary too much.
             !
             depth = depbnd
             !
             ! Tidal contributions are ignored here, they can be included by using
             ! boundary type 2 (waterlevel boundary) combined with wavcmp.eq.true.
             ! In that case Riemann will be used with a mean waterlevel (zm) and
             ! mean velocity (vm) = 0 prescribed using tidal information.
             !
             ! Low frequency Neumann boundary conditions
             ! * take average of water levels and velocities of the first 10 grid points
             !   inside the domain (along grid line perpendicular to the boundary). For
             !   the velocity this includes the velocity at the open boundary.
             ! * underrelaxation in time
             !
             njj = 10
             ss0 = 0.0_fp
             su0 = 0.0_fp
             do jj = 1, njj
                ss0 = ss0 + s0(nmbs + jj*icx_in)
                su0 = su0 + umean(nmbu + (jj-1)*icx_in)
             enddo
             ss0 = ss0/real(njj, fp)
             su0 = su0/real(njj, fp)
             !
             ! Instantaneous adaptation of zm and um to initial condition ! Ad-hoc implementation
             !
             if (zm==0.0_fp) zm=ss0
             if (um==0.0_fp) um=su0
             zm = (1.0_fp - urf)*zm + urf*ss0
             um = (1.0_fp - urf)*um + urf*su0
             !
             ! waves
             !
             if (timsec<10.0_fp) then
                wenm = 0.0_fp
             endif
             do i = 1, ncomp
                rk(i)   = wnr(ombc(i), depth, gdp)
                arg     = (cos(thetbc(i))*xb + sin(thetbc(i))*yb)*rk(i) &
                        & - ombc(i)*timsec + phibc(i)
                co(i)   = cos(arg)
                si(i)   = sin(arg)
                !
                ! rin = 1 if wave component enters domain
                ! rin = 0 if wave component leaves domain
                !
                !rin(i)  = 0.5_fp*(sign(1.0_fp, &
                !                       ctn*cos(thetbc(i)) + stn*sin(thetbc(i))) + 1.0_fp)
                rin(i) = 0.0_fp
                if ((ctn*cos(thetbc(i)) + stn*sin(thetbc(i)))>1.0E-5_fp) rin(i)=1.0_fp
             enddo
             !
             ! Determination of long free waves
             !
             zetai  = 0.0_fp
             rrpuix = 0.0_fp
             rrpuiy = 0.0_fp
             do jj = 1, nsplit
                amptap = rin(jj)*ampbc(jj)*taper
                zetai  = zetai + amptap*co(jj)
                rrpuix = rrpuix + ombc(jj)/rk(jj)*amptap/depth*co(jj)*cos(thetbc(jj))
                rrpuiy = rrpuiy + ombc(jj)/rk(jj)*amptap/depth*co(jj)*sin(thetbc(jj))
             enddo
             if (abs(zetai)<=1.0e-6 .or. abs(rrpuix)<1.0e-6 .or. abs(rrpuiy)<1.0e-6) then
                cti = 1.0_fp
                sti = 0.0_fp
             else
                cti = (ctn*rrpuix + stn*rrpuiy)/sqrt(rrpuix**2 + rrpuiy**2)
                sti = (ctn*rrpuiy - stn*rrpuix)/sqrt(rrpuix**2 + rrpuiy**2)
                !
                cti = cti*zetai/abs(zetai)
                sti = sti*zetai/abs(zetai)
             endif
             !
             ! RESULT:
             ! free incoming waves with elevation zetai and direction
             ! cos(thetai)=cti and sin(thetai)=sti relative to inward normal.
             !
             ! Determination of incoming short waves (carrier waves)
             !
             rrpzc    = 0.0_fp
             ripzc    = 0.0_fp
             rrpzct   = 0.0_fp
             ripzct   = 0.0_fp
             rrpzcx   = 0.0_fp
             ripzcx   = 0.0_fp
             rrpzcy   = 0.0_fp
             ripzcy   = 0.0_fp
             nolock   = .true.
             do jj = nsplit + 1, ncomp
                if (rin(jj)>0.5_fp) then
                   nolock = .false.
                   amptap = taper*ampbc(jj)
                   ctj    = cos(thetbc(jj))
                   stj    = sin(thetbc(jj))
                   rrpzc  = rrpzc + ampbc(jj)*co(jj)
                   ripzc  = ripzc + ampbc(jj)*si(jj)
                   rrpzct = rrpzct + ampbc(jj)*ombc(jj)*si(jj)
                   ripzct = ripzct - ampbc(jj)*ombc(jj)*co(jj)
                   rrpzcx = rrpzcx - ampbc(jj)*rk(jj)*ctj*si(jj)
                   ripzcx = ripzcx + ampbc(jj)*rk(jj)*ctj*co(jj)
                   rrpzcy = rrpzcy - ampbc(jj)*rk(jj)*stj*si(jj)
                   ripzcy = ripzcy + ampbc(jj)*rk(jj)*stj*co(jj)
                endif
             enddo
             zc2       = rrpzc**2 + ripzc**2
             if (nolock .or. abs(zc2)<1.0e-6) then
                reskx      = 0.0_fp
                resky      = 0.0_fp
                resom      = 0.0_fp
                wen        = 0.0_fp
                ctb        = 1.0_fp
                stb        = 0.0_fp
                encg       = 0.0_fp
                zbmn       = 0.0_fp
                zetab      = 0.0_fp
                cgdgh      = 0.0_fp
             else
                !
                ! There are incoming locked waves:
                !
                wen       = min(0.5_fp*rhow*ag*zc2*taper, &
                                0.125_fp*rhow*ag*(0.8_fp*gamdis*depth)**2)
                wenm      = (1.0_fp - urf)*wenm + urf*wen
                !
                ! reskx,resky and resom with under-relaxation determined from
                ! the phase of the total signal:
                !
                ! resulting omega,kx and ky found from derivatives of the phase of the total signal
                ! reskx     = urf * (-ripzc * rrpzcx + rrpzc * ripzcx)/zc2+
                !             (1.0_fp - urf) * reskx
                ! resky     = urf * (-ripzc * rrpzcy + rrpzc * ripzcy)/zc2+
                !             (1.0_fp - urf) * resky
                ! resom     = urf * ( ripzc * rrpzct - rrpzc * ripzct)/zc2+
                !             (1.0_fp - urf) * resom
                !
                ! resulting omega, kx and ky found from energy averages
                resom  = 0.0_fp
                reskx  = 0.0_fp
                resky  = 0.0_fp
                toten  = 0.0_fp
                do jj = nsplit + 1, ncomp
                   if (rin(jj)>0.5_fp) then
                      resom  = resom + ampbc(jj)**2*ombc(jj)
                      reskx  = reskx + ampbc(jj)**2*rk(jj)*cos(thetbc(jj))
                      resky  = resky + ampbc(jj)**2*rk(jj)*sin(thetbc(jj))
                      toten  = toten + ampbc(jj)**2
                   endif
                enddo
                resom  = resom/toten
                reskx  = reskx/toten
                resky  = resky/toten
                !
                ! RESULT:
                ! incoming short waves (carrier waves) with energy value wen
                ! and (reskx,resky) the components of average wavenumber-vector
                ! and resom as average frequency.
                !
                ! Determination of bound long waves (based on hor. bot. approx.)
                !
                rkc        = wnr(resom, depth, gdp)
                th         = tanh(rkc*depth)
                cg         = 0.5_fp*ag/resom*(th + rkc*depth*(1.0_fp - th*th))
                encg       = cg/resom*rkc
                cgdgh      = cg/sqrt(ag*depth)
                zb         = (2.0_fp*encg - 0.5_fp)/(rhow*(cg*cg - ag*depth))                  &
                           & *(wen - wenm)
                !
                ! Slow relaxation of zbmn to zb.
                !
                epsf = 1.0E-3_fp
                if (timsec<4.0_fp) then
                   zbmn = 0.0_fp
                else
                   zbmn = (1.0_fp - epsf)*zbmn + epsf*zb
                endif
                !
                zetab = zb - zbmn
                sqrtk = sqrt(reskx**2 + resky**2)
                !
                if (sqrtk>1.0E-4) then
                   ctb = urf*(ctn*reskx + stn*resky)/sqrtk + (1.0_fp - urf)*ctb
                   stb = urf*(ctn*resky - stn*reskx)/sqrtk + (1.0_fp - urf)*stb
                else
                   ctb = 1.0_fp
                   stb = 0.0_fp
                endif
                !
                ! RESULT:
                ! Incoming locked wave with elevation zetab and direction ctb
                ! is cosine with inward pointing normal at boundary.
                !
             endif
             !
             if (bnd == 1) then
                !
                ! First (left) boundary
                !
                zmeanf(ic) = zm
                umeanf(ic) = um
                zetaif(ic) = zetai
                ctif(ic)   = cti
                stif(ic)   = sti
                zetabf(ic) = zetab
                ctbf(ic)   = ctb
                stbf(ic)   = stb
                zbmnf(ic)  = zbmn
                cgdghf(ic) = cgdgh
                wenf(ic)   = wen
                wenfm(ic)  = wenm
             else
                !
                ! Second (right) boundary
                !
                zmeanl(ic) = zm
                umeanl(ic) = um
                zetail(ic) = zetai
                ctil(ic)   = cti
                stil(ic)   = sti
                zetabl(ic) = zetab
                ctbl(ic)   = ctb
                stbl(ic)   = stb
                zbmnl(ic)  = zbmn
                cgdghl(ic) = cgdgh
                wenl(ic)   = wen
                wenlm(ic)  = wenm
             endif
          endif
       enddo
    enddo
end subroutine incwav
