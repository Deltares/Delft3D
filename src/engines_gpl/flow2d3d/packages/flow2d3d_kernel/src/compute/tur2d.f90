subroutine tur2d(dischy    ,j         ,nmmaxj    ,nmmax     ,nmax      , &
               & mmax      ,kmax      ,icx       ,icy       ,kfs       , &
               & kfu       ,kfv       ,kcs       ,kfd       ,dp        , &
               & dps       ,s1        ,umean     ,vmean     ,rtu2d0    , &
               & rtu2d1    ,rtubnd    ,thick     ,guu       ,gvv       , &
               & guv       ,gvu       ,vicww     ,dicww     ,vicuv     , &
               & vnu2d     ,vnu3d     ,cfurou    ,cfvrou    ,dddksi    , &
               & dddeta    ,z0urou    ,z0vrou    ,windu     ,windv     , &
               & tkepro    ,tkedis    ,bbk       ,ddk       ,bdx       , &
               & bux       ,bdy       ,buy       ,wrka1     ,wrka2     , &
               & wrka3     ,dep2      ,umea      ,vmea      ,gdp       )
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
!    Function: Computes transport in the u and v -direction of
!              the turbulent kinetic energy and enstrophy and
!              determines the 2D eddy viscosity at depth points.
!              The time integration is fully implicit.
!              Timesplitting is used. In the first stage
!              the horizontal advection terms
!              are integrated implicitly.
!              A Gauss Seidel iterative solution method is used.
!              In the second stage the diffusive transport terms
!              are integrated implicitly in combination with
!              the source and sink terms.
!              Sinks are treated implicitly and sources explicit-
!              ly. The systems of equations are decoupled in
!              the ksi- and eta-horizontal direction. The
!              resulting tridiagonal systems of equations are
!              solved by a double sweep.
!              The production of turbulence is due to horizontal
!              shear and wall friction (slip) on vertical
!              walls. The wall friction term may be switched on
!              or off with the parameter IROV.
!
! Method used: Reference : Theoretical foundation of eddy
!              diffusivity (R.E. Uittenbogaard).
!              Barotropic turbulence model, q2-eta2-model.
!              q2 and eta2 defined at depth points.
!              - Horizontal Advection in U-direction :
!                implicit, first order upwind
!              - Horizontal Advection in V-direction :
!                implicit, first order upwind
!              - Horizontal Diffusion : implicit
!              - Sources are integrated explicitly
!              - Sinks are integrated implicitly
!              - Production term contains horizontal gradients
!              - Dissipation term is linearised to improve
!                stability
!              - Gauss Seidel iteration in the horizontal
!     Comment: At the open boundaries the TKE and enstrophy.
!              have to be prescribed by the user
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                , pointer :: lundia
    real(fp)               , pointer :: hdt
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: vonkar
    real(fp)               , pointer :: vicmol
    real(fp)               , pointer :: ceta
    real(fp)               , pointer :: ck
    real(fp)               , pointer :: sigq2e
!
! Global variables
!
    integer                                           , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                        , intent(in) :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                         :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                        , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                     :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                     :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                     :: nmmax  !  Description and declaration in dimens.igs
    integer                                                     :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfd    !!  Mask array for the depth points (time dependent) =0 corner of dry point =1 corner of wet points
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(fp)                                                    :: vicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub,3)      , intent(in)  :: cfurou !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub,3)      , intent(in)  :: cfvrou !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: dddeta !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: dddksi !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dep2   !!  Internal work array, for square of total water depth at (N,M)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: dp     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: vmean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: vnu2d  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: vnu3d  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: windu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: windv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: z0urou !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: z0vrou !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: bbk    !!  Internal work array
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: ddk    !!  Internal work array, diagonal space at (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: dicww  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: vicww  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: wrka1  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: wrka2  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: wrka3  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 2)                   :: rtu2d0 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 2)                   :: rtu2d1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 2)     , intent(in)  :: rtubnd !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bdx    !!  Internal work array, implicit coupling of concentration in (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bdy
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bux    !!  Internal work array, implicit coupling of concentration in (N,M,K) with layer concentration in (N,M+1,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: buy
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: tkedis !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: tkepro !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: umea
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: vmea
    real(fp), dimension(kmax)                         , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    character(8)                                  , intent(in)  :: dischy !  Description and declaration in tricom.igs
!
! Local variables
!
    integer :: icxy
    integer :: ij
    integer :: iter
    integer :: itmax
    integer :: k
    integer :: kcd
    integer :: kd
    integer :: l
    integer :: mmaxddb
    integer :: n
    integer :: ndm
    integer :: nm
    integer :: nmaxddb
    integer :: nmd
    integer :: nmu
    integer :: num
    integer :: numu
    real(fp):: chezy
    real(fp):: d2udx2
    real(fp):: d2udy2
    real(fp):: d2vdx2
    real(fp):: d2vdy2
    real(fp):: dddx
    real(fp):: dddy
    real(fp):: dep
    real(fp):: dx
    real(fp):: dxd
    real(fp):: dxu
    real(fp):: dy
    real(fp):: dyd
    real(fp):: dyu
    real(fp):: ermax
    real(fp):: fac
    real(fp):: omegaz
    real(fp):: prod
    real(fp):: s
    real(fp):: timest
    real(fp):: udo
    real(fp):: umod
    real(fp):: uup
    real(fp):: uuu
    real(fp):: vdo
    real(fp):: vicd
    real(fp):: vicu
    real(fp):: vup
    real(fp):: vvv
    integer :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    ceta        => gdp%gdturcoe%ceta
    ck          => gdp%gdturcoe%ck
    sigq2e      => gdp%gdturcoe%sigq2e
    ag          => gdp%gdphysco%ag
    vonkar      => gdp%gdphysco%vonkar
    vicmol      => gdp%gdphysco%vicmol
    hdt         => gdp%gdnumeco%hdt
    lundia      => gdp%gdinout%lundia
    !
    nmaxddb = nmax + 2*gdp%d%ddbound
    mmaxddb = mmax + 2*gdp%d%ddbound
    nm_pos  = 1
    !
    icxy = max(icx, icy)
    if (dischy=='cn') then
       timest = hdt
    else
       timest = 2*hdt
    endif
    !
    ! set identification array in depth points
    !
    do nm = 1, nmmax
       num  = nm + icy
       numu = nm + icy + icx
       nmu  = nm + icx
       !
       !                 +  - +
       !        cell:
       !                 |  o |
       !
       !                 +  - +
       !
       kfd(nm) = kfu(nm)*kfu(num)*kfv(nmu)*kfv(nm)
    enddo
    call dfexchg ( kfd, 1, 1, dfint, nm_pos, gdp )
    !
    !  mean horizontal velocities at depth point and
    !  depth-averaged vertical viscosity and
    !  total waterdepth at depth point
    !
    if (kmax>1) then
       do nm = 1, nmmax
          if (kfd(nm)==1) then
             nmu         = nm + icx
             num         = nm + icy
             numu        = nm + icy + icx
             vnu3d(nm)   = 0.
             umea(nm, 1) = 0.25*(umean(nm) + umean(num))
             vmea(nm, 1) = 0.25*(vmean(nm) + vmean(nmu))
             do k = 1, kmax
                kd = k - 1
                vnu3d(nm) = vnu3d(nm) + thick(k)                                &
                          & *.125*(vicww(nm, k) + vicww(nmu, k) + vicww(num, k) &
                          & + vicww(numu, k) + vicww(nm, kd) + vicww(nmu, kd)   &
                          & + vicww(num, kd) + vicww(numu, kd))
             enddo
          endif
       enddo
    else
       !
       ! logarithmic velocity profile without wind
       !
       do nm = 1, nmmax
          if (kfd(nm)==1) then
             nmu       = nm + icx
             num       = nm + icy
             numu      = nm + icy + icx
             uuu       = .5*(umean(nm) + umean(num))
             vvv       = .5*(vmean(nm) + vmean(nmu))
             s         = 0.25*(s1(nm) + s1(num) + s1(nmu) + s1(numu))
             dep       = max(dp(nm) + s, 0.01_fp)
             umod      = sqrt(uuu*uuu + vvv*vvv)
             chezy     = .25*(cfurou(nm,1) + cfurou(num,1) + cfvrou(nm,1) + cfvrou(nmu,1))
             fac       = ag/(chezy*chezy*dep)
             vnu3d(nm) = vonkar*sqrt(fac*umod*umod)*dep/6.
          endif
       enddo
    endif
    do nm = 1, nmmax
       if (kfd(nm)/=1) then
          nmu         = nm + icx
          num         = nm + icy
          umea(nm, 1) = 0.25*(umean(nm) + umean(num))
          vmea(nm, 1) = 0.25*(vmean(nm) + vmean(nmu))
       endif
    enddo
    !
    !  Loop for transport q2 and eta2
    !
    do l = 1, 2
       do nm = 1, nmmax
          nmu  = nm + icx
          num  = nm + icy
          numu = nm + icy + icx
          if (kfd(nm)==1) then
             s          = 0.25*(s1(nm) + s1(num) + s1(nmu) + s1(numu))
             dep        = max(dp(nm) + s, 0.01_fp)
             dep2(nm)   = 1.
             if (l==2) dep2(nm) = dep*dep
             bux(nm, 1) = 0.0
             bdx(nm, 1) = 0.0
             buy(nm, 1) = 0.0
             bdy(nm, 1) = 0.0
             bbk(nm, 1) = 1.0/(timest*dep2(nm))
             ddk(nm, 1) = rtu2d0(nm, l)/(timest*dep2(nm))
          endif
       enddo
       !
       ! advection in horizontal U-direction
       !
       do nm = 1, nmmax
          nmd = nm - icx
          nmu = nm + icx
          if (kfd(nm)==1) then
             uuu        = umea(nm, 1)
             udo        = 0.5*(uuu + abs(uuu))*kfd(nmd)/gvv(nm)
             uup        = 0.5*(uuu - abs(uuu))*kfd(nmu)/gvv(nmu)
             bux(nm, 1) = bux(nm, 1) + uup*dep2(nmu)
             bbk(nm, 1) = bbk(nm, 1) - uup*dep2(nm) + udo*dep2(nm)
             bdx(nm, 1) = bdx(nm, 1) - udo*dep2(nmd)
          endif
       enddo
       !
       ! advection in horizontal V-direction
       !
       do nm = 1, nmmax
          ndm = nm - icy
          num = nm + icy
          if (kfd(nm)==1) then
             vvv        = vmea(nm, 1)
             vdo        = 0.5*(vvv + abs(vvv))*kfd(ndm)/guu(nm)
             vup        = 0.5*(vvv - abs(vvv))*kfd(num)/guu(num)
             buy(nm, 1) = buy(nm, 1) + vup*dep2(num)
             bbk(nm, 1) = bbk(nm, 1) - vup*dep2(nm) + vdo*dep2(nm)
             bdy(nm, 1) = bdy(nm, 1) - vdo*dep2(ndm)
          endif
       enddo
       !
       ! Boundary conditions for advection part
       ! For TKE
       !
       if (l==1) then
          do nm = 1, nmmax
             nmu  = nm + icx
             num  = nm + icy
             numu = nm + icx + icy
             kcd  = kcs(nm)*kcs(num)*kcs(nmu)*kcs(numu)
             if (kcd==4) then
                !
                ! Open boundary only. At closed boundary all values are zero
                !
                bux(nm, 1) = 0.0
                bdx(nm, 1) = 0.0
                buy(nm, 1) = 0.0
                bdy(nm, 1) = 0.0
                bbk(nm, 1) = 1.0
                ddk(nm, 1) = rtubnd(nm, l)
             endif
          enddo
       else
          !
          ! Enstrophy
          !
          do nm = 1, nmmax
             nmu  = nm + icx
             num  = nm + icy
             nmd  = nm - icx
             ndm  = nm - icy
             numu = nm + icx + icy
             kcd  = kcs(nm)*kcs(num)*kcs(nmu)*kcs(numu)
             if (kfd(nm)==0) then
                !
                ! Closed boundary
                !
                if (kfd(num)==1) then
                   bux(nm, 1) =  0.0
                   bdx(nm, 1) =  0.0
                   buy(nm, 1) =  1.0
                   bdy(nm, 1) =  0.0
                   bbk(nm, 1) = -1.0
                   ddk(nm, 1) =  0.0
                elseif (kfd(nmu)==1) then
                   bux(nm, 1) =  1.0
                   bdx(nm, 1) =  0.0
                   buy(nm, 1) =  0.0
                   bdy(nm, 1) =  0.0
                   bbk(nm, 1) = -1.0
                   ddk(nm, 1) =  0.0
                elseif (kfd(nmd)==1) then
                   bux(nm, 1) =  0.0
                   bdx(nm, 1) =  1.0
                   buy(nm, 1) =  0.0
                   bdy(nm, 1) =  0.0
                   bbk(nm, 1) = -1.0
                   ddk(nm, 1) =  0.0
                elseif (kfd(ndm)==1) then
                   bux(nm, 1) =  0.0
                   bdx(nm, 1) =  0.0
                   buy(nm, 1) =  0.0
                   bdy(nm, 1) =  1.0
                   bbk(nm, 1) = -1.0
                   ddk(nm, 1) =  0.0
                else
                endif
             elseif (kcd==4) then
                !
                ! Open Boundary
                !
                bux(nm, 1) = 0.0
                bdx(nm, 1) = 0.0
                buy(nm, 1) = 0.0
                bdy(nm, 1) = 0.0
                bbk(nm, 1) = 1.0
                ddk(nm, 1) = rtubnd(nm, l)
             else
             endif
          enddo
       endif
       !
       ! iteration loop
       !
       do nm = 1, nmmax
          rtu2d1(nm, l) = rtu2d0(nm, l)
       enddo
       !
       ! in case of parallel runs, block Jacobi is applied at coupling boundaries
       ! and consequently, rate of convergence of Gauss Seidel iteration will be
       ! deteriorated somewhat en hence, more iterations are needed
       !
       itmax = 2
       if ( nproc > 1 ) itmax = 5
       !
       do iter = 1, itmax
          !
          ! iterative solution method using Gauss Seidel
          ! in horizontal direction
          !
          do ij = 2, nmaxddb + mmaxddb
             nm  = ij*icx - icxy       + (max(1, ij - mmaxddb) - 1)*(icy - icx)
             nmd = ij*icx - icxy - icx + (max(1, ij - mmaxddb) - 1)*(icy - icx)
             ndm = ij*icx - icxy - icy + (max(1, ij - mmaxddb) - 1)*(icy - icx)
             !
             ! VDIR NODEP
             !
             do n = max(1, ij - mmaxddb), min(nmaxddb, ij - 1)
                nm  = nm + (icy - icx)
                nmd = nmd + (icy - icx)
                ndm = ndm + (icy - icx)
                if (kfd(nm)==1) then
                   if (umea(nm, 1)>=0.0 .and. vmea(nm, 1)>=0.0) then
                      rtu2d1(nm, l) = (ddk(nm, 1) - bdx(nm, 1)*rtu2d1(nmd, l)   &
                                    & - bdy(nm, 1)*rtu2d1(ndm, l))/bbk(nm, 1)
                   endif
                endif
             enddo
          enddo
          do ij = 1 - mmaxddb, nmaxddb - 1
             nm  = -ij*icx - icxy       + (max(1, 1 + ij) - 1)*(icy + icx)
             nmu = -ij*icx - icxy + icx + (max(1, 1 + ij) - 1)*(icy + icx)
             ndm = -ij*icx - icxy - icy + (max(1, 1 + ij) - 1)*(icy + icx)
             !
             !VDIR NODEP
             !
             do n = max(1, 1 + ij), min(nmaxddb, mmaxddb + ij)
                nm  = nm + (icy + icx)
                nmu = nmu + (icy + icx)
                ndm = ndm + (icy + icx)
                if (kfd(nm)==1) then
                   if (umea(nm, 1)<0.0 .and. vmea(nm, 1)>=0.0) then
                      rtu2d1(nm, l) = (ddk(nm, 1) - bux(nm, 1)*rtu2d1(nmu, l)   &
                                    & - bdy(nm, 1)*rtu2d1(ndm, l))/bbk(nm, 1)
                   endif
                endif
             enddo
          enddo
          do ij = nmaxddb - 1, 1 - mmaxddb, -1
             nm  = -ij*icx - icxy       + (min(nmaxddb, mmaxddb + ij) + 1)*(icy + icx)
             nmd = -ij*icx - icxy - icx + (min(nmaxddb, mmaxddb + ij) + 1)*(icy + icx)
             num = -ij*icx - icxy + icy + (min(nmaxddb, mmaxddb + ij) + 1)*(icy + icx)
             !
             !VDIR NODEP
             !
             do n = min(nmaxddb, mmaxddb + ij), max(1, 1 + ij), -1
                nm  = nm  - (icy + icx)
                nmd = nmd - (icy + icx)
                num = num - (icy + icx)
                if (kfs(nm)==1) then
                   if (umea(nm, 1)>=0.0 .and. vmea(nm, 1)<0.0) then
                      rtu2d1(nm, l) = (ddk(nm, 1) - bdx(nm, 1)*rtu2d1(nmd, l)   &
                                    & - buy(nm, 1)*rtu2d1(num, l))/bbk(nm, 1)
                   endif
                endif
             enddo
          enddo
          do ij = nmaxddb + mmaxddb, 2, -1
             nm  = ij*icx - icxy       + (min(nmaxddb, ij - 1) + 1)*(icy - icx)
             nmu = ij*icx - icxy + icx + (min(nmaxddb, ij - 1) + 1)*(icy - icx)
             num = ij*icx - icxy + icy + (min(nmaxddb, ij - 1) + 1)*(icy - icx)
             !
             !VDIR NODEP
             !
             do n = min(nmaxddb, ij - 1), max(1, ij - mmaxddb), -1
                nm  = nm  - (icy - icx)
                nmu = nmu - (icy - icx)
                num = num - (icy - icx)
                if (kfs(nm)==1) then
                   if (umea(nm, 1)<0.0 .and. vmea(nm, 1)<0.0) then
                      rtu2d1(nm, l) = (ddk(nm, 1) - bux(nm, 1)*rtu2d1(nmu, l)   &
                                    & - buy(nm, 1)*rtu2d1(num, l))/bbk(nm, 1)
                   endif
                endif
             enddo
          enddo
          !
          ! exchange rtu2d1 with neighbours for parallel runs
          !
          call dfexchg ( rtu2d1(:, l), 1, 1, dfloat, nm_pos, gdp )
       enddo
       !
       ! Second stage: production-dissipation and diffusion
       !
       do nm = 1, nmmax
          if (kfd(nm)==1) then
             bbk(nm, 1) = 1.0/(timest*dep2(nm))
             ddk(nm, 1) = rtu2d1(nm, l)/(timest*dep2(nm))
          endif
       enddo
       !
       if (l==1) then
          !
          ! production term (only horizontal gradients )
          ! in water level points
          !
          do nm = 1, nmmax
             if (kfs(nm)==1) then
                nmd        = nm - icx
                ndm        = nm - icy
                dx         = 0.5*(gvv(nm) + gvv(ndm))
                dy         = 0.5*(guu(nm) + guu(nmd))
                bdx(nm, 1) = ((umean(nm) - umean(nmd))/dx)                      &
                           & **2 + ((vmean(nm) - vmean(ndm))/dy)**2
             else
                bdx(nm, 1) = 0.0
             endif
          enddo
          !
          ! in depth points
          !
          do nm = 1, nmmax
             if (kfd(nm)==1) then
                nmu        = nm + icx
                num        = nm + icy
                dx         = 0.5*(gvv(nm) + gvv(nmu))
                dy         = 0.5*(guu(nm) + guu(num))
                bux(nm, 1) = ((umean(num) - umean(nm))/dy + (vmean(nmu) - vmean(&
                           & nm))/dx)**2
             else
                bux(nm, 1) = 0.0
             endif
          enddo
          !
          ! Production by horizontal shear
          !
          do nm = 1, nmmax
             if (kfd(nm)==1) then
                num        = nm + icy
                nmu        = nm + icx
                numu       = nm + icx + icy
                ddk(nm, 1) = ddk(nm, 1) + vnu2d(nm)                             &
                           & *(bux(nm, 1) + 0.50*((bdx(nm, 1) + bdx(num, 1)     &
                           & + bdx(nmu, 1) + bdx(numu, 1))))
             endif
          enddo
       !
       ! Production enstrophy
       !
       else
          do nm = 1, nmmax
             if (kfd(nm)==1) then
                nmu = nm + icx
                nmd = nm - icx
                ndm = nm - icy
                num = nm + icy
                d2udx2 = (kfd(nmu)*(umea(nmu, 1) - umea(nm, 1))/gvv(nmu)        &
                       & - kfd(nmd)*(umea(nm, 1) - umea(nmd, 1))/gvv(nm))       &
                       & /(.5*(gvv(nm) + gvv(nmu)))
                d2udy2 = (kfd(num)*(umea(num, 1) - umea(nm, 1))/guu(num)        &
                       & - kfd(ndm)*(umea(nm, 1) - umea(ndm, 1))/guu(nm))       &
                       & /(.5*(guu(nm) + guu(num)))
                d2vdx2 = (kfd(nmu)*(vmea(nmu, 1) - vmea(nm, 1))/gvv(nmu)        &
                       & - kfd(nmd)*(vmea(nm, 1) - vmea(nmd, 1))/gvv(nm))       &
                       & /(.5*(gvv(nm) + gvv(nmu)))
                d2vdy2 = (kfd(num)*(vmea(num, 1) - vmea(nm, 1))/guu(num)        &
                       & - kfd(ndm)*(vmea(nm, 1) - vmea(ndm, 1))/guu(nm))       &
                       & /(.5*(guu(nm) + guu(num)))
                omegaz = (vmean(nmu) - vmean(nm))/(.5*(gvv(nm) + gvv(nmu)))     &
                       & - (umean(num) - umean(nm))/(.5*(guu(nm) + guu(num)))
                dddx   = (dddksi(nmu) + dddksi(nm))/(gvv(nm) + gvv(nmu))
                dddy   = (dddeta(num) + dddeta(nm))/(guu(nm) + guu(num))
                prod   = 2*vnu2d(nm)/dep2(nm)                                     &
                       & *((d2udx2 + d2udy2)**2 + (d2vdy2 + d2vdx2)               &
                       & **2 - omegaz/sqrt(dep2(nm))                              &
                       & *((d2udx2 + d2udy2)*dddx + (d2vdy2 + d2vdx2)*dddy))
                if (prod<0) then
                   prod = prod/rtu2d0(nm, 2)
                   bbk(nm, 1) = bbk(nm, 1) - prod
                else
                   ddk(nm, 1) = ddk(nm, 1) + prod
                endif
             endif
          enddo
       endif
       if (l==1) then
          !
          ! external and internal dissipation TKE
          !
          do nm = 1, nmmax
             if (kfd(nm)==1) then
                nmu        = nm + icx
                num        = nm + icy
                numu       = nm + icy + icx
                uuu        = .5*(umean(nm) + umean(num))
                vvv        = .5*(vmean(nm) + vmean(nmu))
                s          = 0.25*(s1(nm) + s1(num) + s1(nmu) + s1(numu))
                dep        = max(dp(nm) + s, 0.01_fp)
                umod       = sqrt(uuu*uuu + vvv*vvv)
                chezy      = .25*(cfurou(nm,1) + cfurou(num,1) + cfvrou(nm,1) + cfvrou(nmu,1))
                fac        = ag/(chezy*chezy*dep)
                bbk(nm, 1) = bbk(nm, 1) + fac*(3.*umod + sqrt(rtu2d0(nm, 1)))
                bbk(nm, 1) = bbk(nm, 1) + (vnu3d(nm) + vicmol)*rtu2d0(nm, 2)    &
                           & /rtu2d0(nm, 1)
             endif
          enddo
       elseif (l==2) then
          !
          ! external and internal dissipation enstrophy
          !
          do nm = 1, nmmax
             if (kfd(nm)==1) then
                nmu        = nm + icx
                num        = nm + icy
                numu       = nm + icy + icx
                uuu        = .5*(umean(nm) + umean(num))
                vvv        = .5*(vmean(nm) + vmean(nmu))
                s          = 0.25*(s1(nm) + s1(num) + s1(nmu) + s1(numu))
                dep        = max(dp(nm) + s, 0.01_fp)
                umod       = sqrt(uuu*uuu + vvv*vvv)
                chezy      = .25*(cfurou(nm,1) + cfurou(num,1) + cfvrou(nm,1) + cfvrou(nmu,1))
                bbk(nm, 1) = bbk(nm, 1) + 2*ag*umod/(chezy*chezy*dep**3)
                bbk(nm, 1) = bbk(nm, 1) + 2*ceta*vnu3d(nm)/dep*rtu2d0(nm, 2)    &
                           & /rtu2d0(nm, 1)
             endif
          enddo
       else
       endif
       !
       ! Matrix coefficients horizontal diffusion
       ! Attention: vnu2d/vnu3d defined at depth points
       !
       do nm = 1, nmmax
          bdx(nm, 1) = 0.
          bux(nm, 1) = 0.
          bdy(nm, 1) = 0.
          buy(nm, 1) = 0.
          nmd        = nm - icx
          nmu        = nm + icx
          ndm        = nm - icy
          num        = nm + icy
          if (kfd(nm)==1) then
             dx  = 0.5*(gvv(nm) + gvv(nmu))
             dxd = gvv(nm)
             dxu = gvv(nmu)
             dy  = 0.5*(guu(nm) + guu(num))
             dyd = guu(nm)
             dyu = guu(num)
             if (l==1) then
                vicu = .5*(vnu2d(nm) + vnu2d(nmu) + vnu3d(nm) + vnu3d(nmu))     &
                     & + vicmol
                vicd = .5*(vnu2d(nm) + vnu2d(nmd) + vnu3d(nm) + vnu3d(nmd))     &
                     & + vicmol
             elseif (l==2) then
                vicu = .5*(vnu2d(nm)/dep2(nm) + vnu2d(nmu)/dep2(nmu))           &
                     & + vnu3d(nm)/dep2(nm)
                vicd = .5*(vnu2d(nm)/dep2(nm) + vnu2d(nmd)/dep2(nmd))           &
                     & + vnu3d(nm)/dep2(nm)
             else
             endif
             bdx(nm, 1) = -vicd*kfd(nmd)/(sigq2e*dxd*dx)
             bux(nm, 1) = -vicu*kfd(nmu)/(sigq2e*dxu*dx)
             bbk(nm, 1) = bbk(nm, 1) - bdx(nm, 1) - bux(nm, 1)
             if (l==1) then
                vicu = .5*(vnu2d(nm) + vnu2d(ndm) + vnu3d(nm) + vnu3d(ndm))     &
                     & + vicmol
                vicd = .5*(vnu2d(nm) + vnu2d(num) + vnu3d(nm) + vnu3d(num))     &
                     & + vicmol
             elseif (l==2) then
                vicu = .5*(vnu2d(nm)/dep2(nm) + vnu2d(num)/dep2(num))           &
                     & + vnu3d(nm)/dep2(nm)
                vicd = .5*(vnu2d(nm)/dep2(nm) + vnu2d(ndm)/dep2(ndm))           &
                     & + vnu3d(nm)/dep2(nm)
             else
             endif
             bdy(nm, 1) = -vicd*kfd(ndm)/(sigq2e*dyd*dy)
             buy(nm, 1) = -vicu*kfd(num)/(sigq2e*dyu*dy)
             bbk(nm, 1) = bbk(nm, 1) - bdy(nm, 1) - buy(nm, 1)
          endif
       enddo
       !
       ! Boundary conditions
       ! For TKE
       !
       if (l==1) then
          do nm = 1, nmmax
             nmu  = nm + icx
             num  = nm + icy
             numu = nm + icx + icy
             kcd  = kcs(nm)*kcs(num)*kcs(nmu)*kcs(numu)
             if (kcd==4) then
                !
                ! Open boundary only. At closed boundary all values are zero
                !
                bux(nm, 1) = 0.0
                bdx(nm, 1) = 0.0
                buy(nm, 1) = 0.0
                bdy(nm, 1) = 0.0
                bbk(nm, 1) = 1.0
                ddk(nm, 1) = rtu2d1(nm, l)
             endif
          enddo
       else
          !
          ! Enstrophy
          !
          do nm = 1, nmmax
             nmu  = nm + icx
             num  = nm + icy
             nmd  = nm - icx
             ndm  = nm - icy
             numu = nm + icx + icy
             kcd  = kcs(nm)*kcs(num)*kcs(nmu)*kcs(numu)
             if (kfd(nm)==0) then
                !
                ! Closed boundary
                !
                if (kfd(num)==1) then
                   bux(nm, 1) =  0.0
                   bdx(nm, 1) =  0.0
                   buy(nm, 1) =  1.0
                   bdy(nm, 1) =  0.0
                   bbk(nm, 1) = -1.0
                   ddk(nm, 1) =  0.0
                elseif (kfd(nmu)==1) then
                   bux(nm, 1) =  1.0
                   bdx(nm, 1) =  0.0
                   buy(nm, 1) =  0.0
                   bdy(nm, 1) =  0.0
                   bbk(nm, 1) = -1.0
                   ddk(nm, 1) =  0.0
                elseif (kfd(nmd)==1) then
                   bux(nm, 1) =  0.0
                   bdx(nm, 1) =  1.0
                   buy(nm, 1) =  0.0
                   bdy(nm, 1) =  0.0
                   bbk(nm, 1) = -1.0
                   ddk(nm, 1) =  0.0
                elseif (kfd(ndm)==1) then
                   bux(nm, 1) =  0.0
                   bdx(nm, 1) =  0.0
                   buy(nm, 1) =  0.0
                   bdy(nm, 1) =  1.0
                   bbk(nm, 1) = -1.0
                   ddk(nm, 1) =  0.0
                else
                endif
             elseif (kcd==4) then
                !
                ! Open Boundary
                !
                bux(nm, 1) = 0.0
                bdx(nm, 1) = 0.0
                buy(nm, 1) = 0.0
                bdy(nm, 1) = 0.0
                bbk(nm, 1) = 1.0
                ddk(nm, 1) = rtu2d1(nm, l)
             else
             endif
          enddo
       endif
       !
       ! Solver for penta-diagonal system of equations
       !
       ! # of iterations added
       !
       itmax = 50
       call itadi2(lundia    ,rtu2d1(j, l)         ,kfd       ,mmax      ,nmax      , &
                 & j         ,nmmaxj    ,nmmax     ,bdx       ,bbk       , &
                 & bux       ,ddk       ,bdy       ,buy       ,iter      , &
                 & itmax     ,ermax     ,wrka1     ,wrka2     ,wrka3     , &
                 & gdp       )
    enddo
    !
    ! Determine 2D eddy viscosity
    !
    do nm = 1, nmmax
       if (kfd(nm)==1) then
          rtu2d0(nm, 1) = rtu2d1(nm, 1)
          rtu2d0(nm, 2) = rtu2d1(nm, 2)
          vnu2d(nm)     = ck*rtu2d1(nm, 1)/sqrt(rtu2d1(nm, 2))
       endif
    enddo
end subroutine tur2d
