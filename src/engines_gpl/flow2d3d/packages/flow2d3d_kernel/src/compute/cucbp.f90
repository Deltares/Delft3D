subroutine cucbp(kmax      ,norow     ,icx       , &
               & icy       ,zmodel    ,irocol    ,kcs       ,kfu       , &
               & kfumn0    ,kfumx0    ,s0        ,u0        ,dpu       , &
               & hu        ,umean     ,tetau     ,guu       ,gvu       , &
               & dzu0      ,thick     ,circ2d    ,circ3d    ,a         , &
               & b         ,c         ,d         ,aa        ,bb        , &
               & cc        ,dd        ,aak       ,bbk       ,cck       , &
               & ddk       ,crbc      ,wavcmp    ,gdp       )
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
!    Function: Coefficients at boundary points MF, MFU, ML, and
!              MLU are set. The coefficients are stored in the
!              arrays AAK, BBK, CCK, and DDK (velocity points).
!              For the depth-averaged equations the coefficients
!              are stored in AA, BB, CC, DD (for the velocity
!              points) and A, B, C and D (for the water level
!              points).
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
    integer                , pointer :: lundia
    real(fp)               , pointer :: hdt
    real(fp)               , pointer :: ag
    integer                , pointer :: iro
    real(fp)               , pointer :: dzmin
!
! Global variables
!
    integer                                         , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                            !!  then computation proceeds in the X-
                                                                            !!  dir. If icx=1 then computation pro-
                                                                            !!  ceeds in the Y-dir.
    integer                                         , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                         , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in)  :: norow  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, norow)                    , intent(in)  :: irocol !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)  :: kfumx0 !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)  :: kfumn0 !  Description and declaration in esm_alloc_int.f90
    logical                                         , intent(in)  :: wavcmp
    logical                                         , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(fp), dimension(12, norow)                  , intent(in)  :: crbc   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(4, norow)                   , intent(in)  :: circ2d !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: a      !!  Internal work array, tridiagonal
                                                                            !!  matrix water levels lower diagonal
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: aa     !!  Internal work array, coupling mean
                                                                            !!  velocity with water level point in
                                                                            !!  (N,M,K) left (down)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: b      !!  Internal work array, tridiagonal ma-
                                                                            !!  trix water levels main diagonal
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: bb     !!  Internal work array, coefficient mean
                                                                            !!  velocity
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: c      !!  Internal work array, tridiagonal
                                                                            !!  matrix water levels upper diagonal
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: cc     !!  Internal work array, coupling mean
                                                                            !!  velocity with water level point
                                                                            !!  right (upper)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: d      !!  Internal work array, Right Hand side
                                                                            !!  of the Continuity equation
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: dd     !!  Internal work array, Right hand side
                                                                            !!  of the momentum eq. at (N,M)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: tetau  !!  Factor for upwind approach S0
                                                                            !!  can be 0.0, 0.5 or 1.0 depending
                                                                            !!  on value of HU, DCO, KSPU and UMEAN
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: aak    !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: bbk    !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: cck    !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: ddk    !!  Internal work array, diagonal space
                                                                            !!  at (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzu0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                       , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax, 2, norow)             , intent(in)  :: circ3d !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer       :: ddb
    integer       :: ibf
    integer       :: ibl
    integer       :: ic
    integer       :: icxy
    integer       :: iexit  ! Exit return value 
    integer       :: k
    integer       :: k0f
    integer       :: k0l
    integer       :: k1f
    integer       :: k1l
    integer       :: ll
    integer       :: mf
    integer       :: ml
    integer       :: n
    integer       :: nmf
    integer       :: nmfd
    integer       :: nmfu
    integer       :: nml
    integer       :: nmlu
    real(fp)      :: aaaf
    real(fp)      :: aaal
    real(fp)      :: alfas
    real(fp)      :: alfau
    real(fp)      :: bbbf
    real(fp)      :: bbbl
    real(fp)      :: cccf
    real(fp)      :: cccl
    real(fp)      :: dddf
    real(fp)      :: dddl
    real(fp)      :: dep
    real(fp)      :: dt
    real(fp)      :: facc
    real(fp)      :: hnm
    real(fp)      :: relthk
    real(fp)      :: sepu
    character(30) :: errtxt
!
!! executable statements -------------------------------------------------------
!
    lundia   => gdp%gdinout%lundia
    hdt      => gdp%gdnumeco%hdt
    ag       => gdp%gdphysco%ag
    iro      => gdp%gdphysco%iro
    dzmin    => gdp%gdzmodel%dzmin
    !
    icxy = max(icx, icy)
    ddb  = gdp%d%ddbound
    dt   = 2.0 * hdt
    !
    ! SET BOUNDARY CONDITIONS
    !
    ! LOOP OVER GRID ROWS FOR BOUNDARY CONDITIONS
    !
    do ic = 1, norow
       !
       n    = irocol(1, ic)
       mf   = irocol(2, ic) - 1
       ml   = irocol(3, ic)
       ibf  = irocol(4, ic)
       ibl  = irocol(5, ic)
       nmf  = (n + ddb)*icy + (mf + ddb)*icx - icxy
       nmfu = nmf + icx
       nmfd = nmf - icx
       nml  = (n + ddb)*icy + (ml + ddb)*icx - icxy
       nmlu = nml + icx
       !
       if (zmodel) then
          k0f = kfumn0(nmf)
          k1f = kfumx0(nmf)
          k0l = kfumn0(nml)
          k1l = kfumx0(nml)
       else
          k0f = 1
          k1f = kmax
          k0l = 1
          k1l = kmax
       endif
       !
       ! SET COEFFICIENTS FOR BEGIN OF ROW IN THE CASE OF A CLOSED BOUNDARY
       !
!      if (kfu(nmf)==0) then
       if (kfu(nmf)==0 .and. ibf/=10) then
          !
          ! CLOSED BOUNDARY OR DRY VELOCITY-POINT AT BEGIN OF ROW
          !
          aa(nmf) = 0.0
          bb(nmf) = 1.0
          cc(nmf) = 0.0
          dd(nmf) = 0.0
          !
          ! LAYER VELOCITIES ARE SET TO ZERO
          !
          do k = k0f, k1f
             aak(nmf, k) = 0.0
             bbk(nmf, k) = 1.0
             cck(nmf, k) = 0.0
             ddk(nmf, k) = 0.0
          enddo
          if (ibf==2 .and. .not.wavcmp) then
             !         A(NMF)=0.0
             b(nmf) = 1.0
             c(nmf) = 0.0
             d(nmf) = circ2d(1, ic)
          else
             !         A(NMF)= 0.0
             b(nmf) =  1.0
             c(nmf) = -1.0
             d(nmf) =  0.0
          endif
       else
          !
          ! SET COEFFICIENTS FOR BEGIN OF ROW IN THE CASE OF AN OPEN BOUNDARY
          !
          dep  = dpu(nmf)
          sepu = tetau(nmf)*s0(nmf) + (1.0 - tetau(nmf))*s0(nmfu)
          if (ibf==2 .and. .not.wavcmp) then
             !
             ! WATERLEVEL BOUNDARY
             !
             alfau = circ2d(3, ic)/dt
             alfas = alfau*sqrt(ag/(dep + sepu))
             !
             aaaf = 1.0 + alfas*tetau(nmf)
             bbbf = alfau
             cccf = alfas*(1.0 - tetau(nmf))
             dddf = circ2d(1, ic) + alfau*umean(nmf) + alfas*sepu
             !
             ! LAYER VELOCITIES ARE COMPUTED USING
             ! A MOMENTUM EQUATION
             !
             a(nmf) = 0.0
             b(nmf) = aaaf - bbbf*aa(nmf)
             c(nmf) = cccf - bbbf*cc(nmf)
             d(nmf) = dddf - bbbf*dd(nmf)
          elseif (ibf==3) then
             !
             ! VELOCITY BOUNDARY
             !
             alfau = circ2d(3, ic)/dt
             alfas = alfau*sqrt(ag/(dep + sepu))
             !
             facc    = 1.0 + alfau
             aa(nmf) = alfas*tetau(nmf)/facc
             bb(nmf) = 1.0
             cc(nmf) = alfas*(1.0 - tetau(nmf))/facc
             dd(nmf) = (circ2d(1, ic) + alfau*umean(nmf) + alfas*sepu)/facc
             !
             ! LAYER VELOCITIES ( VELOCITY PROFILE )
             !
             do k = k0f, k1f
                aak(nmf, k) = aa(nmf)
                bbk(nmf, k) = 1.0
                cck(nmf, k) = cc(nmf)
                ddk(nmf, k) = (circ3d(k, 1, ic) + alfau*u0(nmf, k) + alfas*sepu)/facc
             enddo
             a(nmf) =  0.0
             b(nmf) =  1.0
             c(nmf) = -1.0
             d(nmf) =  0.0
          elseif (ibf==5 .or. ibf==7) then
             !
             ! DISCHARGE BOUNDARY
             !
             if (zmodel) then
                hnm = 0.0_fp
                do k = kfumn0(nmf), kfumx0(nmf)
                   hnm = hnm + dzu0(nmf,k)
                enddo
             else
                hnm = hu(nmf)
             endif
             aa(nmf) = 0.0
             bb(nmf) = 1.0
             cc(nmf) = 0.0
             dd(nmf) = circ2d(1, ic)/(guu(nmf)*hnm)
             !
             ! LAYER VELOCITIES ( VELOCITY PROFILE )
             !
             do k = k0f, k1f
                if (zmodel) then
                   relthk = max(dzu0(nmf, k), dzmin)
                else
                   relthk = hnm*thick(k)
                endif
                aak(nmf, k) = 0.0
                bbk(nmf, k) = 1.0
                cck(nmf, k) = 0.0
                ddk(nmf, k) = circ3d(k, 1, ic)/(guu(nmf)*relthk)
             enddo
             a(nmf) =  0.0
             b(nmf) =  1.0
             c(nmf) = -1.0
             d(nmf) =  0.0
          elseif ((ibf==6 .or. ibf==2) .and. wavcmp) then
             !
             ! WEAKLY REFLECTIVE BOUNDARY CONDITION AT LEFT BOUNDARY
             !
             ! The array crbc contains the coefficients for the Riemann
             ! boundary conditions and is determined in the routine INCRBC
             ! called in TRISOL. Method to be used in the case of time dependent
             ! short wave effects ('roller' and 'wavcmp' in md-file).
             !
             aa(nmf) = crbc(1, ic)
             bb(nmf) = 1.0
             cc(nmf) = crbc(2, ic)
             dd(nmf) = crbc(3, ic)
             !
             ! LAYER VELOCITIES ( VELOCITY PROFILE )
             !
             do k = 1, kmax
                aak(nmf, k) = aa(nmf)
                bbk(nmf, k) = 1.0
                cck(nmf, k) = cc(nmf)
                ddk(nmf, k) = dd(nmf)
             enddo
             a(nmf) = 0.0
             b(nmf) = crbc(4, ic)
             c(nmf) = crbc(5, ic)
             d(nmf) = crbc(6, ic)
          elseif (ibf==6 .and. .not.wavcmp) then
             !
             ! RIEMANN BOUNDARY CONDITIONS BASE ON 1D RIEMANN INVARIANTS
             !
             ! WEAKLY REFLECTIVE BOUNDARY
             !
             alfas   = sqrt(ag/(dep + sepu))
             if (dep<0) then
                aa(nmf) = 0.0
                bb(nmf) = 1.0
                cc(nmf) = 0.0
                dd(nmf) = 0.0
                !
                ! LAYER VELOCITIES ( VELOCITY PROFILE )
                !
                do k = k0f, k1f
                   aak(nmf, k) = 0.0
                   bbk(nmf, k) = 1.0
                   cck(nmf, k) = 0.0
                   ddk(nmf, k) = 0.0
                enddo               
             else
                aa(nmf) = alfas*tetau(nmf)
                bb(nmf) = 1.0
                cc(nmf) = alfas*(1.0 - tetau(nmf))
                dd(nmf) = circ2d(1, ic) + alfas*sepu - 2.*sqrt(ag*(dep + sepu))    &
                     & + 2.*sqrt(ag*dep)
                !
                ! LAYER VELOCITIES ( VELOCITY PROFILE )
                !
                do k = k0f, k1f
                   aak(nmf, k) = aa(nmf)
                   bbk(nmf, k) = 1.0
                   cck(nmf, k) = cc(nmf)
                   ddk(nmf, k) = circ3d(k, 1, ic) + alfas*sepu -                   &
                               & 2.*sqrt(ag*(dep + sepu)) + 2.*sqrt(ag*dep)
                enddo
             endif
             !
             a(nmf) =  0.0
             b(nmf) =  1.0
             c(nmf) = -1.0
             d(nmf) =  0.0
          elseif (ibf==8) then
             !
             ! NEUMANN BOUNDARY CONDITION INHOMOGENEOUS
             !
             b(nmf) = -1.0
             c(nmf) =  1.0
             d(nmf) = circ2d(1, ic)*gvu(nmf)
          elseif (kcs(nmf)==3) then
             !
             ! Begin of row is DD coupling point
             !
             do k = k0f, k1f
                aak(nmfd, k) = 0.0
                bbk(nmfd, k) = 1.0
                cck(nmfd, k) = 0.0
                ddk(nmfd, k) = u0(nmfd, k)
             enddo
!         else
          elseif ( (ibf /= 10) .and. (kcs(nmf) /= -1) ) then
             write (errtxt, '(6i5)') ic, (irocol(ll, ic), ll = 1, 5)
             call prterr(lundia    ,'S200'    ,errtxt    )
             !
             ! stop routine for DELFT3D
             !
             iexit = 4
             call d3stop(iexit     ,gdp       )
          endif
       endif
       !
       ! SET COEFFICIENTS FOR END OF ROW IN THE CASE OF A CLOSED BOUNDARY
       !
!      if (kfu(nml)==0) then
       if (kfu(nml)==0 .and. ibl/=10) then
          !
          ! CLOSED BOUNDARY OR PERMANENTLY DRY VELOCITY POINT AT END OF ROW
          !
          aa(nml) = 0.0
          bb(nml) = 1.0
          cc(nml) = 0.0
          dd(nml) = 0.0
          !
          ! LAYER VELOCITIES ARE SET TO ZERO
          !
          do k = k0l, k1l
             aak(nml, k) = 0.0
             bbk(nml, k) = 1.0
             cck(nml, k) = 0.0
             ddk(nml, k) = 0.0
          enddo
          if (ibl==2 .and. .not.wavcmp) then
             a(nmlu) = 0.0
             b(nmlu) = 1.0
             d(nmlu) = circ2d(2, ic)
          else
             a(nmlu) = -1.0
             b(nmlu) = 1.0
             d(nmlu) = 0.0
          endif
          c(nmlu) = 0.0
       else
          !
          ! SET COEFFICIENTS FOR END OF ROW IN THE CASE OF AN OPEN BOUNDARY
          ! IN CASE OF COUPLING BOUNDARY, DO NOTHING
          !
          sepu = tetau(nml)*s0(nml) + (1.0 - tetau(nml))*s0(nmlu)
          dep = dpu(nml)
          !
          if (ibl==2 .and. .not.wavcmp) then
             !
             ! WATERLEVEL BOUNDARY
             !
             alfau = circ2d(4, ic)/dt
             alfas = -alfau*sqrt(ag/(dep + sepu))
             !
             aaal = alfas*tetau(nml)
             bbbl = alfau
             cccl = -1.0 + alfas*(1.0 - tetau(nml))
             dddl = -circ2d(2, ic) + alfau*umean(nml) + alfas*sepu
             !
             ! LAYER VELOCITIES ARE COMPUTED USING
             ! A MOMENTUM EQUATION
             !
             a(nmlu) = aaal - bbbl*aa(nml)
             b(nmlu) = cccl - bbbl*cc(nml)
             c(nmlu) = 0.0
             d(nmlu) = dddl - bbbl*dd(nml)
          elseif (ibl==3) then
             !
             ! VELOCITY BOUNDARY
             !
             alfau = circ2d(4, ic)/dt
             alfas = -alfau*sqrt(ag/(dep + sepu))
             !
             facc = 1.0 + alfau
             aa(nml) = alfas*tetau(nml)/facc
             bb(nml) = 1.0
             cc(nml) = alfas*(1.0 - tetau(nml))/facc
             dd(nml) = (circ2d(2, ic) + alfau*umean(nml) + alfas*sepu)/facc
             !
             ! LAYER VELOCITIES ( VELOCITY PROFILE )
             !
             do k = k0l, k1l
                aak(nml, k) = aa(nml)
                bbk(nml, k) = 1.0
                cck(nml, k) = cc(nml)
                ddk(nml, k) = (circ3d(k, 2, ic) + alfau*u0(nml, k) + alfas*sepu)/facc
             enddo
             !
             a(nmlu) = -1.0
             b(nmlu) =  1.0
             c(nmlu) =  0.0
             d(nmlu) =  0.0
          elseif (ibl==5 .or. ibl==7) then
             !
             ! DISCHARGE BOUNDARY
             !
             if (zmodel) then
                hnm = 0.0_fp
                do k = kfumn0(nml), kfumx0(nml)
                   hnm = hnm + dzu0(nml,k)
                enddo
             else
                hnm = hu(nml)
             endif
             aa(nml) = 0.0
             bb(nml) = 1.0
             cc(nml) = 0.0
             dd(nml) = circ2d(2, ic)/(guu(nml)*hnm)
             !
             ! LAYER VELOCITIES ( VELOCITY PROFILE )
             !
             do k = k0l, k1l
                if (zmodel) then
                   relthk = max(dzu0(nml, k), dzmin)
                else
                   relthk = hnm*thick(k)
                endif
                aak(nml, k) = 0.0
                bbk(nml, k) = 1.0
                cck(nml, k) = 0.0
                ddk(nml, k) = circ3d(k, 2, ic)/(guu(nml)*relthk)
             enddo
             a(nmlu) = -1.0
             b(nmlu) =  1.0
             c(nmlu) =  0.0
             d(nmlu) =  0.0
          elseif ((ibl==6 .or. ibl==2) .and. wavcmp) then
             !
             ! WEAKLY REFLECTIVE BOUNDARY CONDITION AT RIGHT BOUNDARY
             !
             ! The array crbc contains the coefficients for the Riemann
             ! boundary conditions and is determined in the routine INCRBC
             ! called in TRISOL
             !
             aa(nml) = crbc(7, ic)
             bb(nml) = 1.0
             cc(nml) = crbc(8, ic)
             dd(nml) = crbc(9, ic)
             !
             ! LAYER VELOCITIES ( VELOCITY PROFILE )
             !
             do k = 1, kmax
                aak(nml, k) = aa(nml)
                bbk(nml, k) = 1.0
                cck(nml, k) = cc(nml)
                ddk(nml, k) = dd(nml)
             enddo
             a(nmlu) = crbc(10, ic)
             b(nmlu) = crbc(11, ic)
             c(nmlu) = 0.0
             d(nmlu) = crbc(12, ic)
          elseif (ibl==6 .and. .not.wavcmp) then
             !
             ! old Riemann boundary conditions
             !
             ! WEAKLY REFLECTIVE BOUNDARY
             !
             alfas = sqrt(ag/(dep + sepu))
             if (dep < 0) then
                aa(nml) = 0.0
                bb(nml) = 1.0
                cc(nml) = 0.0
                dd(nml) = 0.0
                !
                ! LAYER VELOCITIES ( VELOCITY PROFILE )
                !
                do k = k0l, k1l
                   aak(nml, k) = 0.0
                   bbk(nml, k) = 1.0
                   cck(nml, k) = 0.0
                   ddk(nml, k) = 0.0
                enddo
             else
                aa(nml) = -alfas*tetau(nml)
                bb(nml) = 1.0
                cc(nml) = -alfas*(1.0 - tetau(nml))
                dd(nml) = circ2d(2, ic) - alfas*sepu + 2.*sqrt(ag*(dep + sepu))    &
                        & - 2.*sqrt(ag*dep)
                !
                ! LAYER VELOCITIES ( VELOCITY PROFILE )
                !
                do k = k0l, k1l
                   aak(nml, k) = aa(nml)
                   bbk(nml, k) = 1.0
                   cck(nml, k) = cc(nml)
                   ddk(nml, k) = circ3d(k, 2, ic) - alfas*sepu +                   &
                               & 2.*sqrt(ag*(dep + sepu)) - 2.*sqrt(ag*dep)
                enddo
             endif
             a(nmlu) = -1.0
             b(nmlu) =  1.0
             c(nmlu) =  0.0
             d(nmlu) =  0.0
          elseif (ibl==8) then
             !
             ! NEUMANN BOUNDARY CONDITIONS INHOMOGENEOUS
             !
             a(nmlu) = -1.0
             b(nmlu) =  1.0
             c(nmlu) =  0.0
             d(nmlu) = circ2d(2, ic)*gvu(nml)
          elseif (kcs(nmlu)==3) then
             !
             ! End of row is DD coupling point
             !
             do k = k0l, k1l
                aak(nmlu, k) = 0.0
                bbk(nmlu, k) = 1.0
                cck(nmlu, k) = 0.0
                ddk(nmlu, k) = u0(nmlu, k)
             enddo
!         else
          elseif ( (ibl/=10) .and. (kcs(nml).ne.-1) ) then
             write (errtxt, '(6i5)') ic, (irocol(ll, ic), ll = 1, 5)
             call prterr(lundia    ,'S201'    ,errtxt    )
             !
             ! stop routine for DELFT3D
             !
             iexit = 4
             call d3stop(iexit     ,gdp       )
          endif
       endif
    enddo
end subroutine cucbp
