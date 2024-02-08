subroutine z_cucbp_nhfull(kmax      ,norow     ,icx       , &
                        & icy       ,irocol    ,kcs       ,kfu       , &
                        & kfumn0    ,kfumx0    ,s0        ,u0        , &
                        & hu        ,umean     ,guu       ,gvu       , &
                        & dzu0      ,circ2d    ,circ3d    ,dpu       , &
                        & aak       ,bbk       ,cck       ,ddk       , &
                        & crbc      ,gdp       )
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
!    Function: For the fully non-hydrostatic approach, boundary 
!              conditions should be set in the momentum equations
!              of the prediction step (Z_UZD).
!              Coefficients at boundary points MF, MFU, ML, and
!              MLU are set. The coefficients are stored in the
!              arrays AAK, BBK, CCK, and DDK (velocity points).
!              For the depth-averaged equations the coefficients
!              are stored in AA, BB, CC, DD (for the velocity
!              points) and A, B, C and D (for the water level
!              points).
! Method used:
!
!!--pseudo code and references--------------------------------------------------
!
! Written by Sebastian Ullmann
!
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
    logical                , pointer :: wavcmp
!
! Global variables
!
    integer                                                       :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                       :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                         , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in)  :: norow  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, norow)                    , intent(in)  :: irocol !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)  :: kfumx0 !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)  :: kfumn0 !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(12, norow)                  , intent(in)  :: crbc   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(4, norow)                   , intent(in)  :: circ2d !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: aak    !!  Internal work array (in Z_CUCNP & Z_UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: bbk    !!  Internal work array (in Z_CUCNP & Z_UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: cck    !!  Internal work array (in Z_CUCNP & Z_UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: ddk    !!  Internal work array, diagonal space at (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzu0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: u0     !  Description and declaration in esm_alloc_real.f90
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
    integer       :: nm
    integer       :: nmf
    integer       :: nmfd
    integer       :: nmfu
    integer       :: nml
    integer       :: nmlu
    real(fp)      :: alfas
    real(fp)      :: hnm
    real(fp)      :: tetau
    real(fp)      :: alfau
    real(fp)      :: dep
    real(fp)      :: dt
    real(fp)      :: facc
    real(fp)      :: relthk
    real(fp)      :: sepu
    character(30) :: errtxt
!
!! executable statements -------------------------------------------------------
!
    lundia   => gdp%gdinout%lundia
    hdt      => gdp%gdnumeco%hdt
    ag       => gdp%gdphysco%ag
    wavcmp   => gdp%gdprocs%wavcmp
    !
    icxy = max(icx, icy)
    ddb  = gdp%d%ddbound
    !
    ! subroutine used for full timestep of non-hydrostatic model
    ! only in combination with Z-layers 
    !
    dt   = 2.0_fp * hdt
    !
    ! SET BOUNDARY CONDITIONS
    !
    ! LOOP OVER GRID ROWS FOR BOUNDARY CONDITIONS
    !
    do ic = 1, norow
       !
       n    = irocol(1,ic)
       mf   = irocol(2,ic) - 1
       ml   = irocol(3,ic)
       ibf  = irocol(4,ic)
       ibl  = irocol(5,ic)
       nmf  = (n+ddb)*icy + (mf+ddb)*icx - icxy
       nmfu = nmf + icx
       nmfd = nmf - icx
       nml  = (n+ddb)*icy + (ml+ddb)*icx - icxy
       nmlu = nml + icx
       !
       k0f = kfumn0(nmf)
       k1f = kfumx0(nmf)
       k0l = kfumn0(nml)
       k1l = kfumx0(nml)
       !
       ! SET COEFFICIENTS FOR BEGIN OF ROW IN THE CASE OF A CLOSED BOUNDARY
       !
       if (kfu(nmf) == 0) then
          !
          ! CLOSED BOUNDARY OR DRY VELOCITY-POINT AT BEGIN OF ROW
          !
          do k = k0f, k1f
             aak(nmf,k) = 0.0_fp
             bbk(nmf,k) = 1.0_fp
             cck(nmf,k) = 0.0_fp
             ddk(nmf,k) = 0.0_fp
          enddo
       else
          !
          ! SET COEFFICIENTS FOR BEGIN OF ROW IN THE CASE OF AN OPEN BOUNDARY
          !
          !
          ! DETERMINE TETAU (for boundary conditions)
          !
          ! Determine umean again based on the velocities in the whole water column
          ! instead of only the velocities in the top layer(s) as done in z_checku
          ! needed in cucbp for alpha-coefficient on open boundary.
          !
          umean(nmf) = 0.0_fp
          hnm        = 0.0_fp
          do k = kfumn0(nmf), kfumx0(nmf)
             umean(nmf) = umean(nmf) + u0(nmf,k)*dzu0(nmf,k)
             hnm        = hnm + dzu0(nmf,k)
          enddo
          umean(nmf) = umean(nmf) / max(hnm, 0.01_fp)
          if (umean(nmf) >= 0.001_fp) then
             tetau = 1.0_fp
          elseif (umean(nmf) <= -0.001_fp) then
             tetau = 0.0_fp
          else
             tetau = 1.0_fp
             if (s0(nmfu) > s0(nmf)) then
                tetau = 0.0_fp
             endif
          endif
          dep  = dpu(nmf)
          sepu = tetau*s0(nmf) + (1.0_fp-tetau)*s0(nmfu)
          !
          if (ibf==2 .and. .not.wavcmp) then
             !
             ! WATERLEVEL BOUNDARY
             ! Set in Z_HYDPRES_NHFULL
             !
          elseif (ibf == 3) then
             !
             ! VELOCITY BOUNDARY
             !
             alfau   = circ2d(3,ic) / dt
             alfas   = alfau * sqrt(ag/(dep+sepu))
             facc    = 1.0_fp + alfau
             do k = k0f, k1f
                aak(nmf,k) = alfas * tetau / facc
                bbk(nmf,k) = 1.0_fp
                cck(nmf,k) = alfas * (1.0_fp-tetau) / facc
                ddk(nmf,k) = (circ3d(k,1,ic)+alfau*u0(nmf,k)+alfas*sepu) / facc
             enddo
          elseif (ibf==5 .or. ibf==7) then
             !
             ! DISCHARGE BOUNDARY
             !
             do k = k0f, k1f
                aak(nmf,k) = 0.0_fp
                bbk(nmf,k) = 1.0_fp
                cck(nmf,k) = 0.0_fp
                ddk(nmf,k) = circ3d(k,1,ic) / (guu(nmf)*dzu0(nmf,k))
             enddo
          elseif ((ibf==6.or.ibf==2) .and. wavcmp) then
             !
             ! WEAKLY REFLECTIVE BOUNDARY CONDITION AT LEFT BOUNDARY
             !
             ! The array crbc contains the coefficients for the Riemann
             ! boundary conditions and is determined in the routine INCRBC
             ! called in TRISOL. Method to be used in the case of time dependent
             ! short wave effects ('roller' and 'wavcmp' in md-file).
             !
             do k = 1, kmax
                aak(nmf,k) = crbc(1,ic)
                bbk(nmf,k) = 1.0_fp
                cck(nmf,k) = crbc(2,ic)
                ddk(nmf,k) = crbc(3,ic)
             enddo
          elseif (ibf==6 .and. .not.wavcmp) then
             !
             ! RIEMANN BOUNDARY CONDITIONS BASE ON 1D RIEMANN INVARIANTS
             !
             ! WEAKLY REFLECTIVE BOUNDARY
             !
             alfas = sqrt(ag/(dep+sepu))
             if (dep < 0) then
                do k = k0f, k1f
                   aak(nmf,k) = 0.0_fp
                   bbk(nmf,k) = 1.0_fp
                   cck(nmf,k) = 0.0_fp
                   ddk(nmf,k) = 0.0_fp
                enddo
             else
                do k = k0f, k1f
                   aak(nmf,k) = alfas * tetau
                   bbk(nmf,k) = 1.0_fp
                   cck(nmf,k) = alfas * (1.0_fp-tetau)
                   ddk(nmf,k) = circ3d(k, 1, ic) + alfas*sepu  &
                           & - 2.0_fp*sqrt(ag*(dep+sepu))   &
                           & + 2.0_fp*sqrt(ag*dep)
                enddo
             endif
          elseif (ibf == 8) then
             !
             ! NEUMANN BOUNDARY CONDITION INHOMOGENEOUS
             !
          elseif (kcs(nmf) == 3) then
             !
             ! Begin of row is DD coupling point
             !
             do k = k0f, k1f
                aak(nmfd,k) = 0.0_fp
                bbk(nmfd,k) = 1.0_fp
                cck(nmfd,k) = 0.0_fp
                ddk(nmfd,k) = u0(nmfd,k)
             enddo
          else
             write (errtxt, '(6i5)') ic, (irocol(ll, ic), ll = 1, 5)
             call prterr(lundia    ,'S200'    ,errtxt    )
             !
             ! stop routine for DELFT3D
             !
             iexit = 4
             call d3stop(iexit,gdp)
          endif
       endif
       !
       ! SET COEFFICIENTS FOR END OF ROW IN THE CASE OF A CLOSED BOUNDARY
       !
       if (kfu(nml) == 0) then
          !
          ! CLOSED BOUNDARY OR PERMANENTLY DRY VELOCITY POINT AT END OF ROW
          !
          do k = k0l, k1l
             aak(nml,k) = 0.0_fp
             bbk(nml,k) = 1.0_fp
             cck(nml,k) = 0.0_fp
             ddk(nml,k) = 0.0_fp
          enddo
       else
          !
          ! SET COEFFICIENTS FOR END OF ROW IN THE CASE OF AN OPEN BOUNDARY
          !
          ! DETERMINE TETAU (for boundary conditions)
          !
          ! Determine umean again based on the velocities in the whole water column
          ! instead of only the velocities in the top layer(s) as done in z_checku
          ! needed in cucbp for alpha-coefficient on open boundary.
          !
          umean(nml) = 0.0_fp
          hnm        = 0.0_fp
          do k = kfumn0(nml), kfumx0(nml)
             umean(nml) = umean(nml) + u0(nml,k)*dzu0(nml,k)
             hnm        = hnm + dzu0(nml,k)
          enddo
          umean(nml) = umean(nml) / max(hnm, 0.01_fp)
          if (umean(nml) >= 0.001_fp) then
             tetau = 1.0_fp
          elseif (umean(nml) <= -0.001_fp) then
             tetau = 0.0_fp
          else
             tetau = 1.0_fp
             if (s0(nmlu) > s0(nml)) then
                tetau = 0.0_fp
             endif
          endif
          dep = dpu(nml)
          sepu = tetau*s0(nml) + (1.0_fp-tetau)*s0(nmlu)
          !
          if (ibl==2 .and. .not.wavcmp) then
             !
             ! WATERLEVEL BOUNDARY
             ! SET in Z_HYDPRES_NHFULL
             !
          elseif (ibl == 3) then
             !
             ! VELOCITY BOUNDARY
             !
             alfau = circ2d(4,ic) / dt
             alfas = -alfau * sqrt(ag/(dep+sepu))
             facc  = 1.0_fp + alfau
             do k = k0l, k1l
                aak(nml,k) = alfas*tetau/facc
                bbk(nml,k) = 1.0_fp
                cck(nml,k) = alfas*(1.0_fp - tetau)/facc
                ddk(nml,k) = (circ3d(k,2,ic)+alfau*u0(nml,k)+alfas*sepu) / facc
             enddo
          elseif (ibl==5 .or. ibl==7) then
             !
             ! DISCHARGE BOUNDARY
             !
             do k = k0l, k1l
                aak(nml,k) = 0.0_fp
                bbk(nml,k) = 1.0_fp
                cck(nml,k) = 0.0_fp
                ddk(nml,k) = circ3d(k,2,ic) / (guu(nml)*dzu0(nml,k))
             enddo
          elseif ((ibl==6 .or. ibl==2) .and. wavcmp) then
             !
             ! WEAKLY REFLECTIVE BOUNDARY CONDITION AT RIGHT BOUNDARY
             !
             ! The array crbc contains the coefficients for the Riemann
             ! boundary conditions and is determined in the routine INCRBC
             ! called in TRISOL
             !
             do k = 1, kmax
                aak(nml,k) = crbc(7,ic)
                bbk(nml,k) = 1.0_fp
                cck(nml,k) = crbc(8,ic)
                ddk(nml,k) = crbc(9,ic)
             enddo
          elseif (ibl==6 .and. .not.wavcmp) then
             !
             ! old Riemann boundary conditions
             !
             ! WEAKLY REFLECTIVE BOUNDARY
             !
             alfas = sqrt(ag/(dep+sepu))
             if (dep < 0) then
                aak(nml,k) = 0.0_fp
                bbk(nml,k) = 1.0_fp
                cck(nml,k) = 0.0_fp
                ddk(nml,k) = 0.0_fp
             else
                do k = k0l, k1l
                   aak(nml,k) = -alfas * tetau
                   bbk(nml,k) = 1.0_fp
                   cck(nml,k) = -alfas * (1.0_fp-tetau)
                   ddk(nml,k) = circ3d(k,2,ic) - alfas*sepu  &
                              & + 2.0_fp*sqrt(ag*(dep+sepu)) &
                              & - 2.0_fp*sqrt(ag*dep)
                enddo
             endif
          elseif (ibl == 8) then
             !
             ! NEUMANN BOUNDARY CONDITIONS INHOMOGENEOUS
             !
          elseif (kcs(nmlu) == 3) then
             !
             ! End of row is DD coupling point
             !
             do k = k0l, k1l
                aak(nmlu,k) = 0.0_fp
                bbk(nmlu,k) = 1.0_fp
                cck(nmlu,k) = 0.0_fp
                ddk(nmlu,k) = u0(nmlu, k)
             enddo
          else
             write (errtxt, '(6i5)') ic, (irocol(ll, ic), ll = 1, 5)
             call prterr(lundia    ,'S201'    ,errtxt    )
             !
             ! stop routine for DELFT3D
             !
             iexit = 4
             call d3stop(iexit,gdp)
          endif
       endif
    enddo
end subroutine z_cucbp_nhfull
