subroutine thahbc(j         ,nmmaxj    ,icx       ,icy       ,kmax      , &
                & lstsci    ,lstsc     ,nrob      ,noroco    ,nto       , &
                & nst       ,kfsmin    ,nob       ,thtim     ,rettim    , &
                & u1        ,v1        ,r0        ,rbnd      ,rthbnd    , &
                & sig       ,dzs1      ,dps       ,s1        ,gdp       )
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
! Computes boundary values at open boundaries,
! based on Thatcher Harleman concept.
! - At each half timestep the bnd. condition at all boundary points are updated.
!   This is done to keep the routine as simple as possible
! - For negative times the boundary value will be
!   replaced by the inner value from R0
! - NOTE: For Z-model simulations, THAHBC is completely based on the old geometry 
!   corresponding to S0, so:
!   DZS1 = DZS0, S1 = S0, U1 = U0 and V1 = V0.
!
!!--pseudo code and references--------------------------------------------------
!
! Thatcher, M.L. and Harleman, R.F., 1972,
! "A mathematical model for the prediction of unsteady salinity intrusion
!  in estruaries",
! Massachusetts Institute of Technology, Report no 144.
!
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    !
    implicit none
!
! Parameters
!
    integer, parameter :: surface = 1
    integer, parameter :: bottom  = 2
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)     , pointer :: hdt
    logical      , pointer :: const
    logical      , pointer :: zmodel
!
! Global variables
!
    integer                                                 , intent(in) :: icx    !  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                 , intent(in) :: icy    !  Increment in the Y-dir. (see ICX)
    integer                                                              :: j      !  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                                 , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in) :: lstsc  !  Description and declaration in dimens.igs
    integer                                                 , intent(in) :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                              :: nmmaxj !  Description and declaration in dimens.igs
    integer                                                 , intent(in) :: noroco !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in) :: nrob   !  Description and declaration in esm_alloc_int.f90
    integer                                                              :: nst    !  Current time step counter
    integer                                                 , intent(in) :: nto    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(8, nrob)                             , intent(in) :: nob    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kfsmin !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in) :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in) :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in) :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in) :: r0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                               , intent(in) :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax, max(1, lstsc), 2, noroco)                  :: thtim  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax, max(1, lstsc), 2, noroco)                  :: rbnd   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax, max(1, lstsc), 2, noroco)                  :: rthbnd !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nto , max(1, lstsc), 2)             , intent(in) :: rettim !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer  :: ddb
    integer  :: i
    integer  :: icxy   ! max (icx, ixy) 
    integer  :: iflc   ! Help var. to determine whether a bnd. point is type first (IFLC=1) or last  (IFLC=2) if icy = nmax iflc = iflr v.v. 
    integer  :: iflr   ! Help var. to determine whether a bnd. point is type first (IFLR=1) or last  (IFLR=2) if icy = nmax iflr = iflc v.v. 
    integer  :: ir     ! Contains a non-zero value if the boundary point under consideration belongs to a computational row 
    integer  :: k
    integer  :: k1st
    integer  :: ll     ! Use ll instead of l to distinct with 1
    integer  :: m
    integer  :: n
    integer  :: nm     ! n,m point in array 
    integer  :: nmr0   ! n,m point for concentration inside model depending on always iflrow 
    integer  :: nmudir ! n,m point for u-velocity depending on iflrow 
    integer  :: nmvdir ! n,m point for v-velocity depending on iflcol 
    integer  :: nseg
    real(fp) :: cosrrt
    real(fp) :: dtsec  ! Integration time step [seconds] 
    real(fp) :: reldep
    real(fp) :: rrt
    real(fp) :: rtrntm
    real(fp) :: udir
    real(fp) :: uflow
    real(fp) :: uvflow
    real(fp) :: vdir
    real(fp) :: vflow
    real(fp) :: zcoor
!
!! executable statements -------------------------------------------------------
!
    const      => gdp%gdprocs%const
    zmodel     => gdp%gdprocs%zmodel
    hdt        => gdp%gdnumeco%hdt
    !
    ddb   = gdp%d%ddbound
    dtsec = 2.0_fp * hdt
    icxy  = max(icx, icy)
    !
    ! For all open boundary points nrob we are only interested in those
    ! which are in the same direction as difu (first rows, then columns)
    ! if icy = 1    then row direction
    ! if icy = nmax then column direction
    !
    do i = 1, nrob
       if (icy == 1) then
          m    = nob(1, i)
          n    = nob(2, i)
          iflr = nob(4, i)
          ir   = nob(5, i)
          iflc = nob(6, i)
       else
          m    = nob(2, i)
          n    = nob(1, i)
          iflr = nob(6, i)
          ir   = nob(7, i)
          iflc = nob(4, i)
       endif
       nseg = nob(8, i)
       !
       ! Only if a RETTIM time for NSEG is given (per definition > 0)
       ! and only if a row is an open boundary the Th. Harleman boundary
       ! conditions will be calculated and set in rbnd (for inflow)
       !
       if (iflr /= 0) then
          nm = (n+ddb)*icy + (m+ddb)*icx - icxy
          !
          ! if IFLR  = 1 -> treat as first boundary point
          !                 velocity from point NMUDIR = NM and
          !                 concentrations from inside the model
          !                 NMR0 = NMUDIR + ICX
          ! else            treat as last  boundary point
          !                 velocity from point NMUDIR = NM - ICX and
          !                 concentrations from inside the model
          !                 NMR0 = NMUDIR
          !
          if (iflr == 1) then
             udir   = -1.0_fp
             nmudir = nm
             nmr0   = nmudir + icx
          else
             udir   = 1.0_fp
             nmudir = nm - icx
             nmr0   = nmudir
          endif
          !
          ! For oblique boundaries the other direction component must
          ! taken into consideration as well
          ! if IFLC  = 1 -> treat as first boundary point
          !                 velocity from point NMVDIR = NM
          ! if IFLC  = 2 -> treat as last  boundary point
          !                 velocity from point NMVIDR = NM - ICY
          ! if IFLC  = 0 -> then no oblique boundary which means
          !                 velocity may not be used (VDIR = 0.)
          !
          if (iflc == 1) then
             vdir   = -1.0_fp
             nmvdir = nm
          elseif (iflc == 2) then
             vdir   = 1.0_fp
             nmvdir = nm - icy
          else
             vdir   = 0.0_fp
             nmvdir = nm
          endif
          !
          ! Loop over all layers for RETTIM > 0
          ! Calculate the dominant velocity by comparing the modula
          ! of the U1 (NMUDIR,K) and V1 (NMVDIR,K)
          ! In case of Z-MODEL array SIG contains the vertical coordinates
          !
          do ll = 1, lstsc
             if (rettim(nseg,ll,surface) > 0.0_fp) then
                if (zmodel) then
                   k1st = kfsmin(nm)
                else
                   k1st = 1
                endif
                !
                zcoor = 0.0_fp
                do k = k1st, kmax
                   if (zmodel) then
                      if (k == k1st) then
                         zcoor = zcoor + 0.5_fp*dzs1(nm,k)
                      else
                         zcoor = zcoor + 0.5_fp*(dzs1(nm,k-1) + dzs1(nm,k))
                      endif
                      reldep = zcoor / (real(dps(nm),fp)+s1(nm))
                   else
                      reldep = 1.0_fp + sig(k)
                   endif
                   uflow = udir * u1(nmudir,k)
                   vflow = vdir * v1(nmvdir,k)
                   if (abs(vflow) > abs(uflow)) then
                      uvflow = vflow
                   else
                      uvflow = uflow
                   endif
                   !
                   ! if UVFLOW > 0.0 outflow condition
                   ! if UVFLOW < 0.0 inflow  condition
                   ! if UVFLOW = 0.0 then velocity boundary is dry, in which
                   !                 case the THTIM will set to 0.
                   !                 when the velocity boundary is set wet
                   !                 again it is the same as a start condition
                   ! NOTE: if start condition is an inflow then
                   !       THTIM := 0. and RTHBND := 0. which means
                   !       RBND calculated here = RBND calculated in INCBCC
                   !
                   if (uvflow > 0.0_fp) then
                      !
                      ! outflow  condition
                      ! NOTE:In the boundary point the constituent value is
                      !      reflected from the inner point. For outflow the
                      !      diffusive flux is switched off.
                      !
                      rthbnd(k,ll,iflr,ir) = r0(nmr0,k,ll)
                      rbnd  (k,ll,iflr,ir) = r0(nmr0,k,ll)
                      thtim (k,ll,iflr,ir) = rettim(nseg,ll,bottom) + reldep*(rettim(nseg,ll,surface)-rettim(nseg,ll,bottom))
                   elseif (uvflow < 0.0_fp) then
                      !
                      ! inflow  condition
                      !
                      thtim(k,ll,iflr,ir) = thtim(k,ll,iflr,ir) - dtsec
                      if (thtim(k,ll,iflr,ir) < 0.0_fp) then
                         thtim(k,ll,iflr,ir) = 0.0_fp
                      endif
                      !
                      ! return time is a clock function using a cosinus
                      ! per definition  0. <= RRT          <= 1.
                      !           thus -1. <= COS (RRT*PI) <= 1.
                      !           thus  0. <= COSRRT       <= 1.
                      !
                      rtrntm = rettim(nseg,ll,bottom) + reldep*(rettim(nseg,ll,surface)-rettim(nseg,ll,bottom))
                      rrt    = thtim(k,ll,iflr,ir) / rtrntm
                      cosrrt = (cos(rrt*pi)+1.0_fp) * 0.5_fp
                      !
                      rbnd(k,ll,iflr,ir) =   rthbnd(k,ll,iflr,ir)  &
                                        & + cosrrt * (rbnd(k,ll,iflr,ir)-rthbnd(k,ll,iflr,ir))
                   else
                      !
                      ! boundary point dry
                      !
                      thtim(k, ll, iflr, ir) = 0.0_fp
                   endif
                enddo
             elseif (rettim(nseg,ll,surface) < 0.0_fp) then
                !
                ! In the boundary point the constituent value is
                ! reflected from the inner point.
                ! For ZMODEL we can use 1 to KMAX as lower/upper bound,
                ! because r0 is zero anyway when the point is inactive
                !
                do k = 1, kmax
                   rbnd(k,ll,iflr,ir) = r0(nmr0,k,ll)
                enddo
             else
                ! rettim(nseg,ll,surface) == 0.0_fp
             endif
          enddo
        endif
     enddo
 end subroutine thahbc