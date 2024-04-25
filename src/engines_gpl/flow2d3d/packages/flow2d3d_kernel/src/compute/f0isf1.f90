subroutine f0isf1(stage     ,dischy    ,nst       ,zmodel    ,j         , &
                & nmmax     ,nmmaxj    ,nmax      ,kmax      ,lstsci    , &
                & ltur      ,nsrc      ,kcu       ,kcv       ,kcs       , &
                & kfs       ,kfu       ,kfv       ,kfsmin    ,kfsmax    , &
                & kfumin    ,kfumax    ,kfvmin    ,kfvmax    ,kfsmn0    , &
                & kfumn0    ,kfvmn0    ,kfsmx0    ,kfumx0    ,kfvmx0    , &
                & kfsz0     ,kfuz0     ,kfvz0     , &
                & kfsz1     ,kfuz1     ,kfvz1     , &
                & s0        ,s1        ,u0        , &
                & u1        ,v0        ,v1        ,volum0    ,volum1    , &
                & r0        ,r1        ,rtur0     ,rtur1     ,disch     , &
                & discum    ,hu        ,hv        ,dzu1      ,dzv1      , &
                & dzs1      ,dzu0      ,dzv0      ,dzs0      ,qxk       , &
                & qyk       ,s00       ,w0        , &
                & w1        ,p0        ,p1        ,hu0       ,hv0       , &
                & ewabr0    ,ewabr1    , &
                & ewave0    ,ewave1    ,eroll0    ,eroll1    ,roller    , &
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
!    Function: - Copies the contents of the variable at the old
!                time level to the new time level
!              - Check whether any instabilities during the simu-
!                lation have occured by commparing the values at
!                the old time level and the new time level
!                (carried out if NST > 0).
!                Conditions for instabilities : |S1 - S0| >25 m
!                                               |V1 - V0| > 5 m/s
!                                               |U1 - U0| > 5 m/s
!              - Calculate cummulative discharges for all layers
!              - Calculate cummulative discharges for discharge
!                points
!              - Extra term for fast convergence to steady state
!                  - d  d zeta     d2 u
!                    -- ----- = -H ----
!                    dx d t        d x2
!                Only if DISCHY = Backward Euler
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp) , pointer :: smax
    real(fp) , pointer :: velmax
    integer  , pointer :: ntstep
    integer  , pointer :: lundia
    logical  , pointer :: nonhyd
    integer  , pointer :: nh_level
!
! Global variables
!
    integer                                                 , intent(in)  :: j      !!  Begin pointer for arrays which have
                                                                                    !!  been transformed into 1D arrays.
                                                                                    !!  Due to the shift in the 2nd (M-)
                                                                                    !!  index, J = -2*NMAX + 1
    integer                                                 , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: nmmaxj !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: nst    !!  Current time step counter
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(out) :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(out) :: kfsmn0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfumax !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(out) :: kfumx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(out) :: kfumn0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfvmax !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfvmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(out) :: kfvmx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(out) :: kfvmn0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: kfsz0  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: kfuz0  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: kfvz0  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: kfsz1  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: kfuz1  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: kfvz1  !  Description and declaration in esm_alloc_int.f90
    logical                                                 , intent(in)  :: roller
    logical                                                 , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(out) :: eroll0 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: eroll1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(out) :: ewabr0 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: ewabr1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(out) :: ewave0 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: ewave1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(out) :: hu0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(out) :: hv0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(out) :: s00    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(out) :: w0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: w1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur), intent(out) :: rtur0  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur), intent(in)  :: rtur1  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: dzs0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: dzu0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzu1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: dzv0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzv1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: p0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: p1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: v0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: volum0 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: volum1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(out) :: r0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                               , intent(in)  :: disch  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                                             :: discum !  Description and declaration in esm_alloc_real.f90
    character(8)                                                          :: dischy !  Description and declaration in tricom.igs
    character(8)                                                          :: stage  ! First or second half time step     
!
! Local variables
!
    integer        :: ddb
    integer        :: icx    ! Increment in the X-dir., if ICX= NMAX then computation proceeds in the X- dir. If icx=1 then computation pro- ceeds in the Y-dir. 
    integer        :: icy    ! Increment in the Y-dir. (see ICX) 
    integer        :: iexit
    integer        :: ierror
    integer        :: k
    integer        :: k2
    integer        :: k0
    integer        :: k1
    integer        :: l
    integer        :: m
    integer        :: n
    integer        :: nm
    integer        :: nmaxddb
    real(fp)       :: delta
    real(fp)       :: suvval
    real(fp)       :: s01max
    real(fp)       :: u01max
    real(fp)       :: v01max
    real(fp)       :: zz
    character(300) :: message
!
!! executable statements -------------------------------------------------------
!
    smax     => gdp%gdf0isf1%smax
    velmax   => gdp%gdf0isf1%velmax
    ntstep   => gdp%gdinttim%ntstep
    lundia   => gdp%gdinout%lundia
    nonhyd   => gdp%gdprocs%nonhyd
    nh_level => gdp%gdnonhyd%nh_level
    !
    s01max = -1.0_fp
    u01max = -1.0_fp
    v01max = -1.0_fp
    k0 = 1
    k1 = kmax
    !
    ddb     = gdp%d%ddbound
    nmaxddb = nmax + 2*gdp%d%ddbound
    icx     = nmaxddb
    icy     = 1
    !
    ! Check on waterlevel and velocity change (previously referred to as "gradient")
    ! Not for initialisation (nst < 0)
    !
    if (nst >= 0) then
       !
       ! Water level change:
       ! if (abs(s0(nm)-s1(nm)) >= smax) then stop
       !
       do nm = 1, nmmax
          if (kcs(nm) > 0) then
             s01max = max( s01max , abs(s0(nm)-s1(nm)) )
          endif
       enddo
       !
       if (s01max >= smax) then
          write(message,'(a,f10.2,a,i0,a)') 'Water level change too high > ', smax, &
               & ' m (per 0.5 DT) after ', ntstep, ' timesteps in the following points:'
          call prterr(lundia, 'P004', trim(message))
          do nm = 1, nmmax
             if (kcs(nm) > 0) then
                suvval = abs(s0(nm) - s1(nm))
                if (suvval >= smax) then
                   call nm_to_n_and_m(nm, n, m, gdp)
                   write (lundia, '(2(a,i0),3(a,e12.3))')    &
                       & '           (m,n) = (', m, ',', n,  &
                       & '), s0, s1, abs(s0-s1): ',          &
                       & s0(nm), ' , ', s1(nm), ' , ', suvval
                endif
             endif
          enddo
          ierror = 1
       else if (isnan(s01max)) then
          write(message,'(a,i0,a)') 'NaN found after ', ntstep, ' timestep(s).'
          call prterr(lundia, 'P004', trim(message))
          ierror = 1
       else
          ierror = 0
       endif
       !
       ! stop routine for DELFT3D,
       !    if in any subdomain s01max >= smax
       !
       call dfreduce_gdp(ierror, 1, dfint, dfmax, gdp)
       if (ierror > 0) then
          iexit = 4
          call d3stop(iexit, gdp)
       endif
       !
       ! Velocity change:
       ! if (abs(u0(nm,k)-u1(nm,k)) >= velmax) then warning
       ! if (abs(v0(nm,k)-v1(nm,k)) >= velmax) then warning
       !
       do nm = 1, nmmax
          if (kcu(nm) > 0) then
             do k = 1, kmax
                u01max = max(u01max, abs(u0(nm, k) - u1(nm, k)))
             enddo
          endif
          if (kcv(nm) > 0) then
             do k = 1, kmax
                v01max = max(v01max, abs(v0(nm, k) - v1(nm, k)))
             enddo
          endif
       enddo
       !
       if (u01max > velmax) then
          write(message,'(a,f10.2,a,i0,a)') 'Velocity change too high > ', velmax, &
               & ' m/s (per 0.5 DT) after ', ntstep, ' timesteps in the following points:'
          call prterr(lundia, 'U190', trim(message))
          do nm = 1, nmmax
             if (kcu(nm) > 0) then
                do k = 1, kmax
                   suvval = abs(u0(nm, k) - u1(nm, k))
                   if (suvval >= velmax) then
                      call nm_to_n_and_m(nm, n, m, gdp)
                      write (lundia, '(3(a,i0),3(a,e12.3))')             &
                          & '           (m,n,k) = (', m, ',', n, ',', k, &
                          & '), u0, u1, abs(u0-u1): ',                   &
                          & u0(nm,k), ' , ', u1(nm,k), ' , ', suvval
                   endif
                enddo
             endif
          enddo
       endif
       !
       if (v01max > velmax) then
          write(message,'(a,f10.2,a,i0,a)') 'Velocity change too high > ', velmax, &
               & ' m/s (per 0.5 DT) after ', ntstep, ' timesteps in the following points:'
          call prterr(lundia, 'U190', trim(message))
          do nm = 1, nmmax
             if (kcv(nm) > 0) then
                do k = 1, kmax
                   suvval = abs(v0(nm, k) - v1(nm, k))
                   if (suvval >= velmax) then
                      call nm_to_n_and_m(nm, n, m, gdp)
                      write (lundia, '(3(a,i0),3(a,e12.3))')             &
                          & '           (m,n,k) = (', m, ',', n, ',', k, &
                          & '), v0, v1, abs(v0-v1): ',                   &
                          & v0(nm,k), ' , ', v1(nm,k), ' , ', suvval
                   endif
                enddo
             endif
          enddo
       endif
    endif
    !
    ! Fast convergence to steady state
    ! Commented out because it is not used (delta = 0)
    !
    !delta = 0.0
    !do nm = 1, nmmax
    !   if (kcs(nm) > 0) then
    !      if (zmodel) then
    !         k0 = kfumin(nm)
    !         k1 = kfumax(nm)
    !      endif
    !      !
    !      ! test "and" to avoid halos
    !      !
    !      if ((kcu(nm)==1) .and. (kfu(nm)==1)) then 
    !         zz = delta * (s1(nm + icx) - s0(nm + icx) - s1(nm) + s0(nm)) / hu(nm)
    !         do k = k0, k1
    !            u1(nm, k) = u1(nm, k) + zz
    !         enddo
    !      endif
    !      if (zmodel) then
    !         k0 = kfvmin(nm)
    !         k1 = kfvmax(nm)
    !      endif
    !      !
    !      ! test "and" to avoid halos
    !      !
    !      if ((kcv(nm)==1) .and. (kfv(nm)==1)) then 
    !         zz = delta * (s1(nm + icy) - s0(nm + icy) - s1(nm) + s0(nm)) / hv(nm)
    !         do k = k0, k1
    !            v1(nm, k) = v1(nm, k) + zz
    !         enddo
    !      endif
    !   endif
    !enddo
    !
    ! copy u1 to u0, v1 to v0 and volum1 to volum0
    !
    do nm = 1, nmmax
       if (zmodel) k0 = kfumin(nm)
       do k = k0, kmax
          if (kcu(nm) /= 0) then
             u0(nm, k) = u1(nm, k)
          endif
       enddo
       if (zmodel) k0 = kfvmin(nm)
       do k = k0, kmax
          if (kcv(nm) /= 0) then
             v0(nm, k) = v1(nm, k)
          endif
       enddo
       if (zmodel) k0 = kfsmin(nm)
       do k = k0, kmax
          if (kcs(nm) /= 0) then
             volum0(nm, k) = volum1(nm, k)
          endif
       enddo
    enddo
    !
    ! copy s1 to s0;hu to hu0; hv to hv0
    !
    do nm = 1, nmmax
       if (kcs(nm) /= 0) then
          s0(nm) = s1(nm)
       endif
       if (kcu(nm) /= 0) then
          hu0(nm) = hu(nm)
       endif
       if (kcv(nm) /= 0) then
          hv0(nm) = hv(nm)
       endif
    enddo
    !
    ! copy r1 to r0
    !
    do l = 1, lstsci
       do nm = 1, nmmax
          if (zmodel) k0 = kfsmin(nm)
          do k = k0, kmax
             if (kcs(nm)/=0) then
                r0(nm, k, l) = r1(nm, k, l)
             endif
          enddo
       enddo
    enddo
    !
    ! copy rtur0 to rtur1
    !
    do l = 1, ltur
       do nm = 1, nmmax
          if (zmodel) k0 = kfsmin(nm)
          do k = k0 - 1, kmax
             if (kcs(nm)/=0) then
                rtur0(nm, k, l) = rtur1(nm, k, l)
             endif
          enddo
       enddo
    enddo
    !!
    !! calculate cumm. discharge  -> being removed to updcomflx.f90
    !!
    ! copy kfsmx/u/v to kfsmax/u/v0
    !
    if (zmodel) then
       do nm = 1, nmmax
          if (stage == 'stage1') then
             kfvmn0(nm) = kfvmin(nm)
             kfvmx0(nm) = kfvmax(nm)
             if (kfs(nm) == 1) then
                do k = kfsmin(nm), kmax
                   kfvz0(nm, k) = kfvz1(nm, k)
                   dzv0 (nm, k) = dzv1 (nm, k)
                enddo
             endif
          elseif (stage == 'stage2') then
             kfumn0(nm) = kfumin(nm)
             kfumx0(nm) = kfumax(nm)
             if (kfs(nm) == 1) then
                do k = kfsmin(nm), kmax
                   kfuz0(nm, k) = kfuz1(nm, k)
                   dzu0 (nm, k) = dzu1 (nm, k)
                enddo
             endif
          elseif (stage == 'both') then
             kfumn0(nm) = kfumin(nm)
             kfumx0(nm) = kfumax(nm)
             kfvmn0(nm) = kfvmin(nm)
             kfvmx0(nm) = kfvmax(nm)
             if (kfs(nm) == 1) then
                do k = kfsmin(nm), kmax
                   kfuz0(nm, k) = kfuz1(nm, k)
                   kfvz0(nm, k) = kfvz1(nm, k)
                   dzu0 (nm, k) = dzu1 (nm, k)
                   dzv0 (nm, k) = dzv1 (nm, k)
                enddo
             endif
          endif
          kfsmn0(nm) = kfsmin(nm)
          kfsmx0(nm) = kfsmax(nm)
          if (kfs(nm) == 1) then
             do k = kfsmin(nm), kmax
                kfsz0(nm, k) = kfsz1(nm, k)
                dzs0 (nm, k) = dzs1 (nm, k)
             enddo
          endif
       enddo
       !
       ! values for hydrodynamic pressure
       !
       if (nonhyd) then
          p0 = 0.0_fp
          do nm = 1, nmmax
             do k = kfsmin(nm), kmax
                if (kcs(nm) /= 0) then
                   p0(nm, k) = p1(nm, k)
                endif
             enddo
          enddo
          !
          ! copy s1 to s0
          !
          ! The test "if (nst== - 1) then" is disabled by Sebastian Ullmann, so:
          if ((nh_level == nh_full) .or. (nh_level== nh_weak .and. nst== - 1)) then
             do nm = 1, nmmax
                if (kcs(nm) /= 0) then
                   s00(nm) = s0(nm)
                endif
                do k = kfsmin(nm), kmax
                   if (kcs(nm) /= 0) then
                      w0(nm, k) = w1(nm, k)
                   endif
                enddo
             enddo
          endif
       endif
    endif
    !
    ! copy ewabr1 to ewabr0, ewave1 to ewave0 and eroll1 to eroll0
    !
    if (roller) then
       do nm = 1, nmmax
          if (kcs(nm) /= 0) then
             ewabr0(nm) = ewabr1(nm)
             ewave0(nm) = ewave1(nm)
             eroll0(nm) = eroll1(nm)
          endif
       enddo
    endif
end subroutine f0isf1
