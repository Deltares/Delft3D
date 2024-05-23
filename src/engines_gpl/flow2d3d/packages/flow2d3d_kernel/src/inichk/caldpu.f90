subroutine caldpu(lundia    ,mmax      ,nmaxus    ,kmax      , &
                & zmodel    , &
                & kcs       ,kcu       ,kcv       , &
                & kspu      ,kspv      ,hkru      ,hkrv      , &
                & umean     ,vmean     ,dpd       ,dpu       ,dpv       , &
                & dps       ,dzs1      ,u1        ,v1        ,s1        , &
                & thick     ,gdp       )
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
!    Function: - Computes depth at vel. points
!              - if general local weir then DPU/V contains crest
!                height then limit DPU/V to local depth values
!              - if 2Dweir for river application then limit
!                HKRU/V to local depth values
!              *** Use upwind method if sediment is present and
!                  bedupd is .true. ***
!
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
    real(fp)     , pointer :: bed
    character(8) , pointer :: dpuopt
!
! Global variables
!
    integer                                                                , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                              :: lundia !  Description and declaration in inout.igs
    integer                                                                , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax), intent(in)  :: kspu   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax), intent(in)  :: kspv   !  Description and declaration in esm_alloc_int.f90
    logical                                                                , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: dpd    !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: dpv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: hkru   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: hkrv   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: vmean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:kmax), intent(in)  :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:kmax), intent(in)  :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)  , intent(in)  :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                                            , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer       :: ddb
    integer       :: k
    integer       :: m      ! Help var. do loop MMAX 
    integer       :: md     ! Help var. M-1 
    integer       :: mu
    integer       :: n      ! Help var. do loop NMAX 
    integer       :: nd     ! Help var. N-1 
    integer       :: nu
    real(fp)      :: dpuw
    real(fp)      :: dpvw
    character(25) :: errms1 ! Help. var. for error message 
    character(25) :: errms2 ! Help. var. for error message 
    integer       :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    dpuopt              => gdp%gdnumeco%dpuopt
    bed                 => gdp%gdmorpar%bed
    !
    errms1 = '(      ,      ) in U-dir.'
    errms2 = '(      ,      ) in V-dir.'
    !
    ddb    = gdp%d%ddbound
    nm_pos = 1
    !
    !   Determine UMEAN and VMEAN
    !
    !   Note: in case of parallel runs, s1, u1 and v1 are also available in halo area
    !         (see rdic), so no exchanging with neighbours is needed, i.e.
    !         redundant for points in halo area at coupling interfaces
    !
    do n = 1 - ddb, nmaxus
       do m = 1 - ddb, mmax
          umean(n, m) = 0.0
          do k = 1, kmax
             if (zmodel) then
                umean(n, m) = umean(n, m) + dzs1(n, m, k)*u1(n, m, k)           &
                            & /max( (real(dps(n, m),fp) + s1(n, m)), 0.01_fp )
             else
                umean(n, m) = umean(n, m) + thick(k)*u1(n, m, k)
             endif
          enddo
          vmean(n, m) = 0.0
          do k = 1, kmax
             if (zmodel) then
                vmean(n, m) = vmean(n, m) + dzs1(n, m, k)*v1(n, m, k)           &
                            & /max( (real(dps(n, m),fp) + s1(n, m)), 0.01_fp )
             else
                vmean(n, m) = vmean(n, m) + thick(k)*v1(n, m, k)
             endif
          enddo
       enddo
    enddo
    !
    !   For Z-model MIN or UPW required
    !
    if (dpuopt=='UPW') then
       do n = 1 - ddb, nmaxus
          do m = 1 - ddb, mmax
             nu = min(n + 1, nmaxus)
             mu = min(m + 1, mmax)
             !
             if (kcu(n, m)==1) then
                if (umean(n, m)>=0.001) then
                   dpu(n, m) = real(dps(n, m),fp)
                elseif (umean(n, m)<= - 0.001) then
                   dpu(n, m) = real(dps(n, mu),fp)
                else
                   dpu(n, m) = min(real(dps(n, mu),fp), real(dps(n, m),fp))
                endif
             endif
             !
             if (kcv(n, m)==1) then
                if (vmean(n, m)>=0.001) then
                   dpv(n, m) = real(dps(n, m),fp)
                elseif (vmean(n, m)<= - 0.001) then
                   dpv(n, m) = real(dps(nu, m),fp)
                else
                   dpv(n, m) = min(real(dps(nu, m),fp), real(dps(n, m),fp))
                endif
             endif
          enddo
       enddo
    !
    !    For Drying and flooding of areas when DPSOPT = DP, like in India -
    !    Use DPUOPT = MIN(DPS(nm), DPS(N,MU))
    !-----IF KSPU(nm,0) = 3
    !        DPU already filled with Crest height (see call STRFIL in RDSTRU)
    !        hence now only limit DPU if crest depth > bottom depth
    !     ELSE
    !         Compute DPU as min of DPS
    !         IF KSPU(n,m,0) = 9
    !               HKRU already filled with Crest height (see call STRFIL in RDSTRU)
    !               hence now only limit HKRU if crest height > bottom depth
    !
    elseif (dpuopt=='MIN') then
       do n = 1 - ddb, nmaxus
          do m = 1 - ddb, mmax
             mu = min(m + 1, mmax)
             nu = min(n + 1, nmaxus)
             if (kcu(n, m)==1) then
                dpuw = min(real(dps(n, mu),fp), real(dps(n, m),fp))
                if (abs(kspu(n, m, 0))==3) then
                   if (dpu(n, m) > dpuw) then
                      hkru(n, m) = dpuw
                      dpu(n, m)  = dpuw
                      write (errms1(2:7) , '(i6)') m
                      write (errms1(9:14), '(i6)') n
                      call prterr(lundia    ,'V058'    ,errms1    )
                   else
                      hkru(n, m) = dpu(n, m)
                   endif
                else
                   dpu(n, m) = dpuw
                   if (abs(kspu(n, m, 0))==9 .and. hkru(n, m)>dpuw) then
                      hkru(n, m) = dpuw
                      write (errms1(2:7) , '(i6)') m
                      write (errms1(9:14), '(i6)') n
                      call prterr(lundia    ,'V058'    ,errms1    )
                   endif
                endif
             endif
             !
             if (kcv(n, m)==1) then
                dpvw = min(real(dps(nu, m),fp), real(dps(n, m),fp))
                if (abs(kspv(n, m, 0))==3) then
                   if (dpv(n, m) > dpvw) then
                      hkrv(n, m) = dpvw
                      dpv(n, m)  = dpvw
                      write (errms2(2:7) , '(i6)') m
                      write (errms2(9:14), '(i6)') n
                      call prterr(lundia    ,'V058'    ,errms2    )
                   else
                      hkrv(n, m) = dpv(n, m)
                   endif
                else
                   dpv(n, m) = dpvw
                   if (abs(kspv(n, m, 0))==9 .and. hkrv(n, m)>dpvw) then
                      hkrv(n, m) = dpvw
                      write (errms2(2:7) , '(i6)') m
                      write (errms2(9:14), '(i6)') n
                      call prterr(lundia    ,'V058'    ,errms2    )
                   endif
                endif
             endif
          enddo
       enddo
    elseif (dpuopt=='MEAN_DPS') then
       do n = 1 - ddb, nmaxus
          do m = 1 - ddb, mmax
             mu = min(m + 1, mmax)
             nu = min(n + 1, nmaxus)
             if (kcu(n, m)==1) then
                dpuw = 0.5_fp*real(dps(n, mu) + dps(n, m),fp)
                if (abs(kspu(n, m, 0))==3) then
                   if (dpu(n, m) > dpuw) then
                      hkru(n, m) = dpuw
                      dpu(n, m)  = dpuw
                      write (errms1(2:7) , '(i6)') m
                      write (errms1(9:14), '(i6)') n
                      call prterr(lundia    ,'V058'    ,errms1    )
                   else
                      hkru(n, m) = dpu(n, m)
                   endif
                else
                   dpu(n, m) = dpuw
                   if (abs(kspu(n, m, 0))==9 .and. hkru(n, m)>dpuw) then
                      hkru(n, m) = dpuw
                      write (errms1(2:7) , '(i6)') m
                      write (errms1(9:14), '(i6)') n
                      call prterr(lundia    ,'V058'    ,errms1    )
                   endif
                endif
             endif
             !
             if (kcv(n, m)==1) then
                dpvw = 0.5_fp*real(dps(nu, m) + dps(n, m),fp)
                if (abs(kspv(n, m, 0))==3) then
                   if (dpv(n, m) > dpvw) then
                      hkrv(n, m) = dpvw
                      dpv(n, m)  = dpvw
                      write (errms2(2:7) , '(i6)') m
                      write (errms2(9:14), '(i6)') n
                      call prterr(lundia    ,'V058'    ,errms2    )
                   else
                      hkrv(n, m) = dpv(n, m)
                   endif
                else
                   dpv(n, m) = dpvw
                   if (abs(kspv(n, m, 0))==9 .and. hkrv(n, m)>dpvw) then
                      hkrv(n, m) = dpvw
                      write (errms2(2:7) , '(i6)') m
                      write (errms2(9:14), '(i6)') n
                      call prterr(lundia    ,'V058'    ,errms2    )
                   endif
                endif
             endif
          enddo
       enddo
    elseif (dpuopt=='MEAN') then
       !
       !     standard option = MEAN
       !-----IF KSPU(nm,0) = 3
       !        DPU already filled with Crest height (see call STRFIL in RDSTRU)
       !        hence now only limit DPU if crest depth > bottom depth
       !     ELSE
       !         Compute DPU from averaged DPD at velocity point
       !         IF KSPU(n,m,0) = 9
       !               HKRU already filled with Crest height (see call STRFIL in RDSTRU)
       !               hence now only limit HKRU if crest height > bottom depth
       !
       do n = 1 - ddb, nmaxus
          do m = 1 - ddb, mmax
             nd = max(n - 1, 1 - ddb)
             md = max(m - 1, 1 - ddb)
             write (errms1(2:7), '(i6)') m
             write (errms1(9:14), '(i6)') n
             errms2(:14) = errms1(:14)
             if (kcu(n, m)==1) then
                dpuw = .5*(dpd(n, m) + dpd(nd, m))
                if (abs(kspu(n, m, 0))==3) then
                   if (dpu(n, m) > dpuw) then
                      hkru(n, m) = dpuw
                      dpu (n, m) = dpuw
                      call prterr(lundia    ,'V058'    ,errms1    )
                   else
                      hkru(n, m) = dpu(n, m)
                   endif
                else
                   dpu(n, m) = dpuw
                   if (abs(kspu(n, m, 0))==9 .and. hkru(n, m)>dpuw) then
                      hkru(n, m) = dpuw
                      call prterr(lundia    ,'V058'    ,errms1    )
                   endif
                endif
             endif
             !
             if (kcv(n, m)==1) then
                dpvw = .5*(dpd(n, m) + dpd(n, md))
                if (abs(kspv(n, m, 0))==3) then
                   if (dpv(n, m) > dpvw) then
                      hkrv(n, m) = dpvw
                      dpv (n, m) = dpvw
                      call prterr(lundia    ,'V058'    ,errms2    )
                   else
                      hkrv(n, m) = dpv(n, m)
                   endif
                else
                   dpv(n, m) = dpvw
                   if (abs(kspv(n, m, 0))==9 .and. hkrv(n, m)>dpvw) then
                      hkrv(n, m) = dpvw
                      call prterr(lundia    ,'V058'    ,errms2    )
                   endif
                endif
             endif
          enddo
       enddo
    else
    endif
    call caldpu_dd(nmaxus ,mmax   ,kcs    ,kcu    ,kcv    , &
                 & umean  ,vmean  ,dpd    ,dps    ,dpu    , &
                 & dpv    ,gdp    ) 
    !
    ! exchange depths and heights with neighbours for parallel runs
    !
    call dfexchg (  dpu, 1, 1, dfloat, nm_pos, gdp )
    call dfexchg (  dpv, 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( hkru, 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( hkrv, 1, 1, dfloat, nm_pos, gdp )
end subroutine caldpu
