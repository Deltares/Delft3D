subroutine chkdry(j         ,nmmaxj    ,nmmax     ,kmax      ,lsec      , &
                & lsecfl    ,lstsci    ,ltur      ,icx       ,icy       , &
                & kcu       ,kcv       ,kcs       ,kfu       , &
                & kfv       ,kfs       ,kspu      ,kspv      ,dpu       , &
                & dpv       ,hu        ,hv        ,hkru      ,hkrv      , &
                & thick     ,s1        ,dps       ,u1        ,v1        , &
                & umean     ,vmean     ,r1        ,rtur1     ,guu       , &
                & gvv       ,qxk       ,qyk       ,gdp       )
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
!    Function: - Initiates the depth values at velocity points
!                and check drying on velocity points
!              - Calculate umean and vmean (depth averaged
!                velocities)
!              - Redefine s1, u1, v1 and r1 according to new
!                values of mask arrays
!              - Calculate qxk and qyk
! Method used:
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
    integer              , pointer :: lundia
    integer, dimension(:), pointer :: kfst0
    real(fp)             , pointer :: dryflc
    logical              , pointer :: temp
    logical              , pointer :: zmodel
    logical              , pointer :: kfuv_from_restart
    character(256)       , pointer :: restid
    integer              , pointer :: nofou
    integer, dimension(:), pointer :: foumask
!
! Global variables
!
    integer                                                 , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                 , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                               :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                                               :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: lsec   !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: lsecfl !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                               :: nmmax  !  Description and declaration in dimens.igs
    integer                                                               :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                     :: kspu   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                     :: kspv   !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: dpv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hkru   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hkrv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: vmean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur)              :: rtur1  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                               , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                             :: k           ! Help var.
    integer                             :: l           ! Help var.
    integer                             :: ndm         ! Help var. NM-ICY
    integer                             :: nm          ! Help var. loops 1,nmmax and j,nmmaxj
    integer                             :: num         ! Help var. NM+ICY
    integer                             :: nmu         ! Help var. nm+icx
    integer                             :: nmd         ! Help var. nm-icx
    integer                             :: nm_pos      ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
    integer , dimension(:), allocatable :: mask        ! temporary array for masking flow arrays
    real(fp)                            :: hucres
    real(fp)                            :: hvcres
!
!! executable statements -------------------------------------------------------
!
    lundia             => gdp%gdinout%lundia
    kfst0              => gdp%gdpostpr%kfst0
    temp               => gdp%gdprocs%temp
    zmodel             => gdp%gdprocs%zmodel
    dryflc             => gdp%gdnumeco%dryflc
    kfuv_from_restart  => gdp%gdrestart%kfuv_from_restart
    restid             => gdp%gdrestart%restid
    nofou              => gdp%d%nofou
    foumask            => gdp%gdfourier%foumask
    !
    nm_pos             =  1
    !
    if ( restid .ne. 'STATE' ) then
       do nm = j, nmmaxj
          kfs(nm) = max(min(1, kcs(nm)),0)
          if (kfuv_from_restart) then
             if (kcu(nm)==0) kfu(nm) = 0
             if (kcv(nm)==0) kfv(nm) = 0
          else
             kfu(nm) = min(1, kcu(nm))
             kfv(nm) = min(1, kcv(nm))
          endif
       enddo
    endif
    !
    ! Apply boundary condition d()/dn=0 at open boundaries for waterlevel points
    ! if the simulation is NOT a restart
    ! first apply boundary condition in horizontal direction then in vertical
    ! direction, that is because uzd is first applied in vertical direction
    !
    if (restid == ' ') then
        do nm = 1, nmmax
           nmd = nm - icx
           nmu = nm + icx
           ndm = nm - icy
           num = nm + icy
           !
           !   Boundary at left side
           !
           if (kcs(nmd)==2 .and. kcs(nm)==1) then
              s1(nmd) = s1(nm)
              do k = 1, kmax
                 do l = 1, lstsci
                   r1(nmd, k, l) = r1(nm, k, l)
                 enddo
              enddo
              do k = 0, kmax
                 do l = 1, ltur
                    rtur1(nmd, k, l) = rtur1(nm, k, l)
                 enddo
              enddo
           endif
           !
           !   Boundary at right side
           !
           if (kcs(nm)==1 .and. kcs(nmu)==2) then
              s1(nmu) = s1(nm)
              do k = 1, kmax
                 do l = 1, lstsci
                   r1(nmu, k, l) = r1(nm, k, l)
                 enddo
              enddo
              do k = 0, kmax
                 do l = 1, ltur
                    rtur1(nmu, k, l) = rtur1(nm, k, l)
                 enddo
              enddo
           endif
           !
           !   Boundary at bottom side
           !
           if (kcs(ndm)==2 .and. kcs(nm)==1) then
              s1(ndm) = s1(nm)
              do k = 1, kmax
                 do l = 1, lstsci
                   r1(ndm, k, l) = r1(nm, k, l)
                 enddo
              enddo
              do k = 0, kmax
                 do l = 1, ltur
                    rtur1(ndm, k, l) = rtur1(nm, k, l)
                 enddo
              enddo
           endif
           !
           !   Boundary at top side
           !
           if (kcs(nm)==1 .and. kcs(num)==2) then
              s1(num) = s1(nm)
              do k = 1, kmax
                 do l = 1, lstsci
                   r1(num, k, l) = r1(nm, k, l)
                 enddo
              enddo
              do k = 0, kmax
                 do l = 1, ltur
                    rtur1(num, k, l) = rtur1(nm, k, l)
                 enddo
              enddo
           endif
        enddo
    endif
    !
    ! redefine S1 in case they are smaller then DPS and reset the mask
    ! arrays KFU,KFV and KFS
    ! -icx := -1 in m-direction, -icy := -1 in n-direction
    !
    if (restid .ne. 'STATE') then
       do nm = 1, nmmax
          nmd = nm - icx
          ndm = nm - icy
          if (kcs(nm)>0) then
             if (s1(nm)<= - real(dps(nm),fp)) then
                s1(nm)   = -real(dps(nm),fp)
                kfu(nm)  = 0
                kfu(nmd) = 0
                kfv(nm)  = 0
                kfv(ndm) = 0
                kfs(nm)  = 0
             endif
          endif
       enddo
    endif
    !
    ! Delft3D-16494: NOT NECESSARY? Could the loop above for determining kfs also be done with kcs/=0?
    ! otherwise also need to communicate kfu/kfv and s1?
    !
    ! exchange mask array kfs with neighbours for parallel runs
    !
    call dfexchg ( kfs, 1, 1, dfint, nm_pos, gdp )
    !
    ! initialize global arrays
    ! (HU, HV, QXK and QYK are initialized in esm_alloc_real)
    !
    do nm = j, nmmaxj
       umean(nm) = 0.0
       vmean(nm) = 0.0
       do k = 1, kmax
          umean(nm) = umean(nm) + thick(k)*u1(nm, k)
          vmean(nm) = vmean(nm) + thick(k)*v1(nm, k)
       enddo
    enddo
    !
    ! calculate HU and HV
    !
    call upwhu(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
             & zmodel    ,kcs       ,kcu       ,kspu      ,dps       , &
             & s1        ,dpu       ,umean     ,hu        ,gdp       )
    call upwhu(j         ,nmmaxj    ,nmmax     ,kmax      ,icy       , &
             & zmodel    ,kcs       ,kcv       ,kspv      ,dps       , &
             & s1        ,dpv       ,vmean     ,hv        ,gdp       )
    !
    ! check for dry velocity points
    ! Approach for 2D weirs (following WAQUA)
    ! HUCRES is initially set to extreme large value to guarantee
    ! the MIN operator works as planned
    !
    if (restid .ne. 'STATE') then
       do nm = 1, nmmax
          hucres = 1E9
          if (abs(kspu(nm, 0))==9) then
             if (umean(nm)>=0.001) then
                hucres = s1(nm) + hkru(nm)
             elseif (umean(nm)<= - 0.001) then
                hucres = s1(nm + icx) + hkru(nm)
             else
                hucres = max(s1(nm + icx), s1(nm)) + hkru(nm)
             endif
          endif
          !
          hvcres = 1E9
          if (abs(kspv(nm, 0))==9) then
             if (vmean(nm)>=0.001) then
                hvcres = s1(nm) + hkrv(nm)
             elseif (vmean(nm)<= - 0.001) then
                hvcres = s1(nm + icy) + hkrv(nm)
             else
                hvcres = max(s1(nm + icy), s1(nm)) + hkrv(nm)
             endif
          endif
          !
          if (.not.kfuv_from_restart) then
             if (kfu(nm)*min(hu(nm), hucres)<dryflc .and. kcu(nm)*kfu(nm)==1) then
                kfu(nm) = 0
             endif
             if (kfv(nm)*min(hv(nm), hvcres)<dryflc .and. kcv(nm)*kfv(nm)==1) then
                kfv(nm) = 0
             endif
          endif
       enddo
       !
       ! Delft3D-16494: NOT NECESSARY?
       !
       ! Exchange mask arrays kfu and kfv with neighbours for parallel runs
       !
       call dfexchg ( kfu, 1, 1, dfint, nm_pos, gdp )
       call dfexchg ( kfv, 1, 1, dfint, nm_pos, gdp )
    endif
    !
    ! mask initial arrays
    ! Note: for parallel runs, a temporary array is allocated for masking
    !
    allocate(mask(gdp%d%nmlb:gdp%d%nmub))
    mask(:) = min(1, abs(kcs(:)))
    !
    ! Delft3D-16494: NOT NECESSARY?
    !
    call dfexchg ( mask, 1, 1, dfint, nm_pos, gdp )
    !
    ! arrays s1, u1, v1, r1 and rtur1 can be computed redundantly to
    ! avoid communication at coupling interfaces
    !
    do nm = 1, nmmax
       s1(nm) = s1(nm)*mask(nm)
       do k = 1, kmax
          u1(nm, k) = u1(nm, k)*kfu(nm)
          v1(nm, k) = v1(nm, k)*kfv(nm)
          do l = 1, lstsci
             r1(nm, k, l) = r1(nm, k, l)*mask(nm)
          enddo
       enddo
       do k = 0, kmax
          do l = 1, ltur
             rtur1(nm, k, l) = rtur1(nm, k, l)*mask(nm)
          enddo
       enddo
    enddo
    deallocate(mask)
    !
    ! Correction for temporarily dry points (secondary flow only)
    ! Note: redundant at coupling interfaces for parallel runs
    !
    if (lsec>0) then
       do nm = 1, nmmax
          do k = 1, kmax
             if (kfs(nm) == 0) then
                r1(nm, k, lsecfl) = 0.0
             endif
          enddo
       enddo
    endif
    !
    ! calculate flows in x- and y- direction
    ! Note: redundant at coupling interfaces for parallel runs
    !
    do nm = 1, nmmax
       do k = 1, kmax
          qxk(nm, k) = guu(nm)*hu(nm)*thick(k)*u1(nm, k)
          qyk(nm, k) = gvv(nm)*hv(nm)*thick(k)*v1(nm, k)
       enddo
    enddo
    !
    ! set KFS to 0 if the surrounding velocity points are dry
    ! -icx := -1 in m-direction, -icy := -1 in n-direction
    !
    do nm = 1, nmmax
       if (kcs(nm)>0) then
          nmd = nm - icx
          ndm = nm - icy
          kfs(nm) = max(kfu(nm), kfu(nmd), kfv(nm), kfv(ndm))
          if (nofou > 0) then
             if (maxval(foumask) == 1) then
                kfst0(nm) = kfs(nm)
             endif
          endif
       endif
    enddo
    !
    ! Delft3D-16494: NOT NECESSARY?
    !
    ! Exchange mask array kfs with neighbours for parallel runs
    !
    call dfexchg ( kfs, 1, 1, dfint, nm_pos, gdp )
end subroutine chkdry
