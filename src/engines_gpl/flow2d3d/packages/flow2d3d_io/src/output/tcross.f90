subroutine tcross(dtsec     ,prshis    ,selhis    ,ntruv     ,ntru      , &
                & lstsci    ,nmaxus    ,nmax      ,mmax      ,kmax      , &
                & kfu       ,kfv       ,ctr       ,fltr      , &
                & atr       ,dtr       ,guu       ,gvv       ,guv       , &
                & gvu       ,thick     ,r1        ,qxk       ,qyk       , &
                & hu        ,hv        ,dicuv     ,lsed      ,lsedtot   , &
                & sbtr      ,sstr      ,sbtrc     ,sstrc     ,sbuu      , &
                & sbvv      ,gdp       )
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
!    Function: Calculates and / or saves at cross sections :
!              - Cumulative volume and momentary flow transport
!              - Cumulative advective constituent transport
!              - Cumulative diffusive constituent transport
!              - Cumulative suspended sediment transport
!              - Cumulative bed-load sediment transport
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
    integer , dimension(:, :)            , pointer :: mnit
    integer                              , pointer :: mfg
    integer                              , pointer :: mlg
    integer                              , pointer :: nfg
    integer                              , pointer :: nlg
    integer                              , pointer :: mmaxgl
    integer                              , pointer :: nmaxgl
    real(fp), dimension(:,:)             , pointer :: ssuu
    real(fp), dimension(:,:)             , pointer :: ssvv
!
! Global variables
!
    integer                                                                           , intent(in) :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in) :: lsed    !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in) :: lsedtot !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in) :: lstsci  !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in) :: mmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                                                        :: nmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in) :: nmaxus  !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in) :: ntru    !  Description and declaration in dimens.igs
    integer                                                                           , intent(in) :: ntruv   !  Description and declaration in dimens.igs
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in) :: kfu     !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in) :: kfv     !  Description and declaration in esm_alloc_int.f90
    real(fp)                                                                          , intent(in) :: dtsec   !!  DT in seconds
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in) :: guu     !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in) :: guv     !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in) :: gvu     !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in) :: gvv     !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in) :: hu      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in) :: hv      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax + 2)     , intent(in) :: dicuv   !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in) :: qxk     !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in) :: qyk     !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci) , intent(in) :: r1      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot)      , intent(in) :: sbuu    !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot)      , intent(in) :: sbvv    !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(kmax)                                                   , intent(in) :: thick   !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(ntruv)                                                               :: ctr     !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(ntruv)                                                               :: fltr    !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(ntruv, lsedtot)                                                      :: sbtr    !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(ntruv, lsedtot)                                                      :: sbtrc   !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(ntruv, lsed)                                                         :: sstr    !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(ntruv, lsed)                                                         :: sstrc   !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(ntruv, lstsci)                                                       :: atr     !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(ntruv, lstsci)                                                       :: dtr     !  Description and declaration in esm_alloc_real.f90
    character(23)                                                                     , intent(in) :: prshis  !  Description and declaration in tricom.igs
    character(23)                                                                     , intent(in) :: selhis  !  Description and declaration in tricom.igs
!
! Local variables
!
    integer  :: i      ! Help var. 
    integer  :: k      ! Help var. 
    integer  :: l      ! Help var. 
    integer  :: ls
    integer  :: m      ! Help var. counter for array index in the X-/M-direction 
    integer  :: mend   ! X-/M-Coordinate of the last  point in the cross section 
    integer  :: mmm    ! Help var. minimum of msta and mend 
    integer  :: mmx    ! Help var. maximum of msta and mend 
    integer  :: msta   ! X-/M-Coordinate of the first point in the cross section 
    integer  :: mu     ! M+1 
    integer  :: n      ! Help var. counter for array index in the Y-/N-direction 
    integer  :: nend   ! Y-/N-Coordinate of the last  point in the cross section 
    integer  :: nnm    ! Help var. minimum of nsta and nend 
    integer  :: nnx    ! Help var. maximum of nsta and nend 
    integer  :: nm     ! Help var.
    integer  :: nsta   ! Y-/N-Coordinate of the first point in the cross section 
    integer  :: nu     ! N+1 
    real(fp) :: dfcatu ! Help var. to determine the local Diffusion coeff. defined at u-point 
    real(fp) :: dfcatv ! Help var. to determine the local Diffusion coeff. defined at v-point 
    real(fp) :: facatr ! Help var. for determination of the local advective transport term (FLOW*DTSEC) 
    real(fp) :: facdtr ! Help var. for determination of the local diffusive transport term (HU*GUU*THICK*DTSEC*DFCATU/V) 
    real(fp) :: rp1    ! Help var. for the concentration array R1 (N ,M ,K ,L ) 
    real(fp) :: rp2    ! Help var. for the concentration array R1 (N ,MU,K ,L ) or array R1 (NU,M ,K ,L ) 
!
!! executable statements -------------------------------------------------------
!
    mnit                => gdp%gdstations%mnit
    mfg                 => gdp%gdparall%mfg
    mlg                 => gdp%gdparall%mlg
    nfg                 => gdp%gdparall%nfg
    nlg                 => gdp%gdparall%nlg
    mmaxgl              => gdp%gdparall%mmaxgl
    nmaxgl              => gdp%gdparall%nmaxgl
    ssuu                => gdp%gderosed%e_ssn
    ssvv                => gdp%gderosed%e_sst
    !
    ! Flows flux
    !
    if (index(selhis(20:21), 'Y')/=0 .or. index(prshis(20:21), 'Y')/=0) then
       !
       ! U-transport section
       !
       do i = 1, ntru
          m      = mnit(1, i)
          nsta   = mnit(2, i)
          nend   = mnit(4, i)
          nnm    = min(nsta, nend)
          nnx    = max(nsta, nend)
          if (nfg == 1) then
             nnm = max(nnm, 1)
          else
             nnm = max(nnm, ihalon + 1)
          endif
          if (nlg == nmaxgl) then
             nnx = min(nnx, nlg - nfg + 1)
          else
             nnx = min(nnx, nlg - nfg + 1 - ihalon)
          endif
          mu     = min(mmax, m + 1)
          ctr(i) = 0.0
          !
          ! in the parallel case, sections that are entirely in the halo regions
          ! (low end halo or high end halo of partition) are discarded  from calculation not to be
          ! counted twice; these sections are accounted for in the neighbouring partition
          !
          if (.not.(mnit(1,i)==mnit(3,i)                                 &
                    .and. (    (mfg/=1      .and. mnit(1,i)<=ihalom    ) &
                           .or.(mlg/=mmaxgl .and. mnit(1,i)>mmax-ihalom)))) then
             do n = nnm, nnx
                !
                ! For in-active (dry) grid points in the section
                ! there will be no transport => kfu = 0
                !
                if (kfu(n,m) /= 0) then
                   do k = 1, kmax
                      ctr(i) = ctr(i) + qxk(n, m, k)
                   enddo
                endif
             enddo
          endif
          fltr(i) = fltr(i) + dtsec*ctr(i)
       enddo
       !
       ! V-transport sections
       !
       do i = ntru + 1, ntruv
          n      = mnit(2, i)
          msta   = mnit(1, i)
          mend   = mnit(3, i)
          mmm    = min(msta, mend)
          if (mfg == 1) then
             mmm = max(mmm, 1)
          else
             mmm = max(mmm, ihalom + 1)
          endif
          mmx    = max(msta, mend)
          if (mlg == mmaxgl) then
             mmx = min(mmx, mlg - mfg + 1)
          else
             mmx = min(mmx, mlg - mfg + 1 - ihalom)
          endif
          nu     = min(nmaxus, n + 1)
          ctr(i) = 0.0
          !
          ! in the parallel case, sections that are entirely in the halo regions
          ! (low end halo or high end halo of partition) are discarded  from calculation not to be
          ! counted twice; these sections are accounted for in the neighbouring partition
          !
          if (.not.(mnit(2,i)==mnit(4,i)                                 &
                    .and. (    (nfg/=1      .and. mnit(2,i)<=ihalon    ) &
                           .or.(nlg/=nmaxgl .and. mnit(2,i)>nmaxus-ihalon)))) then
             do m = mmm, mmx
                !
                ! For in-active (dry) grid points in the section
                ! there will be no transport => kfv = 0
                !
                if (kfv(n,m) /= 0) then
                   do k = 1, kmax
                      ctr(i) = ctr(i) + qyk(n, m, k)
                   enddo
                endif
             enddo
          endif
          fltr(i) = fltr(i) + dtsec*ctr(i)
       enddo
    endif
    !
    ! Advective / Diffusivity fluxes
    !
    if (index(selhis(22:23), 'Y')/=0 .or. index(prshis(22:23), 'Y')/=0) then
       !
       ! U-transport sections
       !
       do i = 1, ntru
          m    = mnit(1, i)
          nsta = mnit(2, i)
          nend = mnit(4, i)
          nnm  = min(nsta, nend)
          nnx  = max(nsta, nend)
          if (nfg == 1) then
             nnm = max(nnm, 1)
          else
             nnm = max(nnm, ihalon + 1)
          endif
          if (nlg == nmaxgl) then
             nnx = min(nnx, nlg - nfg + 1)
          else
             nnx = min(nnx, nlg - nfg + 1 - ihalon)
          endif
          mu   = min(mmax, m + 1)
          do ls = 1, lsedtot
             sbtr(i, ls) = 0.0
          enddo
          do ls = 1, lsed
             sstr(i, ls) = 0.0
          enddo
          !
          ! in the parallel case, sections that are entirely in the halo regions
          ! (low end halo or high end halo of partition) are discarded  from calculation not to be
          ! counted twice; these sections are accounted for in the neighbouring partition
          !
          if (.not.(mnit(1,i)==mnit(3,i)                                 &
                    .and. (    (mfg/=1      .and. mnit(1,i)<=ihalom    ) &
                           .or.(mlg/=mmaxgl .and. mnit(1,i)>mmax-ihalom)))) then
             do n = nnm, nnx
                !
                ! For in-active (dry) grid points in the section
                ! there will be no transport => kfu = 0
                !
                if (kfu(n,m) /= 0) then
                   do k = 1, kmax
                      dfcatu = 0.5*(dicuv(n, m, k) + dicuv(n, mu, k))
                      facdtr = dtsec*hu(n, m)*guu(n, m)*thick(k)*dfcatu
                      facatr = dtsec*qxk(n, m, k)
                      do l = 1, lstsci
                         rp1       = r1(n, m, k, l)
                         rp2       = r1(n, mu, k, l)
                         atr(i, l) = atr(i, l) + facatr*0.5*(rp1 + rp2)
                         dtr(i, l) = dtr(i, l) + facdtr*(rp1 - rp2)/gvu(n, m)
                      enddo
                   enddo
                   !
                   ! integrate bed-load and suspended-load sediment
                   ! transport over cross section:
                   ! - units of sbtr and sstr are KG/S
                   ! - units of sbtrc and sstrc are KG
                   !
                   call n_and_m_to_nm(n, m, nm, gdp)
                   do ls = 1, lsedtot
                      sbtr(i, ls) = sbtr(i, ls) + sbuu(n, m, ls)*guu(n, m)
                   enddo
                   do ls = 1, lsed
                      sstr(i, ls) = sstr(i, ls) + ssuu(nm, ls)*guu(n, m)
                   enddo
                endif
             enddo
          endif
          !
          ! integrate over time and add to cumulative transports
          !
          do ls = 1, lsedtot
             sbtrc(i, ls) = sbtrc(i, ls) + sbtr(i, ls)*dtsec
          enddo
          do ls = 1, lsed
             sstrc(i, ls) = sstrc(i, ls) + sstr(i, ls)*dtsec
          enddo
       enddo
       !
       ! V-transport sections
       !
       do i = ntru + 1, ntruv
          n    = mnit(2, i)
          msta = mnit(1, i)
          mend = mnit(3, i)
          mmm  = min(msta, mend)
          mmx  = max(msta, mend)
          if (mfg == 1) then
             mmm = max(mmm, 1)
          else
             mmm = max(mmm, ihalom + 1)
          endif
          mmx    = max(msta, mend)
          if (mlg == mmaxgl) then
             mmx = min(mmx, mlg - mfg + 1)
          else
             mmx = min(mmx, mlg - mfg + 1 - ihalom)
          endif
          nu   = min(nmaxus, n + 1)
          do ls = 1, lsedtot
             sbtr(i, ls) = 0.0
          enddo
          do ls = 1, lsed
             sstr(i, ls) = 0.0
          enddo
          !
          ! in the parallel case, sections that are entirely in the halo regions
          ! (low end halo or high end halo of partition) are discarded  from calculation not to be
          ! counted twice; these sections are accounted for in the neighbouring partition
          !
          if (.not.(mnit(2,i)==mnit(4,i)                                 &
                    .and. (    (nfg/=1      .and. mnit(2,i)<=ihalon    ) &
                           .or.(nlg/=nmaxgl .and. mnit(2,i)>nmaxus-ihalon)))) then
             do m = mmm, mmx
                !
                ! For in-active (dry) grid points in the section
                ! there will be no transport => kfv = 0
                !
                if (kfv(n,m) /= 0) then
                   do k = 1, kmax
                      dfcatv = 0.5*(dicuv(n, m, k) + dicuv(nu, m, k))
                      facdtr = dtsec*hv(n, m)*gvv(n, m)*thick(k)*dfcatv
                      facatr = dtsec*qyk(n, m, k)
                      do l = 1, lstsci
                         rp1       = r1(n, m, k, l)
                         rp2       = r1(nu, m, k, l)
                         atr(i, l) = atr(i, l) + facatr*0.5*(rp1 + rp2)
                         dtr(i, l) = dtr(i, l) + facdtr*(rp1 - rp2)/guv(n, m)
                      enddo
                   enddo
                   !
                   ! integrate bed-load and suspended-load sediment
                   ! transport over cross section
                   ! - units of sbtr and sstr are KG/S
                   ! - note: units of sbtrc and sstrc are KG
                   !
                   call n_and_m_to_nm(n, m, nm, gdp)
                   do ls = 1, lsedtot
                      sbtr(i, ls) = sbtr(i, ls) + sbvv(n, m, ls)*gvv(n, m)
                   enddo
                   do ls = 1, lsed
                      sstr(i, ls) = sstr(i, ls) + ssvv(nm, ls)*gvv(n, m)
                   enddo
                endif
             enddo
          endif
          !
          ! integrate over time and add to cumulative transports
          !
          do ls = 1, lsedtot
             sbtrc(i, ls) = sbtrc(i, ls) + sbtr(i, ls)*dtsec
          enddo
          do ls = 1, lsed
             sstrc(i, ls) = sstrc(i, ls) + sstr(i, ls)*dtsec
          enddo
       enddo
    endif
end subroutine tcross
