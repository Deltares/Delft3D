subroutine tstat_sed(nostat    ,nmax      ,mmax      ,kmax      , &
               & lsed      ,lsedtot   ,kfu       ,kfv       ,kcs       , &
               & ws        ,zbdsed    ,zrsdeq    ,zdpsed    ,zws       , &
               & zsbu      ,zsbv      ,zssu      ,zssv      ,sbuu      , &
               & sbvv      ,zrca      ,zsourse   ,zsinkse   ,zfrac     , &
               & zmudfrac  ,zsandfrac ,zfixfac   ,ztaub     ,zhidexp   , &
               & seddif    ,zseddif   ,gdp       )
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
!    Function: - Updates the monitoring station informations at
!                each time step
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    use mathconsts
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , dimension(:,:)             , pointer :: mnstat
    real(fp), dimension(:,:)             , pointer :: fixfac
    real(fp), dimension(:,:)             , pointer :: frac
    real(fp), dimension(:,:)             , pointer :: hidexp
    real(fp), dimension(:)               , pointer :: mudfrac
    real(fp), dimension(:,:)             , pointer :: rca
    real(fp), dimension(:,:)             , pointer :: rsedeq
    real(fp), dimension(:)               , pointer :: sandfrac
    real(fp), dimension(:,:)             , pointer :: ssuu
    real(fp), dimension(:,:)             , pointer :: ssvv
    real(fp), dimension(:,:)             , pointer :: sinkse
    real(fp), dimension(:,:)             , pointer :: sourse
    real(fp), dimension(:)               , pointer :: taub
!
! Global variables
!
    integer                                                                      , intent(in)  :: kmax      !< number of layers in water column
    integer                                                                      , intent(in)  :: lsed      !< number of suspended sediment fractions
    integer                                                                      , intent(in)  :: lsedtot   !< total number of sediment fractions
    integer                                                                      , intent(in)  :: mmax      !< number of gridpoints in M direction
    integer                                                                      , intent(in)  :: nmax      !< number of gridpoints in N direction
    integer                                                                      , intent(in)  :: nostat    !< number of stations
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kcs       !< flag indicating cell cell type (1 = internal, 2 = open bound, 3 = DD bound, -1 = ghost cell, 0 = inactive)
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kfu       !< flag indicating flow status of U point  (0 = dry, 1 = flowing)
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kfv       !< flag indicating flow status of V point  (0 = dry, 1 = flowing)
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, lsed), intent(in)  :: ws        !< settling velocity
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot)     , intent(in)  :: sbuu      !< bed load (due to currents and waves) plus suspended transport due to waves at U points
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot)     , intent(in)  :: sbvv      !< bed load (due to currents and waves) plus suspended transport due to waves at V points
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, lsed), intent(in)  :: seddif    !< vertical sediment diffusion
    real(fp)  , dimension(nostat)                                                , intent(out) :: zdpsed    !< thickness of sediment layer at station
    real(fp)  , dimension(nostat, 0:kmax, lsed)                                  , intent(out) :: zws       !< settling velocity at station
    real(fp)  , dimension(nostat, lsed)                                          , intent(out) :: zrsdeq    !< equilibrium sediment concentration at station
    real(fp)  , dimension(nostat, lsedtot)                                       , intent(out) :: zbdsed    !< bed composition at station
    real(fp)  , dimension(nostat, lsed)                                          , intent(out) :: zrca      !< near-bed reference concentration at station
    real(fp)  , dimension(nostat, lsedtot)                                       , intent(out) :: zsbu      !< bed load (due to currents and waves) plus suspended transport due to waves in U direction at station
    real(fp)  , dimension(nostat, lsedtot)                                       , intent(out) :: zsbv      !< bed load (due to currents and waves) plus suspended transport due to waves in V direction at station
    real(fp)  , dimension(nostat, lsed)                                          , intent(out) :: zssu      !< suspended transport due to currents in U direction at station
    real(fp)  , dimension(nostat, lsed)                                          , intent(out) :: zssv      !< suspended transport due to currents in V direction at station
    real(fp), dimension(nostat, lsed)                                            , intent(out) :: zsourse   !< suspended sediment sourse term at station
    real(fp), dimension(nostat, lsed)                                            , intent(out) :: zsinkse   !< suspended sediment sink term at station
    real(fp), dimension(nostat, lsedtot)                                         , intent(out) :: zfrac     !< sediment fraction in top layer at station
    real(fp), dimension(nostat)                                                  , intent(out) :: zmudfrac  !< total mud fraction in top layer at station
    real(fp), dimension(nostat)                                                  , intent(out) :: zsandfrac !< total sand fraction in top layer at station
    real(fp), dimension(nostat, lsedtot)                                         , intent(out) :: zfixfac   !< reduction factor due to limited sediment availability at station
    real(fp), dimension(nostat)                                                  , intent(out) :: ztaub     !< bed shear stress used in morphology at station
    real(fp), dimension(nostat, lsedtot)                                         , intent(out) :: zhidexp   !< hiding and exposure factor at station
    real(fp), dimension(nostat, 0:kmax, lsed)                                    , intent(out) :: zseddif   !< vertical sediment diffusion profile at station
!
! Local variables
!
    integer :: ii
    integer :: k     ! Help var.
    integer :: l     ! Help var.
    integer :: m     ! Help var. counter for array index in the X-/M-direction
    integer :: md    ! M-1
    integer :: n     ! Help var. counter for array index in the Y-/N-direction
    integer :: nd    ! N-1
    integer :: ndm
    integer :: nm
    integer :: nmd
!
!! executable statements -------------------------------------------------------
!
    ! Parallel:
    ! When n,m is in the halo, kcs is -1
    ! => always use the ABSOLUTE value of kcs
    !
    if (lsedtot == 0) return
    !
    mnstat              => gdp%gdstations%mnstat
    fixfac              => gdp%gderosed%fixfac
    frac                => gdp%gderosed%frac
    hidexp              => gdp%gderosed%hidexp
    mudfrac             => gdp%gderosed%mudfrac
    rca                 => gdp%gderosed%rca
    rsedeq              => gdp%gderosed%rsedeq
    sandfrac            => gdp%gderosed%sandfrac
    sinkse              => gdp%gderosed%sinkse
    sourse              => gdp%gderosed%sourse
    ssuu                => gdp%gderosed%e_ssn
    ssvv                => gdp%gderosed%e_sst
    taub                => gdp%gderosed%taub
    !
    ! Store quantities specific for sediment in defined stations
    !
    if (lsed>0) then
       zrca    = -999.0_fp
       zsbu    = -999.0_fp
       zsbv    = -999.0_fp
       zssu    = -999.0_fp
       zssv    = -999.0_fp
       zws     = -999.0_fp
       zrsdeq  = -999.0_fp
       zsourse = -999.0_fp
       zsinkse = -999.0_fp
       zseddif = -999.0_fp
    endif
    
    zmudfrac  = -999.0_fp
    zsandfrac = -999.0_fp
    ztaub     = -999.0_fp
    !
    zfrac     = -999.0_fp
    zfixfac   = -999.0_fp
    zhidexp   = -999.0_fp
    
    do ii = 1, nostat
       m  = mnstat(1, ii)
       if (m<0) cycle
       n  = mnstat(2, ii)
       if (n<0) cycle
       md = max(1, m - 1)
       nd = max(1, n - 1)
       !
       call n_and_m_to_nm(n , m , nm , gdp)
       call n_and_m_to_nm(n , md, nmd, gdp)
       call n_and_m_to_nm(nd, m , ndm, gdp)
       !
       zmudfrac(ii)  = mudfrac(nm)
       zsandfrac(ii) = sandfrac(nm)
       ztaub(ii)     = taub(nm)
       !
       do l = 1, lsed
          zrca(ii, l)    = rca(nm, l)
          zrsdeq(ii, l)  = rsedeq(nm, l)
          zsourse(ii, l) = sourse(nm, l)
          zsinkse(ii, l) = sinkse(nm, l)
          zsbu(ii, l) = abs(kcs(n,m))                          &
                      & * (         kfu(n,m )*sbuu(n,m ,l)     &
                      &    + (m-md)*kfu(n,md)*sbuu(n,md,l) )/2.0_fp
          zsbv(ii, l) = abs(kcs(n,m))                          &
                      & * (         kfv(n ,m)*sbvv(n ,m,l)     &
                      &    + (n-nd)*kfv(nd,m)*sbvv(nd,m,l) )/2.0_fp
          zssu(ii, l) = abs(kcs(n,m))                          &
                      & * (         kfu(n,m )*ssuu(nm ,l)      &
                      &    + (m-md)*kfu(n,md)*ssuu(nmd,l) )/2.0_fp
          zssv(ii, l) = abs(kcs(n,m))                          &
                      & * (         kfv(n ,m)*ssvv(nm ,l)      &
                      &    + (n-nd)*kfv(nd,m)*ssvv(ndm,l) )/2.0_fp
          do k = 0, kmax
             zws(ii, k, l)     = ws(n, m, k, l)
             zseddif(ii, k, l) = seddif(n, m, k, l)
          enddo
       enddo
       !
       do l = 1, lsedtot
          zfrac(ii, l)   = frac(nm, l)
          zfixfac(ii, l) = fixfac(nm, l)
          zhidexp(ii, l) = hidexp(nm, l)
       enddo
    enddo
    !
    if (lsedtot>0) then
       call tstat_bed(nostat, lsedtot, nmax, zdpsed, zbdsed, gdp)
    endif
end subroutine tstat_sed
