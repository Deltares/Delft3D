subroutine iwe_00(nmax      ,mmax      ,kmax      ,kmxdt     ,npiwe     , &
                & ltur      ,lundia    ,w10mag    ,s0        ,dps       , &
                & u0        ,v0        ,dudz      ,dvdz      ,windsu    , &
                & windsv    ,taubpu    ,taubpv    ,sig       ,thick     , &
                & rich      ,bruvai    ,rtur0     ,vicww     ,kfs       , &
                & kfu       ,kfv       ,tgarkx    ,tgarkt    ,tgarnp    , &
                & tkepro    ,tkedis    ,fuiwe     ,fviwe     ,gdp       )
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
!    Function: Main routine to calculate internal wave energy
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: wvlbed
    real(fp)               , pointer :: wvlsur
    real(fp)               , pointer :: frcbed
    real(fp)               , pointer :: frcsur
    real(fp)               , pointer :: siglim
    integer                , pointer :: mwriwe
    integer                , pointer :: nwriwe
    integer                , pointer :: mdebug
    integer                , pointer :: ndebug
    logical                , pointer :: iwedia
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
!
! Global variables
! All reals   : Description and declaration in esm_alloc_real.f90
!
integer                                                                                    :: kmax   !  Description and declaration in esm_alloc_int.f90
integer                                                                                    :: kmxdt  !  Description and declaration in dimens.igs
integer                                                                      , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
integer                                                                                    :: lundia !  Description and declaration in inout.igs
integer                                                                      , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
integer                                                                      , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
integer                                                                                    :: npiwe  !  Description and declaration in dimens.igs
integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
real(fp)  , dimension(0:kmax , 15)                                                         :: tgarkx
real(fp)  , dimension(0:kmxdt, 24)                                                         :: tgarkt
real(fp)  , dimension(0:npiwe,  4)                                                         :: tgarnp
real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                            :: dps
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                            :: s0
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: taubpu
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: taubpv
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                            :: w10mag
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: windsu
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: windsv
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)      , intent(in)  :: bruvai
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)      , intent(in)  :: dudz
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)      , intent(in)  :: dvdz
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)      , intent(in)  :: rich
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)      , intent(in)  :: vicww
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, ltur), intent(in)  :: rtur0
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(out) :: fuiwe
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(out) :: fviwe
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(out) :: tkedis
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(out) :: tkepro
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: u0
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: v0
real(fp)  , dimension(kmax)                                                                :: sig
real(fp)  , dimension(kmax)                                                                :: thick
!
! Local variables
!
    integer  :: k
    integer  :: kenmu  ! Mask value maximum of 1 and KFU's for N,M and N,MD 
    integer  :: kenmv  ! Mask value maximum of 1 and KFV's for N,M and ND,M 
    integer  :: kmxt
    integer  :: m      ! Loop counter 1,NMAX 
    integer  :: md     ! MAX (1,M-1) 
    integer  :: n      ! Loop counter 1,NMAX 
    integer  :: nd     ! MAX (1,N-1) 
    integer  :: nfreqs
    logical  :: savdia ! Save value of IWEDIA, and re-use IWEDIA for writing in TAYLOR sub- routines
    real(fp) :: ustbx
    real(fp) :: ustby
    real(fp) :: ustwix
    real(fp) :: ustwiy
!
!! executable statements -------------------------------------------------------
!
    rhow     => gdp%gdphysco%rhow
    ag       => gdp%gdphysco%ag
    mwriwe   => gdp%gdiwearr%mwriwe
    nwriwe   => gdp%gdiwearr%nwriwe
    mdebug   => gdp%gdiwearr%mdebug
    ndebug   => gdp%gdiwearr%ndebug
    iwedia   => gdp%gdiwearr%iwedia
    wvlbed   => gdp%gdiwepar%wvlbed
    wvlsur   => gdp%gdiwepar%wvlsur
    frcbed   => gdp%gdiwepar%frcbed
    frcsur   => gdp%gdiwepar%frcsur
    siglim   => gdp%gdiwepar%siglim
    !
    ! Calibrate Internal Wave Energy
    !
    savdia = iwedia
    !
    do n = 1, nmax
       do m = 1, mmax
          if (kfs(n, m)==1) then
             md    = max(1, m - 1)
             nd    = max(1, n - 1)
             kenmu = max(1, kfu(n, m)*kfu(n, md))
             kenmv = max(1, kfv(n, m)*kfv(nd, m))
             !
             do k = 0, kmax
                tgarkx(k, 1) = dudz(n, m, k)
                tgarkx(k, 2) = dvdz(n, m, k)
                tgarkx(k, 3) = rich(n, m, k)
                tgarkx(k, 4) = bruvai(n, m, k)
                tgarkx(k, 5) = rtur0(n, m, k, 1)
                tgarkx(k, 6) = rtur0(n, m, k, 2)
                tgarkx(k, 7) = vicww(n, m, k)
             enddo
             do k = 1, kmax
                tgarkx(k, 8) = (u0(n, m, k)*kfu(n, m) + u0(n, md, k)*kfu(n, md))&
                             & /kenmu
                tgarkx(k, 9) = (v0(n, m, k)*kfv(n, m) + v0(nd, m, k)*kfv(nd, m))&
                             & /kenmv
             enddo
             !
             ! Notice minus-signs below because of switched signs for WINDSU/WINDSV
             ! and TAUBPU/TAUBPV, see -/+ signs in UZD which yield finally a correct result:
             !
             ustwix = -windsu(n, m)/rhow
             ustwiy = -windsv(n, m)/rhow
             ustwix = sign(sqrt(abs(ustwix)), ustwix)
             ustwiy = sign(sqrt(abs(ustwiy)), ustwiy)
             !
             ustbx = -1*(taubpu(n, m)*u0(n, m, kmax)*kfu(n, m) + taubpu(n, md)  &
                   & *u0(n, md, kmax)*kfu(n, md))/kenmu
             ustby = -1*(taubpv(n, m)*v0(n, m, kmax)*kfv(n, m) + taubpv(nd, m)  &
                   & *v0(nd, m, kmax)*kfv(nd, m))/kenmv
             ustbx = sign(sqrt(abs(ustbx)), ustbx)
             ustby = sign(sqrt(abs(ustby)), ustby)
             !
             ! Define MDEBUG and NDEBUG (in common IWEARR) for debugging
             !
             mdebug = m
             ndebug = n
             !
             ! Define IWEDIA for (M,N) = (MWRIWE,NWRIWE) for IWEDIA=.false. MWRIWE=NWRIWE=0
             !
             iwedia = (mwriwe==m) .and. (nwriwe==n)
             call taylor(kmax        , kmxdt       , kmxt        , npiwe       , nfreqs      , &
                       & lundia      , w10mag(n, m), siglim      , wvlbed      , wvlsur      , &
                       & frcbed      , frcsur      , ustbx       , ustby       , ustwix      , &
                       & ustwiy      , sig         , thick       , s0(n, m)    , dps(n, m)   , &
                       & tgarkx(0, 1), tgarkx(0, 2), tgarkx(0, 3), tgarkx(0, 4), tgarkx(0, 5), &
                       & tgarkx(0, 7), tgarkx(1, 8), tgarkx(1, 9), tgarkx(1,10), tgarkx(1,11), &
                       & tgarkx(1,12), tgarkx(1,13), tgarkx(0,14), tgarkx(0,15), tgarkt(0, 1), &
                       & tgarkt(0, 2), tgarkt(0, 3), tgarkt(0, 4), tgarkt(0, 5), tgarkt(0, 6), &
                       & tgarkt(0, 7), tgarkt(0, 8), tgarkt(0, 9), tgarkt(0,10), tgarkt(0,11), &
                       & tgarkt(0,12), tgarkt(0,13), tgarkt(0,14), tgarkt(0,15), tgarkt(0,16), &
                       & tgarkt(0,17), tgarkt(0,18), tgarkt(0,19), tgarkt(0,20), tgarnp(0, 1), &
                       & tgarnp(0, 3), gdp  )
             !
             !
             ! Turbulence production and dis
             !
             do k = 1, kmax
                tkepro(n, m, k) = tgarkx(k, 10)
                tkedis(n, m, k) = tgarkx(k, 11)
                fuiwe (n, m, k) = tgarkx(k, 12)
                fviwe (n, m, k) = tgarkx(k, 13)
             enddo
          endif
       enddo
    enddo
    !
    ! Re-define IWEDIA
    !
    iwedia = savdia
end subroutine iwe_00
