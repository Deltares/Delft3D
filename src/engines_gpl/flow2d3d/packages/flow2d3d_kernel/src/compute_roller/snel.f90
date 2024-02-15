subroutine snel(mmax      ,nmax      ,norow     ,noroco    ,ubot      , &
              & irocol    ,dps       ,s0        ,alfas     ,uorb      , &
              & tp        ,teta      ,hrms      ,rlabda    , &
              & ewave0    ,wlen      ,gdp       )
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: timmin
    real(fp)               , pointer :: gammax
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
    integer                , pointer :: iro
    logical                , pointer :: ubot_from_com
    logical                , pointer :: wlen_from_com
    integer, dimension(:)  , pointer :: isdir
    logical                , pointer :: first
!
! Global variables
!
    integer                                                        , intent(in)  :: mmax
    integer                                                        , intent(in)  :: nmax
    integer                                                        , intent(in)  :: noroco
    integer                                                        , intent(in)  :: norow
    integer   , dimension(5, noroco)                               , intent(in)  :: irocol
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: alfas
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: dps
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: ewave0
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: hrms
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: rlabda
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: s0
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: teta
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: tp
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: ubot
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: uorb
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: wlen
!
! Local variables
!
    integer                :: ibf    ! Code for open boundary corresponding
    integer                :: ibl    ! with cell irocol(1),irocol(2) Code for open boundary corresponding
    integer                :: ic
    integer                :: m
    integer                :: mf
    integer                :: mfu
    integer                :: ml
    integer                :: mlu
    integer                :: n
    integer                :: nf
    integer                :: nfu
    integer                :: nl
    integer                :: nh0    ! Number of open boundary points
    integer                :: nwav
    integer                :: nlu
    real(fp)               :: cstdir
    real(fp)               :: h0     ! Average water depth on open boundary points
    real(fp)               :: hrms0
    real(fp)               :: hrmsl
    real(fp)               :: hw
    real(fp)               :: incdir
    real(fp)               :: k
    real(fp)               :: k0
    real(fp)               :: k0h
    real(fp)               :: omega
    real(fp)               :: per
    real(fp)               :: rlabda0
    real(fp)               :: th0rad
    real(fp)               :: thcart
    real(fp)               :: thloc
    real(fp)               :: tp0
    real(fp)               :: uorb1
    real(fp), dimension(7) :: wavcon
    character(37)          :: wavnam
    character(256)         :: case
!
!! executable statements -------------------------------------------------------
!
    timmin          => gdp%gdinttim%timmin
    gammax          => gdp%gdnumeco%gammax
    rhow            => gdp%gdphysco%rhow
    ag              => gdp%gdphysco%ag
    iro             => gdp%gdphysco%iro
    ubot_from_com   => gdp%gdprocs%ubot_from_com
    wlen_from_com   => gdp%gdprocs%wlen_from_com
    isdir           => gdp%gdsnel%isdir
    first           => gdp%gdsnel%first
    !
    ! Initialize local variables
    !
    !
    ! reading of wavecon file for time series of waves
    ! name is build using runid as extension
    !
    write(wavnam,'(a,a)') 'wavecon.', trim(gdp%runid)
    nwav = 7
    call varcon(wavnam, timmin, wavcon, isdir, nwav, gdp)
    hrms0  = wavcon(1) / sqrt(2.0_fp)
    tp0    = wavcon(2)
    incdir = wavcon(3)
    !
    ! h0 is average waterdepth at boundary: first it contains the sum of all waterdepths, then it will be divided by nh0
    !
    h0  = 0.0_fp
    nh0 = 0
    !
    ! orientation of grid is assumed to be constant
    !
    cstdir = 270.0_fp - alfas(2,1)
    th0rad = (incdir - cstdir)*degrad
    
    if (first) then
       !
       ! Initialise EWAVE0 all points; before first step
       ! NOTE: SETWAV is called before timeloop with EEWAVE1 as
       ! actual argument
       !
       first = .false.
       do m = 1, mmax
          do n = 1, nmax
             hrmsl        = min(hrms0, gammax*(real(dps(n,m),fp)+s0(n,m)))
             ewave0(n, m) = rhow * ag * hrmsl * hrmsl / 8.0_fp
          enddo
       enddo
    endif
    !
    ! Calculate values of uorb, tp, and hrms at open boundaries (KCS=2)
    ! Rows
    !
    do ic = 1, norow
       n   = irocol(1,ic)
       mfu = irocol(2,ic)
       ml  = irocol(3,ic)
       ibf = irocol(4,ic)
       ibl = irocol(5,ic)
       mf  = mfu - 1
       mlu = ml  + 1
       do m = mf, ml
          nh0 = nh0 + 1
          h0  = h0 + max(0.1_fp , real(dps(n,m),fp) + s0(n,m))
       enddo
       !
       ! Left side of row
       !
       if (ibf /= 8) then
          ewave0(n, mf) = rhow * ag * hrms0 * hrms0 / 8.0_fp
       endif
       !
       ! Right side of row
       !
       if (ibl /= 8) then
          ewave0(n, mlu) = rhow * ag * hrms0 * hrms0 / 8.0_fp
       endif
    enddo
    !
    ! Columns
    !
    do ic = norow+1, noroco
       m   = irocol(1,ic)
       nfu = irocol(2,ic)
       nl  = irocol(3,ic)
       ibf = irocol(4,ic)
       ibl = irocol(5,ic)
       nf  = nfu - 1
       nlu = nl + 1
       do n = nf, nl
          nh0 = nh0 + 1
          h0  = h0 + max(0.1_fp , real(dps(n,m),fp) + s0(n,m))
       enddo
       !
       ! Lower side of column
       !
       if (ibf /= 8) then
          ewave0(nf, m) = rhow * ag * hrms0 * hrms0 / 8.0_fp
       endif
       !
       ! Upper side of column
       !
       if (ibl /= 8) then
          ewave0(nlu, m) = rhow * ag * hrms0 * hrms0 / 8.0_fp
       endif
    enddo
    !
    ! Divide the sum of all waterdepths by nh0.
    ! nh0 might be zero (no open boundaries)
    ! The resulting h0 must be 0.1 or bigger
    !
    h0 = max(0.1_fp, h0 / real(max(1,nh0),fp) )
    !
    ! Compute the orbital velocity and wave length.
    !
    call wavenr(h0, tp0, k, ag)
    rlabda0 = 2.0_fp * pi / k
    do m = 1, mmax
       do n = 1, nmax
          if (ewave0(n,m) < 1.0e-6_fp) then
             hrms(n,m) = 0.0_fp
          else
             hrms(n,m) = sqrt(8.0_fp*ewave0(n,m)/ag/rhow + 1.0e-6_fp)
          endif
          hw      = max(0.01_fp, real(dps(n,m),fp) + s0(n,m))
          tp(n,m) = tp0
          per     = max(0.01_fp, tp(n,m))
          !
          ! Prevent unrealistic Hrms in shallow water
          !
          hrms(n,m) = min(hrms(n,m) , hw)
          omega     = 2.0_fp * pi / per
          k0        = omega * omega / ag
          k0h       = k0 * hw
          if (k0h > pi) then
             k = k0
          elseif (k0h < 0.005_fp) then
             k = omega / sqrt(ag*hw)
          else
             call wavenr(hw, per, k, ag)
          endif
          if (wlen_from_com) then
             rlabda(n, m) = wlen(n,m)
          else
             rlabda(n,m) = 2.0_fp * pi / k
          endif
          if (k*hw < 80.0_fp) then
             if (ubot_from_com) then
                uorb(n,m) = ubot(n,m)
             else
                uorb1     = 0.5_fp * hrms(n,m) * omega / sinh(k*hw)
                uorb(n,m) = uorb1 * sqrt(pi) / 2.0_fp
             endif
          else
             uorb(n,m) = 0.0_fp
          endif
          !
          ! And now for the real Snellius
          !
          thloc      = asin(rlabda(n,m)/rlabda0*sin(th0rad))/degrad + cstdir
          thcart     = 270.0_fp - thloc
          teta(n, m) = thcart - alfas(n,m)
          if (teta(n,m) > 360.0_fp) then
             teta(n,m) = teta(n,m) - 360.0_fp
          endif
       enddo
    enddo
end subroutine snel
