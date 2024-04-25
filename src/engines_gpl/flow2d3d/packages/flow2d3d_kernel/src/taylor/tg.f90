subroutine tg(kmxdt     ,kmxt      ,angle     ,atg       ,btg       , &
            & bv2       ,ctg       ,crit      ,d2u       ,d2v       , &
            & h0        ,kbed      ,ktop      ,omeg      ,qz        , &
            & r1tg      ,scale     ,tgfind    ,top       ,utg       , &
            & vtg       ,xkh       ,gdp       )
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
!      Function: If TGFIND is false then compute criterion CRIT
!                for finding real-valued eigen-modes R1TG[z(k)]
!                of the second-order Taylor-Goldstein (TG)
!                differential equation:
!
!                d2[r1tg(z)]/dz^2 + qz(z;omeg,angle)*[r1tg(z)] =0
!
!                If TGFIND=.false. this routine is used by the
!                root-finding routine ZBRENT for finding the
!                normalised angular frequency OMEG or wave number
!                direction ANGLE until CRIT=0 holds.
!
!                When calling TG with TGFIND=.true. then the
!                eigen-mode R1TG[z(k)] is presented on the same
!                equidistant grid.
!
!                This routine is preceeded by INTPOL which scales
!                the mean-flow variables that are used for
!                determining function Q_z[z(k);xk,xl,omeg].
!
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
    real(fp) , pointer :: accur
    real(fp) , pointer :: epsuko
!
! Global variables
!
    integer                       , intent(in)  :: kbed   !!  K-value on TG grid: upper level stratified layer
    integer                       , intent(in)  :: kmxdt  !  Description and declaration in dimens.igs
    integer                       , intent(in)  :: kmxt   !  Description and declaration in dimens.igs
    integer                       , intent(in)  :: ktop   !!  K-value on TG grid: lower level stratified layer
    logical                       , intent(in)  :: tgfind
    logical                       , intent(in)  :: top
    real(fp)                      , intent(in)  :: angle  !!  Angle [degrees] between horizontal
                                                          !!  IW wavenumber vector and x-u-axis
    real(fp)                      , intent(out) :: crit   !!  Minimal value found after convergence of root-finding
    real(fp)                      , intent(in)  :: h0     !!  Water depth
    real(fp)                      , intent(in)  :: omeg   !!  Angular frequency of IW with respect to ground (root of TG equation)
    real(fp)                      , intent(in)  :: scale  !!  Length scale for scaling
    real(fp)                      , intent(in)  :: xkh    !!  Non-dimensional XK
    real(fp) , dimension(0:kmxdt)               :: atg
    real(fp) , dimension(0:kmxdt)               :: btg
    real(fp) , dimension(0:kmxdt) , intent(in)  :: bv2
    real(fp) , dimension(0:kmxdt)               :: ctg
    real(fp) , dimension(0:kmxdt) , intent(in)  :: d2u
    real(fp) , dimension(0:kmxdt) , intent(in)  :: d2v
    real(fp) , dimension(0:kmxdt)               :: qz
    real(fp) , dimension(0:kmxdt)               :: r1tg
    real(fp) , dimension(0:kmxdt) , intent(in)  :: utg
    real(fp) , dimension(0:kmxdt) , intent(in)  :: vtg
!
!
! Local variables
!
    integer      :: k
    integer      :: kd
    integer      :: kstart
    integer      :: ksum
    integer      :: ku
    logical      :: stndrd
    real(fp)     :: akbed
    real(fp)     :: bkbed
    real(fp)     :: bktop
    real(fp)     :: cktop
    real(fp)     :: crad
    real(fp)     :: dz
    real(fp)     :: dz2
    real(fp)     :: dzk
    real(fp)     :: pi2
    real(fp)     :: psibed
    real(fp)     :: psitop
    real(fp)     :: r1nrm
    real(fp)     :: sum
    real(fp)     :: uko
    real(fp)     :: xkl2
    real(fp)     :: xm2
    real(fp)     :: xxk
    real(fp)     :: xxl
    real(fp)     :: znrm
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    accur   => gdp%gdiwearr%accur
    epsuko  => gdp%gdiwearr%epsuko
    !
    pi2  = 8*atan(1.)
    crad = pi2/360.
    dz   = h0/(scale*kmxt)
    dz2  = dz**2
    xm2  = (pi2/(2*dz))**2
    !
    ! Subroutine INTPOL scaled the horizontal wavenumber
    ! magnitude XK int XKH. ANGLE (in degrees) is adjustable
    ! for searching standing/lee waves.
    !
    xkl2 = xkh**2
    xxk  = xkh*cos(angle*crad)
    xxl  = xkh*sin(angle*crad)
    !
    ! The Q_z function:
    !
    do k = 0, kmxt
       if (ktop<=k .and. k<=kbed) then
          uko = xxk*utg(k) + xxl*vtg(k) - omeg
          uko = sign(max(epsuko, abs(uko)), uko)
          qz(k) = xkl2*bv2(k)/(uko*uko) - xkl2 - (xxk*d2u(k) + xxl*d2v(k))/uko
          qz(k) = sign(min(xm2, abs(qz(k))), qz(k))
       else
          qz(k) = 0.0
       endif
    enddo
    !
    ! Criterion for selection of solution procedures:
    !
    stndrd = (ktop<=1) .and. (kbed>=kmxt - 1)
    !
    ! Below the standard procedure; it is split into two options:
    ! if top= .true.: "downward" sweep from k=0,
    ! if top=.false.:   "upward" sweep from k=kmxt.
    !
    ! Assemble tri-diagonal system of Taylor-Goldstein equation with
    ! sweep from TOP TO BED for zero-criterion of finding eigenmode.
    !
    if (stndrd .and. top) then
       btg(0) = 1.
       ctg(0) = 0.
       ctg(kmxt) = 0.
       do k = 1, kmxt - 1
          atg(k) = 1.
          ctg(k) = 1.
          btg(k) = qz(k)*dz2 - 2.
          kd = k - 1
          btg(k) = btg(k)*btg(kd) - atg(k)*ctg(kd)
          ctg(k) = ctg(k)*btg(kd)
          if (abs(ctg(k))>accur) ctg(k) = sign(accur, ctg(k))
          if (abs(btg(k))>accur) btg(k) = sign(accur, btg(k))
       enddo
       crit = btg(kmxt - 1)
       if (.not.tgfind) goto 9999
    endif
    !
    ! Reversal of previous solution procedure, now sweep from BED TO TOP:
    !
    if (stndrd .and. (.not.top)) then
       btg(kmxt) = 1.
       atg(kmxt) = 0.
       ctg(0) = 0.
       do k = kmxt - 1, 1, -1
          atg(k) = 1.
          ctg(k) = 1.
          btg(k) = qz(k)*dz2 - 2.
          ku = k + 1
          btg(k) = btg(k)*btg(ku) - atg(ku)*ctg(k)
          if (abs(btg(k))>accur) btg(k) = sign(accur, btg(k))
          atg(k) = atg(k)*btg(ku)
       enddo
       crit = btg(1)
       if (.not.tgfind) goto 9999
    endif
    !
    ! Alternative: inner solution according to Taylor-Goldstein equation,
    ! matching with outer solutions being sinh(kz)-profiles. The latter
    ! profiles, however, neglect velocity curvature in neutrally-stratified
    ! top and/or bed layers.
    !
    ! This alternative procedure is split into two options:
    ! if top= .true.: "downward" sweep from k=ktop,
    ! if top=.false.:   "upward" sweep from k=kbed.
    !
    ! Assemble matrix for inner solution only, with sweep from TOP TO BED:
    !
    if ((.not.stndrd) .and. top) then
       !
       ! Matching inner solution to outer solution extending to water surface:
       !
       dzk = xkh*dz
       psitop = 2.*tanh(dzk*(ktop + 0.5))/dzk
       atg(ktop) = 0.0
       btg(ktop) = 1.0
       if (ktop>0) then
          ctg(ktop) = (1. - psitop)/(1. + psitop)
          r1tg(ktop) = 1.0
          kstart = 1
       else
          r1tg(0) = 0.0
          r1tg(1) = 1.0
          kstart = 2
       endif
       !
       atg(kbed) = 0.0
       btg(kbed) = 1.0
       ctg(kbed) = 0.0
       do k = ktop + 1, kbed
          kd = k - 1
          atg(k) = 1.
          ctg(k) = 1.
          btg(k) = qz(k)*dz2 - 2.
          btg(k) = btg(k)*btg(kd) - atg(k)*ctg(kd)
          ctg(k) = ctg(k)*btg(kd)
          if (abs(ctg(k))>accur) ctg(k) = sign(accur, ctg(k))
          if (abs(btg(k))>accur) btg(k) = sign(accur, btg(k))
       enddo
       !
       ! The inner solution:
       !
       do k = ktop + kstart, kbed
          kd = k - 1
          r1tg(k) = -btg(kd)*r1tg(kd)/ctg(kd)
       enddo
       !
       ! Root-finding criterium is "crit" is based on matching bed profile:
       !
       if (kbed<kmxt) then
          psibed = 2.*tanh(dzk*(kmxt - kbed + 0.5))/dzk
          akbed = (1. - psibed)/(1. + psibed)
          bkbed = 1.
          crit = akbed*r1tg(kbed - 1) + bkbed*r1tg(kbed)
       else
          crit = r1tg(kmxt)
       endif
       if (.not.tgfind) goto 9999
    endif
    !
    ! Assemble matrix for inner solution only, with sweep from BED TO TOP.
    !
    ! Matching inner solution to outer solution, extending to bed:
    !
    if ((.not.stndrd) .and. (.not.top)) then
       dzk = xkh*dz
       psitop = 2.*tanh(dzk*(kmxt - kbed + 0.5))/dzk
       ctg(kbed) = 0.0
       btg(kbed) = 1.0
       if (kbed<kmxt) then
          atg(kbed) = (1. - psitop)/(1. + psitop)
          r1tg(kbed) = 1.0
          kstart = 1
       else
          r1tg(kmxt) = 0.0
          r1tg(kmxt - 1) = 1.0
          kstart = 2
       endif
       !
       btg(ktop) = 1.
       atg(ktop) = 0.
       ctg(ktop) = 0.
       do k = kbed - 1, ktop + 1, -1
          atg(k) = 1.
          ctg(k) = 1.
          btg(k) = qz(k)*dz2 - 2.
          ku = k + 1
          btg(k) = btg(k)*btg(ku) - atg(ku)*ctg(k)
          if (abs(btg(k))>accur) btg(k) = sign(accur, btg(k))
          atg(k) = atg(k)*btg(ku)
       enddo
       !
       ! The inner solution, sweeping from bed to top:
       !
       do k = kbed - kstart, ktop, -1
          ku = k + 1
          r1tg(k) = -btg(ku)*r1tg(ku)/atg(ku)
       enddo
       !
       ! Root-finding criterium "crit" is based on matching top profile:
       !
       if (ktop>0) then
          psitop = 2.*tanh(dzk*(ktop + 0.5))/dzk
          bktop = 1.
          cktop = (1. - psitop)/(1. + psitop)
          crit = bktop*r1tg(ktop) + cktop*r1tg(ktop + 1)
       else
          crit = r1tg(0)
       endif
       if (.not.tgfind) goto 9999
    endif
    !
    ! The eigen-mode, normalization occurs at the end of this subroutine:
    !
    if (stndrd .and. top .and. tgfind) then
       r1tg(0) = 0.0
       r1tg(1) = 1.E-6
       do k = 2, kmxt
          kd = k - 1
          r1tg(k) = -btg(kd)*r1tg(kd)/ctg(kd)
       enddo
    endif
    !
    if (stndrd .and. (.not.top) .and. tgfind) then
       r1tg(kmxt) = 0.0
       r1tg(kmxt - 1) = 1.E-6
       do k = kmxt - 2, 0, -1
          ku = k + 1
          r1tg(k) = -btg(ku)*r1tg(ku)/atg(ku)
       enddo
    endif
    !
    ! In case of solving just inner solution, below matching of inner to outer solution:
    !
    if ((.not.stndrd) .and. tgfind) then
       !
       ! Outer solution, matching to sinh(kz)-profile near bed:
       !
       if (kbed<kmxt) then
          znrm = r1tg(kbed)/sinh((kmxt - kbed)*dzk)
          do k = kbed, kmxt
             r1tg(k) = znrm*sinh((kmxt - k)*dzk)
          enddo
       endif
       !
       ! Outer solution, matching to sinh(kz)-profile near surface:
       !
       if (ktop>0) then
          znrm = r1tg(ktop)/sinh(ktop*dzk)
          do k = 0, ktop
             r1tg(k) = znrm*sinh(k*dzk)
          enddo
       endif
    endif
    !
    ! Normalize eigen-solution such that square root of
    ! depth-averaged variance of R1TG is unity:
    !
    ! Improved normalisation:
    !
    sum = 0.
    ksum = 0
    do k = 0, kmxt
       if (sum<accur .and. abs(r1tg(k))<sqrt(accur)) then
          sum = sum + r1tg(k)**2
          ksum = ksum + 1
       endif
    enddo
    r1nrm = sqrt(sum/ksum)
    do k = 0, kmxt
       r1tg(k) = r1tg(k)/r1nrm
    enddo
    !
 9999 continue
end subroutine tg
