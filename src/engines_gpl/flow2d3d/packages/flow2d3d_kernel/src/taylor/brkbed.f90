subroutine brkbed(kmxdt     ,kmxt      ,np        ,nfreqs    ,atg       , &
                & btg       ,bv2       ,ctg       ,d2u       ,d2v       , &
                & h0        ,kbed      ,ktop      ,qz        ,r1tg      , &
                & scale     ,tgfind    ,utg       ,vtg       ,xkh       , &
                & freqlo    ,frequp    ,luniwe    ,gdp       )
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
!    Function: Bracketing ANGLE of horizontal IW wave number
!              vector, with respect to direction of depth-
!              averaged flow, for lee waves i.e. with OMEG=0.
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
    integer , pointer :: nrange
    integer , pointer :: ninc
    logical , pointer :: iwedia
!
! Global variables
!
    integer         :: kbed
                                   !!  K-value on TG grid: upper level
                                   !!  stratified layer
    integer         :: kmxdt !  Description and declaration in dimens.igs
    integer         :: kmxt !  Description and declaration in dimens.igs
    integer         :: ktop
                                   !!  K-value on TG grid: lower level
                                   !!  stratified layer
    integer, intent(in)            :: luniwe
                                   !!  Unit number for diagnostic reports of
                                   !!  this subprogram as well as called
                                   !!  subroutines
    integer, intent(in)            :: nfreqs !  Description and declaration in dimens.igs
    integer, intent(in)            :: np
                                   !!  Dimension for IWE frequency arrays
                                   !!  MAX (1,NFREQS)
    logical         :: tgfind
                                   !!  Indicator for instructing subroutine
                                   !!  TG to yield the eigen mode for given
                                   !!  input
    real(fp)        :: h0
                                   !!  Water depth
    real(fp)        :: scale
                                   !!  Length scale for scaling
    real(fp)        :: xkh
                                   !!  Non-dimensional XK
    real(fp), dimension(0:kmxdt) :: atg
    real(fp), dimension(0:kmxdt) :: btg
    real(fp), dimension(0:kmxdt) :: bv2
    real(fp), dimension(0:kmxdt) :: ctg
    real(fp), dimension(0:kmxdt) :: d2u
    real(fp), dimension(0:kmxdt) :: d2v
    real(fp), dimension(0:kmxdt) :: qz
    real(fp), dimension(0:kmxdt) :: r1tg
    real(fp), dimension(0:kmxdt) :: utg
    real(fp), dimension(0:kmxdt) :: vtg
    real(fp), dimension(np, 2), intent(out) :: freqlo
    real(fp), dimension(np, 2), intent(out) :: frequp
!
!
! Local variables
!
    integer                        :: i
    integer                        :: j
    integer                        :: jmax
    integer                        :: k
    integer                        :: kcrit                ! K-value on TG grid of the critical level associated with a given IW own mode (R1TG) 
    integer                        :: n
    integer                        :: nr
    logical                        :: top                  ! Indicator for instructing subroutine TG to yield the eigen mode by sweeping from surface to bed, or vice versa 
    real(fp)                       :: angle                ! Angle [degrees] between horizontal IW wavenumber vector and x-u-axis 
    real(fp)                       :: crit
    real(fp)                       :: diropp
    real(fp)                       :: diruv
    real(fp)                       :: flo
    real(fp)                       :: frlo                 ! Left bracket of frequency interval with largest root of TG equation 
    real(fp)                       :: frup                 ! Right bracket of frequency interval with largest root of TG equation 
    real(fp)                       :: fup
    real(fp)                       :: omeg
    real(fp)                       :: ratio                ! Ratio between depth-averaged kin. energy in internal wave and kin. energy of vertical motions, based on critical layer 
    real(fp)                       :: ulee
    real(fp)                       :: umag
    real(fp)                       :: umax
    real(fp)                       :: umin
    real(fp)                       :: vlee
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    nrange  => gdp%gdiwearr%nrange
    ninc    => gdp%gdiwearr%ninc
    iwedia  => gdp%gdiwearr%iwedia
    !
    do nr = 1, 2
       do k = 1, nfreqs
          freqlo(k, nr) = 0.0
          frequp(k, nr) = 0.0
       enddo
    enddo
    !
    ! Search for lee-wave induced critical layer:
    !
    umin = 1.E6
    umax = 0.0
    !
    do k = ktop, kbed
       atg(k) = sqrt(utg(k)**2 + vtg(k)**2)
       umag = atg(k)
       umax = max(umag, umax)
       if (umin>umag) then
          umin = umag
          kcrit = k
       endif
    enddo
    ratio = umin/max(1.E-10_fp, umax)
    !
    ! Probably no critical layer:
    !
    if (ratio>0.1) kcrit = ktop
    !
    ! Direction lower stratified part of flow:
    !
    ulee = 0.0
    vlee = 0.0
    !
    do k = kcrit, kbed
       ulee = ulee + utg(k)
       vlee = vlee + vtg(k)
    enddo
    n = max(1, kbed - kcrit)
    ulee = ulee/n
    vlee = vlee/n
    umag = sqrt(ulee**2 + vlee**2)
    if (umag<1.E-12) then
       diruv = 0.
    else
       diruv = 45.*acos(ulee/umag)/atan(1.)
    endif
    if (vlee<0.0) diruv = 180. + diruv
    !
    ! Initialisation, set ANGLE opposite to
    ! relevant flow direction:
    !
    if (diruv>=180.) then
       diropp = diruv - 180.
    else
       diropp = diruv + 180.
    endif
    angle = diropp
    omeg = 0.0
    top = .false.
    tgfind = .false.
    !
    call tg(kmxdt     ,kmxt      ,angle     ,atg       ,btg       , &
          & bv2       ,ctg       ,crit      ,d2u       ,d2v       , &
          & h0        ,kbed      ,ktop      ,omeg      ,qz        , &
          & r1tg      ,scale     ,tgfind    ,top       ,utg       , &
          & vtg       ,xkh       ,gdp       )
    !
    !
    flo = crit
    frlo = angle
    !
    jmax = 1 + ninc*nint(real(nrange,sp)/ninc)
    if (jmax<nrange) jmax = jmax + ninc
    i = 0
    !
    ! Find angle intervals with lee waves:
    !
    do j = 1, jmax, ninc
       angle = diropp + j
       omeg = 0.0
       top = .false.
       tgfind = .false.
       !
       call tg(kmxdt     ,kmxt      ,angle     ,atg       ,btg       , &
             & bv2       ,ctg       ,crit      ,d2u       ,d2v       , &
             & h0        ,kbed      ,ktop      ,omeg      ,qz        , &
             & r1tg      ,scale     ,tgfind    ,top       ,utg       , &
             & vtg       ,xkh       ,gdp       )
       !
       !
       fup = crit
       frup = angle
       if (flo*fup<0.0) then
          i = i + 1
          freqlo(i, 1) = frlo
          frequp(i, 1) = frup
       endif
       flo = fup
       frlo = frup
    enddo
    !
    ! Report number of zero crossings:
    !
    if (i==0 .and. iwedia) then
       write (luniwe, '(a)') ' BRKBED                          : no lee waves ?'
    else
       tgfind = .true.
    endif
end subroutine brkbed
