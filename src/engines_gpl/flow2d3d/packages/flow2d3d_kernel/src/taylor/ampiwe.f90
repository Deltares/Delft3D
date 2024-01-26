subroutine ampiwe(angle     ,az        ,h0        ,kbed      ,ktop      , &
                & kmxt      ,omeg      ,qz        ,r1tg      ,scale     , &
                & utg       ,vtg       ,xkh       ,zamp      ,dzdz      , &
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
!      Function: Amplitude profiles of several IW variables
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
    real(fp) , pointer :: epsuko
!
! Global variables
!
    integer, intent(in)            :: kbed
                                   !!  KTG-value of upper level strat. layer
    integer, intent(in)            :: kmxt !  Description and declaration in dimens.igs
    integer, intent(in)            :: ktop
                                   !!  KTG-value of lower level strat. layer
    real(fp), intent(in)               :: angle
                                   !!  Angle [degrees] between horizontal
                                   !!  IW wavenumber vector and x-u-axis
    real(fp), intent(in)               :: az
                                   !!  Rms internal wave vertical displace-
                                   !!  ment amplitude [m]
    real(fp), intent(in)               :: h0
                                   !!  Water depth
    real(fp), intent(in)               :: omeg
                                   !!  Angular frequency of IW with respect
                                   !!  to ground (root of TG equation)
    real(fp), intent(in)               :: scale
                                   !!  Length scale for non-dimensionalising
    real(fp), intent(in)               :: xkh
                                   !!  Wave number magnitude normalized by
                                   !!  SCALE
    real(fp), dimension(0:kmxt) :: dzdz
                                   !!  Maximal IW-induced vertical strain
                                   !!  rate
    real(fp), dimension(0:kmxt) :: qz
                                   !!  Vertical wave number squared, in TG
                                   !!  equation
    real(fp), dimension(0:kmxt), intent(in) :: r1tg
    real(fp), dimension(0:kmxt), intent(in) :: utg
    real(fp), dimension(0:kmxt), intent(in) :: vtg
    real(fp), dimension(0:kmxt) :: zamp
                                   !!  Maximal vertical displacement amp-
                                   !!  litude of bed-induced IW
!
!
! Local variables
!
    integer                        :: k
    integer                        :: kmn
    integer                        :: kmx
    real(fp)                       :: crad
    real(fp)                       :: dzmax
    real(fp)                       :: dztg2
    real(fp)                       :: pi2
    real(fp)                       :: uko
    real(fp)                       :: xxk
    real(fp)                       :: xxl
    real(fp)                       :: zmax
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    epsuko  => gdp%gdiwearr%epsuko
    !
    pi2 = 8*atan(1.)
    crad = pi2/360.
    dztg2 = 2*h0/(scale*kmxt)
    !
    xxk = xkh*cos(angle*crad)
    xxl = xkh*sin(angle*crad)
    !
    kmn = 0
    kmx = kmxt
    !
    zmax = 0.0
    dzmax = 0.0
    !
    do k = kmn, kmx
       uko = xxk*utg(k) + xxl*vtg(k) - omeg
       uko = sign(max(epsuko, abs(uko)), uko)
       zamp(k) = az*r1tg(k)/uko
       if (ktop<=k .and. k<=kbed) zmax = max(zmax, abs(zamp(k)))
    enddo
    !
    do k = kmn + 1, kmx - 1
       dzdz(k) = (zamp(k - 1) - zamp(k + 1))/dztg2
       if (ktop<=k .and. k<=kbed) dzmax = max(dzmax, abs(dzdz(k)))
    enddo
    zamp(kmxt) = zmax
    dzdz(kmxt) = dzmax
end subroutine ampiwe
