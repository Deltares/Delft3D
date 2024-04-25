subroutine seliwe(angle     ,kbed      ,ktop      ,kmxt      ,omeg      , &
                & r1tg      ,utg       ,vtg       ,xkh       ,singul    , &
                & top       ,kcrit     ,gdp       )
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
!    Function: Determines whether the eigen mode R1TG for given
!              frequency OMEG has a critical layer and, if so,
!              on which side (top) this layer must have been
!              generated.
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
!
! Global variables
!
    integer, intent(in)            :: kbed
                                   !!  K-value on TG grid: upper level
                                   !!  stratified layer
    integer         :: kcrit
                                   !!  K-value on TG grid of the critical
                                   !!  level associated with a given IW own
                                   !!  mode (R1TG)
    integer, intent(in)            :: kmxt !  Description and declaration in dimens.igs
    integer, intent(in)            :: ktop
                                   !!  K-value on TG grid: lower level
                                   !!  stratified layer
    logical, intent(out)           :: singul
                                   !!  Indicator of singular solution of TG
                                   !!  equation
    logical, intent(out)           :: top
                                   !!  Indicator for instructing subroutine
                                   !!  TG to yield the eigen mode by
                                   !!  sweeping from surface to bed, or vice
                                   !!  versa
    real(fp), intent(in)               :: angle
                                   !!  Angle [degrees] between horizontal
                                   !!  IW wavenumber vector and x-u-axis
    real(fp), intent(in)               :: omeg
                                   !!  Angular frequency of IW with respect
                                   !!  to ground (root of TG equation)
    real(fp), intent(in)               :: xkh
                                   !!  Non-dimensional XK
    real(fp), dimension(0:kmxt), intent(in) :: r1tg
    real(fp), dimension(0:kmxt), intent(in) :: utg
    real(fp), dimension(0:kmxt), intent(in) :: vtg
!
!
! Local variables
!
    integer                        :: k
    real(fp)                       :: crad
    real(fp)                       :: prod
    real(fp)                       :: uko
    real(fp)                       :: ukoold
    real(fp)                       :: varbed
    real(fp)                       :: vartop
    real(fp)                       :: xxk
    real(fp)                       :: xxl
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    accur  => gdp%gdiwearr%accur
    !
    crad = atan(1.)/45.
    xxk = xkh*cos(angle*crad)
    xxl = xkh*sin(angle*crad)
    !
    kcrit = ktop
    ukoold = xxk*utg(kcrit) + xxl*vtg(kcrit) - omeg
    ! -->
   10 continue
    kcrit = kcrit + 1
    uko = xxk*utg(kcrit) + xxl*vtg(kcrit) - omeg
    prod = uko*ukoold
    ukoold = uko
    if (prod>0 .and. kcrit<kbed) goto 10
    ! <--
    if (kcrit==kbed) then
       singul = .false.
    else
       singul = .true.
       !
       ! From which side has R1TG been excited:
       !
       vartop = 0.0
       do k = 0, kcrit - 1
          if (vartop<accur .and. abs(r1tg(k) &
           & )<sqrt(accur)) vartop = vartop + r1tg(k)**2
       enddo
       vartop = vartop/kcrit
       !
       varbed = 0.0
       do k = kcrit, kmxt
          if (varbed<accur .and. abs(r1tg(k) &
           & )<sqrt(accur)) varbed = varbed + r1tg(k)**2
       enddo
       varbed = varbed/(kmxt - kcrit + 1)
       if (varbed>vartop) then
          top = .false.
       else
          top = .true.
       endif
    endif
end subroutine seliwe
