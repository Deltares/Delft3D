subroutine hulpgr(jaar      ,tm1       ,v         ,f         )
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
!    Function: Calulates help var. V and F
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Global variables
!
    integer                , intent(in)   :: jaar !!  Present year
    real(hp)               , intent(in)   :: tm1  !!  Given time in hours referred to
                                                  !!  January 1, 00:00:00
    real(hp), dimension(15)               :: v    !!  Help var. to calculate V0U()
    real(hp), dimension(25)               :: f    !!  Help var. to calculate FR()
!
! Local variables
!
    integer  :: ischrk  ! Number of leap-years since 1900 
    integer  :: j
    real(hp) :: ci
    real(hp) :: ci4
    real(hp) :: cri
    real(hp) :: dhalf   ! Value for 0.5 in SIGN function 
    real(hp) :: p
    real(hp) :: pix2    ! PI*2. 
    real(hp) :: q
    real(hp) :: rad     ! PI/180. 
    real(hp) :: ri
    real(hp) :: rjaar   ! Real value of JAAR - 1900 
    real(hp) :: rk
    real(hp) :: rn1
    real(hp) :: s2ri
    real(hp) :: si
    real(hp) :: si4
    real(hp) :: sri
    real(hp) :: sri3
    real(hp) :: tm3     ! ISCHRK + TM1/24.0, i.e. the number of correction-days since January 1, 1900 00:00 hour, after the length of a year is set to 365 days in the first instance 
    real(hp) :: z
!
!! executable statements -------------------------------------------------------
!
    ! Compute tm3 from tm1 plus the number of intercalary days extra
    ! since 1900. Secular years (at the turn of the century) that can not be divided by 400 are no leap years.
    !
    pix2   = 8.0d0*atan(1.0d0)
    dhalf  = 0.5d0
    rad    = pix2/360.0d0
    rjaar  = real(jaar - 1900, hp)
    ischrk = int((rjaar - 0.99d0)/4.0d0) - int((rjaar - 0.99d0)/100.0d0)        &
           & + int((rjaar + 300.0d0 - 0.99d0)/400.0d0)
    tm3    = real(ischrk, hp) + tm1/24.0d0
    !
    v(1) = (180.000d0 + 360.0000000d0*tm3)*rad
    v(2) = (277.026d0 + 129.3848200d0*rjaar + 13.176396800000d0*tm3)*rad
    v(3) = (334.384d0 +  40.6624700d0*rjaar +  0.111404000000d0*tm3)*rad
    v(4) = (280.190d0 -   0.2387136d0*rjaar +  0.985647360000d0*tm3)*rad
    v(5) = (281.221d0 +   0.0171800d0*rjaar +  0.000047064943d0*tm3)*rad
    v(8) = (259.156d0 + 340.6718100d0*rjaar -  0.052953945000d0*tm3)*rad
    !
    z = 0.009415d0
    p = atan(z*sin(v(8))/(1.0d0 + z*(1.0d0 - cos(v(8)))))
    z = -0.17794d0
    q = atan(z*sin(v(8))/(1.0d0 + z*(1.0d0 - cos(v(8)))))
    !
    v(6) = -p - q
    v(7) = p - q
    !
    rk = 0.9137d0 - 0.03569d0*cos(v(8))
    ri = atan(sqrt(1.0d0 - rk*rk)/rk)
    !
    v(9) = ri
    !
    p   = mod(v(3), pix2) - pix2*(sign(dhalf, v(3)) - dhalf)
    rk  = v(6)
    rn1 = v(7)
    !
    ! Computation of frequently used arguments
    !
    s2ri = sin(2.0d0*ri)
    sri  = sin(ri)
    si   = sin(0.5d0*ri)
    cri  = cos(ri)
    ci   = cos(0.5d0*ri)
    !
    v(10) = atan(s2ri*sin(rn1)/(s2ri*cos(rn1) + 0.3347d0))
    v(11) = atan(sri*sri*sin(2.0d0*rn1)/(sri*sri*cos(2.0d0*rn1) + 0.0727d0))
    v(12) = atan(sin(2.0d0*(p - rk))/(3.0d0*cri/(ci*ci) + cos(2.0d0*(p - rk))))
    v(13) = atan(sin(2.0d0*(p - rk))/(ci*ci/(si*si*6.0d0) - cos(2.0d0*(p - rk)))&
          & )
    v(14) = 3.0d0*v(10)
    v(15) = 0.0d0
    !
    ! Bring all angles inside the interval 0 - 2*pi radians
    !
    do j = 1, 15
       v(j) = mod(v(j), pix2) - pix2*(sign(dhalf, v(j)) - dhalf)
    enddo
    !
    ci4  = ci*ci*ci*ci
    si4  = si*si*si*si
    sri3 = sri*sri*sri
    !
    f(1)  = (2.0d0/3.0d0 - sri*sri)/0.5021d0
    f(2)  = sri*sri/0.1578d0
    f(3)  = sri*ci*ci/0.38d0
    f(4)  = s2ri/0.7214d0
    f(5)  = sri*si*si/0.0164d0
    f(6)  = ci4/0.9154d0
    f(7)  = sri*sri/0.1565d0
    f(8)  = si4/0.0017d0
    f(9)  = (sri - 1.25*sri3)/0.3192d0
    f(10) = sri3/0.063d0
    f(11) = sri*sri*ci*ci/0.1518d0
    f(12) = (1.0d0 - 10.0d0*si*si + 15.0*si4)*ci*ci/0.5873d0
    f(13) = (1.0d0 - 10.0d0*ci*ci + 15.0*ci4)*si*si/0.2147d0
    f(14) = sri*ci4/0.3658d0
    f(15) = (ci*ci - 2.0d0/3.0d0)*sri*ci*ci/0.1114d0
    f(16) = (ci*ci - 1.0d0/3.0d0)*sri*si*si/0.0103d0
    f(17) = ci4*ci*ci/0.8758d0
    f(18) = ci4*si*si/0.038d0
    f(19) = sqrt(0.8965d0*s2ri*s2ri + 0.6001d0*s2ri*cos(rn1) + 0.1006d0)
    f(20) = sqrt(19.0444d0*sri3*sri + 2.7702d0*sri*sri*cos(2.0d0*rn1)           &
          & + 0.0981d0)
    f(21) = 6.0d0*cri*cos(2.0d0*(p - rk))/(ci*ci) + 9.0d0*cri*cri/(ci4)
    f(21) = 2.6316d0*sri*ci*ci*0.5d0*sqrt(1.0d0 + f(21))
    f(22) = 36.0d0*si4/(ci4) - 12.0d0*si*si/(ci*ci)*cos(2.0d0*(p - rk))
    f(22) = 1.0924d0*ci4*sqrt(1.0d0 + f(22))
end subroutine hulpgr
