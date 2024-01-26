subroutine totiwe(kmxdt     ,kmxt      ,ratio     ,singul    ,top       , &
                & r1tg      ,h0        ,scale     ,kcrit     ,xkh       )
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
!    Function: Compute ratio between twice Kin. IWE and <w^2>
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer, intent(in)            :: kcrit
                                   !!  K-value on TG grid of the critical
                                   !!  level associated with a given IW own
                                   !!  mode (R1TG)
    integer, intent(in)            :: kmxdt !  Description and declaration in dimens.igs
    integer, intent(in)            :: kmxt !  Description and declaration in dimens.igs
    logical, intent(in)            :: singul
                                   !!  Indicator of singular solution of TG
                                   !!  equation
    logical, intent(in)            :: top
                                   !!  Indicator for instructing subroutine
                                   !!  TG to yield the eigen mode by
                                   !!  sweeping from surface to bed, or vice
                                   !!  versa
    real(fp), intent(in)               :: h0
                                   !!  Water depth
    real(fp), intent(out)              :: ratio
                                   !!  Ratio between depth-averaged kin.
                                   !!  energy in internal wave and kin.
                                   !!  energy of vertical motions, based on
                                   !!  upper resp. lower critical layer
    real(fp), intent(in)               :: scale
                                   !!  Length scale for scaling
    real(fp), intent(in)               :: xkh
                                   !!  Non-dimensional XK
    real(fp), dimension(0:kmxdt), intent(in) :: r1tg
                                   !!  Eigen mode of TG equation with zero
                                   !!  vertial velocity on bed and surface
!
!
! Local variables
!
    integer                        :: k
    integer                        :: kmn
    integer                        :: kmx
    real(fp)                       :: dzk2
    real(fp)                       :: dztg
    real(fp)                       :: sumuv2
    real(fp)                       :: sumw2
    real(fp)                       :: uvtg
    real(fp)                       :: uvtgd
    real(fp)                       :: uvtgu
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    if (singul .and. top) then
       kmn = 0
       kmx = kcrit
    elseif (singul) then
       kmn = kcrit
       kmx = kmxt
    else
       kmn = 0
       kmx = kmxt
    endif
    !
    dztg = h0/(scale*kmxt)
    dzk2 = 2*xkh*dztg
    !
    sumuv2 = 0.0
    sumw2 = 0.0
    do k = kmn + 1, kmx - 1
       uvtg = (r1tg(k - 1) - r1tg(k + 1))/dzk2
       sumuv2 = sumuv2 + uvtg**2
       sumw2 = sumw2 + r1tg(k)**2
    enddo
    !
    ! Estimate dw/dz at bed or water surface but set dw/dz=0
    ! at critical level (as in viscous solution):
    !
    if (singul .and. top) then
       uvtgd = -(4*r1tg(kmn + 1) - r1tg(kmn + 2))/dzk2
       uvtgu = 0.0
    elseif (singul) then
       uvtgd = 0.0
       uvtgu = (4*r1tg(kmx - 1) - r1tg(kmx - 2))/dzk2
    else
       uvtgd = -(4*r1tg(kmn + 1) - r1tg(kmn + 2))/dzk2
       uvtgu = (4*r1tg(kmx - 1) - r1tg(kmx - 2))/dzk2
    endif
    !
    ! Complete integral of do-10 loop:
    !
    sumuv2 = sumuv2 + 0.5*(uvtgd**2 + uvtgu**2)
    !
    ! RATIO is ratio between <u^2>+<v^2>+<w^2> i.e. sum
    ! of potential and kinetic IWE and <w^2>:
    !
    ratio = 1. + sumuv2/sumw2
end subroutine totiwe
