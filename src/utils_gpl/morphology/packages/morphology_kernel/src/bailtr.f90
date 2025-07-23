subroutine bailtr(h         ,hrms      ,tp        ,thetaw    ,w         , &
                & dzdx      ,dzdy      ,sbksi     ,sbeta     ,ssksi     , &
                & sseta     ,epssl     ,faca      ,facu      ,ag        )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
    !
    implicit none
    !
!
! Arguments
!
    real(fp), intent(in)               :: dzdx
    real(fp), intent(in)               :: dzdy
    real(fp)                           :: h
    real(fp)                           :: hrms
    real(fp), intent(out)              :: sbeta
    real(fp), intent(out)              :: sbksi
    real(fp), intent(out)              :: sseta
    real(fp), intent(out)              :: ssksi
    real(fp), intent(in)               :: thetaw
    real(fp)                           :: tp
    real(fp), intent(in)               :: w
    real(fp), intent(in)               :: epssl
    real(fp), intent(in)               :: faca
    real(fp), intent(in)               :: facu
    real(fp), intent(in)               :: ag
!
!
! Local variables
!
    real(fp)                       :: cr
    real(fp)                       :: cs
    real(fp)                       :: even1b
    real(fp)                       :: even2b
    real(fp)                       :: even3b
    real(fp)                       :: even5b
    real(fp)                       :: facb
    real(fp)                       :: facs
    real(fp)                       :: odd2b
    real(fp)                       :: odd3
    real(fp)                       :: odd3b
    real(fp)                       :: odd4
    real(fp)                       :: odd4b
    real(fp)                       :: qbb
    real(fp)                       :: sn
    real(fp)                       :: tanpsi
!
!
!! executable statements -------------------------------------------------------
!
    tanpsi = 0.63
    cr = 0.
    qbb = 0.
    !
    ! interpolate oscillating velocity moments from table
    !
    call osmom(hrms      ,h         ,tp        ,ag        ,cr        , &
             & qbb       ,even1b    ,even2b    ,even3b    ,even5b    , &
             & odd2b     ,odd3b     ,odd4b     )
    !
    odd3 = odd3b*faca
    odd4 = odd4b*faca
    !
    ! compute coefficients
    !
    facb = 0.817e-4_fp
    facs = 1.03e-5_fp / w
    !
    ! compute wave components relative to ksi-direction
    !
    cs = cos(thetaw)
    sn = sin(thetaw)
    !
    ! compute transport rates
    !
    sbksi = facb*(odd3*cs + even3b*dzdx/tanpsi)
    ssksi = facs*(odd4*cs + even5b*dzdx*epssl/w)
    sbeta = facb*(odd3*sn + even3b*dzdy/tanpsi)
    sseta = facs*(odd4*sn + even5b*dzdy*epssl/w)
end subroutine bailtr
