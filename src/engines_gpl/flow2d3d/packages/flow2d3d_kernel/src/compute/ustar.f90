subroutine ustar(s         ,uz        ,dz        ,zro       ,iro       , &
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
!    Function: Computes shear velocity
! Method used: Logaritmic Law of the Wall
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
    integer , pointer :: lundia
!
! Global variables
!
    integer  , intent(in)  :: iro !  Description and declaration in physco.igs
    real(fp) , intent(in)  :: dz  !!  Distance of the bottom layer from the wall, used to calculate bottom stress velocity
    real(fp)               :: s   !!  Zeta at old time level
    real(fp) , intent(in)  :: uz  !!  U-velocity at z=dz, used in calculation for bottom stress velocity
    real(fp) , intent(in)  :: zro !!  Roughness parameter used to compute ustar if IRO > 0
!
!
! Local variables
!
    integer           :: iexit                ! Exit return value 
    integer           :: nit
    integer           :: nitm
    real(fp)          :: b                    ! Internal work array, tridiagonal ma- trix water levels main diagonal 
    real(fp)          :: ckar
    real(fp)          :: eps                  ! Criterion (EPS/HDT) above which war- ning is issued for omega velocity 
    real(fp)          :: r                    ! Concentrations at old time level 
    real(fp)          :: rv
    real(fp)          :: rz
    real(fp)          :: s1                   ! Zeta at new time level 
    real(fp)          :: u                    ! U-velocities at old time level 
    real(fp)          :: xnu
!
! Data
!
    data xnu/1.E-6/, ckar/0.41/, rv/122.4/, b/5.2/
!
!! executable statements -------------------------------------------------------
!
    !
    ! Initialization
    !
    lundia  => gdp%gdinout%lundia
    !
    u = abs(uz)
    !
    if (iro/=0) then
       !
       ! Rough wall:
       !
       ! Previous approach:
       ! RZ   = DZ/ZRO
       !
       rz = (dz + zro)/zro
       !
       ! Previous approach:
       !
       !IF (RZ.GT.1.) THEN
       s = log(rz)/ckar
       !   UX   = U/S
       !ELSE
       !   S    = 12.
       !ENDIF
    else
       !
       ! Smooth wall:
       !
       eps = 1.E-2
       nitm = 100
       nit = 0
       !
       ! LOCAL RE-NUMBER:
       !
       r = abs(u)*dz/xnu
       if (r<0.001) r = 0.001
       !
       ! (VISCOUS SUBLAYER)
       !
       if (r<rv) then
          s = sqrt(r)
       else
          !
          ! LOG-LAYER; INITIAL TRIAL FOR S:
          !
          s = 12.
          !
          ! -->
          !
  100     continue
          nit = nit + 1
          s1 = s
          s = log(r/s1)/ckar + b
          !
          if (nit>=nitm) then
             write (lundia, '(1x,a)') '*** ERROR: no convergence in ustar.'
             !
             ! stop routine for DELFT3D
             !
             iexit = 4
             call d3stop(iexit     ,gdp       )
          !
          endif
          if (s>r) then
             write (lundia, '(1x,a)') '*** ERROR: s too large in ustar.'
             !
             ! stop routine for DELFT3D
             !
             iexit = 4
             call d3stop(iexit     ,gdp       )
          !
          endif
          if (abs(s1 - s)>(eps*s)) goto 100
       !
       ! <--       CONVERGENCE CRITERIUM:
       !
       endif
    endif
end subroutine ustar
