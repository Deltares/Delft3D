subroutine wangp3(s1        ,kcs       ,irocol    ,norow     ,icx       , &
                & icy       ,j         ,nmmaxj    ,a         ,b         , &
                & c         ,d         ,gdp       )
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
!    Function: WANGP3 performs step 3 of the so-called
!              "Method of Wang", which solves tridiagonal systems
!              (back substitution step)
! Method used:
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
!
! Global variables
!
    integer                                   , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                      !!  then computation proceeds in the X-
                                                                      !!  dir. If icx=1 then computation pro-
                                                                      !!  ceeds in the Y-dir.
    integer                                   , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                 :: j      !!  Begin pointer for arrays which have
                                                                      !!  been transformed into 1D arrays.
                                                                      !!  Due to the shift in the 2nd (M-)
                                                                      !!  index, J = -2*NMAX + 1
                                                                      !!  Begin pointer for arrays which have
                                                                      !!  been transformed into 1D arrays.
                                                                      !!  Due to the shift in the 2nd (M-)
                                                                      !!  index, J = -2*NMAX + 1
    integer                                                 :: nmmaxj !  Description and declaration in dimens.igs
    integer                                   , intent(in)  :: norow  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(5, norow)             , intent(in)  :: irocol !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: a
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: b
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: c      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              :: d
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: s1     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer  :: ddb
    integer  :: ic
    integer  :: icxy
    integer  :: mf
    integer  :: ml
    integer  :: n
    integer  :: nm
    integer  :: nmf
    integer  :: nmfu
    integer  :: nmfuu
    integer  :: nml
    integer  :: nmld
    integer  :: nmlu
    real(fp) :: zeta
!
!! executable statements -------------------------------------------------------
!
    !
    icxy = max(icx, icy)
    ddb = gdp%d%ddbound
    !
    do ic = 1, norow
       n     = irocol(1, ic)
       mf    = irocol(2, ic) - 1
       ml    = irocol(3, ic)
       nmf   = (n + ddb)*icy + (mf + ddb)*icx - icxy
       nmfu  = nmf + icx
       nmfuu = nmf + 2*icx
       nml   = (n + ddb)*icy + (ml + ddb)*icx - icxy
       nmlu  = nml + icx
       nmld  = nml - icx
       !
       !  BACK SUBSTITUTION: ELIMINATION OF LOWER DIAGONAL
       !
       if (kcs(nmf)==3) then
          zeta = d(nmf)
          do nm = nmfuu, nml, icx
             if (kcs(nm)>0) then
                d(nm) = d(nm) - a(nm)*zeta
             endif
          enddo
       endif
       !
       !  BACK SUBSTITUTION: ELIMINATION OF UPPER DIAGONAL
       !
       if (kcs(nmlu)==3) then
          zeta = d(nml)
          do nm = nmfu, nmld, icx
             if (kcs(nm)>0) then
                d(nm) = d(nm) - c(nm)*zeta
             endif
          enddo
       endif
    enddo
    !
    !  COMPUTE SOLUTION
    !
    do ic = 1, norow
       n   = irocol(1, ic)
       mf  = irocol(2, ic) - 1
       ml  = irocol(3, ic) + 1
       nmf = (n + ddb)*icy + (mf + ddb)*icx - icxy
       nml = (n + ddb)*icy + (ml + ddb)*icx - icxy
       do nm = nmf, nml, icx
          if (kcs(nm)>0) then
             s1(nm) = d(nm)/b(nm)
          endif
       enddo
    enddo
end subroutine wangp3
