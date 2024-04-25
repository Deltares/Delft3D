subroutine cucdp(kfu       ,irocol    ,norow     ,j         ,nmmaxj    , &
               & icx       ,icy       ,bb        ,gdp       )
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
!    Function: Re-computes system of equations for discharge
!              boundaries.
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
    integer, intent(in)                                     :: icx
                                                                     !!  Increment in the X-dir., if ICX= NMAX
                                                                     !!  then computation proceeds in the X-
                                                                     !!  dir. If icx=1 then computation pro-
                                                                     !!  ceeds in the Y-dir.
    integer, intent(in)                                     :: icy
                                                                     !!  Increment in the Y-dir. (see ICX)
    integer                                                 :: j
                                                                     !!  Begin pointer for arrays which have
                                                                     !!  been transformed into 1D arrays.
                                                                     !!  Due to the shift in the 2nd (M-)
                                                                     !!  index, J = -2*NMAX + 1
    integer                                                 :: nmmaxj !  Description and declaration in dimens.igs
    integer, intent(in)                                     :: norow  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, norow), intent(in)                :: irocol !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)   :: kfu    !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: bb
                                                                     !!  Internal work array, coefficient mean
                                                                     !!  velocity
!
!
! Local variables
!
    integer                        :: ddb
    integer                        :: ibf
    integer                        :: ibl
    integer                        :: ic
    integer                        :: icxy
    integer                        :: mf
    integer                        :: ml
    integer                        :: n
    integer                        :: nmf
    integer                        :: nml
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    icxy = max(icx, icy)
    ddb = gdp%d%ddbound
    !
    ! ADAPT DISCHARGE BOUNDARY CONDITIONS
    ! value for H(p) / H(p+1) is set to one
    !
    do ic = 1, norow
       !
       n = irocol(1, ic)
       mf = irocol(2, ic) - 1
       ml = irocol(3, ic)
       ibf = irocol(4, ic)
       ibl = irocol(5, ic)
       nmf = (n + ddb)*icy + (mf + ddb)*icx - icxy
       nml = (n + ddb)*icy + (ml + ddb)*icx - icxy
       !
       ! LAYER VELOCITIES ( VELOCITY PROFILE )
       !
       if (kfu(nmf)/=0 .and. (ibf==5 .or. ibf==7)) then
          bb(nmf) = 1.0
       endif
       !
       if (kfu(nml)/=0 .and. (ibl==5 .or. ibl==7)) then
          bb(nml) = 1.0
       endif
    enddo
end subroutine cucdp
