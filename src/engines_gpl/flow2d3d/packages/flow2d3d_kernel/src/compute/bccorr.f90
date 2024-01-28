subroutine bccor(j         ,nmmaxj    ,kmax      ,norow     ,icx       , &
               & icy       ,zmodel    ,irocol    ,kcs       ,kfu       , &
               & qxk       ,thick     ,circ2d    ,gdp       )
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
!    Function: Correction for discharge boundaries so that the incoming
!              or outgoing discharge is exactly the value prescribed 
!              in the MD-file.
!              N.B. This correction is essential for the
!              SOBEK <--> Delft3D-FLOW coupling
!
!              Note: this correction is only implemented for the Sigma
!                    model (and not for the Z-model)
!              Note: It replaces CUCDP , in which the non-linear iteration
!                    was switched off the discharge boundaries
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                , pointer :: iro
!
! Global variables
!
    integer                                         , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                            !!  then computation proceeds in the X-
                                                                            !!  dir. If icx=1 then computation pro-
                                                                            !!  ceeds in the Y-dir.
    integer                                         , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                         , intent(in)  :: j      !!  Begin pointer for arrays which have
                                                                            !!  been transformed into 1D arrays.
                                                                            !!  Due to the shift in the 2nd (M-)
                                                                            !!  index, J = -2*NMAX + 1
    integer                                         , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in)  :: nmmaxj !  Description and declaration in dimens.igs
    integer                                         , intent(in)  :: norow  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, norow)                    , intent(in)  :: irocol !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    logical                                         , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(fp), dimension(4, norow)                   , intent(in)  :: circ2d !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                       , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer       :: ddb
    integer       :: ibf
    integer       :: ibl
    integer       :: ic
    integer       :: icxy
    integer       :: k
    integer       :: mf
    integer       :: ml
    integer       :: n
    integer       :: nmf
    integer       :: nmfu
    integer       :: nml
!
!! executable statements -------------------------------------------------------
!
    iro      => gdp%gdphysco%iro
    !
    !
    icxy = max(icx, icy)
    ddb  = gdp%d%ddbound
    !
    ! Set boundary conditions
    !
    ! Loop over grid rows for boundary conditions
    !
    do ic = 1, norow
       n    = irocol(1, ic)
       mf   = irocol(2, ic) - 1
       ml   = irocol(3, ic)
       ibf  = irocol(4, ic)
       ibl  = irocol(5, ic)
       nmf  = (n + ddb)*icy + (mf + ddb)*icx - icxy
       nmfu = nmf + icx
       nml  = (n + ddb)*icy + (ml + ddb)*icx - icxy
       if (kfu(nmf) == 1) then
          !
          ! Set coefficients for begin of row in the case of an open boundary
          !
          if (ibf==5 .or. ibf==7) then
             !
             ! Discharge boundary
             !
             do k = 1, kmax
                qxk(nmf,k) = circ2d(1,ic) * thick(k)
             enddo
          endif
       endif
       if (kfu(nml) == 1) then
          !
          ! Set coefficients for end of row in the case of an open boundary
          !
          if (ibl==5 .or. ibl==7) then
             !
             ! Discharge boundary
             !
             do k = 1, kmax
                qxk(nml,k) = circ2d(2,ic) * thick(k)
             enddo
          endif
       endif
    enddo
end subroutine bccor
