subroutine checku(hu        ,s1        ,dpu       ,umean     , &
                & kfu       ,kcs       ,kcu       , &
                & kspu      ,hkru      ,j         ,nmmaxj    , &
                & nmmax     ,kmax      ,icx       ,flood     ,dps       , &
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
!    Function: This routine checks the drying and flooding at ve-
!              locity points and sets the value of the mask
!              arrays to zero.
!              Upwind-approach for wet cross section in shallow
!              areas or if the model area contains structures.
!
! Method used: Upwind-approach for wet cross section in shallow
!              areas or if the model area contains structures.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)     , pointer :: dryflc
    real(fp)     , pointer :: drycrt
    logical      , pointer :: zmodel
!
! Global variables
!
    integer                                     , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                   :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                                   :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                   :: nmmax  !  Description and declaration in dimens.igs
    integer                                                   :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                 :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                 :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                 :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)         :: kspu   !  Description and declaration in esm_alloc_int.f90
    logical                                     , intent(in)  :: flood  !!  Flag for activating flooding part of checku subroutine
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)              :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: hkru   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: umean  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: nm
    real(fp):: drytrsh
    real(fp):: hucres
    real(fp):: floodtrsh
    integer :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    zmodel     => gdp%gdprocs%zmodel
    dryflc     => gdp%gdnumeco%dryflc
    drycrt     => gdp%gdnumeco%drycrt
    !
    floodtrsh = dryflc
    drytrsh   = drycrt
    nm_pos    = 1
    !
    call upwhu(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
             & zmodel    ,kcs       ,kcu       ,kspu      ,dps       , &
             & s1        ,dpu       ,umean     ,hu        ,gdp       )
    do nm = 1, nmmax
       !
       ! Approach for 2D weirs (following WAQUA)
       !   HUCRES is initially set to extreme large value to guarantee
       !   the MIN operator works as planned
       !
       hucres = 1.0e9_fp
       if (abs(kspu(nm, 0))==3 .or. abs(kspu(nm, 0))==9) then
          if (umean(nm)>=0.001) then
             hucres = s1(nm) + hkru(nm)
          elseif (umean(nm)<= - 0.001) then
             hucres = s1(nm + icx) + hkru(nm)
          else
             hucres = max(s1(nm + icx), s1(nm)) + hkru(nm)
          endif
       endif
       !
       ! check for drying
       !
       if (kfu(nm)*min(hu(nm), hucres)<drytrsh .and. kcu(nm)*kfu(nm)==1) then
          kfu(nm) = 0
       endif
       !
       ! check for flooding
       !
       if (  flood .and. kfu(nm)==0 .and. (kcu(nm)==1 .or. kcu(nm)==3) &
           & .and. max(s1(nm),s1(nm+icx)) - max(-real(dps(nm),fp),-real(dps(nm+icx),fp)) >= floodtrsh) then
          if (min(hu(nm), hucres)>floodtrsh) then
            kfu(nm) = 1
          endif
       endif
    enddo
    !
    ! exchange mask array kfu with neighbours for parallel runs
    !
    call dfexchg ( kfu, 1, 1, dfint, nm_pos, gdp )
end subroutine checku
