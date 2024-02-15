subroutine filterstructures(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                          & icy       ,kspu      ,kspv      ,evap      ,windsu    , &
                          & windsv    ,w10mag    ,uorb      ,tp        ,teta      , &
                          & dis       ,wsu       ,wsv       ,grmasu    ,grmasv    , &
                          & df        ,gdp       )
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
!    Function: Changes values of parameters that are influenced
!              by local situations (e.g. floating structures)
!              WARNING: localmutations w.r.t. radiation is
!              handled in HEATU
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
    logical , pointer :: wind
    logical , pointer :: wave
    logical , pointer :: struct
!
! Global variables
!
    integer                                          , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                          , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                        :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                          , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                          , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                        :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: kspu   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: kspv   !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(out) :: df     !  Description and declaration in esm_alloc_real.f90    
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub,4)     , intent(out) :: dis    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(out) :: evap   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(out) :: grmasu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(out) :: grmasv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(out) :: teta   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(out) :: tp     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(out) :: uorb   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(out) :: w10mag !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(out) :: windsu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(out) :: windsv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(out) :: wsu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(out) :: wsv    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: ndm
    integer :: nm
    integer :: nmd
!
!! executable statements -------------------------------------------------------
!
    wind       => gdp%gdprocs%wind
    wave       => gdp%gdprocs%wave
    struct     => gdp%gdprocs%struct
    !
    do nm = 1, nmmax
       nmd = nm - icx
       ndm = nm - icy
       !
       ! check whether the u-point is a floating structure
       !
       if (kspu(nm, 0) == 2) then
          windsu(nm) = 0.0_fp
          if (wave) then
             grmasu(nm) = 0.0_fp
             wsu(nm)    = 0.0_fp
          endif
       endif
       !
       ! check whether the v-point is a floating structure
       !
       if (kspv(nm, 0) == 2) then
          windsv(nm) = 0.0_fp
          if (wave) then
             grmasv(nm) = 0.0_fp
             wsv(nm)    = 0.0_fp
          endif
       endif
       !
       ! check whether the zeta-point is a floating structure
       !
       if (      kspu(nm, 0)==2 .and. kspu(nmd, 0)==2 &
         & .and. kspv(nm, 0)==2 .and. kspv(ndm, 0)==2  ) then
          evap(nm)   = 0.0_fp
          w10mag(nm) = 0.0_fp
          if (wave) then
             uorb(nm)  = 0.0_fp
             tp(nm)    = 0.0_fp
             teta(nm)  = 0.0_fp
             dis(nm,:) = 0.0_fp
             df(nm)    = 0.0_fp
          endif
       endif
    enddo
end subroutine filterstructures
