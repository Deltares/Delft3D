subroutine inised(lundia    ,error     ,nmax      ,mmax      ,nmaxus    , &
                & nmmax     ,lsed      ,lsedtot   ,kcs       ,gdp       )
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
!    Function: - Initialisation total sediment at bed in each
!                horizontal point
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    use bedcomposition_module
    use morphology_data_module, only: allocsedtra, CODE_DELFT3D
    use sediment_basics_module
    use m_initsedtra, only: initsedtra
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                             , pointer :: rhow
    real(fp)                             , pointer :: ag
    real(fp)                             , pointer :: vicmol
    !
    integer                              , pointer :: nxx
!
! Global variables
!
    integer                                            , intent(in)  :: lsed    !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in)  :: lsedtot !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in)  :: lundia  !  Description and declaration in inout.igs
    integer                                            , intent(in)  :: mmax    !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in)  :: nmax    !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in)  :: nmaxus  !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in)  :: nmmax   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)  :: kcs     !  Description and declaration in esm_alloc_int.f90
    logical                                                          :: error   !  Flag=TRUE if an error is encountered
!
! Local variables
!
    integer           :: nmaxddb
    integer           :: nmlb
    integer           :: nmub
!
!! executable statements -------------------------------------------------------
!
    rhow                => gdp%gdphysco%rhow
    ag                  => gdp%gdphysco%ag
    vicmol              => gdp%gdphysco%vicmol
    !
    nxx                 => gdp%gdmorpar%nxx
    !
    nmlb    = gdp%d%nmlb
    nmub    = gdp%d%nmub
    nmaxddb = gdp%d%nub - gdp%d%nlb + 1
    !
    if (lsedtot==1 .and. gdp%gdsedpar%flsdia/=' ') then
       call mirror_bnd(1         ,nmaxddb   ,nmmax     ,kcs       , &
                     & gdp%gdsedpar%sedd50fld,nmlb     ,nmub      )
    endif
    !
    call allocsedtra(gdp%gderosed, gdp%gdmorpar%moroutput, gdp%d%kmax, lsed, lsedtot, &
                   & gdp%d%nmlb, gdp%d%nmub, gdp%d%nmlb, gdp%d%nmub, nxx, &
                   & gdp%gdmorpar%moroutput%nstatqnt, CODE_DELFT3D)
    !
    ! Initialise morphology layers
    !
    call inimorlyr(lundia    ,error     ,nmax      ,mmax      ,nmaxus    , &
                 & nmmax     ,lsed      ,lsedtot   ,gdp       )
    !
    ! Compute derived quantities
    !
    call initsedtra(gdp%gderosed, gdp%gdsedpar, gdp%gdtrapar, gdp%gdmorpar, gdp%gdmorlyr, &
                  & rhow, ag, vicmol, nmlb, nmub, nmmax, lsed, lsedtot)
end subroutine inised
