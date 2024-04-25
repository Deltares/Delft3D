subroutine chkstr(lundia    ,nmax      ,mmax      ,nmaxus    , &
                & kmax      ,kcu       ,kcv       ,kspu      ,kspv      , &
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
!    Function: - Check position structure for Local Weirs, Rigid Sheets, (Fixed) Gates and
!                Barrier points, which are defined in the U/V velocity points
!              NOTE: Floating structure should be on active grid point (KCS=1)
!                    which is tested in CALPSH
!                    Discharge location should be on active grid point (KCS=1)
!                    which is tested in CHKDIS
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
! Local parameters
!
    integer, parameter :: mxstr = 11
!
! Global variables
!
    integer                                                             , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                           :: lundia !  Description and declaration in inout.igs
    integer                                                             , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                           :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax), intent(in)  :: kspu   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax), intent(in)  :: kspv   !  Description and declaration in esm_alloc_int.f90
!
!
! Local variables
!
    integer                         :: ksp    ! =0 if structure is on active u/v- point >2 structure on thin dam point 
    integer                         :: m      ! Loop counter MMAX 
    integer                         :: n      ! Loop counter NMAXUS 
    character(18), dimension(mxstr) :: struct ! Array containing names of defined structure 
!
!
!
    data (struct(m), m = 1, mxstr)/'discharge location', 'floating structure',  &
         & 'local weir        ', 'gate              ', 'rigid sheet       ',     &
         & 'porous plate      ', 'bridge            ', 'barrier           ',     &
         & '2D weir           ', 'fixed 3d gate     ', 'UNKNOWN STRUCTURE '/
!
!! executable statements -------------------------------------------------------
!
    do m = 1, mmax
       do n = 1, nmaxus
          ksp = abs(kspu(n, m, 0))*(1 - abs(kcu(n, m)))
          if (ksp>2) then
             call prterr(lundia    ,'V249'    ,struct(min(ksp, mxstr))         )
             write (lundia, '(20x,''(M,N) = '',2i4)') m, n
          endif
          ksp = abs(kspv(n, m, 0))*(1 - abs(kcv(n, m)))
          if (ksp>2) then
             call prterr(lundia    ,'V249'    ,struct(min(ksp, mxstr))         )
             write (lundia, '(20x,''(M,N) = '',2i4)') m, n
          endif
       enddo
    enddo
end subroutine chkstr
