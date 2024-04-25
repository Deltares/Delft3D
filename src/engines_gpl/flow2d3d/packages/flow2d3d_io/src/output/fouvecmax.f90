subroutine fouvecmax(mmax       ,nmaxus      ,kcs        ,nofou     , &
                   & ifou       ,nst         ,gdp        )
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
!    Function: - Determines the maximum of the different vector parameters
!                1) Velocity         (u and v)           [m/s]
!                2) (Unit) Discharge (qxk and qyk)       [m3/m]
!                3) Bed shear stress (taubpu and taubpv) [N/m2]
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
    integer        , dimension(:)        , pointer :: ftmsto
    integer        , dimension(:)        , pointer :: ftmstr
    real(fp)       , dimension(:,:,:)    , pointer :: foucomp
    real(fp)       , dimension(:,:,:)    , pointer :: fouvec
    character(1)   , dimension(:)        , pointer :: fouelp
!
! Global variables
!
    integer                                                                   , intent(in) :: ifou      !!  Counter
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in) :: kcs       !! Description and declaration in esm_alloc_int.f90
    integer                                                                   , intent(in) :: mmax      !  Description and declaration in esm_alloc_int.f90
    integer                                                                   , intent(in) :: nmaxus    !  Description and declaration in esm_alloc_int.f90
    integer                                                                   , intent(in) :: nofou     !  Description and declaration in dimens.igs
    integer                                                                   , intent(in) :: nst       !!  Time step number
!
! Local variables
!
    integer         :: m         ! Loop counter over MMAX 
    integer         :: n         ! Loop counter over NMAXUS
    real(fp)        :: vecmagn   ! Magnitude of vector ( =sqrt(xcomp^2 + ycomp^2) )
!
!! executable statements -------------------------------------------------------
!
    ftmsto    => gdp%gdfourier%ftmsto
    ftmstr    => gdp%gdfourier%ftmstr
    foucomp   => gdp%gdfourier%foucomp
    fouvec    => gdp%gdfourier%fouvec
    fouelp    => gdp%gdfourier%fouelp
    !
    vecmagn  = 0.0_fp
    !
    ! Initialize for MAX = -1.E+30 / MIN = 1.E+30
    !
    if (nst==ftmstr(ifou)) then
       if (fouelp(ifou)=='x') then
          do n = 1, nmaxus
             do m = 1, mmax
                fouvec(n, m, ifou) = -1.0E+30
             enddo
          enddo
       elseif (fouelp(ifou)=='i') then
          do n = 1, nmaxus
             do m = 1, mmax
                fouvec(n, m, ifou) = 1.0E+30
             enddo
          enddo
       else
       endif
    endif
    !
    ! For every time step between FTMSTART and FTMSTOP
    !
    if (nst>=ftmstr(ifou) .and. nst<ftmsto(ifou)) then
       !
       ! Calculate MAX value
       !
       if (fouelp(ifou)=='x') then
          do n = 1, nmaxus
             do m = 1, mmax
                if (kcs(n,m) == 1) then
                   vecmagn = sqrt( foucomp(n, m, ifou)*foucomp(n, m, ifou) + foucomp(n, m, ifou+1)*foucomp(n, m, ifou+1) )
                   fouvec(n, m, ifou) = max( fouvec(n, m, ifou), vecmagn )
                endif
             enddo
          enddo
       !
       ! Calculate MIN value
       !
       elseif (fouelp(ifou)=='i') then
          do n = 1, nmaxus
             do m = 1, mmax
                if (kcs(n,m) == 1) then
                   vecmagn = sqrt( foucomp(n, m, ifou)*foucomp(n, m, ifou) + foucomp(n, m, ifou+1)*foucomp(n, m, ifou+1) )
                   fouvec(n, m, ifou) = min( fouvec(n, m, ifou), vecmagn )
                endif
             enddo
          enddo
       endif
    endif
end subroutine fouvecmax
