subroutine compthick(dps, s1, nmmax, gdp)
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
! Function:   Compute new thickness of transport and exchange layer
!
!!--declarations----------------------------------------------------------------
    use precision
    use bedcomposition_module
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                             , pointer :: lundia
    integer                             , pointer :: ttlform
    integer                             , pointer :: telform
    real(fp)                            , pointer :: ttlalpha
    real(fp)                            , pointer :: ttlmin
    real(fp), dimension(:)              , pointer :: duneheight
!
! Global variables
!
    integer                                     , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                                       :: istat
    integer                                       :: nm
    integer                             , pointer :: iunderlyr
    real(fp)         , dimension(:)     , pointer :: thtrlyr
!
!! executable statements -------------------------------------------------------
!
    lundia      => gdp%gdinout%lundia
    ttlalpha    => gdp%gdmorpar%ttlalpha
    ttlmin      => gdp%gdmorpar%ttlmin
    ttlform     => gdp%gdmorpar%ttlform
    telform     => gdp%gdmorpar%telform
    duneheight  => gdp%gdbedformpar%duneheight
    !
    istat = bedcomp_getpointer_integer(gdp%gdmorlyr,'IUnderLyr',iunderlyr)
    if (istat/=0) then
       call prterr(lundia, 'U021', 'Memory problem in COMPTHICK')
       call d3stop(1, gdp)
    endif
    !
    select case(iunderlyr)
    case(2)
       !
       istat = bedcomp_getpointer_realfp (gdp%gdmorlyr,'ThTrLyr'  ,thtrlyr  )
       if (istat/=0) then
          call prterr(lundia, 'U021', 'Memory problem in COMPTHICK')
          call d3stop(1, gdp)
       endif
       !
       ! Determine new transport layer thickness
       !
       select case(ttlform)
       case(2)
          !
          ! proportional to water depth
          !
          do nm = 1, nmmax
             thtrlyr(nm) = max(ttlalpha*(s1(nm)+real(dps(nm),fp)),ttlmin)
          enddo
       case(3)
          !
          ! proportional to dune height
          !
          do nm = 1, nmmax
             thtrlyr(nm) = max(ttlalpha*duneheight(nm),ttlmin)
          enddo
       case default
          !
          ! nothing to do: constant in time
          !
       endselect
       !
       ! Determine new exchange layer thickness
       !
       select case(telform)
          case(1)
          case default
       endselect
    case default
       !
       ! No active layers: nothing to do
       !
    endselect
end subroutine compthick
