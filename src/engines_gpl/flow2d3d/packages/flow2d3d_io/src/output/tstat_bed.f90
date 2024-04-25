subroutine tstat_bed(nostat, lsedtot, nmax, zdpsed, zbdsed, gdp)
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
!    Function: Determines the bed composition at stations
!
! Method used:
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
    integer          , dimension(:,:)   , pointer :: mnstat
!
! Global variables
!
    integer                               , intent(in)  :: nostat
    integer                               , intent(in)  :: nmax
    integer                               , intent(in)  :: lsedtot
    real(fp)  , dimension(nostat)         , intent(out) :: zdpsed !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, lsedtot), intent(out) :: zbdsed !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                             :: istat
    integer                             :: l
    integer                             :: m
    integer                             :: n
    integer                             :: nm
    integer                             :: ii
    real(prec), dimension(:,:), pointer :: bodsed
    real(fp)  , dimension(:)  , pointer :: dpsed
!
!! executable statements -------------------------------------------------------
!
    mnstat      => gdp%gdstations%mnstat
    !
    istat = bedcomp_getpointer_realprec(gdp%gdmorlyr,'bodsed',bodsed)
    if (istat == 0) then
       istat = bedcomp_getpointer_realfp(gdp%gdmorlyr,'dpsed',dpsed)
    endif
    if (istat /= 0) return
    !
    zdpsed = -999.0_fp
    zbdsed = -999.0_fp
    do ii = 1, nostat
       m  = mnstat(1,ii)
       if (m<0) cycle
       n  = mnstat(2,ii)
       if (n<0) cycle
       !
       call n_and_m_to_nm(n, m, nm, gdp)
       !
       zdpsed(ii) = dpsed(nm)
       do l = 1, lsedtot
          zbdsed(ii,l) = real(bodsed(l, nm),fp)
       enddo
    enddo
end subroutine tstat_bed
