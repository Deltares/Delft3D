!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

!> Contains the global data for all fixed weirs.
!! fxw is the array of cross section paths.
module m_fixedweirs
    use m_crspath
    implicit none

    type (tcrspath), allocatable    :: fxw(:)

    integer                         :: nfxw = 0
    integer, allocatable            :: lnfxw(:)              ! Links with fixed weirs (dim=nfxw)
    integer, allocatable            :: nfxwL(:)              ! fixed weirs on links   (dim=Lnx)
    double precision, allocatable   :: csfxw(:)              ! fixed weir direction
    double precision, allocatable   :: snfxw(:)              ! fixed weir direction
    double precision, allocatable   :: crestlxw(:)           ! crest length of a weir
    double precision, allocatable   :: crestlevxw(:)         ! crest level of a weir
    double precision, allocatable   :: shlxw(:)              ! sill height left of a weir
    double precision, allocatable   :: shrxw(:)              ! sill height right of a weir
    double precision, allocatable   :: taludlxw(:)           ! talud left of a weir
    double precision, allocatable   :: taludrxw(:)           ! talud right of a weir
    double precision, allocatable   :: vegxw(:)              ! vegetation code on a weir
    double precision, allocatable   :: weirdte(:)            ! loss coeff
    integer         , allocatable   :: iweirtxw(:)           ! weir type

    double precision                :: sillheightmin    = 0.0d0 ! waqua dams with both sillheights > sillheightmin go to fixedweirs.pli
                                                                ! the rest goes to
contains



!> Deletes all fixed weirs from fxw.
!! Does not free up memory, use m_crspath::deallocCrossSectionPaths for that.
subroutine delFixedWeirs()
    nfxw = 0
    ! Do not reset fxw data, just let it be overwritten later.
end subroutine delFixedWeirs

   end module m_fixedweirs
