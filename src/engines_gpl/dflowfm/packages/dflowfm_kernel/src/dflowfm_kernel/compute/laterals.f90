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
module m_lateral

implicit none

   public alloc_lateraldata
   public dealloc_lateraldata
   public average_concentrations_for_laterals

   private
   double precision, allocatable, target, dimension(:,:,:), public :: outgoing_lat_concentration   !< average concentration per lateral discharge location
   double precision, allocatable, target, dimension(:,:,:), public :: incoming_lat_concentration   !< concentration of the inflowing water at the lateral discharge location

   contains

   !> allocate the arrays for laterals on 3d/BMI
   subroutine alloc_lateraldata(numconst)
   
      use m_wind

      integer, intent(in) :: numconst        !< number of constitiuents
      
      allocate(incoming_lat_concentration(1, numconst, numlatsg), outgoing_lat_concentration(1, numconst, numlatsg))

   end subroutine alloc_lateraldata

   !> deallocate the arrays for laterals on 3d/BMI
   subroutine dealloc_lateraldata()
   
      use m_wind

      if (allocated(incoming_lat_concentration)) then
         deallocate(incoming_lat_concentration, outgoing_lat_concentration)
      endif
   

   end subroutine dealloc_lateraldata

   !> calculate the average concentration at laterals
   subroutine average_concentrations_for_laterals(numconst, kmx, vol1, constituents)
      use m_wind, only: nnlat, numlatsg, n1latsg, n2latsg

      integer                         , intent(in)    :: numconst       !< Number or constituents.
      integer                         , intent(in)    :: kmx            !< Number of layers (0 means 2d computation).
      double precision, dimension(:)  , intent(in)    :: vol1           !< Cell volume.
      double precision, dimension(:,:), intent(in)    :: constituents   !< concentrations.

      integer :: ilat
      integer :: n, iconst, k, k1, kt, kb

      double precision :: total_volume, salt 

      outgoing_lat_concentration = 0d0
      do ilat = 1, numlatsg
         total_volume = 0d0
         do iconst = 1, numconst
            do k1 = n1latsg(ilat), n2latsg(ilat)
               n = nnlat(k1)
               if (n > 0) then
                  if (kmx < 1) then 
                     k = n
                  else
                     ! For now we only use the top layer 
                     call getkbotktop(n, kb, kt)
                     k = kt
                  endif
                  total_volume = total_volume + vol1(k)
                  salt = constituents(iconst, k)
                  outgoing_lat_concentration(1, iconst, ilat) = vol1(k) * constituents(iconst, k)
               endif
            enddo
         enddo
         outgoing_lat_concentration(:,:, ilat) = outgoing_lat_concentration(:,:, ilat) / total_volume
      enddo

   end subroutine average_concentrations_for_laterals
end module m_lateral
