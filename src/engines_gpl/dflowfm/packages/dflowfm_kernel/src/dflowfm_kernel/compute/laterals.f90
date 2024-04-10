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

   public reset_lateral
   public default_lateral
   public alloc_lateraldata
   public dealloc_lateraldata
   public average_concentrations_for_laterals
!!
!! Laterals
!!
   integer, parameter, public :: ILATTP_ALL = 0 !< Type code for laterals that apply to both 2D and 1D nodes.
   integer, parameter, public :: ILATTP_1D  = 1 !< Type code for laterals that only apply to 1D nodes.
   integer, parameter, public :: ILATTP_2D  = 2 !< Type code for laterals that only apply to 2D nodes.
   
   integer                      , target, public :: numlatsg          !< [-] nr of lateral discharge providers  {"rank": 0}
   double precision, allocatable, target, public :: qplat(:)          !< [m3/s] Lateral discharge of provider {"shape": ["numlatsg"]}
   double precision, allocatable, target, public :: qqlat(:)          !< [m3/s] Lateral discharge at xz,yz {"location": "face", "shape": ["ndx"]}
   double precision, allocatable, target, public :: balat(:)          !< [m2] total area of all cells in provider numlatsg {"shape": ["numlatsg"]}
   character(len=128), allocatable      , public :: lat_ids(:)        !< id of laterals {"shape": ["numlatsg"]}
   double precision, allocatable, target, public :: qplatCum(:)       !< [m3/s] Cumulative lateral discharge of provider {"shape": ["numlatsg"]}
   double precision, allocatable, target, public :: qplatCumPre(:)    !< [m3/s] Cumulative lateral discharge of provider at previous history output time{"shape": ["numlatsg"]}
   double precision, allocatable, target, public :: qplatAve(:)       !< [m3/s] Average lateral discharge of provider during the past history output interal {"shape": ["numlatsg"]}
   double precision, allocatable, target, public :: qLatReal(:)       !< [m3/s] Realized lateral discharge {"shape": ["numlatsg"]}
   double precision, allocatable, target, public :: qLatRealCum(:)    !< [m3/s] Cumulative realized lateral discharge {"shape": ["numlatsg"]}
   double precision, allocatable, target, public :: qLatRealCumPre(:) !< [m3/s] Cumulative realized lateral discharge at previous history output time{"shape": ["numlatsg"]}
   double precision, allocatable, target, public :: qLatRealAve(:)    !< [m3/s] Average realized lateral discharge during the past history output interal{"shape": ["numlatsg"]}
   
   !! Lateral lookup tables: n1/n2latsg(ilat) = n1/n2, nnlat(n1:n2) = { flow node nrs affected by lateral ilat }
   integer                              , public :: nlatnd      !< lateral nodes dimension, counter of nnlat(:)
   integer,          allocatable, target, public :: n1latsg(:)  !< [-] first  nlatnd point in lateral signal numlatsg {"shape": ["numlatsg"]}
   integer,          allocatable, target, public :: n2latsg(:)  !< [-] second nlatnd point in lateral signal numlatsg {"shape": ["numlatsg"]}
   integer,          allocatable, target, public :: nnlat(:)    !< [-] for each lateral node, flow node number == pointer to qplat/balat {"shape": ["nlatnd"]}
   integer,          allocatable, target, public :: kclat(:)    !< [-] for each cell: 0 when not accepting lateral discharge (e.g. pipe) {"location": "face", "shape": ["ndx"]}
   
   !! Lateral geometry variables
   integer                              , public :: nNodesLat           !< [-] Total number of geom nodes for all laterals.
   integer,          allocatable, target, public :: nodeCountLat(:)     !< [-] Count of nodes per lateral.
   double precision, allocatable, target, public :: geomXLat(:)         !< [m] x coordinates of laterals.
   double precision, allocatable, target, public :: geomYLat(:)         !< [m] y coordinates of laterals.
   
   private
   double precision, allocatable, target, dimension(:,:,:), public :: outgoing_lat_concentration   !< Average concentration per lateral discharge location.
   double precision, allocatable, target, dimension(:,:,:), public :: incoming_lat_concentration   !< Concentration of the inflowing water at the lateral discharge location.
   integer,          allocatable, target, dimension(:),     public :: apply_transport              !< Flag to apply transport for laterals (0 means only water and no substances are transported).

   contains

   !> Reset the defaults for laterals
   subroutine default_lateral()
      call reset_lateral()
   end subroutine default_lateral

   !> Reset the counters for lateral data.
   subroutine reset_lateral()
      numlatsg = 0           !< [] nr of lateral discharge providers
      nlatnd   = 0           !< lateral nodes dimension, counter of nnlat(:)
   end subroutine reset_lateral

   !> allocate the arrays for laterals on 3d/BMI
   subroutine alloc_lateraldata(numconst)
   
      use m_alloc
   
      integer, intent(in) :: numconst        !< number of constitiuents
      
      call realloc(incoming_lat_concentration, (/1, numconst, numlatsg/))
      call realloc(outgoing_lat_concentration, (/1, numconst, numlatsg/))

   end subroutine alloc_lateraldata

   !> deallocate the arrays for laterals on 3d/BMI
   subroutine dealloc_lateraldata()
   
      if (allocated(incoming_lat_concentration)) then
         deallocate(incoming_lat_concentration, outgoing_lat_concentration)
      endif
   

   end subroutine dealloc_lateraldata

   !> calculate the average concentration at laterals
   subroutine average_concentrations_for_laterals(numconst, kmx, bottom_area, constituents)

      integer                         , intent(in)    :: numconst       !< Number or constituents.
      integer                         , intent(in)    :: kmx            !< Number of layers (0 means 2d computation).
      double precision, dimension(:)  , intent(in)    :: bottom_area             !< Cell area.
      double precision, dimension(:,:), intent(in)    :: constituents   !< concentrations.

      integer :: ilat
      integer :: n, iconst, k, k1, kt, kb

      outgoing_lat_concentration = 0d0
      do ilat = 1, numlatsg
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
                  outgoing_lat_concentration(1, iconst, ilat) =  outgoing_lat_concentration(1, iconst, ilat) + bottom_area(k) * constituents(iconst, k)
               endif
            enddo
         enddo
         outgoing_lat_concentration(:,:, ilat) = outgoing_lat_concentration(:,:, ilat) / balat(ilat)
      enddo

   end subroutine average_concentrations_for_laterals
end module m_lateral
