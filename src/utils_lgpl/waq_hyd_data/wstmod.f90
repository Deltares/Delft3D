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

      module wstmod

      ! module contains everything for the wastloads discription
      ! created June 2004 by Jan van Beek

      implicit none

      integer, parameter, private               :: NAME_SIZE  =  256      ! size of descriptive names
      integer, parameter, private               :: MAX_NUM    =    5      ! allocated per bunch

      ! wateload types

      integer, parameter                        :: DLWQ_WASTE_NORMAL = 1  ! normal wasteload
      integer, parameter                        :: DLWQ_WASTE_INLET  = 2  ! inlet wasteload
      integer, parameter                        :: DLWQ_WASTE_OUTLET = 3  ! outlet wasteload
      integer, parameter                        :: DLWQ_WASTE_WALK   = 4  ! walking wasteload

      type t_wasteload
         character(len=NAME_SIZE)               :: name                   ! name of wasteload
         character(len=NAME_SIZE)               :: waqtype                ! waqtype of wasteload
         integer                                :: m                      ! m coordinate
         integer                                :: n                      ! n coordinate
         integer                                :: k                      ! k coordinate
         integer                                :: type                   ! type of wasteload
      end type t_wasteload

      type t_wasteload_coll
         type(t_wasteload), pointer             :: wasteload_pnts(:)      ! pointer to the wasteloads
         integer                                :: maxsize                ! maximum size of the current array
         integer                                :: cursize                ! filled up to this size
         integer                                :: no_flow                ! total number of flows
         logical                                :: l_seconds              ! if time is in seconds or ddhhmmss
      end type t_wasteload_coll

      contains

      ! function to find a wasteload name in a collection, case sensitive at the moment

      function wasteload_coll_find( wasteload_coll, name ) result ( iret )

         type(t_wasteload_coll)                 :: wasteload_coll         ! collection of wasteloads
         character(LEN=*)                       :: name                   ! name of wasteload to be found
         integer                                :: iret                   ! result index in collection or 0 if not found

         integer                                :: i                      ! loop counter

         iret = 0
         do i = 1 , wasteload_coll%cursize
            if ( wasteload_coll%wasteload_pnts(i)%name .eq. name ) then
               iret = i
               return
            endif
         end do

      end function wasteload_coll_find

      ! function to add to a collection of wasteloads (copy)

      function wasteload_coll_add( wasteload_coll , wasteload ) result ( cursize )

         type(t_wasteload_coll)                 :: wasteload_coll         ! collection of wasteloads
         type(t_wasteload)                      :: wasteload              ! wasteload to be added
         integer                                :: cursize                ! return value the new current collection size
                                                                          ! and the index of the added wasteload

         type(t_wasteload), pointer             :: wasteload_pnts(:)      ! pointer for the resize operation
         integer                                :: i                      ! loop counter

         if ( wasteload_coll%cursize .eq. wasteload_coll%maxsize ) then

            ! resize, allocate new array

            allocate ( wasteload_pnts ( wasteload_coll%maxsize + MAX_NUM ) )

            ! copy the wasteloads into the new array

            do i = 1 , wasteload_coll%maxsize
               wasteload_pnts(i) = wasteload_coll%wasteload_pnts(i)   ! copies the wasteloads
            enddo

            ! deallocate the old array and attach the new array to the collection

            if ( wasteload_coll%maxsize .ne. 0 ) deallocate ( wasteload_coll%wasteload_pnts )
            wasteload_coll%wasteload_pnts => wasteload_pnts
            wasteload_coll%maxsize = wasteload_coll%maxsize + MAX_NUM

         endif

         wasteload_coll%cursize = wasteload_coll%cursize + 1
         wasteload_coll%wasteload_pnts(wasteload_coll%cursize ) = wasteload
         cursize = wasteload_coll%cursize
         return

      end function wasteload_coll_add


      end module wstmod
