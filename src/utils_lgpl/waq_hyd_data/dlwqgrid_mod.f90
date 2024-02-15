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

      module dlwqgrid_mod
      use m_srstop

!
!          module contains everything for specification of multiple grids
!          created 17 October 2002 by Leo Postma
!
!     contains the fiollowing derived types:
!          GridPointer            ! a set of information with respect to one grid pointer
!          GridPointerColl        ! a collection of these grid pointers
!
!     contains the following functions:
!          GridPointerCollFind    ! to search a Grid in the GridPointerColl ; returns the index or zero if not found
!          GridPointerCollAdd     ! to add a GridPointer to the collection ; returns the current size
!
!     contains the following subroutine:
!
!
      integer, parameter :: NAME_SIZE      =  20                ! size of descriptive names
      integer, parameter :: MAX_NUM        =   5                ! allocated per bunch
!
      integer, parameter :: BaseGrid        =  1                ! implementation of an enumeration
      integer, parameter :: ProcessGrid     =  2                !               type in Fortran
      integer, parameter :: BottomGrid      =  3
      integer, parameter :: AggregationFile =  4
      integer, parameter :: NolayGrid       =  5
      integer, parameter ::              NrGridTypes    =  5
      character*20        GridTypes( NrGridTypes )
      DATA GridTypes / 'BASEGRID', 'PROCESSGRID', 'BOTTOMGRID' , 'AGGREGATIONFILE' , 'NOLAY' /
!
!          this is the grid pointer itself
!
      type GridPointer
         character(len=NAME_SIZE)         :: name               ! name of the grid
         integer                          :: noseg              ! number of segments
         integer                          :: noseg_lay          ! number of segments per layer / 2D
         integer                          :: iref               ! grid reference nr
         character(len=NAME_SIZE)         :: name_ref           ! name of the reference grid
         integer                          :: itype              ! type of grid
         integer, pointer                 :: iarray(:)          ! the pointer to reference the reference grid
         integer, pointer                 :: finalpointer(:)    ! the pointer to the final grid
         logical                          :: space_var_nolay    ! switch for space varying nr of layers
         integer                          :: nolay              ! nr of expandable layers
         integer, pointer                 :: nolay_var(:)       ! space varying nr of layers if any
      end type GridPointer
!
!          this is the collection of the grid pointers
!
      type GridPointerColl
         type(GridPointer), pointer       :: Pointers(:)        ! array with gridpointers
         integer                          :: maxsize            ! maximum size of the current array
         integer                          :: cursize            ! filled up to this size
         integer                          :: base_grid          ! index base grid in collection
         integer                          :: bottom_grid        ! index bottom grid in collection
      end type GridPointerColl

      contains

!          function to find a grid name in a collection of GridPointers

      function GridPointerCollFind( aGridPointerColl, name ) result ( iret )
!
         type(GridPointerColl)            :: aGridPointerColl
         character(LEN=*)                 :: name
         integer                          :: iret
!
         iret = 0
         do i = 1 , aGridPointerColl%cursize         ! search by name
            if ( aGridPointerColl%Pointers(i)%name .eq. name ) then
               iret = i
               return
            endif
         end do
!
      end function GridPointerCollFind
!
!          function to add to a collection of fileproperties
!
      function GridPointerCollAdd( aGridPointerColl , aGridPointer ) result ( cursize )
!
         type(GridPointerColl)               :: aGridPointerColl      ! the collection of GridPointers
         type(GridPointer)                   :: aGridPointer          ! the GridPointer to add to the collection
         integer                             :: cursize               ! return value the new current collection size
                                                                      ! and the index of the added GridPointer

         type(GridPointer), pointer          :: aGridPointerPnts(:)   ! should be a pointer for the resize operation

         if ( aGridPointerColl%cursize .eq. aGridPointerColl%maxsize ) then
            allocate ( aGridPointerPnts ( aGridPointerColl%maxsize + MAX_NUM ) )
            do i = 1 , aGridPointerColl%maxsize
               aGridPointerPnts(i) = aGridPointerColl%Pointers(i)        ! copies the pointers
            enddo
            if ( aGridPointerColl%maxsize .ne. 0 ) deallocate ( aGridPointerColl%Pointers )
            aGridPointerColl%Pointers => aGridPointerPnts                   ! attaches this new array of pointers
            aGridPointerColl%maxsize = aGridPointerColl%maxsize + MAX_NUM
         endif

         aGridPointerColl%cursize = aGridPointerColl%cursize + 1
         aGridPointerColl%Pointers(aGridPointerColl%cursize ) = aGridPointer
         cursize = aGridPointerColl%cursize
         return
!
      end function GridPointerCollAdd

      function GridWrite( ilun, aGrid ) result ( ierror )
!
         integer, intent(in)                :: ilun         ! unit number binary file with data
         type(GridPointer), intent(in)      :: aGrid        ! datastructure to be written
         integer                            :: ierror       !
!
         ierror  = 0

         write(ilun, err = 100 ) aGrid%name
         write(ilun, err = 100 ) aGrid%noseg
         write(ilun, err = 100 ) aGrid%noseg_lay
         write(ilun, err = 100 ) aGrid%iref
         write(ilun, err = 100 ) aGrid%name_ref
         write(ilun, err = 100 ) aGrid%itype
         write(ilun, err = 100 ) aGrid%finalpointer
         write(ilun, err = 100 ) aGrid%space_var_nolay
         write(ilun, err = 100 ) aGrid%nolay
         if ( aGrid%space_var_nolay ) then
            write(ilun, err = 100 ) aGrid%nolay_var
         endif

         return
!
  100    continue

         ierror = 1
         return

      end function GridWrite

      function GridRead( ilun, aGrid, noseg ) result ( ierror )
!
         integer, intent(in)                :: ilun         ! unit number binary file with data
         type(GridPointer), intent(out)     :: aGrid        ! datastructure to be filled
         integer, intent(in)                :: noseg        ! number of segments in base grid (perhaps remove this dependency by adding the length of the pointer to the structure)
         integer                            :: ierror       !
!
         ierror  = 0

         read(ilun, err = 100 ) aGrid%name
         read(ilun, err = 100 ) aGrid%noseg
         read(ilun, err = 100 ) aGrid%noseg_lay
         read(ilun, err = 100 ) aGrid%iref
         read(ilun, err = 100 ) aGrid%name_ref
         read(ilun, err = 100 ) aGrid%itype
         allocate ( aGrid%finalpointer(noseg) , stat = ierr_alloc)
         if ( ierr_alloc .ne. 0 ) then
            write(*,*) 'ERROR : allocating array in GridRead'
            call srstop(1)
         endif
         read(ilun, err = 100 ) aGrid%finalpointer
         read(ilun, err = 100 ) aGrid%space_var_nolay
         read(ilun, err = 100 ) aGrid%nolay
         if ( aGrid%space_var_nolay ) then
            allocate ( aGrid%nolay_var(aGrid%noseg_lay) , stat = ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then
               write(*,*) 'ERROR : allocating array in GridRead'
               call srstop(1)
            endif
            read(ilun, err = 100 ) aGrid%nolay_var
         endif

         return
!
  100    continue

         ierror = 1
         return

      end function GridRead

      end module dlwqgrid_mod
