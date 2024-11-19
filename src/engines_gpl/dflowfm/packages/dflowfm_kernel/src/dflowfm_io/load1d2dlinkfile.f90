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

module m_load1d2dlinkfile

implicit none

contains

   !> Reads custom parameters for 1D2D links from a *.ini file,
   !! and assigns them to the correct flow links.
   subroutine load1D2DLinkFile(filename)
      use string_module, only: strcmpi
      use m_flowgeom, only: lnx1d, kcu, wu1D2D, hh1D2D, lnx, lnx1D
      use m_inquire_flowgeom
      use properties
      use unstruc_messages
      use timespace
      use unstruc_model, only: File1D2DLinkMajorVersion, File1D2DLinkMinorVersion
      use m_linktypetoint

      implicit none

      character(len=*), intent(in) :: filename !< Name of *.ini file containing 1D2D link parameters.

      type(tree_data), pointer :: md_ptr
      type(tree_data), pointer :: node_ptr
      integer :: istat
      integer :: numblocks
      integer :: i

      character(len=IdLen) :: contactId
      character(len=IdLen) :: contactType
      integer :: icontactType

      logical :: success
      integer :: major, minor, ierr
      integer :: numcoordinates
      double precision, allocatable :: xcoordinates(:), ycoordinates(:)
      integer :: loc_spec_type

      integer :: numcontactblocks, numok
      integer, allocatable :: ke1d2dprops(:)
      integer :: num1d2dprops
      integer :: LL, Lf
      double precision :: wu1D2Dread, hh1D2Dread

      call tree_create(trim(filename), md_ptr)
      call prop_file('ini', trim(filename), md_ptr, istat)

      ! check FileVersion
      ierr = 0
      major = 0
      minor = 0
      call get_version_number(md_ptr, major=major, minor=minor, success=success)
      if (.not. success .or. major < File1D2DLinkMajorVersion) then
         write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported format of 1D2DLinkFile detected in '''//trim(filename)//''': v', major, minor, '. Current format: v', File1D2DLinkMajorVersion, File1D2DLinkMinorVersion, '. Ignoring this file.'
         call warn_flush()
         ierr = 1
      end if

      if (ierr /= 0) then
         goto 999
      end if

      allocate (ke1d2dprops(lnx1D))

      numblocks = 0
      if (associated(md_ptr%child_nodes)) then
         numblocks = size(md_ptr%child_nodes)
      end if

      numcontactblocks = 0
      numok = 0
      do i = 1, numblocks
         node_ptr => md_ptr%child_nodes(i)%node_ptr

         if (strcmpi(tree_get_name(node_ptr), 'MeshContactParams')) then
            numcontactblocks = numcontactblocks + 1

            ! Read Data
            contactType = 'all'
            call prop_get(node_ptr, '', 'contactType', contactType, success)
            icontactType = linkTypeToInt(contactType)
            if (icontactType < 0) then
               write (msgbuf, '(a,i0,a)') 'Error reading mesh contact parameters from block #', numcontactblocks, ' in file '''// &
                  trim(filename)//'''. Invalid contactType '''//trim(contactType)//''' given.'
               call err_flush()
               success = .false.
               cycle
            end if

            call prop_get(node_ptr, '', 'contactId', contactId, success)
            if (success) then ! the contact is defined by contactId
               loc_spec_type = LOCTP_CONTACTID
            else ! the contact is defined by x, y coordinates and contactType
               call prop_get(node_ptr, '', 'numCoordinates', numcoordinates, success)
               if (success .and. numcoordinates > 0) then
                  allocate (xcoordinates(numcoordinates), stat=ierr)
                  allocate (ycoordinates(numcoordinates), stat=ierr)
                  call prop_get(node_ptr, '', 'xCoordinates', xcoordinates, numcoordinates, success)
                  if (success) then
                     call prop_get(node_ptr, '', 'yCoordinates', ycoordinates, numcoordinates, success)
                  end if
                  if (success) then
                     loc_spec_type = LOCTP_POLYGON_XY
                  end if
               end if
            end if

            if (.not. success) then
               write (msgbuf, '(a,i0,a)') 'Error Reading mesh contact parameters from block #', numcontactblocks, ' in file '''// &
                  trim(filename)//'''. No contactId or coordinates specified.'
               call err_flush()
               cycle
            end if

            num1d2dprops = 0
            call selectelset_internal_links(lnx, ke1d2dprops(1:lnx1D), num1d2dprops, &
                                            loc_spec_type, nump=numcoordinates, xpin=xcoordinates, ypin=ycoordinates, &
                                            contactId=contactId, linktype=icontactType)

            if (loc_spec_type == LOCTP_CONTACTID .and. num1d2dprops == 1) then
               Lf = ke1d2dprops(1)
               if (icontactType /= IFLTP_ALL .and. icontactType /= kcu(Lf)) then
                  write (msgbuf, '(a,i0,a,i0,a)') 'Error Reading mesh contact parameters from block #', numcontactblocks, ' in file '''// &
                     trim(filename)//'''. Given contactType='//trim(contactType)// &
                     ' does not match the flow link type ', kcu(Lf), '.'
                  call err_flush()
                  cycle
               else
                  ! Auto-determine contactType from this single flow link's type.
                  icontactType = kcu(Lf)
               end if
            end if

            select case (icontactType)
            case (IFLTP_1D2D_STREET)
               call prop_get(node_ptr, '', 'openingWidth', wu1D2Dread, success)
               if (.not. success) then
                  write (msgbuf, '(a,i0,a)') 'Error Reading mesh contact parameters from block #', numcontactblocks, ' in file '''// &
                     trim(filename)//'''. No openingWidth specified.'
                  call err_flush()
                  cycle
               end if

               call prop_get(node_ptr, '', 'openingHeight', hh1D2Dread, success)
               if (.not. success) then
                  write (msgbuf, '(a,i0,a)') 'Error Reading mesh contact parameters from block #', numcontactblocks, ' in file '''// &
                     trim(filename)//'''. No openingHeight specified.'
                  call err_flush()
                  cycle
               end if
            end select

            do LL = 1, num1d2dprops
               Lf = ke1d2dprops(LL)
               wu1D2D(Lf) = wu1D2Dread
               hh1D2D(Lf) = hh1D2Dread
            end do

            numok = numok + 1
         end if
      end do

      write (msgbuf, '(i0,a,i0,2a)') numok, ' of ', numcontactblocks, ' mesh contact parameter blocks have been read from file ', trim(filename)
      call msg_flush()

999   continue
      call tree_destroy(md_ptr)

   end subroutine load1D2DLinkFile

end module m_load1d2dlinkfile
