!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

!! Module DELWAQ2_DATA:
!! Define the derived type holding all the information
!!
      module delwaq2_data

      use hydroset
      use dlwq_hyd_data
      use m_waq_data_buffer
      use m_operation_data
      use dlwqgrid_mod

      integer, parameter, private                 :: iisize = 21  ! from sysi.inc
      integer, parameter, private                 :: insize = 72  ! from sysn.inc

      type delwaq_data
          type(waq_data_buffer)                       :: buffer
          integer, dimension(:), pointer              :: iwstkind   ! steers flow-concentration processing
          integer, dimension(:,:), pointer            :: iexseg     ! zero if volume is explicit
          integer, dimension(:,:), pointer            :: iknmkv     ! time variable feature array (for drying/flooding)

          integer, dimension(insize)                  :: in
          integer, dimension(iisize)                  :: ii
          integer                                     :: itime
          integer                                     :: ifflag
          integer                                     :: iaflag
          integer                                     :: ibflag
          integer                                     :: nddim
          integer                                     :: nvdim
          integer                                     :: nosss
          integer                                     :: noqtt
          integer                                     :: noqt
          integer                                     :: nopred
          integer                                     :: itimel
          integer                                     :: ithandl = 0 ! needs to be zero at the start
          integer                                     :: inwtyp
          integer                                     :: nowarn
          integer                                     :: ioptzb
          integer                                     :: lleng
          logical                                     :: lstrec
          logical                                     :: forester
          logical                                     :: updatr
          logical                                     :: litrep
          logical                                     :: ldummy
          type(operation_data), dimension(:), pointer :: operation => null()
          integer                                     :: number_operations = 0
          real(kind=kind(1.0d0))                      :: tol
          real(kind=kind(1.0d0))                      :: otime
          real(kind=kind(1.0d0))                      :: deltim
          real(kind=kind(1.0d0))                      :: tscale
          logical                                     :: set_timer = .false.
          logical                                     :: inopenda  = .false.

          !
          ! Components from syst.inc
          !
          logical                                     :: bndset
          logical                                     :: wstset
          logical                                     :: funset
          logical                                     :: othset
          integer                                     :: ibndmx
          integer                                     :: iwstmx
          integer                                     :: ifunmx

          !
          ! Components for dealing with the time-dependent data from files
          !
          type(FilePropColl)          :: PropColl
          type(FileUseDefColl)        :: UseDefColl
          type(FileUseDefCollColl)    :: CollColl

          !
          ! Temporary component for dealing with OpenDA multiple instances
          !
          type(FilePropColl), pointer, dimension(:) :: PropCollArray => null()
          !
          ! All the process parameters data from file
          !
          type(t_dlwqdatacoll)     :: proc_pars

          !
          ! Collection of all grid definitions
          !
          type(GridPointerColl)    :: GridPs
      end type delwaq_data

      contains

      ! copy_time_data
      !     Routine to copy to and from the syst time data
      !
      subroutine copy_time_data( dlwqd, todlwqd )

      use m_syst

      type(delwaq_data), intent(inout) :: dlwqd
      logical                          :: todlwqd

      if ( todlwqd ) then
          dlwqd%bndset = bndset
          dlwqd%wstset = wstset
          dlwqd%funset = funset
          dlwqd%othset = othset
          dlwqd%ibndmx = ibndmx
          dlwqd%iwstmx = iwstmx
          dlwqd%ifunmx = ifunmx
      else
          bndset = dlwqd%bndset
          wstset = dlwqd%wstset
          funset = dlwqd%funset
          othset = dlwqd%othset
          ibndmx = dlwqd%ibndmx
          iwstmx = dlwqd%iwstmx
          ifunmx = dlwqd%ifunmx
      endif
      end subroutine copy_time_data

      end module delwaq2_data
