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

!< @file
!! Collect the information on the internal state. Also provide support for
!! OpenDA and Delta Shell influencing the computation's parameters.
!!
!> Constants used in the interface to DELWAQ
      module waq_omi_constants

          implicit none

          integer, parameter         :: DLWQ_CONSTANT          = 1  !< Constant in the processes library (one value)
          integer, parameter         :: DLWQ_PARAMETER         = 2  !< Process parameter varying per cell (noseg values)
          integer, parameter         :: DLWQ_CONCENTRATION     = 3  !< Current concentration of a substance (noseg values)
          integer, parameter         :: DLWQ_BOUNDARY_VALUE    = 4  !< Concentration at a single boundary (one value)
          integer, parameter         :: DLWQ_BOUNDARY_SECTION  = 5  !< Concentration at a boundary section
          integer, parameter         :: DLWQ_DISCHARGE         = 6  !< Concentration at a single discharge location (one value)
          integer, parameter         :: DLWQ_MONITOR_POINT     = 7  !< Concentration at a single monitoring location (one value)
          integer, parameter         :: DLWQ_ACTIVE_SUBSTANCES = 8  !< Retrieve number of transportable (active) substances
          integer, parameter         :: DLWQ_ALL_SUBSTANCES    = 9  !< Retrieve number of all substances

          integer, parameter         :: DLWQ_ALL_CONCENTRATIONS    = 10  !Current concentrations of all substances (notot*noseg values)


          integer, parameter         :: DLWQ_FREE           = -1  ! Special "operation", used for administrative purposes
          integer, parameter         :: DLWQ_SET            = 1
          integer, parameter         :: DLWQ_ADD            = 2
          integer, parameter         :: DLWQ_MULTIPLY       = 3

! copied from openDA:
          integer, parameter ::  ODA_ALL_SEGMENTS            = -1
          integer, parameter ::  ODA_PAR_TYPE_SUBSTANCE      =  1
          integer, parameter ::  ODA_PAR_TYPE_GLOBALPAR      =  2
          integer, parameter ::  ODA_PAR_TYPE_LOCALPAR       =  3
          integer, parameter ::  ODA_PAR_TYPE_ALL_SUBSTANCES =  4
          integer, parameter ::  ODA_LOC_TYPE_SEGMENT        =  1
          integer, parameter ::  ODA_LOC_TYPE_BOUNDARY       =  2
          integer, parameter ::  ODA_LOC_TYPE_DISCHARGE      =  3
          integer, parameter ::  ODA_LOC_TYPE_ALL_SEGMENTS   =  4
          integer, parameter ::  ODA_LOC_TYPE_ALL_BOUNDARIES =  5
          integer, parameter ::  ODA_LOC_TYPE_ALL_DISCHARGES =  6


      contains

      subroutine odatype_2_dlwq(oda_partype, oda_loctype, dlwq_type)

          implicit none

          integer, intent(in)  :: oda_partype, oda_loctype
          integer, intent(out) :: dlwq_type

          dlwq_type = -1;

          if (oda_partype == ODA_PAR_TYPE_SUBSTANCE .and.
     &         oda_loctype == ODA_LOC_TYPE_ALL_SEGMENTS) then
              dlwq_type = DLWQ_CONCENTRATION
          elseif (oda_partype == ODA_PAR_TYPE_LOCALPAR .and.
     &         oda_loctype == ODA_LOC_TYPE_ALL_SEGMENTS) then
              dlwq_type = DLWQ_PARAMETER
          elseif (oda_partype == ODA_PAR_TYPE_GLOBALPAR .and.
     &         oda_loctype == ODA_LOC_TYPE_ALL_SEGMENTS) then
              dlwq_type = DLWQ_CONSTANT
          elseif (oda_partype == ODA_PAR_TYPE_SUBSTANCE .and.
     &         oda_loctype == ODA_LOC_TYPE_BOUNDARY) then
              dlwq_type = DLWQ_BOUNDARY_VALUE
          elseif (oda_partype == ODA_PAR_TYPE_SUBSTANCE .and.
     &         oda_loctype == ODA_LOC_TYPE_DISCHARGE) then
              dlwq_type = DLWQ_DISCHARGE
          elseif (oda_partype == ODA_PAR_TYPE_SUBSTANCE .and.
     &         oda_loctype == ODA_LOC_TYPE_SEGMENT) then
              dlwq_type = DLWQ_MONITOR_POINT
          elseif (oda_partype == ODA_PAR_TYPE_ALL_SUBSTANCES .and.
     &         oda_loctype == ODA_LOC_TYPE_ALL_SEGMENTS) then
              dlwq_type = DLWQ_ALL_CONCENTRATIONS
          else
              write (*,*) ' Wrong combination of ODA-enumeration: '
              write (*,*) ' parameter type: ', oda_partype
              write (*,*) ' location type: ', oda_loctype
              ! stop
          endif

      end subroutine odatype_2_dlwq


      subroutine dlwq_2_odatype(dlwq_type, oda_partype, oda_loctype)

          implicit none

          integer, intent(in) :: dlwq_type
          integer, intent(out)  :: oda_partype, oda_loctype

          if (dlwq_type == DLWQ_CONSTANT) then
              oda_partype = ODA_PAR_TYPE_GLOBALPAR
              oda_loctype = ODA_LOC_TYPE_ALL_SEGMENTS
          elseif (dlwq_type == DLWQ_PARAMETER) then
              oda_partype = ODA_PAR_TYPE_LOCALPAR
              oda_loctype = ODA_LOC_TYPE_ALL_SEGMENTS
          elseif (dlwq_type == DLWQ_CONCENTRATION) then
              oda_partype = ODA_PAR_TYPE_SUBSTANCE
              oda_loctype = ODA_LOC_TYPE_ALL_SEGMENTS
          elseif (dlwq_type == DLWQ_BOUNDARY_VALUE) then
              oda_partype = ODA_PAR_TYPE_SUBSTANCE
              oda_loctype = ODA_LOC_TYPE_BOUNDARY
          elseif (dlwq_type == DLWQ_DISCHARGE) then
              oda_partype = ODA_PAR_TYPE_SUBSTANCE
              oda_loctype = ODA_LOC_TYPE_DISCHARGE
          elseif (dlwq_type == DLWQ_MONITOR_POINT) then
              oda_partype = ODA_PAR_TYPE_SUBSTANCE
              oda_loctype = ODA_LOC_TYPE_SEGMENT
          elseif (dlwq_type == DLWQ_ALL_CONCENTRATIONS) then
              oda_partype = ODA_PAR_TYPE_ALL_SUBSTANCES
              oda_loctype = ODA_LOC_TYPE_ALL_SEGMENTS
          else
              write (*,*) ' Wrong combination of dlwq-enumeration: '
              write (*,*) '  type: ', dlwq_type
            ! stop
          endif


      end subroutine dlwq_2_odatype



      end module waq_omi_constants

!! Module DELWAQ2_DATA:
!! Define the derived type holding all the information
!!
      module delwaq2_data

      use waq_omi_constants
      use report_progress
      use hydroset
      use dlwq_data
      use grids

      integer, parameter, private                 :: iisize = 21  ! from sysi.inc
      integer, parameter, private                 :: insize = 72  ! from sysn.inc

      type operation_data
          integer, dimension(6)                   :: index
          integer                                 :: number_values
          integer                                 :: operation
          real, dimension(:), pointer             :: new_value => null()
          real                                    :: new_scalar
      end type operation_data

      type delwaq_data
          integer, dimension(:), pointer              :: ibuf
          real, dimension(:), pointer                 :: rbuf
          character(len=1), dimension(:), pointer     :: chbuf
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
          type(progress_data)                         :: progress
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

      !> Routine to apply all operations that are in the buffer. The operations
      !! are set to inactive when done
      subroutine apply_operations( dlwqd )

          implicit none

          type(delwaq_data) :: dlwqd          !< Structure holding all state information

          integer                     :: i
          integer                     :: j
          integer                     :: k
          integer                     :: index
          integer                     :: idxmass
          integer                     :: number
          integer                     :: number_values
          integer                     :: step
          integer                     :: volcorr
          integer                     :: kvol
          integer                     :: kconc
          integer                     :: kmass
          integer                     :: idxvol


          real                        :: scalar_value
          real, dimension(:), pointer :: vector_value

          do i = 1,dlwqd%number_operations

              index         =  dlwqd%operation(i)%index(1)
              step          =  dlwqd%operation(i)%index(2)
              number        =  dlwqd%operation(i)%index(3)
              idxmass       =  dlwqd%operation(i)%index(4)
              idxvol        =  dlwqd%operation(i)%index(5)
              volcorr       =  dlwqd%operation(i)%index(6)
              number_values =  dlwqd%operation(i)%number_values
              scalar_value  =  dlwqd%operation(i)%new_scalar
              vector_value  => dlwqd%operation(i)%new_value

              select case ( dlwqd%operation(i)%operation )
                  case ( DLWQ_SET )
                      if ( number == 1 ) then
                          dlwqd%rbuf(index) = scalar_value
                      else
                          if ( number_values == 1 ) then
                              do j = 1,number
                                  k = index + (j-1)*step
                                  dlwqd%rbuf(k) = scalar_value
                              enddo
                          else
                              do j = 1,number
                                  k = index + (j-1)*step
                                  dlwqd%rbuf(k) = vector_value(j)
                              enddo
                          endif
                      endif

                  case ( DLWQ_ADD )
                      if ( number == 1 ) then
                          dlwqd%rbuf(index) = dlwqd%rbuf(index) + scalar_value
                      else
                          if ( number_values == 1 ) then
                              do j = 1,number
                                  k = index + (j-1)*step
                                  dlwqd%rbuf(k) = dlwqd%rbuf(k) + scalar_value
                              enddo
                          else
                              do j = 1,number
                                  k = index + (j-1)*step
                                  dlwqd%rbuf(k) = dlwqd%rbuf(k) + vector_value(j)
                              enddo
                          endif
                      endif

                  case ( DLWQ_MULTIPLY )
                      if ( number == 1 ) then
                          dlwqd%rbuf(index) = dlwqd%rbuf(index) * scalar_value
                      else
                          if ( number_values == 1 ) then
                              do j = 1,number
                                  k = index + (j-1)*step
                                  dlwqd%rbuf(k) = dlwqd%rbuf(k) * scalar_value
                              enddo
                          else
                              do j = 1,number
                                  k = index + (j-1)*step
                                  dlwqd%rbuf(k) = dlwqd%rbuf(k) * vector_value(j)
                              enddo
                          endif
                      endif


                  case default
                      ! Do nothing
              end select

              !
              ! Adjust the mass if the concentration was adjusted
              !
              if ( idxmass > 0 ) then
                  do j = 1,number
                      kmass = idxmass + (j-1)*step
                      kconc = index   + (j-1)*step
                      kvol  = idxvol  + (j-1)/volcorr

                      dlwqd%rbuf(kmass) = dlwqd%rbuf(kconc) * dlwqd%rbuf(kvol)
                  enddo
              endif

              !
              ! We are done with this operation
              !
              dlwqd%operation(i)%operation = DLWQ_FREE

          enddo

          dlwqd%number_operations = 0

      end subroutine apply_operations

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
