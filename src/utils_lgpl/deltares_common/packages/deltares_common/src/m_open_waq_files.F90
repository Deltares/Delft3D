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
      module m_open_waq_files
      use m_srstop


      implicit none

      contains


      subroutine open_waq_files ( lun    , finam  , nropen , opmode , ierr   )

!     Deltares Software Centre

!>\File
!>           Opens all Delwaq files
!
!*********************************************************************
!     SHARED is used for those files which are optionally prepared
!     at runtime by another programme, running on-line.
!     To enable this, the reading is synchronised (DLWQI0, DLWQT4)
!     Files opened with this option are:
!     volumes, flows, areas, lengths, segment functions
!     and harmonics (see local comments)
!*********************************************************************

!     SUBROUTINE CALLED  : SRSTOP, stopexecution

!     LOGICAL UNITS      : LUN = unit to be opened
      use cwd, only: getCWD
      implicit none

!     Parameters          :

!     kind           function         name        description

      integer      , intent(inout) :: lun       !< unit number of file to be opened
      character*(*), intent(in   ) :: finam     !< name of the file to be opened
      integer      , intent(in   ) :: nropen    !< Delwaq number of the file to be opened
      integer      , intent(in   ) :: opmode    !< Indicator how file must be opened
      integer      , intent(inout) :: ierr      !< Error flag
!     Local variables     :

      integer                         ierr2     !< Error flag
      integer                         ierr_cwd  !< Error flag for obtaining current working directory
      character(256)                  wd_path   !< Current working directory path
      
      ierr   =   0


      
!     get the correct open statement

      select case ( nropen )
         case ( 1 )                           !   common-block file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace' )
               case ( 2 )
                  open ( newunit=lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old' )
               case ( 3 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream' )
                  close( lun, status = 'delete', err = 910 )
               case default
                  ierr = 3
            end select

         case ( 2 )                           !   system file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old' )
               case default
                  ierr = 3
            end select

         case ( 3 )                           !     harmonic functions, 'SHARED' because this NROPEN
            select case ( opmode )            !     is used from DLWQI0 also for other files,
               case ( 1 )                     !     which are intended to have the SHARED property
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream'  , status = 'old' )
               case ( 11 )
                  open ( lun, file = finam, err = 910, form='unformatted'  )
               case ( 12 )
                  open ( lun, file = finam, err = 910, form='unformatted' , status = 'old' )
               case ( 21 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream' , &
                                                       convert='big_endian' )
               case ( 22 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream' , status = 'old', &
                                                       convert='big_endian' )
               case ( 31 )
                  open ( lun, file = finam, err = 910, form   ='unformatted' , &
                                                       convert='big_endian' )
               case ( 32 )
                  open ( lun, file = finam, err = 910, form='unformatted' , status = 'old', &
                                                       convert='big_endian' )
               case default
                  ierr = 3
            end select

         case ( 4 )                           !     pointers functions
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old'  )
               case default
               ierr = 3
            end select

         case ( 5 )                           !     time steps file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old'  )
               case default
                  ierr = 3
            end select

         case ( 6 )                           !     grid layout
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old'  )
               case default
                  ierr = 3
            end select

         case ( 7 )                           !     volumes file
            select case ( opmode )
               case (  1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream'  )
               case (  2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old'  )
               case ( 11, 12 )
                  open ( lun, file = finam, err = 910, form='unformatted' )
               case ( 21, 22 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream' , &
                                                       convert='big_endian' )
               case ( 31, 32 )
                  open ( lun, file = finam, err = 910, form='unformatted', convert='big_endian' )
               case default
                  ierr = 3
            end select

         case ( 8 )                           !     to-/from-pointers file
            select case ( opmode )
               case (  1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace'  )
               case (  2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old' )
               case ( 11 )
                  open ( lun, file = finam, err = 910, form='unformatted' )
               case ( 12 )
                  open ( lun, file = finam, err = 910, form='unformatted', status = 'old' )
               case ( 21 )
                  open ( lun, file = finam, err = 910, form   ='unformatted', access='stream', &
                                                       convert='big_endian' )
               case ( 22 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old', &
                                                       convert='big_endian' )
               case ( 31 )
                  open ( lun, file = finam, err = 910, form   ='unformatted', &
                                                       convert='big_endian' )
               case ( 32 )
                  open ( lun, file = finam, err = 910, form='unformatted', status = 'old', &
                                                       convert='big_endian' )
               case default
                  ierr = 3
            end select

         case ( 9 )                           !     dispersion file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old'  )
               case default
                  ierr = 3
            end select

         case ( 10 )                          !     areas file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old'   )
               case ( 11, 12 )
                  open ( lun, file = finam, err = 910, form='unformatted' )
               case ( 21, 22 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream' , &
                                                       convert='big_endian' )
               case ( 31, 32 )
                  open ( lun, file = finam, err = 910, form='unformatted', convert='big_endian' )
               case default
                  ierr = 3
            end select

         case ( 11 )                          !     flows
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old'   )
               case ( 11, 12 )
                  open ( lun, file = finam, err = 910, form='unformatted' )
               case ( 21, 22 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream' , &
                                                       convert='big_endian' )
               case ( 31, 32 )
                  open ( lun, file = finam, err = 910, form='unformatted', convert='big_endian' )
               case default
                  ierr = 3
            end select

         case ( 12 )                          !     velocities file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old'  )
               case default
                  ierr = 3
            end select

         case ( 13 )                          !     length file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old'   )
               case ( 11, 12 )
                  open ( lun, file = finam, err = 910, form='unformatted' )
               case ( 21, 22 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream' , &
                                                       convert='big_endian' )
               case ( 31, 32 )
                  open ( lun, file = finam, err = 910, form='unformatted', convert='big_endian' )
               case default
                  ierr = 3
            end select

         case ( 14 )                          !     boundaries file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old'  )
               case default
                  ierr = 3
            end select

         case ( 15 )                          !     waste loads file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old'  )
               case default
                  ierr = 3
            end select

         case ( 16 )                          !     functions file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old'  )
               case default
                  ierr = 3
            end select

         case ( 17 )                          !     segment functions file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old'   )
               case ( 11, 12 )
                  open ( lun, file = finam, err = 910, form='unformatted' )
               case ( 21, 22 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream' , &
                                                       convert='big_endian' )
               case ( 31, 32 )
                  open ( lun, file = finam, err = 910, form='unformatted', convert='big_endian' )
               case default
                  ierr = 3
            end select

         case ( 18 )                          !     initial conditions file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old'  )
               case default
                  ierr = 3
            end select

         case ( 19 )                          !     DELWAQ2 monitoring file (.mon)
            select case ( opmode )
               case ( 1 )
                  open ( newunit=lun, file = finam, err = 910 )
               case default
                  ierr = 3
            end select

         case ( 20 )                          !     dump file
            select case ( opmode )
               case ( 1 )
                  open ( newunit=lun, file = finam, err = 910 )
               case default
                  ierr = 3
            end select

         case ( 21 )                          !     history file
            select case ( opmode )
               case ( 1 )
                  open ( newunit=lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace'  )
               case default
                  ierr = 3
            end select

         case ( 22 )                          !     map file
            select case ( opmode )
               case ( 1 )
                  open ( newunit=lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace'  )
               case default
                  ierr = 3
            end select

         case ( 23 )                          !     restart file
            select case ( opmode )
               case ( 1 )
                  open ( newunit=lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace'  )
               case default
                  ierr = 3
            end select

         case ( 24 )                          !     process work file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old'  )
               case default
                  ierr = 3
            end select

         case ( 25 )                          !     output work file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old'  )
               case default
                  ierr = 3
            end select

         case ( 26 )                          !     input file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 900, status = 'old' )
               case default
                  ierr = 3
            end select

         case ( 27 )                          !     stripped input file
            select case ( opmode )
               case ( 1 )
                  open ( lun, status = 'scratch' )
               case default
                  ierr = 3
            end select

         case ( 28 )                          !     aux. stripped input file
            select case ( opmode )
               case ( 1 )
                  open ( lun, status = 'scratch' )
                  rewind ( lun )
               case default
                  ierr = 3
            end select

         case ( 29 )                          !     DELWAQ1 input report file (.lst)
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910 )
               case ( 2 )
                  open ( lun, file = finam, err = 900, status = 'new' )
               case default
                  ierr = 3
            end select

         case ( 30 )                          !     dimensioning include file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910 )
               case ( 2 )
                  open ( lun, file = finam, err = 900, status = 'old' )
               case default
                  ierr = 3
            end select

         case ( 31 )                          !     scratch file time functions 1
            select case ( opmode )
               case ( 1 )
                  open ( lun, err = 910, form='unformatted', access='stream', status = 'scratch')
               case default
                  ierr = 3
            end select

         case ( 32 )                          !     scratch file time functions 2
            select case ( opmode )
               case ( 1 )
                  open ( lun, err = 910, form='unformatted', access='stream', status = 'scratch')
               case default
                  ierr = 3
            end select

         case ( 33 )                          !     auxiliary input file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 900, status = 'old' )
               case ( 2 )
                  open ( lun, file = finam, err = 900, form='unformatted', access='stream', status = 'old' )
               case default
                  ierr = 3
            end select

         case ( 34 )                          !     proces definition file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, status = 'old' )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old' )
               case ( 3 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace' )
               case default
                  ierr = 3
            end select

         case ( 35 )                          !     DELWAQ1 proceses report file (.lsp)
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910 )
               case default
                  ierr = 3
            end select

         case ( 36 )                          !     Proces stochi file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910 )
               case default
                  ierr = 3
            end select

         case ( 37 )                          !     bal file
            select case ( opmode )
               case ( 1 )
                  open ( newunit=lun, file = finam, err = 910, form='unformatted', access='stream' )
               case default
                  ierr = 3
            end select

         case ( 40 )                          !     Binary segment attribute file
            !
            ! Because of the reserved range of LU-numbers (see dlwqt4.f) it is not all that easy to
            ! use NEWUNIT= here - leave the old method for the moment
            !
            select case ( opmode )
               case (  1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'replace'  )
               case (  2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old' )
               case ( 11 )
                  open ( lun, file = finam, err = 910, form='unformatted' )
               case ( 12 )
                  open ( lun, file = finam, err = 910, form='unformatted', status = 'old' )
               case ( 21 )
                  open ( lun, file = finam, err = 910, form   ='unformatted', access='stream', &
                                                               convert='big_endian' )
               case ( 22 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old', &
                                                               convert='big_endian' )
               case ( 31 )
                  open ( lun, file = finam, err = 910, form   ='unformatted', &
                                                               convert='big_endian' )
               case ( 32 )
                  open ( lun, file = finam, err = 910, form='unformatted', status = 'old', &
                                                               convert='big_endian' )
               case default
                  ierr = 3
            end select

         case ( 41 )                          !     ASCII file with filenames of binary files
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910 )
               case ( 2 )
                  open ( lun, file = finam, err = 910, status = 'old' )
               case default
                  ierr = 3
            end select

         case ( 42 )                          !     domain names configuration file, online dd
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 900, status = 'old' )
               case default
                  ierr = 3
            end select

         case ( 44 )                          !     pointer file
            select case ( opmode )
               case ( 1 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream'  )
               case ( 2 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream', status = 'old'   )
               case ( 11, 12 )
                  open ( lun, file = finam, err = 910, form='unformatted' )
               case ( 21, 22 )
                  open ( lun, file = finam, err = 910, form='unformatted', access='stream' , &
                                                       convert='big_endian' )
               case ( 31, 32 )
                  open ( lun, file = finam, err = 910, form='unformatted', convert='big_endian' )
               case default
                  ierr = 3
            end select

         case default                         !     no valid number present
            ierr = 2

      end select
      return

!     error while opening with return value

  900 ierr = 1
      return

!     error while opening STOP with message

  910 continue
!     get current working directory
      wd_path = ''
      ierr_cwd = getCWD(wd_path)
      if (ierr_cwd /= 0) then
         wd_path = 'Current working directory not found!'
      endif
      
      write ( * , 2000 ) nropen, lun, trim(finam), trim(wd_path)
      call srstop ( 1 )

 2000 format (   ' ERROR opening file number:',I3,' on unit:',I3  , &
               /,' Filename is: ',A , &
               /,' Searching in working directory: ', /,'  ' A )
      end
      end module m_open_waq_files
