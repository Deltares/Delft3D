!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
module m_rdwnd
use m_stop_exit


implicit none

contains


      subroutine rdwnd ( lunut  , sfile  , nmax   , mmax  , noseg  ,      &
     &                   xb     , yb     , lgrida , ierr  )

!     Deltares Software Centre

!>/File
!>        Initializes the Delft3D meteo system for wind interpolation
!>
!>        This first version is now hardwired for separate x and y values

!     Created         : 23 November 2011 by Leo Postma
!     Modified        :

!     File I/O        : lunut - report file
!                       86    - unit number steering file

!     Routines called : ReadProvider         - to link a variable to a file
!                       AddTimeSpaceRelation - to link a geometry to interpolate on

      use m_waq_precision       ! data definitions
      use timers          ! to time this routine
      use alloc_mod       ! to allocate arrays
      use m_waq_timespace       ! meteo module ?

      implicit none

!     Arguments

!     kind           function         name                description

      integer      , intent(in   ) :: lunut             !< unit number report file
      character( *), intent(in   ) :: sfile             !< steering file
      integer      , intent(in   ) :: nmax              !< first index
      integer      , intent(in   ) :: mmax              !< second index
      integer      , intent(in   ) :: noseg             !< dimension of linear arrays
      real     (real_wp), intent(in   ) :: xb    (nmax,mmax) !< x-cco values
      real     (real_wp), intent(in   ) :: yb    (nmax,mmax) !< y-cco values
      integer      , intent(in   ) :: lgrida(nmax,mmax) !< active grid table
      integer      , intent(  out) :: ierr              !< if non-zero then error

!     Locals

      character( 5 )                 qid         ! item ID for meteo items
      character(128)                 finamx      ! filename of the u,x data for the wind
      character(128)                 finamy      ! filename of the v,y data for the wind
      character( 1 )                 opera       ! becomes O for override
      integer                        kx          ! vector space
      integer                        ftype       ! becomes 4 for arcinfo type of file
      integer                        method      ! becomes 1 for interpolate in space and time
      integer                        yes         ! for those routines ending with an integer
      logical                        yes2        ! for those routines ending with a logical
      real     ( 8 )                 tcoef(2)    ! required for the calls to the meteo system
      integer  ( 4 )                 ik, im, in  ! loop counters
      real     ( 8 ), allocatable :: xz (:)      ! x of cell centres
      real     ( 8 ), allocatable :: yz (:)      ! y of the cell centres
      integer  ( 4 ), allocatable :: kcs(:)      ! cell property
      integer                        lunfil

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "rdwnd", ithndl )

!     make the coordinates in cell centres and the property array kcs (0=inactive)

      allocate ( xz(noseg), yz(noseg), kcs(noseg) )
      kcs = 1
      ik  = 1
      do im = 1, mmax
         do in = 1, nmax
            if ( lgrida(in,im) .gt. 0 ) then        ! compute coords for cell centres
               xz(ik) = ( xb(in-1,im-1) + xb(in-1,im) + xb(in,im-1) + xb(in,im) ) / 4.0d0
               yz(ik) = ( yb(in-1,im-1) + yb(in-1,im) + yb(in,im-1) + yb(in,im) ) / 4.0d0
               ik = ik + 1
            else
               if ( noseg .eq. nmax*mmax ) then     ! full matrix
                  kcs(ik) = 0
                  xz (ik) = 0.0d0
                  yz (ik) = 0.0d0
                  ik = ik + 1
               endif
            endif
         enddo
      enddo

!     open the description file

      open ( newunit=lunfil, file=sfile )

!     identify source of x-data

      ierr = 0
      kx   = 1
      call readprovider           ( lunfil     , qid    , finamx , ftype  , method ,  &
     &                              opera  , tcoef  , yes    )
      if ( qid  .ne. "windx" ) ierr = 1
      if ( yes  .ne. 1       ) ierr = 2
      if ( ierr .ne. 0       ) goto 9000

!     initialize the file read and interpolation system for x-data

      yes2 = AddTimeSpaceRelation ( 1      , qid    , kx     , xz     , yz     ,  &
     &                              kcs    , finamx , ftype  , method , opera  )
      if ( .not. yes2  ) ierr = 3
      if ( ierr .ne. 0 ) goto 9000

!     identify source of y-data

      call readprovider           ( lunfil     , qid    , finamy , ftype  , method ,  &
     &                              opera  , tcoef  , yes    )
      if ( qid  .ne. "windy" ) ierr = 4
      if ( yes  .ne. 1       ) ierr = 5
      if ( ierr .ne. 0       ) goto 9000

!     initialize the file read and interpolation system for y-data

      yes2 = AddTimeSpaceRelation ( 1      , qid    , kx     , xz     , yz     ,  &
     &                              kcs    , finamy , ftype  , method , opera  )
      if ( .not. yes2  ) ierr = 6
      if ( ierr .ne. 0 ) goto 9000

!     Error handling

 9000 select case ( ierr )
         case ( 0 )
            write ( lunut, 1000 )
            write ( lunut, 1001 ) trim(errormessage)
         case ( 1 )
            write ( lunut, 1010 )
            write ( lunut, 1001 ) trim(errormessage)
            call stop_exit(1)
         case ( 2 )
            write ( lunut, 1020 )
            write ( lunut, 1001 ) trim(errormessage)
            call stop_exit(1)
         case ( 3 )
            write ( lunut, 1030 )
            write ( lunut, 1001 ) trim(errormessage)
            call stop_exit(1)
         case ( 4 )
            write ( lunut, 1040 )
            write ( lunut, 1001 ) trim(errormessage)
            call stop_exit(1)
         case ( 5 )
            write ( lunut, 1040 )
            write ( lunut, 1001 ) trim(errormessage)
            call stop_exit(1)
         case ( 6 )
            write ( lunut, 1040 )
            write ( lunut, 1001 ) trim(errormessage)
            call stop_exit(1)
      end select

      if ( timon ) call timstop( ithndl )

!     Formats

 1000 format( ' Setting up of the meteo system for winds was successful !' )
 1001 format( '       ',a)
 1010 format( ' ERROR: first variable windx not found on meteo-file !' )
 1020 format( ' ERROR: defining x-wind variable from meteo-file !' )
 1030 format( ' ERROR: setting up interpolation for x-wind meteo !' )
 1040 format( ' ERROR: second variable windy not found on meteo-file !' )
 1050 format( ' ERROR: defining y-wind variable from meteo-file !' )
 1060 format( ' ERROR: setting up interpolation for y-wind meteo !' )

      end

end module m_rdwnd
