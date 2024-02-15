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

      module rd_token

      implicit none

!         publicly accessable ( this allows for mixed use with direct calls to rdtok1/2 )

      integer  , parameter :: lstack = 6      !< size include files stack
      integer  , parameter :: lchmax = 255    !< string length file name variables
      integer              :: ilun(lstack)    !< unit numbers include files stack
      character(lchmax)    :: lch (lstack)    !< file names include files stack
      character(  1   )    :: cchar           !< comment character
      integer              :: lunut           !< unit number output file
      integer              :: iposr           !< location on the record
      integer              :: npos            !< record length
      logical              :: push            !< use previous token if true

!         private

      integer          , private   :: type    ! type to be expected from rdtok1/2
      character(lchmax), private   :: cdummy  ! character dummy argument
      integer*8        , private   :: idummy  ! integer dummy argument
      real*8           , private   :: rdummy  ! real dummy argument

      interface gettoken
         module procedure get_char_tok
         module procedure get_char_untileol_tok
         module procedure get_int_tok
         module procedure get_int8_tok
         module procedure get_real_tok
         module procedure get_double_tok
         module procedure get_nochar_tok
         module procedure get_noreal_tok
         module procedure get_all_tok
      end interface

      contains

!          force the opening of a new include file

      function force_include_file ( achar ) result ( ierr )

         character*(*), intent(in   ) :: achar
         integer   (4)                   ierr

         type = -999
         call rdtok2 ( lunut  , ilun   , lch    , lstack , cchar  ,              &
     &                 iposr  , npos   , achar  , idummy , rdummy ,              &
     &                 type   , ierr   )
      end function force_include_file


!          get a character string

      function get_char_tok ( achar, ierr2 ) result ( ierr )

         character*(*), intent(  out) :: achar
         integer   (4), intent(inout) :: ierr2
         integer   (4)                   ierr

         if ( push ) then
            achar = cdummy
            ierr2 = 0                   ! this is always possible
            push  = .false.
         else
            type = 1
            call rdtok2 ( lunut  , ilun   , lch    , lstack , cchar  ,              &
     &                    iposr  , npos   , cdummy , idummy , rdummy ,              &
     &                    type   , ierr2  )
            achar = cdummy
         endif
         ierr  = ierr2

      end function get_char_tok

!          get a character string until the end of the line

      function get_char_untileol_tok ( achar, untileol, ierr2 ) result ( ierr )

         character*(*), intent(  out) :: achar
         logical      , intent(in)    :: untileol
         integer   (4), intent(inout) :: ierr2
         integer   (4)                   ierr

         if ( push ) then
            achar = cdummy
            ierr2 = 0                   ! this is always possible
            push  = .false.
         else
            type = 4
            call rdtok2 ( lunut  , ilun   , lch    , lstack , cchar  ,              &
     &                    iposr  , npos   , cdummy , idummy , rdummy ,              &
     &                    type   , ierr2  )
            achar = cdummy
         endif
         ierr  = ierr2

      end function get_char_untileol_tok

!          get an integer

      function get_int_tok ( anint, ierr2 ) result ( ierr )

         integer   (4), intent(  out) :: anint
         integer   (4), intent(inout) :: ierr2
         integer   (4)                   ierr

         if ( push ) then
            if ( type .ne. 2 ) then
               ierr2 = 1                ! there is no integer on the stack
            else
               anint = idummy
               ierr2 = 0
            endif
            push  = .false.
         else
            type = 2
            call rdtok2 ( lunut  , ilun   , lch    , lstack , cchar  ,              &
     &                    iposr  , npos   , cdummy , idummy , rdummy ,              &
     &                    type   , ierr2  )
            anint  = idummy
            rdummy = idummy
         endif
         ierr = ierr2

      end function get_int_tok

!          get an integer

      function get_int8_tok ( anint8, ierr2 ) result ( ierr )

         integer   (8), intent(  out) :: anint8
         integer   (4), intent(inout) :: ierr2
         integer   (4)                   ierr

         if ( push ) then
            if ( type .ne. 2 ) then
               ierr2 = 1                ! there is no integer on the stack
            else
               anint8 = idummy
               ierr2 = 0
            endif
            push  = .false.
         else
            type = 2
            call rdtok2 ( lunut  , ilun   , lch    , lstack , cchar  ,              &
     &                    iposr  , npos   , cdummy , idummy , rdummy ,              &
     &                    type   , ierr2  )
            anint8 = idummy
            rdummy = idummy
         endif
         ierr = ierr2

         end function get_int8_tok

!             get a real

      function get_real_tok ( areal, ierr2 ) result ( ierr )

         real      (4), intent(  out) :: areal
         integer   (4), intent(inout) :: ierr2
         integer                         ierr

         if ( push ) then
            if ( type .ne. 2 .and. type .ne. 3 ) then
               ierr2 = 1                ! there is no number on the stack
            else
               areal = real(rdummy)
               ierr2 = 0
            endif
            push  = .false.
         else
            type = 3
            call rdtok2 ( lunut  , ilun   , lch    , lstack , cchar  ,              &
     &                    iposr  , npos   , cdummy , idummy , rdummy ,              &
     &                    type   , ierr2  )
            areal = real(rdummy)
         endif
         ierr  = ierr2

      end function get_real_tok

      function get_double_tok ( adouble, ierr2 ) result ( ierr )

         real      (8), intent(  out) :: adouble
         integer   (4), intent(inout) :: ierr2
         integer                         ierr

         if ( push ) then
            if ( type .ne. 2 .and. type .ne. 3 ) then
               ierr2 = 1                ! there is no number on the stack
            else
               adouble = rdummy
               ierr2 = 0
            endif
            push  = .false.
         else
            type = 3
            call rdtok2 ( lunut  , ilun   , lch    , lstack , cchar  ,              &
     &                    iposr  , npos   , cdummy , idummy , rdummy ,              &
     &                    type   , ierr2  )
            adouble = rdummy
         endif
         ierr  = ierr2

         end function get_double_tok

!          get a number

      function get_nochar_tok  ( anint, areal, itype, ierr2 ) result ( ierr )

         integer   (4), intent(  out) :: anint
         real      (4), intent(  out) :: areal
         integer   (4), intent(  out) :: itype
         integer   (4), intent(inout) :: ierr2
         integer                         ierr

         if ( push ) then
            if ( type .ne. 2 .and. type .ne. 3 ) then
               ierr2 = 1                ! there is no number on the stack
            else
               anint = idummy
               areal = rdummy
               itype = type
               ierr2 = 0
            endif
            push  = .false.
         else
            type = -1
            call rdtok2 ( lunut  , ilun   , lch    , lstack , cchar  ,              &
     &                    iposr  , npos   , cdummy , idummy , rdummy ,              &
     &                    type   , ierr2  )
            anint = idummy
            areal = rdummy
            itype = type
         endif
         ierr  = ierr2

      end function get_nochar_tok

!          get anything but a real

      function get_noreal_tok  ( achar, anint, itype, ierr2 ) result ( ierr )

         character*(*), intent(  out) :: achar
         integer   (4), intent(  out) :: anint
         integer   (4), intent(  out) :: itype
         integer   (4), intent(inout) :: ierr2
         integer                         ierr

         if ( push ) then
            if ( type .eq. 3 ) then
               ierr2 = 1           ! there is a real on the stack
            else
               achar = cdummy
               anint = idummy
               itype = type
               ierr2 = 0
            endif
            push  = .false.
         else
            type = -3
            call rdtok2 ( lunut  , ilun   , lch    , lstack , cchar  ,              &
     &                    iposr  , npos   , cdummy , idummy , rdummy ,              &
     &                    type   , ierr2  )
            achar = cdummy
            anint = idummy
            itype = type
         endif
         ierr = ierr2

      end function get_noreal_tok

!          get anything

      function get_all_tok  ( achar, anint, areal, itype, ierr2 ) result ( ierr )

         character*(*), intent(  out) :: achar
         integer   (4), intent(  out) :: anint
         real      (4), intent(  out) :: areal
         integer   (4), intent(  out) :: itype
         integer   (4), intent(inout) :: ierr2
         integer                         ierr

         if ( push ) then
            achar = cdummy
            anint = idummy
            areal = rdummy
            itype = type
            ierr2 = 0
            push  = .false.
         else
            type = 0
            call rdtok2 ( lunut  , ilun   , lch    , lstack , cchar  ,              &
     &                    iposr  , npos   , cdummy , idummy , rdummy ,              &
     &                    type   , ierr2  )
            achar = cdummy
            anint = idummy
            areal = rdummy
            itype = type
         endif
         ierr = ierr2

      end function get_all_tok

      function puttoken ( achar ) result ( ierr )

         character*(*), intent(in   ) :: achar
         integer                         ierr

         ierr   = 0
         cdummy = achar
         type   = 1
         push   = .true.

      end function puttoken

      end module rd_token
