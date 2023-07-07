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

subroutine delwaq2_main_init(dlwqd, itota, itoti, itotc, init, action, argc, argv )


  use delwaq2
  use delwaq2_data
  use dhcommand
  use m_actions
  use m_sysn
  use m_sysi


  implicit none
  
  ! Arguments
  integer       imaxa , imaxi , imaxc

  logical, intent(inout)                        :: init
  integer, intent(in)                           :: action
  integer, intent(in)                           :: argc
  character(len=*), dimension(argc), intent(in) :: argv
  type(delwaq_data), target                     :: dlwqd
  type(GridPointerColl), pointer                :: GridPs               ! collection of all grid definitions

  integer, intent(inout)                        :: itota
  integer, intent(inout)                        :: itoti
  integer, intent(inout)                        :: itotc

  

  init = .false.
  if ( action == action_initialisation  .or. action == action_fullcomputation ) then
      init = .true.

      call dhstore_command( argv )

      itota=0
      itoti=0
      itotc=0

      nullify( dlwqd%rbuf  )
      nullify( dlwqd%ibuf  )
      nullify( dlwqd%chbuf )

      allocate( dlwqd%rbuf(0) )
      allocate( dlwqd%ibuf(0) )
      allocate( dlwqd%chbuf(0) )

  endif
     
end subroutine delwaq2_main_init
