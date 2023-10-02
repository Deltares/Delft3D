!!  Copyright (C)  Stichting Deltares, 2021-2023.
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

      subroutine read_atr(file_atr, atr_type, not_atr, noseg, attributes)

      ! function : read a atr file

      ! global declarations

      use m_srstop
      use m_monsys
      use m_evaluate_waq_attribute
      use filmod                   ! module contains everything for the files
      use m_ddhyd                   ! module contains everything for the hydrodynamic description
      implicit none

      ! declaration of the arguments

      type(t_dlwqfile)                       :: file_atr               ! aggregation-file
      integer                                :: atr_type               ! type of attribute information
      integer                                :: not_atr                ! total number of attributes
      integer                                :: noseg                  ! number of segments
      integer                                :: attributes(*)          ! attributes

      ! local declarations

      integer                                :: i                      ! loop counter
      integer                                :: iseg                   ! loop counter
      integer                                :: iopt                   ! option from input
      integer                                :: lunrep                 ! unit number report file
      integer                                :: int                    ! integer token from input
      real                                   :: reel                   ! real token from input
      integer                                :: ierr                   ! error indication
      integer                                :: ierr_alloc             ! error indication
      type(inputfilestack)                   :: inpfil                 ! input file strucure with include stack
      integer                                :: no_block               ! number of blocks of input
      integer                                :: i_block                ! index of attributes
      integer                                :: no_atr                 ! number of attributes
      integer                                :: i_atr                  ! index of attributes
      integer                                :: atr                    ! read attributes
      integer                                :: atr_i_atr              ! single attribute
      integer                                :: atr_prev               ! previous read single attribute
      integer , allocatable                  :: atr_num(:)             ! attribute number
      integer , allocatable                  :: atr_ioff(:)            ! attribute offset in integer representation

      call getmlu(lunrep)

      ! zero attribute array

      not_atr = 0
      do iseg = 1, noseg
         attributes(iseg) = 0
      enddo

      ! open file

      call dlwqfile_open(file_atr)

      ! check for the keyword DELWAQ_COMPLETE_ATTRIBUTES (on the first line!)

      atr_type = ATR_OLD
      read(file_atr%unit_nr,'(a)') inpfil%linbuf(1)
      do i  = 1 , 256-25
         if ( inpfil%linbuf(1)(i:i+25) .eq. 'DELWAQ_COMPLETE_ATTRIBUTES' ) then
            atr_type = ATR_COMPLETE
            exit
         endif
      enddo

      ! rewind, and initialise inputfilestack

      rewind(file_atr%unit_nr)
      inpfil%inplun(1) = file_atr%unit_nr
      inpfil%finame(1) = file_atr%name
      inpfil%cchar  = ';'
      inpfil%iposr  = 0
      inpfil%npos  = len(inpfil%linbuf(1))
      inpfil%token_used = .true.
      inpfil%inputf = 1
      inpfil%nrepeat= 0
      ierr = 0

      ! read

      if ( atr_type .EQ. ATR_COMPLETE ) then

         call dlwq_read_token( inpfil, no_block, ierr)
         if ( ierr .ne. 0 ) then
            write(lunrep,*) ' error reading attributes file: ',trim(file_atr%name)
            write(lunrep,*) ' expected integer with number of blocks'
            write(lunrep,*) inpfil%ctoken
            write(lunrep,*) inpfil%itoken
            write(lunrep,*) inpfil%ierr
            goto 200
         endif

         do i_block = 1 , no_block

            call dlwq_read_token( inpfil, no_atr, ierr)
            if ( ierr .ne. 0 ) then
               write(lunrep,*) ' error reading attributes file: ',trim(file_atr%name)
               write(lunrep,*) ' expected integer with number of attributes in this block'
               goto 200
            endif
            if ( no_atr .le. 0 .or. no_atr .gt. 8 ) then
               write(lunrep,*) ' error reading attributes file: ',trim(file_atr%name)
               write(lunrep,*) ' expected integer with number of attributes in this block'
               ierr = 1
               goto 200
            endif
            allocate(atr_num(no_atr),atr_ioff(no_atr), stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then
               ierr = 1
               write(lunrep,*) ' error allocating data arrays attributes'
               write(lunrep,*) ' number of attributes   :',no_atr
               goto 200
            endif
            do i_atr = 1 , no_atr
               call dlwq_read_token( inpfil, atr_num(i_atr), ierr)
               if ( atr_num(i_atr) .le. 0 .or. atr_num(i_atr) .gt. 8 ) then
                  write(lunrep,*) ' warning attribute number out of range'
                  write(lunrep,*) ' follow number     :',i_atr
                  write(lunrep,*) ' attribute nummber :',atr_num(i_atr)
                  write(lunrep,*) ' attribute not used'
               endif
               atr_ioff(i_atr) = 10**(atr_num(i_atr)-1)
               not_atr = max(not_atr,atr_num(i_atr))
            enddo

            ! file option

            call dlwq_read_token( inpfil, iopt, ierr)
            if ( iopt .ne. 1 ) then
               ierr = 1
               write(lunrep,*) ' error only file option 1 allowed for attributes'
               write(lunrep,*) ' file option :',iopt
            endif

            ! default option

            call dlwq_read_token( inpfil, iopt, ierr)
            if ( iopt .ne. 1 ) then
               ierr = 1
               write(lunrep,*) ' error only option data without defaults allowed for attributes'
               write(lunrep,*) ' defaults option :',iopt
            endif

            ! read and merge attributes (overwrite earlier attribute with the same number = substract)

            do iseg = 1 , noseg
               call dlwq_read_token( inpfil, atr, ierr)
               do i_atr = 1 , no_atr
                  call evaluate_waq_attribute(i_atr,atr,atr_i_atr)
                  call evaluate_waq_attribute(atr_num(i_atr),attributes(iseg),atr_prev)
                  attributes(iseg) = attributes(iseg) + atr_i_atr*atr_ioff(i_atr) - atr_prev*atr_ioff(i_atr)
               enddo
            enddo

            deallocate(atr_num,atr_ioff)

         enddo

      else

      endif

  200 continue
      if ( ierr .ne. 0 ) then
         call srstop(1)
      endif

      close(file_atr%unit_nr)
      file_atr%status = FILE_STAT_UNOPENED

      return
      end
