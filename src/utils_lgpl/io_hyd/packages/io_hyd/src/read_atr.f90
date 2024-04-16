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

      subroutine read_atr(file_atr, atr_type, not_atr, noseg, attributes)

      ! function : read a atr file

      ! global declarations

      use m_srstop
      use m_monsys
      use m_evaluate_waq_attribute
      use m_waq_file                   ! module contains everything for the files
      use m_hydmod                   ! module contains everything for the hydrodynamic description
      use rd_token       ! tokenized reading

      implicit none

      ! declaration of the arguments

      type(t_file)                       :: file_atr               ! aggregation-file
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
      integer                                :: no_block               ! number of blocks of input
      integer                                :: i_block                ! index of attributes
      integer                                :: no_atr                 ! number of attributes
      integer                                :: i_atr                  ! index of attributes
      integer                                :: atr                    ! read attributes
      integer                                :: atr_i_atr              ! single attribute
      integer                                :: atr_prev               ! previous read single attribute
      integer , allocatable                  :: atr_num(:)             ! attribute number
      integer , allocatable                  :: atr_ioff(:)            ! attribute offset in integer representation

      character(256)                         :: line

      call getmlu(lunrep)

      ! zero attribute array

      not_atr = 0
      do iseg = 1, noseg
         attributes(iseg) = 0
      enddo

      ! open file

      call file_atr%open()

      ! check for the keyword DELWAQ_COMPLETE_ATTRIBUTES (on the first line!)

      atr_type = ATR_OLD
      read(file_atr%unit,'(a)') line
      do i  = 1 , 256-25
         if ( line(i:i+25) .eq. 'DELWAQ_COMPLETE_ATTRIBUTES' ) then
            atr_type = ATR_COMPLETE
            exit
         endif
      enddo

      ! rewind, and initialise tokenised reading

      rewind(file_atr%unit)
      ilun    = 0
      ilun(1) = file_atr%unit
      lch (1) = file_atr%name
      npos   = 1000
      cchar  = ';'
      ierr = 0

      ! read

      if ( atr_type .EQ. ATR_COMPLETE ) then

         if ( gettoken ( no_block, ierr) .ne. 0 ) then
            write(lunrep,*) ' error reading attributes file:',trim(file_atr%name)
            write(lunrep,*) ' expected integer with number of blocks'
            goto 200
         endif

         do i_block = 1 , no_block
            if ( gettoken (no_atr, ierr) .ne. 0 ) then
               write(lunrep,*) ' error reading attributes file:',trim(file_atr%name)
               write(lunrep,*) ' expected integer with number of attributes in this block'
               goto 200
            endif
            if ( no_atr .le. 0 .or. no_atr .gt. 8 ) then
               write(lunrep,*) ' error reading attributes file:',trim(file_atr%name)
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
               if (gettoken ( atr_num(i_atr), ierr) .ne. 0) then
                  write(lunrep,*) ' error reading attributes file:',trim(file_atr%name)
                  write(lunrep,*) ' expected integer with attribute number in this block'
                  goto 200
               endif
               if ( atr_num(i_atr) .le. 0 .or. atr_num(i_atr) .gt. 8 ) then
                  ierr = 1
                  write(lunrep,*) ' error attribute number out of range'
                  write(lunrep,*) ' follow number     :',i_atr
                  write(lunrep,*) ' attribute nummber :',atr_num(i_atr)
                  goto 200
               endif
               atr_ioff(i_atr) = 10**(atr_num(i_atr)-1)
               not_atr = max(not_atr,atr_num(i_atr))
            enddo

            ! file option

            if ( gettoken( iopt, ierr) .ne. 0) then
               write(lunrep,*) ' error reading attributes file:',trim(file_atr%name)
               write(lunrep,*) ' expected integer with file option'
               goto 200
            endif
            if ( iopt .ne. 1 ) then
               ierr = 1
               write(lunrep,*) ' error only file option 1 allowed for attributes'
               write(lunrep,*) ' file option :',iopt
               goto 200
            endif

            ! default option

            if ( gettoken( iopt, ierr) .ne. 0) then
               write(lunrep,*) ' error reading attributes file:',trim(file_atr%name)
               write(lunrep,*) ' expected integer with default option'
               goto 200
            endif
            if ( iopt .ne. 1 ) then
               ierr = 1
               write(lunrep,*) ' error only option data without defaults allowed for attributes'
               write(lunrep,*) ' defaults option :',iopt
               goto 200
            endif

            ! read and merge attributes (overwrite earlier attribute with the same number = substract)

            do iseg = 1 , noseg
               if (gettoken ( atr, ierr) .ne. 0 ) then
                  write(lunrep,*) ' error reading attributes file:',trim(file_atr%name)
                  write(lunrep,*) ' expected integer with attribute in this block'
                  goto 200
               endif
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

      close(file_atr%unit)
      file_atr%status = FILE_STAT_UNOPENED

      return
      end
