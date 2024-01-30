!!  Copyright (C)  Stichting Deltares, 2021-2024.
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

      subroutine read_ddb(hyd)

      ! function : read the ddb file from the overall hydrodynamics

      ! global declarations

      use m_monsys
      use hydmod
      use m_write_error_message
      use m_file_path_utils, only : extract_file_extension
      use rd_token       ! tokenized reading

      implicit none

      ! declaration of the arguments

      type(t_hyd)         :: hyd                    ! description of the hydrodynamics

      ! local declarations

      integer             :: lunrep         !< report file
      integer             :: i_domain       !< index in collection
      type(t_domain)      :: domain         !< one domain description
      integer             :: i_dd_bound     !< index in collection
      type(t_dd_bound)    :: dd_bound       !< one dd_bound description
      character(len=255)  :: line           !< line buffer input file
      character(len=255)  :: ctoken         !< line buffer input file
      character(len=255)  :: filext         !< file extension
      integer             :: extpos         !< position file extension
      integer             :: extlen         !< length file extension
      integer             :: i_swap         !< swap help variable
      integer             :: int            !< integer token from input
      real                :: reel           !< real token from input
      integer             :: itype          !< token type found
      character(len=20)   :: string         !< String token
      integer             :: ierr           !< error indicator
      logical             :: token_used     !< token_used
      type(t_dlwqfile)    :: file_src       !< hydrodynamics-file

      file_src = hyd%file_com

      call dlwqfile_open(file_src)

      ilun    = 0
      ilun(1) = file_src%unit_nr
      lch (1) = file_src%name
      npos   = 1000
      cchar  = ';'
      ierr = 0

      if (gettoken( string, int, reel, itype, ierr) .ne. 0) then
         write(lunrep,*) ' error opening ddbound file'
         write(lunrep,*) ' file: ',trim(hyd%file_com%name)
      endif

      hyd%domain_coll%cursize = 0
      hyd%domain_coll%maxsize = 0
      hyd%dd_bound_coll%cursize = 0
      hyd%dd_bound_coll%maxsize = 0

      ! loop over all the tokens in the file

      do

         ! read first domain

         if (gettoken( dd_bound%name1, ierr) .ne. 0) then
            ! if end of file the exit loop
            exit
        endif

         ! read m_begin1, n_begin1, m_end1, n_end1, domain name 2, m_begin2, n_begin2, m_end2, n_end2


         if (gettoken( dd_bound%m_begin1, ierr) .ne. 0 ) goto 900
         if (gettoken( dd_bound%n_begin1, ierr) .ne. 0 ) goto 900
         if (gettoken( dd_bound%m_end1, ierr)   .ne. 0 ) goto 900
         if (gettoken( dd_bound%n_end1, ierr)   .ne. 0 ) goto 900

         if (gettoken( dd_bound%name2, ierr)    .ne. 0 ) goto 900
         if (gettoken( dd_bound%m_begin2, ierr) .ne. 0 ) goto 900
         if (gettoken( dd_bound%n_begin2, ierr) .ne. 0 ) goto 900
         if (gettoken( dd_bound%m_end2, ierr)   .ne. 0 ) goto 900
         if (gettoken( dd_bound%n_end2, ierr)   .ne. 0 ) goto 900

         ! fuzzy: get rid of extension of domain name

         call extract_file_extension(dd_bound%name1,filext, extpos, extlen)
         if (extpos.gt.1) dd_bound%name1(extpos:) = ' '
         call extract_file_extension(dd_bound%name2,filext, extpos, extlen)
         if (extpos.gt.1) dd_bound%name2(extpos:) = ' '

         ! make sure the numbering is always increasing (postpone till overall hyd file is written?)

         if ( dd_bound%m_begin1 .gt. dd_bound%m_end1 ) then
            i_swap            = dd_bound%m_begin1
            dd_bound%m_begin1 = dd_bound%m_end1
            dd_bound%m_end1   = i_swap
         endif
         if ( dd_bound%n_begin1 .gt. dd_bound%n_end1 ) then
            i_swap            = dd_bound%n_begin1
            dd_bound%n_begin1 = dd_bound%n_end1
            dd_bound%n_end1   = i_swap
         endif
         if ( dd_bound%m_begin2 .gt. dd_bound%m_end2 ) then
            i_swap            = dd_bound%m_begin2
            dd_bound%m_begin2 = dd_bound%m_end2
            dd_bound%m_end2   = i_swap
         endif
         if ( dd_bound%n_begin2 .gt. dd_bound%n_end2 ) then
            i_swap            = dd_bound%n_begin2
            dd_bound%n_begin2 = dd_bound%n_end2
            dd_bound%n_end2   = i_swap
         endif

         ! add to dd_bound collection

         i_dd_bound = dd_bound_coll_add(hyd%dd_bound_coll, dd_bound)

      enddo

      return
 900  call write_error_message('error reading dbb file')
      end subroutine read_ddb
