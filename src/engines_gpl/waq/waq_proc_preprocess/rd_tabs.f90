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
      module m_rd_tabs
      use m_waq_precision
      use m_error_status

      implicit none

      contains


      subroutine rd_tabs( pdffil, lunrep, versio, serial, status)

!     Deltares Software Centre

!>/File
!>                read process definition tables from nefis format

!     Created   : June  1999 by Jan van Beek

!     Modified  : Aug   2012 by Jan van Beek : just read the tables

      use m_rd_tabr8
      use m_rd_tabr7
      use m_rd_tabr6
      use m_rd_tabr5
      use m_rd_tabr4
      use m_rd_tabr3
      use m_rd_tabr2
      use m_rd_tabr1
      use m_rd_tabp5
      use m_rd_tabp4
      use m_rd_tabp3
      use m_rd_tabp2
      use m_rd_tabp1
      use m_rd_tabm1
      use m_rd_filid
      use m_dhpfil
      use timers         !< performance timers
      use m_file_path_utils, only : extract_file_extension
      use m_process_lib_data

      implicit none

      ! arguments

      character(len=*)                  :: pdffil                 !< proces defintion file
      integer(kind=int_wp), intent(in   )  ::lunrep                 !< report file
      real(kind=real_wp), intent(  out)  ::versio                 !< version number proces defintion file
      integer(kind=int_wp), intent(  out)  ::serial                 !< serial number proces defintion file

      type(error_status), intent(inout) :: status !< current error status

!
!     declaration of file identification group
!
      real(kind=real_wp) ::vfform
      character(len=20)  rundat
      character(len=40)  fform      , conten      , & 
                   source
      character(len=40)  remark(4)
!
!     local variables
!
      integer(kind=int_wp) ::iconf           , ilen            , & 
                   iend            , i               , & 
                   ierror
      integer(kind=int_wp) ::deffds
      character(len=1)   coding, access
      logical       lexi
      character(len=256) fildef, fildat
      character(len=256) filext
      integer(kind=int_wp) ::extpos, extlen
!
!     external nefis functions
!
      integer(kind=int_wp) ::clsnef & 
              ,crenef
      external  clsnef & 
              ,crenef
      integer(kind=int_wp) ::ithndl = 0      ! handle for performance timer
      if (timon) call timstrt( "rd_tabs", ithndl )
!
!     initialize proces definition file
!
      call extract_file_extension (pdffil, filext, extpos, extlen)
      if ( filext /= ' ' ) then

         ! files with extension, assume nefis file made out of one file, fildat equals fildef

         fildat = pdffil
         fildef = pdffil

      else

         ! no extension assume nefis dat and def file, but check existence def file

         ilen = len(pdffil)
         iend = len_trim(pdffil)
         if ( iend == ilen ) iend = max(0,ilen-4)

         fildat = trim(pdffil)//'.dat'
         fildef = trim(pdffil)//'.def'
         inquire ( file=fildef , exist = lexi )
         if (.not.lexi) fildef = fildat
      endif


      ! open nefis file

      inquire ( file=fildat , exist = lexi )
      if ( lexi ) then
         access = 'r'
         coding = 'n'
         ierror = crenef(deffds, fildat, fildef, coding, access)
         if ( ierror == 8023) then
!           nefis error 8023 means it could not be read as a single file
!           when the file ends in .dat, try if it works when we specify a def-file
!           when the file ends in .def, try if it works when we specify a dat-file
            iend = len_trim(fildef)
            if ( fildef(iend-2:iend)=='dat') then
               fildef(iend-2:iend)='def'
               ierror = crenef(deffds, fildat, fildef, coding, access)
            endif
            if ( fildat(iend-2:iend)=='def') then
               fildat(iend-2:iend)='dat'
               ierror = crenef(deffds, fildat, fildef, coding, access)
            endif
         endif
         if ( ierror /= 0 ) then
            call status%increase_error_count()
            call dhpfil(lunrep,' error opening nefis file(s):',trim(fildat))
            write(lunrep,*) 'error number:',ierror
            goto 900
         endif
      else
         call status%increase_error_count()
         call dhpfil(lunrep,'error opening nefis file(s):',trim(fildat))
         write(lunrep,*) 'files do not exist'
         goto 900
      endif
!
!     last file identification group
!
      call rd_filid ( deffds,         fform , vfform, conten, & 
                     versio, serial, rundat, source, remark, & 
                     lunrep, ierror)
      if ( ierror /= 0 ) then
         call status%increase_error_count()
         write(lunrep,*) 'error reading file identification group'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
!
!     table p1 (substance groups)
!
      call rd_tabp1 ( deffds      , & 
                     nsgrpm      , nsgrp       , & 
                     sgrpid      , sgrpnm      , & 
                     lunrep      , ierror      )
      if ( ierror /= 0 ) then
         call status%increase_error_count()
         write(lunrep,*) 'error reading table p1'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
!
!     table p2 (items)
!
      call rd_tabp2 ( deffds      , & 
                     nitemm      , nitem       , & 
                     itemid      , itemnm      , & 
                     itemun      , itemde      , & 
                     itemag      , itemda      , & 
                     itemgr      , itemse      , & 
                     itemwk      , itemsn      , & 
                     itemsu      , lunrep      , & 
                     ierror      )
      if ( ierror /= 0 ) then
         call status%increase_error_count()
         write(lunrep,*) 'error reading table p2'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
!
!     table p3 (process modules)
!
      call rd_tabp3 ( deffds      , & 
                     nfortm      , nfort       , & 
                     fortid      , lunrep      , & 
                     ierror      )
      if ( ierror /= 0 ) then
         call status%increase_error_count()
         write(lunrep,*) 'error reading table p3'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
!
!     table p4 (processes)
!
      call rd_tabp4 ( deffds      , & 
                     nprocm      , num_processes_activated       , &
                     procid      , procnm      , & 
                     procfo      , procco      , & 
                     lunrep      , ierror      )
      if ( ierror /= 0 ) then
         call status%increase_error_count()
         write(lunrep,*) 'error reading table p4'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
!
!     table p5 (configurations)
!
      call rd_tabp5 ( deffds      , & 
                     nconfm      , nconf       , & 
                     confid      , confnm      , & 
                     lunrep      , ierror      )
      if ( ierror /= 0 ) then
         call status%increase_error_count()
         write(lunrep,*) 'error reading table p5'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
!
!     table r1 (configurations-processes)
!
      call rd_tabr1 ( deffds       , & 
                     nconfm*nprocm, nconf       , & 
                     num_processes_activated        , icnpro      , &
                     lunrep       , ierror      )
      if ( ierror /= 0 ) then
         call status%increase_error_count()
         write(lunrep,*) 'error reading table r1'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
!
!     table r2 (configurations-substances)
!
      call rd_tabr2 ( deffds      , & 
                     ncnsbm      , ncnsb       , & 
                     r2_cid      , r2_sid      , & 
                     lunrep      , ierror      )
      if ( ierror /= 0 ) then
         call status%increase_error_count()
         write(lunrep,*) 'error reading table r2'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
!
!     table r3 (input items)
!
      call rd_tabr3 ( deffds      , & 
                     ninpum      , ninpu       , & 
                     inpupr      , inpuit      , & 
                     inpunm      , inpude      , & 
                     inpudo      , inpusx      , & 
                     lunrep      , ierror      )
      if ( ierror /= 0 ) then
         call status%increase_error_count()
         write(lunrep,*) 'error reading table r3'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
!
!     table r4 (output items)
!
      call rd_tabr4 ( deffds      , & 
                     noutpm      , num_output_files       , &
                     outppr      , outpit      , & 
                     outpnm      , outpdo      , & 
                     outpsx      , lunrep      , & 
                     ierror      )
      if ( ierror /= 0 ) then
         call status%increase_error_count()
         write(lunrep,*) 'error reading table r4'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
!
!     table r5 (output fluxes)
!
      call rd_tabr5 ( deffds      , & 
                     noutfm      , noutf       , & 
                     outfpr      , outffl      , & 
                     outfnm      , outfdo      , & 
                     lunrep      , ierror      )
      if ( ierror /= 0 ) then
         call status%increase_error_count()
         write(lunrep,*) 'error reading table r5'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
!
!     table r6 (flux-substance)
!
      call rd_tabr6 ( deffds      , & 
                     nstocm      , nstoc       , & 
                     stocfl      , stocsu      , & 
                     stocsc      , lunrep      , & 
                     ierror      )
      if ( ierror /= 0 ) then
         call status%increase_error_count()
         write(lunrep,*) 'error reading table r6'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
!
!     table r7 (velocity-substance)
!
      call rd_tabr7 ( deffds      , & 
                     nvelom      , nvelo       , & 
                     veloit      , velosu      , & 
                     velosc      , lunrep      , & 
                     ierror      )
      if ( ierror /= 0 ) then
         call status%increase_error_count()
         write(lunrep,*) 'error reading table r7'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
!
!     table r8 (dispersion-substance)
!
      call rd_tabr8 ( deffds      , & 
                     ndispm      , ndisp       , & 
                     dispit      , dispsu      , & 
                     dispsc      , lunrep      , & 
                     ierror      )
      if ( ierror /= 0 ) then
         call status%increase_error_count()
         write(lunrep,*) 'error reading table r8'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
!
!     table m1 (old-items)
!
      if ( vfform > 1.99 ) then
         call rd_tabm1 ( deffds      , & 
                        n_old_items_max, & 
                        n_old_items, & 
                        old_items_old_name, & 
                        old_items_new_name, & 
                        old_items_old_default, & 
                        old_items_configuration, & 
                        old_items_serial, & 
                        old_items_action_type, & 
                        lunrep      , & 
                        ierror      )
         if ( ierror /= 0 ) then
            call status%increase_error_count()
            write(lunrep,*) 'error reading table m1'
            write(lunrep,*) 'error number:',ierror
            goto 900
         endif
      endif
!
!     close files
!
      ierror = clsnef(deffds)
      if ( ierror /= 0 ) then
         write(lunrep,*) 'error closing nefis process defintion file'
         write(lunrep,*) 'error number:',ierror
         call status%increase_error_count()
      endif

  900 continue
      if (timon) call timstop( ithndl )
      return

      end

      end module m_rd_tabs
