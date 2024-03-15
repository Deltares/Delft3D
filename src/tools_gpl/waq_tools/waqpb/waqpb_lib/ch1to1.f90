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

      subroutine ch1to1 (lu_mes)
      use m_string_utils
      use m_waqpb_data

      integer lu_mes
      logical item_produced_by_process(nitemm)
      integer iconf, item, iproc, ioutp
      character*10 last_process

      do iconf = 1,nconf

!              Zero check list

          do item = 1,nitem
              item_produced_by_process(item) = .false.
          enddo

!              Loop over output items

          last_process = 'notyetdone'
          do ioutp=1,noutp
              if ( outppr(ioutp) .ne. last_process ) then
                  last_process = outppr(ioutp)
                  iproc = index_in_array(last_process,procid(:nproc))
                  if ( iproc .le. 0 ) then
                     stop 'CH1TO1 BUG 01'
                  endif
              endif
              if (conpro(iconf,iproc)) then

!                      Process in current configuration
                  if ( outpdo(ioutp) .ne. ' ' ) then
                      item = index_in_array(outpit(ioutp),itemid(:nitem))
                      if ( item .le. 0 ) stop 'CH1TO1 BUG 02'
                      if ( item_produced_by_process(item) ) then
                          write (lu_mes,'(''Item '',a10, & 
                                '' produced twice in conf '',a10)') & 
                         itemid(item),confid(iconf)
                      else
                          item_produced_by_process(item) = .true.
                      endif
                  endif
              endif
          enddo
      enddo

      return
      end
