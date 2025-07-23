!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
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

      subroutine wrisub ( lu )
      use m_string_utils
      use m_waqpb_data

      integer     i, lu, iitem

!          ITEMS with defined ITEMGR are substances

          do i = 1,nitem
	    if (itemgr(i).ne.'') then
	        if (itemwk(i).eq.'x') then
      	        write (lu,1000) & 
             trim(itemid(i)),trim(itemnm(i)),trim(itemun(i))
	        else
      	        write (lu,1010) & 
             trim(itemid(i)),trim(itemnm(i)),trim(itemun(i))
	        endif
	    endif
	    enddo

!          ITEMS with undefined ITEMGR and defined ITEMDE are input

          do i = 1,nitem
	    if (itemgr(i).eq.''.and.itemde(i).ne.-999.) then
    	        write (lu,1020) & 
       trim(itemid(i)),trim(itemnm(i)),trim(itemun(i)),itemde(i)
	    endif
	    enddo

	    do i = 1,noutp
	        iitem = index_in_array(outpit(i), itemid(:nitem))
              if (iitem.le.0) stop 'Bug 17-01-2011'
	        write (lu,1030) trim(outpit(i)), trim(itemnm(iitem))
	    enddo

	    write (lu,1040)
	    write (lu,1041) (trim(procid(i)),trim(procnm(i)),i=1,nproc)
	    write (lu,1043)


 1000 format ('substance ''',a,''' active'/ & 
             '   description        ''',a,''''/ & 
             '   concentration-unit ''',a,''''/ & 
             '   waste-load-unit    ''-'''/ & 
             'end-substance')
 1010 format ('substance ''',a,''' inactive'/ & 
             '   description        ''',a,''''/ & 
             '   concentration-unit ''',a,''''/ & 
             '   waste-load-unit    ''-'''/ & 
             'end-substance')
 1020 format ('parameter ''',a,''''/ & 
             '   description   ''',a,''''/ & 
             '   unit          ''',a,''''/ & 
             '   value          ',g15.7/ & 
             'end-parameter')
 1030 format ('output ''',a,''''/ & 
             '   description   ''',a,''''/ & 
             'end-output')
 1040 format ('active-processes')
 1041 format ('   name  ''',a,''' ''',a,'''')
 1043 format ('end-active-processes')


      return
      end
