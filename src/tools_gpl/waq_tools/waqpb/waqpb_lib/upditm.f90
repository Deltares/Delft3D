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

      subroutine upd_p2 ( c10   , c50   , value , segmnt, newtab, & 
                         grp   , io_mes, iitem , c20   , newfrm, & 
                         bodem )
      use m_validate_input
      use m_string_utils
      use m_waqpb_data

      character*10 c10, naam
      character*20 c20
      character*30 grp
      character*50 c50, c50l
      real         value
      integer      jndex , segmnt, ihulp1, ihulp2, io_mes, ihulp, j
      integer      ihulp3, ihulp4, iitem
      logical      newtab, newfrm, bodem
      integer      nitem0
      save         nitem0
      data         nitem0 /-999/

      if ( nitem0 .lt. 0 ) nitem0 = nitem

!          segmnt = 1: item defined within segment
!          segmnt = 2: item defined on exchanges
!          segmnt = 0: item defined as substance

      jndex = index_in_array(c10, itemid(:nitem))
      if ( jndex .le. 0 ) then

!              NEW ITEM

          jndex = nitem + 1
          if (jndex.gt.nitemm) stop 'DIMENSION NITEMM'
          nitem = jndex
          itemid(jndex) = c10

!              Alternative approach for newfrm and EXISTING formats

          if (newfrm) then
!                  newfrm format
              if ( c50 .eq. ' ' ) then
                  itemnm(jndex) = 'undefined'
               else
                  itemnm(jndex) = c50
              endif
              if ( c20 .eq. ' ' ) then
                  itemun(jndex) = '{no unit}'
               else
                  itemun(jndex) = c20
              endif
!                  end newfrm format
	    else
!                  existing format
              if ( c50 .eq. ' ' ) then
                  itemnm(jndex) = 'undefined'
                  itemun(jndex) = '{no unit}'
               else
!                      Separate descriptions from units
                  ihulp = 0
                  c50l = c50
                  call finuni ( c50l , ihulp )
                  if ( ihulp .gt. 0 ) then
!                      ihulp = max(ihulp,31)
                  itemun(jndex) = c50l(ihulp:50)
                  do 145 j = ihulp, 50
  145             c50l(j:j) = ' '
                  else
                      itemun(jndex) = '{no unit}'
                  endif
                  itemnm(jndex) = c50l
              endif
!                  end existing format
	    endif
          itemse(jndex) = ' '
          itemex(jndex) = ' '
          itemde(jndex) = -999.
          itemag(jndex) = 'volume'
          itemda(jndex) = 'volume'
          itemgr(jndex) = ' '
          itemwk(jndex) = ' '
          if ( .not. newtab ) write ( io_mes , 1000 ) c10
 1000     format ('Item       ',a10,' added to table P2')
      else

!              Existing item, only actions if certain fields are undefined

!              Alternative approach for newfrm and EXISTING formats

          if (newfrm) then
!                  newfrm format
              if ( itemnm(jndex) .eq. 'undefined' .and. & 
                 c50 .ne. ' ' ) itemnm(jndex) = c50
              if ( itemun(jndex) .eq. '{no unit}' .and. & 
                 c20 .ne. ' ' ) itemun(jndex) = c20
!                  end newfrm format
	    else
!                  existing format
              if ( itemnm(jndex) .eq. 'undefined' .and. & 
                 c50 .ne. ' ' ) then
!                      Separate descriptions from units
                  ihulp = 0
                  c50l = c50
                  call finuni ( c50l , ihulp )
                  if ( ihulp .gt. 0 ) then
!                      ihulp = max(ihulp,31)
                  itemun(jndex) = c50l(ihulp:50)
                  do 146 j = ihulp, 50
  146             c50l(j:j) = ' '
                  else
                      itemun(jndex) = '{no unit}'
                  endif
                  itemnm(jndex) = c50l
              endif
!                  end existing format
          endif
      endif

      call validate_units(itemun(jndex), io_mes)

!          Actions below ONLY for new items

      if ( jndex .gt. nitem0 ) then
          if ( segmnt .eq. 0 ) then

!              Alternative approach for newfrm and EXISTING formats

          if (newfrm) then
!                  newfrm format
              itemgr(jndex) = grp
              if ( bodem ) then
                  itemwk(jndex) = ' '
              else
                  itemwk(jndex) = 'x'
              endif
!                  end newfrm format
	    else
!                  existing format
              itemgr(jndex) = grp
              ihulp1 = index ( c10 , 'S1' )
              ihulp2 = index ( c10 , 'S2' )

              if ( ihulp1 .gt. 2 .or. ihulp2 .gt. 2 .or. & 
                  string_equals(c10(1:4),'SOD ') .or. & 
                  string_equals(c10(1:6),'Zsand ')) then
                  itemwk(jndex) = ' '
              else
                  itemwk(jndex) = 'x'
              endif
!                  end existing format
	    endif
          endif
          if ( segmnt .eq. 1 ) itemse(jndex) = 'x'
          if ( segmnt .eq. 2 ) itemex(jndex) = 'x'
!              The "special values" are excluded as defaults
!               if ( abs(value+999.) .gt. 1e-10 .and.
!          j         abs(value+888.) .gt. 1e-10 ) itemde(jndex) = value
           if ( abs(value+999.) .gt. 1e-10 .and. & 
               abs(value+888.) .gt. 1e-10 .and. & 
               abs(value+101.) .gt. 1e-10 .and. & 
               abs(value+11.)  .gt. 1e-10 .and. & 
               abs(value+1.)   .gt. 1e-10      ) & 
                         itemde(jndex) = value
!     

      endif

!          Set item number

      iitem = jndex

      end subroutine upd_p2


      subroutine upd_p3 ( c10 , newtab , io_mes )
      use m_string_utils
      use m_waqpb_data
      character*10 c10
      logical newtab
      integer io_mes, jndex

      jndex = index_in_array(c10, fortid(:nfort))
      if ( jndex .le. 0 ) then
          jndex = nfort + 1
          if (jndex.gt.nfortm) stop 'DIMENSION NFORTM'
          nfort = jndex
          fortid(jndex) = c10
          if ( .not. newtab ) write ( io_mes , 1000 ) c10
      endif
 1000 format ('Subroutine ',a10,' added to table P3')
      return
      end subroutine upd_p3
