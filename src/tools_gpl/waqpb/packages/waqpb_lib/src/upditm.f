!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
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

      subroutine upd_p2 ( c10   , c50   , value , segmnt, newtab,
     j                    grp   , io_mes, iitem , c20   , newfrm,
     j                    bodem )
      use m_zoek

      include 'data.inc'
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

c     segmnt = 1: item defined within segment
c     segmnt = 2: item defined on exchanges
c     segmnt = 0: item defined as substance

      call zoek ( c10, nitem, itemid, 10, jndex)
      if ( jndex .le. 0 ) then

c         NEW ITEM

          jndex = nitem + 1
          if (jndex.gt.nitemm) stop 'DIMENSION NITEMM'
          nitem = jndex
          itemid(jndex) = c10

c         Alternative approach for newfrm and EXISTING formats

          if (newfrm) then
c             newfrm format
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
c             end newfrm format
	    else
c             existing format
              if ( c50 .eq. ' ' ) then
                  itemnm(jndex) = 'undefined'
                  itemun(jndex) = '{no unit}'
               else
c                 Separate descriptions from units
                  ihulp = 0
                  c50l = c50
                  call finuni ( c50l , ihulp )
                  if ( ihulp .gt. 0 ) then
c                 ihulp = max(ihulp,31)
                  itemun(jndex) = c50l(ihulp:50)
                  do 145 j = ihulp, 50
  145             c50l(j:j) = ' '
                  else
                      itemun(jndex) = '{no unit}'
                  endif
                  itemnm(jndex) = c50l
              endif
c             end existing format
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

c         Existing item, only actions if certain fields are undefined

c         Alternative approach for newfrm and EXISTING formats

          if (newfrm) then
c             newfrm format
              if ( itemnm(jndex) .eq. 'undefined' .and.
     j            c50 .ne. ' ' ) itemnm(jndex) = c50
              if ( itemun(jndex) .eq. '{no unit}' .and.
     j            c20 .ne. ' ' ) itemun(jndex) = c20
c             end newfrm format
	    else
c             existing format
              if ( itemnm(jndex) .eq. 'undefined' .and.
     j            c50 .ne. ' ' ) then
c                 Separate descriptions from units
                  ihulp = 0
                  c50l = c50
                  call finuni ( c50l , ihulp )
                  if ( ihulp .gt. 0 ) then
c                 ihulp = max(ihulp,31)
                  itemun(jndex) = c50l(ihulp:50)
                  do 146 j = ihulp, 50
  146             c50l(j:j) = ' '
                  else
                      itemun(jndex) = '{no unit}'
                  endif
                  itemnm(jndex) = c50l
              endif
c             end existing format
	    endif
      endif

c     Actions below ONLY for new items

      if ( jndex .gt. nitem0 ) then
          if ( segmnt .eq. 0 ) then

c         Alternative approach for newfrm and EXISTING formats

          if (newfrm) then
c             newfrm format
              itemgr(jndex) = grp
              if ( bodem ) then
                  itemwk(jndex) = ' '
              else
                  itemwk(jndex) = 'x'
              endif
c             end newfrm format
	    else
c             existing format
              itemgr(jndex) = grp
              ihulp1 = index ( c10 , 'S1' )
              ihulp2 = index ( c10 , 'S2' )
              naam = 'SOD '
              call zoek (c10,1,naam,4,ihulp3)
              naam = 'Zsand '
              call zoek (c10,1,naam,6,ihulp4)
              if ( ihulp1 .gt. 2 .or. ihulp2 .gt. 2 .or.
     j             ihulp3 .eq. 1 .or. ihulp4 .eq. 1 ) then
                  itemwk(jndex) = ' '
              else
                  itemwk(jndex) = 'x'
              endif
c             end existing format
	    endif
          endif
          if ( segmnt .eq. 1 ) itemse(jndex) = 'x'
          if ( segmnt .eq. 2 ) itemex(jndex) = 'x'
c         The "special values" are excluded as defaults
c          if ( abs(value+999.) .gt. 1e-10 .and.
c     j         abs(value+888.) .gt. 1e-10 ) itemde(jndex) = value
           if ( abs(value+999.) .gt. 1e-10 .and.
     j          abs(value+888.) .gt. 1e-10 .and.
     j          abs(value+101.) .gt. 1e-10 .and.
     j          abs(value+11.)  .gt. 1e-10 .and.
     j          abs(value+1.)   .gt. 1e-10      )
     j                    itemde(jndex) = value
c     

      endif

c     Set item number

      iitem = jndex

      return
      end


      subroutine upd_p3 ( c10 , newtab , io_mes )
      use m_zoek

      include 'data.inc'
      character*10 c10
      logical newtab
      integer io_mes, jndex

      call zoek ( c10, nfort, fortid, 10, jndex)
      if ( jndex .le. 0 ) then
          jndex = nfort + 1
          if (jndex.gt.nfortm) stop 'DIMENSION NFORTM'
          nfort = jndex
          fortid(jndex) = c10
          if ( .not. newtab ) write ( io_mes , 1000 ) c10
      endif
 1000 format ('Subroutine ',a10,' added to table P3')
      return
      end
