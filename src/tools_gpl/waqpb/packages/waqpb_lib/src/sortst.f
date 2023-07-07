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

      subroutine sortst ( c10a , c10b , val , nr )
      use m_zoek

c     Subroutine to sort (part of) tables R6-R7-R8
      integer      nr, ir, jndex
      character*10 c10a(nr), c10b(nr), evea, eveb, sortar(nr)
      real         val(nr), eveval
      logical      flag
      integer      sortnr(nr), evenr, nrarr

c     Set index

      nrarr = 0
      do 50 ir = 1,nr 
          call zoek ( c10a(ir), nrarr, sortar, 10, jndex )
          if ( jndex .le. 0 ) then
              nrarr = nrarr + 1
              sortar(nrarr) = c10a(ir)
              jndex = nrarr
          endif
          sortnr(ir) = jndex
   50 continue
 
c     Perform sort

  100 continue
      flag = .false.
      do 200 ir = 1,nr-1
          if ( sortnr(ir+1) .lt. sortnr(ir) ) then
              flag = .true.
              evenr  = sortnr(ir+1)
              evea   = c10a  (ir+1)
              eveb   = c10b  (ir+1)
              eveval = val   (ir+1)
              sortnr(ir+1) = sortnr(ir)
              c10a  (ir+1) = c10a  (ir)
              c10b  (ir+1) = c10b  (ir)
              val   (ir+1) = val   (ir)
              sortnr(ir) = evenr
              c10a  (ir) = evea
              c10b  (ir) = eveb
              val   (ir) = eveval
          endif
  200 continue

c     back for next sweep

      if ( flag ) goto 100
      return
      end

      subroutine sorts2 ( pr    , it    , nm    , de    , do    ,
     j                    sx    , nr    , do_de , do_sx )
      use m_zoek


c     Subroutine to sort tables R3-R4-R5

      integer      nr, nm(nr), sx(nr), ir, evenm, evesx
      character*10 pr(nr), it(nr), sortar(nr), evepr, eveit
      character*1  de(nr), do(nr), evede, evedo
      logical      flag  , do_de, do_sx
      integer      sortnr(nr), evenr, nrarr, irarr, noffse,
     j             nrsub, jndex

c     Set index for process

c      write (*,*) ' SORTS2 '
      nrarr = 0
      do ir = 1,nr
          call zoek ( pr(ir), nrarr, sortar, 10, jndex )
          if ( jndex .le. 0 ) then
              nrarr = nrarr + 1
              sortar(nrarr) = pr(ir)
              jndex = nrarr
          endif
          sortnr(ir) = jndex
      enddo
c      write (*,*) ' Processes indexed '

c     Perform sort on process

  100 continue
      flag = .false.

      do ir = 1,nr-1
          if ( sortnr(ir+1) .lt. sortnr(ir) ) then
              flag = .true.
              evenr  = sortnr(ir+1)
              evepr  = pr    (ir+1)
              eveit  = it    (ir+1)
              evenm  = nm    (ir+1)
              if ( do_de )
     j        evede  = de    (ir+1)
              evedo  = do    (ir+1)
              if ( do_sx )
     j        evesx  = sx    (ir+1)
              sortnr(ir+1) = sortnr(ir)
              pr    (ir+1) = pr    (ir)
              it    (ir+1) = it    (ir)
              nm    (ir+1) = nm    (ir)
              if ( do_de )
     j        de    (ir+1) = de    (ir)
              do    (ir+1) = do    (ir)
              if ( do_sx )
     j        sx    (ir+1) = sx    (ir)
              sortnr(ir) = evenr
              pr    (ir) = evepr
              it    (ir) = eveit
              nm    (ir) = evenm
              if ( do_de )
     j        de    (ir) = evede
              do    (ir) = evedo
              if ( do_sx )
     j        sx    (ir) = evesx
          endif
      enddo

c     back for next sweep

      if ( flag ) goto 100
c      write (*,*) ' Processes sorted '
c      do ir = 1,nr
c          write (*,*) ir,sortnr(ir),pr(ir),it(ir)
c      enddo

c     Sort on number!!!

      noffse = 0
      do irarr = 1,nrarr

c         Find items within current process

          do ir = noffse + 1, nr
c              write (*,*) ' ir ',ir
              if ( sortnr(ir) .ne. sortnr(noffse+1) ) then
                  nrsub = ir-1-noffse
                  goto 200
              endif
              if ( ir .eq. nr ) then
                  nrsub = ir-noffse
                  goto 200
              endif
          enddo
  200     continue
c          write (*,*) ' Cycle ',irarr, ' from ',noffse+1,' to ',
c     j                 noffse+nrsub

c         Sort

  300     continue
          flag = .false.

          do ir = noffse+1,noffse+nrsub-1
c              write (*,*) ' ir2 ',ir
              if ( nm(ir+1) .lt. nm(ir) ) then
                  flag = .true.
                  evepr  = pr    (ir+1)
                  eveit  = it    (ir+1)
                  evenm  = nm    (ir+1)
                  if ( do_de )
     j            evede  = de    (ir+1)
                  evedo  = do    (ir+1)
                  if ( do_sx )
     j            evesx  = sx    (ir+1)
                  pr    (ir+1) = pr    (ir)
                  it    (ir+1) = it    (ir)
                  nm    (ir+1) = nm    (ir)
                  if ( do_de )
     j            de    (ir+1) = de    (ir)
                  do    (ir+1) = do    (ir)
                  if ( do_sx )
     j            sx    (ir+1) = sx    (ir)
                  pr    (ir) = evepr
                  it    (ir) = eveit
                  nm    (ir) = evenm
                  if ( do_de )
     j            de    (ir) = evede
                  do    (ir) = evedo
                  if ( do_sx )
     j            sx    (ir) = evesx
              endif
          enddo

c         back for next sweep
          if ( flag ) goto 300

          noffse = noffse + nrsub
      enddo
c      write (*,*) ' Items sorted '


      if ( noffse .ne. nr ) stop 'BUG SortNR'
      return
      end

