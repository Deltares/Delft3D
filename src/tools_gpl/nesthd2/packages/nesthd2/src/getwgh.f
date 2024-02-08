      subroutine getwgh (lundia,fout  ,lunadm,mcbsp ,ncbsp ,typbnd,
     *                   mnes  ,nnes  ,weight,angle               )
      implicit none
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
!***********************************************************************
! Deltares                         marine and coastal management
!
! subroutine         : getwgh
! version            : v1.0
! date               : July 1997
! programmer         : Theo van der Kaaij
!
! function           : Gets coordinates and weights of nesting station
!                      with coordinates (mcbsp,ncbsp)
! limitations        :
! subroutines called :
!***********************************************************************
      integer lundia, lunadm, k1, k2, m, n, mcbsp, ncbsp, iwght
      double precision angle
      integer       mnes   (  4  ), nnes (  4  )

      double precision pi

      double precision weight (  4  )

      character*1   typbnd
      character*100 record

      logical       fout

      if (.not. (typbnd == 'z' .or.typbnd == 'c') ) then
          write (lundia,'(a,a,a)') ' *** Warning: The boundary type ',
     *                              typbnd, ' is not supported'
          return
      endif
      pi = acos(-1.)
!
!-----skip first lines administration file
!
      rewind (lunadm)
   10 read (lunadm,'(a100)',end=998) record
      if (record( 1: 1) .eq. '*') goto 10


      backspace (lunadm)
!
!-----search for (mcbsp,ncbsp)
!
   20 read (lunadm,'(a100)',end=998) record
      if ((typbnd .eq. 'z' .and. record(25:29) .eq. 'water' ) .or.
     *    (typbnd .eq. 'c' .and. record(25:29) .eq. 'veloc' ) )then

         k1 = index(record,'(M,N)')+9  ! case sensitive
         k2 = index(record(k1:),',')-1
         read (record(k1:k1+k2-1),*) m
         k1 = k1+k2+1
         read (record(k1:k1+k2-1),*) n

         if (m .eq. mcbsp .and. n .eq. ncbsp) then
!
!-----------read orientation for velocity boundary
!
            if (typbnd .eq. 'c') then
               k1 = index(record,'Angle') + 7    ! case sensitive
               read (record(k1:),*) angle
               angle = angle*pi/180.
            endif
!
!-----------read coordinates and weights
!

            do 30 iwght = 1, 4
               read (lunadm,*) mnes   (iwght), nnes(iwght),
     *                         weight (iwght)
   30       continue
            goto 999
         else
            goto 20
         endif
      else
         goto 20
      endif

      goto 20

  998 continue

      write (lundia,'(''*** Error: Administration file corrupt'')')
      fout = .true.

  999 continue

      return

      end
