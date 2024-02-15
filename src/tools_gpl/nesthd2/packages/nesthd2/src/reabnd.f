!reabnd
      subroutine reabnd (lun   , mcbsp , ncbsp , typbnd, nambnd,
     *                   nobnd                                 )
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
!
!***********************************************************************
! Deltares                         marine and coastal management
!
! subroutine         : reabnd
! version            : v1.0
! date               : June 1997
! programmer         : Theo van der Kaaij
!
! function           : read boundary definition from attribute file
! method             :
! limitations        :
! error messages     :
! subroutines        :
!
!***********************************************************************
!
      integer       mcbsp (nobnd, 2), ncbsp (nobnd, 2)
!
      character*  1 char
      character*  1 time
      character*  1 typbnd (nobnd)
      character* (*) nambnd (nobnd)
      character* 20 nmbnd
      character* 80 record
!
      rewind (lun)
      ibnd   = 0

  160 read (lun   ,'(a)',end=200) record

      ip = 20
  170 ip = ip + 1
      if( record(ip:ip) .eq. ' ' )then
         goto 170
      else
         char   = record (ip:ip)
         call small (char,1)
         goto 180
      endif

  180 ip = ip + 1
      if( record(ip:ip) .eq. ' ' )then
         goto 180
      else
         time   = record (ip:ip)
         call small (time  ,1     )
      endif

      if (time   .eq. 't') then
         ibnd = ibnd + 1
         read (record( 1: 20), '(a20)') nambnd(ibnd)
         typbnd (ibnd) = char

         ip = ip + 1
         read (record(ip:80),*)
     *        mcbsp (ibnd,1), ncbsp (ibnd,1),
     *        mcbsp (ibnd,2), ncbsp (ibnd,2)

      else
         read (record( 1:20),'(a20)') nmbnd

         write (*,'('' *** Warning: No BC generated'',
     *              '' for boundary :'',a20)') nmbnd
         write (*,'(''              Not defined as time-series BC'')')
      endif

      goto 160

  200 continue

      write (*,'( )')

      end
