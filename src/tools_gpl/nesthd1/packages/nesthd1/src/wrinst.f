      subroutine wrinst (lun   , mcbsp , ncbsp , mcnes , ncnes , weight,
     *                   angle , maxbnd, nobnd , type                  )
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
! subroutine         : wrinst
! version            : v1.0
! date               : June 1997
! specs last update  :
! programmer         : Theo van der Kaaij
!
! function           : write nest administration to file
!
!***********************************************************************

      integer       mcbsp (nobnd ,2)  , ncbsp (nobnd ,2)  ,
     *              mcnes (maxbnd,2,4), ncnes (maxbnd,2,4)

      real          angle (maxbnd)

      real          weight(maxbnd,2,4)

      character*  2 type

      do 10 ibnd = 1, nobnd
         do 10 isize = 1, 2
            !
            ! Case sensitive, nesthd2 interpreted the strings    
            !
            if (type .eq. 'WL') then
               write (lun   ,'(''Nest administration for water level'',
     *                         '' support point (M,N) = ('',i5,'','',
     *                         i5,'')'')') mcbsp(ibnd,isize),
     *                                     ncbsp(ibnd,isize)
            else
               write (lun   ,'(''Nest administration for velocity   '',
     *                         '' support point (M,N) = ('',
     *                         i5,'','',i5,'')'', '' Angle = '',f8.3)')
     *                         mcbsp(ibnd,isize), ncbsp(ibnd,isize),
     *                         angle(ibnd)
            endif

            do 10 inst = 1, 4
               write (lun   ,'(2(1x,i5),1x,f7.4)')
     *               mcnes (ibnd,isize,inst), ncnes (ibnd,isize,inst),
     *               weight(ibnd,isize,inst)
   10 continue

!-----------------------------------------------------------------------
!---- return to calling module
!-----------------------------------------------------------------------

      return

      end

