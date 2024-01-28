!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

   subroutine maketekaltimes()
   use m_flowtimes
   use time_module, only : seconds_to_datetimestring
   implicit none
   logical          :: jawel
   integer          :: minp, mout, i, k
   double precision :: tim, a(30)

   character*20 dateandtime

   inquire (file = '102023.DAT', exist = jawel)
   if (jawel) then
     refdat = '19920831'
     call oldfil(minp, '102023.DAT')
     call newfil(mout, '102023.tek')
     write(mout,'(a)')      '*COLUMN  1 : DATE'
     write(mout,'(a)')      '*COLUMN  2 : TIME'
     write(mout,'(a)')      '*COLUMN  3 : REF TEU'
     write(mout,'(a)')      '*COLUMN  4 : V TEU'
     write(mout,'(a)')      '*COLUMN  5 : Z TEU'
     write(mout,'(a)')      '*COLUMN  6 : TRIM TEU '
     write(mout,'(a)')      '*COLUMN  7 : VX2 STROOM'
     write(mout,'(a)')      '*COLUMN  8 : VY2 STROOM '
     write(mout,'(a)')      '*COLUMN  9 : GOLF 1    '
     write(mout,'(a)')      '*COLUMN 10 : GOLF 2'
     write(mout,'(a)')      '*COLUMN 11 : GOLF 3'
     write(mout,'(a)')      '*COLUMN 12 : X COG PAN '
     write(mout,'(a)')      '*COLUMN 13 : Y COG PAN '
     write(mout,'(a)')      '*COLUMN 14 : Z COG PAN '
     write(mout,'(a)')      '*COLUMN 15 : ROLL PAN  '
     write(mout,'(a)')      '*COLUMN 16 : PITCH PAN '
     write(mout,'(a)')      '*COLUMN 17 : YAW PAN '
     write(mout,'(a)')      '*COLUMN 18 : F LIJN 1 '
     write(mout,'(a)')      '*COLUMN 19 : F LIJN 2 '
     write(mout,'(a)')      '*COLUMN 20 : F LIJN 3 '
     write(mout,'(a)')      '*COLUMN 21 : F LIJN 4 '
     write(mout,'(a)')      '*COLUMN 22 : F LIJN 5 '
     write(mout,'(a)')      '*COLUMN 23 : F LIJN 6 '
     write(mout,'(a)')      '*COLUMN 24 : FX TOT   '
     write(mout,'(a)')      '*COLUMN 25 : FY TOT   '
     write(mout,'(a)')      '*COLUMN 26 : MZ TOT   '
     write(mout,'(a)')      '*COLUMN 27 : FX FEND V'
     write(mout,'(a)')      '*COLUMN 28 : FY FEND V'
     write(mout,'(a)')      '*COLUMN 29 : FZ FEND V'
     write(mout,'(a)')      '*COLUMN 30 : FX FEND A'
     write(mout,'(a)')      '*COLUMN 31 : FY FEND A'
     write(mout,'(a)')      '*COLUMN 32 : FZ FEND A'
     write(mout,'(a)') 'bl01'
     write(mout,'(a)') '6202  32'

     do i=1,4
        read(minp,*)
     enddo
     do i=1,6202
        read(minp,*) tim, (a(k), k = 1,30)
        call seconds_to_datetimestring(dateandtime, refdat, tim)
        dateandtime(9:9) = ' '
        write(mout,'(a, 30F10.3)') dateandtime, (a(k), k = 1,30)
     enddo
     call doclose(minp)
     call doclose(mout)

   endif

   end subroutine maketekaltimes
