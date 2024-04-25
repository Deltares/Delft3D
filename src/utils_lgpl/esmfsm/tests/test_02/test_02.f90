!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
      program test_02
      use precision     ! pntrsize, used in fsm.i
      implicit none
!
      include 'fsm.i'
!     
      integer                :: i 
      integer(kind=pntrsize) :: iptr
      character*6            :: pntnam
!      
      integer fsm_flags
      integer mcid
      integer res

      mcid = 0
      fsm_flags = FSM_TRACE ! FSM_SILENT, FSM_TRACE
      res = fsmini(mcid, fsm_flags)
!
      write(*,*) 'Test programma FSM'
!
      pntnam = 'a1'
      iptr = makptr('a1', 1, 3000)
      write(*,'('' Makptr '',a6,'' : '',i10)') pntnam, iptr
!
      pntnam = 'a2'
      iptr = makptr(pntnam, 2, 3250)
      write(*,'('' Makptr '',a6,'' : '',i10)') pntnam, iptr
!
      pntnam = 'a3'
      iptr = makptr(pntnam, 1, 3500)
      write(*,'('' Makptr '',a6,'' : '',i10)') pntnam, iptr
!
      pntnam = 'a4'
      iptr = makptr(pntnam, 2, 3000)
      write(*,'('' Makptr '',a6,'' : '',i10)') pntnam, iptr
!
      pntnam = 'a5'
      iptr = makptr(pntnam, 1, 3000)
      write(*,'('' Makptr '',a6,'' : '',i10)') pntnam, iptr
!
      pntnam = 'a6'
      iptr = makptr(pntnam, 2, 3000)
      write(*,'('' Makptr '',a6,'' : '',i10)') pntnam, iptr
!
      pntnam = 'a6'
      iptr = relptr(pntnam)
      write(*,'('' Relptr '',a6,'' : '',i10)') pntnam, iptr
!
      pntnam = 'a6'
      iptr = makptr(pntnam, 1, 4000)
      write(*,'('' Makptr '',a6,'' : '',i10)') pntnam, iptr
!
      do 100 i = 1, 3
        pntnam = 'a5'
        iptr = relptr(pntnam)
        write(*,'('' Relptr '',i2,a6,'' : '',i10)') i,pntnam, iptr
          res = prtkey()

        pntnam = 'a3'
        iptr = relptr(pntnam)
        write(*,'('' Relptr '',i2,a6,'' : '',i10)') i,pntnam, iptr
          res = prtkey()

        pntnam = 'a5'
        iptr = makptr(pntnam, 2, 5000)
        write(*,'('' Makptr '',i2,a6,'' : '',i10)') i,pntnam, iptr
          res = prtkey()

        pntnam = 'a3'
        iptr = makptr(pntnam, 1, 1000)
        write(*,'('' Makptr '',i2,a6,'' : '',i10)') i,pntnam, iptr
          res = prtkey()
  100 continue
!
        pntnam = 'a1'
        iptr = relptr(pntnam)
        pntnam = 'a2'
        iptr = relptr(pntnam)
        pntnam = 'a3'
        iptr = relptr(pntnam)
        pntnam = 'a4'
        iptr = relptr(pntnam)
        pntnam = 'a5'
        iptr = relptr(pntnam)
        pntnam = 'a6'
        iptr = relptr(pntnam)
!
      stop
      end
