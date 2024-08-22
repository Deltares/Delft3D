!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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

      SUBROUTINE FNDDAT (WNDDIR, NWIND, NTIMW, ILOC, IEnd, ISTART, &
                         IDEBUG, SYEAR, SMO, SDAY, SHOUR, SMIN, INDX)

      INTEGER NWIND, NTIMW, ILOC, IT, ISTART, IEnd, IDEBUG, &
              SYEAR, SMO, SDAY, SHOUR, SMIN, INDX
      Double Precision WNDDIR (NWIND,NTIMW,2), TIME1, TIME2, TIMES

!
!********************************************************************
! Find date/time in wind arrays
!  start at index ISTART
!  always return a positive index for which date <=searched date
!  if date is before start of wind array, return index 1.
! for the time being a stupid linear search (update later with Binsrc)
!
! March 2000:
! - smart thing is that IStart is updated in CmpWindPrediction
! - IStart can be different for different wind stations!
!********************************************************************

      TIMES = SYEAR*10000. +SMO*100.+SDAY +SHOUR/100.+SMIN/10000.

      INDX = 1
      IF (TIMES .LE. WNDDIR(ILOC,1,1)) GOTO 999

      INDX = IEnd
      IF (TIMES .GT. WNDDIR(ILOC,IEnd,1)) GOTO 999

      DO IT=ISTART,IEnd-1
         TIME1 = WNDDIR(ILOC,IT,1)
         TIME2 = WNDDIR(ILOC,IT+1,1)
         IF (TIMES .GE. TIME1 .AND. TIMES .LT. TIME2) THEN
            INDX = IT
            GOTO 999
         ENDIF
      ENDDO

  999 CONTINUE
      IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' FNDDAT', TIMES, INDX, ISTART, IEnd

      RETURN
      END
