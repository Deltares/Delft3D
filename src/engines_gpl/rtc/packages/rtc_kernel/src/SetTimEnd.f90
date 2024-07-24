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

      SUBROUTINE SetTimEnd (ChkTime, IEvent, Idebug)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RTC  version 1.0.                   Date: June 1997
! *********************************************************************
! *** Last update: June   1997       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Given start date, event duration: set end date
! *********************************************************************
! *********************************************************************

      Use ParameterModule
      Use LocationDataModule
      Use OtherData

      Double Precision ChkTime
      Integer          IEvent

      INTEGER   SYEAR, SMO, SDAY, SHOUR, SMIN, SSEC
      DOUBLE PRECISION RSSEC
      INTEGER   I, Idebug

       If (Idebug .gt. 0) Write(Idebug,*) ' SetTimEnd'

       SYEAR = EvStrt(Ievent,1)
       SMO   = EvStrt(Ievent,2)
       SDAY  = EvStrt(Ievent,3)
       SHOUR = EvStrt(Ievent,4)
       SMIN  = EvStrt(Ievent,5)
       SSEC  = EvStrt(Ievent,6)
       RSSEC = SSEC

       ChkTime=SYEAR*10000. +SMO*100.+SDAY + Dble(Float(SHOUR)/100.+ Float(SMIN)/10000.)
       If (Idebug .gt. 0) Write(Idebug,*) ' Start event',ChkTime

       If (NEvent .gt. 1) Then
          LASTTM = EVDURA(Ievent,1)*86400 + EVDURA(Ievent,2)*3600 +  EVDURA(Ievent,3)*60  + EVDURA(Ievent,4)
          LASTTM = LASTTM / ITMSIZ
       Endif

       DO I=1,LASTTM+NTIMH
          CALL NXTSTP (IDEBUG, SYEAR,SMO,SDAY, SHOUR, SMIN, SSEC, RSSEC, IDHR, IDMIN, RDSEC)
       ENDDO
       ChkTime=SYEAR*10000. +SMO*100.+SDAY + Dble(Float(SHOUR)/100.+Float(SMIN)/10000.)
       If (Idebug .gt. 0) Write(Idebug,*) ' End event',ChkTime

      RETURN
      END
