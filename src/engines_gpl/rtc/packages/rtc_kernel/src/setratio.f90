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

      Subroutine SetRatio (Ratio, ILoc, Indx, Indx1, Itype, &
                           IDEBUG, SYEAR, SMO, SDAY, SHOUR, SMIN, SSec)

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


      Double Precision Ratio
      Integer Iloc, Indx, Indx1, Itype
      Integer IDate1, IDate2, IDate3
      Integer ITime1, ITime2, Itime3
      Double precision JulianDate1, JulianDate2, JulianDate3, Julian, ChkTime1, ChkTime2

      INTEGER   SYEAR, SMO, SDAY, SHOUR, SMIN, SSEC
      INTEGER   TYEAR, TMO, TDAY, THOUR, TMIN, TSEC
      INTEGER   Idebug

      If (Idebug .gt. 0) Write(Idebug,*) ' SetRatio'


      If (Itype .eq. 1) Then
          ChkTime1 = WndDir(ILOC,Indx,1)
          ChkTime2 = WndDir(ILOC,Indx1,1)
      ElseIf (Itype .eq. 2) Then
          ChkTime1 = WndVel(ILOC,Indx,1)
          ChkTime2 = WndVel(ILOC,Indx1,1)
      Endif
!     Current date/time
      Idate3 = 10000*SYear + 100*SMo  + SDay
      Itime3 = 10000*SHour + 100*Smin + SSec
      JulianDate3= Julian (idate3,itime3)

!     Table date/time 1 for interpolation
      if (idebug .gt. 0)  write(idebug,*) ' ChkTime1 ', ChkTime1
      TYear    =  ChkTime1 / 10000
      ChkTime1 =  ChkTime1 - 10000 * TYear
      TMo      =  ChkTime1 / 100
      ChkTime1 =  ChkTime1 - 100 * TMo
      TDay     =  ChkTime1
      ChkTime1 =  ChkTime1 - TDay
      THour    =  ChkTime1 * 100
      ChkTime1 =  ChkTime1 - 0.01 * THour
      TMin     =  ChkTime1 * 10000
      ChkTime1 =  ChkTime1 - 0.01 * TMin
      TSec     =  0
! ARS XXXX May 21, 2002: check correctness date/time; IHour =0..23, IMinute = 0..59
      If (Tmin .ge. 60) then
          Tmin = 0
          Thour = Thour + 1
      Endif
      If (Thour .ge. 24) then
         Thour = 0
         Call NxtDay (Idebug, TYear, TMo, TDay)
      Endif
! End ARS
      if (idebug .gt. 0)  write(idebug,*) ' Tyear etc', Tyear, Tmo, TDay, THour, TMin, TSec
      Idate1 = 10000*TYear + 100*TMo  + TDay
      Itime1 = 10000*THour + 100*Tmin + TSec
      JulianDate1= Julian (idate1,itime1)

!     Table date/time 2 for interpolation
      if (idebug .gt. 0)  write(idebug,*) ' ChkTime2 ', ChkTime2
      TYear    =  ChkTime2 / 10000
      ChkTime2 =  ChkTime2 - 10000 * TYear
      TMo      =  ChkTime2 / 100
      ChkTime2 =  ChkTime2 - 100 * TMo
      TDay     =  ChkTime2
      ChkTime2 =  ChkTime2 - TDay
      THour    =  ChkTime2 * 100
      If (Idebug .gt. 0) Write(Idebug,*) ' ChkTime2 before', ChkTime2,Tyear, Tmo,Tday, THour
      ChkTime2 =  ChkTime2 - 0.01 * THour
      If (Idebug .gt. 0) Write(Idebug,*) ' ChkTime2 after', ChkTime2,Tyear, Tmo,Tday, THour
      TMin     =  ChkTime2 * 10000
      ChkTime2 =  ChkTime2 - 0.01 * TMin
      TSec     =  0
! ARS XXXX May 21, 2002: check correctness date/time; IHour =0..23, IMinute = 0..59
      If (Tmin .ge. 60) then
          Tmin = 0
          Thour = Thour + 1
      Endif
      If (Thour .ge. 24) then
         Thour = 0
         Call NxtDay (Idebug, TYear, TMo, TDay)
      Endif
! End ARS
      if (idebug .gt. 0)  write(idebug,*) ' Tyear etc', Tyear, Tmo, TDay, THour, TMin, TSec
      Idate2 = 10000*TYear + 100*TMo  + TDay
      Itime2 = 10000*THour + 100*Tmin + TSec
      JulianDate2 = Julian (idate2,itime2)

! ARS xxxx: May 22, 2002: it may occur that table ends before EndTimeSimulation+Wind/Precipitation predicion horizon
!                         in that case JulianDate1 and 2 are equal, since they refer to the same (last) element of the table
      If (Idebug .gt. 0) write(Idebug,*) ' JulianDates', JulianDate1, JulianDate2, JulianDate3
      If (abs(JulianDate2-JulianDate1) .le. 1E-9) then
         Ratio = 1.0
      Else
         Ratio = 1.0 - (JulianDate3-JulianDate1) / (JulianDate2-JulianDate1)
      Endif
      if (JulianDate3 .lt. 0 .or. Ratio .gt. 1 .or. Ratio .lt. 0) then
          if (idebug .gt. 0)  write(idebug,*) ' Error: Julian Date negative or Ratio invalid; put ratio=1'
          Ratio = 1.0
      endif

      RETURN
      END
