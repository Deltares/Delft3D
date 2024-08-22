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

      Subroutine CmpWindPrediction (Ievent, Itmstp, idebug)

      Use ParameterModule
      Use LocationDataModule
      Use DecisionModule
      Use MeasureModule
      Use OtherData


      Integer Ievent, ItmStp, Idebug

      Integer Iloc, Ipar, istart, Iend, indx, indx1, j, IExt
      INTEGER SYEAR, SMO, SDAY, SHOUR, SMIN, SSEC
      DOUBLE PRECISION RSSEC

      Double Precision  Rhlp1, Rhlp2, Ratio
!
      If (IDEBUG .GT. 0) Write(IDEBUG,*) ' CmpWindPrediction'

! *********************************************************************
! *** Compute prediction wind
! *********************************************************************
!
          If (NWIND .GT. 0) Then
            If (USEW .AND. IMODEW .EQ. 0) Then
              If (IDEBUG .GT. 0) Write(IDEBUG,*) ' Upd_wind timestep',ITMSTP,NWIND,NTIMHW
! Loop over External locations (wind) and related wind station
              Do IExt=1,NEXTD
               ILoc = WindLoc2Stat(IExt)
               If (IDEBUG .GT. 0) Write(Idebug,*) ' Ext and stat',IExt,ILoc

! wind direction
               If (ConsWD(ILoc)) Then
                 Do IPAR=1,NTIMHW
                    RESEXT(IExt,IPAR) = VALCWD(ILoc)
                 EndDo
                 If (IDEBUG .GT. 0) Write(IDEBUG,*) ' Constant wind direction', &
                                   VALCWD(ILoc), (RESEXT(iExt,IPAR),IPAR=1,NTIMHW)
               Else
                 ISTART= SearchFromIndex(Iloc,1)
                 IEnd  = SearchToIndex(Iloc,1)
                 SYEAR = IfYEAR
                 SMO   = IfMO
                 SDAY  = IfDAY
                 SHOUR = IfHOUR
                 SMIN  = IfMIN
                 SSEC  = IfSEC
                 RSSEC = SSEC
                 Do IPAR=1,NTIMHW
                  If (IPAR .GT. 1) Then
                     Call NXTSTP (IDEBUG, SYEAR,SMO,SDAY,SHOUR, SMIN, SSEC, RSSEC, IDHR, IDMIN, RDSEC)
                  EndIf
                  Call FNDDAT (WNDDIR, NWIND, NTIMW, ILOC, IEnd, ISTART, &
                             IDEBUG,SYEAR, SMO, SDAY, SHOUR, SMIN, INDX)
                  if (Ipar .eq. 1) SearchFromIndex(Iloc,1) = INDX
                  If (.not. WindInterpDir(iloc)) then
!                    block interpolation = no interpolation
                     RESEXT(IExt,IPAR) = WNDDIR(ILOC,INDX,2)
                  else
!                    lineair interpolation
                     Rhlp1 = WNDDIR(ILOC,INDX,2)
                     Indx1 = min (SearchToIndex(Iloc,1),Indx+1)
                     Rhlp2 = WNDDIR(ILOC,INDX1,2)
                     Call SetRatio (Ratio, ILoc, Indx, Indx1, 1, &
                                    IDEBUG,SYEAR, SMO, SDAY, SHOUR, SMIN, SSec)
                     RESEXT(IExt,IPAR) = Ratio * WNDDIR(ILOC,INDX,2) + (1.-Ratio) * WNDDIR(ILOC,INDX1,2)
                  Endif
                  If (IDEBUG .GT. 0) Write(IDEBUG,*) ' Wind direction',  &
                                      (WNDDIR(ILOC,Indx,j),J=1,2), 'ResExt', RESEXT(IExt,IPAR)
                 EndDo
                 If (IDEBUG .GT. 0) Write(IDEBUG,*) ' Wind direction', (WNDDIR(ILOC,Indx,j),J=1,2), &
                                                    ' ResExt', (RESEXT(IExt,IPAR),IPAR=1,NTIMHW)
               EndIf
! wind velocity
               If (ConsWV(Iloc)) Then
                 Do IPAR=1,NTIMHW
                    RESEXT(IExt,NTIMHW+IPAR) = VALCWV(ILoc)
                 EndDo
                 If (IDEBUG .GT. 0) Write(IDEBUG,*) ' Constand Wind velocity',  &
                              VALCWV(Iloc), (RESEXT(IExt,NTIMHW+IPAR),IPAR=1,NTIMHW)
               Else
                 ISTART= SearchFromIndex(Iloc,2)
                 IEnd  = SearchToIndex(Iloc,2)
                 SYEAR = IfYEAR
                 SMO   = IfMO
                 SDAY  = IfDAY
                 SHOUR = IfHOUR
                 SMIN  = IfMIN
                 SSEC  = IfSEC
                 RSSEC = SSEC
                 Do IPAR=1,NTIMHW
                  If (IPAR .GT. 1) Then
                   Call NXTSTP (IDEBUG, SYEAR,SMO,SDAY,SHOUR, SMIN, SSEC, RSSEC, IDHR, IDMIN, RDSEC)
                  EndIf
                  Call FNDDAT (WNDVEL, NWIND, NTIMW, ILOC, IEnd, ISTART, &
                           IDEBUG, SYEAR, SMO, SDAY, SHOUR, SMIN, INDX)
                  if (Ipar .eq. 1) SearchFromIndex(Iloc,2) = INDX
                  If (.not. WindInterpVel(iloc)) then
!                    block interpolation = no interpolation
                     RESEXT(IExt,IPAR+NTIMHW) = WNDVEL(ILOC,INDX,2)
                  else
!                    lineair interpolation
                     Rhlp1 = WNDVEL(ILOC,INDX,2)
                     Indx1 = min (SearchToIndex(Iloc,2),Indx+1)
                     Rhlp2 = WNDVEL(ILOC,INDX1,2)
                     Call SetRatio (Ratio, ILoc, Indx, Indx1, 2, &
                                    IDEBUG,SYEAR, SMO, SDAY, SHOUR, SMIN, SSec)
                     RESEXT(IExt,IPAR+NTIMHW) = Ratio * WNDVEL(ILOC,INDX,2) + (1.-Ratio) * WNDVEL(ILOC,INDX1,2)
                  Endif
                  If (IDEBUG .GT. 0)  Write(IDEBUG,*) ' Wind velocity', &
                                      (WNDVEL(ILOC,Indx,j),J=1,2), 'ResExt', RESEXT(IExt,NTIMHW+IPAR)
                 EndDo
                 If (IDEBUG .GT. 0) Write(IDEBUG,*) ' Wind velocity', (WNDVEL(ILOC,Indx,j),J=1,2),  &
                                                    'ResExt', (RESEXT(IExt,NTIMHW+IPAR),IPAR=1,NTIMHW)
               EndIf
              EndDo
            EndIf
          EndIf

      Return
      END
