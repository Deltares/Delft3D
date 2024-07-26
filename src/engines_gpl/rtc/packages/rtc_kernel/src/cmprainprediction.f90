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

      Subroutine CmpRainPrediction (Ievent, Itmstp, Ibui_itmstp, Ibui_rem, Time_ratio, idebug)

      Use ParameterModule
      Use LocationDataModule
      Use DecisionModule
      Use MeasureModule
      Use OtherData


      Integer Ievent, ItmStp, IBui_itmstp, Ibui_rem, idebug, iteller, itim
      Double Precision Time_ratio

      Integer Iloc, IStat, Ipar, BuiTijdstap, IBui_Loc

! *********************************************************************
! *** Compute prediction precipitation
! *********************************************************************

          If (Idebug .gt. 0)  write(Idebug,*) ' dimensions array Buidat ', Nevnt, Nstat,NTim
          If (Idebug .gt. 0)  write(Idebug,*) ' dimensions ResPre       ', NPre , NParP
          If (Idebug .gt. 0)  write(Idebug,*) ' NPrecP                  ', NPrecP
          IF (NPRECP .GT. 0) THEN
            IF (USEP .AND. IMODEP .EQ. 0) THEN
              IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Upd_rain timestep',ITMSTP,NPRECP,NTIMHP
              DO ILOC=1,NPRECP
                IStat = Loc2Stat(Iloc)
                IF (IDEBUG .GT. 0) Then
                  WRITE(IDEBUG,*) ' Iloc Istat', ILOC, ISTAT
                  WRITE(IDEBUG,*) ' TimeRatio ', Time_Ratio
                  WRITE(IDEBUG,*) ' Ievent Itmstp ibui_itmstp', Ievent,Itmstp, ibui_itmstp
                Endif
! default situation: calculation timestep equal to rainfall timestep
! ARS 10827 March 2004: use IBui_itmstp instead of itmstp, since subset of bui can be calculated
                IF (Time_Ratio .eq. 1) THEN
                  IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' TimeRatio=1'
                  DO IPAR=1,NTIMHP
                    if (idebug .gt. 0) then
                      Write(IDEBUG,*) ' Loop CmpRainPrediction iPar',Ipar
                      Write(IDEBUG,*)  Itmstp, Ipar, Lasttm
                      Write(IDEBUG,*)  ' Dimensions',NPre, NParp, NEvnt, NStat,NTim
                      Write(IDEBUG,*)  ' Indices   ',Iloc, IPar, IEvent, IStat, Itmstp+Ipar
                    Endif
                    IF (IBui_Itmstp+IPAR .LE. NTIM) THEN
                      RESPRE(ILOC,IPAR)=BUIDAT(IEVENT,ISTAT,IBui_Itmstp-1+IPAR)
                    ELSE
                      RESPRE(ILOC,IPAR)=0.0
                    ENDIF
                  ENDDO
                  IF (IDEBUG .GT. 0) Then
                    Iteller = min(3,NTim-IBui_ItmStp,NTimHp)
                    Iteller = max(0,ITeller)
                    Iteller = min(ITeller,NTim-IBui_Itmstp-iTeller)
                    WRITE(IDEBUG,*) ' Buidata',ILOC,ISTAT,NTimHp,Iteller
                    if (Iteller .gt. 0) WRITE(IDEBUG,*) (BUIDAT(IEVENT,ISTAT,Ibui_iTMSTP-1+IPar),IPar=1,Iteller)
                    Iteller = min(3,NParP,NTim,NTimHp)
                    if (Iteller .gt. 0) WRITE(IDEBUG,*) ' ResPre', (RESPRE(ILOC,IPAR),IPAR=1,ITeller)
                  Endif
                ELSE
! May 1998:
!   different timestep sizes bui (NRSRAI) and calculation (ITMSIZ)
!   NTIMHP is number of calculation timesteps ahead
!   ITMSTP is current calculation timestep
!   IBUI_ITMSTP is current rainfall timestep
!   IBUI_rem is remaining number of calculation timesteps using same rainfall timestep

                  IF (IDEBUG .GT. 0)  WRITE(IDEBUG,*) ' TimeRatio <> 1'
                  BuiTijdstap = IBUI_Itmstp
                  IBUI_loc = IBUI_rem

                  DO IPAR=1,NTIMHP
                    IF (ITMSTP+IPAR .LE. LASTTM) THEN
                      IBUi_loc = IBUI_loc - 1
                      IF (Ibui_loc .LE. 0) THEN
                         BuiTijdstap= BuiTijdstap + 1
                         IBUI_Loc = Time_Ratio
                      ENDIF
                      IF (IDEBUG .GT. 0) THEN
                        WRITE(IDEBUG,*) ' BuiTijdstap  IBui_loc  ipar', BuiTijdstap, IBui_loc, ipar
                        WRITE(IDEBUG,*) ' Iloc IPar Ievent Istat '
                        WRITE(IDEBUG,*)  Iloc, IPar, Ievent, Istat
                      ENDIF
! Taiwan March 2004   RESPRE(ILOC,IPAR)=BUIDAT(IEVENT,ISTAT,BuiTijdstap)
                      itim = min (Buitijdstap-1+Ipar, ntim)
                      RESPRE(ILOC,IPAR)=BUIDAT(IEVENT,ISTAT,itim)
                      IF (IDEBUG .GT. 0) THEN
                        WRITE(IDEBUG,*) ' Respre ',RESPRE(ILOC, IPAR) !' Buidat ',BUIDAT(IEVENT,ISTAT,BuiTijdstap)
                      ENDIF
                    ELSE
                      RESPRE(ILOC,IPAR)=0.0
                    ENDIF
                  ENDDO
                  IF (IDEBUG .GT. 0) THEN
!                   WRITE(IDEBUG,*) ' ITMSTP IBUI_ITMSTP IBUI_rem'
!                   WRITE(IDEBUG,*) ITMSTP, IBUI_ITMSTP, IBUI_rem
!                   If (IBUI_ITMSTP .gt. 0)  WRITE(IDEBUG,*) BUIDAT(IEVENT,ISTAT,IBUI_ITMSTP)
!                   IF (ITMSTP .LT. NTIM) &
!                       WRITE(IDEBUG,*) 'Buidata ', ILOC, ISTAT,  BUIDAT(IEVENT,ISTAT,IBUI_ITMSTP+1)
!                   Iteller = min(3,NParP,NTim,NTimHp)
!                   WRITE(IDEBUG,*) 'Respre ', (RESPRE(ILOC,ipar),ipar=1,iteller)
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDIF



      Return
      END
