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

      Function CheckAndSetSeriesReference (Iout1, Idebug, IndPardim) result(RetVal)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RTC version 1.0.                 Date: March 2001
! *********************************************************************
! *** Last update: March 2001                     By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Read decision parameter file
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***    IndParDim = index ParDim array; 1=Sbk, 2=RR, 5=WQ
! *********************************************************************

      Use ParameterModule
      Use LocationDataModule
      Use DecisionModule
      Use OtherData
      Use Readlib

      implicit none
      
      Integer :: RetVal

      Integer Iout1, Idebug, IPara, IndParDim, I, I2
      Integer ilen1, ilen2

      RetVal = 0

! *********************************************************************
! Check validiteit HIS file series used in Decision parameters, with series id's
! *********************************************************************

         DO IPARA=1,NPARA

           If (IndPardim .eq. 1) then
! Sobek
            DO I=1,PARDIM(IPARA,IndParDim)
              IF (VARIDSBK(IPARA,I) .NE. '') Then
!               Search Series id in IdPara list and fill VarSbk Array
                 Do I2=1,NParS
                   ilen1 = len_trim(VarIdSbk(Ipara,i))
                   ilen2 = len_trim (IdPars(i2))
                   If (VarIdSbk(IPara,i)(1:ilen1) .eq. IdPars(i2)(1:ilen2)) VarSbk(IPara,i) = i2
                 Enddo
!                Check whether VarSbk has valid value
                 IF (VARSBK(IPARA,I) .GT. NPARS .or. VarSbk(Ipara,i) .le. 0) THEN
                    WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                    CALL ERRMSG (916, NPARS, 'Rdpara', ' Beslisparameterfile', IOUT1)
                    RetVal = 916
                    Return
                 ENDIF
              ENDIF
            ENDDO
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Varsbk ', (VARSBK(IPARA,I), I=1,PARDIM(IPARA,1))

           Elseif (IndPardim .eq. 2) then
! RR
            DO I=1,PARDIM(IPARA,IndParDim)
              IF (VARID3B(IPARA,I) .NE. '') Then
!               Search Series id in IdPara list and fill Var3B Array
                 Do I2=1,NPar3
                   ilen1 = len_trim(VarId3B(Ipara,i))
                   ilen2 = len_trim (IdPar3(i2))
                   If (VarId3B(IPara,i)(1:ilen1) .eq. IdPar3(i2)(1:ilen2)) Var3B(IPara,i) = i2
                 Enddo
!                Check whether Var3B has valid value
                 IF (VAR3B (IPARA,I) .GT. NPAR3 .or. Var3B(Ipara,i) .le. 0) THEN
                     WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                     CALL ERRMSG (916, NPAR3, 'Rdpara', ' Beslisparameterfile', IOUT1)
                     RetVal = 916
                     Return
                 ENDIF
              ENDIF
            ENDDO
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Var3b ', (VAR3B(IPARA,I), I=1,PARDIM(IPARA,2))

           Elseif (IndPardim .eq. 5) then
! WQ
            DO I=1,PARDIM(IPARA,IndParDim)
              IF (VARIDWQ(IPARA,I) .NE. '') Then
!               Search Series id in IdPara list and fill VarWQ Array
                 Do I2=1,NParQ
                   ilen1 = len_trim(VarIdWQ(Ipara,i))
                   ilen2 = len_trim (IdParQ(i2))
                   If (VarIdWQ(IPara,i)(1:ilen1) .eq. IdParQ(i2)(1:ilen2)) VarWQ(IPara,i) = i2
                 Enddo
!                Check whether VarWQ has valid value
                 IF (VARWQ (IPARA,I) .GT. NPARQ .or. VarWQ (Ipara,i) .le. 0) THEN
                     WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                     CALL ERRMSG (916, NPARQ, 'Rdpara', ' Beslisparameterfile', IOUT1)
                     RetVal = 916
                     Return
                 ENDIF
              ENDIF
            ENDDO
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' VarWQ ', (VARWQ(IPARA,I), I=1,PARDIM(IPARA,5))

! D3DFlow
           Elseif (IndPardim .eq. 9) then
            DO I=1,PARDIM(IPARA,IndParDim)
              IF (VARID3D(IPARA,I) .NE. '') Then
!               Search Series id in IdPara list and fill Var3D Array
                 Do I2=1,NPar3D
                   ilen1 = len_trim(VarId3D(Ipara,i))
                   ilen2 = len_trim (IdPar3D(i2))
                   If (VarId3D(IPara,i)(1:ilen1) .eq. IdPar3D(i2)(1:ilen2)) Var3D(IPara,i) = i2
                 Enddo
!                Check whether Var3D has valid value
                 IF (VAR3D (IPARA,I) .GT. NPAR3D .or. Var3D(Ipara,i) .le. 0) THEN
                     WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                     CALL ERRMSG (916, NPAR3, 'Rdpara', ' Beslisparameterfile', IOUT1)
                     RetVal = 916
                     Return
                 ENDIF
              ENDIF
            ENDDO
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Var3D ', (VAR3D(IPARA,I), I=1,PARDIM(IPARA,9))

           Endif

         ENDDO

! *********************************************************************
! *** end of function
! *********************************************************************
      RETURN
      END Function CheckAndSetSeriesReference
