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

      subroutine write_error_message_rtc (icode, iecode, namsub, string, iout1)

! *********************************************************************
! *** RTC module for use with SOBEK, Delft_3B
! *** LAST UPDATE : June  1997          BY : Geert Prinsen
! **********************************************************************
! *** Write error message
! **********************************************************************
! ***  ICODE  = code foutboodschap
! ***  IECODE = Fortran error code of knoop-id
! ***  NAMSUB = naam subroutine
! ***  STRING = string met verdere identificatie
! ***  IOUT1  = message file
! **********************************************************************


      Use ParameterModule
      Use FileModule
      Use LanguageModule_rtc
      Use OtherData
      Use SyncRtcFlow
      Use ReadLib_rtc

      INTEGER       IOUT1, IECODE, ICODE
      CHARACTER*(*) NAMSUB
      CHARACTER*(*) STRING
      CHARACTER(999) STR(10), Messg    !, TranslateString
!
! **********************************************************************
! *** Write error message
! **********************************************************************

!      IF (ICODE .NE. 936  .AND. ICODE .NE. 945 .and. ICode .ne. 993) Then
      IF (ICODE .NE. 936  .AND. ICode .ne. 993) Then
         WRITE (IFLRTNRtc,'(I5)') ICODE
         If (SOBEKMODE .and. IdControlModule .ge. 0) then
           Call CrashCt (IdControlModule, .true.)
           RTCSelfCrash = .true.
         Endif
!        Close Matlab if it is in use
#if (defined(USE_MATLAB))
         Call CloseMatlab(0)
#endif

!        If in D3DMode and communication is active send message to flow
!        to quit
         If (CoupledToDelft3D .and. D3DACTIVE) then
           call SyncRtcFlow_Send(-1)
           call SyncRtcFlow_Close
           D3DACTIVE = .false.
         Endif

      Endif

      IF (ICODE .EQ. 901) THEN
         STR(1) = ' Error found in sub '
         STR(2) = NAMSUB
         STR(3) = ' End of file with filenames; not all files specified'
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(3) = TranslateString (RTCLanguageHandle,STR(3))
         Messg = CONSTR (STR,3)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 901
      ELSEIF (ICODE .EQ. 902) THEN
         STR(1) = ' Error found in sub '
         STR(2) = NAMSUB
         STR(3) = ' Fortran error code ='
         STR(4) = INTCH4(IECODE)
         STR(5) = ' at reading '
         STR(6) = STRING
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(3) = TranslateString (RTCLanguageHandle,STR(3))
         STR(5) = TranslateString (RTCLanguageHandle,STR(5))
         Messg = CONSTR (STR,6)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 902
      ELSEIF (ICODE .EQ. 903) THEN
         STR(1) = ' Usage: RTC [Name of file with file names] [Name of return code file]'
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         Messg = CONSTR (STR,1)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 903
      ELSEIF (ICODE .EQ. 904) THEN
         STR(1) = ' Input of measures not consistent in file '
         STR(2) = STRING
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 904
      ELSEIF (ICODE .EQ. 905) THEN
         STR(1) = ' Unknown Sobek-CF location in Sobek-RR location file'
         STR(2) = STRING
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 905
      ELSEIF (ICODE .EQ. 911) THEN
         STR(1) = ' Unexpected end-of-file in file'
         STR(2) = STRING
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 911
      ELSEIF (ICODE .EQ. 912) THEN
         STR(1) = ' String '
         STR(2) = NAMSUB
         STR(3) = ' not found in file'
         STR(4) = STRING
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(3) = TranslateString (RTCLanguageHandle,STR(3))
         Messg = CONSTR (STR,4)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 912
      ELSEIF (ICODE .EQ. 913) THEN
         STR(1) = ' Dimension problem for parameter '
         STR(2) = STRING
         STR(3) = ' for '
         STR(4) = NAMSUB
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(3) = TranslateString (RTCLanguageHandle,STR(3))
         Messg = CONSTR (STR,4)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 913
      ELSEIF (ICODE .EQ. 9131) THEN
         STR(1) = STRING
         STR(2) = ' for '
         STR(3) = NAMSUB
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(3) = TranslateString (RTCLanguageHandle,STR(3))
         Messg = CONSTR (STR,3)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 913
      ELSEIF (ICODE .EQ. 914) THEN
         STR(1) = ' Unknown keyword for type of measure '
         STR(2) = STRING
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 914
      ELSEIF (ICODE .EQ. 915) THEN
         STR(1) = ' Time shift desired in file '
         STR(2) = STRING
         STR(3) = ' too large.'
         STR(4) = ' Time shift put at maximum value of '
         STR(5) = INTCH4(IECODE)
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(3) = TranslateString (RTCLanguageHandle,STR(3))
         STR(4) = TranslateString (RTCLanguageHandle,STR(4))
         Messg = CONSTR (STR,5)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!C       STOP 915
      ELSEIF (ICODE .EQ. 916) THEN
         STR(1) = ' Series nr desired from HIS file <=0 or too large in '
         STR(2) = STRING
         STR(3) = ' Maximum nr. of series is '
         STR(4) =  INTCH4(IECODE)
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(3) = TranslateString (RTCLanguageHandle,STR(3))
         Messg = CONSTR (STR,4)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 916
      ELSEIF (ICODE .EQ. 917) THEN
         STR(1) = ' Desired location for decision parameter in '
         STR(2) = STRING
         STR(3) = '  not present in HIS file; id= '
         STR(4) =  NAMSUB
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(3) = TranslateString (RTCLanguageHandle,STR(3))
         Messg = CONSTR (STR,4)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 917
      ELSEIF (ICODE .EQ. 918) THEN
         STR(1) = ' Decision parameter wants to use '
         STR(2) = STRING
         STR(3) = ' data'
         STR(4) =' but INI file says this data is not available/in use!'
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(3) = TranslateString (RTCLanguageHandle,STR(3))
         STR(4) = TranslateString (RTCLanguageHandle,STR(4))
         Messg = CONSTR (STR,4)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 918
      ELSEIF (ICODE .EQ. 919) THEN
         STR(1) = ' Desired decision parameter for measure in '
         STR(2) = STRING
         STR(3) = '  not present in beslisparameter file; id= '
         STR(4) =  NAMSUB
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(3) = TranslateString (RTCLanguageHandle,STR(3))
         Messg = CONSTR (STR,4)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 919
      ELSEIF (ICODE .EQ. 920) THEN
         STR(1) = " Desired check of value of decision parameter should be '<' or '=' or '>'"
         STR(2) = ' Now read from measure file: '
         STR(3) = STRING
         STR(4) = ' Record/Measure number '
         STR(5) = INTCH4(IECODE)
         STR(6) = ' Measure id '
         STR(7) = NAMSUB
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(2) = TranslateString (RTCLanguageHandle,STR(2))
         STR(4) = TranslateString (RTCLanguageHandle,STR(4))
         STR(6) = TranslateString (RTCLanguageHandle,STR(6))
         Messg = CONSTR (STR,7)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!       STOP 920
      ELSEIF (ICODE .EQ. 921) THEN
         STR(1) = ' This version of RTC should be used together with SobekCF/RR or with Delft3D.'
         STR(2) = ' Correct the settings in your RTC.DAT file.'
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(2) = TranslateString (RTCLanguageHandle,STR(2))
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 921
      ELSEIF (ICODE .EQ. 922) THEN
         STR(1) = ' Desired measure parameter for 3B-location in '
         STR(2) = STRING
         STR(3) = ' not present in 3B-measure file; id= '
         STR(4) =  NAMSUB
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(3) = TranslateString (RTCLanguageHandle,STR(3))
         Messg = CONSTR (STR,4)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 922
      ELSEIF (ICODE .EQ. 923) THEN
         STR(1) = ' Time shift desired in file '
         STR(2) = STRING
         STR(3) = ' not allowed.'
         STR(4) = ' Time shift put at minimum value of '
         STR(5) = INTCH4(IECODE)
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(3) = TranslateString (RTCLanguageHandle,STR(3))
         STR(4) = TranslateString (RTCLanguageHandle,STR(4))
         Messg = CONSTR (STR,5)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 923
      ELSEIF (ICODE .EQ. 924) THEN
         STR(1)= ' Desired decision parameter for decisionparameter in '
         STR(2)= STRING
         STR(3)= '  not present in beslisparameter file; id= '
         STR(4)=  NAMSUB
         STR(1)= TranslateString (RTCLanguageHandle,STR(1))
         STR(3)= TranslateString (RTCLanguageHandle,STR(3))
         Messg = CONSTR (STR,4)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 924
      ELSEIF (ICODE .EQ. 925) THEN
         STR(1)= ' Order of decision parameter file not correct for parameter '
         STR(2)= NAMSUB
         STR(1)= TranslateString (RTCLanguageHandle,STR(1))
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 925
      ELSEIF (ICODE .EQ. 926) THEN
        STR(1)=' Order of decision parameters in inputfile not correct.'
        STR(2)=' No PARA records allowed after PAR2 records.'
        STR(1)= TranslateString (RTCLanguageHandle,STR(1))
        STR(2)= TranslateString (RTCLanguageHandle,STR(2))
        Messg = CONSTR (STR,2)
        IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
        WRITE(*,'(A)') MESSG
!       STOP 926
      ELSEIF (ICODE .EQ. 927) THEN
        STR(1)=' Decision parameter id or location id should be unique.'
        STR(2)= NAMSUB
        STR(1)= TranslateString (RTCLanguageHandle,STR(1))
        Messg = CONSTR (STR,2)
        IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
        WRITE(*,'(A)') MESSG
!       STOP 927
      ELSEIF (ICODE .EQ. 928) THEN
         STR(1) = ' Search String '
         STR(2) = NAMSUB
         STR(3) = ' found twice in 1 record in file'
         STR(4) = STRING
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(3) = TranslateString (RTCLanguageHandle,STR(3))
         Messg = CONSTR (STR,4)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 928
      ELSEIF (ICODE .EQ. 929) THEN
        STR(1)=' Error allocating arrays in subroutine '
        STR(2)= NAMSUB
        STR(1)= TranslateString (RTCLanguageHandle,STR(1))
        Messg = CONSTR (STR,2)
        IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
        WRITE(*,'(A)') MESSG
!       STOP 929
      ELSEIF (ICODE .EQ. 930) THEN
        STR(1)=' Error in Ribasim reservoir computations subroutine '
        STR(2)= NAMSUB
        STR(1)= TranslateString (RTCLanguageHandle,STR(1))
        Messg = CONSTR (STR,2)
        IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
        WRITE(*,'(A)') MESSG
!       STOP 930
      ELSEIF (ICODE .EQ. 936) THEN
         STR(1) = ' Warning from computation of decision parameter '
         STR(2) = String
         STR(3) = ' Division by zero; Put divider to +- 10**-6'
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(3) = TranslateString (RTCLanguageHandle,STR(3))
         Messg = CONSTR (STR,3)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
!        WRITE(*,'(A)') MESSG
!C       STOP 936
      ELSEIF (ICODE .EQ. 937) THEN
         STR(1) = ' Error from computation of Matlab decision parameter'
         STR(2) = String
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 937
      ELSEIF (ICODE .EQ. 938) THEN
         STR(1) = ' Sobek-RTC-precipitation location not found in rainfall file'
         STR(2) = STRING
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 938
      ELSEIF (ICODE .EQ. 939) THEN
         STR(1) = ' Variable nr larger than precipitation time horizon'
         STR(2) = STRING
         STR(3) = ' Adjust precipitation time horizon in INI file'
         STR(4) =  INTCH4(IECODE)
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(3) = TranslateString (RTCLanguageHandle,STR(3))
         Messg = CONSTR (STR,4)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 939
      ELSEIF (ICODE .EQ. 940) THEN
         STR(1) = ' Variable nr larger than wind time horizon'
         STR(2) = STRING
         STR(3) = ' Adjust wind time horizon in INI file '
         STR(4) =  INTCH4(IECODE)
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(3) = TranslateString (RTCLanguageHandle,STR(3))
         Messg = CONSTR (STR,4)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 940
      ELSEIF (ICODE .EQ. 941) THEN
         STR(1) = ' Unexpected order in simultaneous running: RTC first'
         STR(2) = STRING
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 941
      ELSEIF (ICODE .EQ. 942) THEN
         STR(1) = ' Sobek-RTC-external location not found in wind file : '
         STR(2) = STRING
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 942
      ELSEIF (ICODE .EQ. 943) THEN
         STR(1) = ' This version of RTC cannot be used with SobekCF/RR and Delft3D together.'
         STR(2) = ' Correct the settings in your RTC.DAT file.'
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(2) = TranslateString (RTCLanguageHandle,STR(2))
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 943
      ELSEIF (ICODE .EQ. 944) THEN
         STR(1) = ' Wind cannot be used without External Locations.'
         STR(2) = ' Correct the settings in your RTC.DAT file.'
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(2) = TranslateString (RTCLanguageHandle,STR(2))
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 944
      ELSEIF (ICODE .EQ. 945) THEN
         STR(1) = ' Routine: '
         STR(2) = NAMSUB
         STR(3) = ': Shutdown from D3DFlow.'
         Messg = CONSTR (STR,3)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 945
      ELSEIF (ICODE .EQ. 946) THEN
         STR(1) = ' Routine: '
         STR(2) = NAMSUB
         STR(3) = ':'
         STR(4) = STRING
         Messg = CONSTR (STR,4)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 946
      ELSEIF (ICODE .EQ. 951) THEN
         STR(1) = ' Error in RTC decision parameter input; no correct sequence can be computed'
         STR(2) = ' There are circular references; resolve these e.g. by introducing a time delay'
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(2) = TranslateString (RTCLanguageHandle,STR(2))
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 951
      ELSEIF (ICODE .EQ. 952) THEN
         STR(1) = ' WQ parameter to be passed on to Matlab not specified'
         STR(2) = INTCH4(IECODE)
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 952
      ELSEIF (ICODE .EQ. 953) THEN
         STR(1) = ' Output format to Sobek, RR and WQ set by DelftIO (HIS).'
         STR(2) = ' Options to use ASCII file not supported anymore.'
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(2) = TranslateString (RTCLanguageHandle,STR(2))
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 953
      ELSEIF (ICODE .EQ. 954) THEN
         STR(1) = ' Error getting data from Matlab'
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         Messg = CONSTR (STR,1)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 954
      ELSEIF (ICODE .EQ. 956) THEN
         STR(1) = ' Error opening shared library'
         STR(2) = TranslateString (RTCLanguageHandle,STRING)
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
         STOP 956
      ELSEIF (ICODE .EQ. 957) THEN
         STR(1) = ' Shared library defined, but no shared library function defined'
         STR(2) = TranslateString (RTCLanguageHandle,STRING)
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
         STOP 957
      ELSEIF (ICODE .EQ. 958) THEN
         STR(1) = TranslateString (RTCLanguageHandle,STRING)
         Messg = CONSTR (STR,1)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
         STOP 958
      ELSEIF (ICODE .EQ. 959) THEN
         STR(1) = ' Desired location-id or parameter-id='
         STR(2) = STRING (1:Len_Trim(String))
         STR(3) = '  not present in HIS file; error from subroutine'
         STR(4) = NAMSUB
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         STR(3) = TranslateString (RTCLanguageHandle,STR(3))
         Messg = CONSTR (STR,4)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
      ELSEIF (ICODE .EQ. 972) THEN
         STR(1) = ' Error in language file'
         STR(2) = STRING
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 972
      ELSEIF (ICODE .EQ. 973) THEN
         STR(1) = ' Error getting on-line data:'
         STR(2) = STRING
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 973
      ELSEIF (ICODE .EQ. 992) THEN
         STR(1)=' Sobek-RTC License problem:'
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         MESSG = CONSTR (STR, 1)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
         STR(1)=STRING
         MESSG = CONSTR (STR, 1)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP 992
      ELSEIF (ICODE .EQ. 993) THEN
         STR(1)= NamSub
         STR(1) = TranslateString (RTCLanguageHandle,STR(1))
         MESSG = CONSTR (STR, 1)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
         STR(1)= STRING
         MESSG = CONSTR (STR, 1)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
      ELSE
         STR(1) = NAMSUB
         STR(2) = STRING
         Messg = CONSTR (STR,2)
         IF (IOUT1 .gt. 0) WRITE(IOUT1,'(A)') MESSG
         WRITE(*,'(A)') MESSG
!        STOP
      ENDIF
!
!
      RETURN
      END
