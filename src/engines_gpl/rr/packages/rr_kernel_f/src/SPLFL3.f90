!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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

 ! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 19-08-97 4:36p   $
!
! current revision: $Revision:: 4               $


      SUBROUTINE SPLFL3 (IN, IVNT, CheckYear, STRING, YearNul)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.0.                 Date: March 1995
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Spool kas initialisatie of gebruiksfile to first date next event
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! ***  YearNul= logical, indicating whether year in file = 0 or not
! *********************************************************************

      USE CONF_FIL
      USE CONF_ARR
      use Messages
      use Network

!
      INTEGER        Ivnt, In, IeCode, year, month, day, iDefltl, iDefltOld
      Integer        CheckYear
      CHARACTER(len=*) STRING
      Integer        iDebug, Iout1, IExit, iCount, i
      Logical        YearNul

      iDefltl = Network_get_Default()
      iDebug = ConfFil_get_iDebug()
      iOut1 = ConfFil_get_iOut1()
!     YearNul = .false.
      ICount = 0
!
      IF (iDebug /= 0) WRITE (IDEBUG,1)
    1 FORMAT (' SPLFL3')

! *********************************************************************
! *** Read date/time record
! *********************************************************************

   90 READ(IN,*,END=30,ERR=150,IOSTAT=IECODE)  year, month, day
      if (idebug .ne. 0) write(idebug,*) ' Splfl3',year, month, day

! if (evporation) datafile contains average values: checking on year is not necessary
! whatever kind the rainfallfile is.
      if (year .eq. 0) then
        YearNul = .true.
        iDefltOld = iDefltl
        iDefltl = 1
      elseif (Checkyear .eq. -1 .and. year .ne. 0) then
! if CheckYear for Evaporation or KasInit file =-1 ==> always check on year
! whatever kind the rainfallfile is.
        iDefltOld = iDefltl
        iDefltl = 0
      end if

      IF (IDEFLTl .EQ. 0) THEN
!        Data uit reeks file; check jaar, maand, dag
         Call TestDate (EvStrt(1), Evstrt(2), Evstrt(3), Year, Month, Day, Iexit)
      Elseif (IDEFLTl .EQ. 1 .and. year .ne. 0)  THEN
!        data uit default file voor 1 jaar; check alleen maand, dag volgens start gebeurtenis
!        en check dat CheckYear = year
         Call TestDate (CheckYear, Evstrt(2), Evstrt(3), Year, Month, Day, Iexit)
      Elseif (IDEFLTl .EQ. 1 .and. year .eq. 0)  THEN
!        data uit default file voor 1 jaar; check alleen maand, dag volgens start gebeurtenis
!        en check dat CheckYear = year
         Call TestDate (0, Evstrt(2), Evstrt(3), Year, Month, Day, Iexit)
      Endif

      If (IExit .eq. 90) then
          GOTO 90
      Elseif (IExit .eq. 999) then
          BACKSPACE(IN)
          GOTO 999
      Else
         IF (IDEFLTl .EQ. 0) then
            call ErrMsgStandard (915, IVNT, ' Splfl3', STRING)
         Elseif (IDEFLTl .EQ. 1)  THEN
            if (Icount .eq. 0) then
              ICount = ICount + 1
              Rewind(IN)
              Call SplFil(In,String)
              goto 90
            else
              call ErrMsgStandard (915, IVNT, ' Splfl3', STRING)
            endif
         endif
      Endif
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 CONTINUE
      call ErrMsgStandard (902, IECODE, '  Splfl3', STRING)

! *********************************************************************
! *** end of file
! *********************************************************************

   30 CONTINUE
      Write(Iout1,'(A,A)') ' Could not find startdate in ', String
      Write(Iout1,'(A,I4,I3,I3)') ' Was trying to find date (Year/month/day): ',(EvStrt(i),i=1,3)
      call ErrMsgStandard (911, IECODE, '  Splfl3', STRING)
!
  999 CONTINUE

      if (year .eq. 0 .or. CheckYear .eq. -1) then
        iDefltl = iDefltOld
      end if
!
      RETURN
      END
