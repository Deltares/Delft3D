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

       Subroutine ScanFile (In, FileName, Iout1, IOpt, KeyWord, NrRecords, Minimaal1)

! *********************************************************************
! *** Sobek_RTC
! *** LAST UPDATE : March 2000          BY : Geert Prinsen
! **********************************************************************
! *** Precondition: file FileName exists
! *** Scan number of records with keyword KEYWORD in file FILENAME on unit IN
! *** Output: NrRecords (altijd >= 1)
! **********************************************************************

      implicit none
      
      Integer       In, IOut1, IOpt
      Character(len=*) FileName
      Character(len=4) KeyWord, String
      Integer       NrRecords
      Logical       Eof, Minimaal1


      Eof = .false.
      NrRecords = 0

      Call OpenFl(In, FileName, Iout1, IOpt)
      Do While (.not. eof)
         Read (In,'(A4)',Err=999,End=999) String
         If (String(1:4) .eq. KeyWord(1:4)) NrRecords = NrRecords + 1
      Enddo
 999  Eof = .true.

      Close(IN)

      If (Minimaal1) NrRecords = max (1, NrRecords)

      RETURN
      END Subroutine ScanFile
