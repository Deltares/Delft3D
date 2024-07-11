!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

      SUBROUTINE UPDATEFILES(FILNAM,FILIST,NUMFIL,NUMDIR,IFDATE,IFSIZE,IXP,IYP,IH)
      use unstruc_display
      implicit none
      integer :: i, j, k, L, ic, ic0
      integer :: iday
      integer :: ih
      integer :: ihour
      integer :: imonth
      integer :: isecnd
      integer :: ixp
      integer :: iyear
      integer :: iyp
      integer :: maxfil
      integer :: minute
      integer :: n
      integer :: numdir
      integer :: numfil
      PARAMETER (MAXFIL = 2000)
      INTEGER IFDATE(MAXFIL), IFSIZE(MAXFIL)
      CHARACTER FILIST(MAXFIL)*86,FILNAM*(*)
      ! Work arrays for merging+sorting two file lists
      ! when multiple wildcard patterns are used in filnam.
      character filistt(maxfil)*86
      integer ifdatet(maxfil), ifsizet(maxfil)

      NUMFIL = MAXFIL
      NUMDIR = MAXFIL
      CALL INHIGHLIGHT('WHITE','BLUE')
      do I = 1,MAXFIL
         FILIST(I) = '                                          '
end do
      CALL IOUTMenuScroll(FILIST,80,IXP,IYP+10,' ',IH-7,0,1)

      CALL IOSDIRENTRYTYPE('D')
      CALL IOsDirInfo(' ','*',FILIST,NUMDIR,IFDATE,IFSIZE)
      IF (NUMDIR .EQ. MAXFIL) THEN
         NUMDIR = MAXFIL - 1
         CALL QNERROR('NOT ALL DIRECTORIES ARE LISTED', ' ', ' ')
      ENDIF

      IF (NOPSYS .EQ. 4) THEN
         do I = NUMDIR+1,2,-1
            FILIST(I) = FILIST(I-1)
            IFDATE(I) = IFDATE(I-1)
            IFSIZE(I) = IFSIZE(I-1)
         end do
         FILIST(1) = '..                                        '
         NUMDIR = NUMDIR + 1
      ENDIF

      IF (FILIST(1)(1:3) .EQ. '.  ') THEN
         NUMDIR = NUMDIR - 1
         do i = 1,NUMDIR
            FILIST(I) = FILIST(I+1)
            IFDATE(I) = IFDATE(I+1)
            IFSIZE(I) = IFSIZE(I+1)
      end do
      ENDIF

      NUMFIL = NUMDIR ! current nr of 'files'
      CALL IOSDIRENTRYTYPE('F')
      ic  = 0
      ic0 = 0
      do ! patterns...
          ic = index(filnam(ic0+1:), ',')
          N = NUMFIL + 1

          if (ic == 0) then
            ic = len(filnam)+1
          else
            ic = ic0+ic
          end if
          numfil = maxfil-numfil ! Max nr of files to read
          CALL IOsDirInfo(' ',FILNAM(ic0+1:ic-1),FILIST(N),NUMFIL,IFDATE(N),IFSIZE(N))
          ic0 = ic

          i = NUMDIR ! Start index(-1) of sorted files until now
          j = N-1    ! Start index(-1) of newly found files for next pattern
          L = 0      ! nr of elements in merged result filistt(:), etc.
          do
            if (i==N-1) then ! All 'old' files are already in merged result, just copy remaining 'new' files.
                do K=j+1,N+numfil-1
                    L = L+1
                    filistt(L) = filist(k)
                    ifdatet(L) = ifdate(k)
                    ifsizet(L) = ifsize(k)
                end do
                exit
            end if
            if (j==N+numfil-1) then ! All 'new' files are already in merged result, just copy remaining 'old' files.
                do K=i+1,N-1
                    L = L+1
                    filistt(L) = filist(k)
                    ifdatet(L) = ifdate(k)
                    ifsizet(L) = ifsize(k)
                end do
                exit
            end if

            ! Check which of the two next files (old and new) should come first
            if (lle(filist(i+1), filist(j+1))) then
                i = i+1 ! increase i and leave j
                k = i
            else
                j = j+1 ! increase j and leave i
                k = j
            end if
            L = L+1
            filistt(L) = filist(k)
            ifdatet(L) = ifdate(k)
            ifsizet(L) = ifsize(k)
          end do

          ! And now put the merged+sorted file list back into the actual file list.
          do k=1,L
            filist(NUMDIR+k) = filistt(k)
            ifdate(NUMDIR+k) = ifdatet(k)
            ifsize(NUMDIR+k) = ifsizet(k)
          end do
          NUMFIL = NUMFIL + N-1

          if (ic == len(filnam)+1) then
            exit ! No further patterns in filnam, proceed.
          end if
      end do

      IF (NUMFIL .EQ. MAXFIL) THEN
         CALL QNERROR('NOT ALL FILES ARE LISTED', ' ', ' ')
      ENDIF

      do I = 1,NUMFIL
         IF (I .LE. NUMDIR) THEN
            IF (NOPSYS .NE. 4) THEN
               CALL IUPPERCASE( FILIST(I)(1:44) )
            ENDIF
            IF (FILIST(I)(1:3) .EQ. '.. ') THEN
               WRITE(FILIST(I)(56:67),'(A12)') '      UP-DIR'
            ELSE
               WRITE(FILIST(I)(56:67),'(A12)') '     SUB-DIR'
            ENDIF
         ELSE
            IF (NOPSYS .NE. 4) THEN
               CALL ILOWERCASE( FILIST(I)(1:54) )
            ENDIF
            WRITE(FILIST(I)(56:67),'(I12)') IFSIZE(I)
         ENDIF

         CALL IOsFileDate(IFDATE(I),IYEAR,IMONTH,IDAY)
         WRITE(FILIST(I)(70:79),'(I2,A1,I2,A1,I4)') IDAY ,'-',IMONTH,'-',IYEAR
         IF (IMONTH .LE. 9) WRITE(FILIST(I)(73:73),'(A1)') '0'
         CALL IOsFileTime(IFDATE(I),IHOUR,MINUTE,ISECND)
         WRITE(FILIST(I)(82:86),'(I2,A1,I2)') IHOUR,':',MINUTE
         IF (MINUTE .LE. 9) WRITE(FILIST(I)(85:85),'(A1)') '0'
end do
      CALL ITEXTCOLOUR('WHITE','BLU')
      CALL IOUTMenuScroll(FILIST,NUMFIL,IXP,IYP+10,' ',IH-7,0,1)
      RETURN
      END
