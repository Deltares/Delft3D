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

      SUBROUTINE REASAM(MSAM, JADOORLADEN)
      USE M_MISSING
      USE M_SAMPLES
      use m_alloc
      implicit none
      integer, intent(inout) :: msam        !< already opened file pointer to sample file
      integer, intent(in)    :: jadoorladen !< whether to append to global set (1) or start empty (0)
      integer :: ierr
      integer :: jflow
      integer :: jqn
      integer :: mcs
      integer :: ncs
      integer :: ndraw
      integer :: nkol
      integer :: nrow
      integer :: ns1
      integer :: nsm
      integer :: num
      integer :: K, K0
      double precision :: x, y, z
      double precision :: XX, YY, ZZ, ZZ2


      COMMON /PHAROSFLOW/  JFLOW
      COMMON /PHAROSLINE/  REC1
      COMMON /SAMPLESADM/  MCS,NCS,NS1
      COMMON /QNRGF/ JQN
      COMMON /DRAWTHIS/ ndraw(50)

      CHARACTER REC*132, TEX*10, REC1*132
      LOGICAL THISISANUMBER

      CALL SAVESAM()
      NSM = 0
      MXSAM = 0
      MYSAM = 0
      IPSTAT = IPSTAT_NOTOK
      nkol = 0
      CALL READYY('Counting nr. of Samples ',0d0)
   11 READ (MSAM,'()',END = 31)
         NSM = NSM + 1
      GOTO 11
   31 NSMAX = 1.2d0*(NSM + JADOORLADEN*NS)
      IF (NSMAX .GT. 100000) NDRAW(32) = 7
      IF (NSMAX .GT. 500000) NDRAW(32) = 3
      IF (ALLOCATED (XS) ) DEALLOCATE (XS,YS,ZS)
      ALLOCATE (XS(NSMAX),YS(NSMAX),ZS(NSMAX),STAT=IERR)
      CALL AERR ('XS(NSMAX),YS(NSMAX),ZS(NSMAX)',IERR,NSMAX)
      if ( allocated(ipsam) ) deallocate(ipsam)
      allocate(ipsam(NSMAX),stat=ierr)
      call aerr('ipsam(NSMAX)',ierr,NSMAX)
      CALL READYY(' ',-1d0)

      REWIND(MSAM)

      WRITE(TEX,'(I10)') NSM
      CALL READYY('Reading '//TRIM(TEX)//' Sample Points',0d0)
      IF (JADOORLADEN .EQ. 0) THEN
         CALL XMISAR(XS,NSMAX)
         CALL XMISAR(YS,NSMAX)
         CALL  MISAR(ZS,NSMAX)
         K = 0
      ELSE
         CALL RESTORESAM()
         K   = NS
         NS1 = NS
      ENDIF
      K0 = K

!    check of dit een PHAROS file is
      JFLOW = 1
   14 READ (MSAM,'(A)',END = 30) REC1
      if (rec1(1:1) == '*') goto 14

      IF ( .NOT. (THISISANUMBER(REC1)) ) THEN
         READ (MSAM,'(A)',END = 30) REC
         IF ( THISISANUMBER(REC) ) THEN
            READ (REC,*,ERR = 16) NROW,NKOL
            GOTO 15
   16       CONTINUE
            READ (MSAM,'(A)',END = 30) REC
            READ (REC,*,ERR = 15) NUM,X,Y,Z
            JFLOW = 3
         ENDIF
      ENDIF
   15 CONTINUE

      REWIND (MSAM)


      KMOD = max(1, NSM/100)
   10 CONTINUE
      READ (MSAM,'(A)',END = 30) REC
      IF (REC(1:1) .EQ. '*') GOTO 10
      IF ( .NOT. (THISISANUMBER(REC)) ) THEN
!        we nemen aan dat er net een blokcode is gelezen
!        en we lezen meteen de nrow ncol regel, maar checken die regel niet
         READ  (MSAM,'(A)',END = 30) REC
      ELSE

         IF (JFLOW .EQ. 3) THEN
            READ (REC,*,ERR = 40) NUM,XX,YY,ZZ
         ELSE IF (NKOL == 4) THEN
            READ (REC,*,ERR = 40) XX,YY, ZZ, ZZ2
            if (zz .ne. -999d0) then
               zz = sqrt(zz*zz + zz2*zz2)
            endif
         ELSE
            READ (REC,*,end = 40) XX,YY,ZZ
            READ (REC,*,ERR = 40) XX,YY,ZZ
         ENDIF

         IF (K  .LE. NSMAX-1 .AND. XX .NE. XYMIS .AND.   &
             ZZ .NE. dmiss .AND. ZZ .NE. 999.999d0 .and. &
             .not.(isnan(XX) .or. isnan(YY) .or. isnan(ZZ)) ) THEN
            K     = K + 1
            NS    = K
            XS(K) = XX
            YS(K) = YY
            ZS(K) = ZZ
         ENDIF
         IF (MOD(K-K0,KMOD) .EQ. 0) THEN
            CALL READYY(' ',MIN( 1d0,dble(K)/NSM) )
         ENDIF
      ENDIF
      GOTO 10

   40 CONTINUE
      WRITE(TEX,'(I10)') K
      CALL QNERROR('ERROR READING SAMPLES FILE LINE NR ',TEX,REC)

   30 CONTINUE
      IF (K .GT. NSMAX) THEN
         WRITE(TEX,'(I8)') NSMAX
         CALL QNERROR('ONLY',TEX,'SAMPLE POINTS CAN BE LOADED')
         WRITE(TEX,'(I8)') K
         CALL QNERROR('YOU TRIED TO LOAD',TEX,'SAMPLE POINTS')
      ENDIF
      CALL READYY(' ',-1d0)
      WRITE(TEX,'(I10)') NS
      CALL READYY('Sorting '//TRIM(TEX)//' Samples Points',0d0)
      IF (NS .GT. 1) THEN
         CALL TIDYSAMPLES(XS,YS,ZS,IPSAM,NS,MXSAM,MYSAM)
         call get_samples_boundingbox()
         IPSTAT = IPSTAT_OK
      END IF
      CALL READYY(' ',-1d0)
      call doclose(MSAM)
      RETURN
      END
