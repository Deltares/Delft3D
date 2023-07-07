!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            :
!
!     V0.02  230395  Jos van Gils  Modify for hybrid coupling
!     V0.01  040894  Jos van Gils  First version
!
!     FUNCTION            : Utility to manipulate PDF file for
!                           Delwaq 4.0, prior to use of Charon
!
!     SUBROUTINES CALLED  :
!
!     FILES               : -
!
!     COMMON BLOCKS       : -
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     LUIC    I        4                  LU INPUT FILE CHARON
!     LUOC    I        4                  LU OUTPUT FILE CHARON
!     LUREP   I        4                  LU REPORT FILE
!     LUPDF   I        4                  LU PDF FILE
!
!
!     Declarations
!
      use m_charon

      INTEGER         LUPDF , MMAX  , NAIJ2 , NMAX  , LUREP , NNOTRA,
     J                LUIC  , LUOC  , I     , NALTOX,
     J                NTOX  , NALADS, NALSPE, NALSP2, N1MAX , N2MAX
      PARAMETER      (MMAX = 1000, N1MAX = 1000, NMAX = 1000,
     J                N2MAX = 1000)
      PARAMETER      (NALTOX = 7, NALADS = 6, NALSPE = 4, NALSP2 = 2)
      CHARACTER*12    RUNNAM
      CHARACTER*6     VERSIO, NAMTOX(MMAX)
      CHARACTER*10    ALIADS(2,NALADS),ALISPE(2,NALSPE),
     J                ALISP2(2,NALSP2),ALITOX(2,NALTOX)
      INTEGER         ICTOX(MMAX)
      INTEGER         ITTTOB(N1MAX), IBTTOB(N1MAX),
     J                ICCTOT(N2MAX), ITCTOT(N2MAX), IOUTP(NMAX)
      REAL            ABTTOB(N1MAX), RMTTOB(N1MAX),
     J                ABCTOT(N2MAX), RMCTOT(N2MAX)



!     Definition of programme version

      DATA VERSIO /'V0.02 '/

!     Definition of unitnrs files

      DATA LUPDF,LUIC,LUOC,LUREP / 13,11,12,14 /

!     Aliases toxical substances:
!          if first name occurs in CHARON component vector
!          it will be remembered in order to write dissolved
!          concentrations and quality of solid phase in DELWAQ,
!          second name is DW4 equivalent,
!          also used to manipulate species-array

      DATA ALITOX /'CD++      ','CD        ','CU++      ','CU        ',
     J             'ZN++      ','ZN        ','NI++      ','NI        ',
     J             'CR+++     ','CR        ','HG        ','HG        ',
     J             'PB++      ','PB        '/

!     Aliases for adsorbents:
!          if first name occurs in TRANSPORTED VECTOR
!          it will be replaced by the second one for DELWAQ
!          in order to find masses in TRANSPORTED VECTOR

      DATA ALIADS /'CEC   _par','IM1       ','CEC   _tot','IM1       ',
     J             'FEADS _par','AdsFer    ','FEADS _par','AdsFer    ',
     J             'HHUM  _dis','DOC       ','HHUM  _dis','DOC       '/

!     Aliases for species
!          if first name occurs in CHARON species vector
!          it will be replaced by the second one for DELWAQ
!          in order to define substances that influence DW4 processes
!          (species molar mass in CHARON input will be applied!!)

      DATA ALISPE/ 'O2        ','OXY       ','CL-       ','CL        ',
     J             'NO3-      ','NO3       ','PARTP     ','AAP       '/

!     Aliases for species NH4 and PO4
!          to be used after writing the process definition files
!          species NH4 and PO4 are explicitly computed by process CHARON
!          but NH4 and PO4 stoichiometry terms should be recognized!!

      DATA ALISP2/ 'NH4+      ','NH4       ','PO4---    ','PO4       '/

!     List program name and version number

      WRITE (*,1000) VERSIO

!     Request filename of CHARON files (extensions prescribed)

      RUNNAM(1:8) = 'CHARON  '

!     Read input of CHARON (also opening of files)

      CALL CHINPU (RUNNAM, LUIC  , LUOC  )

!     Check number of components for local arrays

      IF ( M .GT. MMAX ) GOTO 901
      IF ( N .GT. NMAX ) GOTO 901

!     Find last index of first phase in arrays with dimension NAIJ
!     (arrays with all non-zero entries in stoichiometric matrix)

      CALL CHPHAS ( NAIJ2 )

!     Define mapping of Charon-Delwaq systems,
!     administration arrays used in writing PDF for CHARON
!     and in coupling CHARON and DELWAQ (see module for definition)

      CALL  CHMAPD ( NAIJ2 , MMAX  , NMAX  , N1MAX , N2MAX ,
     J               NTTOB , ITTTOB, IBTTOB, ABTTOB, RMTTOB,
     J               NCTOT , ICCTOT, ITCTOT, ABCTOT, RMCTOT,
     J               IOUTP , NNOTRA, NALTOX, NTOX  , ICTOX , ALITOX)

!     Replace names of adsorbents by DELWAQ-equivalents

      CALL CHALIA ( NTRANS, VARNAM , 10 , NALADS , ALIADS )

!     Put DELWAQ-aliasses in species names insofar as they influence
!     DW4 processes (exclude NH4 and PO4 because they are output CHARON)
!     Put DELWAQ-aliasses in TRANSPORTED VECTOR as well

      CALL  CHALIA ( N      , KN     , 6  , NALSPE , ALISPE )
      CALL  CHALIA ( NTRANS , VARNAM , 10 , NALSPE , ALISPE )

!     Write CHARON process

      DO 15 I =1,NTOX
   15 NAMTOX(I) = NR(ICTOX(I),1)
      CALL WRICHA ( LUPDF , NTRANS, VARNAM, NNOTRA, IOUTP ,
     J              N     , KN    , NTOX  , NAMTOX)

!     Write administration arrays, with same aliasses as WRICHA

      OPEN ( LUREP , FILE = 'PDFCHA.REP' )
      WRITE ( LUREP , '(/
     J ''Arrays describing composition of B-vector:''/
     J ''B[mol] = B[mol] + T[g] * A[mol/mol] / RM[g/mol]''/
     J ''T          B              A        RM''             )' )
      WRITE ( LUREP , '(A10,1X,A6,2F10.3)' )
     J    ( VARNAM(ITTTOB(I)),
     J      NR(IBTTOB(I),1),
     J      ABTTOB(I),
     J      RMTTOB(I),             I = 1,NTTOB   )

      WRITE ( LUREP , '(/
     J ''Arrays describing reconstr. of T-vector:''/
     J ''T[g] = T[g] + X[mol] * A[mol/mol] * RM[g/mol]''/
     J ''X      T                  A        RM''              )' )
      WRITE ( LUREP , '(A6,1X,A10,2F10.3)' )
     J    ( KN(ICCTOT(I)),
     J      VARNAM(ITCTOT(I)),
     J      ABCTOT(I),
     J      RMCTOT(I),              I = 1,NCTOT   )

!     Put DELWAQ-aliasses in species names and transported vector
!     insofar as they are used to process stoichiometry
!     Include NH4 and PO4, Yes or No?????

      CALL  CHALIA ( N      , KN     , 6  , NALTOX , ALITOX )
      CALL  CHALIA ( NTRANS , VARNAM , 10 , NALTOX , ALITOX )

!     Manipulate PDF file

      WRITE ( LUREP , '(/''Report from manipulation .pdf file:'')')
      CALL  CHSTOC ( LUREP , NAIJ2 )

      CLOSE ( LUREP )
      STOP 'Normal end'
  901 STOP 'PDFCHA: Dimension error'

 1000 FORMAT (/' PDFCHA'
     J /' Modification utility for DELWAQ 4.0 PDF-files'
     J /' To be used prior to chemical equilibrium module CHARON'
     J /' Deltares - WATER RESOURCES & ENVIRONMENT DIVISION'
     J /' Version ',A6)
 1010 FORMAT (/' Type input file ID (up to 8 characters)..'/)
 1020 FORMAT ('''',A10,10X,'''')
 1030 FORMAT ('d',A6,'Cha')
      END
