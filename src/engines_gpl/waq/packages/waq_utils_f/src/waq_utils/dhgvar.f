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
      module m_dhgvar
      use m_monsys


      implicit none

      contains


      SUBROUTINE DHGVAR ( IAR_NR, INDX  , IVAR  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            : Jan van Beek
!
!     FUNCTION            : Initialisation of Variables structure
!
!     SUBROUTINES CALLED  :
!
!     FILES               :
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     IAR_NR  INTEGER       1     INPUT   Array number
!     INDX    INTEGER       1     INPUT   Index number variable in array
!     IVAR    INTEGER       1     OUTPUT  Variable number, else -1
!
!     Declaration of arguments

      use m_sysn          ! System characteristics
!
      INTEGER             IAR_NR, INDX  , IVAR

      INTEGER IIVOL  
      INTEGER IIAREA 
      INTEGER IIFLOW 
      INTEGER IILENG 
      INTEGER IIDISP 
      INTEGER IICONC 
      INTEGER IIMASS 
      INTEGER IIDERV 
      INTEGER IIBOUN 
      INTEGER IIBSET 
      INTEGER IIBSAV 
      INTEGER IIWSTE 
      INTEGER IICONS 
      INTEGER IIPARM 
      INTEGER IIFUNC 
      INTEGER IISFUN 
      INTEGER IIDNEW 
      INTEGER IIDIFF 
      INTEGER IIVNEW 
      INTEGER IIVELO 
      INTEGER IIHARM 
      INTEGER IIFARR 
      INTEGER IIMAS2 
      INTEGER IITIMR 
      INTEGER IIVOL2 
      INTEGER IITRAC 
      INTEGER IIGWRK 
      INTEGER IIGHES 
      INTEGER IIGSOL 
      INTEGER IIGDIA 
      INTEGER IIGTRI 
      INTEGER IISMAS 
      INTEGER IIPLOC 
      INTEGER IIDEFA 
      INTEGER IIFLUX 
      INTEGER IISTOC 
      INTEGER IIFLXD 
      INTEGER IIFLXI 
      INTEGER IIRIOB 
      INTEGER IIDSPX 
      INTEGER IIVELX 
      INTEGER IILOCX 
      INTEGER IIDSTO 
      INTEGER IIVSTO 
      INTEGER IIDMPQ 
      INTEGER IIDMPS 
      INTEGER IITRRA 
      INTEGER IINRSP 
      INTEGER IIVOLL 
      INTEGER IIVOL3 
      INTEGER IIR1   
      INTEGER IIQXK  
      INTEGER IIQYK  
      INTEGER IIQZK  
      INTEGER IIDIFX 
      INTEGER IIDIFY 
      INTEGER IIDIFZ 
      INTEGER IIVOLA 
      INTEGER IIVOLB 
      INTEGER IIGUV  
      INTEGER IIGVU  
      INTEGER IIGZZ  
      INTEGER IIAAK  
      INTEGER IIBBK  
      INTEGER IICCK  
      INTEGER IIBD3X 
      INTEGER IIBDDX 
      INTEGER IIBDX  
      INTEGER IIBU3X 
      INTEGER IIBUUX 
      INTEGER IIBUX  
      INTEGER IIWRK1 
      INTEGER IIWRK2 
      INTEGER IIAAKL 
      INTEGER IIBBKL 
      INTEGER IICCKL 
      INTEGER IIDDKL 
      
      INTEGER IVVOL
      INTEGER IVARE
      INTEGER IVFLO
      INTEGER IVLEN
      INTEGER IVCNS
      INTEGER IVPAR
      INTEGER IVFUN
      INTEGER IVSFU
      INTEGER IVCNC
      INTEGER IVMAS
      INTEGER IVDER
      INTEGER IVDSP
      INTEGER IVVEL
      INTEGER IVDEF
      INTEGER IVLOC
      INTEGER IVDSX
      INTEGER IVVLX
      INTEGER IVLCX
      INTEGER IVFLX
      INTEGER LUNREP
!
!     Just take the used array's in the right order
!
      IIVOL  =  1
      IIAREA =  2
      IIFLOW =  3
      IILENG =  4
      IIDISP =  5
      IICONC =  6
      IIMASS =  7
      IIDERV =  8
      IIBOUN =  9
      IIBSET = 10
      IIBSAV = 11
      IIWSTE = 12
      IICONS = 13
      IIPARM = 14
      IIFUNC = 15
      IISFUN = 16
      IIDNEW = 17
      IIDIFF = 18
      IIVNEW = 19
      IIVELO = 20
      IIHARM = 21
      IIFARR = 22
      IIMAS2 = 23
      IITIMR = 24
      IIVOL2 = 25
      IITRAC = 26
      IIGWRK = 27
      IIGHES = 28
      IIGSOL = 29
      IIGDIA = 30
      IIGTRI = 31
      IISMAS = 32
      IIPLOC = 33
      IIDEFA = 34
      IIFLUX = 35
      IISTOC = 36
      IIFLXD = 37
      IIFLXI = 38
      IIRIOB = 39
      IIDSPX = 40
      IIVELX = 41
      IILOCX = 42
      IIDSTO = 43
      IIVSTO = 44
      IIDMPQ = 45
      IIDMPS = 46
      IITRRA = 47
      IINRSP = 48
      IIVOLL = 49
      IIVOL3 = 50
      IIR1   = 51
      IIQXK  = 52
      IIQYK  = 53
      IIQZK  = 54
      IIDIFX = 55
      IIDIFY = 56
      IIDIFZ = 57
      IIVOLA = 58
      IIVOLB = 59
      IIGUV  = 60
      IIGVU  = 61
      IIGZZ  = 62
      IIAAK  = 63
      IIBBK  = 64
      IICCK  = 65
      IIBD3X = 66
      IIBDDX = 67
      IIBDX  = 68
      IIBU3X = 69
      IIBUUX = 70
      IIBUX  = 71
      IIWRK1 = 72
      IIWRK2 = 73
      IIAAKL = 74
      IIBBKL = 75
      IICCKL = 76
      IIDDKL = 77
!
      IVVOL = 1
      IVARE = IVVOL + 1
      IVFLO = IVARE + 1
      IVLEN = IVFLO + 1
      IVCNS = IVLEN + 2
      IVPAR = IVCNS + NOCONS
      IVFUN = IVPAR + NOPA
      IVSFU = IVFUN + NOFUN
      IVCNC = IVSFU + NOSFUN
      IVMAS = IVCNC + NOTOT
      IVDER = IVMAS + NOTOT
      IVDSP = IVDER + NOTOT
      IVVEL = IVDSP + NODISP
      IVDEF = IVVEL + NOVELO
      IVLOC = IVDEF + NODEF
      IVDSX = IVLOC + NOLOC
      IVVLX = IVDSX + NDSPX
      IVLCX = IVVLX + NVELX
      IVFLX = IVLCX + NLOCX
!
      IVAR = -1
!
      IF ( IAR_NR .EQ. IIVOL ) THEN
         IF ( INDX .GT. 1 ) GOTO 900
         IVAR = IVVOL + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIAREA ) THEN
         IF ( INDX .GT. 1 ) GOTO 900
         IVAR = IVARE + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIFLOW ) THEN
         IF ( INDX .GT. 1 ) GOTO 900
         IVAR = IVFLO + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IILENG ) THEN
         IF ( INDX .GT. 2 ) GOTO 900
         IVAR = IVLEN + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IICONS ) THEN
         IF ( INDX .GT. NOCONS ) GOTO 900
         IVAR = IVCNS + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIPARM ) THEN
         IF ( INDX .GT. NOPA ) GOTO 900
         IVAR = IVPAR + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIFUNC ) THEN
         IF ( INDX .GT. NOFUN ) GOTO 900
         IVAR = IVFUN + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IISFUN ) THEN
         IF ( INDX .GT. NOSFUN ) GOTO 900
         IVAR = IVSFU + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IICONC ) THEN
         IF ( INDX .GT. NOTOT ) GOTO 900
         IVAR = IVCNC + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIMASS ) THEN
         IF ( INDX .GT. NOTOT ) GOTO 900
         IVAR = IVMAS + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIDERV ) THEN
         IF ( INDX .GT. NOTOT ) GOTO 900
         IVAR = IVDER + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIDISP ) THEN
         IF ( INDX .GT. NODISP ) GOTO 900
         IVAR = IVDSP + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIVELO ) THEN
         IF ( INDX .GT. NOVELO ) GOTO 900
         IVAR = IVVEL + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIDEFA ) THEN
         IF ( INDX .GT. NODEF ) GOTO 900
         IVAR = IVDEF + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIPLOC ) THEN
         IF ( INDX .GT. NOLOC ) GOTO 900
         IVAR = IVLOC + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIDSPX  ) THEN
         IF ( INDX .GT. NDSPX ) GOTO 900
         IVAR = IVDSX + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIVELX  ) THEN
         IF ( INDX .GT. NVELX ) GOTO 900
         IVAR = IVVLX + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IILOCX  ) THEN
         IF ( INDX .GT. NLOCX ) GOTO 900
         IVAR = IVLCX + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIFLUX  ) THEN
         IF ( INDX .GT. NFLUX ) GOTO 900
         IVAR = IVFLX + INDX - 1
      ENDIF
!
      IF ( IVAR .EQ. -1 ) GOTO 900
!
      RETURN
!
  900 CONTINUE
      CALL GETMLU(LUNREP)
      WRITE(LUNREP,2000) IAR_NR,INDX
      RETURN
 2000 FORMAT (' WARNING in DHGVAR, array or index out of range',I10,I10)
!
      END
      end module m_dhgvar
