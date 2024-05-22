!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
    use m_waq_precision
    use m_logger

    implicit none

contains


    SUBROUTINE DHGVAR (IAR_NR, INDX, IVAR)
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
        INTEGER(kind = int_wp) :: IAR_NR, INDX, IVAR

        INTEGER(kind = int_wp) :: IIVOL
        INTEGER(kind = int_wp) :: IIAREA
        INTEGER(kind = int_wp) :: IIFLOW
        INTEGER(kind = int_wp) :: IILENG
        INTEGER(kind = int_wp) :: IIDISP
        INTEGER(kind = int_wp) :: IICONC
        INTEGER(kind = int_wp) :: IIMASS
        INTEGER(kind = int_wp) :: IIDERV
        INTEGER(kind = int_wp) :: IIBOUN
        INTEGER(kind = int_wp) :: IIBSET
        INTEGER(kind = int_wp) :: IIBSAV
        INTEGER(kind = int_wp) :: IIWSTE
        INTEGER(kind = int_wp) :: IICONS
        INTEGER(kind = int_wp) :: IIPARM
        INTEGER(kind = int_wp) :: IIFUNC
        INTEGER(kind = int_wp) :: IISFUN
        INTEGER(kind = int_wp) :: IIDNEW
        INTEGER(kind = int_wp) :: IIDIFF
        INTEGER(kind = int_wp) :: IIVNEW
        INTEGER(kind = int_wp) :: IIVELO
        INTEGER(kind = int_wp) :: IIHARM
        INTEGER(kind = int_wp) :: IIFARR
        INTEGER(kind = int_wp) :: IIMAS2
        INTEGER(kind = int_wp) :: IITIMR
        INTEGER(kind = int_wp) :: IIVOL2
        INTEGER(kind = int_wp) :: IITRAC
        INTEGER(kind = int_wp) :: IIGWRK
        INTEGER(kind = int_wp) :: IIGHES
        INTEGER(kind = int_wp) :: IIGSOL
        INTEGER(kind = int_wp) :: IIGDIA
        INTEGER(kind = int_wp) :: IIGTRI
        INTEGER(kind = int_wp) :: IISMAS
        INTEGER(kind = int_wp) :: IIPLOC
        INTEGER(kind = int_wp) :: IIDEFA
        INTEGER(kind = int_wp) :: IIFLUX
        INTEGER(kind = int_wp) :: IISTOC
        INTEGER(kind = int_wp) :: IIFLXD
        INTEGER(kind = int_wp) :: IIFLXI
        INTEGER(kind = int_wp) :: IIRIOB
        INTEGER(kind = int_wp) :: IIDSPX
        INTEGER(kind = int_wp) :: IIVELX
        INTEGER(kind = int_wp) :: IILOCX
        INTEGER(kind = int_wp) :: IIDSTO
        INTEGER(kind = int_wp) :: IIVSTO
        INTEGER(kind = int_wp) :: IIDMPQ
        INTEGER(kind = int_wp) :: IIDMPS
        INTEGER(kind = int_wp) :: IITRRA
        INTEGER(kind = int_wp) :: IINRSP
        INTEGER(kind = int_wp) :: IIVOLL
        INTEGER(kind = int_wp) :: IIVOL3
        INTEGER(kind = int_wp) :: IIR1
        INTEGER(kind = int_wp) :: IIQXK
        INTEGER(kind = int_wp) :: IIQYK
        INTEGER(kind = int_wp) :: IIQZK
        INTEGER(kind = int_wp) :: IIDIFX
        INTEGER(kind = int_wp) :: IIDIFY
        INTEGER(kind = int_wp) :: IIDIFZ
        INTEGER(kind = int_wp) :: IIVOLA
        INTEGER(kind = int_wp) :: IIVOLB
        INTEGER(kind = int_wp) :: IIGUV
        INTEGER(kind = int_wp) :: IIGVU
        INTEGER(kind = int_wp) :: IIGZZ
        INTEGER(kind = int_wp) :: IIAAK
        INTEGER(kind = int_wp) :: IIBBK
        INTEGER(kind = int_wp) :: IICCK
        INTEGER(kind = int_wp) :: IIBD3X
        INTEGER(kind = int_wp) :: IIBDDX
        INTEGER(kind = int_wp) :: IIBDX
        INTEGER(kind = int_wp) :: IIBU3X
        INTEGER(kind = int_wp) :: IIBUUX
        INTEGER(kind = int_wp) :: IIBUX
        INTEGER(kind = int_wp) :: IIWRK1
        INTEGER(kind = int_wp) :: IIWRK2
        INTEGER(kind = int_wp) :: IIAAKL
        INTEGER(kind = int_wp) :: IIBBKL
        INTEGER(kind = int_wp) :: IICCKL
        INTEGER(kind = int_wp) :: IIDDKL

        INTEGER(kind = int_wp) :: IVVOL
        INTEGER(kind = int_wp) :: IVARE
        INTEGER(kind = int_wp) :: IVFLO
        INTEGER(kind = int_wp) :: IVLEN
        INTEGER(kind = int_wp) :: IVCNS
        INTEGER(kind = int_wp) :: IVPAR
        INTEGER(kind = int_wp) :: IVFUN
        INTEGER(kind = int_wp) :: IVSFU
        INTEGER(kind = int_wp) :: IVCNC
        INTEGER(kind = int_wp) :: IVMAS
        INTEGER(kind = int_wp) :: IVDER
        INTEGER(kind = int_wp) :: IVDSP
        INTEGER(kind = int_wp) :: IVVEL
        INTEGER(kind = int_wp) :: IVDEF
        INTEGER(kind = int_wp) :: IVLOC
        INTEGER(kind = int_wp) :: IVDSX
        INTEGER(kind = int_wp) :: IVVLX
        INTEGER(kind = int_wp) :: IVLCX
        INTEGER(kind = int_wp) :: IVFLX
        INTEGER(kind = int_wp) :: LUNREP
        !
        !     Just take the used array's in the right order
        !
        IIVOL = 1
        IIAREA = 2
        IIFLOW = 3
        IILENG = 4
        IIDISP = 5
        IICONC = 6
        IIMASS = 7
        IIDERV = 8
        IIBOUN = 9
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
        IIR1 = 51
        IIQXK = 52
        IIQYK = 53
        IIQZK = 54
        IIDIFX = 55
        IIDIFY = 56
        IIDIFZ = 57
        IIVOLA = 58
        IIVOLB = 59
        IIGUV = 60
        IIGVU = 61
        IIGZZ = 62
        IIAAK = 63
        IIBBK = 64
        IICCK = 65
        IIBD3X = 66
        IIBDDX = 67
        IIBDX = 68
        IIBU3X = 69
        IIBUUX = 70
        IIBUX = 71
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
        IF (IAR_NR == IIVOL) THEN
            IF (INDX > 1) GOTO 900
            IVAR = IVVOL + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IIAREA) THEN
            IF (INDX > 1) GOTO 900
            IVAR = IVARE + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IIFLOW) THEN
            IF (INDX > 1) GOTO 900
            IVAR = IVFLO + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IILENG) THEN
            IF (INDX > 2) GOTO 900
            IVAR = IVLEN + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IICONS) THEN
            IF (INDX > NOCONS) GOTO 900
            IVAR = IVCNS + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IIPARM) THEN
            IF (INDX > NOPA) GOTO 900
            IVAR = IVPAR + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IIFUNC) THEN
            IF (INDX > NOFUN) GOTO 900
            IVAR = IVFUN + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IISFUN) THEN
            IF (INDX > NOSFUN) GOTO 900
            IVAR = IVSFU + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IICONC) THEN
            IF (INDX > NOTOT) GOTO 900
            IVAR = IVCNC + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IIMASS) THEN
            IF (INDX > NOTOT) GOTO 900
            IVAR = IVMAS + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IIDERV) THEN
            IF (INDX > NOTOT) GOTO 900
            IVAR = IVDER + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IIDISP) THEN
            IF (INDX > NODISP) GOTO 900
            IVAR = IVDSP + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IIVELO) THEN
            IF (INDX > NOVELO) GOTO 900
            IVAR = IVVEL + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IIDEFA) THEN
            IF (INDX > NODEF) GOTO 900
            IVAR = IVDEF + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IIPLOC) THEN
            IF (INDX > NOLOC) GOTO 900
            IVAR = IVLOC + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IIDSPX) THEN
            IF (INDX > NDSPX) GOTO 900
            IVAR = IVDSX + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IIVELX) THEN
            IF (INDX > NVELX) GOTO 900
            IVAR = IVVLX + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IILOCX) THEN
            IF (INDX > NLOCX) GOTO 900
            IVAR = IVLCX + INDX - 1
        ENDIF
        !
        IF (IAR_NR == IIFLUX) THEN
            IF (INDX > NFLUX) GOTO 900
            IVAR = IVFLX + INDX - 1
        ENDIF
        !
        IF (IVAR == -1) GOTO 900
        !
        RETURN
        !
        900 CONTINUE
        CALL get_log_unit_number(LUNREP)
        WRITE(LUNREP, 2000) IAR_NR, INDX
        RETURN
        2000 FORMAT (' WARNING in DHGVAR, array or index out of range', I10, I10)
        !
    END
end module m_dhgvar
