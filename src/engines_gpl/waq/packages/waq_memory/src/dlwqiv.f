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
      module m_dlwqiv
      use m_waq_type_definitions


      implicit none

      contains


      SUBROUTINE DLWQIV ( LUREP , NOCONS, NOPA  , NOFUN , NOSFUN,
     +                    NOSYS , NOTOT , NODISP, NOVELO, NODEF ,
     +                    NOLOC , NDSPX , NVELX , NLOCX , NFLUX ,
     +                    NOPRED, NOVAR , VARARR, VARIDX, VARTDA,
     +                    VARDAG, VARTAG, VARAGG, NOGRID, VGRSET)
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
!
!     Declaration of arguments
!

      use m_dhzeri
      use timers

      INTEGER(kind=int_32) ::LUREP , NOCONS, NOPA  , NOFUN , NOSFUN,
     +                    NOSYS , NOTOT , NODISP, NOVELO, NODEF ,
     +                    NOLOC , NDSPX , NVELX , NLOCX , NFLUX ,
     +                    NOPRED, NOVAR, NOGRID
      INTEGER(kind=int_32) ::VARARR(NOVAR) , VARIDX(NOVAR) ,
     +                    VARTDA(NOVAR) , VARDAG(NOVAR) ,
     +                    VARTAG(NOVAR) , VARAGG(NOVAR)
      INTEGER(kind=int_32) ::VGRSET(NOVAR,NOGRID)

!
!     Just take the used array's in the right order
!
      integer(kind=int_32) ::IIVOL  =  1
      integer(kind=int_32) ::IIAREA =  2
      integer(kind=int_32) ::IIFLOW =  3
      integer(kind=int_32) ::IILENG =  4
      integer(kind=int_32) ::IIDISP =  5
      integer(kind=int_32) ::IICONC =  6
      integer(kind=int_32) ::IIMASS =  7
      integer(kind=int_32) ::IIDERV =  8
      integer(kind=int_32) ::IIBOUN =  9
      integer(kind=int_32) ::IIBSET = 10
      integer(kind=int_32) ::IIBSAV = 11
      integer(kind=int_32) ::IIWSTE = 12
      integer(kind=int_32) ::IICONS = 13
      integer(kind=int_32) ::IIPARM = 14
      integer(kind=int_32) ::IIFUNC = 15
      integer(kind=int_32) ::IISFUN = 16
      integer(kind=int_32) ::IIDNEW = 17
      integer(kind=int_32) ::IIDIFF = 18
      integer(kind=int_32) ::IIVNEW = 19
      integer(kind=int_32) ::IIVELO = 20
      integer(kind=int_32) ::IIHARM = 21
      integer(kind=int_32) ::IIFARR = 22
      integer(kind=int_32) ::IIMAS2 = 23
      integer(kind=int_32) ::IITIMR = 24
      integer(kind=int_32) ::IIVOL2 = 25
      integer(kind=int_32) ::IITRAC = 26
      integer(kind=int_32) ::IIGWRK = 27
      integer(kind=int_32) ::IIGHES = 28
      integer(kind=int_32) ::IIGSOL = 29
      integer(kind=int_32) ::IIGDIA = 30
      integer(kind=int_32) ::IIGTRI = 31
      integer(kind=int_32) ::IISMAS = 32
      integer(kind=int_32) ::IIPLOC = 33
      integer(kind=int_32) ::IIDEFA = 34
      integer(kind=int_32) ::IIFLUX = 35
      integer(kind=int_32) ::IISTOC = 36
      integer(kind=int_32) ::IIFLXD = 37
      integer(kind=int_32) ::IIFLXI = 38
      integer(kind=int_32) ::IIRIOB = 39
      integer(kind=int_32) ::IIDSPX = 40
      integer(kind=int_32) ::IIVELX = 41
      integer(kind=int_32) ::IILOCX = 42
      integer(kind=int_32) ::IIDSTO = 43
      integer(kind=int_32) ::IIVSTO = 44
      integer(kind=int_32) ::IIDMPQ = 45
      integer(kind=int_32) ::IIDMPS = 46
      integer(kind=int_32) ::IITRRA = 47
      integer(kind=int_32) ::IINRSP = 48
      integer(kind=int_32) ::IIVOLL = 49
      integer(kind=int_32) ::IIVOL3 = 50
      integer(kind=int_32) ::IIR1   = 51
      integer(kind=int_32) ::IIQXK  = 52
      integer(kind=int_32) ::IIQYK  = 53
      integer(kind=int_32) ::IIQZK  = 54
      integer(kind=int_32) ::IIDIFX = 55
      integer(kind=int_32) ::IIDIFY = 56
      integer(kind=int_32) ::IIDIFZ = 57
      integer(kind=int_32) ::IIVOLA = 58
      integer(kind=int_32) ::IIVOLB = 59
      integer(kind=int_32) ::IIGUV  = 60
      integer(kind=int_32) ::IIGVU  = 61
      integer(kind=int_32) ::IIGZZ  = 62
      integer(kind=int_32) ::IIAAK  = 63
      integer(kind=int_32) ::IIBBK  = 64
      integer(kind=int_32) ::IICCK  = 65
      integer(kind=int_32) ::IIBD3X = 66
      integer(kind=int_32) ::IIBDDX = 67
      integer(kind=int_32) ::IIBDX  = 68
      integer(kind=int_32) ::IIBU3X = 69
      integer(kind=int_32) ::IIBUUX = 70
      integer(kind=int_32) ::IIBUX  = 71
      integer(kind=int_32) ::IIWRK1 = 72
      integer(kind=int_32) ::IIWRK2 = 73
      integer(kind=int_32) ::IIAAKL = 74
      integer(kind=int_32) ::IIBBKL = 75
      integer(kind=int_32) ::IICCKL = 76
      integer(kind=int_32) ::IIDDKL = 77
!
      integer(kind=int_32) ::IVVOL, IVARE, IVFLO, IVLEN, IVCNS, IVPAR, IVFUN, IVSFU,
     +           IVCNC, IVMAS, IVDER, IVDSP, IVVEL, IVDEF, IVLOC, IVDSX,
     +           IVVLX, IVLCX, IVFLX

      integer(kind=int_32) ::ivar, icons, ipa , ifun, isys, isfun, idsp, ivel, iloc,
     +           idsx, ivlx , ilcx, idef, iflx

      integer(kind=int_32) ::ithandl = 0
      if ( timon ) call timstrt ( "dlwqiv", ithandl )

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
!
!
      CALL DHZERI(VGRSET,NOVAR*NOGRID)
!
!     Volume
!
      IVAR = 1
!     VARARR(IVAR) = IIVOL
!     VARIDX(IVAR) = 1
!     VARTDA(IVAR) = 0
!     VARDAG(IVAR) = 0
!     VARTAG(IVAR) = 1
!     VARAGG(IVAR) = 0
      VGRSET(IVAR,1) = 1
!
!     Area
!
      IVAR = IVAR + 1
!     VARARR(IVAR) = IIAREA
!     VARIDX(IVAR) = 1
!     VARTDA(IVAR) = 0
!     VARDAG(IVAR) = 0
!     VARTAG(IVAR) = 0
!     VARAGG(IVAR) = 0
      VGRSET(IVAR,1) = 1
!
!     Flow
!
      IVAR = IVAR + 1
!     VARARR(IVAR) = IIFLOW
!     VARIDX(IVAR) = 1
!     VARTDA(IVAR) = 0
!     VARDAG(IVAR) = 0
!     VARTAG(IVAR) = 0
!     VARAGG(IVAR) = 0
      VGRSET(IVAR,1) = 1
!
!     Length , two length
!
      IVAR = IVAR + 1
!     VARARR(IVAR) = IILENG
!     VARIDX(IVAR) = 1
!     VARTDA(IVAR) = 0
!     VARDAG(IVAR) = 0
!     VARTAG(IVAR) = 0
!     VARAGG(IVAR) = 0
      VGRSET(IVAR,1) = 1
      IVAR = IVAR + 1
!     VARARR(IVAR) = IILENG
!     VARIDX(IVAR) = 2
!     VARTDA(IVAR) = 0
!     VARDAG(IVAR) = 0
!     VARTAG(IVAR) = 0
!     VARAGG(IVAR) = 0
      VGRSET(IVAR,1) = 1
!
!     Cons
!
      DO ICONS = 1 , NOCONS
         IVAR = IVAR + 1
!        VARARR(IVAR) = IICONS
!        VARIDX(IVAR) = ICONS
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Param
!
      DO IPA = 1 , NOPA
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIPARM
!        VARIDX(IVAR) = IPA
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 3
!        VARAGG(IVAR) = 1
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Func
!
      DO IFUN = 1 , NOFUN
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIFUNC
!        VARIDX(IVAR) = IFUN
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Seg Func
!
      DO ISFUN = 1 , NOSFUN
         IVAR = IVAR + 1
!        VARARR(IVAR) = IISFUN
!        VARIDX(IVAR) = ISFUN
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 3
!        VARAGG(IVAR) = 1
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Conc
!
      DO ISYS = 1 , NOSYS
         IVAR = IVAR + 1
!        VARARR(IVAR) = IICONC
!        VARIDX(IVAR) = ISYS
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 3
!        VARAGG(IVAR) = 1
         VGRSET(IVAR,1) = 1
      ENDDO
      DO ISYS = NOSYS + 1 , NOTOT
         IVAR = IVAR + 1
!        VARARR(IVAR) = IICONC
!        VARIDX(IVAR) = ISYS
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 1
!        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Mass
!
      DO ISYS = 1 , NOTOT
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIMASS
!        VARIDX(IVAR) = ISYS
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 1
!        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Deriv
!
      DO ISYS = 1 , NOTOT
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIDERV
!        VARIDX(IVAR) = ISYS
!        VARTDA(IVAR) = 2
!        VARDAG(IVAR) = IVMAS + ISYS - 1
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
      ENDDO
!
!     Disp
!
      DO IDSP = 1 , NODISP
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIDISP
!        VARIDX(IVAR) = IDSP
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Velo
!
      DO IVEL = 1 , NOVELO
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIVELO
!        VARIDX(IVAR) = IVEL
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Default
!
      DO IDEF = 1 , NODEF
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIDEFA
!        VARIDX(IVAR) = IDEF
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Local
!
      DO ILOC = 1 , NOLOC
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIPLOC
!        VARIDX(IVAR) = ILOC
!        VARTDA(IVAR) = 1
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 3
!        VARAGG(IVAR) = 1
      ENDDO
!
!     DSPX
!
      DO IDSX = 1 , NDSPX
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIDSPX
!        VARIDX(IVAR) = IDSX
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
      ENDDO
!
!     VELX
!
      DO IVLX = 1 , NVELX
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIVELX
!        VARIDX(IVAR) = IVLX
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
      ENDDO
!
!     LOCX
!
      DO ILCX = 1 , NLOCX
         IVAR = IVAR + 1
!        VARARR(IVAR) = IILOCX
!        VARIDX(IVAR) = ILCX
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
      ENDDO
!
!     FLUX
!
      DO IFLX = 1 , NFLUX
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIFLUX
!        VARIDX(IVAR) = IFLX
!        VARTDA(IVAR) = 2
!        VARDAG(IVAR) = IVVOL
!        VARTAG(IVAR) = 1
!        VARAGG(IVAR) = 0
      ENDDO
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      end module m_dlwqiv
