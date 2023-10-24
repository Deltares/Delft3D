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

      INTEGER             LUREP , NOCONS, NOPA  , NOFUN , NOSFUN,
     +                    NOSYS , NOTOT , NODISP, NOVELO, NODEF ,
     +                    NOLOC , NDSPX , NVELX , NLOCX , NFLUX ,
     +                    NOPRED, NOVAR, NOGRID
      INTEGER             VARARR(NOVAR) , VARIDX(NOVAR) ,
     +                    VARTDA(NOVAR) , VARDAG(NOVAR) ,
     +                    VARTAG(NOVAR) , VARAGG(NOVAR)
      INTEGER             VGRSET(NOVAR,NOGRID)

!
!     Just take the used array's in the right order
!
      integer    :: IIVOL  =  1
      integer    :: IIAREA =  2
      integer    :: IIFLOW =  3
      integer    :: IILENG =  4
      integer    :: IIDISP =  5
      integer    :: IICONC =  6
      integer    :: IIMASS =  7
      integer    :: IIDERV =  8
      integer    :: IIBOUN =  9
      integer    :: IIBSET = 10
      integer    :: IIBSAV = 11
      integer    :: IIWSTE = 12
      integer    :: IICONS = 13
      integer    :: IIPARM = 14
      integer    :: IIFUNC = 15
      integer    :: IISFUN = 16
      integer    :: IIDNEW = 17
      integer    :: IIDIFF = 18
      integer    :: IIVNEW = 19
      integer    :: IIVELO = 20
      integer    :: IIHARM = 21
      integer    :: IIFARR = 22
      integer    :: IIMAS2 = 23
      integer    :: IITIMR = 24
      integer    :: IIVOL2 = 25
      integer    :: IITRAC = 26
      integer    :: IIGWRK = 27
      integer    :: IIGHES = 28
      integer    :: IIGSOL = 29
      integer    :: IIGDIA = 30
      integer    :: IIGTRI = 31
      integer    :: IISMAS = 32
      integer    :: IIPLOC = 33
      integer    :: IIDEFA = 34
      integer    :: IIFLUX = 35
      integer    :: IISTOC = 36
      integer    :: IIFLXD = 37
      integer    :: IIFLXI = 38
      integer    :: IIRIOB = 39
      integer    :: IIDSPX = 40
      integer    :: IIVELX = 41
      integer    :: IILOCX = 42
      integer    :: IIDSTO = 43
      integer    :: IIVSTO = 44
      integer    :: IIDMPQ = 45
      integer    :: IIDMPS = 46
      integer    :: IITRRA = 47
      integer    :: IINRSP = 48
      integer    :: IIVOLL = 49
      integer    :: IIVOL3 = 50
      integer    :: IIR1   = 51
      integer    :: IIQXK  = 52
      integer    :: IIQYK  = 53
      integer    :: IIQZK  = 54
      integer    :: IIDIFX = 55
      integer    :: IIDIFY = 56
      integer    :: IIDIFZ = 57
      integer    :: IIVOLA = 58
      integer    :: IIVOLB = 59
      integer    :: IIGUV  = 60
      integer    :: IIGVU  = 61
      integer    :: IIGZZ  = 62
      integer    :: IIAAK  = 63
      integer    :: IIBBK  = 64
      integer    :: IICCK  = 65
      integer    :: IIBD3X = 66
      integer    :: IIBDDX = 67
      integer    :: IIBDX  = 68
      integer    :: IIBU3X = 69
      integer    :: IIBUUX = 70
      integer    :: IIBUX  = 71
      integer    :: IIWRK1 = 72
      integer    :: IIWRK2 = 73
      integer    :: IIAAKL = 74
      integer    :: IIBBKL = 75
      integer    :: IICCKL = 76
      integer    :: IIDDKL = 77
!
      integer    IVVOL, IVARE, IVFLO, IVLEN, IVCNS, IVPAR, IVFUN, IVSFU,
     +           IVCNC, IVMAS, IVDER, IVDSP, IVVEL, IVDEF, IVLOC, IVDSX,
     +           IVVLX, IVLCX, IVFLX

      integer    ivar, icons, ipa , ifun, isys, isfun, idsp, ivel, iloc,
     +           idsx, ivlx , ilcx, idef, iflx

      integer(4) ithandl /0/
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
