!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_setvat
    use m_waq_precision
    use m_string_utils

    implicit none

contains


    subroutine setvat (lurep, num_constants, num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, &
            num_substances_transported, num_substances_total, num_dispersion_arrays, num_velocity_arrays, num_defaults, &
            num_local_vars, num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, num_fluxes, &
            nopred, num_vars, vararr, varidx, vartda, &
            vardag, vartag, varagg, num_grids, coname, &
            paname, funame, sfname, dename, syname, &
            locnam, varnam)
        ! Set variable atributes
        use m_logger_helper, only : stop_with_error, write_log_message
        use timers       !   performance timers

        integer(kind = int_wp) :: lurep, num_constants, num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, &
                num_substances_transported, num_substances_total, num_dispersion_arrays, num_velocity_arrays, num_defaults, &
                num_local_vars, num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, num_fluxes, &
                nopred, num_vars, num_grids
        integer(kind = int_wp) :: vararr(num_vars), varidx(num_vars), &
                vartda(num_vars), vardag(num_vars), &
                vartag(num_vars), varagg(num_vars)
        character(len = 20)        varnam(num_vars)

        integer(kind = int_wp), parameter :: maxloc = 2000
        integer(kind = int_wp) :: vattag(maxloc), vattda(maxloc)
        character(len = 20) vatnam(maxloc), vatnag(maxloc), &
                vatnda(maxloc)
        character(len = 20) coname(*), paname(*), &
                funame(*), sfname(*), &
                syname(*), locnam(*), &
                dename(*)
        character(len = 79) line, name
        logical      lexi
        integer(kind = int_wp) :: file_unit, iivol, iiarea, iiflow, iileng, iidisp, iiconc, &
                iimass, iiderv, iiboun, iibset, iibsav, iiwste, iicons, &
                iiparm, iifunc, iisfun, iidnew, iidiff, iivnew, iivelo, &
                iiharm, iifarr, iimas2, iitimr, iivol2, iitrac, iigwrk, &
                iighes, iigsol, iigdia, iigtri, iismas, iiploc, iidefa, &
                iiflux, iistoc, iiflxd, iiflxi, iiriob, iidspx, iivelx, &
                iilocx, iidsto, iivsto, iidmpq, iidmps, iitrra, iinrsp, &
                iivoll, iivol3, iir1, iiqxk, iiqyk, iiqzk, iidifx, &
                iidify, iidifz, iivola, iivolb, iiguv, iigvu, iigzz, &
                iiaak, iibbk, iicck, iibd3x, iibddx, iibdx, iibu3x, iibuux, &
                iibux, iiwrk1, iiwrk2, iiaakl, iibbkl, iicckl, iiddkl

        integer(kind = int_wp) :: ivvol, ivare, ivflo, ivlen, ivcns, ivpar, ivfun, ivsfu, ivcnc, &
                ivmas, ivder, ivdsp, ivvel, ivdef, ivloc, ivdsx, ivvlx, ivlcx, &
                ivflx

        integer(kind = int_wp) :: isys, ipa, ifun, idsp, isfun, ivel, idef, ivar, iv_da, iv_ag, &
                idsx, ivlx, ilcx, iflx, novat, ierr, ivat, iloc, icons

        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("setvat", ithndl)

        ! Just take the used array's in the right order
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
        IVPAR = IVCNS + num_constants
        IVFUN = IVPAR + num_spatial_parameters
        IVSFU = IVFUN + num_time_functions
        IVCNC = IVSFU + num_spatial_time_fuctions
        IVMAS = IVCNC + num_substances_total
        IVDER = IVMAS + num_substances_total
        IVDSP = IVDER + num_substances_total
        IVVEL = IVDSP + num_dispersion_arrays
        IVDEF = IVVEL + num_velocity_arrays
        IVLOC = IVDEF + num_defaults
        IVDSX = IVLOC + num_local_vars
        IVVLX = IVDSX + num_dispersion_arrays_extra
        IVLCX = IVVLX + num_velocity_arrays_extra
        IVFLX = IVLCX + num_local_vars_exchange
        !
        !
        !
        !
        !     Volume
        !
        IVAR = 1
        VARNAM(IVAR) = 'VOLUME'
        VARARR(IVAR) = IIVOL
        VARIDX(IVAR) = 1
        VARTDA(IVAR) = 0
        VARDAG(IVAR) = 0
        VARTAG(IVAR) = 1
        VARAGG(IVAR) = 0
        !
        !     Area
        !
        IVAR = IVAR + 1
        VARNAM(IVAR) = 'XAREA'
        VARARR(IVAR) = IIAREA
        VARIDX(IVAR) = 1
        VARTDA(IVAR) = 0
        VARDAG(IVAR) = 0
        VARTAG(IVAR) = 0
        VARAGG(IVAR) = 0
        !
        !     Flow
        !
        IVAR = IVAR + 1
        VARNAM(IVAR) = 'FLOW'
        VARARR(IVAR) = IIFLOW
        VARIDX(IVAR) = 1
        VARTDA(IVAR) = 0
        VARDAG(IVAR) = 0
        VARTAG(IVAR) = 0
        VARAGG(IVAR) = 0
        !
        !     Length , two length
        !
        IVAR = IVAR + 1
        VARNAM(IVAR) = 'XLENFROM'
        VARARR(IVAR) = IILENG
        VARIDX(IVAR) = 1
        VARTDA(IVAR) = 0
        VARDAG(IVAR) = 0
        VARTAG(IVAR) = 0
        VARAGG(IVAR) = 0
        IVAR = IVAR + 1
        VARNAM(IVAR) = 'XLENTO'
        VARARR(IVAR) = IILENG
        VARIDX(IVAR) = 2
        VARTDA(IVAR) = 0
        VARDAG(IVAR) = 0
        VARTAG(IVAR) = 0
        VARAGG(IVAR) = 0
        !
        !     Cons
        !
        DO ICONS = 1, num_constants
            IVAR = IVAR + 1
            VARNAM(IVAR) = CONAME(ICONS)
            VARARR(IVAR) = IICONS
            VARIDX(IVAR) = ICONS
            VARTDA(IVAR) = 0
            VARDAG(IVAR) = 0
            VARTAG(IVAR) = 0
            VARAGG(IVAR) = 0
        ENDDO
        !
        !     Param
        !
        DO IPA = 1, num_spatial_parameters
            IVAR = IVAR + 1
            VARNAM(IVAR) = PANAME(IPA)
            VARARR(IVAR) = IIPARM
            VARIDX(IVAR) = IPA
            VARTDA(IVAR) = 0
            VARDAG(IVAR) = 0
            VARTAG(IVAR) = 3
            VARAGG(IVAR) = 1
            VARTDA(IVAR) = 0
            VARDAG(IVAR) = 0
            VARTAG(IVAR) = 3
            VARAGG(IVAR) = 1
        ENDDO
        !
        !     Func
        !
        DO IFUN = 1, num_time_functions
            IVAR = IVAR + 1
            VARNAM(IVAR) = FUNAME(IFUN)
            VARARR(IVAR) = IIFUNC
            VARIDX(IVAR) = IFUN
            VARTDA(IVAR) = 0
            VARDAG(IVAR) = 0
            VARTAG(IVAR) = 0
            VARAGG(IVAR) = 0
        ENDDO
        !
        !     Seg Func
        !
        DO ISFUN = 1, num_spatial_time_fuctions
            IVAR = IVAR + 1
            VARNAM(IVAR) = SFNAME(ISFUN)
            VARARR(IVAR) = IISFUN
            VARIDX(IVAR) = ISFUN
            VARTDA(IVAR) = 0
            VARDAG(IVAR) = 0
            VARTAG(IVAR) = 3
            VARAGG(IVAR) = 1
        ENDDO
        !
        !     Conc
        !
        DO ISYS = 1, num_substances_transported
            IVAR = IVAR + 1
            VARNAM(IVAR) = SYNAME(ISYS)
            VARARR(IVAR) = IICONC
            VARIDX(IVAR) = ISYS
            VARTDA(IVAR) = 0
            VARDAG(IVAR) = 0
            VARTAG(IVAR) = 3
            VARAGG(IVAR) = 1
        ENDDO
        DO ISYS = num_substances_transported + 1, num_substances_total
            IVAR = IVAR + 1
            VARNAM(IVAR) = SYNAME(ISYS)
            VARARR(IVAR) = IICONC
            VARIDX(IVAR) = ISYS
            VARTDA(IVAR) = 0
            VARDAG(IVAR) = 0
            VARTAG(IVAR) = 1
            VARAGG(IVAR) = 0
        ENDDO
        !
        !     Mass
        !
        DO ISYS = 1, num_substances_total
            IVAR = IVAR + 1
            VARNAM(IVAR) = 'MASS_' // SYNAME(ISYS)
            VARARR(IVAR) = IIMASS
            VARIDX(IVAR) = ISYS
            VARTDA(IVAR) = 0
            VARDAG(IVAR) = 0
            VARTAG(IVAR) = 1
            VARAGG(IVAR) = 0
        ENDDO
        !
        !     Deriv
        !
        DO ISYS = 1, num_substances_total
            IVAR = IVAR + 1
            VARNAM(IVAR) = 'DERIV_' // SYNAME(ISYS)
            VARARR(IVAR) = IIDERV
            VARIDX(IVAR) = ISYS
            VARTDA(IVAR) = 2
            VARDAG(IVAR) = IVMAS + ISYS - 1
            VARTAG(IVAR) = 0
            VARAGG(IVAR) = 0
        ENDDO
        !
        !     Disp
        !
        DO IDSP = 1, num_dispersion_arrays
            IVAR = IVAR + 1
            NAME = ' '
            WRITE (NAME, '("DISP_ARRAY_",I0)') IDSP
            VARNAM(IVAR) = NAME
            VARARR(IVAR) = IIDISP
            VARIDX(IVAR) = IDSP
            VARTDA(IVAR) = 0
            VARDAG(IVAR) = 0
            VARTAG(IVAR) = 0
            VARAGG(IVAR) = 0
        ENDDO
        !
        !     Velo
        !
        DO IVEL = 1, num_velocity_arrays
            IVAR = IVAR + 1
            NAME = ' '
            WRITE (NAME, '("VELO_ARRAY_",I0)') IVEL
            VARNAM(IVAR) = NAME
            VARARR(IVAR) = IIVELO
            VARIDX(IVAR) = IVEL
            VARTDA(IVAR) = 0
            VARDAG(IVAR) = 0
            VARTAG(IVAR) = 0
            VARAGG(IVAR) = 0
        ENDDO
        !
        !     Default
        !
        DO IDEF = 1, num_defaults
            IVAR = IVAR + 1
            VARNAM(IVAR) = DENAME(IDEF)
            VARARR(IVAR) = IIDEFA
            VARIDX(IVAR) = IDEF
            VARTDA(IVAR) = 0
            VARDAG(IVAR) = 0
            VARTAG(IVAR) = 0
            VARAGG(IVAR) = 0
        ENDDO
        !
        !     Local
        !
        DO ILOC = 1, num_local_vars
            IVAR = IVAR + 1
            VARNAM(IVAR) = LOCNAM(ILOC)
            VARARR(IVAR) = IIPLOC
            VARIDX(IVAR) = ILOC
            VARTDA(IVAR) = 1
            VARDAG(IVAR) = 0
            VARTAG(IVAR) = 3
            VARAGG(IVAR) = 1
        ENDDO
        !
        !     DSPX
        !
        DO IDSX = 1, num_dispersion_arrays_extra
            IVAR = IVAR + 1
            VARARR(IVAR) = IIDSPX
            VARIDX(IVAR) = IDSX
            VARTDA(IVAR) = 0
            VARDAG(IVAR) = 0
            VARTAG(IVAR) = 0
            VARAGG(IVAR) = 0
        ENDDO
        !
        !     VELX
        !
        DO IVLX = 1, num_velocity_arrays_extra
            IVAR = IVAR + 1
            NAME = ' '
            WRITE (NAME, '("VELX_ARRAY_",I0)') IVLX
            VARNAM(IVAR) = NAME
            VARARR(IVAR) = IIVELX
            VARIDX(IVAR) = IVLX
            VARTDA(IVAR) = 0
            VARDAG(IVAR) = 0
            VARTAG(IVAR) = 0
            VARAGG(IVAR) = 0
        ENDDO
        !
        !     LOCX
        !
        DO ILCX = 1, num_local_vars_exchange
            IVAR = IVAR + 1
            NAME = ' '
            WRITE (NAME, '("VLOCX_",I0)') ILCX
            VARNAM(IVAR) = NAME
            VARARR(IVAR) = IILOCX
            VARIDX(IVAR) = ILCX
            VARTDA(IVAR) = 0
            VARDAG(IVAR) = 0
            VARTAG(IVAR) = 0
            VARAGG(IVAR) = 0
        ENDDO
        !
        !     FLUX
        !
        DO IFLX = 1, num_fluxes
            IVAR = IVAR + 1
            VARARR(IVAR) = IIFLUX
            VARIDX(IVAR) = IFLX
            VARTDA(IVAR) = 2
            VARDAG(IVAR) = IVVOL
            VARTAG(IVAR) = 1
            VARAGG(IVAR) = 0
        ENDDO
        !
        !     Read list of overrulings
        !
        NOVAT = 0
        INQUIRE (FILE = 'aggrlist.dat', EXIST = LEXI)
        IF (LEXI) THEN
            OPEN(NEWUNIT = file_unit, FILE = 'aggrlist.dat')
            DO
                NOVAT = NOVAT + 1
                IF (NOVAT > MAXLOC) THEN
                    LINE = 'ERROR : local dimension overflow in SETVAT'
                    CALL write_log_message(line)
                    WRITE(*, *) LINE
                    CALL stop_with_error()
                ENDIF
                READ(file_unit, *, IOSTAT = IERR) VATNAM(NOVAT), VATTAG(NOVAT), VATNAG(NOVAT), &
                        VATTDA(NOVAT), VATNDA(NOVAT)
                IF (IERR /=0) THEN
                    EXIT
                ENDIF
            ENDDO
            NOVAT = NOVAT - 1
            CLOSE (file_unit)
        ENDIF
        !
        !     Check if there are overrulings
        !
        DO IVAR = 1, num_vars
            IVAT = index_in_array(VARNAM(IVAR), VATNAM(:NOVAT))
            IF (IVAT   > 0) THEN
                !
                !           aggregation
                !
                IF (VATTAG(IVAT) == 0) THEN
                    !
                    !              NO aggregation
                    !
                    VARTAG(IVAR) = VATTAG(IVAT)
                    VARAGG(IVAR) = 0
                    !
                ELSEIF (VATTAG(IVAT) == 1) THEN
                    !
                    !              Accumulate
                    !
                    VARTAG(IVAR) = VATTAG(IVAT)
                    VARAGG(IVAR) = 0
                    !
                ELSEIF (VATTAG(IVAT) == 2) THEN
                    !
                    !              Average
                    !
                    VARTAG(IVAR) = VATTAG(IVAT)
                    VARAGG(IVAR) = 0
                    !
                ELSEIF (VATTAG(IVAT) == 3) THEN
                    !
                    !              Weight average
                    !
                    IV_AG = index_in_array(VATNAG(IVAT), VARNAM)
                    IF (IV_AG > 0) THEN
                        VARTAG(IVAR) = VATTAG(IVAT)
                        VARAGG(IVAR) = IV_AG
                    ELSE
                        VARTAG(IVAR) = 2
                        VARAGG(IVAR) = 0
                    ENDIF
                    !
                ELSE
                    !
                    !              Error undefined type of aggregtation
                    !
                    IERR = IERR + 1
                    LINE = 'ERROR : undefined type off aggregation for :' // &
                            VARNAM(IVAR)
                    CALL write_log_message(line)
                    WRITE(LINE, '(''type:'',I5,'' from aggrlist.dat'')') &
                            VATTAG(IVAT)
                    CALL write_log_message(line)
                    !
                ENDIF
                !
                !           dis-aggregation
                !
                IF (VATTDA(IVAT) == 0) THEN
                    !
                    !              NO dis-aggregation
                    !
                    VARTDA(IVAR) = VATTDA(IVAT)
                    VARDAG(IVAR) = 0
                    !
                ELSEIF (VATTDA(IVAT) == 1) THEN
                    !
                    !              expansion
                    !
                    VARTDA(IVAR) = VATTDA(IVAT)
                    VARDAG(IVAR) = 0
                    !
                ELSEIF (VATTDA(IVAT) == 2) THEN
                    !
                    !              distribute with weight
                    !
                    IV_DA = index_in_array(VATNDA(IVAT), VARNAM)
                    IF (IV_DA > 0) THEN
                        VARTDA(IVAR) = VATTDA(IVAT)
                        VARDAG(IVAR) = IV_DA
                    ELSE
                        VARTDA(IVAR) = 3
                        VARDAG(IVAR) = 0
                    ENDIF
                    !
                ELSEIF (VATTDA(IVAT) == 3) THEN
                    !
                    !              distribute
                    !
                    VARTDA(IVAR) = VATTDA(IVAT)
                    VARDAG(IVAR) = 0
                    !
                ELSE
                    !
                    !              Error undefined type of aggregtation
                    !
                    IERR = IERR + 1
                    LINE = 'ERROR : undefined type off dis-aggregation for :' &
                            // VARNAM(IVAR)
                    CALL write_log_message(line)
                    WRITE(LINE, '(''type:'',I5,'' from aggrlist.dat'')') &
                            VATTAG(IVAT)
                    CALL write_log_message(line)
                    !
                ENDIF
            ENDIF
        ENDDO

        if (timon) call timstop(ithndl)

    end subroutine setvat

end module m_setvat
