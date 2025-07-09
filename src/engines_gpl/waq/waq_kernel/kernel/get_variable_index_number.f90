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
module m_get_variable_index_number
    use m_waq_precision
    use m_logger_helper

    implicit none

contains


    !!  Initialisation of Variables structure
    subroutine get_variable_index_number(iar_nr, indx, ivar)

        use m_waq_memory_dimensions

        integer(kind = int_wp) :: iar_nr    !!  Array number
        integer(kind = int_wp) :: indx      !! Index number variable in array
        integer(kind = int_wp) :: ivar      !! Variable number, else -1

        integer(kind = int_wp) :: iivol
        integer(kind = int_wp) :: iiarea
        integer(kind = int_wp) :: iiflow
        integer(kind = int_wp) :: iileng
        integer(kind = int_wp) :: iidisp
        integer(kind = int_wp) :: iiconc
        integer(kind = int_wp) :: iimass
        integer(kind = int_wp) :: iiderv
        integer(kind = int_wp) :: iiboun
        integer(kind = int_wp) :: iibset
        integer(kind = int_wp) :: iibsav
        integer(kind = int_wp) :: iiwste
        integer(kind = int_wp) :: iicons
        integer(kind = int_wp) :: iiparm
        integer(kind = int_wp) :: iifunc
        integer(kind = int_wp) :: iisfun
        integer(kind = int_wp) :: iidnew
        integer(kind = int_wp) :: iidiff
        integer(kind = int_wp) :: iivnew
        integer(kind = int_wp) :: iivelo
        integer(kind = int_wp) :: iiharm
        integer(kind = int_wp) :: iifarr
        integer(kind = int_wp) :: iimas2
        integer(kind = int_wp) :: iitimr
        integer(kind = int_wp) :: iivol2
        integer(kind = int_wp) :: iitrac
        integer(kind = int_wp) :: iigwrk
        integer(kind = int_wp) :: iighes
        integer(kind = int_wp) :: iigsol
        integer(kind = int_wp) :: iigdia
        integer(kind = int_wp) :: iigtri
        integer(kind = int_wp) :: iismas
        integer(kind = int_wp) :: iiploc
        integer(kind = int_wp) :: iidefa
        integer(kind = int_wp) :: iiflux
        integer(kind = int_wp) :: iistoc
        integer(kind = int_wp) :: iiflxd
        integer(kind = int_wp) :: iiflxi
        integer(kind = int_wp) :: iiriob
        integer(kind = int_wp) :: iidspx
        integer(kind = int_wp) :: iivelx
        integer(kind = int_wp) :: iilocx
        integer(kind = int_wp) :: iidsto
        integer(kind = int_wp) :: iivsto
        integer(kind = int_wp) :: iidmpq
        integer(kind = int_wp) :: iidmps
        integer(kind = int_wp) :: iitrra
        integer(kind = int_wp) :: iinrsp
        integer(kind = int_wp) :: iivoll
        integer(kind = int_wp) :: iivol3
        integer(kind = int_wp) :: iir1
        integer(kind = int_wp) :: iiqxk
        integer(kind = int_wp) :: iiqyk
        integer(kind = int_wp) :: iiqzk
        integer(kind = int_wp) :: iidifx
        integer(kind = int_wp) :: iidify
        integer(kind = int_wp) :: iidifz
        integer(kind = int_wp) :: iivola
        integer(kind = int_wp) :: iivolb
        integer(kind = int_wp) :: iiguv
        integer(kind = int_wp) :: iigvu
        integer(kind = int_wp) :: iigzz
        integer(kind = int_wp) :: iiaak
        integer(kind = int_wp) :: iibbk
        integer(kind = int_wp) :: iicck
        integer(kind = int_wp) :: iibd3x
        integer(kind = int_wp) :: iibddx
        integer(kind = int_wp) :: iibdx
        integer(kind = int_wp) :: iibu3x
        integer(kind = int_wp) :: iibuux
        integer(kind = int_wp) :: iibux
        integer(kind = int_wp) :: iiwrk1
        integer(kind = int_wp) :: iiwrk2
        integer(kind = int_wp) :: iiaakl
        integer(kind = int_wp) :: iibbkl
        integer(kind = int_wp) :: iicckl
        integer(kind = int_wp) :: iiddkl

        integer(kind = int_wp) :: ivvol
        integer(kind = int_wp) :: ivare
        integer(kind = int_wp) :: ivflo
        integer(kind = int_wp) :: ivlen
        integer(kind = int_wp) :: ivcns
        integer(kind = int_wp) :: ivpar
        integer(kind = int_wp) :: ivfun
        integer(kind = int_wp) :: ivsfu
        integer(kind = int_wp) :: ivcnc
        integer(kind = int_wp) :: ivmas
        integer(kind = int_wp) :: ivder
        integer(kind = int_wp) :: ivdsp
        integer(kind = int_wp) :: ivvel
        integer(kind = int_wp) :: ivdef
        integer(kind = int_wp) :: ivloc
        integer(kind = int_wp) :: ivdsx
        integer(kind = int_wp) :: ivvlx
        integer(kind = int_wp) :: ivlcx
        integer(kind = int_wp) :: ivflx
        integer(kind = int_wp) :: lunrep
        !
        !     Just take the used array's in the right order
        !
        iivol = 1
        iiarea = 2
        iiflow = 3
        iileng = 4
        iidisp = 5
        iiconc = 6
        iimass = 7
        iiderv = 8
        iiboun = 9
        iibset = 10
        iibsav = 11
        iiwste = 12
        iicons = 13
        iiparm = 14
        iifunc = 15
        iisfun = 16
        iidnew = 17
        iidiff = 18
        iivnew = 19
        iivelo = 20
        iiharm = 21
        iifarr = 22
        iimas2 = 23
        iitimr = 24
        iivol2 = 25
        iitrac = 26
        iigwrk = 27
        iighes = 28
        iigsol = 29
        iigdia = 30
        iigtri = 31
        iismas = 32
        iiploc = 33
        iidefa = 34
        iiflux = 35
        iistoc = 36
        iiflxd = 37
        iiflxi = 38
        iiriob = 39
        iidspx = 40
        iivelx = 41
        iilocx = 42
        iidsto = 43
        iivsto = 44
        iidmpq = 45
        iidmps = 46
        iitrra = 47
        iinrsp = 48
        iivoll = 49
        iivol3 = 50
        iir1 = 51
        iiqxk = 52
        iiqyk = 53
        iiqzk = 54
        iidifx = 55
        iidify = 56
        iidifz = 57
        iivola = 58
        iivolb = 59
        iiguv = 60
        iigvu = 61
        iigzz = 62
        iiaak = 63
        iibbk = 64
        iicck = 65
        iibd3x = 66
        iibddx = 67
        iibdx = 68
        iibu3x = 69
        iibuux = 70
        iibux = 71
        iiwrk1 = 72
        iiwrk2 = 73
        iiaakl = 74
        iibbkl = 75
        iicckl = 76
        iiddkl = 77
        !
        ivvol = 1
        ivare = ivvol + 1
        ivflo = ivare + 1
        ivlen = ivflo + 1
        ivcns = ivlen + 2
        ivpar = ivcns + num_constants
        ivfun = ivpar + num_spatial_parameters
        ivsfu = ivfun + num_time_functions
        ivcnc = ivsfu + num_spatial_time_fuctions
        ivmas = ivcnc + num_substances_total
        ivder = ivmas + num_substances_total
        ivdsp = ivder + num_substances_total
        ivvel = ivdsp + num_dispersion_arrays
        ivdef = ivvel + num_velocity_arrays
        ivloc = ivdef + num_defaults
        ivdsx = ivloc + num_local_vars
        ivvlx = ivdsx + num_dispersion_arrays_extra
        ivlcx = ivvlx + num_velocity_arrays_extra
        ivflx = ivlcx + num_local_vars_exchange
        !
        ivar = -1
        !
        if (iar_nr == iivol) then
            if (indx > 1) goto 900
            ivar = ivvol + indx - 1
        endif
        !
        if (iar_nr == iiarea) then
            if (indx > 1) goto 900
            ivar = ivare + indx - 1
        endif
        !
        if (iar_nr == iiflow) then
            if (indx > 1) goto 900
            ivar = ivflo + indx - 1
        endif
        !
        if (iar_nr == iileng) then
            if (indx > 2) goto 900
            ivar = ivlen + indx - 1
        endif
        !
        if (iar_nr == iicons) then
            if (indx > num_constants) goto 900
            ivar = ivcns + indx - 1
        endif
        !
        if (iar_nr == iiparm) then
            if (indx > num_spatial_parameters) goto 900
            ivar = ivpar + indx - 1
        endif
        !
        if (iar_nr == iifunc) then
            if (indx > num_time_functions) goto 900
            ivar = ivfun + indx - 1
        endif
        !
        if (iar_nr == iisfun) then
            if (indx > num_spatial_time_fuctions) goto 900
            ivar = ivsfu + indx - 1
        endif
        !
        if (iar_nr == iiconc) then
            if (indx > num_substances_total) goto 900
            ivar = ivcnc + indx - 1
        endif
        !
        if (iar_nr == iimass) then
            if (indx > num_substances_total) goto 900
            ivar = ivmas + indx - 1
        endif
        !
        if (iar_nr == iiderv) then
            if (indx > num_substances_total) goto 900
            ivar = ivder + indx - 1
        endif
        !
        if (iar_nr == iidisp) then
            if (indx > num_dispersion_arrays) goto 900
            ivar = ivdsp + indx - 1
        endif
        !
        if (iar_nr == iivelo) then
            if (indx > num_velocity_arrays) goto 900
            ivar = ivvel + indx - 1
        endif
        !
        if (iar_nr == iidefa) then
            if (indx > num_defaults) goto 900
            ivar = ivdef + indx - 1
        endif
        !
        if (iar_nr == iiploc) then
            if (indx > num_local_vars) goto 900
            ivar = ivloc + indx - 1
        endif
        !
        if (iar_nr == iidspx) then
            if (indx > num_dispersion_arrays_extra) goto 900
            ivar = ivdsx + indx - 1
        endif
        !
        if (iar_nr == iivelx) then
            if (indx > num_velocity_arrays_extra) goto 900
            ivar = ivvlx + indx - 1
        endif

        if (iar_nr == iilocx) then
            if (indx > num_local_vars_exchange) goto 900
            ivar = ivlcx + indx - 1
        endif

        if (iar_nr == iiflux) then
            if (indx > num_fluxes) goto 900
            ivar = ivflx + indx - 1
        endif

        if (ivar == -1) goto 900

        return

        900 continue
        call get_log_unit_number(lunrep)
        write(lunrep, 2000) iar_nr, indx
        return
        2000 FORMAT (' WARNING in get_variable_index_number, array or index out of range', I10, I10)

    end subroutine

end module m_get_variable_index_number
