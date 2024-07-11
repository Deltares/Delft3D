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
module m_get_sfrac
    use m_waq_precision

    implicit none

contains


    subroutine get_sfrac (lunrep, num_substances_total, syname, nomult, imultp, &
            sfracs)

        ! identifies the number of subtances which consist of fractions

        use processet
        use timers       !   performance timers

        implicit none

        ! declaration of arguments

        integer(kind = int_wp) :: lunrep          !< report file
        integer(kind = int_wp) :: num_substances_total           !< number of substances
        character(len = 20) :: syname(num_substances_total)   !< substance name
        integer(kind = int_wp), intent(in) :: nomult          !< number of multiple substances
        integer(kind = int_wp), intent(in) :: imultp(2, nomult)!< multiple substance administration
        type(sfracsprop) :: sfracs          !< substance fraction properties

        ! local decalarations

        integer(kind = int_wp) :: isys            ! loop counter substances
        integer(kind = int_wp) :: isys2           ! loop counter substances
        integer(kind = int_wp) :: ilen            ! length substance name
        integer(kind = int_wp) :: ifound          ! ifound
        integer(kind = int_wp) :: isfrac          ! substance fraction number
        integer(kind = int_wp) :: isfrac2         ! substance fraction number
        integer(kind = int_wp) :: io_error        ! read error indication
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("get_sfrac", ithndl)

        ! allocate sfracs

        allocate(sfracs%name(nomult), sfracs%nfrac(nomult), sfracs%linked(nomult), sfracs%linklist(nomult, nomult))
        sfracs%linklist = 0

        ! loop over the fractions

        sfracs%nsfrac = nomult
        do isfrac = 1, nomult
            isys = imultp(1, isfrac)
            sfracs%nfrac(isfrac) = imultp(2, isfrac) - imultp(1, isfrac) + 1
            ilen = len(trim(syname(isys)))
            sfracs%name(isfrac) = syname(isys)(1:ilen - 2)

            ! report

            write(lunrep, *)
            write(lunrep, 2000) trim(sfracs%name(isfrac))
            write(lunrep, 2001) sfracs%nfrac(isfrac)

            ! linked ?

            sfracs%linked(isfrac) = 0
            do isfrac2 = 1, isfrac - 1
                if (sfracs%nfrac(isfrac2) == sfracs%nfrac(isfrac)) then
                    if (sfracs%linked(isfrac2) == 0) then
                        sfracs%linked(isfrac2) = isfrac2
                    endif
                    sfracs%linked(isfrac) = isfrac2
                    sfracs%linklist(isfrac, isfrac2) = 1
                    sfracs%linklist(isfrac2, isfrac) = 1
                    write(lunrep, 2002) trim(sfracs%name(isfrac2))
                    exit
                endif
            enddo

        enddo

        write(lunrep, *)
        write(lunrep, 2003) sfracs%nsfrac

        if (timon) call timstop(ithndl)
        return
        2000 format ('substance fractions detected for substance:', a)
        2001 format ('number of fractions                       :', i3)
        2002 format ('substance fractions linked with substance :', a)
        2003 format ('total number of substances with fractions :', i3)
    end

end module m_get_sfrac
