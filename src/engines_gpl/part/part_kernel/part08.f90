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
module m_part08

    implicit none

contains


    subroutine part08 (lun, nodye, nocont, ictmax, amassd, &
            ictime, amassc, aconc, tmass, tmassc, &
            nosubs, ndprt, tmassu, ftime, linear, &
            substi, nmdyer, nmconr)

        !     CALCULATES TOTAL RELEASED MASS FOR DYE AND CONTINOUS RELEASES
        !              (per time step)

        !     system administration : m. zeeuw

        !     created      : february 1990, by l. postma

        !     modified      : july 1992 by r.j. vos
        !                    - for 8 substances with identical trans-
        !                      port but different decay
        !                      june 1993 by m. zeeuw
        !                    - initialized array tmassu with zero's

        !     note      : none.

        !     logical unit numbers  : lun - output log file

        !     subroutines called    : stop_exit

        !     functions   called    : none.

        use m_waq_precision    ! single/double precision
        use timers
        implicit none

        !     parameters

        !     kind          function         name                          Descriptipon

        integer (int_wp), intent(in) :: lun                         !< output unit number
        integer (int_wp), intent(in) :: nodye                       !< number of dye releases
        integer (int_wp), intent(in) :: nocont                      !< number of continuous releases
        integer (int_wp), intent(in) :: nosubs                      !< number of substances
        integer (int_wp), intent(in) :: ictmax(nocont)              !< number of breakpoints per continuous discharge
        real    (real_wp), intent(in) :: amassd(nosubs, nodye)        !< masses of dye releases (per subst)
        integer (int_wp), intent(in) :: ictime(nocont, *)          !< breakpoint times per continuous load
        real    (real_wp), intent(in) :: amassc(nocont, nosubs, *)     !< rates of continuous releases at breakpoints per substances
        real    (real_wp), intent(out) :: aconc (nodye + nocont, nosubs) !< mass per particle for each load and substance
        real    (real_wp), intent(out) :: tmass (nosubs)              !< total mass per substance
        real    (real_wp), intent(out) :: tmassc(nocont, nosubs)       !< total mass continuous loads per substance
        integer (int_wp), intent(in) :: ndprt (nodye + nocont)        !< number of particles per load
        real    (real_wp), intent(out) :: tmassu(nocont)              !< total unit mass continuous loads
        real    (real_wp), intent(in) :: ftime (nocont, *)            !< unit release rate per breakpoint
        integer (int_wp), intent(in) :: linear(nocont)              !< 1 = block; 2 = linear
        character(*), intent(in) :: substi(nosubs)              !< substance names
        character(*), intent(in) :: nmdyer(nodye)              !< dye release names
        character(*), intent(in) :: nmconr(nocont)              !< continuous release names
        !
        !     parameters     :
        !
        !     name    kind     length       funct.  description
        !     ====    ====     ======       ======  ===========
        !     mmap    integer   1           input   dimension of plot grid
        !     nmap    integer   1           input   dimension of plot grid
        !     surf    integer   1           output  surface of one plot grid cell
        !     window  real      4           input   plotgrid window
        !
        !     ----    ----       ------  ------  -----------
        !     am      real   1      local      help var. for mass/substance
        !     aminm   real   1      local      min. mass
        !     fac1    real   1      local      factor 1
        !     fac2    real   1      local      factor 2
        !     i       integer   1   local      index pointer
        !     i1      integer   1   local      index pointer 1
        !     i2      integer   1   local      index pointer 2
        !     id      integer   1   local      help pointer
        !     isub    integer   1   local      substance index
        !     itime   integer   1   local      help var. for ictime
        !     subnam  char.* 6  1   local      name of this program/file
        !
        !
        save
        !
        !     local scalars
        !
        integer(int_wp) :: i, id, idt, isub
        real   (sp) :: am

        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("part08", ithndl)
        !
        !     determine total mass dye releases
        !     (for the moment no  determination of the smallest)
        !
        tmass = 0.0  ! whole array assignment
        do i = 1, nodye
            do isub = 1, nosubs
                tmass(isub) = tmass(isub) + amassd(isub, i)
            end do
        end do
        !
        write(lun, '(3x,a,a/3x,a)')                            &
                'Total mass to be released during simulation for ', &
                'instantaneous releases ', &
                '(summarized mass over all releases)'
        do isub = 1, nosubs
            write (lun, 1035) substi(isub), tmass(isub)
        enddo
        !
        !     add amount of waste loads, convert to units per second
        !    (for the moment no  determination of the smallest)
        !
        !     time integration of continuous released masses
        !
        if (nocont > 0) then
            tmassc = 0.0  ! whole array assignment
            tmassu = 0.0  ! whole array assignment
        endif
        !
        !     determination of the total mass per substance for cont releases
        !
        do  isub = 1, nosubs
            do i = 1, nocont
                do id = 1, ictmax(i) - 1
                    idt = ictime(i, id + 1) - ictime(i, id)
                    !
                    !                  linear = 0 : block interpolation
                    !                         = 1 : lineair interpoaltion
                    !
                    if      (linear(i)==0) then
                        am = amassc(i, isub, id)
                    else if (linear(i)==1) then
                        am = (amassc(i, isub, id) + &
                                amassc(i, isub, id + 1)) / 2.0
                    endif
                    tmassc(i, isub) = idt * am + tmassc(i, isub)
                end do
            end do
        end do
        !
        !     determination of the total unit mass for cont releases
        !
        do i = 1, nocont
            do id = 1, ictmax(i) - 1
                idt = ictime(i, id + 1) - ictime(i, id)
                !
                !              linear = 0 : block interpolation
                !                     = 1 : lineair interpoaltion
                !
                if      (linear(i)==0) then
                    am = ftime(i, id)
                else if (linear(i)==1) then
                    am = (ftime(i, id) + ftime(i, id + 1)) / 2.0
                endif
                tmassu(i) = idt * am + tmassu(i)
            end do
        end do
        !
        !     end of time integration / determine now total mass contiuous
        !     releases
        !
        tmass = 0.0  ! whole array assignment
        do isub = 1, nosubs
            do i = 1, nocont
                tmass(isub) = tmass(isub) + tmassc(i, isub)
            end do
        end do
        !
        write(lun, '(3x,a,a/3x,a)')                                &
                'Total mass to be released during simulation for ', &
                'continuous releases ', &
                '(time integrated load over all releases)'
        do isub = 1, nosubs
            write (lun, 1035) substi(isub), tmass(isub)
        enddo
        !
        !     write statistics, determine mass per particle for each load and each
        !     substance
        !
        do isub = 1, nosubs
            do i = 1, nodye
                aconc(i, isub) = amassd(isub, i) / ndprt(i)
            end do
            !
            do i = 1, nocont
                aconc(i + nodye, isub) = tmassc(i, isub) / ndprt(i + nodye)
            end do
        end do
        !
        do i = 1, nodye
            write (lun, 1000) nmdyer(i), ndprt(i)
            write(lun, '(5x,a)') ' Particle mass per substance:'
            do isub = 1, nosubs
                write (lun, 1001) substi(isub), aconc(i, isub)
            enddo
        end do
        !
        do i = nodye + 1, nodye + nocont
            write (lun, 1002) nmconr(i - nodye), ndprt(i)
            write(lun, '(5x,a)') ' Particle mass per substance:'
            do isub = 1, nosubs
                write (lun, 1003) substi(isub), aconc(i, isub)
            enddo
        end do
        !
        !     end of routine
        !
        if (timon) call timstop (ithndl)
        return
        !
        !     formats
        !
        1000 format (/3x, 'Instantaneous release : ', a, /     &
                5x, ' Number of particles for this release: ', i7)
        1001 format (10x, a, e15.6, ' kg')
        1002 format (/3x, ' Continuous release : ', a, /       &
                5x, ' Number of particles for this release: ', i7)
        1003 format (10x, a, e15.6, ' kg')
        1035 format (6x, a, e15.6, ' kg')
        !
    end subroutine

end module m_part08
