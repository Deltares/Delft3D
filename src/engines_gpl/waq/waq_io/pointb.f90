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
module m_pointb
    use m_waq_precision
    use m_error_status

    implicit none

contains


    subroutine pointb (lun, ioutpt, gridps, ibnd, ipoint, &
            noqt, status)

        !       Deltares Software Centre

        !>\file
        !>         Makes and write additional pointer for the water bed
        !>
        !>         This routine makes pointers as follows:
        !>         - for the first bed layer the pointers with all water cells are made
        !>           per bed cell, using the backpointer from bed grid to base grid
        !>         - per bed layer the pointers within the bed are made
        !>         - at the end of the (may be varying) bed column length, an open boundary
        !>           is created
        !>         - each column is doubled right afterwards ( for advection and diffusion )
        !>         The amount thus created exchanges is checked to the given amount noqt./n
        !>         The result is written to the system file and arrays are deallocated

        !     Created            : January 2003  by Leo Postma

        !     Modified           : ???     2003  by Jan van Beek : allow for different nr of layers
        !                                                          in the bed
        !                          May     2011  by Leo Postma   : merge with trunk

        !     Subroutines called : none

        !     Logical units      : LUN(29) = unit formatted output file
        !                          LUN( 2) = unit intermediate file (system)

        use timers       !   performance timers
        use dlwqgrid_mod ! for the storage of contraction grids
        use m_sysn          ! System characteristics

        implicit none


        !     Parameters

        !     kind           function         name                        Descriptipon

        integer(kind = int_wp), intent(in) :: lun   (*)         !< array with unit numbers
        integer(kind = int_wp), intent(in) :: ioutpt              !< how extensive is output ?
        type(GridPointerColl)           GridPs             !< Collection of grid pointers
        integer(kind = int_wp), intent(in) :: ibnd  (nobnd, 2)  !< normal boundary pointers
        integer(kind = int_wp), intent(in) :: noqt                !< total number of exchanges
        integer(kind = int_wp), intent(inout) :: ipoint(4, noqt)  !< exchange pointers

        type(error_status), intent(inout) :: status !< current error status
        !
        !     COMMON BLOCK  / SYSN / :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ---------------------------------------------------------
        !     NOSEG   INTEGER  1           INPUT   number of segments
        !     NSEG2   INTEGER  1           INPUT   number of bottom segments
        !     NOSYS   INTEGER  1           INPUT   number of active substances
        !     NODISP  INTEGER  1           OUTPUT  number of dispersion arrays
        !     NOVELO  INTEGER  1           OUTPUT  number of velocity arrays
        !     NOQ1    INTEGER  1           OUTPUT  number of exch. 1st direction
        !     NOQ2    INTEGER  1           OUTPUT  number of exch. 2nd direction
        !     NOQ3    INTEGER  1           OUTPUT  number of exch. 3rd direction
        !     NOQ4    INTEGER  1           OUTPUT  number of exch. bottom direction
        !     NOQ     INTEGER  1           OUTPUT  number of exchanges
        !     NOBND   INTEGER  1           OUTPUT  number of boundaries
        !     JTRACK  INTEGER  1           OUTPUT  number of codiagonals
        !     NDMPAR  INTEGER  1           INPUT   number of dump areas
        !     NDMPQ   INTEGER  1           OUTPUT  number exchanges dumped
        !     NDMPS   INTEGER  1           OUTPUT  number segments dumped
        !     NTDMPQ  INTEGER  1           OUTPUT  total number exchanges in dump area
        !     NTDMPS  INTEGER  1           INPUT   total number segments in dump area
        !     NORAAI  INTEGER  1           INPUT   number of raaien
        !     NTRAAQ  INTEGER  1           INPUT   total number of exch. in raaien
        !     NOMAT   INTEGER  1           OUTPUT  size of the fastsolvers matrix

        !     local declarations

        integer(kind = int_wp) :: lunut            ! output unit number (lun(29))
        integer(kind = int_wp), allocatable :: IAbnd(:, :)       ! array with boundary information in the bed
        integer(kind = int_wp) :: ilay             ! index layer number
        integer(kind = int_wp) :: isegb            ! counter for bed volumes
        integer(kind = int_wp) :: iq               ! loop counter for exchanges in one bed column
        integer(kind = int_wp) :: ib               ! loop counter for bed volumes
        integer(kind = int_wp) :: iqt              ! counter for exchanges in the bed
        integer(kind = int_wp) :: jbott            ! grid number of the bottom_grid
        integer(kind = int_wp) :: jbase            ! grid number of the base_grid
        integer(kind = int_wp) :: nsegl            ! nr of volumes per water layer
        integer(kind = int_wp) :: nlay             ! nr of layers in the water
        integer(kind = int_wp) :: nsegb            ! nr of volumes per bed layer
        integer(kind = int_wp) :: nlayb            ! nr of layers in the bed
        integer(kind = int_wp), allocatable :: botmatrix(:, :)   ! matrix with bottom segment number in case of space varying number of layers
        logical :: space_var_nolay ! space varying number of layers in the bed ?
        integer(kind = int_wp) :: nolaymax         ! maximum number of bed layers in a bed column
        integer(kind = int_wp) :: ioff1            ! offset volume nr's last water layer
        integer(kind = int_wp) :: ioff2            ! offset volume nr's one but last water layer
        integer(kind = int_wp) :: inaarplus        ! the 'to+1' exchange pointer
        integer(kind = int_wp) :: i, k             ! loop counters
        logical :: odd             ! mention only the first boundary
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("pointb", ithndl)

        lunut = lun(29)

        !        is there a bottom direction ?

        if (noq4 == 0) then
            if (nobnd > 0) then
                write (lun(2)) (ibnd (k, 1), k = 1, nobnd)
                write (lun(2)) (ibnd (k, 2), k = 1, nobnd)
            endif
            goto 9999
        endif

        !        is there a bottom grid ?

        JBott = GridPs%bottom_grid
        if (JBott == 0) then
            write (lunut, 1050)
            call status%increase_error_count()
            goto 9999
        endif

        !        allocate memory

        JBase = GridPs%base_grid
        nsegl = GridPs%Pointers(JBase)%noseg_lay ! nr of segments per layer
        nlay = GridPs%Pointers(JBase)%nolay     !             in the water
        nlayb = GridPs%Pointers(JBott)%nolay     !
        nsegb = GridPs%Pointers(JBott)%noseg_lay ! and in the bottom
        allocate(IAbnd(nsegb, 2))

        space_var_nolay = GridPs%Pointers(JBott)%space_var_nolay
        if (space_var_nolay) then
            !        make complete bottom matrix with segment numbers in bottom
            !              (one could also construct the matrix for fixed layers in order to simplify the algorithm)
            nolaymax = maxval(GridPs%Pointers(JBott)%nolay_var)
        else
            nolaymax = nlayb
        endif
        allocate(botmatrix(nsegb, nolaymax))
        botmatrix = 0
        isegb = 0
        do ilay = 1, nolaymax
            do ib = 1, nsegb
                if (space_var_nolay) then
                    if (ilay <= GridPs%Pointers(JBott)%nolay_var(ib)) then
                        isegb = isegb + 1
                        botmatrix(ib, ilay) = isegb
                    endif
                else
                    isegb = isegb + 1
                    botmatrix(ib, ilay) = isegb
                endif
            enddo
        enddo

        !        sorted after bottom segment number !!

        if (ioutpt < 4) write (lunut, 1000)
        ioff1 = (nlay - 1) * nsegl
        ioff2 = max((nlay - 2) * nsegl, 0)
        iqt = noq
        write (lunut, *) ' nsegb: ', nsegb
        do isegb = 1, nsegb

            if (space_var_nolay) nlayb = GridPs%Pointers(JBott)%nolay_var(isegb)
            ib = botmatrix(isegb, 1)

            !              header for water-bottom

            if (ioutpt >= 4) then
                write (lunut, 1010) ib, noseg + ib
                write (lunut, 1030)
            endif

            if (nlayb > 1) then
                inaarplus = botmatrix(isegb, 2) + noseg
            else
                inaarplus = -nobnd - ib
            endif

            !              get every pointer for this bottom cell

            iq = 0
            do i = 1, nsegl          ! from water towards the bottom
                if (GridPs%Pointers(JBott)%iarray(i) == ib) then
                    iq = iq + 1
                    ipoint(1, iq + iqt) = ioff1 + i
                    ipoint(2, iq + iqt) = ib + noseg
                    ipoint(3, iq + iqt) = ioff2 + i
                    ipoint(4, iq + iqt) = inaarplus
                    if (ioutpt >= 4) write(lunut, 1040)iq + iqt, (ipoint(k, iq + iqt), k = 1, 4)
                endif
            end do
            !              header within the bottom
            if (ioutpt >= 4) then
                write (lunut, 1020)
                write (lunut, 1030)
            endif

            do ilay = 1, nlayb     ! from bottom to next bottom layer
                iq = iq + 1            ! the number of the pointer

                !           from pointer

                ipoint(1, iq + iqt) = botmatrix(isegb, ilay) + noseg

                !           to pointer

                if (ilay  < nlayb) then   ! 'to'  can be boundary
                    ipoint(2, iq + iqt) = botmatrix(isegb, ilay + 1) + noseg
                else
                    ipoint(2, iq + iqt) = -ib - nobnd
                    IAbnd(ib, 1) = iq + iqt
                    IAbnd(ib, 2) = ipoint(1, iq + iqt)
                endif

                !           from-1

                if (ilay == 1) then
                    ipoint(3, iq + iqt) = ipoint(1, iq + iqt)
                else
                    ipoint(3, iq + iqt) = botmatrix(isegb, ilay - 1) + noseg
                endif

                !           to+1

                if (ilay < nlayb - 1) then ! 'to+1'  can be boundary
                    ipoint(4, iq + iqt) = botmatrix(isegb, ilay + 2) + noseg
                else
                    ipoint(4, iq + iqt) = -ib - nobnd
                endif
                if (ioutpt >= 4) write(lunut, 1040)iq + iqt, (ipoint(k, iq + iqt), k = 1, 4)

            end do
            !              copy the column
            do i = 1, iq
                ipoint(1, iq + iqt + i) = ipoint(1, iqt + i)
                ipoint(2, iq + iqt + i) = ipoint(2, iqt + i)
                ipoint(3, iq + iqt + i) = ipoint(3, iqt + i)
                ipoint(4, iq + iqt + i) = ipoint(4, iqt + i)
            end do
            iqt = iqt + 2 * iq
        end do
        if (noqt /= iqt) then
            write (lunut, 1110) noq4, iqt - noq
            call status%increase_error_count()
            goto 9999
        endif
        write (lunut, 1060) nsegb
        odd = .true.
        if (ioutpt >= 3) then
            write (lunut, 1070)
            do iq = noq + 1, noq + noq4
                if (ipoint(1, iq) < 0 .or. &
                        ipoint(2, iq) < 0) then
                    ib = min (ipoint(1, iq), ipoint(2, iq))
                    if (odd) then
                        write (lunut, 1080) ib, iq, (ipoint(k, iq), k = 1, 2)
                        odd = .false.
                    else
                        odd = .true.
                    endif
                endif
            enddo
        else
            write (lunut, 1090)
        endif
        write (lun(8)) ((ipoint(i, iq), i = 1, 4), iq = noq + 1, iqt)
        write (lunut, 1100)

        !     Write boundary pointers to work file

        if (nobnd > 0 .or. nsegb > 0) then
            write (lun(2)) (ibnd (k, 1), k = 1, nobnd), (iabnd(k, 1), k = 1, nsegb)
            write (lun(2)) (ibnd (k, 2), k = 1, nobnd), (iabnd(k, 2), k = 1, nsegb)
        endif
        deallocate (iabnd)
        nobnd = nobnd + nsegb

        9999 if (timon) call timstop(ithndl)
        return

        !       Output formats

        1000 FORMAT (/ ' Exchange pointers are printed for output option 4 and higher !')
        1010 FORMAT (/, '     Additional exchanges between water and ' &
                'bottom at bottom segment:', I10, ' WAQ:', I10)
        1020 FORMAT (/, '     Additional exchanges within the bottom:')
        1030 FORMAT ('   Item nr.  From      To  From-1    To+1')
        1040 FORMAT (5I8)
        1050 FORMAT (/, ' ERROR. No bottom grid information found')
        1060 FORMAT (/, ' Number of additional bottom boundaries  :', I4, ' times 2 !')
        1070 FORMAT (' boundary  exchange    from        to'/ &
                '  number    number    segment    segment')
        1080 FORMAT (I7, 3I10)
        1090 FORMAT (' exchanges with open boundaries are printed for', &
                ' output option 3 and higher !')
        1100 FORMAT (' all bottom exchanges are duplicated.')
        1110 FORMAT (/' ERROR, Theoretical number of bottom exchanges:', I10, &
                /'        does not match number in practice     :', I10)

    END

end module m_pointb
