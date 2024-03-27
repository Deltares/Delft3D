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
module m_dlwq06
    use m_waq_precision
    use m_string_utils
    use m_opt1
    use m_opt0
    use m_dlwq5a
    use m_error_status

    implicit none

contains


    subroutine dlwq06 (lun, lchar, filtype, icmax, car, &
            iimax, iar, irmax, rar, notot, &
            noseg, sname, nowst, nowtyp, nrftot, &
            nrharm, dtflg1, dtflg3, iwidth, &
            ioutpt, chkpar, status)

        !       Deltares Software Centre

        !>\file
        !>                          Reads all inputs associated with waste loads and withdrawals
        !>
        !>                          This routine reads:
        !>                             - the number of wasteloads and withdrawals
        !>                             - the wasteloads ID's and volume numbers (and names and types for modern files)
        !>                             - the wasteload concentration/mass values

        !     Created            : April    1988 by Marjolein Sileon and Leo Postma

        !     Modified           : April    1997 by Rinze Bruinsma  : Tokenized input data file reading added
        !                          July     2002 by Leo Postma      : Call to Opt1 changed.
        !                          December 2010 by Leo Postma      : Addition diffuse sources
        !                                                             Fortran 90 style
        !                                                             simpler call to tokenized input

        !     Function           : Reads waste loads

        !     Subroutines called : opt0     : previous versions input processing (one matrix does all)
        !                          gettoken : tokenized data input
        !                          srstop   : stop after error with return code
        !                          zoek     : search for presence of a string
        !                          dlwq5a   : modern context sensitive input data processing
        !                          check    : check whether end of data block is encountred correctly

        !     Logical units :      lun(27) = unit DELWAQ input file
        !                          lun(29) = unit formatted output file
        !                          lun( 2) = unit intermediate file (system)
        !                          lun( 3) = unit intermediate file (harmonics)
        !                          lun( 4) = unit intermediate file (pointers)
        !                          lun(15) = unit intermediate file (waste load)

        use m_check
        use m_srstop
        use rd_token
        use timers       !   performance timers
        implicit none

        !     Arguments:

        integer(kind = int_wp), intent(inout) :: lun    (:)      !< array with unit numbers
        character(*), intent(inout) :: lchar  (:)     !< Filenames for the items
        integer(kind = int_wp), intent(inout) :: filtype(*)      !< type of binary files
        integer(kind = int_wp), intent(in) :: icmax           !< size of the character workspace
        character(20), intent(inout) :: car   (icmax)  !< local character workspace
        integer(kind = int_wp), intent(in) :: iimax           !< size of the integer   workspace
        integer(kind = int_wp), intent(inout) :: iar   (iimax)   !< local integer   workspace
        integer(kind = int_wp), intent(in) :: irmax           !< size of the real      workspace
        real(kind = real_wp), intent(inout) :: rar   (irmax)   !< local real      workspace
        integer(kind = int_wp), intent(in) :: notot           !< total number of substances
        integer(kind = int_wp), intent(in) :: noseg           !< number of computational volumes
        character(20), intent(inout) :: sname (notot)  !< IDs of the substances
        integer(kind = int_wp), intent(out) :: nowst           !< number of waste loads
        integer(kind = int_wp), intent(out) :: nowtyp          !< number of waste load types
        integer(kind = int_wp), intent(inout) :: nrftot(11)    !< number of function items per kind
        integer(kind = int_wp), intent(inout) :: nrharm(11)    !< number of harmonic items per kind
        logical, intent(in) :: dtflg1         !< if true then 'date'-format for 2nd time scale
        logical, intent(in) :: dtflg3         !< 'date'-format (F;ddmmhhss,T;yydddhh)
        integer(kind = int_wp), intent(in) :: iwidth          !< width of the output file
        integer(kind = int_wp), intent(in) :: ioutpt          !< Degree of output in report file
        logical, intent(out) :: chkpar(2)     !< Check for parameters SURF and LENGTH

        type(error_status), intent(inout) :: status !< current error status

        !     Locals

        character(40)                 chulp       (3) !  Help for reading
        character(255)                 cdummy          !  Help for reading
        character(20), allocatable :: wstid       (:) !  wasteload id's 20 character
        character(40), allocatable :: wstname     (:) !  wasteload names
        character(20), allocatable :: wsttype     (:) !  wasteload types
        character(256), allocatable :: wstid_long  (:) !  array to buffer the non truncated wasteload id's
        character(256), allocatable :: wsttype_long(:) !  array to buffer the non truncated wasteload types
        integer(kind = int_wp), allocatable :: iwstseg     (:)  !  wasteload segment
        integer(kind = int_wp), allocatable :: iwsttype    (:)  !  index wasteload type
        integer(kind = int_wp), allocatable :: iwstkind    (:)  !  kind array: 0 = pres: use present situation
        !              1 = mass: use data as mass/sec      even with flow > 0
        !              2 = conc: use data as concentration even with flow = 0
        !              3 = rain: flow >= 0 use concentration, < 0 use 0.0
        !              4 = well: flow >= 0 use concentration, < 0 use model-C
        real(kind = dp), allocatable :: drar        (:)  !  double precission workspace (very large !lp)
        integer(kind = int_wp) :: lunwr            !  binary unit for wasteloads
        integer(kind = int_wp) :: itype            !  type of token that is read
        integer(kind = int_wp) :: ierr2            !  local error indicator
        integer(kind = int_wp) :: ierr_alloc       !  local error indicator for allocation
        integer(kind = int_wp) :: i                !  loop counter
        integer(kind = int_wp) :: ifound           !  help variable in searches
        integer(kind = int_wp) :: ifound2          !  help variable in searches
        logical                        ldummy          !  dummy logical
        integer(kind = int_wp) :: idummy           !  dummy integer
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("dlwq06", ithndl)

        !     Init

        lunwr = lun(2)
        iposr = 0
        chkpar = .false.
        nowtyp = 0

        !        Read number of waste loads

        if (gettoken(nowst, ierr2) > 0) goto 20
        if (nowst < 0) then       !   it says that info comes from auxiliary file
            write (lunut, 2000) nowst
            call opt1   (-1, lun, 15, lchar, filtype, &
                    dtflg1, dtflg3, 0, ierr2, status, &
                    .false.)
            if (ierr2 > 0) goto 20
            if (gettoken(nowst, ierr2) > 0) goto 20
        endif
        if (nowst == 0) then
            write (lunut, 2010)
            goto 20
        endif

        !     read waste names, from version 4.9 on names are ID's
        !                                           names are 40 characters
        !                                           types are 20 characters
        allocate (wstid     (nowst), wsttype     (nowst), wstname(nowst), &
                wstid_long(nowst), wsttype_long(nowst), iwstseg(nowst), &
                iwsttype  (nowst), iwstkind    (nowst), stat = ierr_alloc)
        if (ierr_alloc /= 0) then
            write (lunut, 2160) ierr_alloc
            write (lunut, 2040) nowst
            call SRSTOP(1)
        endif
        write (lunut, 2040) nowst
        if (ioutpt < 3) then
            write (lunut, 2045)
        else
            if (iwidth == 5) then
                write (lunut, 2050)
            else
                write (lunut, 2060)
            endif
        endif

        !       read wasteload identification

        do i = 1, nowst

            iwsttype(i) = 0
            iwstkind(i) = 0
            if (gettoken(chulp(1), iwstseg(i), itype, ierr2) > 0) goto 20
            if (itype == 1) then                  !    character, either SURFACE, BANK or BOTTOM
                iwstseg(i) = 0
                if (chulp(1) == "SURFACE") iwstseg(i) = -1; iwstkind(i) = 2 ! e.g. atmospheric deposition
                if (chulp(1) == "BANK") iwstseg(i) = -2; iwstkind(i) = 2 ! e.g. bank infiltration, 1D river systems
                if (chulp(1) == "BED") iwstseg(i) = -3; iwstkind(i) = 2 ! e.g. well and sink
                if (iwstseg(i) == 0) then
                    ierr2 = 1
                    goto 20
                endif

                if (iwstseg(i) == -1 .or. iwstseg(i) == -3) then
                    chkpar(1) = .true.
                endif
                if (iwstseg(i) == -2) then
                    chkpar(2) = .true.
                endif
            endif
            if (gettoken(wstid_long(i), ierr2) > 0) goto 20
            select case (wstid_long(i))
            case ("MASS")
                iwstkind(i) = 1
                if (gettoken(wstid_long(i), ierr2) > 0) goto 20
            case ("CONC")
                iwstkind(i) = 2
                if (gettoken(wstid_long(i), ierr2) > 0) goto 20
            case ("RAIN")
                iwstkind(i) = 3
                if (gettoken(wstid_long(i), ierr2) > 0) goto 20
            case ("WELL")
                iwstkind(i) = 4
                if (gettoken(wstid_long(i), ierr2) > 0) goto 20
            end select

            if (gettoken(wstname     (i), ierr2) > 0) goto 20
            if (gettoken(wsttype_long(i), ierr2) > 0) goto 20

            if (wstid_long(i)  == ' ') write (wstid_long(i), '(''waste-load id'',i7)') i
            if (wstname(i)     == ' ') write (wstname(i), '(''waste-load name '',i7)') i
            if (wsttype_long(i)== ' ') wsttype_long(i) = 'waste-load type 1'

            wstid(i) = wstid_long(i)
            wsttype(i) = wsttype_long(i)

            if (ioutpt >= 3) then
                if (iwidth == 5) then
                    if (iwstseg(i) > 0) &
                            write (lunut, 2070) i, iwstseg(i), iwstkind(i), wstid(i), wstname(i), wsttype(i)
                    if (iwstseg(i) == -1) &
                            write (lunut, 2075) i, "SURFACE", iwstkind(i), wstid(i), wstname(i), wsttype(i)
                    if (iwstseg(i) == -2) &
                            write (lunut, 2075) i, "BANK   ", iwstkind(i), wstid(i), wstname(i), wsttype(i)
                    if (iwstseg(i) == -3) &
                            write (lunut, 2075) i, "BED    ", iwstkind(i), wstid(i), wstname(i), wsttype(i)
                else
                    if (iwstseg(i) > 0) &
                            write (lunut, 2080) i, iwstseg(i), iwstkind(i), wstid(i), wstname(i), wsttype(i)
                    if (iwstseg(i) == -1) &
                            write (lunut, 2085) i, "SURFACE", iwstkind(i), wstid(i), wstname(i), wsttype(i)
                    if (iwstseg(i) == -2) &
                            write (lunut, 2085) i, "BANK   ", iwstkind(i), wstid(i), wstname(i), wsttype(i)
                    if (iwstseg(i) == -3) &
                            write (lunut, 2085) i, "BED    ", iwstkind(i), wstid(i), wstname(i), wsttype(i)
                endif
            endif

            !          check for unique ID, error if non-truncated ID is unique otherwise warning

            ifound = index_in_array(wstid(i), wstid(:i - 1))
            if (ifound > 0) then
                ifound2 = index_in_array(wstid_long(i), wstid_long(:i - 1))
                if (ifound == ifound2) then
                    write(lunut, 2130) wstid(i)
                    call status%increase_warning_count()
                else
                    write(lunut, 2140) wstid(i)
                    call status%increase_error_count()
                endif
            endif

            !          check if truncated type and non truncated type give the same number

            ifound = index_in_array(wsttype(i), wsttype(:nowtyp))
            ifound2 = index_in_array(wsttype_long(i), wsttype_long(:nowtyp))
            if (ifound /= ifound2) then
                write(lunut, 2150) trim(wsttype_long(i))
                call status%increase_error_count()
            endif

            !          if type found set type, otherwise add type

            if (ifound > 0) then
                iwsttype(i) = ifound
            else
                nowtyp = nowtyp + 1
                wsttype(nowtyp) = wsttype(i)
                wsttype_long(nowtyp) = wsttype_long(i)
                iwsttype(i) = nowtyp
            endif

            !          check segment number

            if (iwstseg(i) < -3 .or.  iwstseg(i) > noseg .or. &
                    iwstseg(i) ==  0) then
                write (lunut, 2090) iwstseg(i)
                call status%increase_error_count()
            endif

            !          write ID and name to system file

            write (lunwr)  iwstseg(i), iwstkind(i), wstid(i), wstname(i)

        end do

        !          provide information about the special parameters

        if (chkpar(1)) then
            write(lunut, 2210)
        endif
        if (chkpar(2)) then
            write(lunut, 2220)
        endif


        !          give list of all identified wasteload types and write info to system file

        write (lunut, *)
        write (lunut, 2110) nowtyp
        if (ioutpt < 2) then
            write (lunut, 2115)
        else
            write (lunut, 2112)
            do i = 1, nowtyp
                write (lunut, 2120) i, wsttype(i)
            enddo
            write (lunut, *)
        endif
        write (lunwr)  (wsttype(i), i = 1, nowtyp)
        write (lunwr)  (iwsttype(i), i = 1, nowst)

        !          these arrays are not needed further

        deallocate(wstname, wstid_long, wsttype_long, iwstseg, iwsttype)

        !          now get the values

        allocate(drar(irmax))             ! this array is 100 mb lp
        idummy = notot + 1
        call dlwq5a (lun, lchar, 15, iwidth, icmax, &
                car, iimax, iar, irmax, rar, &
                sname, wstid, wsttype, nowst, idummy, &
                nowtyp, drar, dtflg1, dtflg3, &
                ioutpt, ierr2, status)
        deallocate(drar)
        if (ierr2 ==  0) then
            deallocate(wstid, wsttype)
            goto 30
        endif
        deallocate(wstid, wsttype)
        call status%increase_error_count_with(ierr2)
        ierr2 = 0

        !     error processing

        20 if (ierr2 > 0) call status%increase_error_count()      !   if 2, end of block reached
        if (ierr2 == 3) call SRSTOP(1)        !   end of file reached
        call check  (cdummy, iwidth, 6, ierr2, status)
        30 if (timon) call timstop(ithndl)
        return

        !       Output formats

        2000 format (/, ' First  selected option   : ', I7)
        2010 format (/, ' No waste loads ')
        2040 format (/, ' Number of waste loads: ', I4//)
        2045 format (/, ' Names of waste loads are printed for', &
                ' output option 3 and higher !')
        2050 format ('Number  segment knd wasteload ID          '/ &
                20X, 'wasteload name      ', 22X, 'wasteload type')
        2060 format ('Number  segment knd wasteload ID          ', &
                'wasteload name      ', 22X, 'wasteload type')
        2070 format (I6, 2X, I7, 2X, I1, 2X, A20/20X, A40, 2X, A20)
        2075 format (I6, 2X, A7, 2X, I1, 2X, A20/20X, A40, 2X, A20)
        2080 format (I6, 2X, I7, 2X, I1, 2X, A20, 2X, A40, 2X, A20)
        2085 format (I6, 2X, A7, 2X, I1, 2X, A20, 2X, A40, 2X, A20)
        2090 format (' ERROR invalid segment number:', I8)
        2110 format (' Number of different wasteload types: ', I4)
        2112 format (' Type:  Type-string')
        2115 format (' Waste load types are printed for output option', &
                ' 2 or higher !')
        2120 format (I6, 2X, A20)
        2130 format (' WARNING: wasteload ID is not unique:', A)
        2140 format (' ERROR: truncated wasteload ID is not unique:', A)
        2150 format (' ERROR: truncated wasteload type not unique:', A)
        2160 format (' ERROR: allocating wasteload arrays:', I8)
        2210 format (' Note: one or more special waste loads - parameter/se &
                gment function SURF required')
        2220 format (' Note: one or more special waste loads - parameter/se &
                gment function LENGTH required')

    end

end module m_dlwq06
