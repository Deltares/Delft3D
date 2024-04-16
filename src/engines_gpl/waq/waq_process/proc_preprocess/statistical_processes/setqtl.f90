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
module m_setqtl
    use m_waq_precision
    use m_string_utils
    use m_error_status

    implicit none

contains


    SUBROUTINE SETQTL (LUNREP, NOKEY, &
            KEYNAM, KEYVAL, &
            PERNAM, PERSFX, &
            PSTART, PSTOP, &
            IPROC, aProcesProp, &
            AllItems, status)
        !
        !     Deltares
        !
        !     CREATED:            : februari 2002 by Jan van Beek
        !
        !     FUNCTION            : Sets io list for statistical routine STAQTL
        !
        !     SUBROUTINES CALLED  : SRSTOP, stops execution
        !                           ZOEK  , finds string in character array
        !
        !
        !     PARAMETERS          :
        !
        !     NAME    KIND      LENGTH  FUNCT.  DESCRIPTION
        !     ----    -----     ------  ------- -----------
        !     LUNREP  INTEGER(kind=int_wp) ::1  INPUT   unit number report file
        !     NOKEY   INTEGER(kind=int_wp) ::1  INPUT   number of keywords for this process
        !     KEYNAM  CHAR*20    NOKEY  INPUT   keyword name
        !     KEYVAL  CHAR*20    NOKEY  INPUT   keyword value
        !     PERNAM  CHAR*20        1  INPUT   period name
        !     PERSFX  CHAR*20        1  INPUT   period suffix
        !     PSTART  INTEGER(kind=int_wp) ::1  INPUT   period start
        !     PSTOP   INTEGER(kind=int_wp) ::1  INPUT   period stop
        !     aProcesProp               OUTPUT  properties for this proces
        !     AllItems                  INPUT   all items known to the proces system
        !
        use m_srstop
        use m_string_manipulation, only : get_trimmed_length
        USE ProcesSet
        use timers       !   performance timers
        !
        IMPLICIT NONE
        !
        !     Declaration of arguments
        !
        INTEGER(kind = int_wp) :: LUNREP, NOKEY, PSTART, PSTOP, IPROC
        character(len=20)  PERNAM, PERSFX
        character(len=20)  KEYNAM(NOKEY), KEYVAL(NOKEY)
        type(ProcesProp) :: aProcesProp         ! output statistical proces definition
        type(ItemPropColl) :: AllItems            ! all items of the proces system
        type(error_status), intent(inout) :: status !< current error status
        !
        !     Local declarations
        !
        INTEGER(kind = int_wp) :: IERR_ALLOC, IKEY, ISTART, ISTOP, ISLEN, &
                IERR2, IRET
        INTEGER(kind = int_wp), ALLOCATABLE :: ISUSED(:)
        character(len=20)  SUFFIX
        character(len=10)  STANAM
        REAL(kind = real_wp) :: CLOBND, CUPBND, CQLEV
        INTEGER(kind = int_wp) :: NOBUCK, IBUCK
        type(ItemProp) :: aItemProp            ! one item
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("setqtl", ithndl)
        !
        !     init
        !
        ALLOCATE(ISUSED(NOKEY), STAT = IERR_ALLOC)
        IF (IERR_ALLOC /= 0) THEN
            WRITE(LUNREP, *) 'ERROR allocating buffer array:', IERR_ALLOC
            WRITE(LUNREP, *) 'in routine SETQTL_3, buffer length:', NOKEY
            WRITE(*, *) 'ERROR allocating buffer array:', IERR_ALLOC
            CALL SRSTOP(1)
        ENDIF
        ISUSED = 0
        IKEY = index_in_array('OUTPUT-OPERATION', KEYNAM)
        IF (IKEY > 0) THEN
            ISUSED(IKEY) = 1
        ENDIF
        !
        !     Fill the Propces Properties
        !
        aProcesProp%name = 'STAQTL'
        WRITE(aProcesProp%name(7:10), '(I4.4)') IPROC
        STANAM = aProcesProp%name(1:10)
        aProcesProp%routine = 'STAQTL'
        aProcesProp%text = 'quantiles (concentration percentiles)'
        aProcesProp%swtransp = 123
        aProcesProp%type = PROCESTYPE_STAT
        !
        !     get the number of buckets to determine the amount of IO items
        !     actual number of buckets is one larger, bucket00 the rest bucket for values below CLOBND
        !
        IKEY = index_in_array('NOBUCK', KEYNAM)
        IF (IKEY <= 0) THEN
            NOBUCK = 10
        ELSE
            ISUSED(IKEY) = 1
            READ(KEYVAL(IKEY), '(I20.0)', IOSTAT = IERR2) NOBUCK
            IF (IERR2 /= 0) THEN
                WRITE(LUNREP, *)'ERROR interpreting number of buckets:', &
                        KEYVAL(IKEY)
                call status%increase_error_count()
                NOBUCK = 10
            ENDIF
        ENDIF
        !
        aProcesProp%no_input = 10 + nobuck + 1
        aProcesProp%no_output = 2 + nobuck + 1
        aProcesProp%no_FluxOutput = 0
        aProcesProp%no_FluxStochi = 0
        aProcesProp%no_DispStochi = 0
        aProcesProp%no_VeloStochi = 0
        ALLOCATE(aProcesProp%input_item(aProcesProp%no_input), &
                aProcesProp%output_item(aProcesProp%no_output), &
                STAT = IERR_ALLOC)
        IF (IERR_ALLOC /= 0) THEN
            WRITE(LUNREP, *) 'ERROR allocating IOitem array:', IERR_ALLOC
            WRITE(LUNREP, *) 'in routine SETDAY_1, array length:', aProcesProp%no_input, aProcesProp%no_output
            WRITE(*, *) 'ERROR allocating array:', IERR_ALLOC
            CALL SRSTOP(1)
        ENDIF
        !
        !     input on segments
        !
        IKEY = index_in_array('SUBSTANCE', KEYNAM)
        IF (IKEY <= 0) THEN
            WRITE(LUNREP, *) 'ERROR no parameter specified for statistics'
            call status%increase_error_count()
        ELSE
            ISUSED(IKEY) = 1
            aProcesProp%input_item(1)%name = KEYVAL(IKEY)
            aProcesProp%input_item(1)%type = IOTYPE_SEGMENT_INPUT
            aProcesProp%input_item(1)%actdef = -999.
            aProcesProp%input_item(1)%indx = 1
            aProcesProp%input_item(1)%ip_val = 0
            aItemProp%name = KEYVAL(IKEY)
            iret = ItemPropCollFind(AllItems, aItemProp)
            if (iret <= 0) then
                aItemProp%text = 'input parameter for statistics'
                aItemProp%default = -999.
                aItemProp%waqtype = WAQTYPE_NONE
                iret = ItemPropCollAdd(AllItems, aItemProp)
            endif
            aProcesProp%input_item(1)%item => AllItems%ItemPropPnts(iret)%pnt
        ENDIF
        !
        aItemProp%name = 'START     ' // aProcesProp%name(1:10)
        aItemProp%default = PSTART
        aItemProp%text = 'start of statistic output period'
        aItemProp%waqtype = WAQTYPE_NONE
        iret = ItemPropCollAdd(AllItems, aItemProp)
        aProcesProp%input_item(2)%name = aItemProp%name
        aProcesProp%input_item(2)%type = IOTYPE_SEGMENT_INPUT
        aProcesProp%input_item(2)%item => AllItems%ItemPropPnts(iret)%pnt
        aProcesProp%input_item(2)%actdef = PSTART
        aProcesProp%input_item(2)%indx = 2
        aProcesProp%input_item(2)%ip_val = 0
        !
        aItemProp%name = 'STOP      ' // aProcesProp%name(1:10)
        aItemProp%default = PSTOP
        aItemProp%text = 'stop of statistic output period'
        aItemProp%waqtype = WAQTYPE_NONE
        iret = ItemPropCollAdd(AllItems, aItemProp)
        aProcesProp%input_item(3)%name = aItemProp%name
        aProcesProp%input_item(3)%type = IOTYPE_SEGMENT_INPUT
        aProcesProp%input_item(3)%item => AllItems%ItemPropPnts(iret)%pnt
        aProcesProp%input_item(3)%actdef = PSTOP
        aProcesProp%input_item(3)%indx = 3
        aProcesProp%input_item(3)%ip_val = 0
        !
        aItemProp%name = 'ITIME'
        iret = ItemPropCollFind(AllItems, aItemProp)
        if (iret <= 0) then
            aItemProp%default = -999.
            aItemProp%text = 'time in calculation'
            aItemProp%waqtype = WAQTYPE_DEFAULT
            iret = ItemPropCollAdd(AllItems, aItemProp)
        endif
        aProcesProp%input_item(4)%name = aItemProp%name
        aProcesProp%input_item(4)%type = IOTYPE_SEGMENT_INPUT
        aProcesProp%input_item(4)%item => AllItems%ItemPropPnts(iret)%pnt
        aProcesProp%input_item(4)%actdef = -999.
        aProcesProp%input_item(4)%indx = 4
        aProcesProp%input_item(4)%ip_val = 0
        !
        aItemProp%name = 'IDT'
        iret = ItemPropCollFind(AllItems, aItemProp)
        if (iret <= 0) then
            aItemProp%default = -999.
            aItemProp%text = 'time step'
            aItemProp%waqtype = WAQTYPE_DEFAULT
            iret = ItemPropCollAdd(AllItems, aItemProp)
        endif
        aProcesProp%input_item(5)%name = aItemProp%name
        aProcesProp%input_item(5)%type = IOTYPE_SEGMENT_INPUT
        aProcesProp%input_item(5)%item => AllItems%ItemPropPnts(iret)%pnt
        aProcesProp%input_item(5)%actdef = -999.
        aProcesProp%input_item(5)%indx = 5
        aProcesProp%input_item(5)%ip_val = 0
        !
        aItemProp%name = 'NOBUCK    ' // aProcesProp%name(1:10)
        aItemProp%default = NOBUCK
        aItemProp%text = 'number of bucktes'
        aItemProp%waqtype = WAQTYPE_NONE
        iret = ItemPropCollAdd(AllItems, aItemProp)
        aProcesProp%input_item(6)%name = aItemProp%name
        aProcesProp%input_item(6)%type = IOTYPE_SEGMENT_INPUT
        aProcesProp%input_item(6)%item => AllItems%ItemPropPnts(iret)%pnt
        aProcesProp%input_item(6)%actdef = NOBUCK
        aProcesProp%input_item(6)%indx = 6
        aProcesProp%input_item(6)%ip_val = 0
        !
        IKEY = index_in_array('CLOBND', KEYNAM)
        IF (IKEY <= 0) THEN
            CLOBND = 0.0
        ELSE
            ISUSED(IKEY) = 1
            READ(KEYVAL(IKEY), '(F20.0)', IOSTAT = IERR2) CLOBND
            IF (IERR2 /= 0) THEN
                WRITE(LUNREP, *)'ERROR lower boundary:', &
                        KEYVAL(IKEY)
                call status%increase_error_count()
            ENDIF
        ENDIF
        aItemProp%name = 'CLOBND    ' // aProcesProp%name(1:10)
        aItemProp%default = CLOBND
        aItemProp%text = 'lower boundary'
        aItemProp%waqtype = WAQTYPE_NONE
        iret = ItemPropCollAdd(AllItems, aItemProp)
        aProcesProp%input_item(7)%name = aItemProp%name
        aProcesProp%input_item(7)%type = IOTYPE_SEGMENT_INPUT
        aProcesProp%input_item(7)%item => AllItems%ItemPropPnts(iret)%pnt
        aProcesProp%input_item(7)%actdef = CLOBND
        aProcesProp%input_item(7)%indx = 7
        aProcesProp%input_item(7)%ip_val = 0
        !
        IKEY = index_in_array('CUPBND', KEYNAM)
        IF (IKEY <= 0) THEN
            CUPBND = 0.0
        ELSE
            ISUSED(IKEY) = 1
            READ(KEYVAL(IKEY), '(F20.0)', IOSTAT = IERR2) CUPBND
            IF (IERR2 /= 0) THEN
                WRITE(LUNREP, *)'ERROR upper boundary:', &
                        KEYVAL(IKEY)
                call status%increase_error_count()
            ENDIF
        ENDIF
        aItemProp%name = 'CUPBND    ' // aProcesProp%name(1:10)
        aItemProp%default = CUPBND
        aItemProp%text = 'upper boundary'
        aItemProp%waqtype = WAQTYPE_NONE
        iret = ItemPropCollAdd(AllItems, aItemProp)
        aProcesProp%input_item(8)%name = aItemProp%name
        aProcesProp%input_item(8)%type = IOTYPE_SEGMENT_INPUT
        aProcesProp%input_item(8)%item => AllItems%ItemPropPnts(iret)%pnt
        aProcesProp%input_item(8)%actdef = CUPBND
        aProcesProp%input_item(8)%indx = 8
        aProcesProp%input_item(8)%ip_val = 0
        !
        IKEY = index_in_array('CQLEV', KEYNAM)
        IF (IKEY <= 0) THEN
            WRITE(LUNREP, *) 'ERROR quantile not specified'
            call status%increase_error_count()
            CQLEV = 0.0
        ELSE
            ISUSED(IKEY) = 1
            READ(KEYVAL(IKEY), '(F20.0)', IOSTAT = IERR2) CQLEV
            IF (IERR2 /= 0) THEN
                WRITE(LUNREP, *)'ERROR quantile:', &
                        KEYVAL(IKEY)
                call status%increase_error_count()
            ENDIF
        ENDIF
        aItemProp%name = 'CQLEV     ' // aProcesProp%name(1:10)
        aItemProp%default = CQLEV
        aItemProp%text = 'quantile'
        aItemProp%waqtype = WAQTYPE_NONE
        iret = ItemPropCollAdd(AllItems, aItemProp)
        aProcesProp%input_item(9)%name = aItemProp%name
        aProcesProp%input_item(9)%type = IOTYPE_SEGMENT_INPUT
        aProcesProp%input_item(9)%item => AllItems%ItemPropPnts(iret)%pnt
        aProcesProp%input_item(9)%actdef = CQLEV
        aProcesProp%input_item(9)%indx = 9
        aProcesProp%input_item(9)%ip_val = 0
        !
        aItemProp%name = 'TCOUNT    ' // aProcesProp%name(1:10)
        aItemProp%default = -999.
        aItemProp%text = 'time step counter (work array)'
        aItemProp%waqtype = WAQTYPE_NONE
        iret = ItemPropCollAdd(AllItems, aItemProp)
        aProcesProp%input_item(10)%name = aItemProp%name
        aProcesProp%input_item(10)%type = IOTYPE_SEGMENT_WORK
        aProcesProp%input_item(10)%item => AllItems%ItemPropPnts(iret)%pnt
        aProcesProp%input_item(10)%actdef = 0.0
        aProcesProp%input_item(10)%indx = 10
        aProcesProp%input_item(10)%ip_val = 0
        !
        IKEY = index_in_array('SUFFIX', KEYNAM)
        IF (IKEY <= 0) THEN
            !        something involving pcount ??
            SUFFIX = ' '
        ELSE
            SUFFIX = KEYVAL(IKEY)
            ISUSED(IKEY) = 1
        ENDIF
        CALL get_trimmed_length(SUFFIX, ISLEN)
        IF (SUFFIX(1:ISLEN) /= ' ') THEN
            SUFFIX = SUFFIX(1:ISLEN) // '_' // PERSFX
        ELSE
            SUFFIX = 'QUANT_' // PERSFX
        ENDIF
        CALL get_trimmed_length(SUFFIX, ISLEN)
        !
        aItemProp%name = SUFFIX(1:ISLEN) // '_' // aProcesProp%input_item(1)%name
        aItemProp%default = -999.
        aItemProp%text = 'Quantile ' // aProcesProp%input_item(1)%name
        aItemProp%waqtype = WAQTYPE_NONE
        iret = ItemPropCollAdd(AllItems, aItemProp)
        aProcesProp%output_item(1)%name = aItemProp%name
        aProcesProp%output_item(1)%type = IOTYPE_SEGMENT_OUTPUT
        aProcesProp%output_item(1)%item => AllItems%ItemPropPnts(iret)%pnt
        aProcesProp%output_item(1)%indx = 1
        aProcesProp%output_item(1)%ip_val = 0
        WRITE(LUNREP, 2000) 'Statistical output named [', aItemProp%name, &
                '] created with quantile average from [', aProcesProp%input_item(1)%name, ']'

        ! Add the companion for the TCOUNT input item
        aItemProp%name = 'TCOUNT    ' // aProcesProp%name(1:10)
        aItemProp%default = -999.
        aItemProp%text = 'time step counter (work array)'
        aItemProp%waqtype = WAQTYPE_NONE
        iret = ItemPropCollAdd(AllItems, aItemProp)
        aProcesProp%output_item(2)%name = aItemProp%name
        aProcesProp%output_item(2)%type = IOTYPE_SEGMENT_OUTPUT
        aProcesProp%output_item(2)%item => AllItems%ItemPropPnts(iret)%pnt
        aProcesProp%output_item(2)%indx = 2
        aProcesProp%output_item(2)%ip_val = 0
        !
        !     buckets input and output
        !
        DO IBUCK = 1, NOBUCK + 1
            aItemProp%name = 'BUCKET    ' // STANAM
            WRITE(aItemProp%name(7:10), '(I4.4)') IBUCK - 1
            aItemProp%default = -999.
            aItemProp%text = 'work array for quantile'
            aItemProp%waqtype = WAQTYPE_NONE
            iret = ItemPropCollAdd(AllItems, aItemProp)
            aProcesProp%input_item(10 + ibuck)%name = aItemProp%name
            aProcesProp%input_item(10 + ibuck)%type = IOTYPE_SEGMENT_WORK
            aProcesProp%input_item(10 + ibuck)%item => AllItems%ItemPropPnts(iret)%pnt
            aProcesProp%input_item(10 + ibuck)%actdef = -999.
            aProcesProp%input_item(10 + ibuck)%indx = 10 + ibuck
            aProcesProp%input_item(10 + ibuck)%ip_val = 0
            aProcesProp%output_item(2 + ibuck)%name = aItemProp%name
            aProcesProp%output_item(2 + ibuck)%type = IOTYPE_SEGMENT_WORK
            aProcesProp%output_item(2 + ibuck)%item => AllItems%ItemPropPnts(iret)%pnt
            aProcesProp%output_item(2 + ibuck)%indx = 2 + ibuck
            aProcesProp%output_item(2 + ibuck)%ip_val = 0
        ENDDO
        !
        !     check the use of the key words
        !
        DO IKEY = 1, NOKEY
            IF (ISUSED(IKEY) == 0) THEN
                call status%increase_warning_count()
                WRITE(LUNREP, *) 'WARNING: keyword not used'
                WRITE(LUNREP, *) 'key   :', KEYNAM(IKEY)
                WRITE(LUNREP, *) 'value :', KEYVAL(IKEY)
            ENDIF
        ENDDO
        !
        DEALLOCATE (ISUSED)
        !
        if (timon) call timstop(ithndl)
        RETURN
        2000 FORMAT(5A)
    END

end module m_setqtl
