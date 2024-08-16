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
module m_dlwqt4
    use m_waq_precision

    implicit none

contains


    subroutine dlwqt4 (file_unit_list, luntxt, ftype, lunout, ilun, &
            itime, result, ipoint, nosub, nrftot, &
            isflag, ifflag, update, ntotal, lstrec, &
            lrewin, reclst, dlwqd)

        !! Steps along in a time variable database and interpolates linearly.
        !                           At exit ITIME1 < ITIME-IDTIME < ITIME2
        !                           and     ARRAY1 -->  VALUES  <-- ARRAY2

        !     LOGICAL UNITNUMBERS : file_unit_list    - input unit intermediate file
        !                           LUNOUT - monitor file
        use m_open_waq_files
        use timers
        use delwaq2_data
        USE HydroSet           ! for composed hydrodynamics

        integer(kind = int_wp), intent(inout) :: file_unit_list(*)          ! logical unitnumbers of files
        character*(*), intent(in) :: luntxt(*)       ! file names
        integer(kind = int_wp), intent(in) :: ftype(*)        ! type of the files
        integer(kind = int_wp), intent(in) :: lunout          ! unit number monitor file
        integer(kind = int_wp), intent(in) :: ilun            ! entry in file_unit_list/LUNTXT for this item
        integer(kind = int_wp), intent(in) :: itime           ! Model timer
        integer(kind = int_wp), intent(in) :: nosub           ! number of functions for this item
        integer(kind = int_wp), intent(in) :: nrftot          ! record length (nosub*nopoints)
        real(kind = real_wp), intent(out) :: result(nosub, *) ! result array at ITIME
        integer(kind = int_wp), intent(in) :: ipoint(*)       ! pointer to result array
        integer(kind = int_wp), intent(in) :: isflag          ! = 1 then 'ddhhmmss' format
        integer(kind = int_wp), intent(in) :: ifflag          ! = 1 then first invocation
        logical, intent(out) :: update          ! set to T if function is updated
        integer(kind = int_wp), intent(in) :: ntotal          ! Length of result array NOT USED
        logical, intent(in) :: lstrec          ! True if last record on rewind wanted
        logical, intent(out) :: lrewin          ! True, then rewind took place
        real(kind = real_wp), intent(out) :: reclst(nosub, *) ! last record before rewind
        type(delwaq_data), intent(inout), target :: dlwqd           ! derived type for persistent storage

        !     local declarations        :

        type(FileProp) :: Prop            ! FileProperty
        type(FilePropPnt) :: PropPnt         ! Ponter to attach property to UseDef
        type(FilePropColl), pointer :: PropColl        ! Collection of them
        type(FileUseDef) :: UseDef          ! FileUseDefinition
        type(FileUseDefColl), pointer :: UseDefColl      ! Collection of them
        type(FileUseDefCollColl), pointer :: CollColl        ! Collection of collections

        !   NOTE: those not declared as pointer are all copies of the one in collections
        !         care should be taken if values within the collections are updated

        integer(kind = int_wp) :: ierr                              ! error indicator when opening a file
        integer(kind = int_wp) :: ioerr                             ! error indicator when opening a file
        integer(kind = int_wp) :: ilt, llun                         ! help variables for unit numbers
        integer(kind = int_wp) :: nfil                              ! number of files in the steering file
        integer(kind = int_wp) :: i, i2                             ! general loop variables
        real(kind = real_wp) :: fact                              ! factor for this file in the steering file
        integer(kind = int_wp) :: it1, it2, it3                   ! time in the steering file
        integer(kind = int_wp) :: it1a, it2a, it3a                  ! time in the steering file
        integer(kind = int_wp) :: iret                              ! general return code for functions
        integer(kind = int_wp) :: icoll                             ! number in the collection of collections
        integer(kind = int_wp) :: iup, igo, itt                     ! help variables
        integer(kind = int_wp) :: idef                              ! number of the file definition in a collection
        integer(kind = int_wp) :: idt                               ! time step size in a file
        integer(kind = int_wp) :: iset                              ! incremental counter in double loops
        integer(kind = int_wp) :: np                                ! number of items (nrftot/nosub)
        integer(kind = int_wp) :: ip                                ! item number
        logical        stream_access                     ! help variable to detect the type of file access
        character(20)  access                            ! help variable to detect the type of file access
        character(14)  strng                             ! help variable to detect th string 'Steering file '
        character(255) sfile                             ! help variable for file names
        logical        updat2                            ! help variable to determine updated record
        logical        lre3                              ! help variable to determine rewind
        integer(kind = int_wp), save :: islun            ! base unit number for work files
        integer(kind = int_wp) :: filtype                ! type of file
        real(kind = real_wp), pointer :: array1(:), array2(:), array3(:) ! help arrays for interpolation
        logical                :: first = .true.         ! construct for initialisation
        integer(kind = int_wp) :: ithandl = 0            ! performance timer construct
        if (timon) call timstrt ("dlwqt4", ithandl)

        lrewin = .false.
        lre3 = .false.
        if (nrftot == 0) goto 9999        ! no data to read

        propcoll => dlwqd%propcoll
        usedefcoll => dlwqd%usedefcoll
        collcoll => dlwqd%collcoll

        !         luns between 800 and 900 for binary functions and segfuns those files are already open

        if (ilun <= 800 .or. ilun >= 900) then
            ierr = 0
            if (ifflag == 1 .and. nrftot > 0) call open_waq_files (file_unit_list(ilun), luntxt(ilun), ilun, 2 + ftype(ilun), ierr)
            if (ierr /= 0) call messag (lunout, 5, isflag, file_unit_list(ilun), luntxt(ilun), itime, 0)
            ilt = ilun
            llun = file_unit_list(ilun)
        else
            ilt = 1
            llun = ilun
        endif

        !         if first time call for this routine initialise the top level collections

        IF (FIRST) THEN
            FIRST = .FALSE.
            ISLUN = 700                        ! this is the base unitnumber for the work files
            PropColl = FilePropColl      (NULL(), 0, 0)
            CollColl = FileUseDefCollColl(NULL(), 0, 0)
        ENDIF

        !         if first time call for this physical property, fill the description arrays

        IF (IFFLAG /= 0) THEN             ! make the FileUseDefColl for this physics
            UseDefColl = FileUseDefColl (NULL(), 0, 0, ILUN, 0, 0, 0, 0, nrftot, NULL(), NULL(), NULL())
            read (llun, iostat = ioerr) strng
            if (ioerr /= 0) strng = 'x'
            !               A steering file is present, then fill the compound structures
            if (strng(1:14) == 'Steering file ') then
                read (llun) nfil, UseDefColl%intopt  ! number of files and interpolation option
                do i = 1, nfil                         ! read all descriptions
                    read (llun) fact, it1, it2, it3, sfile, it1a, it2a, it3a
                    if (i == 1) then                ! to determine start and finish for the
                        UseDefColl%istart = IT1          ! whole collection. The collection as a
                        UseDefColl%istop = IT2          ! whole can also be rewound later
                    else
                        UseDefColl%istart = min(UseDefColl%istart, IT1)
                        UseDefColl%istop = max(UseDefColl%istop, IT2)
                    endif                               ! make a file property
                    Prop = FileProp (SFILE, ISLUN, IT1a, IT2a, IT3a, 0, 0, 0, -1, .false., NULL(), NULL())
                    iret = FilePropCollFind(PropColl, Prop)   ! See if it already exists
                    if (iret == 0) then             ! this is the first time for this file
                        filtype = 0
                        CALL open_waq_files (ISLUN, SFILE, 3, 2 + filtype, ierr)   ! open the file
                        IF (ierr /= 0) CALL MESSAG (LUNOUT, 5, ISFLAG, ISLUN, SFILE, ITIME, 0)
                        iret = FilePropCollAdd(PropColl, Prop, nrftot)   ! add a copy of this all to the collection
                        if (iret == 0) CALL MESSAG (LUNOUT, 4, ISFLAG, ISLUN, SFILE, ITIME, 0)
                        ISLUN = ISLUN + 1                 ! add one to the 700 group unitnumbers
                    endif
                    PropPnt = PropColl%FilePropPnts(iret)  ! take the property from the collection
                    UseDef = FileUseDef (PropPnt, FACT, IT1, IT2, IT3, .false.) ! Make the FileUseDefinition
                    iret = FileUseDefCollAdd (UseDefColl, UseDef)  ! Add a copy to the collection
                end do
                close (llun)                       ! Close binary description file
            ELSE
                !               A steering file is NOT present, then fill the structures with one description
                if (ipoint(1) > 0) UseDefColl%intopt = 1   !   linear interpolation

                inquire(llun, access = access)
                stream_access = access == 'STREAM'
                if (stream_access) then
                    read(llun, iostat = ierr, pos = 1)
                else
                    rewind llun                            ! Start at the beginning again
                endif

                Prop = FileProp (LUNTXT(ilt), LLUN, 0, 0, 0, 0, 0, 0, -1, stream_access, NULL(), NULL())
                iret = FilePropCollAdd(PropColl, Prop, nrftot)  ! UseDef endtime of zero means till end of simulation
                if (iret == 0) CALL MESSAG (LUNOUT, 4, ISFLAG, LLUN, LUNTXT(ilt), ITIME, 0)
                PropPnt = PropColl%FilePropPnts(iret)       ! take the property from the collection
                UseDef = FileUseDef (PropPnt, 1.0, PropPnt%pnt%itime1, 0, PropPnt%pnt%itime1, .false.)
                iret = FileUseDefCollAdd (UseDefColl, UseDef)
            ENDIF
            iret = FileUseDefCollCollAdd (CollColl, UseDefColl)  ! Add collection to the collections
        ENDIF

        !         Find the entry in the collection of collections for this physical property

        UPDATE = .FALSE.
        iColl = FileUseDefCollCollFind (CollColl, ILUN)   ! Find this collection in collection of collections
        UseDefColl = CollColl%FileUseDefColls(iColl)        ! Watch out, this is a copy
        array1 => CollColl%FileUseDefColls(iColl)%array1 ! and these are the original arrays
        array2 => CollColl%FileUseDefColls(iColl)%array2
        array3 => CollColl%FileUseDefColls(iColl)%array3
        10 continue
        array1 = 0.0
        array2 = 0.0
        iUp = 0
        iGo = 0
        iTt = 0
        do i = 1, UseDefColl%current_size   ! it is important to use the original here
            iDef = FileUseDefCollFind (CollColl%FileUseDefColls(iColl), i, ITIME, UPDAT2, LREWIN)
            IF (UPDAT2) UPDATE = .TRUE.
            if (iDef >= 0) then                         ! The arrays have been updated
                iUp = iUp + 1
                if (LREWIN .and. iDef /= 0)               & ! Signal the rewind
                        CALL MESSAG (LUNOUT, 1, ISFLAG, llun, &
                                UseDefColl%FileUseDefs(i)%afilePnt%pnt%name, ITIME, iDef)
            else                                            ! Before the range for this description
                iTt = min (iTt, iDef)
                iGo = iGo + 1
            endif
        end do

        if (iGo == UseDefColl % current_size)                & ! Before the range of all descriptions (simulation stops in MESSAG)
                CALL MESSAG (LUNOUT, 6, ISFLAG, UseDefColl%unitnr, &
                        UseDefColl%FileUseDefs(1)%afilePnt%pnt%name, ITIME, -iTt)

        if (UseDefColl%intopt == 2 .or. &
                UseDefColl%intopt == 3) then ! logarithmic averaging
            array1 = exp(array1)
            array2 = exp(array2)
        endif

        if (ITIME /= UseDefColl%istart .and. ITIME - UseDefColl%ioffset == UseDefColl%istop) then  ! exactly at the end of this collection
            array3 = array1                                 ! to save the last result for closure error correction
            iTt = ITIME - UseDefColl%ioffset                ! Rewind the whole collection of file defs
            iDt = UseDefColl%istop - UseDefColl%istart      ! This is the time span to increase the offset with
            UseDefColl%ioffset = UseDefColl%ioffset + iDt                  ! Update the offset of the copy used here
            CollColl%FileUseDefColls(iColl)%ioffset = UseDefColl%ioffset   ! Update the original as well !!
            do i = 1, UseDefColl%current_size        ! but now all file-offsets should be zero again
                CollColl%FileUseDefColls(iColl)%FileUseDefs(i)%afilePnt%pnt%ioffset = 0  ! reset the file offset of all associated files
            end do
            CALL MESSAG (LUNOUT, 1, ISFLAG, UseDefColl%unitnr, 'Compound file descriptor', ITIME, iTt)
            LREWIN = .FALSE.
            lre3 = .true.                      ! this logical is tested later
            goto 10               ! After rewind of the whole set, seek the new values
        endif

        if (lre3) then
            lrewin = .true.
            array1 = array3
        endif

        !         pointer assignment of the values itself

        iset = 1
        np = nrftot / nosub
        do i = 1, np
            ip = abs(ipoint(i))    ! pointer is negative for linear interpolation
            do i2 = 1, nosub
                if (lrewin .and. .not. lstrec) then
                    result(i2, ip) = array2(iset)
                else
                    result(i2, ip) = array1(iset)
                endif
                iset = iset + 1
            enddo
        enddo

        !     If wanted save last record in extra result

        if (lstrec .and. lrewin) then
            iset = 1           ! pointer assignment of the saved values of the last record
            do i = 1, np
                ip = abs(ipoint(i))   ! pointer is negative for linear interpolation
                do i2 = 1, nosub
                    reclst(i2, ip) = array2(iset)
                    iset = iset + 1
                enddo
            enddo
        endif

        9999 if (timon) call timstop (ithandl)

    END

    subroutine messag (lunout, messge, isflag, llun, sfile, itime, itime1)

        use timers
        use m_logger_helper, only: stop_with_error
        integer(kind = int_wp) :: lunout, isflag, llun, itime, itime1, messge

        character(len = 24)  MSGTXT(6)
        character(len = *) SFILE
        DATA MSGTXT / ' REWIND ON              ', ' WARNING READING        ', &
                ' REWIND ERROR           ', ' ERROR READING          ', &
                ' ERROR OPENING          ', ' ERROR: TIMES TOO LATE  ' /
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("messag", ithandl)

        if (messge == 0) goto 9999
        if (isflag == 1) then
            write(lunout, 2010) msgtxt(messge), llun, trim(sfile), &
                    itime / 86400, &
                    mod(itime, 86400) / 3600, &
                    mod(itime, 3600) / 60, &
                    mod(itime, 60), &
                    itime1 / 86400, &
                    mod(itime1, 86400) / 3600, &
                    mod(itime1, 3600) / 60, &
                    mod(itime1, 60)
        elseif (isflag == 2) then
            write(lunout, 2020) msgtxt(messge), llun, trim(sfile), &
                    itime / 31536000, &
                    mod(itime, 31536000) / 86400, &
                    mod(itime, 86400) / 3600, &
                    mod(itime, 3600) / 60, &
                    mod(itime, 60), &
                    itime1 / 31536000, &
                    mod(itime1, 31536000) / 86400, &
                    mod(itime1, 86400) / 3600, &
                    mod(itime1, 3600) / 60, &
                    mod(itime1, 60)
        else
            write(lunout, 2000) msgtxt(messge), llun, trim(sfile), itime
        endif
        if (messge == 1) goto 9999
        if (messge == 2) goto 9999
        call stop_with_error()
        9999 if (timon) call timstop (ithandl)
        return

        2000 FORMAT (/, A20, ' UNIT: ', I3, ', READING: ', A, / &
                ' AT SIMULATION TIME:', I10)
        2010 FORMAT (/, A20, ' UNIT: ', I3, ', READING: ', A, / &
                ' AT SIMULATION TIME:', I5, 'D ', I2, 'H ', I2, 'M ', I2, 'S !', / &
                ' TIME IN FILE:      ', I5, 'D ', I2, 'H ', I2, 'M ', I2, 'S !')
        2020 FORMAT (/, A20, ' UNIT:', I10, ', READING: ', A, / &
                ' SIMULATION TIME :', I2, 'Y ', I3, 'D ', I2, 'H ', I2, 'M ', I2, 'S .', / &
                ' TIME IN FILE    :', I2, 'Y ', I3, 'D ', I2, 'H ', I2, 'M ', I2, 'S .')

    end subroutine messag

end module m_dlwqt4
