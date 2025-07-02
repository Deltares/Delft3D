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

module HydroSet
    use m_waq_precision
    use m_logger_helper, only : stop_with_error

    !
    !          module contains everything for composition of hydrodynamics from multiple files
    !          created 19 July 2002 by Leo Postma
    !
    !     contains the following derived types:
    !          FileProp               ! a set of information with respect to one file. Per file there is only one Prop
    !          FilePropColl           ! a collection of these file properties (can be searched for uniqueness).
    !          FileUseDef             ! a set of information on the use of a file. Several defs can use one file.
    !          FileUseDefColl         ! a collection of these FileUseDefs. For each physical entity a separate Coll
    !          FileUseDefCollColl     ! a collection of these collections, so for all physical entities together.
    !
    !     contains the following functions:
    !          FilePropCollFind       ! to search a file in the FilePropColl  ; returns the index or zero if not found
    !          FilePropCollAdd        ! to add a FileProp   to the collection ; returns the current size
    !          FileUseDefCollAdd      ! to add a FileUseDef to the collection ; returns the current size
    !          FileUseDefCollCollAdd  ! to add a FileUseDefColl to the collection ; returns the current size
    !          FileUseDefCollCollFind ! to search for a FileUseDefColl by the unitnr of this physics
    !          FileUseDefCollFind     ! sees which FileUseDefs are active for the current simulation time
    !                                   it adds there contribution to the output arrays optionally interpolated
    !                                   or transformed logarithmically or both.
    !                                   this is the function with the main functionality of this system
    !
    !     contains the following subroutine:
    !          Flinterpol             ! performs the actual interpolation, routine is for local use.
    !
    integer(kind = int_wp), parameter :: FILE_NAME_SIZE = 256             ! max length file path size
    integer(kind = int_wp), parameter :: MAX_NUM = 5             ! allocated per bunch
    !
    !          Lock the files by LU-number, so that multiple instances
    !          may use the library
    !
    logical, dimension(1000), save :: file_locked


    ! Properties of the file itself
    type FileProp
        character(len = FILE_NAME_SIZE) :: name    !< File path
        integer(kind = int_wp) :: ilun             !< Unit number
        integer(kind = int_wp) :: istart           !< Start time in file
        integer(kind = int_wp) :: istop            !< Stop time in file
        integer(kind = int_wp) :: istep            !< Step time in file
        integer(kind = int_wp) :: ioffset          !< Offset at rewind of this file
        integer(kind = int_wp) :: itime1           !< Time array1
        integer(kind = int_wp) :: itime2           !< Time array2
        integer(kind = INT64) :: position = -1     !< Position in the file
        logical :: stream_access                   !< Stream or sequential access?
        real(kind = real_wp), pointer :: array1(:) !< Interpolation arrays
        real(kind = real_wp), pointer :: array2(:) !< Interpolation arrays
    end type FileProp

    ! Index of properties of files
    type FilePropPnt
        type(FileProp), pointer :: pnt
    end type FilePropPnt

    ! Collection of the files
    type FilePropColl
        type(FilePropPnt), pointer :: FilePropPnts(:) !< Array with file properties
        integer(kind = int_wp) :: maxsize             !< Maximum size of the current array
        integer(kind = int_wp) :: current_size        !< Filled up to this size
    end type FilePropColl

    ! One entry of the table of files
    type FileUseDef
        type(FilePropPnt) :: afilePnt        ! pointer to a file property
        real(kind = real_wp) :: weight          ! weight for interpolation
        integer(kind = int_wp) :: istart          ! waq start time for use
        integer(kind = int_wp) :: istop           ! waq stop time for use
        integer(kind = int_wp) :: ioffset         ! time in file for istart
        logical :: active          ! time in file for istart
    end type FileUseDef
    !
    !          this is the set of files as defined for this physical entity
    !
    type FileUseDefColl
        type(FileUseDef), pointer :: FileUseDefs(:)  ! array with file definitions
        integer(kind = int_wp) :: maxsize         ! maximum size of the current array
        integer(kind = int_wp) :: current_size         ! filled up to this size
        integer(kind = int_wp) :: unitnr          ! the entry for the physical property
        integer(kind = int_wp) :: intopt          ! interpolation option
        integer(kind = int_wp) :: istart          ! start time of the UseDefCollection
        integer(kind = int_wp) :: istop           ! stop time of the UseDefCollection
        integer(kind = int_wp) :: ioffset         ! offset at rewind of full UseDefCollection
        integer(kind = int_wp) :: nrftot          ! length of the arrays
        real(kind = real_wp), pointer :: array1(:)       ! interpolation arrays
        real(kind = real_wp), pointer :: array2(:)       ! interpolation arrays
        real(kind = real_wp), pointer :: array3(:)       ! interpolation arrays
    end type FileUseDefColl
    !
    !          this is the collection of sets of files
    !
    type FileUseDefCollColl
        type(FileUseDefColl), pointer :: FileUseDefColls(:) ! array with file definition collections
        integer(kind = int_wp) :: maxsize            ! maximum size of the current array
        integer(kind = int_wp) :: current_size            ! filled up to this size
    end type FileUseDefCollColl
    !

contains

    !
    !          function to find a file prop in a collection of fileproperties
    !
    function FilePropCollFind(aFilePropColl, aFileProp) result (iret)
        !
        type(FilePropColl) :: aFilePropColl
        type(FileProp) :: aFileProp
        integer(kind = int_wp) :: iret
        !
        iret = 0
        do i = 1, aFilePropColl%current_size         ! search by name
            if (aFilePropColl%FilePropPnts(i)%pnt%name == aFileProp%name) then
                iret = i
                return
            endif
        end do
        !
    end function FilePropCollFind
    !
    !          function to add to a collection of fileproperties
    !
    function FilePropCollAdd(aFilePropColl, aFileProp, nrftot) result (current_size)
        !
        type(FilePropColl) :: aFilePropColl
        type(FileProp) :: aFileProp
        type(FileProp), pointer :: aPropPnt           ! should be a pointer to preserve space
        type(FilePropPnt), pointer :: aFilePropPnts(:)   ! should be a pointer for the resize operation
        integer(kind = int_wp) :: nrftot, current_size
        !                                                   ! this is the standard procedure to enlarge collections
        if (aFilePropColl%current_size == aFilePropColl%maxsize) then
            allocate (aFilePropPnts (aFilePropColl%maxsize + MAX_NUM))
            do i = 1, aFilePropColl%maxsize
                aFilePropPnts(i) = aFilePropColl%FilePropPnts (i)        ! copies the pointers
            enddo
            if (aFilePropColl%maxsize /= 0) deallocate (aFilePropColl%FilePropPnts)
            aFilePropColl%FilePropPnts => aFilePropPnts                   ! attaches this new array of pointers
            aFilePropColl%maxsize = aFilePropColl%maxsize + MAX_NUM
        endif
        aFilePropColl%current_size = aFilePropColl%current_size + 1
        allocate (aPropPnt)                                  ! this is important, allocate space to
        aPropPnt = aFileProp                                   !                    preserve argument
        aFilePropColl%FilePropPnts(aFilePropColl%current_size)%pnt => aPropPnt       ! put reference to space in array
        allocate (aPropPnt%array1(nrftot))                   ! allocate the arrays  etc.
        allocate (aPropPnt%array2(nrftot))

        if (aPropPnt%stream_access .and. aPropPnt%position == -1) then
            inquire(aPropPnt%ilun, pos = aPropPnt%position)
        endif

        if (aPropPnt%stream_access) then
            read (aPropPnt%ilun, end = 10, err = 10, pos = aPropPnt%position) aPropPnt%itime1, (aPropPnt%array1(i), i = 1, nrftot)
        else
            read (aPropPnt%ilun, end = 10, err = 10) aPropPnt%itime1, (aPropPnt%array1(i), i = 1, nrftot)
        endif
        read (aPropPnt%ilun, end = 10, err = 10) aPropPnt%itime2, (aPropPnt%array2(i), i = 1, nrftot)

        if (aPropPnt%stream_access) then
            inquire(aPropPnt%ilun, pos = aPropPnt%position)
        endif

        aPropPnt%istart = aPropPnt%itime1
        aPropPnt%istep = aPropPnt%itime2 - aPropPnt%itime1
        if (aPropPnt%istop == 0) aPropPnt%istop = aPropPnt%itime2  ! This is the convention if stop time is unknown !
        current_size = aFilePropColl%current_size

        if (aPropPnt%istart == aPropPnt%istop) then
            write(*, *) 'Error: times in two consecutive records are equal!'
            write(*, *) 'File in question: ', trim(aPropPnt%name)
            write(*, *) 'Stopping the program'
            call stop_with_error()
        endif

        return
        10    current_size = 0
        aFilePropColl%current_size = aFilePropColl%current_size - 1
        return
        !
    end function FilePropCollAdd
    !
    !          function to add to a collection of file use definitions
    !
    function FileUseDefCollAdd(aFileUseDefColl, aFileUseDef) result (current_size)
        !
        type(FileUseDefColl) :: aFileUseDefColl
        type(FileUseDef) :: aFileUseDef
        type(FileUseDef), pointer :: aFileUseDefs(:) ! should be a pointer for the resize operation
        integer(kind = int_wp) :: current_size
        !                                                   ! this is the standard procedure to enlarge collections
        if (aFileUseDefColl%current_size == aFileUseDefColl%maxsize) then
            allocate (aFileUseDefs (aFileUseDefColl%maxsize + MAX_NUM))
            do i = 1, aFileUseDefColl%maxsize
                aFileUseDefs(i) = aFileUseDefColl%FileUseDefs (i)
            enddo
            if (aFileUseDefColl%maxsize /= 0) deallocate (aFileUseDefColl%FileUseDefs)
            aFileUseDefColl%FileUseDefs => aFileUseDefs
            aFileUseDefColl%maxsize = aFileUseDefColl%maxsize + MAX_NUM
        endif
        !
        aFileUseDefColl%current_size = aFileUseDefColl%current_size + 1
        aFileUseDefColl%FileUseDefs(aFileUseDefColl%current_size) = aFileUseDef
        current_size = aFileUseDefColl%current_size
        !
    end function FileUseDefCollAdd
    !
    !          function to add to a collection of collections of file use definitions
    !
    function FileUseDefCollCollAdd(aFileUseDefCollColl, aFileUseDefColl) result (current_size)
        !
        type(FileUseDefCollColl) :: aFileUseDefCollColl
        type(FileUseDefColl) :: aFileUseDefColl
        type(FileUseDefColl), pointer :: aFileUseDefColls(:)  ! should be a pointer for the resize operation
        integer(kind = int_wp) :: current_size
        !                                                   ! this is the standard procedure to enlarge collections
        if (aFileUseDefCollColl%current_size == aFileUseDefCollColl%maxsize) then
            allocate (aFileUseDefColls (aFileUseDefCollColl%maxsize + MAX_NUM))
            do i = 1, aFileUseDefCollColl%maxsize
                aFileUseDefColls(i) = aFileUseDefCollColl%FileUseDefColls (i)
            enddo
            if (aFileUseDefCollColl%maxsize /= 0) deallocate (aFileUseDefCollColl%FileUseDefColls)
            aFileUseDefCollColl%FileUseDefColls => aFileUseDefColls
            aFileUseDefCollColl%maxsize = aFileUseDefCollColl%maxsize + MAX_NUM
        endif
        !
        aFileUseDefCollColl%current_size = aFileUseDefCollColl%current_size + 1
        allocate (aFileUseDefColl%array1(aFileUseDefColl%nrftot), &
                aFileUseDefColl%array2(aFileUseDefColl%nrftot), &
                aFileUseDefColl%array3(aFileUseDefColl%nrftot))
        aFileUseDefCollColl%FileUseDefColls(aFileUseDefCollColl%current_size) = aFileUseDefColl
        current_size = aFileUseDefCollColl%current_size
        !
    end function FileUseDefCollCollAdd
    !
    !          function to find a collection in a collection of collections of file use definitions
    !
    function FileUseDefCollCollFind(aFileUseDefCollColl, ilun) result (found)
        !
        type(FileUseDefCollColl) :: aFileUseDefCollColl
        integer(kind = int_wp) :: ilun, found
        !
        found = 0
        do i = 1, aFileUseDefCollColl%current_size          ! search by unitc number for this phisics
            if (aFileUseDefCollColl%FileUseDefColls(i)%unitnr == ilun) then
                found = i
                return
            endif
        enddo
        !
    end function FileUseDefCollCollFind
    !
    !          function to find a file use def in a collection of file use definitions and make the result
    !
    function FileUseDefCollFind(aUseDefColl, i, itime, UPDATE, LREWIN) result (found)
        !
        !          aUseDefColl    I    ! a collection of FileUseDefs in use for this physical entity
        !                              ! also contains the array for return values
        !          i              I    ! sequence number of this file in the UseDefColl for this entity
        !          itime          I    ! time for evaluation
        !          UPDATE         O    ! result has been updated
        !          LREWIN         O    ! file has been rewound
        !          found          R    ! return value ** if -aUseDef%istart then itime is befor start of this definition
        !                                                not clear what happens if aUseDef%istart itself is negative
        !
        type(FileUseDefColl) :: aUseDefColl
        type(FileUseDef), pointer :: aUseDef
        type(FileProp), pointer :: aProp
        integer(kind = int_wp) :: i, i2, itime, found, nrftot
        logical                             UPDATE, LREWIN, LrLocal


        !
        found = -1                                         ! default reaction
        UPDATE = .FALSE.                                    ! default reaction
        !jtest   LREWIN  = .FALSE.                                    ! default reaction
        LrLocal = .FALSE.                                    ! local to test rewind
        !
        nrftot = aUseDefColl%nrftot
        itLocal = itime - aUseDefColl%ioffset                ! time within this collection of file defs
        aUseDef => aUseDefColl%FileUseDefs(i)                ! get the FileUseDef for this call
        aProp => aUseDef%afilePnt%pnt                      ! get the FileProp for this FileUseDef
        itFile = itLocal - aUseDef%istart + aUseDef%ioffset - aProp%ioffset  ! compute time in file

        call lock_this_file(aProp%ilun)
        !
        if (itLocal < aUseDef%istart) then              ! if earlier than start of this definition
            found = -aUseDef%istart                           !      return minus start of this definition
            aUseDef%active = .false.
            call unlock_this_file(aProp%ilun)
            return
        endif
        !
        if (aUseDef%istop == 0             .or.          & ! if stop of the definition is not defined
                itLocal < aUseDef%istop       .or.          & ! or local time before stop
                (itLocal == aUseDef%istop .and.               & ! or local time is stop,
                        itLocal == aUseDefColl%istop)) then  !    but also end of collection then
            !
            found = 0                                         ! we are in business
            aUseDef%active = .true.
            !
            if (itFile < aProp%istart) then              ! before physical start of file (strange)
                found = -aProp%istart                          !      probably an uncaught error in the input
                aUseDef%active = .false.                       !      it implies that aProp%istart > aProp%istop
                call unlock_this_file(aProp%ilun)
                return
            endif
            !
            if (itFile < aProp%itime1) then              ! aparently a remnant from earlier invokation
                UPDATE = .TRUE.                                ! reset everything in this file property
                if (aProp%stream_access) then
                    read (aProp%ilun, iostat = ierr, pos = 1)
                else
                    rewind (aProp%ilun)                       ! this is the standard rewind
                endif
                read   (aProp%ilun) aProp%itime1, (aProp%array1(i2), i2 = 1, nrftot)
                read   (aProp%ilun) aProp%itime2, (aProp%array2(i2), i2 = 1, nrftot)

                if (aProp%stream_access) then
                    inquire(aProp%ilun, pos = aProp%position)
                endif

                aProp%ioffset = 0
                aUseDef%active = .false.
            endif
            !
            if (aProp%istop /= aProp%itime2 .and.           & ! if file-stoptime is known (otherwise it increases with itime2)
                    aProp%istop < itFile) then               ! and time in the file is after file-stoptime
                idt = ((itFile - aProp%istop) / (aProp%istop - aProp%istart) + 1) * (aProp%istop - aProp%istart)
                aProp%ioffset = aProp%ioffset + idt            ! this prevents many readings to initialise
                itFile = itFile - idt            ! the timers at start of simulation, note that idt is always
                aUseDef%active = .false.                       ! a whole number times aProp%istop-aProp%istart
            endif
            !
            do while (itFile >= aProp%itime2)             ! after or at time of second record ?
                UPDATE = .TRUE.                                ! update takes place

                !
                ! If we are dealing with stream access files, read the previous record
                ! again. Otherwise we copy the second interpolation array into the first
                ! one. This is needed for working with multiple instances
                !
                aProp%itime1 = aProp%itime2                    ! copy info of second record to first record
                do i2 = 1, nrftot
                    aProp%array1(i2) = aProp%array2(i2)
                enddo

                if (aProp%stream_access) then
                    read (aProp%ilun, end = 10, pos = aProp%position) aProp%itime2, (aProp%array2(i2), i2 = 1, nrftot)   ! read new second record
                    inquire(aProp%ilun, pos = aProp%position)
                else
                    read (aProp%ilun, end = 10) aProp%itime2, (aProp%array2(i2), i2 = 1, nrftot)   ! read new second record
                endif

                goto 20                                        ! go to the optional detection of end time of the file
                !
                10          if (itFile == aProp%itime2) then        ! only accept timings exactly on breakpoints
                    call Flinterpol (aProp, aUseDefColl%array1, aUseDefColl%nrftot, &
                            aUseDefColl%intopt, aUseDef, itLocal, itFile, UPDATE)
                    LrLocal = .true.                         ! this file has been saved
                endif
                LREWIN = .true.                             ! rewind takes place, previous setting stored
                idt = ((itFile - aProp%istop) / (aProp%istop - aProp%istart) + 1) * (aProp%istop - aProp%istart)
                aProp%ioffset = aProp%ioffset + idt
                itFile = itFile - idt
                if (aProp%stream_access) then
                    read (aProp%ilun, iostat = ierr, pos = 1)
                else
                    rewind (aProp%ilun)                       ! this is the standard rewind
                endif
                found = aProp%itime1                           ! save the time in file for the message to the user

                read   (aProp%ilun) aProp%itime1, (aProp%array1(i2), i2 = 1, nrftot)
                read   (aProp%ilun) aProp%itime2, (aProp%array2(i2), i2 = 1, nrftot)

                if (aProp%stream_access) then
                    inquire(aProp%ilun, pos = aProp%position)
                endif
                !
                20          if (aProp%istop == aProp%itime1) aProp%istop = aProp%itime2  ! update file stop
            end do                                            ! if all is alright, this should be it
            !
            if (.not. LrLocal .and. .not. LREWIN) then    ! also sum in the saved system jtest also not when a previous rewind
                call Flinterpol (aProp, aUseDefColl%array1, aUseDefColl%nrftot, &
                        aUseDefColl%intopt, aUseDef, itLocal, itFile, UPDATE)
            endif
            call Flinterpol (aProp, aUseDefColl%array2, aUseDefColl%nrftot, &
                    aUseDefColl%intopt, aUseDef, itLocal, itFile, UPDATE)
            !
        else                                                 ! we exceeded the stop time of the UseDef
            !
            if (aUseDef%active) then                        ! it was apparently previously active
                if (itFile <= aProp%itime2)                 & ! only accept timings exactly on breakpoints
                        call Flinterpol (aProp, aUseDefColl%array1, aUseDefColl%nrftot, &
                                aUseDefColl%intopt, aUseDef, itLocal, itFile, UPDATE)
                if (aProp%stream_access) then
                    read (aProp%ilun, iostat = ierr, pos = 1)
                else
                    rewind (aProp%ilun)                       ! this is the standard rewind
                endif
                read   (aProp%ilun) aProp%itime1, (aProp%array1(i2), i2 = 1, nrftot)
                read   (aProp%ilun) aProp%itime2, (aProp%array2(i2), i2 = 1, nrftot)

                if (aProp%stream_access) then
                    inquire(aProp%ilun, pos = aProp%position)
                endif

                aProp%ioffset = 0
                aUseDef%active = .false.
                found = aProp%itime1                          ! save the time in file for the message to the user
                UPDATE = .true.                                ! update has apparently taken place
                LREWIN = .true.
            endif
            !
        endif
        call unlock_this_file(aProp%ilun)
        !
    end function FileUseDefCollFind

    !
    !          subroutine to lock a file identified by the LU-number or wait until it is freed
    !
    subroutine lock_this_file(ilun)

        integer(kind = int_wp), intent(in) :: ilun

        if (file_locked(ilun)) then
            do while (file_locked(ilun))
                call sleep_a_while(ilun)
            enddo
        endif
        file_locked(ilun) = .true.

    end subroutine lock_this_file

    !
    !          subroutine to unlock a file identified by the LU-number or wait until it is freed
    !
    subroutine unlock_this_file(ilun)

        integer(kind = int_wp), intent(in) :: ilun

        file_locked(ilun) = .false.

    end subroutine unlock_this_file

    !
    !          subroutine to sleep a while (the argument is used to make the compiler aware of
    !          the fact that the file_locked variable may change outside the routine)
    !
    subroutine sleep_a_while(ilun)

        integer(kind = int_wp), intent(in) :: ilun

        if (file_locked(ilun)) then
            call sleep(1)
        endif

    end subroutine sleep_a_while

    !
    subroutine Flinterpol (aProp, array3, nrftot, intopt, aDef, itLocal, itFile, UPDATE)
        !
        type(FileProp)   aProp
        type(FileUseDef) aDef
        real(kind = real_wp) :: array3(nrftot), weight
        logical          UPDATE
        !
        weight = aDef%weight
        if (abs(aDef%weight) < 1.0E-30) weight = real((itLocal - aDef%istart)) / real(aDef%istop - aDef%istart)
        if (abs(aDef%weight + 1.0) < 1.0E-30) weight = real((aDef%istop - itLocal)) / real(aDef%istop - aDef%istart)
        !
        if (intopt == 0) then
            do i = 1, nrftot
                if (itLocal == aDef%istart) UPDATE = .true.
                array3(i) = array3(i) + weight * aProp%array1(i) ! no interpolation
            enddo
        endif
        if (intopt == 1) then                                      ! linear interpolation
            UPDATE = .true.
            if (aProp%itime1 == aProp%itime2) then
                fact = 1.0
            else
                fact = real(itFile - aProp%itime1) / (aProp%itime2 - aProp%itime1)
            endif
            do i = 1, nrftot
                array3(i) = array3(i) + weight * (fact * aProp%array2(i) + (1.0 - fact) * aProp%array1(i))
            enddo
        endif                                                ! logarithmic interpolation between files
        if (intopt == 2) then
            UPDATE = .true.
            do i = 1, nrftot
                array3(i) = array3(i) + weight * alog(max(aProp%array1(i), 1.0E-25))
            enddo
        endif
        if (intopt == 3) then                            ! logarithmic interpolation between files and times
            UPDATE = .true.
            if (aProp%itime1 == aProp%itime2) then
                fact = 1.0
            else
                fact = real(itFile - aProp%itime1) / (aProp%itime2 - aProp%itime1)
            endif
            do i = 1, nrftot
                array3(i) = array3(i) + weight * (fact * alog(max(aProp%array2(i), 1.0E-25)) + &
                        (1.0 - fact) * alog(max(aProp%array1(i), 1.0E-25)))
            enddo
        endif

    end subroutine Flinterpol

    !> Close all open hydrodynamic files
    subroutine close_hydro_files(collection)
        type(FileUseDefCollColl) :: collection !< Collection of open hydrodynamic files

        type(FileUseDefColl), pointer :: files
        integer(kind = int_wp) :: i, j

        do i = 1, collection%current_size
            files => collection%FileUseDefColls(i)

            do j = 1, files%current_size
                close(files%FileUseDefs(j)%aFilePnt%pnt%ilun)
            enddo
        enddo
    end subroutine close_hydro_files

end module HydroSet
