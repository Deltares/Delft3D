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

module alloc_mod
    !
    !  Generic module for memory allocation,
    !  including proper error handling
    !
    !
    !  data definition module(s)
    !
    use m_stop_exit
    use m_waq_precision, only: int_wp, dp, sp               ! single and double precision
    implicit none

    integer(int_wp), private :: lunmem        ! output unit memory allocation
    integer(int_wp), private :: lunrep        ! output unit dellpar report file
    integer(int_wp), private :: accu          ! accumulated memory size
    integer(int_wp), private :: number        ! accumulated memory size
    !
    !
    interface alloc
        module procedure alloc_int_1D     ! for allocating 1D integer arrays
        module procedure alloc_int_2D     ! for allocating 2D integer arrays
        module procedure alloc_int_3D     ! for allocating 3D integer arrays
        module procedure alloc_int_4D     ! for allocating 4D integer arrays
        module procedure alloc_real_1D    ! for allocating 1D real    arrays
        module procedure alloc_real_2D    ! for allocating 2D real    arrays
        module procedure alloc_real_3D    ! for allocating 3D real    arrays
        module procedure alloc_real_4D    ! for allocating 4D real    arrays
        module procedure alloc_double_1D  ! for allocating 1D double  arrays
        module procedure alloc_char_1D    ! for allocating 1D char.   arrays
        module procedure alloc_char_2D    ! for allocating 2D char.   arrays
    end interface
contains
    ! ----------------------------------------------------
    !     initialisation
    ! ----------------------------------------------------
    subroutine init_alloc (lun, lunut)

        integer(int_wp), intent(out) :: lun
        integer(int_wp), intent(in) :: lunut

        accu = 0
        number = 0
        lunrep = lunut
        open  (newunit = lunmem, file = "part_memory_map.out")
        lun = lunmem
        write (lunmem, '(/'' ====> allocated array space in 4-byte words <===='' )')
        write (lunmem, '( ''  nr typ       kind array name          array size''/)')

        return
    end subroutine
    ! ----------------------------------------------------
    !     wrap up
    ! ----------------------------------------------------
    subroutine exit_alloc (accu2)
        integer(int_wp) accu2
        accu2 = accu
        write (lunmem, '(''                                       ==========='' )')
        write (lunmem, 10)      accu / 4, &
                accu / 1000000000, mod(accu, 1000000000) / 1000000, &
                mod(accu, 1000000) / 1000, mod(accu, 1000)
        close (lunmem)
        return
        10 format (' Grand total of all array space:       ', i11, ' words or:', &
                i3, '-GB ', i3, '-MB ', i3, '-KB ', i3, '-B.')
    end subroutine
    !                               integer
    !                               real
    !                               character(256)
    ! ----------------------------------------------------
    !     INTEGER ARRAYS
    ! ----------------------------------------------------
    subroutine alloc_int_1D(name, arr, n1)

        character(*), intent(in) :: name       !< array name
        integer, dimension(:), pointer :: arr        !< array pointer
        integer, intent(in) :: n1         !< array size
        integer                           knd        ! array kind
        integer :: stat       ! return from allocate
        integer, pointer :: wrk (:)    ! for resizing
        integer                           increm     ! increment

        knd = kind(arr)
        increm = knd * n1
        if (associated(arr)) then
            increm = increm - knd * size(arr)
            if (increm <= 0) return
        endif
        number = number + 1
        allocate (wrk(n1), stat = stat)
        if (stat == 0 .and. n1 > 0) then
            accu = accu + increm
            if (associated(arr)) then
                wrk(1:size(arr)) = arr
                deallocate (arr)
                write (lunmem, '(i4,'' integer    ('',i1,'') '',a,t40,i11,'' <== resized'')')      &
                        number, knd, name, increm / 4
            else
                write (lunmem, '(i4,'' integer    ('',i1,'') '',a,t40,i11)') number, knd, name, increm / 4
            endif
            arr => wrk
        else
            write (lunmem, '('' ERROR allocating: '',a,'' requested size: '',i11)')name, n1
            write (*, *) ' Allocation error. Inspect memory_map file.'
            call stop_exit(1)
        endif
        return

    end subroutine alloc_int_1D
    ! ----------------------------------------------------
    subroutine alloc_int_2D(name, arr, n1, n2)

        character(*), intent(in) :: name        !< array name
        integer, pointer :: arr   (:, :) !< array pointer
        integer, intent(in) :: n1, n2      !< array sizes
        integer                            knd         ! array kind
        integer :: stat        ! return from allocate
        integer, pointer :: wrk   (:, :) ! work array
        integer                            first      !< resulting first dimension
        integer                            secnd      !< resulting first dimension
        integer                            increm     ! increment

        knd = kind(arr)
        if (associated(arr)) then
            if (n1 <= size(arr, 1) .and. n2 <= size(arr, 2)) return
            first = max(n1, size(arr, 1))
            secnd = max(n2, size(arr, 2))
            increm = knd * first * secnd - knd * size(arr)
        else
            first = n1
            secnd = n2
            increm = knd * n1 * n2
        endif
        number = number + 1
        allocate (wrk(first, secnd), stat = stat)
        if (stat == 0 .and. n1 > 0 .and. n2 > 0) then
            accu = accu + increm
            if (associated(arr)) then
                wrk(1:size(arr, 1), 1:size(arr, 2)) = arr(:, :)
                deallocate (arr)
                write (lunmem, '(i4,'' integer    ('',i1,'') '',a,t40,i11,'' <== resized'')')      &
                        number, knd, name, increm / 4
            else
                write (lunmem, '(i4,'' integer    ('',i1,'') '',a,t40,i11)') number, knd, name, increm / 4
            endif
            arr => wrk
        else
            write (lunmem, '('' ERROR allocating: '',a,'' requested size: '',i11)')name, n1, n2
            write (*, *) ' Allocation error. Inspect memory_map file.'
            call stop_exit(1)
        endif
        return

    end subroutine alloc_int_2D
    ! ----------------------------------------------------
    subroutine alloc_int_3D(name, arr, n1, n2, n3)

        character(*), intent(in) :: name          !< array name
        integer, pointer :: arr   (:, :, :) !< array pointer
        integer, intent(in) :: n1, n2, n3    !< array sizes
        integer                           knd           ! array kind
        integer :: stat          ! return from allocate

        allocate (arr(n1, n2, n3), stat = stat)
        if (stat == 0 .and. n1 > 0 .and. n2 > 0 .and. n3 > 0) then
            number = number + 1
            knd = kind(arr)
            accu = accu + n1 * n2 * n3 * knd
            write (lunmem, '(i4,'' integer    ('',i1,'') '',a,t40,i11)') number, knd, name, n1 * n2 * n3 * knd / 4
        else
            write (lunmem, '('' ERROR allocating: '',a,'' requested size: '',3i11)')name, n1, n2, n3
            write (*, *) ' Allocation error. Inspect memory_map file.'
            call stop_exit(1)
        endif
        return

    end subroutine alloc_int_3D
    ! ----------------------------------------------------
    subroutine alloc_int_4D(arr, n1, n2, n3, n4)
        integer, dimension(:, :, :, :), pointer :: arr
        integer :: stat
        integer :: n1, n2, n3, n4
        logical :: alloc_ok
        allocate(arr(n1, n2, n3, n4), stat = stat)
        alloc_ok = (stat == 0)
        if (.not.alloc_ok) call alloc_error()
        return
    end subroutine alloc_int_4D
    ! ----------------------------------------------------
    !     REAL ARRAYS
    ! ----------------------------------------------------
    subroutine alloc_real_1D(name, arr, n1)

        character(*), intent(in) :: name       !< array name
        real, pointer :: arr  (:)   !< array pointer
        integer, intent(in) :: n1         !< array size
        integer                        knd        ! array kind
        integer :: stat       ! return from allocate
        real, pointer :: wrk  (:)   ! work array
        integer                        increm     ! increment

        knd = kind(arr)
        increm = knd * n1
        if (associated(arr)) then
            increm = increm - knd * size(arr)
            if (increm <= 0) return
        endif
        number = number + 1
        allocate (wrk(n1), stat = stat)
        if (stat == 0 .and. n1 > 0) then
            accu = accu + increm
            if (associated(arr)) then
                wrk(1:size(arr)) = arr
                deallocate (arr)
                write (lunmem, '(i4,'' real       ('',i1,'') '',a,t40,i11,'' <== resized'')')      &
                        number, knd, name, increm / 4
            else
                write (lunmem, '(i4,'' real       ('',i1,'') '',a,t40,i11)') number, knd, name, increm / 4
            endif
            arr => wrk
        else
            write (lunmem, '('' ERROR allocating: '',a,'' requested size: '',i11)')name, n1
            write (*, *) ' Allocation error. Inspect memory_map file.'
            call stop_exit(1)
        endif
        return

    end subroutine alloc_real_1D
    ! ----------------------------------------------------
    subroutine alloc_double_1D(name, arr, n1)

        character(*), intent(in) :: name       !< array name
        real(dp), pointer :: arr  (:)   !< array pointer
        integer, intent(in) :: n1         !< array size
        integer                           knd        ! array kind
        integer :: stat       ! return from allocate

        allocate (arr(n1), stat = stat)
        if (stat == 0 .and. n1 > 0) then
            number = number + 1
            knd = kind(arr)
            accu = accu + n1 * knd
            write (lunmem, '(i4,'' real       ('',i1,'') '',a,t40,i11)') number, knd, name, n1 * knd / 4
        else
            write (lunmem, '('' ERROR allocating: '',a,'' requested size: '',i11)')name, n1
            write (*, *) ' Allocation error. Inspect memory_map file.'
            call stop_exit(1)
        endif
        return

    end subroutine alloc_double_1D
    ! ----------------------------------------------------
    subroutine alloc_real_2D(name, arr, n1, n2)

        character(*), intent(in) :: name       !< array name
        real     (sp), pointer :: arr(:, :)   !< array pointer
        integer, intent(in) :: n1, n2     !< array sizes
        integer                            knd        ! array kind
        integer :: stat       ! return from allocate
        real     (sp), pointer :: wrk(:, :)   !< work arraoy pointer
        integer                            first      !< resulting first dimension
        integer                            secnd      !< resulting first dimension
        integer                            increm     ! increment

        knd = kind(arr)
        if (associated(arr)) then
            if (n1 <= size(arr, 1) .and. n2 <= size(arr, 2)) return
            first = max(n1, size(arr, 1))
            secnd = max(n2, size(arr, 2))
            increm = knd * first * secnd - knd * size(arr)
        else
            first = n1
            secnd = n2
            increm = knd * n1 * n2
        endif
        number = number + 1
        allocate (wrk(first, secnd), stat = stat)
        if (stat == 0 .and. n1 > 0 .and. n2 > 0) then
            accu = accu + increm
            if (associated(arr)) then
                wrk(1:size(arr, 1), 1:size(arr, 2)) = arr(:, :)
                deallocate (arr)
                write (lunmem, '(i4,'' real       ('',i1,'') '',a,t40,i11,'' <== resized'')')      &
                        number, knd, name, increm / 4
            else
                write (lunmem, '(i4,'' real       ('',i1,'') '',a,t40,i11)') number, knd, name, increm / 4
            endif
            arr => wrk
        else
            write (lunmem, '('' ERROR allocating: '',a,'' requested size: '',i11)')name, n1, n2
            write (*, *) ' Allocation error. Inspect memory_map file.'
            call stop_exit(1)
        endif
        return

    end subroutine alloc_real_2D
    ! ----------------------------------------------------
    subroutine alloc_real_3D(name, arr, n1, n2, n3)

        character(*), intent(in) :: name         !< array name
        real, pointer :: arr(:, :, :)   !< array pointer
        integer, intent(in) :: n1, n2, n3   !< array sizes
        integer                            knd          ! array kind
        integer :: stat         ! return from allocate
        real, pointer :: wrk(:, :, :)   !< work array pointer
        integer                            first      !< resulting first dimension
        integer                            secnd      !< resulting secnd dimension
        integer                            third      !< resulting third dimension
        integer                            increm     ! increment

        knd = kind(arr)
        if (associated(arr)) then
            if (n1 <= size(arr, 1) .and. n2 <= size(arr, 2) .and. n3 <= size(arr, 3)) return
            first = max(n1, size(arr, 1))
            secnd = max(n2, size(arr, 2))
            third = max(n3, size(arr, 3))
            increm = knd * first * secnd * third - knd * size(arr)
        else
            first = n1
            secnd = n2
            third = n3
            increm = knd * n1 * n2 * n3
        endif
        number = number + 1
        allocate (wrk(first, secnd, third), stat = stat)
        if (stat == 0 .and. n1 > 0 .and. n2 > 0 .and. n3 > 0) then
            accu = accu + increm
            if (associated(arr)) then
                wrk(1:size(arr, 1), 1:size(arr, 2), 1:size(arr, 3)) = arr(:, :, :)
                deallocate (arr)
                write (lunmem, '(i4,'' real       ('',i1,'') '',a,t40,i11,'' <== resized'')')      &
                        number, knd, name, increm / 4
            else
                write (lunmem, '(i4,'' real       ('',i1,'') '',a,t40,i11)') number, knd, name, increm / 4
            endif
            arr => wrk
        else
            write (lunmem, '('' ERROR allocating: '',a,'' requested size: '',i11)')name, n1, n2
            write (*, *) ' Allocation error. Inspect memory_map file.'
            call stop_exit(1)
        endif
        return

    end subroutine alloc_real_3D
    ! ----------------------------------------------------
    subroutine alloc_real_4D(name, arr, n1, n2, n3, n4)

        character(*), intent(in) :: name            !< array name
        real, pointer :: arr(:, :, :, :)    !< array pointer
        integer, intent(in) :: n1, n2, n3, n4   !< array sizes
        integer                           knd             ! array kind
        integer :: stat            ! return from allocate

        allocate (arr(n1, n2, n3, n4), stat = stat)
        if (stat == 0 .and. n1 > 0 .and. n2 > 0 .and. n3 > 0 .and. n4 > 0) then
            number = number + 1
            knd = kind(arr)
            accu = accu + n1 * n2 * n3 * n4 * knd
            write (lunmem, '(i4,'' real       ('',i1,'') '',a,t40,i11)') number, knd, name, n1 * n2 * n3 * n4 * knd / 4
        else
            write (lunmem, '('' ERROR allocating: '',a,'' requested size: '',4i11)')name, n1, n2, n3, n4
            write (*, *) ' Allocation error. Inspect memory_map file.'
            call stop_exit(1)
        endif
        return

    end subroutine alloc_real_4D
    ! ----------------------------------------------------
    !     CHARACTER ARRAYS
    ! ----------------------------------------------------
    subroutine alloc_char_1D(name, arr, n1)

        character(*), intent(in) :: name       !< array name
        character(*), pointer :: arr  (:)   !< array pointer
        integer, intent(in) :: n1         !< array size
        integer                           knd        ! array kind
        integer :: stat       ! return from allocate

        allocate (arr(n1), stat = stat)
        if (stat == 0 .and. n1 > 0) then
            number = number + 1
            knd = len(arr)
            accu = accu + n1 * knd
            write (lunmem, '(i4,'' character('',i3,'') '',a,t40,i11)') number, knd, name, n1 * knd / 4
        else
            write (lunmem, '('' ERROR allocating: '',a,'' requested size: '',i11)')name, n1 * knd
            write (*, *) ' Allocation error. Inspect memory_map file.'
            call stop_exit(1)
        endif
        return

    end subroutine alloc_char_1D
    ! ----------------------------------------------------
    subroutine alloc_char_2D(arr, n1, n2)
        character (len = *), dimension(:, :), pointer :: arr
        integer :: stat
        integer :: n1, n2
        logical :: alloc_ok
        allocate(arr(n1, n2), stat = stat)
        alloc_ok = (stat == 0)
        if (.not.alloc_ok) call alloc_error()
        return
    end subroutine alloc_char_2D
    ! ----------------------------------------------------
    subroutine alloc_error()
        write(lunrep, '(//a)') ' * Part Memory Error '
        write(lunrep, '(  a)') ' *    Could not allocate required memory'
        write(lunrep, '(  a)') ' *    Please contact Delft3D Support        '
        write(lunrep, '(a//)') ' *    Part aborted                          '
        write(*, '(//a)') ' * Part Memory Error '
        write(*, '(  a)') ' *    Could not allocate required memory'
        write(*, '(  a)') ' *    Please contact Delft3D Support    '
        write(*, '(a//)') ' *    Part aborted                          '
        call stop_exit(1)
    end subroutine alloc_error
end module alloc_mod
