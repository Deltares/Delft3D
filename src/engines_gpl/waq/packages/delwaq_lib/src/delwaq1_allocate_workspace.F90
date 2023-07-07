!!  Copyright (C)  Stichting Deltares, 2012-2023.
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


!>\file
!>                    delwaq1_allocate_workspace

subroutine delwaq1_allocate_workspace(argc, argv, errorcode)
    use m_getcom
    use m_delwaq1_data
      
    implicit none
      
    integer, intent(in)                           :: argc
    character(len=*), dimension(argc), intent(in) :: argv
    integer, intent(inout)                        :: errorcode
    

      
    !
    !     allocate workspace
    !
    call getcom ( '-imax', 1 , lfound, imax  , rdummy, cdummy, ierr )
    if ( lfound ) then
        if ( ierr .eq. 0 ) then
          write(lunrep,'(A,I12)') " Command line argument -IMAX, size of integer work array:",imax
        else
          write(lunrep,'(A)') " ERROR: interpreting command line argument -IMAX, size of integer work array:"
          ierr = 1
          call delwaq1_write_messages(argc, argv, errorcode)
          return
        endif
    else
        imax = iimax
    endif
    call getcom ( '-rmax', 1 , lfound, rmax  , rdummy, cdummy, ierr )
    if ( lfound ) then
        if ( ierr .eq. 0 ) then
          write(lunrep,'(A,I12)') " Command line argument -RMAX, size of real work array:",rmax
        else
          write(lunrep,'(A)') " ERROR: interpreting command line argument -RMAX, size of real work array:"
          ierr = 1
          call delwaq1_write_messages(argc, argv, errorcode)
          return
        endif
    else
        rmax = irmax
    endif
    call getcom ( '-cmax', 1 , lfound, cmax  , rdummy, cdummy, ierr )
    if ( lfound ) then
        if ( ierr .eq. 0 ) then
          write(lunrep,'(A,I12)') " Command line argument -CMAX, size of character work array:",cmax
        else
          write(lunrep,'(A)') " ERROR: interpreting command line argument -CMAX, size of character work array:"
          ierr = 1
          call delwaq1_write_messages(argc, argv, errorcode)
          return
        endif
    else
        cmax = icmax
    endif
    allocate(iar(imax),stat=ierr_alloc)
    if ( ierr_alloc .ne. 0 ) then
        write ( lunrep , '(A,I6,A,I12)') " ERROR: allocating integer work array:",ierr_alloc," with length:",imax
        ierr = 1
        call delwaq1_write_messages(argc, argv, errorcode)
        return
    endif
    allocate(rar(rmax),stat=ierr_alloc)
    if ( ierr_alloc .ne. 0 ) then
        write ( lunrep , '(A,I6,A,I12)') " ERROR: allocating real work array:",ierr_alloc," with length:",rmax
        ierr = 1
        call delwaq1_write_messages(argc, argv, errorcode)
        return
    endif
    allocate(car(cmax),stat=ierr_alloc)
    if ( ierr_alloc .ne. 0 ) then
        write ( lunrep , '(A,I6,A,I12)') " ERROR: allocating character work array:",ierr_alloc," with length:",cmax
        ierr = 1
        call delwaq1_write_messages(argc, argv, errorcode)
        return
    endif

end subroutine delwaq1_allocate_workspace