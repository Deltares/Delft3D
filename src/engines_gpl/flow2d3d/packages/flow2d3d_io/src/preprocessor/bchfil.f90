subroutine bchfil(lundia    ,error     ,filbch    ,fmttmp    ,ntof      , &
                & mxnto     ,kc        ,mxkc      ,omega     ,hydrbc    , &
                & gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
!
!    Function: Reads the harmonic boundary condition records from
!              an unformatted or formatted file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    use system_utils, only: exifil
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                                  , intent(in)  :: kc      ! Description and declaration in dimens.igs
    integer                                  , intent(in)  :: lundia  ! Description and declaration in inout.igs
    integer                                  , intent(in)  :: mxkc    ! Maximum number of frequencies
    integer                                  , intent(in)  :: mxnto   ! Maximum number of open boundaries, MXNTO := NTO
    integer                                  , intent(in)  :: ntof    ! Description and declaration in dimens.igs
    logical                                  , intent(out) :: error   ! Flag=TRUE if an error is encountered
    real(fp)     , dimension(4, mxnto, mxkc)               :: hydrbc  ! Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(kc)                           :: omega   ! Description and declaration in esm_alloc_real.f90
    character(*)                             , intent(in)  :: filbch  ! Name of the relevant file
    character(11)                            , intent(in)  :: fmttmp  ! Help var. for the attribute file formats (eg. the grid file)
!
! Local variables
!
    integer           :: i           ! Help var.  
    integer           :: ibeg        ! Begin position in the RECORD from where the search for data/record is started 
    integer           :: iend        ! Last position in the RECORD when the searched data/record is finished 
    integer           :: ier         ! =  0 -> end of record encountered 
                                     ! =  1 -> real value found 
                                     ! = -1 -> length or number of data is larger then specified by the calling routine 
    integer           :: iocond      ! IO status for reading 
    integer           :: k           ! Help var.  
    integer           :: lfile       ! Help var. specifying the length of character variables for file names 
    integer           :: lr132       ! Standard length of a record in the attribute file = 132 
    integer           :: luntmp      ! Help var. for a unit number of an attribute file 
    integer           :: n           ! Help var. 
    integer, external :: newlun
    real(fp)          :: rdef        ! Help var. containing default va- lue(s) for real variable 
    character(300)    :: errmsg      ! Character var. containing the error message to be written to file. The message depend on the error. 
    character(132)    :: rec132      ! Standard rec. length in an attribute file (132) 
!
!
!! executable statements -------------------------------------------------------
!
    !
    ! Initialize local parameters
    !
    lr132 = 132
    ier   = 0
    rdef  = 0.0
    !
    ! Test file existence and if so read
    !
    call remove_leading_spaces(filbch, lfile)
    !
    if (exifil(filbch, lundia)) then
       open (newunit=luntmp, file = filbch(1:lfile), form = fmttmp, status = 'old')
       !
       ! Unformatted file
       !
       if (fmttmp(:2) == 'un') then
          !
          ! Read kc frequencies (kc including mean value)
          ! Hereafter read blank record
          !
          read (luntmp, iostat = iocond) (omega(k), k = 1, kc)
          if (iocond /= 0) then
             if (iocond < 0) then
                call prterr(lundia, 'G006', filbch(1:lfile))
             else
                call prterr(lundia, 'G007', filbch(1:lfile))
             endif
             error = .true.
             goto 200
          endif
          !
          ! Blank record
          !
          read (luntmp, iostat = iocond)
          if (iocond /= 0) then
             if (iocond < 0) then
                call prterr(lundia, 'G006', filbch(1:lfile))
             else
                call prterr(lundia, 'G007', filbch(1:lfile))
             endif
             error = .true.
             goto 200
          endif
          !
          ! Read per ntof kc amplitude for end A
          !
          do n = 1, ntof
             read (luntmp, iostat = iocond) (hydrbc(1, n, k), k = 1, kc)
             if (iocond /= 0) then
                if (iocond < 0) then
                   call prterr(lundia, 'G006', filbch(1:lfile))
                else
                   call prterr(lundia, 'G007', filbch(1:lfile))
                endif
                error = .true.
                goto 200
             endif
          enddo
          !
          ! Now the same for end B
          !
          do n = 1, ntof
             read (luntmp, iostat = iocond) (hydrbc(2, n, k), k = 1, kc)
             if (iocond /= 0) then
                if (iocond < 0) then
                   call prterr(lundia, 'G006', filbch(1:lfile))
                else
                   call prterr(lundia, 'G007', filbch(1:lfile))
                endif
                error = .true.
                goto 200
             endif
          enddo
          !
          ! Blank record only if kc > 1
          !
          if (kc > 1) then
             read (luntmp, iostat = iocond)
             if (iocond /= 0) then
                if (iocond < 0) then
                   call prterr(lundia, 'G006', filbch(1:lfile))
                else
                   call prterr(lundia, 'G007', filbch(1:lfile))
                endif
                error = .true.
                goto 200
             endif
             !
             ! Read per ntof kc-1 phases for end A
             ! kc = 1 is mean value, no phases
             !
             do n = 1, ntof
                read (luntmp, iostat = iocond) (hydrbc(3, n, k), k = 2, kc)
                if (iocond /= 0) then
                   if (iocond < 0) then
                      call prterr(lundia, 'G006', filbch(1:lfile))
                   else
                      call prterr(lundia, 'G007', filbch(1:lfile))
                   endif
                   error = .true.
                   goto 200
                endif
             enddo
             !
             ! Now the same for end B
             !
             do n = 1, ntof
                read (luntmp, iostat = iocond) (hydrbc(4, n, k), k = 2, kc)
                if (iocond /= 0) then
                   if (iocond < 0) then
                      call prterr(lundia, 'G006', filbch(1:lfile))
                   else
                      call prterr(lundia, 'G007', filbch(1:lfile))
                   endif
                   error = .true.
                   exit
                endif
             enddo
          endif
          !
          ! Stop reading file
          !
       else
          !
          ! Freeformatted file
          ! skip lines starting with a '*'
          ! read kc frequencies (kc including mean value) from rec132
          ! default value not allowed incase ier < 0
          !
  205     continue
          read (luntmp, '(a)', iostat = iocond) rec132
          if (iocond /= 0) then
             if (iocond < 0) then
                call prterr(lundia, 'G006', filbch(1:lfile))
             else
                call prterr(lundia, 'G007', filbch(1:lfile))
             endif
             error = .true.
             goto 200
          endif
          if (rec132(1:1) == '*') goto 205
          !
          iend = 0
          do k = 1, kc
             ibeg = iend + 1
             call read1r(rec132    ,lr132     ,ibeg      ,iend      ,omega(k)  , &
                       & rdef      ,ier       )
             !
             ! End of record (ier = 0), read new record and read omega (k)
             !
             if (ier == 0) then
                read (luntmp, '(a)', iostat = iocond) rec132
                if (iocond /= 0) then
                   if (iocond < 0) then
                      call prterr(lundia, 'G006', filbch(1:lfile))
                   else
                      call prterr(lundia, 'G007', filbch(1:lfile))
                   endif
                   error = .true.
                   goto 200
                endif
                !
                ibeg = 1
                call read1r(rec132    ,lr132     ,ibeg      ,iend      ,omega(k)  , &
                          & rdef      ,ier       )
             endif
             if (ier < 0) then
                error = .true.
                call prterr(lundia, 'G006', filbch(1:lfile))
                !
                goto 200
             endif
          enddo
          !
          ! Blank record
          !
          read (luntmp, '(a)', iostat = iocond) rec132
          if (iocond /= 0) then
             if (iocond < 0) then
                call prterr(lundia, 'G006', filbch(1:lfile))
             else
                call prterr(lundia, 'G007', filbch(1:lfile))
             endif
             error = .true.
             goto 200
          endif
          !
          ! Read per ntof kc amplitudues from rec132
          ! default value not allowed => ier    must be > 0
          !
          do n = 1, ntof
             read (luntmp, '(a)', iostat = iocond) rec132
             if (iocond /= 0) then
                if (iocond < 0) then
                   call prterr(lundia, 'G006', filbch(1:lfile))
                else
                   call prterr(lundia, 'G007', filbch(1:lfile))
                endif
                error = .true.
                goto 200
             endif
             !
             iend = 0
             do k = 1, kc
                ibeg = iend + 1
                call read1r(rec132    ,lr132     ,ibeg      ,iend      ,hydrbc(1, n, k)      , &
                          & rdef      ,ier       )
                !
                ! End of record (ier = 0), read new record and read amplitude end A
                !
                if (ier == 0) then
                   read (luntmp, '(a)', iostat = iocond) rec132
                   if (iocond /= 0) then
                      if (iocond < 0) then
                         call prterr(lundia, 'G006', filbch(1:lfile))
                      else
                         call prterr(lundia, 'G007', filbch(1:lfile))
                      endif
                      error = .true.
                      goto 200
                   endif
                   !
                   ibeg = 1
                   call read1r(rec132    ,lr132     ,ibeg      ,iend      ,hydrbc(1, n, k)      , &
                             & rdef      ,ier       )
                endif
                if (ier < 0) then
                   error = .true.
                   call prterr(lundia, 'G006', filbch(1:lfile))
                   !
                   goto 200
                endif
             enddo
          enddo
          !
          ! Now the same for end B
          !
          do n = 1, ntof
             read (luntmp, '(a)', iostat = iocond) rec132
             if (iocond /= 0) then
                if (iocond < 0) then
                   call prterr(lundia, 'G006', filbch(1:lfile))
                else
                   call prterr(lundia, 'G007', filbch(1:lfile))
                endif
                error = .true.
                goto 200
             endif
             !
             iend = 0
             do k = 1, kc
                ibeg = iend + 1
                call read1r(rec132    ,lr132     ,ibeg      ,iend      ,hydrbc(2, n, k)      , &
                          & rdef      ,ier       )
                !
                ! End of record (ier = 0), read new record and read amplitude end B
                !
                if (ier == 0) then
                   read (luntmp, '(a)', iostat = iocond) rec132
                   if (iocond /= 0) then
                      if (iocond < 0) then
                         call prterr(lundia, 'G006', filbch(1:lfile))
                      !
                      else
                         call prterr(lundia, 'G007', filbch(1:lfile))
                      !
                      endif
                      error = .true.
                      goto 200
                   endif
                   !
                   ibeg = 1
                   call read1r(rec132    ,lr132     ,ibeg      ,iend      ,hydrbc(2, n, k)      , &
                             & rdef      ,ier       )
                endif
                if (ier < 0) then
                   error = .true.
                   call prterr(lundia, 'G006', filbch(1:lfile))
                   goto 200
                endif
             enddo
          enddo
          !
          ! Blank record and phases only if kc > 1
          !
          if (kc > 1) then
             read (luntmp, '(a)', iostat = iocond) rec132
             if (iocond /= 0) then
                if (iocond < 0) then
                   call prterr(lundia, 'G006', filbch(1:lfile))
                else
                   call prterr(lundia, 'G007', filbch(1:lfile))
                endif
                error = .true.
                goto 200
             endif
             !
             ! Read per ntof phases from rec132 after reading a blank
             ! rec. first read rec132 for end A. Read kc-1 phases from
             ! rec132 default value not allowed
             !
             do n = 1, ntof
                read (luntmp, '(a)', iostat = iocond) rec132
                if (iocond /= 0) then
                   if (iocond < 0) then
                      call prterr(lundia, 'G006', filbch(1:lfile))
                   else
                      call prterr(lundia, 'G007', filbch(1:lfile))
                   endif
                   error = .true.
                   goto 200
                endif
                !
                iend = 0
                do k = 2, kc
                   ibeg = iend + 1
                   call read1r(rec132    ,lr132     ,ibeg      ,iend      ,hydrbc(3, n, k)      , &
                             & rdef      ,ier       )
                   !
                   ! End of record (ier = 0), read new record and read phases end A
                   !
                   if (ier == 0) then
                      read (luntmp, '(a)', iostat = iocond) rec132
                      if (iocond /= 0) then
                         if (iocond < 0) then
                            call prterr(lundia, 'G006', filbch(1:lfile))
                         else
                            call prterr(lundia, 'G007', filbch(1:lfile))
                         endif
                         error = .true.
                         goto 200
                      endif
                      !
                      ibeg = 1
                      call read1r(rec132    ,lr132     ,ibeg      ,iend      ,hydrbc(3, n, k)      , &
                                & rdef      ,ier       )
                   endif
                   if (ier < 0) then
                      error = .true.
                      call prterr(lundia, 'G006', filbch(1:lfile))
                      !
                      goto 200
                   endif
                enddo
             enddo
             !
             ! Now the same for end B
             !
             do n = 1, ntof
                read (luntmp, '(a)', iostat = iocond) rec132
                if (iocond /= 0) then
                   if (iocond < 0) then
                      call prterr(lundia, 'G006', filbch(1:lfile))
                   else
                      call prterr(lundia, 'G007', filbch(1:lfile))
                   endif
                   error = .true.
                   exit
                endif
                !
                iend = 0
                do k = 2, kc
                   ibeg = iend + 1
                   call read1r(rec132    ,lr132     ,ibeg      ,iend      ,hydrbc(4, n, k)      , &
                             & rdef      ,ier       )
                   !
                   ! End of record (ier = 0), read new record and read amplitude end B
                   !
                   if (ier == 0) then
                      read (luntmp, '(a)', iostat = iocond) rec132
                      if (iocond /= 0) then
                         if (iocond < 0) then
                            call prterr(lundia, 'G006', filbch(1:lfile))
                         else
                            call prterr(lundia, 'G007', filbch(1:lfile))
                         endif
                         error = .true.
                         goto 200
                      endif
                      !
                      ibeg = 1
                      call read1r(rec132    ,lr132     ,ibeg      ,iend      ,hydrbc(4, n, k)      , &
                                & rdef      ,ier       )
                   endif
                   if (ier < 0) then
                      error = .true.
                      call prterr(lundia, 'G006', filbch(1:lfile))
                      !
                      goto 200
                   endif
                enddo
             enddo
          endif
          !
          ! Stop reading file
          !
       endif
       ! 
       ! If a NaN is read -> error
       !
       do k = 1, kc
          if ( isnan(omega(k)) ) then 
             write(errmsg,'(2a)') 'NaN found in file ', filbch(1:lfile)
             call prterr(lundia, 'P004', errmsg)
             error = .true.
             goto 200
          endif
       enddo
       do k = 1, kc
          do n = 1, ntof
             do i= 1, 4
                if ( isnan(hydrbc(i,n,k)) ) then 
                   write(errmsg,'(2a)') 'NaN found in file ', filbch(1:lfile)
                   call prterr(lundia, 'P004', errmsg)
                   error = .true.
                   goto 200
                endif
             enddo
          enddo
       enddo
  200  continue
       !
       ! Close file
       !
       close (luntmp)
    else
       !
       ! File does not exist
       !
       error = .true.
    endif
end subroutine bchfil
