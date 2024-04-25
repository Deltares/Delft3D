subroutine reacmp(lundia    ,error     ,filana    ,statns    ,nto       , &
                & ampl      ,phas      ,jacor     ,kc        ,gdp       )
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
!    Function: - Reads 'TRIANA' file or 'GETIJSYS' file
!                (filana =' ')
!              - Parameter JACOR defines if to read from a
!                correction-file or from a 'TRIANA' file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'pardef.igd'
    integer , dimension(:), pointer :: pindex
!
! Global variables
!
    integer                            , intent(in)  :: jacor  !  Flag which file has to be read
    integer                            , intent(in)  :: kc     !  actual number of components used
    integer                                          :: lundia !  Description and declaration in inout.igs
    integer                            , intent(in)  :: nto    !  Description and declaration in esm_alloc_int.f90
    logical                            , intent(out) :: error
    real(fp)     , dimension(nto*2, kc)              :: ampl   !!  Amplitudes
    real(fp)     , dimension(nto*2, kc)              :: phas   !!  Phases
    character(*)                       , intent(in)  :: filana !!  Name of the TRIANA-file or the keyword 'GETIJSYS'
    character(12), dimension(mxnto, 2)               :: statns !!  Names of referenced tidal stations
!
!
! Local variables
!
    integer                        :: i      ! Help var. 
    integer                        :: ibeg   ! Help var. 
    integer                        :: iend   ! Help var. 
    integer                        :: ier    ! Errorflag 
    integer                        :: j      ! Help var. 
    integer                        :: k      ! Help var. 
    integer                        :: l      ! Help var. 
    integer                        :: lr132  ! Standard length of a record in the attribute file = 132 
    integer                        :: lstat
    integer                        :: mcmp   ! Logical unit number 
    integer                        :: ncomp  ! Number of components 
    integer                        :: newlun
    integer                        :: nrcmp  ! Index of component 
    integer        , external      :: numcmp
    logical                        :: ex     ! Flag of existing file 
    real(fp)                       :: amp    ! Amplitude 
    real(fp)                       :: fas    ! Phase 
    real(fp)                       :: rdef
    character(12)                  :: string
    character(132)                 :: rec132 ! Standard rec. length in an attribute file (132) 
    character(300)                 :: chulp  ! help character string 
    character(8)                   :: name   ! Name of tidal component 
!
!
!! executable statements -------------------------------------------------------
!
    pindex => gdp%gdbcdat%pindex
    !
    !
    !
    lr132 = 132
    lstat = 20
    if (jacor==1 .or. (jacor==0 .and. filana/=' ')) then
       !
       ! Read in this way when it is a correctionsfile,
       ! (jacor is 1), or when it is not a correctionsfile,
       ! but it is a triana file.
       !
       inquire (file = filana, exist = ex)
       if (ex) then
          open (newunit=mcmp, file = filana)
       else
          !
          ! Triana file not found
          !
          error = .true.
          chulp = filana
          if (filana==' ') chulp = '--Triana--'
          call prterr(lundia    ,'G004'    ,chulp     )
          !
          goto 9999
       endif
       !
       k = 0
       do i = 1, nto
          do j = 1, 2
             k = k + 1
             call search2(mcmp      ,statns(i, j)         ,ier       ,rec132    )
             if (ier== - 1) then
                if (jacor==0) then
                   !
                   ! Station name not found in triana file
                   !
                   error = .true.
                   chulp = ' Station ' // statns(i, j)                          &
                         & // ' not found in file ' // filana
                   call prterr(lundia    ,'U021'    ,chulp     )
                   !
                   goto 9999
                !
                ! continue with correctionsfile
                !
                elseif (jacor==1) then
                else
                endif
             else
   20           continue
                read (mcmp, '(a)', end = 10) rec132
                if (rec132(1:1)=='*' .or. rec132(1:1)=='+' .or. rec132(1:1)     &
                  & ==' ') goto 20
                ibeg = 1
                call read1c(rec132    ,lr132     ,ibeg      ,iend      ,string    , &
                          & lstat     ,ier       )
                !
                ! Empty line, continue reading
                !
                if (ier==0) goto 20
                !
                ! Check whether string is a component name
                !
                name = string(1:8)
                nrcmp = numcmp(name)
                if (nrcmp>=0) then
                   !
                   ! If so, read amplitude for component nrcmp
                   !
                   ibeg = iend + 1
                   call read1r(rec132    ,lr132     ,ibeg      ,iend      ,ampl(k, pindex(nrcmp))    , &
                             & rdef      ,ier       )
                   if (ier<=0) then
                      !
                      ! No amplitude or phase given for component name
                      !
                      error = .true.
                      chulp = ' No amplitude or phase given for ' //            &
                             & ' component ' // name // ' at ' // statns(i, j)
                      call prterr(lundia    ,'U021'    ,chulp     )
                      !
                      goto 9999
                   endif
                   if (name(1:2)/='A0') then
                      !
                      ! And read the phase, if name is not A0
                      !
                      ibeg = iend + 1
                      call read1r(rec132    ,lr132     ,ibeg      ,iend      ,phas(k, pindex(nrcmp))       , &
                                & rdef      ,ier       )
                      if (ier<=0) then
                         !
                         ! No amplitude or phase given for component name
                         !
                         error = .true.
                         chulp = ' No amplitude or phase given for' //          &
                                & ' component ' // name // ' at ' // statns(i, j)
                         call prterr(lundia    ,'U021'    ,chulp     )
                         !
                         goto 9999
                      endif
                   endif
                   !
                   ! And continue to check whether more components are given
                   !
                   goto 20
                !
                ! string is no component => next
                !
                endif
             endif
   10     enddo
       enddo
    !
    else
       !
       ! Read getijsys files
       !
       k = 0
       do i = 1, nto
          do j = 1, 2
             inquire (file = statns(i, j), exist = ex)
             if (ex) then
                open (newunit=mcmp, file = statns(i, j))
             else
                !
                ! Getijsys file not found
                !
                chulp = ' Getijsys file ' // statns(i, j) // ' not found '
                call prterr(lundia    ,'U021'    ,chulp     )
                !
                error = .true.
                goto 9999
             endif
             k = k + 1
             !
  100        continue
             read (mcmp, '(a)', end = 4444) rec132
             if (rec132(21:30)/='MEAN LEVEL') goto 100
             read (rec132(35:45), '(f11.6)') ampl(k, pindex(0))
             !
  110        continue
             read (mcmp, '(a)', end = 4444) rec132
             if (rec132(1:20)/='NUMBER OF COMPONENTS') goto 110
             read (rec132(25:30), '(i6)') ncomp
             !
             read (mcmp, '(a)', end = 4444)
             read (mcmp, '(a)', end = 4444)
             read (mcmp, '(a)', end = 4444)
             !
             do l = 1, ncomp
                read (mcmp, '(a8,f10.3,f10.1)', end = 4444) name, amp, fas
                nrcmp = numcmp(name)
                ampl(k, pindex(nrcmp)) = amp
                phas(k, pindex(nrcmp)) = fas
             enddo
             close (mcmp)
          enddo
       enddo
       !
       ! Skip errorprint
       !
       goto 9999
       !
 4444  continue
       error = .true.
       !
       ! Reading problem in getijsys file
       !
       chulp = 'Getijsys ' // statns(i, j)
       call prterr(lundia    ,'G006'    ,chulp     )
       !
       close (mcmp)
    endif
    !
    !
 9999 continue
end subroutine reacmp
