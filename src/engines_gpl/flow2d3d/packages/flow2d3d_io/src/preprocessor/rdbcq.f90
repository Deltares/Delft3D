subroutine rdbcq(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
               & runid     ,filbcq    ,eol       ,nambnd    ,nto       , &
               & ntof      ,ntoq      ,bubble    ,kmax      ,gdp       )
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
!    Function: - Reads the QH boundary condition records from the
!                MD-file: FILBCQ
!              - The order of reading is sequential for each
!                opening.
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
    integer , pointer :: itis
!
! Global variables
!
    integer                              :: lundia !  Description and declaration in inout.igs
    integer                              :: lunmd  !  Description and declaration in inout.igs
    integer                              :: nrrec  !!  Pointer to the record number in the
                                                   !!  MD-file
    integer                              :: nto    !  Description and declaration in esm_alloc_int.f90
    integer                              :: ntof   !  Description and declaration in dimens.igs
    integer                              :: ntoq   !  Description and declaration in dimens.igs
    integer                , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    logical                , intent(in)  :: bubble !  Description and declaration in procs.igs    
    logical                              :: error  !!  Flag=TRUE if an error is encountered
    character(*)                         :: filbcq !!  File name for the QH-rel.
                                                   !!  boundary conditions file
    character(*)                         :: mdfrec !!  Standard rec. length in MD-file (300)
    character(*)                         :: runid
    character(1)                         :: eol    !!  ASCII code for End-Of-Line (^J)
    character(20), dimension(nto)        :: nambnd !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer            :: iocond
    integer            :: lenc    ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer            :: lf      ! Help var. specifying the length of character variables for file names 
    integer            :: lkw     ! Length (in characters) of keyword 
    integer            :: lrec    ! Length of direct access records if file already exists 
    integer            :: lrid    ! Length of character string runid 
    integer            :: lunout  ! Unit number for the transformed file between tdatom and trisim 
    integer            :: lunrd
    integer            :: nlook   ! Help var.: nr. of data to look for in the MD-file 
    integer            :: ntrec   ! Help. var to keep track of NRREC 
    integer, external  :: newlun
    logical            :: ex      ! Flag to test if file exists 
    logical            :: found   ! FOUND=TRUE if KEYW in the MD-file was found 
    logical            :: lerror  ! Flag=TRUE if a local error is encountered 
    logical            :: newkw   ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical            :: noread  ! Flag if FILBCQ is equal to TMP file and should not be read. 
    character(1)       :: cdummy  ! Character help variable 
    character(12)      :: fildef  ! Default file name (usually = blank) 
    character(256)     :: filout  ! Help variable for file name 
    character(6)       :: keyw    ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM)
    character(300)     :: message
!
!! executable statements -------------------------------------------------------
!
    itis  => gdp%gdrdpara%itis
    !
    lerror = .false.
    newkw  = .true.
    found  = .false.
    noread = .false.
    nlook  = 1
    fildef = ' '
    filout = ' '
    !
    lunout = 8
    !
    ! Initialize global parameters
    !
    filbcq = ' '
    !
    ! Locate 'FilbcQ' record for QH-relations
    ! in extra input file
    !
    keyw = 'FilbcQ'
    lkw = 6
    ntrec = nrrec
    lenc = 12
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    !
    if (.not.found) then
       !
       ! QH boundary keyword not in MD file
       !
       call prterr(lundia    ,'U038'    ,keyw      )
       !
       error = .true.
       goto 9999
    endif
    !
    !
    ! Read name of file ...
    !
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,filbcq    ,fildef    ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    !
    ! Reading error?
    !
    if (lerror) then
       lerror = .false.
       filbcq = fildef
    endif
    !
    ! Read data from external file
    !
    if (filbcq/=fildef) then
       call remove_leading_spaces(runid     ,lrid      )
       filout = 'TMP_' // runid(:lrid) // '.bcq'
       !
       ! Check filename and access
       !
       if (filout==filbcq) then
          inquire (file = filout(:8 + lrid), exist = ex)
          if (.not.ex) then
             call prterr(lundia    ,'G004'    ,filout    )
             !
             error = .true.
             goto 9999
          endif
          !
          open (newunit=lunout, file = filout(:8 + lrid))
          read (lunout, '(a1,i5)', iostat = iocond) cdummy, lrec
          close (lunout)
          lunout = 8
          !
          ! Not able to read record length for direct access
          !
          if (iocond/=0) then
             call prterr(lundia    ,'U082'    ,filout    )
             !
             error = .true.
             goto 9999
          endif
          !
          ! Record length read
          !
          noread = .true.
       endif
       !
       ! define length of file name
       !
       call remove_leading_spaces(filbcq    ,lf        )
       !
       ! test file existence <YES> -> open file <NO> -> error
       !
       if (exifil(filbcq, lundia)) then
          if (.not.noread) then
             !
             ! Open FILBCQ to read data from
             !
             open (newunit=lunrd, file = filbcq(:lf), form = 'formatted',               &
                 & status = 'old')
             write (message, '(2a)') 'Reading BC-hydrodynamic file ', filbcq(:lf)
             call prterr(lundia, 'G051', trim(message))
             call rdqh(lundia    ,lunout    ,lunrd     ,error     ,filout    , &
                     & filbcq    ,runid     ,eol       ,nto       ,ntof      , &
                     & ntoq      ,nambnd    ,bubble    ,kmax      ,gdp       )
             !
             close (lunrd)
          else
             !
             ! Reading file for BC-hydrodynamic data skipped in TDATOM
             !
             write (message, '(3a)') 'BC-hydrodynamic file ', filbcq(:lf), ' will be skipped in TDATOM'
             call prterr(lundia, 'G051', trim(message))
          endif
       else
          error = .true.
       endif
    endif
    !
    ! close files
    !
 9999 continue
    if (lunout/=8) then
       if (error) then
          close (lunout, status = 'delete')
       else
          close (lunout)
       endif
    endif
end subroutine rdbcq
