subroutine rdqh(lundia    ,lunout    ,lunrd     ,error     ,filout    , &
              & filbcq    ,runid     ,eol       ,nto       ,ntof      , &
              & ntoq      ,nambnd    ,bubble    ,kmax      ,gdp       )
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
!    Function: Reads QH boundary condition from the .bcq file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'pardef.igd'
    character*20, dimension(:) , pointer :: keywrd
!
! Global variables
!
    integer                                    :: lundia !  Description and declaration in inout.igs
    integer                                    :: lunout !!  Unit number for unformatted FLOW
                                                         !!  help file between tdatom and trisim
    integer                                    :: lunrd  !!  Unit number of the attribute file
                                                         !!  containing the time series
    integer                      , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                      , intent(in)  :: nto    !  Description and declaration in esm_alloc_int.f90
    integer                      , intent(in)  :: ntof   !  Description and declaration in dimens.igs
    integer                      , intent(in)  :: ntoq   !  Description and declaration in dimens.igs
    logical                      , intent(in)  :: bubble !  Description and declaration in procs.igs        
    logical                                    :: error  !!  Flag=TRUE if an error is encountered
    character(*)                               :: filbcq !!  File name for the time varying
                                                         !!  boundary conditions file
    character(*)                               :: filout !!  Name of the output file to be opened
    character(*)                 , intent(in)  :: runid
    character(1)                 , intent(in)  :: eol    !!  ASCII code for End-Of-Line (^J)
    character(20), dimension(nto)              :: nambnd !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                                :: ibeg    ! Begin position in the RECORD from where the search for data/record is started 
    integer                                :: idummy
    integer                                :: iend    ! Last position in the RECORD when the searched data/record is finished 
    integer                                :: ier     ! =  0 -> end of record encountered =  1 -> real value found = -1 -> length or number of data is larger then specified by the calling routine 
    integer                                :: iocond  ! IO status for reading 
    integer                                :: irec
    integer                                :: irecrd  ! Counter of records if input file is a direct access file 
    integer                                :: lflout  ! Actual length of FILOUT 
    integer                                :: lrecrd  ! Length of record read 
    integer                                :: mxlrec  ! Actual maximum record length (:= KMAX*24*2 + 24) 
    integer                                :: n       ! Help var. for the nr. of data to be read (see LENDAT) 
    integer                                :: nn
    integer                                :: np
    integer                                :: npara
    integer                                :: nparrd
    integer                                :: nqhrd
    integer                                :: nr      ! Sequence nr of the time read 
    integer, external                      :: newlun
    logical                                :: access  ! Flag to read file as direct access or sequential 
    logical                                :: ex      ! Flag to test if file exists 
    real(fp)                               :: qold    ! Help var. to store last read discharg to test accending order 
    real(fp)                               :: qrd     ! Discharge read from input file 
    real(fp)                               :: rdfaul
    real(fp)                               :: rdummy
    real(fp)                               :: zold    ! Help var. to store last read waterlev to test increasing Z for (absolute) value of Q increasing 
    real(fp)                               :: zrd     ! Waterlevel read from input file 
    character(1)                           :: interp
    character(1)                           :: quote   ! Apostrophe ASCII-character 39 
    character(10), dimension(2)            :: parunt  ! Unit name fitting the parameter 
    character(20)                          :: cntent
    character(36), dimension(1 + 2*kmax)   :: parrd   ! Parameter names read 
    character(36), dimension(2)            :: parnam  ! Names of the paramaters to write to BCQ file 
    character(37), dimension(13)           :: fmtbcq
    character(300)                         :: errmsg ! Character var. containing the error message to be written to file. The message depends on the error.
    character(42)                          :: tablnm  ! Table name specification 
    character(kmax*24*2 + 48)              :: record  ! Standard rec. length in an attribute file (maximum kmax*24*2 + 48) 
    !
    !
    data parnam/'total discharge (t) ', 'water elevation (z) '/
    data parunt/'[ m**3/s ]', '[   m    ]'/
    data fmtbcq/'(a1,i5,                     t83  ,a1)',                        &
        & '(a20,a1,a42,a3,a1,          t83  ,a1)',                               &
        & '(2(a20,a1),a40,             t83  ,a1)',                               &
        & '(2(a20,a1),                 t83  ,a1)',                               &
        & '(a20,                       t83  ,a1)',                               &
        & '(a20,                       t83  ,a1)',                               &
        & '(a20,a1,a15,a1,             t83  ,a1)',                               &
        & '(a20,i9,                    t83  ,a1)',                               &
        & '(a20,a1,a7,a1,              t83  ,a1)',                               &
        & '(a20,a1,a6,a1,              t83  ,a1)',                               &
        & '(a20,a1,a36,a1,1x,2(a10,a1),t83  ,a1)',                               &
        & '(a20,i6,                    t83  ,a1)',                               &
        & '(2g14.6,                    t83  ,a1)'/
!
!! executable statements -------------------------------------------------------
!
    ! >>>>>for now reading the direct access file not possible
    !
    keywrd  => gdp%gdkeywtd%keywrd
    !
    access = .false.
    irecrd = 0
    !
    quote  = char(39)
    interp = 'Y'
    irec   = 0
    !
    call remove_leading_spaces(filout    ,lflout    )
    !
    ! Start reading the QH relations.
    !
    do nn = 1, ntoq
       n = ntof + nn
       read (lunrd, '(a)', iostat = iocond) record
       !
       ! Check for EOL in file and find maximum number of characters
       ! to read if EOL is located. Only for UNIX systems where EOL = ^J
       !
       lrecrd = len(record)
       if (eol/=' ' .and. index(record, eol)/=0) lrecrd = index(record, eol)
       !
       ! Premature EOR encountered or error (IOCOND <> 0)
       ! Test last discharge read
       ! Error message not completely correct
       !
       if (iocond/=0) then
          error = .true.
          call prterr(lundia    ,'G007'    ,filbcq    )
          !
          if (iocond<0) then
             if (nn<ntoq) then
                call prterr(lundia    ,'U021'    ,'not for all QH boundaries BCQ data defined'          )
             !
             endif
          endif
       endif
       !
       ! Read all keywords prior to QH table records
       ! Define index number for parameter name
       ! PARNAM = <total discharge (t) >  <water-elevation (z) >
       !
       npara = 2
       idummy = 0
       rdummy = 0.0
       call flhnew(lunrd     ,lundia    ,error     ,record(:lrecrd)      ,access    , &
                 & irecrd    ,nambnd(n) ,cntent    ,interp    ,idummy    , &
                 & rdummy    ,nqhrd     ,parrd     ,npara     ,nparrd    , &
                 & bubble    ,gdp       )
       !
       if (error) then
          exit
       endif
       !
       ! Test number of parameters read incombination with defined
       !
       if (nparrd/=2) then
          call prterr(lundia    ,'V097'    ,' '       )
          !
          error = .true.
          exit
       endif
       !
       ! Test name of constituent conform read parameter name
       !
       do np = 1, nparrd
          if (parrd(np)(1:20)/=parnam(np)(1:20)) then
             call prterr(lundia    ,'V096'    ,parrd(np) )
             !
             error = .true.
             goto 9999
          endif
       enddo
       !
       ! Determine the record length for direct access file and the
       ! corresponding format
       !
       if (nn==1) then
          mxlrec = 83
          !
          ! Open output file
          ! and write length direct access file to file first
          !
          inquire (file = filout(:lflout), exist = ex)
          if (ex) then
             open (newunit=lunout, file = filout(:lflout))
             close (lunout, status = 'delete')
          endif
          open (newunit=lunout, file = filout(:lflout), form = 'formatted',             &
               & access = 'direct', status = 'unknown', recl = mxlrec)
          !
          irec = 1
          write (lunout, fmtbcq(1), rec = irec) '#', mxlrec, eol
       endif
       !
       ! Define table name
       !
       tablnm = 'QH-relation ' // nambnd(n) // ' for run: '
       !
       ! Write first 7 description records to file
       !
       write (lunout, fmtbcq(2), rec = irec + 1) &
           & keywrd(1), quote, tablnm, runid, quote, eol
       write (lunout, fmtbcq(4), rec = irec + 2) &
           & keywrd(3), quote, nambnd(n), quote, eol
       write (lunout, fmtbcq(10), rec = irec + 3) &
           & keywrd(12), quote, 'linear', quote, eol
       write (lunout, fmtbcq(11), rec = irec + 4) &
           & keywrd(14), quote, parnam(1), quote, keywrd(15)(:10), quote, parunt(1), quote, eol
       write (lunout, fmtbcq(11), rec = irec + 5) &
           & keywrd(14), quote, parnam(2), quote, keywrd(15)(:10), quote, parunt(2), quote, eol
       !
       ! Write number of QH records
       !
       write (lunout, fmtbcq(12), rec = irec + 6) keywrd(16), nqhrd, eol
       !
       ! Re-define IREC 6 description records forward
       !
       irec = irec + 6
       !
       ! Read time dependent data for open boundary N (NTOF+NN)
       ! for all times NQHRD on file
       !
       do nr = 1, nqhrd
          !
          ! Read discharge
          !
          ibeg = 1
          call read1r(record    ,lrecrd    ,ibeg      ,iend      ,qrd       , &
                    & rdfaul    ,ier       )
          !
          ! Premature EOR (IER = 0) or empty value (IER = -1)
          !
          if (ier<=0) then
             error = .true.
             call prterr(lundia    ,'G007'    ,filbcq    )
             !
             write (lundia, '(a,a)') 'RECORD: ', record(:72)
             goto 9999
          endif
          !
          ! If an NaN is read -> error
          !
          if ( isnan(qrd) ) then 
            write(errmsg,'(a,a)') 'NaN in ',filbcq
            call prterr(lundia    ,'P004'    ,errmsg      )
            error = .true.
            goto 9999
          endif
          !
          ! Perform some checks on discharge value read
          !
          if (nr==1) then
             qold = qrd
          elseif (qrd<qold) then
             error = .true.
             call prterr(lundia    ,'U021'    ,'Discharges in QH relation must be increasing.'       )
             !
             write (lundia, '(a,a)') 'RECORD: ', record(:72)
             goto 9999
          else
          endif
          !
          ! H (=Z) value
          !
          ibeg = iend + 1
          call read1r(record    ,lrecrd    ,ibeg      ,iend      ,zrd       , &
                    & rdfaul    ,ier       )
          !
          ! If an NaN is read -> error
          !
          if ( isnan(zrd) ) then 
            write(errmsg,'(a,a)') 'NaN in ',filbcq
            call prterr(lundia    ,'P004'    ,errmsg      )
            error = .true.
            goto 9999
          endif
          !
          ! Perform some checks on waterlevel value read
          !
          if (nr==1) then
             zold = zrd
          elseif (((zrd<zold) .and. (qrd>0)) .or. ((zrd>zold) .and. (qrd<0)))   &
                & then
             error = .true.
             call prterr(lundia    ,'U021'    ,'Waterlevel must increase for increasing abs(Q).'     )
             !
             write (lundia, '(a,a)') 'RECORD: ', record(:72)
             goto 9999
          else
          endif
          !
          ! Premature EOR (IER = 0) or empty value (IER = -1)
          !
          if (ier<=0) then
             error = .true.
             call prterr(lundia    ,'G007'    ,filbcq    )
             !
             write (lundia, '(a,a)') 'RECORD: ', record(:72)
             goto 9999
          endif
          !
          ! Write QH data
          !
          write (lunout, fmtbcq(13), rec = irec + nr) qrd, zrd, eol
          !
          ! Re-define QOLD and
          ! As long as NR < NQHRD read new RECORD
          !
          qold = qrd
          if (nr<=nqhrd - 1) read (lunrd, '(a)') record(:lrecrd)
       enddo
       !
       ! Re-define IREC NQHRD records forward
       !
       irec = irec + nqhrd
    enddo
    !
    ! Stop reading file
    !
    !
 9999 continue
end subroutine rdqh
