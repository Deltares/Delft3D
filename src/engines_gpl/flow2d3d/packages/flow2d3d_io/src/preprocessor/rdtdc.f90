subroutine rdtdc(lundia    ,lunout    ,lunrd     ,error     ,filbcc    , &
               & runid     ,profil    ,eol       ,itstrt    ,itfinish  , &
               & nto       ,lstsc     ,nambnd    ,namcon    ,gdp       )
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
!    Function: Reads time dependent constituent data from old
!              bc-file for the constituents
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
    integer                    , pointer :: itdate
    real(fp)                   , pointer :: dt
    character*20, dimension(:) , pointer :: keywrd
    character*38, dimension(:) , pointer :: fmtbcc
!
! Global variables
!
    integer                         , intent(in)  :: itfinish   !  Description and declaration in inttim.igs
    integer                         , intent(in)  :: itstrt     !  Description and declaration in inttim.igs
    integer                         , intent(in)  :: lstsc      !  Description and declaration in dimens.igs
    integer                         , intent(in)  :: lundia     !  Description and declaration in inout.igs
    integer                         , intent(in)  :: lunout     !  Unit number for unformatted FLOW help file between tdatom and trisim
    integer                                       :: lunrd      !  Unit number of the attribute file containing the time series
    integer                         , intent(in)  :: nto        !  Description and declaration in esm_alloc_int.f90
    logical                         , intent(out) :: error      !  Flag=TRUE if an error is encountered
    character(*)                    , intent(in)  :: filbcc     !  File name for the time varying data on boundaries for processes file
    character(*)                    , intent(in)  :: runid      !  Run identification code for the current simulation (used to determine
                                                                !  the names of the in- /output files used by the system)
    character(1)                    , intent(in)  :: eol        !  ASCII code for End-Of-Line (^J)
    character(20), dimension(lstsc) , intent(in)  :: namcon     !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nto)   , intent(in)  :: nambnd     !  Description and declaration in esm_alloc_char.f90
    character(40)                   , intent(in)  :: profil     !  Total string of possible profiles
!
!
! Local variables
!
    integer                                 :: ib
    integer                                 :: ibeg                 ! Begin position in the RECORD from where the search for data/record is started 
    integer                                 :: iend                 ! Last position in the RECORD when the searched data/record is finished 
    integer                                 :: ier                  ! =  0 -> end of record encountered =  1 -> real value found = -1 -> length or number of data is larger then specified by the calling routine 
    integer                                 :: inprof               ! Index number of first character in PROFIL string of TPROFC definition 
    integer                                 :: irec
    integer                                 :: itold                ! Help var. to store last read time to test accending order 
    integer                                 :: j
    integer                                 :: l
    integer                                 :: lenc                 ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                                 :: lrec                 ! Actual maximum record length (:= 2*24*2+24+LENC) 
    integer                                 :: n                    ! Help var. for the nr. of data to be read (see LENDAT) 
    integer                                 :: nbcctm               ! Nr. of times read 
    integer                                 :: nlook                ! Nr. of data to look for in RECORD 
    integer                                 :: nrskip
    integer         , dimension(0:7)        :: ntpara               ! Total number of parameter records 
    integer         , dimension(7)          :: npara                ! Number of parameter records 
    real(fp)                                :: rdfaul               ! Default value when RARRAY(N) not found 
    real(fp)                                :: timrd                ! Time in minutes read 
    real(fp)        , dimension(4, mxnto, 7):: rwbval               ! Array for the time dependent data 1,N,L = value at surface, at A 2,N,L = value at bottom , at A 3,N,L = value at surface, at B 4,N,L = value at bottom , at B 
    real(fp)        , dimension(5)          :: rarray               ! Help array to read 5 values from RECORD 
    real(fp)        , dimension(mxnto, 7)   :: zstep                ! Time varying location of disconti- nuity for the 3D BC for constituents 
    character(1)                            :: quote                ! Apostrophe ASCII-character 39 
    character(10)                           :: chulp
    character(10)   , dimension(2)          :: parunt               ! Unit name fitting the parameter 
    character(10)   , dimension(mxnto, 7)   :: tprofc               ! Shape function for constituent - uniform - linear - step - 3d-profile 
    character(132)                          :: record               ! Standard rec. length in an attribute file (maximum 2*24*2 + 24 + LENC) 
    character(36)   , dimension(2)          :: parnam               ! Names of the paramaters to write to time dependent files for BCC 
    character(40)                           :: cntain
    character(400)                          :: errmsg               ! Character var. containing the error message to be written to file. The message depends on the error.
    character(63)                           :: tablnm               ! Table name specification 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    fmtbcc  => gdp%gdfmtbcc%fmtbcc
    keywrd  => gdp%gdkeywtd%keywrd
    itdate  => gdp%gdexttim%itdate
    dt      => gdp%gdexttim%dt
    !
    quote = char(39)
    cntain = ' # at ends A&B of open boundary segment '
    lenc = 10
    nlook = 5
    !
    !-----Define parameter name for time column
    !
    parnam(1) = 'Time starting at ITDATE = 0.0       '
    parunt(1) = '[   min  ]'
    ntpara(0) = 0
    !
    !-----Count number of records in the file
    !     File is build with one time record and NTO*LSTSC value records
    !     Number of records to skip is NTO*LSTSC. In RDTOLD the value of
    !     NRSKIP will be substracted (-1) so here call 1 to large
    !
    itold = -1
    nbcctm = 0
    nrskip = nto*lstsc + 1
    call rdtold(lunrd     ,lundia    ,error     ,filbcc    ,nbcctm    , &
              & nrskip    ,dt        ,itstrt    ,itfinish  ,itold     , &
              & gdp       )
    !
    if (error) goto 9999
    !
    !-----Start reading the data
    !
    rewind (lunrd)
    do ib = 1, nbcctm
       !
       !--------Initialize RWBVAL array
       !
       do n = 1, nto
          do l = 1, lstsc
             rwbval(1, n, l) = 0.0
             rwbval(2, n, l) = 0.0
             rwbval(3, n, l) = 0.0
             rwbval(4, n, l) = 0.0
             zstep(n, l) = 0.0
          enddo
       enddo
       !
       !--------Read file
       !
       read (lunrd, *) timrd
       !
       !--------read profile, step value & concentrations for all open
       !        boundaries and for all constituents
       !
       do n = 1, nto
          do l = 1, lstsc
             record = ' '
             read (lunrd, '(a)') record
             chulp = record(:lenc)
             !
             !--------------define name in small characters, safe original name in
             !              case of an error (first entry)
             !
             if (ib==1) then
                tprofc(n, l) = chulp
                call small(tprofc(n, l)         ,lenc      )
                !
                !-----------------check for undefined profile definition
                !
                inprof = index(profil, tprofc(n, l))
                if (inprof==0) then
                   call prterr(lundia    ,'U066'    ,chulp     )
                   !
                   tprofc(n, l) = 'uniform   '
                endif
                !
                !-----------------check for <3d-profile>, not allowed in old file
                !
                if (tprofc(n, l)=='3d-profile') then
                   call prterr(lundia    ,'V095'    ,'for v248 or less'   )
                   !
                   error = .true.
                   goto 9999
                endif
                !
                !-----------------Define number of parameter records
                !
                if (tprofc(n, l)(:7)=='uniform') then
                   ntpara(l) = ntpara(l - 1) + 3
                elseif (tprofc(n, l)(:6)=='linear') then
                   ntpara(l) = ntpara(l - 1) + 5
                elseif (tprofc(n, l)(:4)=='step') then
                   ntpara(l) = ntpara(l - 1) + 6
                else
                endif
             else
                !
                !-----------------check for changing of profile definition in time
                !                 not allowed
                !
                call small(chulp     ,lenc      )
                if (chulp/=tprofc(n, l)) then
                   error = .true.
                   call prterr(lundia    ,'U066'    ,record(:lenc)        )
                   !
                   goto 9999
                endif
             endif
             !
             !--------------NLOOK values for open boundary point A&B
             !              Old file has always 5 values defined
             !
             ibeg = lenc + 1
             lrec = 5*24 + lenc
             call readnr(record    ,lrec      ,ibeg      ,iend      ,nlook     , &
                       & rarray    ,rdfaul    ,ier       )
             !
             !--------------End-of-Record (IER = 0) or default value (IER = -1)
             !              are not allowed
             !
             if (ier<=0) then
                error = .true.
                call prterr(lundia    ,'G007'    ,filbcc    )
                !
                write (lundia, '(a,a)') 'RECORD: ', record(ibeg:132)
                goto 9999
             endif
             !
             !--------------Fill values to respective array's
             !
             zstep(n, l) = rarray(1)
             rwbval(1, n, l) = rarray(2)
             rwbval(2, n, l) = rarray(3)
             rwbval(3, n, l) = rarray(4)
             rwbval(4, n, l) = rarray(5)
             !
             !--------------check for negative values
             !
             if (rwbval(1, n, l)<0.0) error = .true.
             if (rwbval(2, n, l)<0.0) error = .true.
             if (rwbval(3, n, l)<0.0) error = .true.
             if (rwbval(4, n, l)<0.0) error = .true.
             if (error) then
                write(errmsg,'(a,a)') 'Concentration at open boundary in ', trim(filbcc)
                call prterr(lundia    ,'V061'    ,errmsg)
                !
                goto 9999
             endif
             !
             ! If an NaN is read -> error
             !
             if (isnan(rwbval(1, n, l))) error = .true.
             if (isnan(rwbval(2, n, l))) error = .true.
             if (isnan(rwbval(3, n, l))) error = .true.
             if (isnan(rwbval(4, n, l))) error = .true.
             if (error) then
                write(errmsg,'(a,a)') 'Concentration at open boundary contains NaN in ', trim(filbcc)
                call prterr(lundia    ,'P004'    ,errmsg      )
                !
                goto 9999
             endif
             
             !
             !--------------Write first 8+NPARA(L) records with block information
             !              for time IB=1
             !
             irec = (n - 1)*(lstsc*(nbcctm + 8) + ntpara(lstsc)) + (l - 1)      &
                  & *(nbcctm + 8) + ntpara(l - 1) + 1
             if (ib==1) then
                !
                !-----------------Define table name
                !
                tablnm = 'T-serie BCC ' // nambnd(n) // '-' // namcon(l)        &
                        & // ' for run: '
                !
                !-----------------Write first 7 description records to file
                !
                write (lunout, fmtbcc(2), rec = irec + 1) &
                    & keywrd(1), quote, tablnm, runid, quote, eol
                write (lunout, fmtbcc(3), rec = irec + 2) &
                    & keywrd(2), quote, tprofc(n, l), quote, cntain, eol
                write (lunout, fmtbcc(4), rec = irec + 3) &
                    & keywrd(3), quote, nambnd(n), quote, eol
                write (lunout, fmtbcc(7), rec = irec + 4) &
                    & keywrd(8), quote, 'non-equidistant', quote, eol
                write (lunout, fmtbcc(8), rec = irec + 5) keywrd(9), itdate, eol
                write (lunout, fmtbcc(9), rec = irec + 6) &
                    & keywrd(10), quote, 'minutes', quote, eol
                write (lunout, fmtbcc(10), rec = irec + 7) &
                    & keywrd(12), quote, 'linear', quote, eol
                !
                !-----------------Write parameter name for time to file
                !
                npara(l) = 1
                write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                    & ) keywrd(14) , quote, parnam(1), quote, keywrd(15) (:10),  &
                    & quote, parunt(1), quote, eol
                !
                !-----------------Define parameter name for constituent L
                !
                parnam(2)(:20) = namcon(l)
                parunt(2) = '[    -   ]'
                if (parnam(2)(:8)=='salinity') parunt(2) = '[   ppt  ]'
                if (parnam(2)(:11)=='temperature') parunt(2) = '[   deg  ]'
                !
                !-----------------Write parameter names for profile <uniform>
                !
                if (tprofc(n, l)(:7)=='uniform') then
                   npara(l) = npara(l) + 1
                   parnam(2)(21:) = ' end A uniform  '
                   write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                       & ) keywrd( 14), quote, parnam (2), quote, keywrd (15) (:10),  &
                       & quote, parunt (2), quote, eol
                   npara(l) = npara(l) + 1
                   parnam(2)(21:) = ' end B uniform  '
                   write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                       & ) keywrd( 14), quote, parnam (2), quote, keywrd (15) (:10),  &
                       & quote, parunt (2), quote, eol
                !
                !-----------------Write parameter names for profile <linear>
                !
                elseif (tprofc(n, l)(:6)=='linear') then
                   npara(l) = npara(l) + 1
                   parnam(2)(21:) = ' end A surface  '
                   write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                       & ) keywrd( 14), quote, parnam (2), quote, keywrd (15) (:10),  &
                       & quote, parunt (2), quote, eol
                   npara(l) = npara(l) + 1
                   parnam(2)(21:) = ' end A bed      '
                   write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                       & ) keywrd( 14), quote, parnam (2), quote, keywrd (15) (:10),  &
                       & quote, parunt (2), quote, eol
                   !
                   npara(l) = npara(l) + 1
                   parnam(2)(21:) = ' end B surface  '
                   write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                       & ) keywrd( 14), quote, parnam (2), quote, keywrd (15) (:10),  &
                       & quote, parunt (2), quote, eol
                   npara(l) = npara(l) + 1
                   parnam(2)(21:) = ' end B bed      '
                   write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                       & ) keywrd( 14), quote, parnam (2), quote, keywrd (15) (:10),  &
                       & quote, parunt (2), quote, eol
                !
                !-----------------Write parameter names for profile <step>
                !
                elseif (tprofc(n, l)(:4)=='step') then
                   npara(l) = npara(l) + 1
                   parnam(2)(21:) = ' end A surface  '
                   write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                       & ) keywrd( 14), quote, parnam (2), quote, keywrd (15) (:10),  &
                       & quote, parunt (2), quote, eol
                   npara(l) = npara(l) + 1
                   parnam(2)(21:) = ' end A bed      '
                   write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                       & ) keywrd( 14), quote, parnam (2), quote, keywrd (15) (:10),  &
                       & quote, parunt (2), quote, eol
                   !
                   npara(l) = npara(l) + 1
                   parnam(2)(21:) = ' end B surface  '
                   write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                       & ) keywrd( 14), quote, parnam (2), quote, keywrd (15) (:10),  &
                       & quote, parunt (2), quote, eol
                   npara(l) = npara(l) + 1
                   parnam(2)(21:) = ' end B bed      '
                   write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                       & ) keywrd( 14), quote, parnam (2), quote, keywrd (15) (:10),  &
                       & quote, parunt (2), quote, eol
                   !
                   npara(l) = npara(l) + 1
                   parnam(2) = 'discontinuity'
                   parunt(2) = '[    m   ]'
                   write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                       & ) keywrd( 14), quote, parnam (2), quote, keywrd (15) (:10),  &
                       & quote, parunt (2), quote, eol
                else
                endif
                !
                !-----------------Write number of time dependent data to file
                !
                write (lunout, fmtbcc(12), rec = irec + 8 + npara(l) &
                    & ) keywrd(16) , nbcctm, eol
             endif
             !
             !--------------Write time dependent data to block for constituent
             !              L skipping first 8+NPARA(L) records with block info
             !
             irec = irec + ib + 8 + npara(l)
             if (tprofc(n, l)(:7)=='uniform') then
                fmtbcc(13)(8:10) = '  2'
                write (lunout, fmtbcc(13), rec = irec) &
                    & timrd, rwbval(1, n, l), rwbval(3, n, l), eol
             elseif (tprofc(n, l)(:6)=='linear') then
                fmtbcc(13)(8:10) = '  4'
                write (lunout, fmtbcc(13), rec = irec) &
                    & timrd, (rwbval(j, n, l), j = 1, 4), eol
             elseif (tprofc(n, l)(:4)=='step') then
                fmtbcc(13)(8:10) = '  5'
                write (lunout, fmtbcc(13), rec = irec) &
                    & timrd, (rwbval(j, n, l), j = 1, 4), zstep(n, l), eol
             else
             endif
          enddo
       enddo
    enddo
    !
    !-----close file and define maximum time
    !
 9999 continue
    close (lunrd)
    if (itold/= - 1) then
       if (itold < itfinish) then
          write(errmsg,'(a,a,a)') 'Last time in file ', trim(filbcc), ' <' 
          call prterr(lundia    ,'U042'    ,errmsg)
          error = .true.
       endif
    endif
end subroutine rdtdc
