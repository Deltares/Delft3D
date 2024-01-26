subroutine rdtdt(lundia    ,lunout    ,lunrd     ,error     ,filbct    , &
               & runid     ,typtst    ,eol       ,itstrt    ,itfinish  , &
               & nto       ,ntot      ,ntof      ,tprofu    ,nambnd    , &
               & typbnd    ,namtyp    ,unttyp    ,gdp       )
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
!    Function: reads the time dependent hydro dynamic data from
!              attribute file (old and new)
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                    , pointer :: itdate
    real(fp)                   , pointer :: dt
    character(20), dimension(:), pointer :: keywrd
    character(37), dimension(:), pointer :: fmtbct
!
! Global variables
!
    integer                                    :: itfinish !  Description and declaration in inttim.igs
    integer                                    :: itstrt   !  Description and declaration in inttim.igs
    integer                                    :: lundia   !  Description and declaration in inout.igs
    integer                      , intent(in)  :: lunout   !  Unit number for the transformed output file
    integer                                    :: lunrd    !  Unit number of the attribute file containing the time series
    integer                      , intent(in)  :: nto      !  Description and declaration in esm_alloc_int.f90
    integer                      , intent(in)  :: ntof     !  Description and declaration in dimens.igs
    integer                      , intent(in)  :: ntot     !  Description and declaration in dimens.igs
    logical                                    :: error    !  Flag=TRUE if an error is encountered
    character(*)                               :: filbct   !  File name for the time varying boundary conditions file
    character(*)                 , intent(in)  :: runid
    character(1)                 , intent(in)  :: eol      !  ASCII code for End-Of-Line (^J)
    character(1) , dimension(nto), intent(in)  :: typbnd   !  Description and declaration in esm_alloc_char.f90
    character(10), dimension(6)  , intent(in)  :: unttyp   !  Unit name fitting the parameter depending on value of TYPBND
    character(20), dimension(6)  , intent(in)  :: namtyp   !  Names of the paramaters to write to  time dependent files for BCT depending on value of TYPBND
    character(20), dimension(nto), intent(in)  :: nambnd   !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nto), intent(in)  :: tprofu   !  Description and declaration in esm_alloc_char.f90
    character(6)                 , intent(in)  :: typtst   !  Data string to test type of boundary
!
! Local variables
!
    integer                     :: ib
    integer                     :: irec
    integer                     :: i         
    integer                     :: imint  ! Help var. to store first read time 
    integer                     :: itold  ! Help var. to store last read time to test accending order 
    integer                     :: n      ! Help var. for the nr. of data to be read (see LENDAT) 
    integer                     :: nbcttm
    integer                     :: nrskip
    integer                     :: ntyp
    real(fp)                    :: timrd  ! Time in minutes read 
    real(fp)     , dimension(2) :: rwbval
    character(40)               :: errmsg ! Character var. containing the error message to be written to file. The message depend on the error. 
    character(1)                :: quote  ! Apostrophe ASCII-character 39 
    character(10), dimension(2) :: parunt ! Unit name fitting the parameter 
    character(36), dimension(2) :: parnam ! Names of the paramaters to write to time dependent files for BCT 
    character(40)               :: cntain
    character(42)               :: tablnm ! Table name specification 
!
!! executable statements -------------------------------------------------------
!
    fmtbct  => gdp%gdfmtbct%fmtbct
    keywrd  => gdp%gdkeywtd%keywrd
    itdate  => gdp%gdexttim%itdate
    dt      => gdp%gdexttim%dt
    !
    quote  = char(39)
    cntain = ' # at ends A&B of open boundary segment '
    !
    ! Define parameter name for time column
    !
    parnam(1) = 'Time starting at ITDATE = 0.0       '
    parunt(1) = '[   min  ]'
    !
    ! Test profile definition, because <3d-profile> is not allowed for old file
    !
    do n = 1, ntot
       if (tprofu(ntof + n)(:10) == '3d-profile') then
          call prterr(lundia, 'V095', 'for v248 or less')
          !
          error = .true.
          goto 9999
       endif
    enddo
    !
    ! Count number of records in the file
    ! File is build with 2*NTOT time records
    ! Number of records to skip is 2*NTOT-1. In RDTOLD the value of
    ! NRSKIP will be substracted (-1) so here call 2*NTOT
    !
    itold = -1
    nbcttm = 0
    nrskip = ntot*2
    call rdtold(lunrd     ,lundia    ,error     ,filbct    ,nbcttm    , &
              & nrskip    ,dt        ,itstrt    ,itfinish  ,itold     ,gdp       )
    !
    if (error) goto 9999
    !
    ! Start reading the data
    !
    rewind (lunrd)
    !
    do ib = 1, nbcttm
       do n = 1, ntot
          !
          ! reading for all NTOT end A and end B
          !
          read (lunrd, *) timrd, rwbval(1)
          read (lunrd, *) timrd, rwbval(2)
          ! 
          ! If a NaN is read -> error
          !
          do i = 1, 2
             if ( isnan(rwbval(i)) ) then 
                 write(errmsg,'(a,a)') 'NaN found in file ', trim(filbct)
                 call prterr(lundia, 'P004', errmsg)
                 error = .true.
                 goto 9999
             endif
          enddo
          !
          ! Write first 11 records with block information for time IB=1
          ! Define NAMTYP index dependent on value of TYPBND(NTOF+N)
          !
          irec = (n - 1)*(11 + nbcttm) + 1
          if (ib == 1) then
             ntyp           = index(typtst, typbnd(ntof + n))
             parnam(2)(:20) = namtyp(ntyp)
             parunt(2)      = unttyp(ntyp)
             !
             ! Define table name
             !
             tablnm = 'T-serie BCT ' // nambnd(ntof + n) // ' for run: '
             !
             ! Write first 7 description records to file
             !
             write (lunout, fmtbct(2), rec = irec + 1) &
                 & keywrd(1), quote, tablnm, runid, quote, eol
             write (lunout, fmtbct(3), rec = irec + 2) &
                 & keywrd(2), quote, tprofu(ntof + n), quote, cntain, eol
             write (lunout, fmtbct(4), rec = irec + 3) &
                 & keywrd(3), quote, nambnd(ntof + n), quote, eol
             write (lunout, fmtbct(7), rec = irec + 4) &
                 & keywrd(8), quote, 'non-equidistant', quote, eol
             write (lunout, fmtbct(8), rec = irec + 5) keywrd(9), itdate, eol
             write (lunout, fmtbct(9), rec = irec + 6) &
                 & keywrd(10), quote, 'minutes', quote, eol
             write (lunout, fmtbct(10), rec = irec + 7) &
                 & keywrd(12), quote, 'linear', quote, eol
             !
             ! Write parameter name for time to file
             !
             write (lunout, fmtbct(11), rec = irec + 8) &
                 & keywrd(14), quote, parnam(1), quote, keywrd(15)(:10), quote,  &
                 & parunt(1), quote, eol
             !
             ! Write parameter name for TYPBND for end A and end B
             !
             parnam(2)(21:) = ' end A          '
             write (lunout, fmtbct(11), rec = irec + 9) &
                 & keywrd(14), quote, parnam(2), quote, keywrd(15)(:10), quote,  &
                 & parunt(2), quote, eol
             parnam(2)(21:) = ' end B          '
             write (lunout, fmtbct(11), rec = irec + 10) &
                 & keywrd(14), quote, parnam(2), quote, keywrd(15)(:10), quote,  &
                 & parunt(2), quote, eol
             !
             ! Write number of time dependent data to file
             !
             write (lunout, fmtbct(12), rec = irec + 11) keywrd(16), nbcttm, eol
          endif
          !
          ! Write time dependent data to block for open boundary N
          ! skipping first 11 records with block info
          !
          irec = irec + 11 + ib
          write (lunout, fmtbct(13), rec = irec) timrd, rwbval(1), rwbval(2), eol
       !
       enddo
    enddo
    !
    ! close file and define maximum time frame
    !
 9999 continue
    if (itold /= -1) then
       if (itold < itfinish) then
          write(errmsg,'(a,a,a)') 'Last time in file ', trim(filbct), ' <' 
          call prterr(lundia    ,'U042'    ,errmsg)
          error = .true.
       endif
    endif
    close (lunrd)
end subroutine rdtdt
