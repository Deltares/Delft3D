subroutine rdtdd(lundia    ,lunout    ,lunrd     ,error     ,fildis    , &
               & runid     ,cntain    ,eol       ,itstrt    ,itfinish  ,&
               & nsrc      ,lstsc     ,nrval     ,rval      ,namsrc    , &
               & disint    ,parnam    ,parunt    ,gdp       )
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
!    Function: This general purpose routine reads the time depen-
!              dent data from file
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
    integer                    , pointer :: itdate
    real(fp)                   , pointer :: dt
    character*20, dimension(:) , pointer :: keywrd
    character*39, dimension(:) , pointer :: fmtdis
!
! Global variables
!
    integer                                               :: itfinish   !  Description and declaration in inttim.igs
    integer                                               :: itstrt     !  Description and declaration in inttim.igs
    integer                                 , intent(in)  :: lstsc      !  Description and declaration in dimens.igs
    integer                                 , intent(in)  :: lundia     !  Description and declaration in inout.igs
    integer                                 , intent(in)  :: lunout     !  Unit number for the transformed output file
    integer                                               :: lunrd      !  Unit number of the attribute file containing the time series
    integer                                 , intent(in)  :: nrval      !  NR. of data in a record to be read
    integer                                 , intent(in)  :: nsrc       !  Description and declaration in esm_alloc_int.f90
    logical                                 , intent(out) :: error      !  Flag=TRUE if an error is encountered
    real(fp)        , dimension(nrval)                    :: rval       !  Array for the time dependent data
    character(*)                                          :: fildis     !  Name of the relevant file
    character(*)                            , intent(in)  :: runid      !  Run identification code for the current simulation (used to determine
                                                                        !  the names of the in- /output files used by the system)
    character(1)                            , intent(in)  :: eol        !  ASCII code for End-Of-Line (^J)
    character(1)    , dimension(nsrc)       , intent(in)  :: disint     !  Description and declaration in esm_alloc_char.f90
    character(10)   , dimension(4 + lstsc)  , intent(in)  :: parunt     !  Unit name fitting the parameter
    character(20)   , dimension(nsrc)       , intent(in)  :: namsrc     !  Description and declaration in esm_alloc_char.f90
    character(36)   , dimension(4 + lstsc)  , intent(in)  :: parnam     !  Names of the paramaters to write to time dependent file DIS
    character(40)                           , intent(in)  :: cntain
!
!
! Local variables
!
    integer                        :: id
    integer                        :: irec
    integer                        :: itold         ! Help var. to store last read time to test accending order 
    integer                        :: j
    integer                        :: l
    integer                        :: n             ! Help var. for the nr. of data to be read (see LENDAT) 
    integer                        :: ndistm
    integer                        :: nrskip
    real(fp)                       :: timrd         ! Time in minutes read 
    character(1)                   :: quote         ! Apostrophe ASCII-character 39 
    character(10)                  :: interp        ! Character string containing inter- polation option 
    character(42)                  :: tablnm        ! Table name specification 
    character(300)                 :: errmsg        ! Character var. containing the error message to be written to file. The message depends on the error. 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    fmtdis  => gdp%gdfmtdis%fmtdis
    keywrd  => gdp%gdkeywtd%keywrd
    itdate  => gdp%gdexttim%itdate
    dt      => gdp%gdexttim%dt
    !
    quote = char(39)
    irec = 1
    !
    !-----Define writing format for FMTDIS(13)
    !
    write (fmtdis(13)(8:10), '(i3.3)') nrval
    !
    !-----Count number of records in the file
    !     File is build with NSRC time records including LSTSC+2 data fields
    !     Number of records to skip is NSRC-1. In RDTOLD the value of
    !     NRSKIP will be substracted (-1) so here call with NSRC
    !
    itold = -1
    ndistm = 0
    nrskip = nsrc
    call rdtold(lunrd     ,lundia    ,error     ,fildis    ,ndistm    , &
              & nrskip    ,dt        ,itstrt    ,itfinish  ,itold     ,gdp       )
    !
    if (error) goto 9999
    !
    !-----Start reading the data
    !
    rewind (lunrd)
    do id = 1, ndistm
       do n = 1, nsrc
          read (lunrd, *) timrd, (rval(j), j = 1, nrval)
          !
          ! If an NaN is read -> error
          !
          do j = 2, nrval
             if (isnan(rval(j))) then
                write(errmsg,'(a,a)') 'NaN in ', trim(fildis)
                call prterr(lundia    ,'P004'    ,errmsg      )
                !
                error = .true.
                goto 9999
             endif
          enddo
          !
          !-----------check for negative values,
          !
          do j = 2, nrval
             if (rval(j)<0.0) then
                write(errmsg,'(a,a)') 'Concentration at discharge points in ', trim(fildis)
                call prterr(lundia    ,'V061'    ,errmsg        )
                !
                error = .true.
                goto 9999
             endif
          enddo
          !
          !-----------Write first 9+NRVAL records with block information for
          !           time ID=1
          !
          irec = (n - 1)*(ndistm + 9 + nrval) + 1
          if (id==1) then
             !
             !--------------Define interpolation option
             !
             interp = 'linear    '
             if (disint(n)=='N') interp = 'block    '
             !
             !--------------Define table name
             !
             tablnm = 'T-serie DIS ' // namsrc(n) // ' for run: '
             !
             !--------------Write first 7 description records to file
             !
             write (lunout, fmtdis(2), rec = irec + 1) &
                 & keywrd(1), quote, tablnm, runid, quote, eol
             write (lunout, fmtdis(3), rec = irec + 2) &
                 & keywrd(2), quote, cntain(:10), quote, cntain(11:40), eol
             write (lunout, fmtdis(4), rec = irec + 3) &
                 & keywrd(3), quote, namsrc(n), quote, eol
             write (lunout, fmtdis(7), rec = irec + 4) &
                 & keywrd(8), quote, 'non-equidistant', quote, eol
             write (lunout, fmtdis(8), rec = irec + 5) keywrd(9), itdate, eol
             write (lunout, fmtdis(9), rec = irec + 6) &
                 & keywrd(10), quote, 'minutes', quote, eol
             write (lunout, fmtdis(10), rec = irec + 7) &
                 & keywrd(12), quote, interp, quote, eol
             !
             !--------------Write parameter name for time to file
             !
             write (lunout, fmtdis(11), rec = irec + 8) &
                 & keywrd(14), quote, parnam(1), quote, keywrd(15)(:10), quote,  &
                 & parunt(1), quote, eol
             !
             !--------------Write parameter names for discharges and concentrations
             !
             do l = 1, nrval
                write (lunout, fmtdis(11), rec = irec + 8 + l) &
                    & keywrd(14), quote, parnam(1 + l), quote, keywrd(15) (:10), quote,  &
                    & parunt(1 + l), quote, eol
             enddo
             !
             !--------------Write number of time dependent data to file
             !
             write (lunout, fmtdis(12), rec = irec + 9 + nrval) &
                 & keywrd(16), ndistm, eol
          endif
          !
          !-----------Write time dependent data to block for discharge N
          !           skipping first 9+NRVAL records with block info
          !
          write (lunout, fmtdis(13), rec = irec + 9 + nrval + id) &
              & timrd, (rval(l), l = 1, nrval), eol
       enddo
    enddo
    !
    !-----close file and define maximum time frame
    !
 9999 continue
    if (itold/= - 1) then
       if (itold < itfinish) then
          write(errmsg,'(a,a,a)') 'Last time in file ', trim(fildis), ' <' 
          call prterr(lundia    ,'U042'    ,errmsg)
          error = .true.
       endif
    endif
    close (lunrd)
end subroutine rdtdd
