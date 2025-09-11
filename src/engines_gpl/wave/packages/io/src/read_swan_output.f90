subroutine read_swan_output (sof, sr, offset, deletefile)
!
! Head routine for calling read_bot
!
use swan_flow_grid_maps
use swan_input
implicit none
!
! parameters
type (output_fields)           :: sof
type (swan_type)               :: sr
integer                        :: offset
logical                        :: deletefile
!
! local
real                           :: north
logical                        :: cart
!
! body
   north    = sr%northdir
   cart     = .not.sr%nautconv
   call hisout( &
        & sof%hs     ,sof%dir    ,sof%dirc      ,sof%dirs  ,sof%period , &
        & sof%depth  ,sof%fx     ,sof%fy        ,sof%mx    ,sof%my     , &
        & sof%dissip(:,:,1)      ,sof%ubot      ,sof%steep ,sof%wlen   ,sof%u      , &
        & sof%v      ,sof%dspr   ,sof%rleak     ,sof%qb    ,sof%nmax   , &
        & sof%mmax   ,north      ,cart          , &
        & sof%dissip(:,:,2)      ,sof%dissip(:,:,3)        ,sof%dissip(:,:,4)      , &
        & sof%rtp    ,sof%pdir   ,sof%windu     ,offset    ,deletefile , &
        & sof%windv  ,sof%tps    ,sof%tm02      ,sof%tmm10 ,sof%dhsign , &
        & sof%drtm01 ,sof%setup  ,sof%n_outpars ,sof%add_out_vals  )
end subroutine read_swan_output


subroutine hisout(hs        ,dir       ,dirc      ,dirs      ,period    , &
                & depth     ,fx        ,fy        ,mx        ,my        , &
                & dissip    ,ubot      ,steep     ,wlen      ,u         , &
                & v         ,dspr      ,rleak     ,qb        ,n         , &
                & m         ,north     ,cart      ,dissurf   ,diswcap   , disbot , &
                & rtp       ,pdir      ,windu     ,offset    ,deletefile, &
                & windv     ,tps       ,tm02      ,tmm10     ,dhsign    , &
                & drtm01    ,setup     ,n_outpars ,add_out_vals)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision_basics
    implicit none
!
! Global variables
!
    integer               , intent(in)  :: m
    integer               , intent(in)  :: n
    integer                             :: n_outpars
    integer               , intent(in)  :: offset     ! # of blocks to skip in the SWAN output files. Default: 0
    logical               , intent(in)  :: cart
    logical               , intent(in)  :: deletefile ! true: delete SWAN file after reading. This used to be default
                                                      ! But now it may contain two fields to be read.
    real                  , intent(in)  :: north
    real    , dimension(*), intent(out) :: depth
    real    , dimension(*), intent(out) :: dhsign
    real    , dimension(*)              :: dir
    real    , dimension(*), intent(out) :: dirc
    real    , dimension(*), intent(out) :: dirs
    real    , dimension(*)              :: dissip
    real    , dimension(*)              :: dissurf
    real    , dimension(*)              :: diswcap
    real    , dimension(*)              :: disbot
    real    , dimension(*), intent(out) :: drtm01
    real    , dimension(*)              :: dspr
    real    , dimension(*), intent(out) :: fx
    real    , dimension(*), intent(out) :: fy
    real    , dimension(*)              :: hs
    real    , dimension(*), intent(out) :: mx
    real    , dimension(*), intent(out) :: my
    real    , dimension(*)              :: period
    real    , dimension(*)              :: qb
    real    , dimension(*)              :: rtp
    real    , dimension(*)              :: rleak
    real    , dimension(*), intent(out) :: setup
    real    , dimension(*)              :: steep
    real    , dimension(*), intent(out) :: tmm10
    real    , dimension(*), intent(out) :: tm02
    real    , dimension(*), intent(out) :: tps
    real    , dimension(*), intent(out) :: u
    real    , dimension(*), intent(out) :: ubot
    real    , dimension(*), intent(out) :: v
    real    , dimension(*)              :: wlen
    real    , dimension(*)              :: pdir
    real    , dimension(*)              :: windu
    real    , dimension(*)              :: windv
    real    , dimension(*)              :: add_out_vals
!
! Local variables
!
    integer           :: fstat
    integer           :: i
    integer           :: inf
    integer           :: iocond
    integer           :: j
    integer           :: k
    integer           :: lunhis
    integer           :: npnt
    integer           :: outfile          ! The SWAN output is written to n_outfiles files
    integer           :: n_outfiles       ! Can be 2 or 3, depending on whether additional output is requested
    integer           :: neg_dissip_found ! = 1,2,3,4 when a negative dissip, dissurf, diswcap or disbot is found in the swanout file, repectively
    logical           :: neg_hs_found     ! = true when a negative hs     is found in the swanout file
    logical           :: neg_period_found ! = true when a negative period is found in the swanout file
    logical           :: neg_dspr_found   ! = true when a negative dspr   is found in the swanout file
    logical           :: neg_rleak_found  ! = true when a negative rleak  is found in the swanout file
    logical           :: neg_qb_found     ! = true when a negative qb     is found in the swanout file
    logical           :: neg_steep_found  ! = true when a negative steep  is found in the swanout file
    logical           :: neg_wlen_found   ! = true when a negative wlen   is found in the swanout file
    logical           :: neg_rtp_found    ! = true when a negative rtp    is found in the swanout file
    logical           :: open
    logical           :: replaced
    real              :: pi
    real              :: d1
    real              :: d2
    real              :: d3
    real              :: missingValue
    character(500)    :: record
    character(8)      :: filnam
!
!! executable statements -------------------------------------------------------
!
    !     TP values were unrealistic high in areas "behind" islands
    !     MAX value introduced of 100. for TP
    !     where TP = PERIOD * PERFAC and 1.07 < PERFAC < 1.30
    !     So if we limit PERIOD to 100. this will be ok as well.
    !
    !     GLOBAL DATA
    !
    !     global data structure definition and access functions
    !
    ! The usage of missingValue=-999.0 is introduced.
    ! For now, all missingValues are replaced by zero
    !
    pi           = 4.*atan(1.)
    npnt         = n*m
    missingValue = -999.0
    !
    ! Set flags for negative values found to false.
    !
    neg_dissip_found = 0
    neg_hs_found     = .false.
    neg_period_found = .false.
    neg_dspr_found   = .false.
    neg_rleak_found  = .false.
    neg_qb_found     = .false.
    neg_steep_found  = .false.
    neg_wlen_found   = .false.
    neg_rtp_found    = .false.
    !
    ! Additional output parameters requested?
    ! Then three swanout files must be read
    !
    if (n_outpars > 0) then
       n_outfiles = 3
    else
       n_outfiles = 2
    endif
    do outfile = 1, n_outfiles
       write(filnam,'(a,i0)') 'SWANOUT', outfile
       inquire (file = filnam, opened = open)
       if (open) then
          inquire (file = filnam, number = lunhis)
       else
          open (newunit=lunhis, file=filnam, iostat=fstat, status='old')
       endif
       if (fstat /= 0) then
          write (*, '(2a)') '*** ERROR: Unable to open file ',trim(filnam)
          call wavestop(1, '*** ERROR: Unable to open file '//trim(filnam))
       endif
       rewind lunhis
       do j = 1, offset
          do i = 1, npnt
             read (lunhis, '(A)', end = 150) record
          enddo
       enddo
       do i = 1, npnt
          read (lunhis, '(A)', end = 150) record
          do
             if (outfile == 1) then
                read (record, *, iostat = iocond) &
                   & hs(i)    , dir(i)   , period(i), depth(i) , u(i)     , v(i) , &
                   & mx(i)    , my(i)    , dspr(i)  , dissip(i), rleak(i) , qb(i), &
                   & d1       , d2       , d3       , ubot(i)  , steep(i) , &
                   & wlen(i)  , fx(i)    , fy(i)    , rtp(i)   , pdir(i)  , &
                   & windu(i) , windv(i)
                !
                ! Replace all missingValues by zero
                !
                if (comparereal(missingValue,hs(i))     == 0) hs(i)     = 0.0
                if (comparereal(missingValue,dir(i))    == 0) dir(i)    = 0.0
                if (comparereal(missingValue,period(i)) == 0) period(i) = 0.0
                if (comparereal(missingValue,depth(i))  == 0) depth(i)  = 0.0
                if (comparereal(missingValue,u(i))      == 0) u(i)      = 0.0
                if (comparereal(missingValue,v(i))      == 0) v(i)      = 0.0
                if (comparereal(missingValue,mx(i))     == 0) mx(i)     = 0.0
                if (comparereal(missingValue,my(i))     == 0) my(i)     = 0.0
                if (comparereal(missingValue,dspr(i))   == 0) dspr(i)   = 0.0
                if (comparereal(missingValue,dissip(i)) == 0) dissip(i) = 0.0
                if (comparereal(missingValue,rleak(i))  == 0) rleak(i)  = 0.0
                if (comparereal(missingValue,qb(i))     == 0) qb(i)     = 0.0
                if (comparereal(missingValue,ubot(i))   == 0) ubot(i)   = 0.0
                if (comparereal(missingValue,steep(i))  == 0) steep(i)  = 0.0
                if (comparereal(missingValue,wlen(i))   == 0) wlen(i)   = 0.0
                if (comparereal(missingValue,fx(i))     == 0) fx(i)     = 0.0
                if (comparereal(missingValue,fy(i))     == 0) fy(i)     = 0.0
                if (comparereal(missingValue,rtp(i))    == 0) rtp(i)    = 0.0
                if (comparereal(missingValue,pdir(i))   == 0) pdir(i)   = 0.0
                if (comparereal(missingValue,windu(i))  == 0) windu(i)  = 0.0
                if (comparereal(missingValue,windv(i))  == 0) windv(i)  = 0.0
                !
                ! Check for negative values; set to zero when negative
                !
                if (hs(i)     < 0.0) then
                   hs(i)            = 0.0
                   neg_hs_found     = .true.
                endif
                if (period(i) < 0.0) then 
                   period(i)        = 0.0
                   neg_period_found = .true.
                endif
                if (dspr(i)   < 0.0) then
                   dspr(i)          = 0.0
                   neg_dspr_found   = .true.
                endif
                if (dissip(i) < 0.0) then
                   dissip(i)        = 0.0
                   neg_dissip_found = 1
                endif
                if (rleak(i)  < 0.0) then
                   rleak(i)         = 0.0
                   neg_rleak_found  = .true.
                endif
                if (qb(i)     < 0.0) then
                   qb(i)            = 0.0
                   neg_qb_found     = .true.
                endif
                if (steep(i)  < 0.0) then
                   steep(i)         = 0.0
                   neg_steep_found  = .true.
                endif
                if (wlen(i)   < 0.0) then
                   wlen(i)          = 0.0
                   neg_wlen_found   = .true.
                endif
                if (rtp(i)    < 0.0) then
                   rtp(i)           = 0.0
                   neg_rtp_found    = .true.
                endif
             elseif (outfile == 2) then
                read (record, *, iostat = iocond) &
                   & tps(i)   , tm02(i)  , tmm10(i) , dhsign(i), drtm01(i), &
                   & setup(i) , dissurf(i) , diswcap(i) , disbot(i)
                !
                ! Replace all missingValues by zero
                !
                if (comparereal(missingValue,tps(i))    == 0) tps(i)     = 0.0
                if (comparereal(missingValue,tm02(i))   == 0) tm02(i)    = 0.0
                if (comparereal(missingValue,tmm10(i))  == 0) tmm10(i)   = 0.0
                if (comparereal(missingValue,dhsign(i)) == 0) dhsign(i)  = 0.0
                if (comparereal(missingValue,drtm01(i)) == 0) drtm01(i)  = 0.0
                if (comparereal(missingValue,setup(i))  == 0) setup(i)   = 0.0
                if (comparereal(missingValue,dissurf(i))== 0) dissurf(i) = 0.0
                if (comparereal(missingValue,diswcap(i))== 0) diswcap(i) = 0.0
                if (comparereal(missingValue,disbot(i)) == 0) disbot(i)  = 0.0
                if (dissurf(i) < 0.0) then
                   dissurf(i)       = 0.0
                   neg_dissip_found = 2
                endif
                if (diswcap(i) < 0.0) then
                   diswcap(i)       = 0.0
                   neg_dissip_found = 3
                endif
                if (disbot(i) < 0.0) then
                   disbot(i)        = 0.0
                   neg_dissip_found = 4
                endif
             elseif (outfile == 3) then
                read (record, *, iostat = iocond) &
                   & (add_out_vals((k-1)*npnt+i), k=1,n_outpars)
                !
                ! Replace all missingValues by zero
                !
                do k=1,n_outpars
                   if (comparereal(missingValue,add_out_vals((k-1)*npnt+i)) == 0) add_out_vals((k-1)*npnt+i) = 0.0
                enddo
             else
             endif
             if (iocond == 0) exit
             !
             ! Read error? Replace 'Infinity' and (E11.4) and try again
             !
             replaced = .false.
             inf      = index(record, 'Infinity')
             if (inf > 0) then
                write (*, *) 'WARNING: In SWAN output <Infinity> found'
                write (*, *) record
                write (record(inf:inf + 7), '(F8.1)') 1.0
                replaced = .true.
             endif
             inf = index(record, '(E11.4)')
             if (inf > 0) then
                write (*, *) 'WARNING: In SWAN output (E11.4) found'
                write (*, *) record
                write (record(inf:inf + 6), '(F6.1)') 1.0
                replaced = .true.
             endif
             if (.not.replaced) then
                !
                ! Something else is wrong
                !
                close (lunhis)
                write (*, '(2a)') '*** ERROR: Unable to read values from file ', trim(filnam)
                call wavestop(1, '*** ERROR: Unable to read values from file '//trim(filnam))
             endif
          enddo
          if (outfile == 1) then
             if (.not.cart) then
                dir(i) = 180. + north - dir(i)
                if (dir(i)>360.) dir(i) = dir(i) - 360.
                if (dir(i)<0.)   dir(i) = dir(i) + 360.
                pdir(i) = 180. + north - pdir(i)
                if (pdir(i)>360.) pdir(i) = pdir(i) - 360.
                if (pdir(i)<0.)   pdir(i) = pdir(i) + 360.
             endif
             dirc(i) = cos(dir(i)/180.*pi)*hs(i)
             dirs(i) = sin(dir(i)/180.*pi)*hs(i)
          endif
       enddo
       !
150    continue
       if (deletefile) then
          close (lunhis, status = 'delete')
       else
          close (lunhis)
       endif
    enddo
    !
    ! Negative values found in swanout1 file? write to screen
    !
    if (neg_hs_found) then
       write (*, '(a)') '  WARNING: Found negative value(s) for HS in swanout1 file;'
       write (*, '(a)') '           value(s) set to zero'
    endif
    if (neg_period_found) then
       write (*, '(a)') '  WARNING: Found negative value(s) for PERIOD in swanout1 file;'
       write (*, '(a)') '           value(s) set to zero'
    endif
    if (neg_dspr_found) then
       write (*, '(a)') '  WARNING: Found negative value(s) for DSPR in swanout1 file;'
       write (*, '(a)') '           value(s) set to zero'
    endif
    if (neg_dissip_found > 0) then
       write (*, '(a,i1,a)') '  WARNING: Found negative value(s) for dissip(', neg_dissip_found,') in swanout file;'
       write (*, '(a)') '           value(s) set to zero'
    endif
    if (neg_rleak_found) then
       write (*, '(a)') '  WARNING: Found negative value(s) for RLEAK in swanout1 file;'
       write (*, '(a)') '           value(s) set to zero'
    endif
    if (neg_qb_found) then
       write (*, '(a)') '  WARNING: Found negative value(s) for QB in swanout1 file;'
       write (*, '(a)') '           value(s) set to zero'
    endif
    if (neg_steep_found) then
       write (*, '(a)') '  WARNING: Found negative value(s) for STEEP in swanout1 file;'
       write (*, '(a)') '           value(s) set to zero'
    endif
    if (neg_wlen_found) then
       write (*, '(a)') '  WARNING: Found negative value(s) for WLEN in swanout1 file;'
       write (*, '(a)') '           value(s) set to zero'
    endif
    if (neg_rtp_found) then
       write (*, '(a)') '  WARNING: Found negative value(s) for RTP in swanout1 file;'
       write (*, '(a)') '           value(s) set to zero'
    endif
end subroutine hisout
