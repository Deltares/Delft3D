!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
      module m_rdodef
      use m_waq_precision
      use m_error_status

      implicit none

      contains

      subroutine rdodef ( noutp  , nrvar  , nrvarm , isrtou , ounam  , &
                         infile , nx     , ny     , nodump , ibflag , &
                         lmoutp , ldoutp , lhoutp , lncout , status , &
                         igrdou , ndmpar )

!       Deltares Software Centre

!>\file
!>                 Reads the ascii output definition file. Checks input

!     CREATED : December 1992 by Jan van Beek

!     LOGICAL UNITNUMBERS : LUNIN  - output definition file
!                         : LUNUT  - report file

      use rd_token     !   for the reading of tokens
      use timers       !   performance timers
      use results, only: enable_netcdf_output => lncout
      use results, only : imo3, imo2, imon, idm2, ihis, ihnf, ihi2, ihn3, ihi4, &
                  ihnc, ihnc2, ihnc3, ihnc4, imnf, imn2, imo4, iba2, idmp, &
                 ibal, ncopt, imnc2, imnc, ima2, imap, ihn2, ihi3, ihn4

      implicit none

!     Parameters         :

!     kind           function         name                   Descriptipon

      integer(kind=int_wp), intent(in   ) ::  noutp                  !< Number of output files
      integer(kind=int_wp), intent(  out) ::  nrvar (noutp )         !< Nr. of extra output variables
      integer(kind=int_wp), intent(in   ) ::  nrvarm                 !< Max. nr. of extra output var.
      integer(kind=int_wp), intent(inout) ::  isrtou(noutp )         !< Sort of output
      character(20), intent(  out) :: ounam (nrvarm,noutp)  !< Name extra output variables
      logical      , intent(inout) :: infile                !< Flag if default(f) or in file(t)
      integer(kind=int_wp), intent(in   ) ::  nx                     !< Width of grid
      integer(kind=int_wp), intent(in   ) ::  ny                     !< Depth of grid
      integer(kind=int_wp), intent(in   ) ::  nodump                 !< Number of monitor points
      logical      , intent(in   ) :: ibflag                !< Mass balance option flag
      logical      , intent(in   ) :: lmoutp                !< Monitor output active
      logical      , intent(in   ) :: ldoutp                !< Dump output active
      logical      , intent(in   ) :: lhoutp                !< History output active
      logical      , intent(in   ) :: lncout                !< NetCDF output active
      integer(kind=int_wp), intent(in   ) ::  igrdou(4)              !< Output grid indication
      integer(kind=int_wp), intent(in   ) ::  ndmpar                 !< number of dump areas

      type(error_status), intent(inout) :: status !< current error status

!     Local

      integer(kind=int_wp), parameter ::  igseg= 1 , igmon= 2 , iggrd= 3 , igsub = 4
      integer               hissrt, hisnrv, mapsrt, mapnrv
      integer               io          !  loop variable
      integer(kind=int_wp) :: ioopt       !  output specification option
      integer(kind=int_wp) :: ierr2       !  not used error variable
      integer(kind=int_wp) :: max2        !  maximum read space
      integer(kind=int_wp) :: nrv         !  number of variables
      integer(kind=int_wp) :: ivar        !  loop variable
      integer(kind=int_wp) :: ioptf       !  option for a file
      character(255)        cdummy     !  dummy string
      character(60)         keyword    !  keyword for tokenized reading
      integer(kind=int_wp) :: keyvalue    !  value for tokenized reading
      integer(kind=int_wp) :: itype       !  type of token for tokenized reading

      integer(kind=int_wp) ::  ithndl = 0
      if (timon) call timstrt( "rdodef", ithndl )

!     Read output option and output vars

      if ( infile ) then

         do io = 1 , 4
            select case ( io )
               case ( 1 )
                  write (lunut,2000)         ! monitor file
               case ( 2 )
                  write (lunut,2010)         ! grid file
               case ( 3 )
                  write (lunut,2020)         ! his file
               case ( 4 )
                  write (lunut,2030)         ! map file
            end select

!         Read output specification option

            if ( gettoken( ioopt, ierr2 ) > 0 ) goto 100

            select case ( ioopt )

               case ( 0 )               !   No output
                  write (lunut,2060)
                  isrtou(io) = 0
                  nrvar(io)  = 0

               case ( 1 )               !   Default action
                  write (lunut,2070)

               case ( 2, 3 )            !   Extra output variables
                  if ( ioopt == 2 ) then
                     write (lunut,2080)
                  else
                     write (lunut,2090)
                     isrtou(io) = isrtou(io) + 1
                  endif
                  if ( igrdou(io) == igsub ) then
                     max2 = nrvarm/2
                     if ( gettoken( nrv, ierr2 ) > 0 ) goto 100
                     do ivar = 1, min(nrv,max2)
                           if ( gettoken( ounam(ivar    ,io), ierr2 ) > 0 ) goto 100
                           if ( gettoken( ounam(ivar+nrv,io), ierr2 ) > 0 ) goto 100
                     enddo
                     do ivar = 1, nrv-max2
                           if ( gettoken( cdummy, ierr2 ) > 0 ) goto 100
                           if ( gettoken( cdummy, ierr2 ) > 0 ) goto 100
                     enddo
                     if ( nrv < 0 ) then
                        write (lunut,2100)
                        call status%increase_error_count()
                        nrvar(io) = 0
                     else if ( nrv > max2 ) then
                        write (lunut,2110) nrv,max2  ,(nrv-max2)*noutp*2
                        call status%increase_error_count()
                        nrvar(io) = max2
                     else
                        nrvar(io) = nrv
                     endif
                     write (lunut,2120) nrvar(io)
                     write (lunut,3020)
                     write (lunut,3030) (ivar,ounam(ivar,io),ounam(nrvar(io)+ivar,io),ivar=1,nrvar(io))
                     nrvar(io) = nrvar(io)*2
                  else
                     if ( gettoken( nrv, ierr2 ) > 0 ) goto 100
                     do ivar = 1, min(nrv,nrvarm)
                         if (gettoken( ounam(ivar    ,io), ierr2 ) > 0 ) goto 100
                     enddo
                     do ivar = 1, nrv-nrvarm
                         if ( gettoken( cdummy, ierr2 ) > 0 ) goto 100
                     enddo
                     if ( nrv < 0 ) then
                        write (lunut,2100)
                        call status%increase_error_count()
                        nrvar(io) = 0
                     else if ( nrv > nrvarm+1 ) then
                        write (lunut,2110) nrv,max2  ,(nrv-max2)*noutp*2
                        call status%increase_error_count()
                        nrvar(io) = nrvarm
                     else
                        nrvar(io) = nrv
                     endif
                     write (lunut,2120) nrvar(io)
                     write (lunut,2130)
                     write (lunut,2140) (ivar,ounam(ivar,io),ivar=1,nrvar(io))
                  endif

               case default    !   Option not implemented
                  write (lunut,2150) ioopt
                  call status%increase_error_count()
            end select
          end do
      endif

!     Store the sort output var ( ISRTOU ) for MAP and HIS to a temporary
!     variable to remember the choise for NEFIS output if the binary
!     output is turned of

      hissrt    = isrtou(3)
      hisnrv    = nrvar(3)
      mapsrt    = isrtou(4)
      mapnrv    = nrvar(4)

!     Special options for certain files

      if ( infile ) then

!       Switch for HIS BINARY

         if ( gettoken( ioptf, ierr2 ) > 0 ) goto 100
         select case ( ioptf )
            case ( 0 )
               write (lunut,3000) ' Binary history file switched off'
               isrtou(3) = 0
               nrvar (3) = 0
            case ( 1 )
               write (lunut,3000) ' Binary history file switched on'
            case default
               write (lunut,3010) ' Binary history file option =',ioptf
               write (lunut,3000) ' ERROR option out of range!'
               isrtou(3) = 0
               nrvar (3) = 0
               call status%increase_error_count()
         end select

!       Switch for MAP BINARY

         if ( gettoken( ioptf, ierr2 ) > 0 ) goto 100
         select case ( ioptf )
            case ( 0 )
               write (lunut,3000) ' Binary map file switched off'
               isrtou(4) = 0
               nrvar (4) = 0
            case ( 1 )
               write (lunut,3000) ' Binary map file switched on'
            case default
               write (lunut,3010) ' Binary map file option =',ioptf
               write (lunut,3000) ' ERROR option out of range!'
               isrtou(4) = 0
               nrvar (4) = 0
               call status%increase_error_count()
         end select

!       Switch for HIS NEFIS, copy HIS definition if active

         if ( gettoken( ioptf, ierr2 ) > 0 ) goto 100
         select case ( ioptf )
            case ( 0 )
               if (.not. lncout) then
                   write (lunut,3000) ' NEFIS history file switched off'
               else
                   write (lunut,3000) ' NetCDF history file switched off'
               endif
            case ( 1 )
               if (.not. lncout) then
                   write (lunut,3000) ' NEFIS history file switched on'
                   if ( hissrt == ihis ) isrtou(6) = ihnf
                   if ( hissrt == ihi2 ) isrtou(6) = ihn2
                   if ( hissrt == ihi3 ) isrtou(6) = ihn3
                   if ( hissrt == ihi4 ) isrtou(6) = ihn4
               else
                   write (lunut,3000) ' NEFIS history file switched on'
                   if ( hissrt == ihis ) isrtou(6) = ihnc
                   if ( hissrt == ihi2 ) isrtou(6) = ihnc2
                   if ( hissrt == ihi3 ) isrtou(6) = ihnc3
                   if ( hissrt == ihi4 ) isrtou(6) = ihnc4
               endif
               nrvar(6)  = hisnrv
               do ivar = 1 , nrvar(6)
                  ounam(ivar,6) = ounam(ivar,3)
               enddo
            case default
               write (lunut,3010) ' NEFIS/NetCDF history file option =',ioptf
               write (lunut,3000) ' ERROR option out of range!'
               call status%increase_error_count()
         end select

!       Switch for MAP NEFIS, copy MAP definition if active

         if ( gettoken( ioptf, ierr2 ) > 0 ) goto 100
         select case ( ioptf )
            case ( 0 )
               if (.not. lncout) then
               write (lunut,3000) ' NEFIS map file switched off'
               else
                  write (lunut,3000) ' NetCDF map file switched off'
               end if
            case ( 1 )
               if (.not. lncout) then
                  write (lunut,3000) ' NEFIS map file switched on'
                  if ( mapsrt == imap ) isrtou(7) = imnf
                  if ( mapsrt == ima2 ) isrtou(7) = imn2
               else
                  write (lunut,3000) ' NetCDF map file switched on'
                  if ( mapsrt == imap ) isrtou(7) = imnc
                  if ( mapsrt == ima2 ) isrtou(7) = imnc2
               end if
               nrvar(7)  = mapnrv
               do ivar = 1 , nrvar(7)
                  ounam(ivar,7) = ounam(ivar,4)
               enddo
            case default
               write (lunut,3010) ' NEFIS/NetCDF map file option =',ioptf
               write (lunut,3000) ' ERROR option out of range!'
               call status%increase_error_count()
         end select

         ! Read the options for the NetCDF file:
         ! ncFormat (4), ncDeflate (0), ncChunk (0), ncShuffle (0 = false)

         ncopt = [4, 0, 0, 0]
         do
            if ( gettoken( keyword, ierr2 ) > 0 ) exit
            if ( keyword(1:1) == '#' ) exit

            select case ( keyword )
               case ('NCFORMAT' )
                  if ( gettoken( keyvalue, ierr2 ) > 0 ) exit
                  ncopt(1) = merge( keyvalue, 4, keyvalue == 3 .or. keyvalue == 4 )
               case ('NCDEFLATE' )
                  if ( gettoken( keyvalue, ierr2 ) > 0 ) exit
                  ncopt(2) = merge( keyvalue, 2, keyvalue >= 0 .and. keyvalue <= 9 )
               case ('NCCHUNK' )
                  if ( gettoken( keyvalue, ierr2 ) > 0 ) exit
                  ncopt(3) = merge( keyvalue, 0, keyvalue >= 0 )
               case ('NCSHUFFLE' )
                  if ( gettoken( keyword, ierr2 ) > 0 ) exit
                  ncopt(4) = merge( 1, 0, keyword == 'YES' )
               case default
                  write (lunut,4010) ' ERROR: unknown option - ', trim(keyword), ' - ignored'
            end select
         enddo

         if ( ncopt(1) == 3 ) then
             ncopt(2:) = 0
         endif

         if ( lncout ) then
             write (lunut,4020) ncopt(1:3),merge('ON ', 'OFF', ncopt(4) == 1)
         endif

         infile = .false. ! We have already encountered the end-block marker
      endif

!     Help variables bal file

      if ( isrtou(5) == ibal ) then
         if ( nrvarm >= 4 ) then
            nrvar(5)   = 4
            ounam(1,5) = 'VOLUME'
            ounam(2,5) = 'SURF'
            ounam(3,5) = ' '
            ounam(4,5) = ' '
         else
            write (lunut,2110) 4,nrvarm,(4-nrvarm)*noutp
            call status%increase_error_count()
         endif
      endif

!     Check if output is defined for each file

      do io = 1 , noutp

         if ( isrtou(io) == idmp .or. isrtou(io) == idm2 ) then
            if ( nx*ny  == 0 )  then
               isrtou(io) = 0
               nrvar (io) = 0
               write (lunut,2160)
            endif
            if ( .not. ldoutp ) then
               isrtou(io) = 0
               nrvar (io) = 0
               write (lunut,2170)
            endif
         elseif ( isrtou(io) == ihis .or. isrtou(io) == ihi2 .or.  &
                 isrtou(io) == ihnf .or. isrtou(io) == ihn2      ) then
            if ( nodump == 0 )  then
               isrtou(io) = 0
               nrvar (io) = 0
               write (lunut,2180)
            endif
            if ( .not. lhoutp ) then
               isrtou(io) = 0
               nrvar (io) = 0
               write (lunut,2190)
            endif
         elseif ( isrtou(io) == ihi3 .or. isrtou(io) == ihi4 .or. &
                 isrtou(io) == ihn3 .or. isrtou(io) == ihn4      ) then
            if ( ndmpar == 0 )  then
               isrtou(io) = 0
               nrvar (io) = 0
               write (lunut,2180)
            endif
            if ( .not. lhoutp ) then
               isrtou(io) = 0
               nrvar (io) = 0
               write (lunut,2190)
            endif
         elseif ( isrtou(io) == imap .or. isrtou(io) == ima2 .or. &
                 isrtou(io) == imnf .or. isrtou(io) == imn2      ) then
            if ( .not. ldoutp ) then
               isrtou(io) = 0
               nrvar (io) = 0
               write (lunut,2200)
            endif
         elseif ( isrtou(io) == ibal .or. isrtou(io) == iba2 ) then
            if ( .not. ibflag )  then
               isrtou(io) = 0
               nrvar (io) = 0
               write (lunut,2210)
            endif
            if ( ndmpar == 0 )  then
               isrtou(io) = 0
               nrvar (io) = 0
               write (lunut,2220)
            endif
            if ( .not. lmoutp ) then
               isrtou(io) = 0
               nrvar (io) = 0
               write (lunut,2230)
            endif
            if ( isrtou(io) == ibal ) then
               write(lunut,3040)
            elseif ( isrtou(io) == iba2 ) then
               write(lunut,3050)
            endif
         elseif ( isrtou(io) == imon .or. isrtou(io) == imo2 ) then
            if ( nodump == 0 )  then
               nrvar (io) = 0
               write (lunut,2240)
            endif
            if ( .not. lmoutp ) then
               isrtou(io) = 0
               nrvar (io) = 0
               write (lunut,2250)
            endif
         elseif ( isrtou(io) == imo3 .or. isrtou(io) == imo4 ) then
            if ( ndmpar == 0 )  then
               nrvar (io) = 0
               write (lunut,2240)
            endif
            if ( .not. lmoutp ) then
               isrtou(io) = 0
               nrvar (io) = 0
               write (lunut,2250)
            endif
         endif

      end do
      if (timon) call timstop( ithndl )
      return
  100 call status%increase_error_count()
      if (timon) call timstop( ithndl )
      return

 2000 format ( /,' Specification of monitor output')
 2010 format ( /,' Specification of grid dump output')
 2020 format ( /,' Specification of history output')
 2030 format ( /,' Specification of map output')
 2060 format ( /,' No output for this file')
 2070 format ( /,' Default output for this file')
 2080 format ( /,' All substances plus extra output variables')
 2090 format ( /,' Only the output variables defined here')
 2100 format ( /,' ERROR negative number of output variables (',I7,')')
 2110 format ( /,' ERROR the number of output variables (',I7,') exceeds the maximum (',I7,').', &
              / ' The maximum is limited by CHARACTER array space', &
              / ' Consult your system manager to obtain ',I7,' words of additional storage.' )
 2120 format ( /,' (',I7,') number of output variables specified')
 2130 format ( /,' Number           Identification '  )
 2140 format (    I8,11X,A20 )
 2150 format ( /,' ERROR output specification option not implemented (',I7,')')
 2160 format ( /,' NO GRID output : output grid not defined !' )
 2170 format ( /,' NO GRID output : dump timer not active !' )
 2180 format ( /,' NO HISTORY output : no monitor points defined !' )
 2190 format ( /,' NO HISTORY output : history timer not active !' )
 2200 format ( /,' NO MAP output : dump timer not active !' )
 2210 format ( /,' NO BALANCE output : balance option not active !' )
 2220 format ( /,' NO BALANCE output : no monitor points defined !' )
 2230 format ( /,' NO BALANCE output : monitor timer not active !' )
 2240 format ( /,' MONITOR output only totals : no monitor points defined !' )
 2250 format ( /,' NO MONITOR output : monitor timer not active !' )
 3000 format ( /,A)
 3010 format ( /,A,I4)
 3020 format ( /,' Number           Identification        Weight variable'  )
 3030 format (    I8,11X,A20,2X,A20 )
 3040 format ( /,' Balance file set to old format' )
 3050 format ( /,' Balance file set to new format' )
 4010 format ( /,' ',3A)
 4020 format ( /,' NetCDF output options:',/, &
                '     NetCDF format:   ',I1,/, &
                '     Deflation level: ',I1,/, &
                '     Chunksize:       ',I0, ' - 0 means no chunking',/, &
                '     Shuffling:       ',A)
      end

      end module m_rdodef
