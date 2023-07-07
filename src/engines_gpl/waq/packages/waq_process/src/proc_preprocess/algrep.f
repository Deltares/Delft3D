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

      subroutine algrep ( procesdef, notyp , nocof , algtyp , algact,
     +                    abrtyp   , cofnam, algcof, maxcof , alggrp,
     +                    nogrp    , grpnam, grpabr, nouttyp, outtyp,
     +                    noutgrp  , outgrp)

      ! replace the proto names from proces.def with the actual BLOOM names
      ! for the moment not the names and defaults in itemprop, consequence?

      use m_zoek
      use m_dhslen
      use processet
      use timers       !   performance timers

      implicit none

      ! declaration of arguments

      type(procespropcoll)      :: procesdef       ! all processes
      integer                   :: notyp           ! number of algae types
      integer                   :: nocof           ! number of coefficients
      character(len=*)          :: algtyp(notyp)   ! type names
      integer                   :: algact(notyp)   ! active indication
      character(len=*)          :: abrtyp(notyp)   ! type abbrevation
      character(len=*)          :: cofnam(nocof)   ! coefficient name
      real                      :: algcof(maxcof,notyp) ! coefficient values
      integer                   :: maxcof          ! max number of coefficiets
      character(len=*)          :: alggrp(notyp)   ! group of type
      integer                   :: nogrp           ! number of groups
      character(len=*)          :: grpnam(nogrp)   ! group names
      character(len=*)          :: grpabr(nogrp)   ! group abbrevation
      integer                   :: nouttyp         ! number of outputs per type
      character(len=*)          :: outtyp(nouttyp) ! name of output per type
      integer                   :: noutgrp         ! number of outputs per group
      character(len=*)          :: outgrp(noutgrp) ! name of output per group

      ! local decalarations

      integer                   :: nproc           ! number of processes
      integer                   :: iproc           ! loop counter processes
      type(procesprop), pointer :: proc            ! process description
      character(len=10)         :: name1           ! name
      character(len=10)         :: name2           ! name
      integer                   :: i_input         ! index input item
      integer                   :: ioutput         ! index output item
      integer                   :: istochi         ! index stochi item
      integer                   :: ifound          ! indicates match
      integer                   :: nalg            ! number of algae types
      integer                   :: ialg            ! index algae types
      integer                   :: igrp            ! index algae group
      real                      :: rgrp            ! index algae group
      integer                   :: icof            ! index coefficient
      integer                   :: ilen            ! nonblank length of string
      integer                   :: iout            ! index output
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "algrep", ithndl )

      ! some init

      nproc = procesdef%cursize

      ! input types

      nalg = 0
      do ialg = 1 , notyp
         if ( algact(ialg) .eq. 1 ) then
            nalg   = nalg + 1

            ! types themselves

            write(name1,'(''bloomalg'',i2.2)') nalg
            do iproc = 1 , nproc
               proc => procesdef%procesprops(iproc)
               do i_input = 1 , proc%no_input
                  call zoek(proc%input_item(i_input)%name,1,name1,10,ifound)
                  if ( ifound .gt. 0 ) then
                     proc%input_item(i_input)%name = algtyp(ialg)
                  endif
               enddo
               do ioutput = 1 , proc%no_output
                  call zoek(proc%output_item(ioutput)%name,1,name1,10,ifound)
                  if ( ifound .gt. 0 ) then
                     proc%output_item(ioutput)%name = algtyp(ialg)
                  endif
               enddo
               do istochi = 1 , proc%no_fluxstochi
                  call zoek(proc%fluxstochi(istochi)%substance,1,name1,10,ifound)
                  if ( ifound .gt. 0 ) then
                     proc%fluxstochi(istochi)%substance = algtyp(ialg)
                  endif
                  call zoek(proc%fluxstochi(istochi)%ioitem,1,name1,10,ifound)
                  if ( ifound .gt. 0 ) then
                     proc%fluxstochi(istochi)%ioitem = algtyp(ialg)
                  endif
               enddo
               do istochi = 1 , proc%no_dispstochi
                  call zoek(proc%dispstochi(istochi)%substance,1,name1,10,ifound)
                  if ( ifound .gt. 0 ) then
                     proc%dispstochi(istochi)%substance = algtyp(ialg)
                  endif
                  call zoek(proc%dispstochi(istochi)%ioitem,1,name1,10,ifound)
                  if ( ifound .gt. 0 ) then
                     proc%dispstochi(istochi)%ioitem = algtyp(ialg)
                  endif
               enddo
               do istochi = 1 , proc%no_velostochi
                  call zoek(proc%velostochi(istochi)%substance,1,name1,10,ifound)
                  if ( ifound .gt. 0 ) then
                     proc%velostochi(istochi)%substance = algtyp(ialg)
                  endif
                  call zoek(proc%velostochi(istochi)%ioitem,1,name1,10,ifound)
                  if ( ifound .gt. 0 ) then
                     proc%velostochi(istochi)%ioitem = algtyp(ialg)
                  endif
               enddo
            enddo

            ! groep number type

            write(name1,'(''specalg'',i2.2,'' '')') nalg
            call zoek( alggrp(ialg), nogrp , grpnam, 10 , igrp )
            rgrp  = igrp
            do iproc = 1 , nproc
               proc => procesdef%procesprops(iproc)
               do i_input = 1 , proc%no_input
                  call zoek(proc%input_item(i_input)%name,1,name1,10,ifound)
                  if ( ifound .gt. 0 ) then
                     proc%input_item(i_input)%actdef = rgrp
                  endif
               enddo
            enddo

            ! coefficients, including defaults

            do icof = 1 , nocof
               name1 = cofnam(icof)
               name2 = cofnam(icof)
               call dhslen(name1,ilen)
               write(name1(ilen+1:),'(i2.2)') nalg
               name2(ilen-2:) = abrtyp(ialg)
               do iproc = 1 , nproc
                  proc => procesdef%procesprops(iproc)
                  do i_input = 1 , proc%no_input
                     call zoek(proc%input_item(i_input)%name,1,name1,10,ifound)
                     if ( ifound .gt. 0 ) then
                        proc%input_item(i_input)%name = name2
                        proc%input_item(i_input)%actdef = algcof(icof,ialg)
                     endif
                  enddo
                  do ioutput = 1 , proc%no_output
                     call zoek(proc%output_item(ioutput)%name,1,name1,10,ifound)
                     if ( ifound .gt. 0 ) then
                        proc%output_item(ioutput)%name = name2
                     endif
                  enddo
                  do istochi = 1 , proc%no_fluxstochi
                     call zoek(proc%fluxstochi(istochi)%substance,1,name1,10,ifound)
                     if ( ifound .gt. 0 ) then
                        proc%fluxstochi(istochi)%substance = name2
                     endif
                     call zoek(proc%fluxstochi(istochi)%ioitem,1,name1,10,ifound)
                     if ( ifound .gt. 0 ) then
                        proc%fluxstochi(istochi)%ioitem = name2
                     endif
                  enddo
                  do istochi = 1 , proc%no_dispstochi
                     call zoek(proc%dispstochi(istochi)%substance,1,name1,10,ifound)
                     if ( ifound .gt. 0 ) then
                        proc%dispstochi(istochi)%substance = name2
                     endif
                     call zoek(proc%dispstochi(istochi)%ioitem,1,name1,10,ifound)
                     if ( ifound .gt. 0 ) then
                        proc%dispstochi(istochi)%ioitem = name2
                     endif
                  enddo
                  do istochi = 1 , proc%no_velostochi
                     call zoek(proc%velostochi(istochi)%substance,1,name1,10,ifound)
                     if ( ifound .gt. 0 ) then
                        proc%velostochi(istochi)%substance = name2
                     endif
                     call zoek(proc%velostochi(istochi)%ioitem,1,name1,10,ifound)
                     if ( ifound .gt. 0 ) then
                        proc%velostochi(istochi)%ioitem = name2
                     endif
                  enddo
               enddo
            enddo

         endif
      enddo

      ! output types, also input because output can be used as input

      nalg = 0
      do ialg = 1 , notyp
         if ( algact(ialg) .eq. 1 ) then
            nalg   = nalg + 1
            do iout = 1 , nouttyp
               name1 = outtyp(iout)
               name2 = outtyp(iout)
               call dhslen(name1,ilen)
               write(name1(ilen+1:),'(i2.2)') nalg
               name2(ilen-2:) = abrtyp(ialg)
               do iproc = 1 , nproc
                  proc => procesdef%procesprops(iproc)
                  do i_input = 1 , proc%no_input
                     call zoek(proc%input_item(i_input)%name,1,name1,10,ifound)
                     if ( ifound .gt. 0 ) then
                        proc%input_item(i_input)%name = name2
                     endif
                  enddo
                  do ioutput = 1 , proc%no_output
                     call zoek(proc%output_item(ioutput)%name,1,name1,10,ifound)
                     if ( ifound .gt. 0 ) then
                        proc%output_item(ioutput)%name = name2
                     endif
                  enddo
                  do istochi = 1 , proc%no_fluxstochi
                     call zoek(proc%fluxstochi(istochi)%substance,1,name1,10,ifound)
                     if ( ifound .gt. 0 ) then
                        proc%fluxstochi(istochi)%substance = name2
                     endif
                     call zoek(proc%fluxstochi(istochi)%ioitem,1,name1,10,ifound)
                     if ( ifound .gt. 0 ) then
                        proc%fluxstochi(istochi)%ioitem = name2
                     endif
                  enddo
                  do istochi = 1 , proc%no_dispstochi
                     call zoek(proc%dispstochi(istochi)%substance,1,name1,10,ifound)
                     if ( ifound .gt. 0 ) then
                        proc%dispstochi(istochi)%substance = name2
                     endif
                     call zoek(proc%dispstochi(istochi)%ioitem,1,name1,10,ifound)
                     if ( ifound .gt. 0 ) then
                        proc%dispstochi(istochi)%ioitem = name2
                     endif
                  enddo
                  do istochi = 1 , proc%no_velostochi
                     call zoek(proc%velostochi(istochi)%substance,1,name1,10,ifound)
                     if ( ifound .gt. 0 ) then
                        proc%velostochi(istochi)%substance = name2
                     endif
                     call zoek(proc%velostochi(istochi)%ioitem,1,name1,10,ifound)
                     if ( ifound .gt. 0 ) then
                        proc%velostochi(istochi)%ioitem = name2
                     endif
                  enddo
               enddo
            enddo
         endif
      enddo

      ! output groups

      do igrp = 1 , nogrp

         ! special, groups themselves

         write(name1,'(''bloomgrp'',i2.2)') igrp
         name2 = grpnam(igrp)
         do iproc = 1 , nproc
            proc => procesdef%procesprops(iproc)
            do i_input = 1 , proc%no_input
               call zoek(proc%input_item(i_input)%name,1,name1,10,ifound)
               if ( ifound .gt. 0 ) then
                  proc%input_item(i_input)%name = name2
               endif
            enddo
            do ioutput = 1 , proc%no_output
               call zoek(proc%output_item(ioutput)%name,1,name1,10,ifound)
               if ( ifound .gt. 0 ) then
                  proc%output_item(ioutput)%name = name2
               endif
            enddo
            do istochi = 1 , proc%no_fluxstochi
               call zoek(proc%fluxstochi(istochi)%substance,1,name1,10,ifound)
               if ( ifound .gt. 0 ) then
                  proc%fluxstochi(istochi)%substance = name2
               endif
               call zoek(proc%fluxstochi(istochi)%ioitem,1,name1,10,ifound)
               if ( ifound .gt. 0 ) then
                  proc%fluxstochi(istochi)%ioitem = name2
               endif
            enddo
            do istochi = 1 , proc%no_dispstochi
               call zoek(proc%dispstochi(istochi)%substance,1,name1,10,ifound)
               if ( ifound .gt. 0 ) then
                  proc%dispstochi(istochi)%substance = name2
               endif
               call zoek(proc%dispstochi(istochi)%ioitem,1,name1,10,ifound)
               if ( ifound .gt. 0 ) then
                  proc%dispstochi(istochi)%ioitem = name2
               endif
            enddo
            do istochi = 1 , proc%no_velostochi
               call zoek(proc%velostochi(istochi)%substance,1,name1,10,ifound)
               if ( ifound .gt. 0 ) then
                  proc%velostochi(istochi)%substance = name2
               endif
               call zoek(proc%velostochi(istochi)%ioitem,1,name1,10,ifound)
               if ( ifound .gt. 0 ) then
                  proc%velostochi(istochi)%ioitem = name2
               endif
            enddo
         enddo

         ! the ones from the bloom database

         do iout = 1 , noutgrp
            name1 = outgrp(iout)
            name2 = outgrp(iout)
            call dhslen(name1,ilen)
            write(name1(ilen+1:),'(i2.2)') igrp
            name2(ilen-2:) = grpabr(igrp)
            do iproc = 1 , nproc
               proc => procesdef%procesprops(iproc)
               do i_input = 1 , proc%no_input
                  call zoek(proc%input_item(i_input)%name,1,name1,10,ifound)
                  if ( ifound .gt. 0 ) then
                     proc%input_item(i_input)%name = name2
                  endif
               enddo
               do ioutput = 1 , proc%no_output
                  call zoek(proc%output_item(ioutput)%name,1,name1,10,ifound)
                  if ( ifound .gt. 0 ) then
                     proc%output_item(ioutput)%name = name2
                  endif
               enddo
               do istochi = 1 , proc%no_fluxstochi
                  call zoek(proc%fluxstochi(istochi)%substance,1,name1,10,ifound)
                  if ( ifound .gt. 0 ) then
                     proc%fluxstochi(istochi)%substance = name2
                  endif
                  call zoek(proc%fluxstochi(istochi)%ioitem,1,name1,10,ifound)
                  if ( ifound .gt. 0 ) then
                     proc%fluxstochi(istochi)%ioitem = name2
                  endif
               enddo
               do istochi = 1 , proc%no_dispstochi
                  call zoek(proc%dispstochi(istochi)%substance,1,name1,10,ifound)
                  if ( ifound .gt. 0 ) then
                     proc%dispstochi(istochi)%substance = name2
                  endif
                  call zoek(proc%dispstochi(istochi)%ioitem,1,name1,10,ifound)
                  if ( ifound .gt. 0 ) then
                     proc%dispstochi(istochi)%ioitem = name2
                  endif
               enddo
               do istochi = 1 , proc%no_velostochi
                  call zoek(proc%velostochi(istochi)%substance,1,name1,10,ifound)
                  if ( ifound .gt. 0 ) then
                     proc%velostochi(istochi)%substance = name2
                  endif
                  call zoek(proc%velostochi(istochi)%ioitem,1,name1,10,ifound)
                  if ( ifound .gt. 0 ) then
                     proc%velostochi(istochi)%ioitem = name2
                  endif
               enddo
            enddo
         enddo
      enddo

      if (timon) call timstop( ithndl )
      return
      end
