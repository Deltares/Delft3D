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

      subroutine intoou ( procesdef, nproc , nflux , nsvar , pronam,
     +                    iflux    , ipmsa , ipssa , nipmsa, ioffx ,
     +                    nocons   , nopa  , nofun , nosfun, notot ,
     +                    nodisp   , novelo, nodef , noloc , ndspx ,
     +                    nvelx    , nlocx , nopred, prvvar, prvtyp,
     +                    novar    , progrd, prondt)

      ! maps input structure to output structure

      use processet
      use timers       !   performance timers

      implicit none

      ! declaration of arguments

      type(procespropcoll)      :: procesdef       ! all processes
      integer                   :: nproc           ! number of processes (active)
      integer                   :: nflux           ! number of fluxes
      integer                   :: nsvar(*)        ! number of variables
      character(len=*)          :: pronam(*)       ! routine name
      integer                   :: iflux(*)        ! index in flux array
      integer                   :: ipmsa(*)        ! index in pmsa array
      integer                   :: ipssa(*)        ! index in ssa  array
      integer                   :: nipmsa          ! length pmsa array
      integer                   :: ioff            ! offset in pointer for segment items
      integer                   :: ioffx           ! offset in pointer for exchange items
      integer                   :: nocons          ! number of constants
      integer                   :: nopa            ! number of parameters
      integer                   :: nofun           ! number of functions
      integer                   :: nosfun          ! number of segment functions
      integer                   :: notot           ! number of substances
      integer                   :: nodisp          ! number of dispersions
      integer                   :: novelo          ! number of velocities
      integer                   :: nodef           ! number of default
      integer                   :: noloc           ! number of local values
      integer                   :: ndspx           ! number of dispersions
      integer                   :: nvelx           ! number of velocities
      integer                   :: nlocx           ! number of local values on exchanges
      integer                   :: nopred          ! number of predfined values
      integer                   :: prvvar(*)       ! variable pointer
      integer                   :: prvtyp(*)       ! type of variable
      integer                   :: novar           ! number of variables
      integer                   :: progrd(*)       ! process grid
      integer                   :: prondt(*)       ! process ndt

      ! local decalarations

      integer                   :: nproctot        ! number of processes
      integer                   :: iproc           ! loop counter processes
      integer                   :: iproc_act       ! index active processes
      type(procesprop), pointer :: proc            ! process description
      integer                   :: i_input         ! index input item
      integer                   :: ioutput         ! index output item
      integer                   :: ibfl            ! index flux
      integer                   :: itel            ! counter
      integer                   :: itel0           ! counter
      integer                   :: itel1           ! counter
      character(len=80)         :: line            ! output buffer
      integer                   :: ivvol           ! pointer index to array
      integer                   :: ivare           ! pointer index to array
      integer                   :: ivflo           ! pointer index to array
      integer                   :: ivlen           ! pointer index to array
      integer                   :: ivcns           ! pointer index to array
      integer                   :: ivpar           ! pointer index to array
      integer                   :: ivfun           ! pointer index to array
      integer                   :: ivsfu           ! pointer index to array
      integer                   :: ivcnc           ! pointer index to array
      integer                   :: ivmas           ! pointer index to array
      integer                   :: ivder           ! pointer index to array
      integer                   :: ivdsp           ! pointer index to array
      integer                   :: ivvel           ! pointer index to array
      integer                   :: ivdef           ! pointer index to array
      integer                   :: ivloc           ! pointer index to array
      integer                   :: ivdsx           ! pointer index to array
      integer                   :: ivvlx           ! pointer index to array
      integer                   :: ivlcx           ! pointer index to array
      integer                   :: ivflx           ! pointer index to array
      integer                   :: ioffx2          ! pointer index to array
      integer                   :: nfluxx          ! second counter nflux
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "intoou", ithndl )

      ! first calculate nproc, nflux

      nproc  = 0
      nflux  = 0
      nproctot = procesdef%cursize
      do iproc = 1 , nproctot
         proc => procesdef%procesprops(iproc)
         if ( proc%active ) then
            nproc = nproc + 1
            nflux = nflux + proc%no_fluxoutput
         endif
      enddo
      ioffx2 = ioffx + nflux

      ! set pointer index to the delwaq arrays

      ivvol = 1
      ivare = ivvol + 1
      ivflo = ivare + 1
      ivlen = ivflo + 1
      ivcns = ivlen + 2
      ivpar = ivcns + nocons
      ivfun = ivpar + nopa
      ivsfu = ivfun + nofun
      ivcnc = ivsfu + nosfun
      ivmas = ivcnc + notot
      ivder = ivmas + notot
      ivdsp = ivder + notot
      ivvel = ivdsp + nodisp
      ivdef = ivvel + novelo
      ivloc = ivdef + nodef
      ivdsx = ivloc + noloc
      ivvlx = ivdsx + ndspx
      ivlcx = ivvlx + nvelx
      ivflx = ivlcx + nlocx
      novar = ivflx + nflux - 1

      ! initialize totals , including nflux again (principally the same)

      itel   = 0
      nfluxx = 0

      ! loop over processes

      nproctot = procesdef%cursize
      iproc_act= 0
      do iproc = 1 , nproctot
         proc => procesdef%procesprops(iproc)
         if ( proc%active ) then
            iproc_act = iproc_act + 1
            nsvar(iproc_act) = proc%no_input + proc%no_output + proc%no_fluxoutput
            pronam(iproc_act)= proc%routine
            progrd(iproc_act)= proc%grid
            prondt(iproc_act)= proc%ndt
            iflux(iproc_act) = nfluxx + 1

            ! map in- and output pointers on ipmsa

            itel0 = itel
            do i_input = 1 , proc%no_input
               itel = itel + 1
               if ( proc%input_item(i_input)%indx .gt. 0 ) then
                  itel1 = itel0 + proc%input_item(i_input)%indx
               else
                  itel1 = itel
                  write(*,*) 'geen volgorde voor proces:',proc%name
               endif
               if ( proc%input_item(i_input)%type .eq. IOTYPE_SEGMENT_INPUT .or.
     +              proc%input_item(i_input)%type .eq. IOTYPE_SEGMENT_WORK  ) then
                  if ( proc%input_item(i_input)%ip_val .eq. 3 ) then
                     ! idt, voor fractional step aparte ingang in default array
                     ioff   = nopred + nocons + nopa + nofun + nosfun + notot + noloc
                     proc%input_item(i_input)%ip_val = ioff + nodef - 2*nproc + iproc_act
                  endif
                  if ( proc%input_item(i_input)%ip_val .eq. 4 ) then
                     ! delt, voor fractional step aparte ingang in default array
                     ioff   = nopred + nocons + nopa + nofun + nosfun + notot + noloc
                     proc%input_item(i_input)%ip_val = ioff + nodef - nproc + iproc_act
                  endif
                  ipmsa(itel1) = proc%input_item(i_input)%ip_val
                  prvtyp(itel1) = 1
               else
                  if ( proc%input_item(i_input)%ip_val .ne. 0 ) then
                     ipmsa(itel1) = ioffx2 + proc%input_item(i_input)%ip_val
                  else
                     ipmsa(itel1) = 0
                  endif
                  prvtyp(itel1) = 2
               endif
               ipssa(itel1) = 0
               call ip2var
     +              ( ipmsa(itel1),prvvar(itel1), nocons, nopa  , nofun,
     +                nosfun      , notot       , nodisp, novelo, nodef ,
     +                noloc       , ndspx       , nvelx , nlocx , nflux ,
     +                nopred      )
            enddo

            itel0 = itel
            do ioutput = 1 , proc%no_output
               itel = itel + 1
               if ( proc%output_item(ioutput)%indx .gt. 0 ) then
                  itel1 = itel0 + proc%output_item(ioutput)%indx
               else
                  itel1 = itel
                  write(*,*) 'geen volgorde voor proces:',proc%name
               endif
               ipmsa(itel1) = 0
               if ( proc%output_item(ioutput)%type .eq. IOTYPE_SEGMENT_OUTPUT .or.
     +              proc%output_item(ioutput)%type .eq. IOTYPE_SEGMENT_WORK   ) then
                  ipssa(itel1) = proc%output_item(ioutput)%ip_val
                  prvtyp(itel1) = 3
               else
                  if ( proc%output_item(ioutput)%ip_val .ne. 0 ) then
                     ipssa(itel1) = ioffx2 + proc%output_item(ioutput)%ip_val
                  else
                     ipssa(itel1) = 0
                  endif
                  prvtyp(itel1) = 4
               endif
               call ip2var
     +              ( ipssa(itel1),prvvar(itel1), nocons, nopa  , nofun,
     +                nosfun      , notot       , nodisp, novelo, nodef ,
     +                noloc       , ndspx       , nvelx , nlocx , nflux ,
     +                nopred      )
            enddo

            do ibfl = 1 , proc%no_fluxoutput
               itel = itel + 1
               ipmsa(itel) = 0
               ipssa(itel) = 0
               prvvar(itel) = ivflx + nfluxx + ibfl - 1
               prvtyp(itel) = 5
            enddo
            nfluxx = nfluxx + proc%no_fluxoutput
         endif
      enddo

      nipmsa = itel

      if (timon) call timstop( ithndl )
      return
      end
      subroutine ip2var ( ipin  , ivar  , nocons, nopa  , nofun ,
     +                    nosfun, notot , nodisp, novelo, nodef ,
     +                    noloc , ndspx , nvelx , nlocx , nflux ,
     +                    nopred)

      use timers       !   performance timers
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "ip2var", ithndl )

      ! a bit the other way around but set now the new variable pointers
      ! later we will do this different

      ivvol = 1
      ivare = ivvol + 1
      ivflo = ivare + 1
      ivlen = ivflo + 1
      ivcns = ivlen + 2
      ivpar = ivcns + nocons
      ivfun = ivpar + nopa
      ivsfu = ivfun + nofun
      ivcnc = ivsfu + nosfun
      ivmas = ivcnc + notot
      ivder = ivmas + notot
      ivdsp = ivder + notot
      ivvel = ivdsp + nodisp
      ivdef = ivvel + novelo
      ivloc = ivdef + nodef
      ivdsx = ivloc + noloc
      ivvlx = ivdsx + ndspx
      ivlcx = ivvlx + nvelx
      ivflx = ivlcx + nlocx

      ip = ipin
      if ( ip .eq. 1 ) then

         ! volume

         ivar = ivvol
         goto 65
      endif
      if ( ip .le. nopred      ) then

         ! default, pre-defined

         if ( ip .eq. 0 ) ip = 1
         ivar = ivdef + ip - 1
         goto 65
      endif
      ioff =        nopred
      if ( ip .le. nocons+ioff ) then

         ! constant

         ivar = ivcns + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + nocons
      if ( ip .le. nopa  +ioff ) then

         ! parameter

         ivar = ivpar + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + nopa
      if ( ip .le. nofun +ioff ) then

         ! function

         ivar = ivfun + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + nofun
      if ( ip .le. nosfun+ioff ) then

         ! segment function

         ivar = ivsfu + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + nosfun
      if ( ip .le. notot+ioff ) then

         ! concentration

         ivar = ivcnc + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + notot
      if ( ip .le. noloc+ioff ) then

         ! local

         ivar = ivloc + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + noloc
      if ( ip .le. nodef+ioff ) then

         ! default

         ivar = ivdef + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + nodef
      if ( ip .le. nflux+ioff ) then

         ! flux

         ivar = ivflx + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + nflux
      if ( ip .le. 1    +ioff ) then

         ! flow

         ivar = ivflo + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + 1
      if ( ip .le. 1    +ioff ) then

         ! area

         ivar = ivare + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + 1
      if ( ip .le. 2    +ioff ) then

         ! length

         ivar = ivlen + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + 2
      if ( ip .le. nodisp +ioff ) then

         ! dispersion

         ivar = ivdsp + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + nodisp
      if ( ip .le. novelo +ioff ) then

         ! velocity

         ivar = ivvel + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + novelo
      if ( ip .le. nofun +ioff ) then

         ! function

         ivar = ivfun + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + nofun
      if ( ip .le. nocons +ioff ) then

         ! constant

         ivar = ivcns + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + nocons
      if ( ip .le. ndspx +ioff ) then

         ! extra dispersion

         ivar = ivdsx + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + ndspx
      if ( ip .le. nvelx +ioff ) then

         ! extra velocity

         ivar = ivvlx + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + nvelx
      if ( ip .le. nlocx +ioff ) then

         ! local on exchange

         ivar = ivlcx + ip - ioff - 1
         goto 65
      endif
      ioff = ioff + nlocx
      if ( ip .gt. ioff ) then

         ! local on exchange

         ivar = ivdef + ip - ioff - 1
         goto 65
      endif
   65 continue

      if (timon) call timstop( ithndl )
      return
      end
