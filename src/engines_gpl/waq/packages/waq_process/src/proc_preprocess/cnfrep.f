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

      subroutine cnfrep( noalg   , noprot, namprot, nampact, nopralg,
     +                   nampralg)

      use m_dhucas
      use m_dhslen
      use timers       !   performance timers

      integer        noalg , noprot , nopralg
      character*(*)  namprot(noprot)  , nampact(noprot),
     +               nampralg(nopralg)

      include 'data.inc'

      character*10   namep1
      character*10   namep2
      character*10   namep3
      logical        found
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "cnfrep", ithndl )

      ! copy the license from the proto process to the process

      do iproc=1,nproc
         found = .false.
         call dhucas(procid(iproc),namep1,10)

         ! the one to one processes

         do ipro = 1 , noprot
            if ( namep1 .eq. nampact(ipro) ) then
               found = .true.
               do iproc2=1,nproc
                  call dhucas(procid(iproc2),namep2,10)
                  if ( namep2 .eq. namprot(ipro) ) then
                     do ic = 1 , nconf
                        iprcnf = (iproc-1)*nconf + ic
                        iprcnf2= (iproc2-1)*nconf + ic
                        if ( icnpro(iprcnf2).gt.0 ) then
                           icnpro(iprcnf) = icnpro(iprcnf2)
                        endif
                     enddo
                     exit
                  endif
               enddo
               exit
            endif
         enddo

         ! the processes that need to be extended

         if ( .not. found ) then
            do ipro = 1 , nopralg
               do ialg = 1 , noalg
                  namep3 = nampralg(ipro)
                  call dhslen(namep3,ilen)
                  write(namep3(ilen+1:),'(i2.2)') ialg
                  if ( namep1 .eq. namep3 ) then
                     do iproc2=1,nproc
                        call dhucas(procid(iproc2),namep2,10)
                        if ( namep2 .eq. namprot(ipro) ) then
                           do ic = 1 , nconf
                              iprcnf = (iproc-1)*nconf + ic
                              iprcnf2= (iproc2-1)*nconf + ic
                              if ( icnpro(iprcnf2).gt.0 ) then
                                 icnpro(iprcnf) = icnpro(iprcnf2)
                              endif
                           enddo
                           exit
                        endif
                     enddo
                     found =.true.
                     exit
                  endif
               enddo
               if ( found ) exit
            enddo
         endif
      enddo

      if (timon) call timstop( ithndl )
      return
      end
