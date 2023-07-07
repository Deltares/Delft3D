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

      subroutine prsort( lurep , ProcesDef, notot , nopa     , nosfun,
     +                   syname, nocons   , nofun , constants, paname,
     +                   funame, sfname   , nowarn)

      ! sort processes according to input - output relation, simpel linear sort at the moment

      use m_zoek
      use dlwq_data
      use ProcesSet
      use timers       !   performance timers

      implicit none

      ! decalaration of arguments

      integer                   :: lurep           ! unit number report file
      type(ProcesPropColl)      :: ProcesDef       ! all processes
      integer                   :: notot           ! number of substances
      integer                   :: nopa            ! number of parameters
      integer                   :: nosfun          ! number of segment functions
      character(len=*)          :: syname(*)       ! substance name
      integer                   :: nocons          ! number of constants
      integer                   :: nofun           ! number of functions
      type(t_dlwq_item)   , intent(inout) :: constants       !< delwaq constants list
      character(len=*)          :: paname(*)       ! parameter names
      character(len=*)          :: funame(*)       ! function names
      character(len=*)          :: sfname(*)       ! segment function names
      integer                   :: nowarn          ! number of warnings

      ! local declaration

      type(ProcesProp)          :: aProces         ! array with proces properties
      integer                   :: iproc
      integer                   :: iproc1
      integer                   :: iproc2
      integer                   :: nproc
      integer                   :: i_in, i_out
      integer                   :: i_flx
      integer                   :: ifound
      integer                   :: new_rank
      integer                   :: i_lowest_rank
      integer                   :: nloop
      character(len=20)         :: valnam
      integer                   :: ivalip
      character(len=100)        :: line
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "prsort", ithndl )

      ! loop over the processes

      nproc         = ProcesDef%cursize
      i_lowest_rank = 1
      nloop         = 0

      do

         if ( i_lowest_rank .eq. nproc .or. nloop .gt. nproc) exit

         iproc1        = i_lowest_rank
         i_lowest_rank = nproc
         nloop         = nloop + 1
         do iproc = iproc1 , ProcesDef%cursize

            ! check if output is used by previous processes

            new_rank = iproc
            do iproc2 = 1 , iproc - 1

               do i_out = 1 , ProcesDef%ProcesProps(iproc)%no_output
                  do i_in = 1 , ProcesDef%ProcesProps(iproc2)%no_input
                     call zoek( ProcesDef%ProcesProps(iproc)%output_item(i_out)%name, 1,
     +                          ProcesDef%ProcesProps(iproc2)%input_item(i_in)%name , 20, ifound)
                     if ( ifound .gt. 0 ) then

                        ! see if it not specified in the input, then the process needs to be moved

                        valnam = ProcesDef%ProcesProps(iproc)%output_item(i_out)%name
                        call valpoi ( notot , nopa     , nosfun, syname, nocons,
     +                                nofun , constants, paname, funame, sfname,
     +                                valnam, ivalip   , line  )

                        if ( ivalip .eq. -1 ) then
                           new_rank = iproc2
                           goto 10
                        endif

                     endif
                  enddo
               enddo

              ! also check fluxes

               do i_flx = 1 , ProcesDef%ProcesProps(iproc)%no_fluxoutput
                  do i_in = 1 , ProcesDef%ProcesProps(iproc2)%no_input
                     call zoek( ProcesDef%ProcesProps(iproc)%fluxoutput(i_flx)%name, 1,
     +                          ProcesDef%ProcesProps(iproc2)%input_item(i_in)%name , 20, ifound)
                     if ( ifound .gt. 0 ) then

                        ! see if it not specified in the input, then the process needs to be moved

                        valnam = ProcesDef%ProcesProps(iproc)%fluxoutput(i_flx)%name
                        call valpoi ( notot , nopa     , nosfun, syname, nocons,
     +                                nofun , constants, paname, funame, sfname,
     +                                valnam, ivalip   , line  )

                        if ( ivalip .eq. -1 ) then
                           new_rank = iproc2
                           goto 10
                        endif

                     endif
                  enddo
               enddo

            enddo
   10       continue

            ! insert process at new position

            if ( new_rank .lt. iproc ) then
               i_lowest_rank = min(i_lowest_rank,new_rank)
               aProces = ProcesDef%ProcesProps(iproc)
               do iproc2 = iproc , new_rank + 1 , -1
                  ProcesDef%ProcesProps(iproc2) = ProcesDef%ProcesProps(iproc2-1)
               enddo
               ProcesDef%ProcesProps(new_rank) = aProces
            endif

         enddo
      enddo

      ! check if there is conflict, report it but allow it to continue, to be done
      ! this is tricky because the user has no means to influence the final order

      if ( nloop .gt. nproc ) then
         write(lurep,'(a)') ' WARNING: circular input output relation detected in process library'
         nowarn = nowarn + 1
      endif

      if (timon) call timstop( ithndl )
      return
      end
