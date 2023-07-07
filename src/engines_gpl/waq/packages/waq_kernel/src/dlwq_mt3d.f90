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
module m_dlwq_mt3d

implicit none

contains


subroutine dlwq_mt3d   ( lunrep, itime , idt   , itstop, notot ,&
                         nosys , noseg , nobnd , syname, bndid ,&
                         ibpnt , conc  , bound , noq   , ipoint,&
                         flow  , ndmpq , iqdmp , dmpq  )

         ! Function:     initialese mt3d coupling
         !               accumulate fluxes over the mt3d exchanges (from the balance output)
         !               set new timestep size
         !               communicate flux averaged boundary concentrations
         !               read new boundary concentration from mt3d

   use m_zoek
   use m_srstop
   use m_gkwini
   use m_getcom
   use dlwq_mt3d_data
   use timers

   implicit none

   ! arguments

   integer                   :: lunrep             ! report file
   integer                   :: itime              ! current time
   integer                   :: idt                ! time step
   integer                   :: itstop             ! stop time
   integer                   :: notot              ! number of substances
   integer                   :: nosys              ! number of active substances
   integer                   :: noseg              ! number of segments
   integer                   :: nobnd              ! number of boundaries
   character(len=20)         :: syname(nosys)      ! substance names
   character(len=20)         :: bndid(nobnd)       ! boundary id
   integer                   :: ibpnt(4,nobnd)     ! boundaruy administration
   real                      :: conc(notot,noseg)  ! concentrations
   real                      :: bound(nosys,nobnd) ! boundary concentrations
   integer  ( 4), intent(in   ) :: noq                  !< total number of interfaces
   integer  ( 4), intent(in   ) :: ipoint(  4   ,noq)   !< from, to, from-1, to+1 volume numbers
   real     ( 4), intent(in   ) :: flow  (noq)          !< flows through the exchange areas in m3/s
   integer(4), intent(in   ) :: ndmpq                 !< number of dumped exchanges
   integer(4), intent(in   ) :: iqdmp  ( noq )        !< pointers dumped exchages
   real   (4), intent(in   ) :: dmpq  (nosys,ndmpq,2) !< flux accumulation array for monitoring areas

   ! locals

   integer                   :: idt_left           ! timestep left to do
   integer                   :: itel               ! itel
   logical                   :: l_exist            ! if the files exists
   integer                   :: luninp             ! unit number configuration file
   integer                   :: ierr               ! error count
   integer                   :: idummy             ! idummy
   real                      :: rdummy             ! rdummy
   character*256             :: group              ! group name in ini file
   character*256             :: key                ! key name in ini file
   character*256             :: keyval             ! key value in ini file
   integer                   :: i                  ! loop counter
   integer                   :: iq                 ! loop counter
   integer                   :: isub               ! loop counter
   integer                   :: isys               ! loop counter
   integer                   :: iseg               ! loop counter
   integer                   :: i_gsl              ! loop counter
   integer                   :: iq_gsl             ! loop counter
   integer                   :: ip1                ! segment pointer
   integer                   :: ip2                ! segment pointer
   integer                   :: igsl_id            ! id
   real                      :: frac
   real                      :: inf_q
   real                      :: upw_q
   real                      :: inf_conc
   real                      :: flux

   integer(4) ithandl /0/
   if ( timon ) call timstrt ( "dlwq_mt3d", ithandl )

   ! Init, lees configuratie, initialiseer IO

   if ( mt3d_init ) then
      call getcom ( '-mt3d'  , 3 , mt3d_active, idummy, rdummy, mt3d_config, ierr)
      if ( mt3d_active ) then
         write(lunrep,*)
         write(lunrep,*) 'found switch -mt3d : coupling with mt3d activated'
         if ( ierr .ne. 0 ) then
            write(*,*) 'Error initialising mt3d coupling'
            write(lunrep,*) 'ERROR : reading configuration file for mt3d from command line'
            call srstop(1)
         endif
         write(lunrep,'(2a)') ' configuration filename : ', trim(mt3d_config)
         inquire(file = mt3d_config, exist = mt3d_active )
         if ( mt3d_active ) then
            open(newunit=luninp,file=mt3d_config)
            group = 'SOBEK-WAQ / MT3D'
            call gkwini(luninp,group,'mt3dflagfile',mt3dflagfile)
            write(lunrep,'(2a)') ' mt3dflagfile : ', trim(mt3dflagfile)
            call gkwini(luninp,group,'dwqflagfile',dwqflagfile)
            write(lunrep,'(2a)') ' dwqflagfile : ', trim(dwqflagfile)
            call gkwini(luninp,group,'dwqcommfile',dwqcommfile)
            write(lunrep,'(2a)') ' dwqcommfile : ', trim(dwqcommfile)
            call gkwini(luninp,group,'mt3dcommfile',mt3dcommfile)
            write(lunrep,'(2a)') ' mt3dcommfile : ', trim(mt3dcommfile)
            call gi_ini(luninp,group,'idt_hyd',idt_hyd)
            write(lunrep,'(a,i12)') ' delwaq hydrodynamic timestep : ', idt_hyd
            call gl_ini(luninp,group,'online',mt3d_online)
            if ( mt3d_online ) then
               write(lunrep,'(a,i12)') ' online                       : True'
            else
               write(lunrep,'(a,i12)') ' online                       : False'
            endif
            call gl_ini(luninp,group,'mt3d_to_delwaq',mt3d_to_delwaq)
            if ( mt3d_to_delwaq ) then
               write(lunrep,'(a,i12)') ' mt3d_to_delwaq               : True'
            else
               write(lunrep,'(a,i12)') ' mt3d_to_delwaq               : False'
            endif
            call gl_ini(luninp,group,'delwaq_to_mt3d',delwaq_to_mt3d)
            if ( delwaq_to_mt3d ) then
               write(lunrep,'(a,i12)') ' delwaq_to_mt3d               : True'
            else
               write(lunrep,'(a,i12)') ' delwaq_to_mt3d               : False'
            endif
            call gr_ini(luninp,group,'rdt_mt3d',rdt_mt3d)
            write(lunrep,'(a,f12.4)') ' rdt_mt3d                     : ', rdt_mt3d
            idt_mt3d = nint(rdt_mt3d*86400.)

            ! files

            if ( .not. mt3d_online ) then
               if ( delwaq_to_mt3d ) then
                  open(newunit=lundwqcomm,file=dwqcommfile)
               endif
               if ( mt3d_to_delwaq ) then
                  open(newunit=lunmt3dcomm,file=mt3dcommfile,status="old")
               endif
            endif

            ! map boundaries

            call gi_ini(luninp,group,'no_gsl',no_gsl)
            write(lunrep,'(a,i12)') ' number of gsl nodes : ', no_gsl
            allocate(ib_gsl(no_gsl))
            do i = 1, no_gsl
               write(key,'(''gsl'',i8.8)') i
               call gkwini(luninp,group,key,keyval)
               call zoek(keyval,nobnd,bndid,20,ib_gsl(i))
               if ( ib_gsl(i) .le. 0 ) then
                  write(lunrep,'(3a,i12)') 'gsl node ',trim(keyval),' not found in delwaq schematisation'
               else
                  write(lunrep,'(3a,i12)') 'gsl node ',trim(keyval),' mapped to boundery nr:',ib_gsl(i)
               endif
            enddo

            ! map substances

            call gi_ini(luninp,group,'no_sub_gsl',no_sub_gsl)
            write(lunrep,'(a,i12)') ' number of substances coupled : ', no_sub_gsl
            allocate(isub_gsl(no_sub_gsl))
            do i = 1, no_sub_gsl
               write(key,'(''sub'',i8.8)') i
               call gkwini(luninp,group,key,keyval)
               call zoek(keyval,nosys,syname,20,isub_gsl(i))
               if ( isub_gsl(i) .le. 0 ) then
                  write(lunrep,'(3a,i12)') 'substance ',trim(keyval),' not found in delwaq schematisation'
               else
                  write(lunrep,'(3a,i12)') 'substance ',trim(keyval),' mapped to substance nr:',isub_gsl(i)
               endif
            enddo
            close (luninp)

            allocate(gsl_conc_inf(no_sub_gsl,no_gsl))
            allocate(gsl_conc_upw(no_sub_gsl,no_gsl))
            allocate(gsl_cum_inf(no_sub_gsl,no_gsl))
            allocate(gsl_cum_upw(no_sub_gsl,no_gsl))
            allocate(gsl_prev_inf(no_sub_gsl,no_gsl))
            allocate(gsl_prev_upw(no_sub_gsl,no_gsl))
            allocate(gsl_q(no_gsl))
            allocate(gsl_cumq(no_gsl))
            allocate(gsl_q_inf(no_gsl))
            allocate(gsl_q_upw(no_gsl))
            allocate(gsl_cumq_inf(no_gsl))
            allocate(gsl_cumq_upw(no_gsl))
            idt_delwaq  = idt
            itstop_mt3d = -2**30
            itstop_hyd  = -2**30
            gsl_prev_inf = 0.0
            gsl_prev_upw = 0.0

            ! set up exchanges involved administration
            ! for the moment max initialisation (noq)

            allocate(iq_iq_gsl(noq))
            allocate(igsl_iq_gsl(noq))
            noq_gsl = 0
            do iq = 1, noq
               ip1 = ipoint(1,iq)
               ip2 = ipoint(2,iq)
               if ( ip1 .lt. 0 .and. ip2 .gt. 0 ) then
                  do i_gsl = 1, no_gsl
                     if ( -ip1 .eq. ib_gsl(i_gsl) ) then
                        noq_gsl = noq_gsl + 1
                        iq_iq_gsl(noq_gsl) = iq
                        igsl_iq_gsl(noq_gsl) = i_gsl

                        ! check if exchange is in balance output

                        if ( iqdmp(iq) .eq. 0 ) then
                           write(lunrep,*) 'ERROR : mt3d boundary exchange not in balance'
                           write(lunrep,*) 'boundary:',ib_gsl(i_gsl)
                           call srstop(1)
                        endif
                        exit
                     endif
                  enddo
               elseif ( ip2 .lt. 0 .and. ip1 .gt. 0 ) then
                  do i_gsl = 1, no_gsl
                     if ( -ip2 .eq. ib_gsl(i_gsl) ) then
                        noq_gsl = noq_gsl + 1
                        iq_iq_gsl(noq_gsl) = -iq
                        igsl_iq_gsl(noq_gsl) = i_gsl

                        ! check if exchange is in balance output

                        if ( iqdmp(iq) .eq. 0 ) then
                           write(lunrep,*) 'ERROR : mt3d boundary exchange not in balance'
                           write(lunrep,*) 'boundary:',ib_gsl(i_gsl)
                           call srstop(1)
                        endif
                        exit
                     endif
                  enddo
               endif
            enddo
         else
            write(*,*) 'Error initialising mt3d coupling for boundaries'
            write(lunrep,*) 'ERROR : configuration file for mt3d coupling does not exist'
            call srstop(1)
         endif
      endif
      mt3d_init = .false.
   endif

   if ( .not. mt3d_active ) goto 9999 !return

   ! accumulate previous step, problem dmpq is already cummulative
   ! so substract the previously accumulated value, which is reset at the same time as dmpq
   ! flow is accumulated on beforehand at the bottom of this routine

   do i_gsl = 1, no_gsl
      do isub = 1, no_sub_gsl
         gsl_cum_inf(isub,i_gsl) = gsl_cum_inf(isub,i_gsl) - gsl_prev_inf(isub,i_gsl)
         gsl_cum_upw(isub,i_gsl) = gsl_cum_upw(isub,i_gsl) - gsl_prev_upw(isub,i_gsl)
      enddo
   enddo
   gsl_prev_inf = 0.0
   gsl_prev_upw = 0.0
   do iq_gsl = 1, noq_gsl
      iq     = iq_iq_gsl(iq_gsl)
      i_gsl  = igsl_iq_gsl(iq_gsl)
      do isub = 1, no_sub_gsl
         isys = isub_gsl(isub)
         if ( iq .gt. 0 ) then
            gsl_cum_inf(isub,i_gsl) = gsl_cum_inf(isub,i_gsl) + dmpq(isys,iqdmp(iq),2)
            gsl_cum_upw(isub,i_gsl) = gsl_cum_upw(isub,i_gsl) - dmpq(isys,iqdmp(iq),1)
            gsl_prev_inf(isub,i_gsl) = gsl_prev_inf(isub,i_gsl) + dmpq(isys,iqdmp(iq),2)
            gsl_prev_upw(isub,i_gsl) = gsl_prev_upw(isub,i_gsl) - dmpq(isys,iqdmp(iq),1)
         else
            gsl_cum_inf(isub,i_gsl) = gsl_cum_inf(isub,i_gsl) - dmpq(isys,iqdmp(iq),2)
            gsl_cum_upw(isub,i_gsl) = gsl_cum_upw(isub,i_gsl) + dmpq(isys,iqdmp(iq),1)
            gsl_prev_inf(isub,i_gsl) = gsl_prev_inf(isub,i_gsl) - dmpq(isys,iqdmp(iq),2)
            gsl_prev_upw(isub,i_gsl) = gsl_prev_upw(isub,i_gsl) + dmpq(isys,iqdmp(iq),1)
         endif
      enddo
   enddo

   ! see if we reached the end of the mt3d_step

   if ( itime .ge. itstop_mt3d ) then

      ! write previous timestep (except for the first time)

      if ( .not. first_step ) then

         ! make flux averaged concentration (if possible else actual concentration)

         do i_gsl = 1, no_gsl
            do isub = 1, no_sub_gsl
               if ( abs(gsl_cumq_inf(i_gsl)) .gt. 1e-20 ) then

                  ! flux averaged concentration

                  gsl_conc_inf(isub,i_gsl) = gsl_cum_inf(isub,i_gsl)/gsl_cumq_inf(i_gsl)

               else

                  ! no water flux, set actual concentration

                  isys = isub_gsl(isub)
                  iseg = ibpnt(3,ib_gsl(i_gsl))
                  if ( iseg .gt. 0 ) then
                     gsl_conc_inf(isub,i_gsl) = conc(isys,iseg)
                  else
                     gsl_conc_inf(isub,i_gsl) = 0.0
                  endif

               endif
               if ( abs(gsl_cumq_upw(i_gsl)) .gt. 1e-20 ) then

                  ! flux averaged concentration

                  gsl_conc_upw(isub,i_gsl) = gsl_cum_upw(isub,i_gsl)/gsl_cumq_upw(i_gsl)

               else

                  ! no water flux, set actual concentration

                  isys = isub_gsl(isub)
                  iseg = ibpnt(3,ib_gsl(i_gsl))
                  if ( iseg .gt. 0 ) then
                     gsl_conc_upw(isub,i_gsl) = conc(isys,iseg)
                  else
                     gsl_conc_upw(isub,i_gsl) = 0.0
                  endif

               endif
            enddo
         enddo

         if ( delwaq_to_mt3d ) then
            if ( mt3d_online ) then
               open(newunit=lundwqcomm,file=dwqcommfile)
            endif
            write(lundwqcomm,*) rdt_mt3d
            do i_gsl = 1, no_gsl
               write(lundwqcomm,'(i10,100(1x,e12.4))') i_gsl,                                               &
                                         gsl_cumq_upw(i_gsl),(gsl_conc_upw(isub,i_gsl),isub=1,no_sub_gsl), & ! fluxes and concentrations at nodes
                                         gsl_cumq_inf(i_gsl),(gsl_conc_inf(isub,i_gsl),isub=1,no_sub_gsl)    ! infiltration
            enddo
            if ( mt3d_online ) then
               close(lundwqcomm)
               open(newunit=lundwqflag,file=dwqflagfile)
               close(lundwqflag)
            endif
         endif

      else

         first_step = .false.

      endif

      ! zero the cummulative flux arrays

      gsl_cumq     = 0.0
      gsl_cumq_inf = 0.0
      gsl_cumq_upw = 0.0
      gsl_cum_inf  = 0.0
      gsl_cum_upw  = 0.0

      ! read new mt3d_step, except if we reached itstop

      if ( itime .lt. itstop ) then
         if ( mt3d_to_delwaq ) then
            if ( mt3d_online ) then
               itel = 0
               do
                  inquire(file=mt3dflagfile,exist=l_exist)
                  if(l_exist)then
                      open(newunit=lunmt3dcomm,file=mt3dcommfile,status="old",err=1)
   1                  exit
                  endif
                  itel = itel + 1
                  if ( itel .eq. 1 ) write(*,*) 'waiting for MT3D'
                  call sleep(1)
               enddo
            endif

            write(*,*) 'reading MT3D communication file'
            write(lunrep,*) 'reading MT3D communication file'

            read(lunmt3dcomm,*) rdt_mt3d
            write(lunrep,*) 'length next mt3d step:',rdt_mt3d
            idt_mt3d = nint(rdt_mt3d*86400.)
            do i_gsl = 1, no_gsl
               read(lunmt3dcomm,*) igsl_id,gsl_q_upw(i_gsl),(gsl_conc_upw(isub,i_gsl),isub=1,no_sub_gsl), & ! upw fluxes and concentrations at nodes
                                           gsl_q_inf(i_gsl),(gsl_conc_inf(isub,i_gsl),isub=1,no_sub_gsl)    ! inf fluxes and concentrations at nodes
            enddo

            if ( mt3d_online ) then
               close(lunmt3dcomm,status='delete')
               open(newunit=lunmt3dflag,file=mt3dflagfile,status="old",err=11)
  11           close(lunmt3dflag,status="delete")
            endif

         endif

         itstop_mt3d = itime + idt_mt3d
      else

         ! last timestep no more actions needed
         goto 9999

      endif

   endif

   ! accumulate the flow at the gsl nodes

   gsl_q = 0.0
   do iq_gsl = 1, noq_gsl
      iq     = iq_iq_gsl(iq_gsl)
      i_gsl  = igsl_iq_gsl(iq_gsl)
      if ( iq .gt. 0 ) then
         gsl_q(i_gsl) = gsl_q(i_gsl) - flow(iq)
         gsl_cumq(i_gsl) = gsl_cumq(i_gsl) - flow(iq)*idt
      else
         gsl_cumq(i_gsl) = gsl_cumq(i_gsl) + flow(-iq)*idt
      endif
   enddo

   ! set boundary conditions, assuming explicit time integration, adjust for upward seepage and inflitration

   do i_gsl = 1, no_gsl
      if ( ib_gsl(i_gsl) .gt. 0 ) then
         inf_q =  gsl_q_inf(i_gsl)/86400.
         upw_q =  gsl_q_upw(i_gsl)/86400.
         if ( abs(inf_q+upw_q) .gt. 1.e-20 ) then
            frac  = inf_q/(inf_q+upw_q)
            inf_q =  frac*gsl_q(i_gsl)
            upw_q =  (1.-frac)*gsl_q(i_gsl)
         endif
         gsl_cumq_inf(i_gsl) = gsl_cumq_inf(i_gsl) + inf_q*idt
         gsl_cumq_upw(i_gsl) = gsl_cumq_upw(i_gsl) + upw_q*idt
         do isub = 1, no_sub_gsl
            isys = isub_gsl(isub)
            if ( isys .gt. 0 ) then
               iseg = ibpnt(3,ib_gsl(i_gsl))
               if ( iseg .gt. 0 ) then
                  inf_conc = conc(isys,iseg)
               else
                  inf_conc = 0.0
               endif
               flux = inf_conc*inf_q + gsl_conc_upw(isub,i_gsl)*upw_q
               if ( abs(gsl_q(i_gsl)) .gt. 1.e-20 ) then
                  bound(isys,ib_gsl(i_gsl)) = flux/gsl_q(i_gsl)
               else
                  ! this could mean that the flux is not reached if inf_q == -upw_q
                  bound(isys,ib_gsl(i_gsl)) = 0.0
               endif
            endif
         enddo
      endif
   enddo

   ! see if we reached the end of the hyddro step

   if ( itime .ge. itstop_hyd ) then

      ! set new itstop_hyd

      itstop_hyd  = itime + idt_hyd

   endif

   ! set new timestep for delwaq, we need to reach mt3d step stop

   idt_left = itstop_mt3d - itime
   idt = min(idt_delwaq,idt_left)
   write(*,'(a,4i8)') ' itime, idt_delwaq, idt_left, idt_hyd_left:',itime,idt_delwaq, idt_left, itstop_hyd  - itime

   ! set new timestep for delwaq, we need to reach hyd step stop

   idt_left = itstop_hyd  - itime
   idt = min(idt,idt_left)

9999 continue
   if ( timon ) call timstop ( ithandl )
   return

end subroutine

end module m_dlwq_mt3d
