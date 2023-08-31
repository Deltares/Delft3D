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
      module m_dlwq14

      implicit none

      contains


      subroutine dlwq14 ( deriv  , notot  , noseg  , itfact , amass2 ,
     &                    idt    , iaflag , dmps   , intopt , isdmp  )

!     Deltares Software Centre

!>\File
!>          Scales deriv and accumulates processes in the balances arrays

!     Created             : april 1988 by L.Postma

!     Logical units       : none

!     Subroutines called  : none

      use timers

      implicit none

!     Parameters          :

!     kind           function         name                   description

      real     ( 4), intent(inout) :: deriv (notot,noseg)  !< Derivatives to be scaled
      integer  ( 4), intent(in   ) :: notot                !< Total number of substances
      integer  ( 4), intent(in   ) :: noseg                !< Number of computational volumes
      integer  ( 4), intent(in   ) :: itfact               !< Factor between process and transport clock
      real     ( 4), intent(inout) :: amass2(notot,5)      !< Mass balance array
      integer  ( 4), intent(in   ) :: idt                  !< Integration time step size
      integer  ( 4), intent(in   ) :: iaflag               !< if 1 then accumulation
      real     ( 4), intent(inout) :: dmps  (notot,*)      !< Integrated fluxes if intopt > 7
      integer  ( 4), intent(in   ) :: intopt               !< Integration suboptions
      integer  ( 4), intent(in   ) :: isdmp (noseg)        !< Pointer dumped segments

!     Local variables

      real     ( 4) atfac           ! helpvariable 1.0/itfact
      real     ( 4) dtfac           ! helpvariable idt
      integer  ( 4) iseg            ! loop variable
      integer  ( 4) ip              ! help variable

      integer  ( 4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq14", ithandl )

!         loop accross deriv

      atfac = 1.0/itfact
      dtfac = idt
      if ( iaflag .eq. 1 ) then
         do iseg = 1 , noseg
            deriv (:,iseg) = deriv(:,iseg) * atfac
            amass2(:,2)    = deriv(:,iseg) * dtfac + amass2(:,2)
         enddo
      else
         do iseg = 1 , noseg
            deriv (:,iseg) = deriv(:,iseg) * atfac
         enddo
      endif

!         accumulate processes for dump segments

      if ( mod(intopt,16) .ge. 8  ) then
         do iseg = 1 , noseg
            ip = isdmp(iseg)
            if ( ip .gt. 0 ) then
               dmps(:,ip) = dmps(:,ip) + deriv(:,iseg) * dtfac
            endif
         enddo
      endif

      if ( timon ) call timstop ( ithandl )

      return
      end

      end module m_dlwq14
