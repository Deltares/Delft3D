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
module m_rd_stt
use m_srstop


implicit none

contains


   subroutine rd_stt(lunrep, sttfil, statprocesdef, allitems, noinfo, iwar, ierr)
   
   use dlwq_data      ! for definition and storage of data
   use processet      ! processet definitions
   use rd_token       ! tokenized reading
   
   implicit none
   
   integer             , intent(inout) :: lunrep          !< logical unit of report file
   character(len=256)  , intent(inout) :: sttfil          !< filename stt
   type(procespropcoll), intent(inout) :: statprocesdef   !< the statistical proces definition
   type(itempropcoll)  , intent(inout) :: allitems        !< all items of the proces system
   integer             , intent(inout) :: noinfo
   integer             , intent(inout) :: iwar
   integer             , intent(inout) :: ierr
   
   real                                :: vrsion
   integer                             :: ioutpt
   logical                             :: dtflg1
   logical                             :: dtflg3
   
   ilun    = 0
   lch (1) = sttfil
   open ( newunit=ilun(1), file=lch(1), status='old',iostat=ierr)
   if(ierr.ne.0) then 
       write(*,*) 'Error reading file: ',trim(lch(1))
       call srstop(1)
   endif
   npos   = 1000
   cchar  = ';'
   vrsion = -1.0
   noinfo = 0
   iwar = 0
   ierr = 0
   ioutpt  = 0
   dtflg1 = .true.
   dtflg3 = .false.

   call dlwqs1 ( lunrep       , npos         , cchar        , vrsion       , ilun         , lch          , &
                 lstack       , ioutpt       , dtflg1       , dtflg3       , statprocesdef, allitems     , &
                 noinfo       , iwar         , ierr         )
   close(ilun(1))

   end subroutine rd_stt
end module m_rd_stt
