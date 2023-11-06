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
      module m_values

      implicit none

      contains


      SUBROUTINE VALUES ( NAME   , NOSSS  , VALUE  , NOCONS , NOPA   ,
     *                    NOFUN  , NOSFUN , CONST  , CONAME , PARAM  ,
     *                    PANAME , FUNCS  , FUNAME , SFUNCS , SFNAME ,
     *                    LGET   , IERR   )
      use m_zoek
      use timers

!
      CHARACTER*20 NAME  , CONAME(*), PANAME(*), FUNAME(*), SFNAME(*)
      REAL         VALUE(NOSSS), CONST(NOCONS), PARAM (NOPA ,NOSSS ),
     *                           FUNCS(NOFUN ), SFUNCS(NOSSS,NOSFUN)
      LOGICAL      LGET
      integer      NOSSS, NOCONS , NOPA, NOFUN, NOSFUN, IERR

!     local
      integer INDX
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "values", ithandl )
!
      IERR = 1
      CALL ZOEK20 ( NAME, NOSFUN, SFNAME, 10, INDX )
      IF ( INDX .GT. 0 ) THEN
         if ( lget ) then
            value(1:nosss) = sfuncs(1:nosss,INDX)
         else
            sfuncs(1:nosss,INDX) = value(1:nosss)
         endif
         ierr = 0
         goto 100
      endif
      CALL ZOEK20 ( NAME, NOPA  , PANAME , 10, INDX )
      IF ( INDX .GT. 0 ) THEN
         if ( lget ) then
            value(1:nosss) = param(INDX,1:nosss)
         else
            param(INDX,1:nosss) = value(1:nosss)
         endif
         ierr = 0
         goto 100
      endif
      CALL ZOEK20 ( NAME, NOFUN , FUNAME , 10, INDX )
      IF ( INDX .GT. 0 ) THEN
         if ( lget ) then
            value(1:nosss) = funcs(INDX)
            ierr = 0
         endif
         goto 100
      endif
      CALL ZOEK20 ( NAME, NOCONS, CONAME , 10, INDX )
      IF ( INDX .GT. 0 ) THEN
         if ( lget ) then
            value(1:nosss) = const(INDX)
            ierr = 0
         endif
         goto 100
      endif
!
  100 continue
      if ( timon ) call timstop ( ithandl )
      return
      end

      end module m_values
