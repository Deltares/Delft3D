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

      module d00sub
      interface

         subroutine dlwq01 ( lun    , syname , nosys  , notot  , nomult ,
     &                       multp  , iwidth , otime  , isfact , refday ,
     &                       vrsion ,ioutpt , ierr   , iwar   )
            integer  ( 4), intent(in   ) :: lun  (*)      ! array with unit numbers
            character(20), pointer       :: syname(:)     ! array with substance names
            integer  ( 4), intent(  out) :: nosys         ! number of transported substances
            integer  ( 4), intent(  out) :: notot         ! total number of substances
            integer  ( 4), intent(  out) :: nomult        ! number of multiple substances
            integer  ( 4), pointer       :: multp (:,:)   ! multiple substance administration
            integer  ( 4), intent(  out) :: iwidth        ! width of the output file
            real     ( 8), intent(  out) :: otime         ! Offset of the system time (Julian)
            integer  ( 4), intent(  out) :: isfact        ! Units (in sec) of the system clock
            integer  ( 4), intent(  out) :: refday        ! Reference day, varying from 1 till 365
            real     ( 4), intent(  out) :: vrsion        ! version number of this input
            integer  ( 4), intent(  out) :: ioutpt        ! flag for more or less output
            integer  ( 4), intent(inout) :: ierr          ! cumulative error   count
            integer  ( 4), intent(inout) :: iwar          ! cumulative warning count
         end subroutine

         subroutine dlwq02 ( lun     , lchar   , filtype , nrftot  , nlines  ,
     &                       npoins  , dtflg1  , dtflg2  , nodump  , iopt    ,
     &                       noint   , iwidth  , dtflg3  , ndmpar  , ntdmps  ,
     &                       noraai  , ntraaq  , nosys   , notot   , nototp  ,
     &                       vrsion  , ioutpt  , nsegdmp , isegdmp , nexcraai,
     &                       iexcraai, ioptraai, ierr    , iwar    )
            integer  ( 4), intent(in   ) :: lun    (*)    ! array with unit numbers
            character( *), intent(inout) :: lchar  (*)    ! array with file names of the files
            integer  ( 4), intent(inout) :: filtype(*)    ! type of binary file
            integer  ( 4), intent(inout) :: nrftot (*)    ! number of function items
            integer  ( 4), intent(inout) :: nlines        ! cumulative record  space
            integer  ( 4), intent(inout) :: npoins        ! cumulative pointer space
            logical      , intent(  out) :: dtflg1        ! 'date'-format 1st timescale
            logical      , intent(  out) :: dtflg2        ! 'date'-format 2nd timescale
            integer  ( 4), intent(  out) :: nodump        ! number of monitoring points output
            integer  ( 4), intent(in   ) :: iopt   (*)    ! array with valid integration options
            integer  ( 4), intent(in   ) :: noint         ! dimension of iopt
            integer  ( 4), intent(in   ) :: iwidth        ! width of the output file
            logical      , intent(  out) :: dtflg3        ! 'date'-format (F;ddmmhhss,T;yydddhh)
            integer  ( 4), intent(  out) :: ndmpar        ! number of dump areas
            integer  ( 4), intent(  out) :: ntdmps        ! total number segments in dump area
            integer  ( 4), intent(  out) :: noraai        ! number of raaien
            integer  ( 4), intent(  out) :: ntraaq        ! total number of exch. in raaien
            integer  ( 4), intent(in   ) :: nosys         ! number of transported substances
            integer  ( 4), intent(in   ) :: notot         ! total number of substances
            integer  ( 4), intent(  out) :: nototp        ! nr of particle substances
            real     ( 4), intent(in   ) :: vrsion        ! version number of this input
            integer  ( 4), intent(in   ) :: ioutpt        ! flag for more or less output
            integer  ( 4), pointer       :: nsegdmp (:)   ! number of volumes in this monitoring area
            integer  ( 4), pointer       :: isegdmp (:)   ! computational volume numbers
            integer  ( 4), pointer       :: nexcraai(:)   ! number of exchanges in this monitoring transect
            integer  ( 4), pointer       :: iexcraai(:)   ! exchange area numbers of the transect
            integer  ( 4), pointer       :: ioptraai(:)   ! option for the transects
            integer  ( 4), intent(inout) :: ierr          ! cumulative error   count
            integer  ( 4), intent(inout) :: iwar          ! cumulative warning count
         end subroutine

      SUBROUTINE DLWQS1 ( LUNREP       , NPOS       ,
     +                    CCHAR        , VRSION     ,
     +                    ILUN         , LCH        ,
     +                    LSTACK       , IOUTPT     ,
     +                    DTFLG1       , DTFLG3     ,
     +                    StatProcesDef, AllItems ,
     +                    NOINFO       , NOWARN   ,
     +                    IERR         )
!
      USE ProcesSet
!
      INTEGER       LUNREP , NPOS   , LSTACK , IOUTPT , NOINFO ,
     +              NOWARN , IERR
      LOGICAL       DTFLG1 , DTFLG3
      REAL          VRSION
      INTEGER       ILUN(*)
      CHARACTER*(*) LCH  (*)
      CHARACTER*1   CCHAR
      type(ProcesPropColl)  :: StatProcesDef       ! the statistical proces definition
      type(ItemPropColl)    :: AllItems            ! all items of the proces system
      END SUBROUTINE

      end interface
      end module

      module subs02
      interface

         subroutine readmp ( lun    , lchar  , filtype, duname , nsegdmp,
     &                       isegdmp, dmpbal , ndmpar , ntdmps , ioutpt ,
     &                       ierr   , iwar   )
            integer  ( 4), intent(in   ) :: lun    (*)    ! array with unit numbers
            character( *), intent(inout) :: lchar  (*)    ! array with file names of the files
            integer  ( 4), intent(inout) :: filtype(*)    ! type of binary file
            character(20), pointer       :: duname (:)    ! name of monitoring areas
            integer  ( 4), pointer       :: nsegdmp(:)    ! number of volumes per monitoring area
            integer  ( 4), pointer       :: isegdmp(:)    ! volumes numbers per monitoring area
            integer  ( 4), pointer       :: dmpbal(:)         !< balance option (flag) per monitoring area
            integer  ( 4), intent(  out) :: ndmpar        ! number of monitoring areas
            integer  ( 4), intent(  out) :: ntdmps        ! total number of volumes in monitoring areas
            integer  ( 4), intent(in   ) :: ioutpt        ! flag for more or less output
            integer  ( 4), intent(inout) :: ierr          ! error   count
            integer  ( 4), intent(inout) :: iwar          ! cumulative warning count
         end subroutine

         subroutine rearaa ( lun     , lchar   , filtype , raname  , nexcraai,
     &                       iexcraai, ioptraai, noraai  , ntraaq  , ioutpt  ,
     &                       ierr    , iwar    )
            integer  ( 4), intent(in   ) :: lun     (*)   ! array with unit numbers
            character( *), intent(inout) :: lchar   (*)   ! array with file names of the files
            integer  ( 4), intent(inout) :: filtype (*)   ! type of binary file
            character(20), pointer       :: raname  (:)   ! name of monitoring areas
            integer  ( 4), pointer       :: nexcraai(:)   ! number of exchanges per monitoring transect
            integer  ( 4), pointer       :: iexcraai(:)   ! exchange numbers per monitoring transect
            integer  ( 4), pointer       :: ioptraai(:)   ! transport summation option for transects
            integer  ( 4), intent(  out) :: noraai        ! number of monitoring transects
            integer  ( 4), intent(  out) :: ntraaq        ! total number of exchanges in monitoring transects
            integer  ( 4), intent(in   ) :: ioutpt        ! flag for more or less output
            integer  ( 4), intent(inout) :: ierr          ! error   count
            integer  ( 4), intent(inout) :: iwar          ! cumulative warning count
         end subroutine

      end interface
      end module
