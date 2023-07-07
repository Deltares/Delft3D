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
      module processes_input

      integer  ( 4)              :: notot = 0       !< Number of systems
      integer  ( 4)              :: nosys = 0       !< Number of active systems
      integer  ( 4)              :: nocons          !< Number of constants used
      integer  ( 4)              :: noconm          !< Maximum number of constants used
      integer  ( 4)              :: nopa            !< Number of parameters
      integer  ( 4)              :: nofun           !< Number of functions ( user )
      integer  ( 4)              :: nosfun          !< Number of segment functions
      integer  ( 4)              :: nosfunext       !< Number of segment functions from the ext file
      integer  ( 4)              :: nodisp          !< Number of dispersion arrays
      integer  ( 4)              :: novelo          !< Number of velocity arrays
      integer  ( 4)              :: noout_map       !< Total number of map outputs
      integer  ( 4)              :: noout_user      !< Number of user outputs
      integer  ( 4)              :: noout_statt     !< Number of stat map time outputs
      integer  ( 4)              :: noout_state     !< Number of stat map end outputs
      integer  ( 4)              :: noout           !< Total number of outputs
      character(20),allocatable  :: syname_sub(:)   !< substance names from sub-file (before old items replacement)
      character(20),allocatable  :: syname(:)       !< substance names
      character(20),allocatable  :: syunit(:)       !< substance names
      character(20),allocatable  :: coname(:)       !< constant names
      character(20),allocatable  :: paname(:)       !< parameter names
      real     ( 4),allocatable  :: painp(:,:)      !< parameter input
      character(20),allocatable  :: funame(:)       !< function names
      real     ( 8),pointer      :: funinp(:,:)     !< function input
      character(20),allocatable  :: sfunname(:)     !< segm.func. names
      real     ( 8),pointer      :: sfuninp(:,:)    !< segment function input
      character(20),allocatable  :: diname(:)       !< dispersion names
      character(20),allocatable  :: vename(:)       !< velocity names
      character(20),allocatable  :: dename(:)       !< default array names
      character(20),allocatable  :: locnam(:)       !< local array names

      integer                    :: itstrt_process  !< Simulation start time ( scu )
      integer                    :: itstop_process  !< Simulation stop time ( scu )
      real   ( 8)                :: otime           !< t0 (Julian offset of the real time)
      integer                    :: isfact          !< system clock in seconds
      integer                    :: itfact          !< time scale factor processes

! Dispersion arrays, are used in advection/difusion, set through process library. Useable in FM?
! There might also be dispersion arrays from other input in Delwa itself...

      integer( 4)                :: ndspn           !< Number of new dispersion arrays
      integer( 4), allocatable   :: idpnew(: )      !< Pointer to new disp array
      real   ( 4), allocatable   :: dispnw(: ,:)    !< New dispersion array
      integer( 4), allocatable   :: idpnt (: )      !< Pointer to original dispersion
      real   ( 4), allocatable   :: disper(:,:)     !< Original dispersions
      real   ( 4), allocatable   :: dspx  (:)       !< Calculated dispersions
      real   ( 4), allocatable   :: dsto  (:)       !< Factor for calc. dispersions

! Set in processlibry for sedimentation and burial/digging in 'layered sediment' module
! Where to apply? Also instant explicitly calculate and apply fluxes?
!
! There might also be velocity arrays from other input in Delwaq itself...
! novelo is from block 4 (can be deleted?), nveln is from processes

      integer( 4)                :: nveln           !< Nr. of new velocity array's
      real   ( 4), allocatable   :: velonw(:,:)     !< New velocity array
      integer( 4), allocatable   :: ivpnt (: )      !< pointer to original velo
      real   ( 4), allocatable   :: velo  (:,:)     !< Original velocities
      integer( 4)                :: nvelx           !< Nr. of calculated velocities
      real   ( 4), allocatable   :: velx  (:,:)     !< Calculated velocities
      real   ( 4), allocatable   :: vsto  (:)       !< Factor for velocitie

      end module

      module processes_pointers
      use iso_c_binding
      integer(C_INTPTR_T), save           :: dll_opb         !< open proces library dll handle

      integer                    :: nipmsa          !< Length IPMSA
      integer                    :: nproc           !< Number of called processes
      integer                    :: noloc           !< Number of local vars in the proces subsystem
      integer                    :: novar           !< Number of variables
      integer                    :: nflux = 0       !< total number of fluxes
      integer                    :: nodef           !< Number of defaults in proces subsystem
      integer                    :: noutp           !< Number of files in OUTPUT system
      integer                    :: nrvart          !< Number of extra output variables
      integer                    :: nbufmx          !< Length output buffer
      integer                    :: ndspx           !< Number of extra dispersion array's
      integer                    :: nlocx           !< Number of local variables on exch. level
      integer                    :: ndmpar          !< Number of dump area's for balance output
      integer                    :: nogrid          !< Number of defined grids
      integer                    :: nrref           !< Maximum nr of input references for processes

      integer      , allocatable :: prvnio(:)       !< Number of io pointers of process
      integer      , allocatable :: prvpnt(:)       !< Entry in process io pointers prvvar/prvtyp (cummulative of prvnio)

      integer      , allocatable :: iflux(:)        !< Index of first flux op process
      integer      , allocatable :: ipssa(:)        !< Index in ssa  array

      integer      , allocatable :: progrd(:)       !< Process grid
      integer      , allocatable :: prondt(:)       !< Process fractional step
      character*10 , allocatable :: pronam(:)       !< Process name
      integer      , allocatable :: promnr(:)       !< Process number

      integer      , allocatable :: prvvar(:)       !< Index of variable
      integer      , allocatable :: prvtyp(:)       !< Type of variable

      character*20 , allocatable :: varnam(:)       !< Variable name
      integer      , allocatable :: vararr(:)       !< Variable array
      integer      , allocatable :: varidx(:)       !< Variable index in array

      integer      , allocatable :: vartda(:)       !< Variable type of dis-aggregation
      integer      , allocatable :: vardag(:)       !< Variable dis-aggregation variable
      integer      , allocatable :: vartag(:)       !< Variable type of aggregation
      integer      , allocatable :: varagg(:)       !< Variable aggregation variable

      integer      , allocatable :: outvar(:)       !< Variable index of outputs

      integer                    :: arrpoi(78)      !< starting point of the array
      integer                    :: arrknd(78)      !< Kind of array 1=(NOVAR), 2=(NOVAR,NOSEG) or 3=(NOSEG,NOVAR), switch which type of increment should be used
      integer                    :: arrtyp(78)      !< For type 1 the increment is 0, for type 2 it is nofun
      integer                    :: arrbyt(78)      !< Byte size of this array
      integer                    :: arrlen(78)      !< Total length of this array
      integer                    :: arrdm1(78)      !< 'increment' in this array, is the same for all data in this array...
      integer                    :: arrdm2(78)      !< second dimension in this array
      integer                    :: arrdm3(78)      !< second dimension in this array
      character(20)              :: arrnam(78)      !< array names

      integer                    :: ipbloo          !< Number of Bloom module  (if >0)
      integer                    :: ioffbl          !< Offset in IPMSA for Bloom
      
      integer      , allocatable :: idpnw(:)        !< New dispersion pointers
      integer      , allocatable :: ivpnw(:)        !< New velocity pointers
      real         , allocatable :: defaul(:)       !< Values for default constants

      integer      , allocatable :: ipmsa(:)        !< Start index in PMSA array
      integer      , allocatable :: increm(:)       !< Increment in PMSA array

      integer                    :: no_flu          !< Number of fluxes
      real         , allocatable :: stochi(:,:)     !< Stoichiometry factors
      character(10), allocatable :: fluxname(:)     !< Flux names
      character(10), allocatable :: fluxprocname(:) !< Process for each flux
      
      integer                    :: totfluxsys      !< Total number of fluxes for all substances
      integer      , allocatable :: nfluxsys(:)     !< Number of fluxes per substances (dim=nosys)
      integer      , allocatable :: ipfluxsys(:)    !< Index pointer of fluxes per substances (dim=nosys)
      integer      , allocatable :: fluxsys(:)      !< Index of flux for this substance (dim=totfluxsys)

      integer   iivol  / 1/, iiarea / 2/, iiflow / 3/, iileng / 4/, &
                iiconc / 6/, iicons /13/, iiparm /14/, iifunc /15/, &
                iisfun /16/, iiploc /33/, iidefa /34/, iiflux /35/, &
                iidspx /40/, iivelx /41/, iilocx /42/

      end module
