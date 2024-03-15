!!  Copyright (C)  Stichting Deltares, 2012-2024.
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

!>\file
!>                    m_delwaq1_data  - contains data needed for initialisation of delwaq1

module m_delwaq1_data

    use m_delwaq1_const
    use dlwqgrid_mod        !   for the storage of contraction grids
    use dlwq_hyd_data    !   for definition and storage of data
    use results, only : OutputPointers       !!   for the output names and pointers
    use timers       !   performance timers

    use ProcesSet
    use Workspace
    use Rd_token

    use m_sysn          ! System characteristics
    use m_sysi          ! Timer characteristics
    use m_sysa          ! Pointers in real array workspace
    use m_sysj          ! Pointers in integer array workspace
    use m_sysc          ! Pointers in character array workspace
    use m_waq_data_buffer

    implicit none

    integer :: lun(nlun)              !< unit numbers input / output files
    character(len = lchmax) :: lchar(nlun)            !< file names input / output files
    integer :: iopt(noint)            !< integration option list

    !     work arrays
    integer :: imax                   !< dynamic size integer work array
    integer :: rmax                   !< dynamic size real work array
    integer :: cmax                   !< dynamic size character work array
    integer, allocatable :: iar(:)                 !< integer work array
    real, allocatable :: rar(:)                 !< real work array
    character(len = 20), allocatable :: car(:)                 !< character work array

    type(waq_data_buffer) :: buffer

    !
    !     files, unit numbers, include file stack, input file settings
    !
    integer :: filtype(nlun)
    character(len = lchmax) :: runid                  !< runid
    logical :: dtflg1                 !< first flag concerning time formats
    logical :: dtflg2                 !< second flag concerning time formats
    logical :: dtflg3                 !< third flag concerning time formats
    logical :: has_hydfile            !< does the input file refer to a hyd file?
    integer, dimension(3) :: nexch                  !< number of exchanges in each direction from hyd file
    type(inputfilestack) :: inpfil                 !< input file strucure with include stack and flags
    !
    !     various input-output structures
    !
    integer :: nrftot(noitm)          !< number of function per item
    integer :: nrharm(noitm)          !< number of harmoncs per item
    integer :: ioutps(7, nooutp)      !< output file defintion structure
    character(len = 20), pointer :: psynam(:)              !< substance names read buffer copies into syname
    integer(4), pointer :: multp(:, :)            !< multiplication substances pointer copies into imultp
    character(len = 20), allocatable :: syname(:)              !< substance names final array
    integer(4), allocatable :: imultp(:, :)           !< multiplication substances pointer
    integer, pointer :: nsegdmp(:)             !< number of monitored segments
    integer, pointer :: isegdmp(:)             !< segment numbers of monitored segments
    integer, pointer :: nexcraai(:)            !< number of exchanges used in transects
    integer, pointer :: iexcraai(:)            !< exchange numbers used in transects
    integer, pointer :: ioptraai(:)            !< option number for transects
    type(ProcesPropColl) :: StatProcesDef          !< the statistical proces definition
    type(ItemPropColl) :: AllItems               !< all items of the proces system
    type(t_dlwq_item) :: constants              !< delwaq constants list
    !
    !     help variables
    !
    logical :: nolic                  !< No valid license?
    logical :: lfound                 !< help variable indicating if command line argument is found
    character(len = 20) :: rundat                 !< execution date-time string
    character :: cdummy
    real :: rdummy
    integer(4) :: nomult                 !< number of multiple substances
    integer(4) :: iwidth                 !< width of the output file
    integer(4) :: refday                 !< reference day, varying from 1 till 365
    integer(4) :: ioutpt                 !< flag for more or less output
    logical :: chkpar(2)              !< flags to check for parameters SURF and LENGTH (used for special waste loads)
    type(GridPointerColl) GridPs
    type(OutputPointers) :: Outputs
    integer :: narg                   !< nr of command line arguments
    character(lchmax) :: arg                    !< a command line argument

    integer :: i, k, icmak
    integer :: itota
    integer :: itoti
    integer :: itotc
    integer :: lunrep
    integer :: nosss
    integer :: ierr_alloc
    logical :: unitop
    character(len = 200) :: nameoffile
    integer :: ioerr
    integer(4), save :: ithndl = 0

    !
    !       initialisations
    !
    data lun/14, 15, 16, 17, 18, 19, 20, 21, 22, 23, &
            24, 25, 26, 27, 28, 29, 30, 31, 32, 33, &
            34, 35, 36, 37, 38, 39, 40, 41, 42, 43, &
            44, 45, 46, 47, 48, 49, 50, 51, 52, 53, &
            54, 55, 56, 57, 58, -1, -1, -1, -1, -1/
    data lchar/'-delwaq03.wrk', '-delwaq04.wrk', &    ! lun,lchar  1,  2
            '-harmonic.wrk', '-pointers.wrk', &            !            3,  4
            '-timestep.wrk', '-gridding.wrk', &            !            5,  6
            '-volumes.wrk ', '-to_from.wrk ', &            !            7,  8
            '-dispersi.wrk', '-areas.wrk   ', &            !            9, 10
            '-flows.wrk   ', '-velocity.wrk', &            !           11, 12
            '-lengthes.wrk', '-boundary.wrk', &            !           13, 14
            '-wastload.wrk', '-function.wrk', &            !           15, 16
            '-segfunc.wrk ', '-initials.wrk', &            !           17, 18
            '.mon         ', '.dmp         ', &            !           19, 20
            '.his         ', '.map         ', &            !           21, 22
            '.res         ', '-proces.wrk  ', &            !           23, 24
            '-output.wrk  ', '.inp         ', &            !           25, 26
            '             ', '-delwaq02.wrk', &            !           27, 28
            '.lst         ', '             ', &            !           29, 30
            '-scratch1opt3', '-scratch2opt3', &            !           31, 32
            '-auxfileop1  ', '-proces.def  ', &            !           33, 34
            '.lsp         ', '-stochi.inp  ', &            !           35, 36
            '-bal.his     ', '.hdf         ', &            !           37, 38
            '.adf         ', '-kenmerk.wrk ', &            !           39, 40
            '-filenaam.wrk', '-stat.map    ', &            !           41, 42
            '-stat.mon    ', '             ', &            !           43, 44
            '<delparfile> ', '<ncgridfile> ', &            !           45, 46
            '_his.nc      ', '_bal_his.nc  ', &            !           47, 48
            '_map.nc      ', '_stat_map.nc '/              !           49, 50

    data iopt/10, 11, 12, 13, 14, 15, 16, 17, &
            20, 21, 22, 23, 24, 25, 26, 27, &
            30, 31, 32, 33, 34, 35, 36, 37, &
            40, 41, 42, 43, 44, 45, 46, 47, &
            50, 51, 52, 53, 54, 55, 56, 57, &
            60, 61, 62, 63, 64, 65, 66, 67, &
            70, 71, 72, 73, 74, 75, 76, 77, &
            80, 81, 82, 83, 84, 85, 86, 87, &
            90, 91, 92, 93, 94, 95, 96, 97, &
            100, 101, 102, 103, 104, 105, 106, 107, &
            110, 111, 112, 113, 114, 115, 116, 117, &
            120, 121, 122, 123, 124, 125, 126, 127, &
            130, 131, 132, 133, 134, 135, 136, 137, &
            140, 141, 142, 143, 144, 145, 146, 147, &
            150, 151, 152, 153, 154, 155, 156, 157, &
            160, 161, 162, 163, 164, 165, 166, 167, &
            170, 171, 172, 173, 174, 175, 176, 177, &
            180, 181, 182, 183, 184, 185, 186, 187, &
            190, 191, 192, 193, 194, 195, 196, 197, &
            200, 201, 202, 203, 204, 205, 206, 207, &
            210, 211, 212, 213, 214, 215, 216, 217, &
            220, 221, 222, 223, 224, 225, 226, 227, &
            230, 231, 232, 233, 234, 235, 236, 237, &
            240, 241, 242, 243, 244, 245, 246, 247, &
            250, 251, 252, 253, 254, 255, 256, 255/

end module
