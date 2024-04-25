subroutine rdcul(nsrc, namsrc ,mnksrc, voldis, gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
! Reads dimensions, allocates space for data and reads data from INI file for
! culverts.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use system_utils, only:SHARED_LIB_PREFIX, SHARED_LIB_EXTENSION
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                        , pointer :: lundia
    real(fp)      , dimension(:)   , pointer :: arcul
    real(fp)      , dimension(:)   , pointer :: calfa
    real(fp)      , dimension(:)   , pointer :: clcul
    real(fp)      , dimension(:)   , pointer :: cleng
    real(fp)      , dimension(:,:) , pointer :: closs1
    real(fp)      , dimension(:,:) , pointer :: closs2
    real(fp)      , dimension(:,:) , pointer :: closs3
    real(fp)      , dimension(:)   , pointer :: cmann
    real(fp)      , dimension(:)   , pointer :: htcul
    integer       , dimension(:)   , pointer :: numrel1
    integer       , dimension(:)   , pointer :: numrel2
    integer       , dimension(:)   , pointer :: numrel3
    real(fp)      , dimension(:)   , pointer :: poscul
    real(fp)      , dimension(:,:) , pointer :: wetar1
    real(fp)      , dimension(:,:) , pointer :: wetar2
    real(fp)      , dimension(:,:) , pointer :: wetar3
    real(fp)      , dimension(:)   , pointer :: wtcul
    character(256)                 , pointer :: culverfile
!
! Global variables
!
    integer                          , intent(in)  :: nsrc   ! Description and declaration in dimens.igs
    integer      , dimension(7, nsrc), intent(in)  :: mnksrc ! Description and declaration in r-i-ch.igs
    real(fp)     , dimension(nsrc)   , intent(out) :: voldis ! Description and declaration in esm_alloc_real.f90       
    character(20), dimension(nsrc)   , intent(in)  :: namsrc
!
! Local variables
!
    integer                     :: i
    integer                     :: isrc
    integer                     :: istat
    integer(pntrsize)           :: istat_ptr
    integer                     :: j
    integer                     :: maxnumrel
    integer                     :: numculvert
    integer                     :: numrel
    integer                     :: scansofculvertfile
    integer(pntrsize), external :: open_shared_library
    character(80)               :: culvert_name
    character(80)               :: culvert_type
    character(256)              :: errmsg
    character(256)              :: message
    character(80)               :: paragraph_name
    character(256)              :: rec
    character(20)               :: versionnr
    character(20)               :: versionnrinput
    type(tree_data)  , pointer  :: cul_ptr
    type(tree_data)  , pointer  :: link_ptr
    type(tree_data)  , pointer  :: note_ptr
    !
    integer                        , pointer :: max_integers
    integer                        , pointer :: max_reals
    integer                        , pointer :: max_strings
    character(256)   , dimension(:), pointer :: dll_function
    character(256)   , dimension(:), pointer :: dll_name
    character(256)   , dimension(:), pointer :: dll_usrfil
    integer(pntrsize), dimension(:), pointer :: dll_handle
    integer          , dimension(:), pointer :: dll_integers
    real(hp)         , dimension(:), pointer :: dll_reals
    character(256)   , dimension(:), pointer :: dll_strings
!
!! executable statements -------------------------------------------------------
!
    culverfile       => gdp%gdculver%culverfile
    lundia           => gdp%gdinout%lundia
    !
    max_integers     => gdp%gdculver%max_integers
    max_reals        => gdp%gdculver%max_reals
    max_strings      => gdp%gdculver%max_strings
    dll_name         => gdp%gdculver%dll_name
    dll_function     => gdp%gdculver%dll_function
    dll_usrfil       => gdp%gdculver%dll_usrfil
    dll_handle       => gdp%gdculver%dll_handle
    dll_integers     => gdp%gdculver%dll_integers
    dll_reals        => gdp%gdculver%dll_reals
    dll_strings      => gdp%gdculver%dll_strings
    !
    ! statics
    !
    versionnr      = '01.02'
    versionnrinput = '00.00'
    !
    ! Test for Culvert
    !
    call tree_create_node(gdp%input_tree, 'Culvert', cul_ptr)
    call tree_put_data(cul_ptr, transfer(trim(culverfile), node_value), 'STRING')
    !
    ! Put cul-file in input tree
    !
    call prop_file('ini', trim(culverfile), cul_ptr, istat)
    if (istat /= 0) then
        select case (istat) 
        case(1)
            call prterr(lundia, 'G004', culverfile)
        case(3)
            call prterr(lundia, 'G006', culverfile)
        case default
            call prterr(lundia, 'G007', culverfile)
        endselect
        call d3stop(1, gdp)
    endif
    write (lundia, *) ' '
    write (lundia, *) '***************************************'
    write (lundia, *) '*** Culvert data included     '
    write (lundia, *) '***************************************'
    !
    ! Check version number of culvert input file
    !
    call prop_get_string(cul_ptr, 'CulvertFileInformation', 'FileVersion', &
                       & versionnrinput)
    if (trim(versionnrinput) /= trim(versionnr)) then
       write (message, '(a,a)') 'Culvert input file must have version number ', &
                               & versionnr
       call prterr(lundia, 'U021', trim(message))
       call d3stop(1, gdp)
    endif
    !
    ! Read dimensions from input file
    ! 
    numculvert = 0
    if ( associated(cul_ptr%child_nodes) ) then
        maxnumrel = 0
        do scansofculvertfile = 1, 2
            !
            ! Culvert file will be scanned twice:
            ! first (k=1) determine the value of maxnumrel (and to allocate arrays),
            ! second (k=2) get data and store data in arrays.
            !
            do i = 1, size(cul_ptr%child_nodes)
                !
                ! Does cul_ptr contain a child with name 'Culvert'?
                !
                link_ptr => cul_ptr%child_nodes(i)%node_ptr
                paragraph_name = tree_get_name( link_ptr )
                if ( paragraph_name == 'culvert') then
                    !
                    ! Culvert specification found
                    !
                    ! Get culvert name
                    !
                    numculvert = numculvert+1
                    culvert_name = ' '
                    call prop_get_string(link_ptr, '*', 'Name', culvert_name)
                    !
                    ! Determine indices of mnksrc that contain data for culvert,
                    ! temporary solution for matching old fashioned overstructured
                    ! data in mnksrc and less structured, self contained data in
                    ! ini file format.
                    !
                    do isrc = 1, nsrc
                        if (culvert_name .eq. namsrc(isrc)) then
                            exit
                        elseif (isrc .eq. nsrc) then
                            write (message, '(a)') &
                                 'Culvert name "' // trim(culvert_name) // '" ' // &
                                 'in culvert input file does not match ' // &
                                 'with any culvert name in src file'
                            call prterr(lundia, 'U021', trim(message))
                            call d3stop(1, gdp)
                        endif       
                    enddo
                    !
                    ! Get data depending on culvert type
                    !
                    select case(mnksrc(7, isrc))
                    case(3)
                        !
                        ! This culvert type 'c' is specified by loss coefficient (dimensionless)
                        ! and area of culvert opening (m^2).
                        !
                        if (scansofculvertfile .eq. 2) then
                            call prop_get(link_ptr, '*', 'LossCoefficient', clcul(isrc))
                            call prop_get(link_ptr, '*', 'WetArea', &
                                             & arcul(isrc))
                            write (lundia, '(a,a,a)') 'for discharge ', &
                                                     & namsrc(isrc),':'
                            write (lundia, '(a,a,i4,a,i4,a,a,i4,a,i4,a)') &
                                         & '* culvert of type "C" between  ', &
                                         & ' (m = ', mnksrc(1, isrc), ', n = ', &
                                         & mnksrc(2, isrc), ')  and ', &
                                         & ' (m = ', mnksrc(4, isrc), ', n = ', &
                                         & mnksrc(5, isrc),')'
                            write (lundia,*) '* with wet area ',arcul(isrc)
                            write (lundia,*) '* and loss coefficient ', &
                                            & clcul(isrc)
                            write (lundia,*) '  '
                        endif
                    case(4, 5)
                        !
                        ! These culvert types 'd' and 'e' are specified by height (m), width (m),
                        ! vertical position (m), length (m), energy loss coefficient
                        ! (s/m^(1/3)) and energy loss correction coefficient (dimensionless),  
                        ! and three "functions" (given as datasets) between loss coefficients
                        ! and wet area.
                        !
                        if (scansofculvertfile .eq. 1) then
                            call prop_get_integer(link_ptr, '*', &
                                                & 'NumberOfRelations1', numrel)
                            if (numrel .gt. maxnumrel) maxnumrel = numrel
                            call prop_get_integer(link_ptr, '*', &
                                                & 'NumberOfRelations2', numrel)
                            if (numrel .gt. maxnumrel) maxnumrel = numrel                            
                            call prop_get_integer(link_ptr, '*', &
                                                & 'NumberOfRelations3', numrel)
                            if (numrel .gt. maxnumrel) maxnumrel = numrel                            
                        else                     
                            call prop_get(link_ptr, '*', 'Height', &
                                             & htcul(isrc))
                            call prop_get(link_ptr, '*', 'Width', &
                                             & wtcul(isrc))
                            call prop_get(link_ptr, '*', &
                                             & 'VerticalPosition', poscul(isrc))
                            call prop_get(link_ptr, '*', 'Length', &
                                             & cleng(isrc))
                            call prop_get(link_ptr, '*', &
                                             & 'FrictionCoefficient', cmann(isrc))
                            call prop_get(link_ptr, '*', &
                                             & 'CorrectionCoefficient', &
                                             & calfa(isrc))
                            call prop_get_integer(link_ptr, '*', &
                                                & 'NumberOfRelations1', &
                                                & numrel1(isrc))
                            call prop_get(link_ptr, '*', &
                                              & 'LossCoefficient1', &
                                              & closs1(isrc, 1 : numrel1(isrc)), &
                                              & numrel1(isrc))
                            call prop_get(link_ptr, '*', 'WetArea1', &
                                              & wetar1(isrc, 1 : numrel1(isrc)), &
                                              & numrel1(isrc))
                            call prop_get_integer(link_ptr, '*', &
                                                & 'NumberOfRelations2', &
                                                & numrel2(isrc))
                            call prop_get(link_ptr, '*', &
                                              & 'LossCoefficient2', &
                                              & closs2(isrc, 1 : numrel2(isrc)), &
                                              & numrel2(isrc))
                            call prop_get(link_ptr, '*', 'WetArea2', &
                                              & wetar2(isrc, 1 : numrel2(isrc)), &
                                              & numrel2(isrc))
                            call prop_get_integer(link_ptr, '*', &
                                                & 'NumberOfRelations3', &
                                                & numrel3(isrc))
                            call prop_get(link_ptr, '*', &
                                              & 'LossCoefficient3', &
                                              & closs3(isrc, 1 : numrel3(isrc)), &
                                              & numrel3(isrc))
                            call prop_get(link_ptr, '*', 'WetArea3', &
                                              & wetar3(isrc, 1 : numrel3(isrc)), &
                                              & numrel3(isrc))
                            if (numrel1(isrc).gt. 10 .or. &
                              & numrel2(isrc).gt. 10 .or. &
                              & numrel3(isrc) .gt. 10) then
                               write (message, '(a)') &
                                    & 'Number of culvert coefficients too large'
                               call prterr(lundia, 'U021', trim(message))
                               call d3stop(1, gdp)         
                            endif
                            write (lundia,'(a,a,a)') 'for discharge ', &
                                                    & namsrc(isrc),':'
                            write (lundia, '(a,a,i4,a,i4,a,a,i4,a,i4,a)') &
                                     & '* culvert of type "D" or "E" between  ', &
                                     & '(m = ',mnksrc(1,isrc),         &
                                     & ' n = ',mnksrc(2,isrc),')  and ', &
                                     & '(m = ',mnksrc(4,isrc),         &
                                     & ' n = ',mnksrc(5,isrc),')'
                            write (lundia,*) ' * with height of ',htcul(isrc)
                            write (lundia,*) ' * and width of ',wtcul(isrc)
                            write (lundia,*) ' * and vertical position of bottom of culvert at ', &
                                           & poscul(isrc)
                            write (lundia,*) ' * and length ',cleng(isrc)
                            write (lundia,*) ' * and friction coefficient ', &
                                           & cmann(isrc)
                            write (lundia,*) ' * and correction coefficient ', &
                                           & calfa(isrc)
                            write (lundia,*) '  '
                            write (lundia, '(a,100f6.2)') &
                                      & '* 1: loss coefficient and wet area-table:', &
                                      & (closs1(isrc, j), wetar1(isrc, j), &
                                      & j = 1, numrel1(isrc))
                            write (lundia, '(a,100f6.2)') &
                                      & '* 2: loss coefficient and wet area-table:', &
                                      & (closs2(isrc, j), wetar2(isrc, j), &
                                      & j = 1, numrel2(isrc))
                            write (lundia, '(a,100f6.2)') &
                                      & '* 3: loss coefficient and wet area-table:', &
                                      & (closs3(isrc, j), wetar3(isrc, j), &
                                      & j = 1, numrel3(isrc))
                        endif                                        
                    case(7)
                        !
                        ! This culvert type 'u' requires a DLL and function name.
                        !
                        if (scansofculvertfile .eq. 2) then
                            rec = ' '
                            call prop_get(link_ptr, '*', 'CulvertLib', rec)
                            dll_name(isrc) = rec
                            if (rec /= ' ') then
                               write(rec,'(3a)') SHARED_LIB_PREFIX, trim(dll_name(isrc)), SHARED_LIB_EXTENSION
                               dll_name(isrc) = rec
                               istat_ptr = 0
                               istat_ptr = open_shared_library(dll_handle(isrc), dll_name(isrc))
                               if (istat_ptr /= 0) then
                                  write(errmsg,'(a,a)') 'Can not open shared library ', trim(dll_name(isrc))
                                  call prterr(lundia, 'P004', trim(errmsg))
                                  call d3stop(1, gdp)
                               endif
                               !
                               dll_function(isrc) = ' '
                               call prop_get_string(link_ptr, '*', 'CulvertFunction', dll_function(isrc))
                               dll_usrfil(isrc) = ' '
                               call prop_get_string(link_ptr, '*', 'CulvertFile', dll_usrfil(isrc))
                            endif
                            write (lundia, '(a,a,a)') 'for discharge ', &
                                                     & namsrc(isrc),':'
                            write (lundia, '(a,a,i4,a,i4,a,a,i4,a,i4,a)') &
                                         & '* culvert of type "U" between  ', &
                                         & ' (m = ', mnksrc(1, isrc), ', n = ', &
                                         & mnksrc(2, isrc), ')  and ', &
                                         & ' (m = ', mnksrc(4, isrc), ', n = ', &
                                         & mnksrc(5, isrc),')'
                            write (lundia,*) '* Library used         : ',trim(dll_name(isrc))
                            write (lundia,*) '* Function called      : ',trim(dll_function(isrc))
                            if (dll_usrfil(isrc) /= ' ') then
                               write (lundia,*) '* Specific culvert file: ',trim(dll_usrfil(isrc))
                            endif
                            write (lundia,*) '  '
                        endif
                    case(8)
                        !
                        ! This culvert type 'f' is specified by loss coefficient (dimensionless)
                        ! and area of culvert opening (m^2).
                        !
                        if (scansofculvertfile .eq. 2) then
                            call prop_get(link_ptr, '*', 'LossCoefficient', clcul(isrc))
                            call prop_get(link_ptr, '*', 'Height', &
                                             & htcul(isrc))
                            call prop_get(link_ptr, '*', 'Width', &
                                             & wtcul(isrc))
                            call prop_get(link_ptr, '*', &
                                             & 'VerticalPosition', poscul(isrc))
                            write (lundia, '(a,a,a)') 'for discharge ', &
                                                     & namsrc(isrc),':'
                            write (lundia, '(a,a,i4,a,i4,a,a,i4,a,i4,a)') &
                                         & '* culvert of type "F" between  ', &
                                         & ' (m = ', mnksrc(1, isrc), ', n = ', &
                                         & mnksrc(2, isrc), ')  and ', &
                                         & ' (m = ', mnksrc(4, isrc), ', n = ', &
                                         & mnksrc(5, isrc),')'
                            write (lundia,*) '* with loss coefficient ', &
                                            & clcul(isrc)
                            write (lundia,*) ' * with height of ',htcul(isrc)
                            write (lundia,*) ' * and width of ',wtcul(isrc)
                            write (lundia,*) ' * and vertical position of bottom of culvert at ', &
                                           & poscul(isrc)
                            write (lundia,*) '  '
                        endif
                    case default
                        write (message, '(a)') 'Wrong culvert type in culvert input'
                        call prterr(lundia, 'U021', trim(message))
                        call d3stop(1, gdp)         
                    end select
                endif
            enddo
            if (numculvert == 0) then
                !
                ! Stop if there is no [Culvert] block in the input file (issue Delft3D-14304).
                !
                call prterr(lundia, 'U021', 'No [Culvert] block found in culvert input file; check file.')
                call d3stop(1, gdp)
            endif
            if (scansofculvertfile .eq. 1) then
                !
                ! Most of these allocation statements are independent of this loop: only maxnumrel is
                ! determined in the first scan of the culvert file.
                !
                allocate (gdp%gdculver%arcul(nsrc), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%calfa(nsrc), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%clcul(nsrc), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%cleng(nsrc), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%closs1(nsrc, maxnumrel), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%closs2(nsrc, maxnumrel), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%closs3(nsrc, maxnumrel), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%cmann(nsrc), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%htcul(nsrc), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%numrel1(nsrc), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%numrel2(nsrc), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%numrel3(nsrc), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%poscul(nsrc), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%wetar1(nsrc, maxnumrel), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%wetar2(nsrc, maxnumrel), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%wetar3(nsrc, maxnumrel), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%wtcul(nsrc), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%dll_name(nsrc), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%dll_function(nsrc), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%dll_handle(nsrc), stat = istat)
                if (istat .eq. 0) allocate (gdp%gdculver%dll_usrfil(nsrc), stat = istat)
                max_reals    =  7
                max_integers =  8
                max_strings  =  2
                if (istat==0) allocate (gdp%gdculver%dll_integers(max_integers), stat = istat)
                if (istat==0) allocate (gdp%gdculver%dll_reals   (max_reals   ), stat = istat)
                if (istat==0) allocate (gdp%gdculver%dll_strings (max_strings ), stat = istat)
                if (istat .ne. 0) then
                    call prterr(lundia, 'U021', 'Culvert: memory allocation error')
                    call d3stop(1, gdp)
                endif
                arcul            => gdp%gdculver%arcul
                calfa            => gdp%gdculver%calfa
                clcul            => gdp%gdculver%clcul
                cleng            => gdp%gdculver%cleng
                closs1           => gdp%gdculver%closs1
                closs2           => gdp%gdculver%closs2
                closs3           => gdp%gdculver%closs3
                cmann            => gdp%gdculver%cmann
                htcul            => gdp%gdculver%htcul
                numrel1          => gdp%gdculver%numrel1
                numrel2          => gdp%gdculver%numrel2
                numrel3          => gdp%gdculver%numrel3
                poscul           => gdp%gdculver%poscul
                wetar1           => gdp%gdculver%wetar1
                wetar2           => gdp%gdculver%wetar2
                wetar3           => gdp%gdculver%wetar3
                wtcul            => gdp%gdculver%wtcul
                culverfile       => gdp%gdculver%culverfile
                dll_function     => gdp%gdculver%dll_function
                dll_name         => gdp%gdculver%dll_name
                dll_handle       => gdp%gdculver%dll_handle
                dll_usrfil       => gdp%gdculver%dll_usrfil
                !
                voldis = 0.0_fp
                dll_name = ' '
                dll_function = ' '
            endif
        enddo
    endif
end subroutine rdcul
