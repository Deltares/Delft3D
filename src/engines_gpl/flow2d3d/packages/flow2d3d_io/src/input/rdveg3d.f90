subroutine rdveg3d(mmax      ,nmax      ,nmaxus    , &
                 & xz        ,yz        ,gdp       )
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
!
! Reads 3D vegetation input file.
! Allocates related arrays.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use polygon_module, only: ipon
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                             , pointer :: amiss
    integer                              , pointer :: imiss
    integer                              , pointer :: itplant
    integer                              , pointer :: nveg
    integer, dimension(:,:)              , pointer :: planttype
    real(fp), dimension(:,:)             , pointer :: nplants
    real(fp)                             , pointer :: clplant
    character(256)                       , pointer :: filvg3d
    type (dpm_vegetation), dimension(:)  , pointer :: vegs
    type (gd_veg3d)                      , pointer :: gdveg3d
    integer                              , pointer :: lundia
!
! Global variables
!
    integer                                                  , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                  , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                  , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: xz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: yz     !  Description and declaration in esm_alloc_real.f90 
!
! Local variables
!
    integer                                 :: cntveg
    integer                                 :: cntarea
    integer                                 :: i
    integer                                 :: inout
    integer, dimension(2)                   :: inputivals
    integer                                 :: ip
    integer                                 :: istat
    integer                                 :: j
    integer                                 :: k
    integer                                 :: narea
    integer                                 :: nvps
    integer                                 :: np
    integer                                 :: strlen
    integer                                 :: npmax           ! maxim number of points in a polygon
    integer                                 :: vegmatch
    integer                                 :: vps
    real(fp)                                :: cntplants
    real(fp), dimension(5)                  :: inputvals
    real(fp), allocatable, dimension(:,:)   :: nplantdep
    real(fp), allocatable, dimension(:)     :: xpol
    real(fp), allocatable, dimension(:)     :: ypol
    real(fp)                                :: versionnr
    real(fp)                                :: versionnrhigh
    real(fp)                                :: versionnrlow
    character(30)                           :: node_type
    character(80)                           :: parname
    character(300)                          :: message
    character(256)                          :: inputstring
    character(20)                           :: versionnrinput
    character(256)                          :: polygonfile
    character(len=1), dimension(:), pointer :: data_ptr
    logical                                 :: success
    logical                                 :: error
    type(tree_data)               , pointer :: vg3d_ptr
    type(tree_data)               , pointer :: link_ptr
    type(tree_data)               , pointer :: link_ptr2
    type(tree_data)               , pointer :: node_ptr
    type(tree_data)               , pointer :: pol_ptr=>NULL()
!
!! executable statements -------------------------------------------------------
!
    amiss      => gdp%gdconst%amiss
    imiss      => gdp%gdconst%imiss
    itplant    => gdp%gdveg3d%itplant
    nveg       => gdp%gdveg3d%nveg
    planttype  => gdp%gdveg3d%planttype
    nplants    => gdp%gdveg3d%nplants
    clplant    => gdp%gdveg3d%clplant
    filvg3d    => gdp%gdveg3d%filvg3d
    vegs       => gdp%gdveg3d%vegs
    gdveg3d    => gdp%gdveg3d
    lundia     => gdp%gdinout%lundia
    !
    versionnrlow   = 1.00_fp
    versionnrhigh  = 1.99_fp
    versionnrinput = '00.00'
    call tree_create_node( gdp%input_tree, '3D Vegetation', vg3d_ptr )
    call tree_put_data( vg3d_ptr, transfer(trim(filvg3d),node_value), 'STRING' )
    !
    ! Put pla-file in input tree
    !
    call prop_file('ini',trim(filvg3d),vg3d_ptr,istat)
    if (istat /= 0) then
       select case (istat)
       case(1)
          call prterr(lundia, 'G004', filvg3d)
       case(3)
          call prterr(lundia, 'G006', filvg3d)
       case default
          call prterr(lundia, 'G007', filvg3d)
       endselect
       call d3stop(1, gdp)
    endif
    !
    ! "[DPMVFileInformation]" in input file? This old type is not supported anymore
    !
    call tree_get_node_by_name( vg3d_ptr, 'DPMVFileInformation', node_ptr )
    if (associated(node_ptr)) then
       write (message,'(3a)') 'Name "[DPMVFileInformation]" found in file "',trim(filvg3d), &
           & '". This file is too old. Use version 1.00 or higher. See the manual.' 
       call prterr(lundia, 'U021', trim(message))
       call d3stop(1, gdp)
    endif
    !
    ! Put polygon file in input tree (optional)
    !
    call tree_get_node_by_name( vg3d_ptr, 'General', node_ptr )
    call tree_get_node_by_name( node_ptr, 'PolygonFile', pol_ptr )
    if (associated (pol_ptr)) then
       call tree_get_data_string(pol_ptr,polygonfile,success)
       call prop_file('tekal',polygonfile,pol_ptr,istat)
       if (istat /= 0) then
          select case (istat)
          case(1)
             call prterr(lundia, 'G004', polygonfile)
          case(3)
             call prterr(lundia, 'G006', polygonfile)
          case default
             call prterr(lundia, 'G007', polygonfile)
          endselect
          call d3stop(1, gdp)
       endif
    else
     !
     ! A polygon file is not obliged
     !
    endif
    !
    ! Check version number of 3D vegetation input file
    !
    call prop_get(vg3d_ptr,'VegetationFileInformation','FileVersion',versionnrinput)
    read(versionnrinput,fmt=*,iostat=istat) versionnr
    if (istat /= 0) then
       call prterr(lundia, 'U021', 'Unable to read version number from 3D vegetation input file')
       call d3stop(1, gdp)
    endif
    if (       comparereal(versionnr,versionnrlow ) == -1 &
        & .or. comparereal(versionnr,versionnrhigh) ==  1  ) then
       write (message,'(a,f5.2,a)') '3D vegetation input file version number (',versionnr, ') must be 1.xx'
       call prterr(lundia, 'U021', trim(message))
       call d3stop(1, gdp)
    endif
    !
    ! Read ItPlant
    !
    itplant = imiss
    call prop_get(vg3d_ptr,'General','ItPlant',itplant)
    if (itplant == imiss) then
       write (message,'(a,a)') 'Parameter ItPlant not found in file',trim(filvg3d)
       call prterr(lundia, 'U021', trim(message))
       call d3stop(1, gdp)
    endif
    !
    ! Read ClPlant
    !
    clplant = amiss
    call prop_get(vg3d_ptr,'General','ClPlant',clplant)
    if (comparereal(clplant,amiss) == 0) then
       write (message,'(a,a)') 'Parameter ClPlant not found in file',trim(filvg3d)
       call prterr(lundia, 'U021', trim(message))
       call d3stop(1, gdp)
    endif
    !
    ! Read dimensions from input files
    ! Add nvps dimensions to the vegetation part of the input_tree
    ! They are used during reading
    !
    ! Unfortunately, almost the complete input tree must be scanned, just
    ! to get the dimensions.
    ! Result: the following giant if loop
    ! 
    nveg  = 0
    narea = 0
    npmax = 0
    if ( associated(vg3d_ptr%child_nodes) ) then
       do i = 1,size(vg3d_ptr%child_nodes)
          !
          ! Does vg3d_ptr contain a child with name 'Vegetation'?
          !
          link_ptr => vg3d_ptr%child_nodes(i)%node_ptr
          parname = tree_get_name( link_ptr )
          if ( parname == 'vegetation') then
             !
             ! Vegetation specification found
             !
             nveg = nveg + 1
             !
             ! Count the number of childs with name 'Vps'
             !
             nvps = 0
             if ( associated(link_ptr%child_nodes) ) then
                do j = 1,size(link_ptr%child_nodes)
                   link_ptr2 => link_ptr%child_nodes(j)%node_ptr
                   parname = tree_get_name( link_ptr2 )
                   if (trim(parname) == 'vps') nvps = nvps + 1
                enddo
             else
                write (message,'(a,a,a)') 'Vegetation read without vps-lines in pla-file'
                call prterr(lundia, 'U021', trim(message))
                call d3stop(1, gdp)
             endif
             !
             ! Put nvps in the tree
             !
             write(message,'(i0)') nvps
             call tree_create_node( link_ptr, "nvps", node_ptr)
             call tree_put_data( node_ptr, transfer(trim(adjustl(message)),node_value), "STRING")
          elseif ( parname == 'area') then
             narea = narea + 1
          endif
       enddo
    endif
    if ( associated(pol_ptr) ) then
       if ( associated(pol_ptr%child_nodes) ) then
          do i = 1,size(pol_ptr%child_nodes)
             link_ptr => pol_ptr%child_nodes(i)%node_ptr
             call tree_get_data_ptr( link_ptr, data_ptr, node_type )
             inputivals = transfer( data_ptr, inputivals )
             npmax = max(npmax,inputivals(1))
          enddo
       endif
    endif
    !
    ! Allocate arrays used during computation
    !
                  allocate (gdp%gdveg3d%planttype(gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub), stat = istat)
    if (istat==0) allocate (gdp%gdveg3d%nplants  (gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub), stat = istat)
    if (istat==0) allocate (gdp%gdveg3d%vegs(nveg), stat = istat)
    if (istat/=0) then
       call prterr(lundia, 'U021', 'VEG3D: memory alloc error')
       call d3stop(1, gdp)
    endif
    !
    ! update local pointers
    !
    planttype  => gdp%gdveg3d%planttype
    nplants    => gdp%gdveg3d%nplants
    vegs       => gdp%gdveg3d%vegs
    !
    ! Allocate local arrays used for nplant input file in dep-format
    !
    allocate (nplantdep(gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub))
    !
    ! Allocate local arrays used for nplant input with polygons
    !
    allocate (xpol(npmax))
    allocate (ypol(npmax))
    !
    ! initialisation
    !
    planttype = 0
    nplants   = 0.0
    !
    write (lundia,*)
    write (lundia,'(a)') '*** Start of Rigid 3D Vegetation Model input'
    write (lundia,'(a,i0)')    '    Number of vegetations: ',nveg
    write (lundia,'(a,i0)')    '    Number of areas      : ',narea
    write (lundia,'(a,i0)')    '    ItPlant              : ',itplant
    write (lundia,'(a,f10.5)') '    ClPlant              : ',clplant
    !
    ! Finally the input can be read
    !
    cntveg  = 0
    cntarea = 0
    if ( associated(vg3d_ptr%child_nodes) ) then
       do i = 1,size(vg3d_ptr%child_nodes)
          !
          ! Does vg3d_ptr contain a child with name 'vegetation'?
          !
          link_ptr => vg3d_ptr%child_nodes(i)%node_ptr
          parname = tree_get_name( link_ptr )
          if ( parname == 'vegetation') then
             !
             ! Vegetation specification found
             !
             cntveg = cntveg + 1
             !
             ! set vegetation type
             !
             inputstring = ' '
             call prop_get(link_ptr, '*', 'Type', inputstring)
             inputstring = adjustl(inputstring)
             strlen = len(vegs(cntveg)%typename)
             vegs(cntveg)%typename = inputstring(:strlen)
             write (lundia,'(a,i0,a,a)') '    Vegetation number ',cntveg,', type : ',trim(vegs(cntveg)%typename)
             !
             ! read nvps, added to the tree during dimension reading
             !
             nvps = imiss
             call prop_get(link_ptr, '*', 'nvps', nvps)
             if (nvps == imiss) then
                write(message,'(a,i0)') 'errordetecting number of vps-lines in vegetation ',cntveg
                call prterr(lundia, 'U021', message)
                call d3stop(1, gdp)
             else
                vegs(cntveg)%nvps = nvps
             endif
             !write (lundia,'(a,i0,a,i0)') '    Number of vps-lines in vegetation number ',cntveg,':',vegs(cntveg)%nvps
             !
             ! Allocate arrays in structure vegs(cntveg)
             !
                           allocate (gdp%gdveg3d%vegs(cntveg)%dia   (nvps), stat = istat)
             if (istat==0) allocate (gdp%gdveg3d%vegs(cntveg)%nstem (nvps), stat = istat)
             if (istat==0) allocate (gdp%gdveg3d%vegs(cntveg)%cdcoef(nvps), stat = istat)
             if (istat==0) allocate (gdp%gdveg3d%vegs(cntveg)%rho   (nvps), stat = istat)
             if (istat==0) allocate (gdp%gdveg3d%vegs(cntveg)%z     (nvps), stat = istat)
             if (istat/=0) then
                call prterr(lundia, 'U021', 'VEG3D: memory alloc error')
                call d3stop(1, gdp)
             endif
             !
             ! Read vps-lines
             !
             if ( associated(link_ptr%child_nodes) ) then
                vps = 0
                do j = 1,size(link_ptr%child_nodes)
                   !
                   ! Does link_ptr contain a child with name 'vps'?
                   !
                   link_ptr2 => link_ptr%child_nodes(j)%node_ptr
                   parname = tree_get_name( link_ptr2 )
                   if ( parname == 'vps') then
                      vps = vps + 1
                      inputvals = amiss
                      call prop_get(link_ptr2, '*', 'vps', inputvals, 5)
                      do k = 1,4
                         if (comparereal(inputvals(k),amiss) == 0 ) then
                            write(message,'(a,i0,a,i0,a,i0)') 'Unable to read value ',k,' of vps ', &
                                 &                            vps,' of vegetation ',cntveg
                            call prterr(lundia, 'U021', message)
                            call d3stop(1, gdp)
                         endif
                      enddo
                      vegs(cntveg)%z     (vps) = inputvals(1)
                      vegs(cntveg)%dia   (vps) = inputvals(2)
                      vegs(cntveg)%nstem (vps) = int(inputvals(3))
                      vegs(cntveg)%cdcoef(vps) = inputvals(4)
                      vegs(cntveg)%rho   (vps) = inputvals(5)
                      !
                      ! if no rho specified, rho = amiss
                      !
                      if (        comparereal(vegs(cntveg)%rho(vps),amiss) /= 0 &
                          & .and. comparereal(vegs(cntveg)%rho(vps),0.0_fp) /= 1 ) then
                         write(message,'(a,f10.1,a,i0,a,i0)') 'Negative or zero density read (', &
                             & vegs(cntveg)%rho(vps), ') for vps ', vps, ', vegetation ', cntveg
                         call prterr(lundia, 'U021', message)
                         call d3stop(1, gdp)
                      endif
                   endif
                enddo
             endif
             if (comparereal(vegs(cntveg)%rho(1),amiss) /= 0) then
                write (lundia,'(a)') '    Angle of vegetation is calculated using specified densities'
             endif
          elseif ( parname == 'area') then
             !
             ! Area specification found
             !
             cntarea = cntarea + 1
             !
             ! Vegetation Type
             !
             inputstring = ' '
             call prop_get(link_ptr, '*', 'VegetationType', inputstring)
             inputstring = adjustl(inputstring)
             vegmatch = 0
             do j = 1,nveg
                if (trim(vegs(j)%typename) == trim(inputstring)) then
                   vegmatch = j
                   write (lundia,'(a,i0,a,a)') '    Area number ',cntarea,', type     : ',trim(vegs(j)%typename)
                   exit ! matching vegetation found
                endif
             enddo
             if (vegmatch == 0) then
                write(message,'(a,a,a,i0,a)') 'Unable to match VegetationType ',trim(inputstring), &
                     &                        ' (used in Area ',cntarea, &
                     &                        ') with the specified vegetations'
                call prterr(lundia, 'U021', message)
                call d3stop(1, gdp)
             endif
             !
             ! Polygon or dep-file?
             !
             inputstring = ' '
             call prop_get(link_ptr, '*', 'Polygon', inputstring)
             inputstring = adjustl(inputstring)
             if (inputstring /= ' ') then
                !
                ! Polygon used for nplants specification
                !
                if ( .not. associated(pol_ptr) ) then
                   write(message,'(a)') 'Using polygons but no polygon file specified in group [General]'
                   call prterr(lundia, 'U021', message)
                   call d3stop(1, gdp)
                endif
                !
                ! Read NPlants
                !
                cntplants = amiss
                call prop_get(link_ptr, '*', 'NPlants', cntplants)
                if (comparereal(cntplants,amiss) == 0) then
                   write(message,'(a,i0)') 'NPlants not specified for area ',cntarea
                   call prterr(lundia, 'U021', message)
                   call d3stop(1, gdp)
                elseif (comparereal(cntplants,0.0_fp) == -1) then
                   write(message,'(a,i0)') 'Negative value for NPlants in area ',cntarea
                   call prterr(lundia, 'U021', message)
                   call d3stop(1, gdp)
                endif
                !
                ! Search polygon with corresponding name in polygon file
                !
                success = .false.
                do j =1,size(pol_ptr%child_nodes)
                   link_ptr2 => pol_ptr%child_nodes(j)%node_ptr
                   parname = tree_get_name( link_ptr2 )
                   if (trim(parname) == trim(inputstring)) then
                      success = .true.
                      write (lundia,'(a,a)') '                   polygon  : ',trim(parname)
                      exit ! matching polygon found
                   endif
                enddo 
                if (.not. success) then
                   write(message,'(a,a,a,i0,a)') 'Unable to match Polygon ',trim(inputstring), &
                        &                        ' (used in Area ',cntarea, &
                        &                        ') with the specified polygons'
                   call prterr(lundia, 'U021', message)
                   call d3stop(1, gdp)
                endif
                call tree_get_data_ptr( link_ptr2, data_ptr, node_type )
                inputivals = transfer( data_ptr, inputivals )
                np   = inputivals(1)
                xpol = amiss
                ypol = amiss
                !write (lundia,'(a,i0,a,i0)') '    Number of points in vegetation polygon of area ',cntarea,': ',np
                !
                ! read the vegetation polygon points
                !
                do ip = 1, np
                   inputvals = amiss
                   write (parname,'(a,i0)')'row_',ip
                   call tree_get_node_by_name( link_ptr2, parname, node_ptr )
                   call tree_get_data_ptr( node_ptr, data_ptr, node_type )
                   inputvals = transfer( data_ptr, 0., 5 )
                   if (comparereal(inputvals(1),amiss) == 0 .or. &
                     & comparereal(inputvals(2),amiss) == 0        ) then
                      write(message,'(a,i0,a,i0)') 'Unable to read vegetation polygon point ', &
                           &                       ip,' of area ',cntarea
                      call prterr(lundia, 'U021', message)
                      call d3stop(1, gdp)
                   endif
                   xpol(ip) = inputvals(1)
                   ypol(ip) = inputvals(2)
                   !write (lundia,'(a,i3,a,i3,a,f13.5,f13.5)') '    Point ',ip,', vegetation area ',cntarea,':',xpol(ip), ypol(ip)
                enddo
                do k = 2, nmax-1
                   do j = 2, mmax-1
                      call ipon(xpol ,ypol ,np ,xz(k, j) ,yz(k, j) ,inout)
                      if (inout>=0) then
                         planttype(k, j) = vegmatch
                         nplants  (k, j) = cntplants
                      endif
                   enddo
                enddo
             else
                !
                ! Dep-format file used for nplants specification
                !
                inputstring = ' '
                call prop_get(link_ptr, '*', 'NPlantsFile', inputstring)
                inputstring = adjustl(inputstring)
                if (inputstring /= ' ') then
                   nplantdep = 0.0
                   error     = .false.
                   message   = 'formatted'
                   write (lundia,'(a,a)') '                   dep-file : ',trim(inputstring)
                   call depfil(lundia    ,error     ,trim(inputstring),message(1:11)      , &
                             & nplantdep ,1                ,1                  ,gdp%griddim)
                   if (error) call d3stop(1, gdp)
                   !
                   ! Array nplantdep contains positive reals
                   ! All values below 0.0 (including 0.0, -999 and -999.999) should be neglected
                   ! Only positive reals are stored in the nplants-array
                   !
                   do k = 1, nmax
                      do j = 1, mmax
                         if (comparereal(nplantdep(k,j),0.0_fp) > 0) then
                            planttype(k, j) = vegmatch
                            nplants  (k, j) = nplantdep(k,j)
                         endif
                      enddo
                   enddo
                else
                   write(message,'(a,i0)') 'No Polygon or NPlantsFile specified for area',cntarea
                   call prterr(lundia, 'U021', message)
                   call d3stop(1, gdp)
                endif
             endif
          endif
       enddo
    endif
    !
    ! Write nplants to diagnose file
    !
    !do i = 1, nmax
    !   do j = 1, mmax
    !      if (nplants(i,j,1)/=0) then
    !         write (lundia, '(a,i0,a,i0,a,i0,a,i0)') '    (m,n) = (',j,',',i,'): vegetation type = ',nplants(i,j,1),', count = ',nplants(i,j,2)
    !      endif
    !   enddo
    !enddo
    write (lundia,'(a)') '*** End of Rigid 3D Vegetation Model input'
    write (lundia,*)
    deallocate(xpol)
    deallocate(ypol)
    deallocate(nplantdep)
end subroutine rdveg3d
