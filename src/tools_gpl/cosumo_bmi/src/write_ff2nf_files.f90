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
!  $Id$
!  $HeadURL$
!-------------------------------------------------------------------------------
!
module m_write_ff2nf_files
    use m_nfl_data
    use message_module
    implicit none
    !
    integer        :: idis
    character(3)   :: c_idis
    character(14)  :: cctime

contains
!
!
!==============================================================================
subroutine write_ff2nf_files()
    use m_nfl_data
    use string_module, only : str_tolower
    !
    ! Locals
    integer              :: slashLoc
    character(MAXSTRLEN) :: nf2ff_dir
    !
    ! Body
    call mess(LEVEL_INFO, "Writing FF2NF files")
    !
    write(cctime,'(f14.3)') current_time/60.0_hp
    !
    ! Convert flow results to input for cormix and write to input file
    !
    ! Write all input files (one for each discharge) in the following do loop
    !
    do idis = 1, nf_num_dif
        nf2ff_dir = trim(basecase(idis))
        slashLoc = index(nf2ff_dir, slash, BACK=.true.)
        if (slashLoc == len_trim(nf2ff_dir)) then
            nf2ff_dir(slashLoc:slashLoc) = ' '
            slashLoc = index(nf2ff_dir, slash, BACK=.true.)
        endif
        write(*,*) "'",str_tolower(trim(nf2ff_dir(slashLoc+1:))),"'"
        if (str_tolower(trim(nf2ff_dir(slashLoc+1:))) == 'cosumo') then
            nf2ff_dir = trim(nf2ff_dir)//slash
        else
            nf2ff_dir = trim(nf2ff_dir)//slash//'COSUMO'//slash
        endif        
        !
        ! Add SubGridModel number and uniqueId to filename to prevent overwriting
        !
        write(c_idis,'(i3.3)') idis
        !
        filename(1) = trim(base_path(idis))//'FF2NF_'//trim(uniqueid)//'_'//trim(runid)//'_SubMod'//c_idis//'_'//trim(adjustl(cctime))//'.xml'
        filename(2) = trim(nf2ff_dir)//'NF2FF'//slash//'NF2FF_'//trim(uniqueid)//'_'//trim(runid)//'_SubMod'//c_idis//'_'//trim(adjustl(cctime))//'.xml'
        filename(3) = trim(basecase(idis))
        waitfiles(idis) = filename(2)
        !
        ! You should get the filenames (the dirs) from the COSUMOsettings.xml
        !
        call wri_FF2NF(idis)
    enddo
end subroutine write_ff2nf_files
!!
!!
!!==============================================================================
subroutine wri_FF2NF(idis)
    use mathconsts, only: pi, raddeg
    use m_nfl_data
    use precision
    use properties
    use m_densprof
    use m_density
    !
    implicit none
    !
    ! Global variables
    !
    integer, intent(in)  :: idis
    !
    ! Local variables
    !
    integer                                :: i
    integer                                :: ierror
    integer                                :: inode
    integer                                :: ipnt
    integer                                :: istat
    integer                                :: k
    integer                                :: npnt
    integer                                :: luntmp
    integer , dimension(:), allocatable    :: idx
    real(hp)                               :: drohj
    real(hp)                               :: frac
    real(hp), dimension(:), allocatable    :: ha
    real(hp), dimension(:), allocatable    :: hd
    real(hp)                               :: hint
    real(hp)                               :: rhoam
    real(hp)                               :: rhoas
    real(hp)                               :: rhoab
    real(hp), dimension(:), allocatable    :: taurel
    real(hp), dimension(:), allocatable    :: uuu
    real(hp), dimension(:), allocatable    :: s1
    real(hp), dimension(:), allocatable    :: ua
    real(hp), dimension(:), allocatable    :: umag
    real(hp), dimension(:), allocatable    :: vvv
    real(hp), dimension(:), allocatable    :: taua
    real(hp)                               :: sal
    real(hp)                               :: temp
    real(hp)                               :: add
    real(hp)                               :: rho0
    real(hp), dimension(:,:), allocatable  :: xy
    character(1)                           :: tab
    character(1)                           :: stype1
    character(50)                          :: stype1_output
    character(1)                           :: stype2
    character(50)                          :: stype2_output
    character(3)                           :: c_inode
    character(80)                          :: node_name
    character(12)                          :: crhoam
    character(500)                         :: crhoam_output
    character(12)                          :: crhoas
    character(500)                         :: crhoas_output
    character(12)                          :: crhoab
    character(500)                         :: crhoab_output
    character(12)                          :: chint
    character(500)                         :: chint_output
    character(12)                          :: cdrohj
    character(500)                         :: cdrohj_output
    character(500)                         :: ctaua
    character(12)                          :: inttostring
    character(1000)                        :: string
    logical                                :: changed
    type(tree_data)              , pointer :: outfile_ptr       ! pointer to the output xml file.
    type(tree_data)              , pointer :: outfile_cosumo_ptr        ! pointer to the cosumo bloxk of the output xml file.
    type(tree_data)              , pointer :: subgrid_ptr
    type(tree_data)              , pointer :: node_ptr
    type(tree_data)              , pointer :: subnode_ptr
    type(tree_data)              , pointer :: data_ptr
    !
    ! for output of the settings
    type(tree_data)              , pointer :: cosumoblock_ptr   ! pointer to the blocks.
    type(tree_data)              , pointer :: settings_node_ptr ! pointer to the settings block.
!
!!! executable statements -------------------------------------------------------
!!
    call mess(LEVEL_INFO, "Subroutine wri_FF2NF")
    !
    inode = 0
    write(c_inode(1:3),'(i3.3)') inode
    !
    tab = char(9)
    !
    ! Read the general diffuser characteritics from cormix input file
    ! Parallel: Only the master partition executes this
    ! the n_diff(idis), etc. will be global indices
    !
    allocate(ha      (no_amb(idis)), stat=ierror)
    allocate(hd      (no_amb(idis)), stat=ierror)
    allocate(ua      (no_amb(idis)), stat=ierror)
    allocate(umag    (no_amb(idis)), stat=ierror)
    allocate(uuu     (no_amb(idis)), stat=ierror)
    allocate(vvv     (no_amb(idis)), stat=ierror)
    allocate(taua    (no_amb(idis)), stat=ierror)
    allocate(taurel  (no_amb(idis)), stat=ierror)
    allocate(s1      (no_amb(idis)), stat=ierror)
    !
    ! Set the depths
    !
    do i = 1, no_amb(idis)
        ha(i) = fm_water_depth(n_amb(idis,i))
        !
        ! This is all the same for all the ambient points, just a copy
        !
        hd(i) = fm_water_depth(n_diff(idis))
    enddo
    !
    ! Compute depth averaged velocity magnitude and direction
    !
    uuu           = 0.0_hp  
    vvv           = 0.0_hp
    stype1_output = ''
    stype2_output = ''
    crhoam_output = ''
    crhoas_output = ''
    crhoab_output = ''
    chint_output  = ''
    cdrohj_output = ''
    do i = 1, no_amb(idis)
        !
        ! Depth averaged velocity magnitude and direction
        !
        ! We now take the velocity at the k-level of the corresponding cell centre.
        ! If we loop over the kfumn0 to kfumx0 of the velocity points (corresponding to n_amb(idis), m_amb(idis) and n_amb(idis), m_amb(idis)-1),
        ! and divide by dzu0/hu and dzv0/hv, would it then be more accurate?
        !
        do k = fm_kbot(n_amb(idis,i)), fm_ktop(n_amb(idis,i))
            frac    = (fm_z_level(k) - fm_z_level(k-1)) / max(ha(i), 0.01_hp)
            uuu(i)  = uuu(i) + fm_velocity_x(k)*frac
            vvv(i)  = vvv(i) + fm_velocity_y(k)*frac
        enddo
        umag(i) = sqrt (uuu(i)*uuu(i) + vvv(i)*vvv(i))
        taua(i) = atan2(vvv(i),uuu(i))*raddeg
        taua(i) = mod(taua(i) + 360.0_hp,360.0_hp)
        ua(i)   = umag(i)
        !
        ! Density profile classification (Cormixtype)
        !
        call determine_densprof(fm_kbot(n_amb(idis,i)), fm_ktop(n_amb(idis,i))   , ha(i) , hd(i)  , &
                              & stype1                , stype2                   , rhoam , &
                              & rhoas                 , rhoab                    , hint  , drohj  )
        !
        ! Compute the density of the discharged water
        !
        if (fm_isalt /= 0) then
            sal  = const_diff(idis,fm_isalt)
            call coupled(fm_kbot(n_intake(idis)), fm_ktop(n_intake(idis)), fm_isalt, &
                       & n_intake(idis)         , k_intake(idis)         , add     )
            sal = sal + add
        endif
        if (fm_itemp /= 0) then
            temp = const_diff(idis,fm_itemp)
            call coupled(fm_kbot(n_intake(idis)), fm_ktop(n_intake(idis)), fm_itemp, &
                       & n_intake(idis)         , k_intake(idis)         , add     )
            temp = temp + add
        endif
        rho0 = density(DENS_UNESCO, temp, sal)
        !
        ! Make character strings from all requested input
        !
        if (stype1 == 'U') then
            write(crhoam(1:12),'(f12.3)') rhoam
            crhoas = '-'
            crhoab = '-'
            stype2 = '-'
            chint  = '-'
            cdrohj = '-'
        else
            crhoam ='-'
            write (crhoas(1:12),'(f12.3)') rhoas
            write (crhoab(1:12),'(f12.3)') rhoab
            if (stype2 == 'C') then
                write (chint (1:12),'(f12.3)') hint
                write (cdrohj(1:12),'(f12.3)') drohj
            else
                chint  = '-'
                cdrohj = '-'
            endif
        endif
        stype1_output = trim(stype1_output) //' '//trim(stype1)
        stype2_output = trim(stype2_output) //' '//trim(stype2)
        crhoam_output = trim(crhoam_output) //' '//trim(crhoam)
        crhoas_output = trim(crhoas_output) //' '//trim(crhoas)
        crhoab_output = trim(crhoab_output) //' '//trim(crhoab)
        chint_output  = trim(chint_output)  //' '//trim(chint)  
        cdrohj_output = trim(cdrohj_output) //' '//trim(cdrohj)
    enddo
    !
    ! sigma0 given as direction relative to north in stead of main flow direction; 0, pointing to east, 90 pointing to north etc.
    ! ctaua is port direction relative to main flow direction
    !
    string = ''
    do i = 1, no_amb(idis)
        taua(i) = mod(taua(i),360.0_hp)
        taurel(i) = mod(sigma0(idis) - taua(i) + 360.0_hp,360.0_hp)
        if (taurel(i) > 179.0_hp .and. taurel(i) < 181.0_hp) then
            taurel(i) = 179.0_hp
        endif
        write (ctaua,'(f12.3)') taurel(i)
        string = trim(string) // ctaua
    enddo
    ctaua = string
    !
    ! Generate a format string
    !
    write(string, '(i0)')no_amb(idis)
    !
    ! Fill new tree with data to be written
    !
    call tree_create('COSUMO Input, created by Delft3D-FLOW', outfile_ptr)
    call tree_put_data(outfile_ptr, transfer(trim(adjustl(filename(1))),node_value), 'STRING:XML')
    call tree_create_node(outfile_ptr, '?xml version="1.0" encoding="utf-8"?', node_ptr)
    call tree_create_node(outfile_ptr, 'COSUMO', outfile_cosumo_ptr)
    call tree_create_node(outfile_cosumo_ptr, 'fileVersion', node_ptr)
    call tree_put_data(node_ptr, transfer("0.3",node_value), 'STRING:XMLDATA')
    call tree_create_node(outfile_cosumo_ptr, 'comm', node_ptr)
    !
    ! Filenames should always be written in Windows style, even on Linux,
    ! Because Cosumo is reading/using it and runs on Windows
    !
    call tree_create_node(node_ptr, 'Filename', subnode_ptr)
    call tree_put_data(subnode_ptr, transfer(trim(adjustl(filename(1))),node_value), 'STRING:XMLDATA')
    call tree_create_node(node_ptr, 'waitForFile', subnode_ptr)
    call tree_put_data(subnode_ptr, transfer(trim(adjustl(filename(2))),node_value), 'STRING:XMLDATA')
    call tree_create_node(node_ptr, 'FFrundir', subnode_ptr)
    call tree_put_data(subnode_ptr, transfer(trim(adjustl(filename(3))),node_value), 'STRING:XMLDATA')
    string = trim(runid) // '.mdu'
    call tree_create_node(node_ptr, 'FFinputFile', subnode_ptr)
    call tree_put_data(subnode_ptr, transfer(trim(adjustl(string)),node_value), 'STRING:XMLDATA')
    call tree_create_node(node_ptr, 'FFuniqueID', subnode_ptr)
    call tree_put_data(subnode_ptr, transfer(trim(adjustl(uniqueid)),node_value), 'STRING:XMLDATA')
    !
    call tree_create_node(outfile_cosumo_ptr, 'SubgridModel', subgrid_ptr)
    write(string,'(i0)') idis
    call tree_create_node(subgrid_ptr, 'SubgridModelNr', subnode_ptr)
    call tree_put_data(subnode_ptr, transfer(trim(adjustl(string)),node_value), 'STRING:XMLDATA')
    write(string,'(e24.17)') current_time/60.0_hp
    call tree_create_node(subgrid_ptr, 'TIME', subnode_ptr)
    call tree_put_data(subnode_ptr, transfer(trim(adjustl(string)),node_value), 'STRING:XMLDATA')
    call tree_create_node(subgrid_ptr, 'constituentsNames', subnode_ptr)
    write(string,'(i0)') fm_numconst
    call tree_put_data(subnode_ptr, transfer(trim(adjustl(string)),node_value), 'STRING:XMLNUMDATALINES')
    do i=1,fm_numconst
        write(inttostring,'(i0)') i
        call tree_create_node(subnode_ptr, trim(inttostring), data_ptr)
        call tree_put_data(data_ptr, transfer(trim(fm_namcon(i)),node_value), "STRING:XMLDATALINE")
    enddo
    !
    call tree_create_node(subgrid_ptr, '<!-- The cormix block is not written anymore -->', subnode_ptr)
    !
    ! Diffuser
    !
    call tree_create_node(subgrid_ptr, 'FFDiff', node_ptr)
    call tree_create_node(node_ptr, '<!-- Z: zero=reference level, down is positive -->', subnode_ptr)
    allocate(idx(1), stat=ierror)
    allocate(xy(1,2), stat=ierror)
    idx(1)  = n_diff(idis)
    xy(1,1) = x_diff(idis)
    xy(1,2) = y_diff(idis)
    !
    call writePointInfoToFF2NF()
    !
    deallocate(idx, stat=ierror)
    deallocate(xy, stat=ierror)
    !
    ! Intake
    !
    call tree_create_node(subgrid_ptr, 'FFIntake', node_ptr)
    call tree_create_node(node_ptr, '<!-- Z: zero=reference level, down is positive -->', subnode_ptr)
    allocate(idx(1), stat=ierror)
    allocate(xy(1,2), stat=ierror)
    idx(1)  = n_intake(idis)
    xy(1,1) = x_intake(idis)
    xy(1,2) = y_intake(idis)
    !
    call writePointInfoToFF2NF()
    !
    deallocate(idx, stat=ierror)
    deallocate(xy, stat=ierror)
    !
    ! Ambients
    !
    call tree_create_node(subgrid_ptr, 'FFAmbient', node_ptr)
    call tree_create_node(node_ptr, '<!-- Z: zero=reference level, down is positive -->', subnode_ptr)
    allocate(idx(no_amb(idis)), stat=ierror)
    allocate(xy(no_amb(idis),2), stat=ierror)
    do i=1,no_amb(idis)
        idx(i) = n_amb(idis,i)
        xy(i,1) = x_amb(idis,i)
        xy(i,2) = y_amb(idis,i)
    enddo
    !
    call writePointInfoToFF2NF()
    !
    deallocate(idx, stat=ierror)
    deallocate(xy, stat=ierror)
    !
    !
    ! cosumofile_ptr%cosumoblock_ptr is from reading.
    ! settings_node_ptr
    ! copying setting files and writing to the FF2NF file.
    !
    
    !
    ! Create Cosumo input tree
    !
    call mess(LEVEL_INFO, "Reading file '", trim(infile), "' ...")
    call tree_create( 'COSUMO FF2NF', cosumofile_ptr )
    call tree_put_data( cosumofile_ptr, transfer(trim(infile),node_value), 'STRING' )
    !
    ! Put file in input tree
    !
    call prop_file('xml',trim(infile),cosumofile_ptr,istat)
    if (istat /= 0) then
        select case (istat)
        case(1)
            call mess(LEVEL_ERROR, "File not found: ", trim(infile))
        case(3)
            errmsg = PREMATURE_EOF // trim(infile)
            call mess(LEVEL_ERROR, errmsg)
        case default
            errmsg = FILE_READ_ERROR // trim(infile)
            call mess(LEVEL_ERROR, errmsg)
        endselect
        return
    endif
       
    call tree_get_node_by_name( cosumofile_ptr, 'cosumo', cosumoblock_ptr )
    if (.not.associated(cosumoblock_ptr)) then
        call mess(LEVEL_ERROR, "ERROR: Tag '<COSUMO>' not found")
        return
    endif
    
    !
    ! Add the settings blocks as read in the COSUMO input file to the output structure
    do i=1, size(cosumoblock_ptr%child_nodes)
        settings_node_ptr => cosumoblock_ptr%child_nodes(i)%node_ptr
        node_name = tree_get_name(settings_node_ptr)
        if (node_name /= "settings") cycle
        call tree_add_node(outfile_cosumo_ptr,settings_node_ptr,istat)      
    enddo   
        
    !
    ! Actually write the FF2NF file
    write(*,'(3a)') "Writing file '", trim(filename(1)), "' ..."
    open (newunit=luntmp, file=trim(filename(1)), status='new', iostat=istat)
    if (istat /= 0) then
        call mess(LEVEL_ERROR, "File '", trim(filename(1)), "' already exists.")
        return
    endif
    call prop_write_xmlfile(luntmp, outfile_ptr, 0, istat)
    close (luntmp)
    !
    deallocate(ha      , stat=ierror)
    deallocate(hd      , stat=ierror)
    deallocate(ua      , stat=ierror)
    deallocate(umag    , stat=ierror)
    deallocate(uuu     , stat=ierror)
    deallocate(vvv     , stat=ierror)
    deallocate(taua    , stat=ierror)
    deallocate(taurel  , stat=ierror)
    
    !
    ! Before destroying the outfile_ptr:
    ! Disconnect the pointers to the "settings" blocks in the cosumo input file
    ! Otherwise these settings blocks will be removed too, which is too early (in case of multiple discharges)
    ! When a reference to a "settings" block is removed, the order of the children is altered: 
    ! start again at child number 1 to find other "settings" blocks to be disconnected
    ! Finished when all children are checked and no "settings" blocks found at all
    changed = .true.
    do while (changed)
        changed = .false.
        do i=1, size(outfile_cosumo_ptr%child_nodes)
            settings_node_ptr => outfile_cosumo_ptr%child_nodes(i)%node_ptr
            node_name = tree_get_name(settings_node_ptr)
            if (node_name /= "settings") cycle
            call tree_disconnect_node(outfile_cosumo_ptr, i, istat)
            changed = .true.
            exit
        enddo
    enddo
    !
    ! Now it's safe to destroy outfile_ptr
    call tree_destroy(outfile_ptr)
    nullify(outfile_cosumo_ptr)
    nullify(subgrid_ptr)
    nullify(node_ptr)
    nullify(subnode_ptr)
    nullify(data_ptr)
    nullify(cosumoblock_ptr)
    nullify(settings_node_ptr)
    nullify(outfile_ptr)
    !
return
!
!
! Subroutine wri_FF2NF contains another subroutine: writePointInfoToFF2NF
! Advantage: all local parameters of wri_FF2NF are available
contains


subroutine writePointInfoToFF2NF
    integer :: k_top
    integer :: k_down
    integer :: k_incr
    !
    npnt = size(idx)
    write(string,'(i0)') (fm_ktop(idx(1)) - fm_kbot(idx(1)) + 1) * npnt
    call tree_create_node(node_ptr, 'XYZ', subnode_ptr)
    call tree_put_data(subnode_ptr, transfer(trim(adjustl(string)),node_value), 'STRING:XMLNUMDATALINES')
    do ipnt=1,npnt
        k_top  = fm_ktop(idx(ipnt))
        k_down = fm_kbot(idx(ipnt))
        k_incr = -1
        hd = fm_water_depth(idx(ipnt))
        do k = k_top, k_down, k_incr
            write(string,'(3(e24.17,x))') xy(ipnt,1), xy(ipnt,2), -(fm_z_level(k) + fm_z_level(k-1))/2.0_hp
            write(inttostring,'(i0)') (ipnt-1)*k + k
            call tree_create_node(subnode_ptr, trim(inttostring), data_ptr)
            call tree_put_data(data_ptr, transfer(trim(string),node_value), "STRING:XMLDATALINE")
        enddo
    enddo
    !
    write(string,'(i0)') npnt
    call tree_create_node(node_ptr, 'waterDepth', subnode_ptr)
    call tree_put_data(subnode_ptr, transfer(trim(adjustl(string)),node_value), 'STRING:XMLNUMDATALINES')
    do ipnt=1,npnt
        write(string,'(e24.17)') fm_water_depth(idx(ipnt))
        write(inttostring,'(i0)') ipnt
        call tree_create_node(subnode_ptr, trim(inttostring), data_ptr)
        call tree_put_data(data_ptr, transfer(trim(string),node_value), "STRING:XMLDATALINE")
    enddo
    !
    write(string,'(i0)') (fm_ktop(idx(1)) - fm_kbot(idx(1)) + 1) * npnt
    call tree_create_node(node_ptr, 'XYvelocity', subnode_ptr)
    call tree_put_data(subnode_ptr, transfer(trim(adjustl(string)),node_value), 'STRING:XMLNUMDATALINES')
    do ipnt=1,npnt
        k_top  = fm_ktop(idx(ipnt))
        k_down = fm_kbot(idx(ipnt))
        k_incr = -1
        do k = k_top, k_down, k_incr
            write(string,'(2(e24.17,x))') fm_velocity_x(k), fm_velocity_y(k)
            write(inttostring,'(i0)') (ipnt-1)*k + k
            call tree_create_node(subnode_ptr, trim(inttostring), data_ptr)
            call tree_put_data(data_ptr, transfer(trim(adjustl(string)),node_value), "STRING:XMLDATALINE")
        enddo
    enddo
    !
    write(string,'(i0)') (fm_ktop(idx(1)) - fm_kbot(idx(1)) + 1) * npnt
    call tree_create_node(node_ptr, 'rho', subnode_ptr)
    call tree_put_data(subnode_ptr, transfer(trim(adjustl(string)),node_value), 'STRING:XMLNUMDATALINES')
    do ipnt=1,npnt
        k_top  = fm_ktop(idx(ipnt))
        k_down = fm_kbot(idx(ipnt))
        k_incr = -1
        do k = k_top, k_down, k_incr
            write(string,'(e24.17)') fm_rho(idx(ipnt))
            write(inttostring,'(i0)') (ipnt-1)*k + k
            call tree_create_node(subnode_ptr, trim(inttostring), data_ptr)
            call tree_put_data(data_ptr, transfer(trim(adjustl(string)),node_value), "STRING:XMLDATALINE")
        enddo
    enddo
    !
    write(string,'(i0)') (fm_ktop(idx(1)) - fm_kbot(idx(1)) + 1) * npnt
    call tree_create_node(node_ptr, 'constituents', subnode_ptr)
    call tree_put_data(subnode_ptr, transfer(trim(adjustl(string)),node_value), 'STRING:XMLNUMDATALINES')
    do ipnt=1,npnt
        k_top  = fm_ktop(idx(ipnt))
        k_down = fm_kbot(idx(ipnt))
        k_incr = -1
        do k = k_top, k_down, k_incr
            string = ' '
            do i=1,fm_numconst
                write(string,'(a,e24.17,x)') trim(string)//' ', fm_constituents(i, idx(ipnt))
            enddo
            write(inttostring,'(i0)') (ipnt-1)*k + k
            call tree_create_node(subnode_ptr, trim(inttostring), data_ptr)
            call tree_put_data(data_ptr, transfer(trim(adjustl(string)),node_value), "STRING:XMLDATALINE")
        enddo
    enddo
end subroutine writePointInfoToFF2NF
end subroutine wri_FF2NF



end module m_write_ff2nf_files
