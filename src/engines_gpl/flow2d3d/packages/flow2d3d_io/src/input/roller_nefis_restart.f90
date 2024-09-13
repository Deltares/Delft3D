subroutine roller_nefis_restart(lundia    ,error     ,restid1 , &
                              & i_restart ,ewave1    ,eroll1    ,qxkr      , &
                              & qykr      ,qxkw      ,qykw      ,fxw       ,fyw       , &
                              & wsu       ,wsv       ,guu       ,gvv       , &
                              & hrms      ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: roller_nefis_restart.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/input/roller_nefis_restart.f90 $
!!--description-----------------------------------------------------------------
! Reads initial field condition records from an
! NEFIS flow output map file
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use globaldata
    use string_module
    use dfparall
    use nan_check_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp)       , pointer :: tstart
    real(fp)       , pointer :: dt
    !
    character(256) , pointer :: restid
!
! Global variables
!
    integer                                                      , intent(in)  :: i_restart
    integer                                                                    :: lundia    !  Description and declaration in inout.igs
    logical                                                      , intent(out) :: error
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: eroll1    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: ewave1    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: fxw       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: fyw       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: guu       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: gvv       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: hrms      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: qxkr      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: qxkw      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: qykr      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: qykw      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: wsu       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: wsv       !  Description and declaration in esm_alloc_real.f90
    character(*)                                                               :: restid1
!
! Local variables
!
    integer                                   :: ier1
    integer                                   :: ier2
    integer                                   :: ierr
    integer                     , pointer     :: mfg
    integer                     , pointer     :: mlg
    integer                     , pointer     :: nfg
    integer                     , pointer     :: nlg
    integer                     , pointer     :: nmaxgl
    integer                     , pointer     :: mmaxgl   
    integer                                   :: lrid        ! character variables for files Help var., length of restid
    integer, external                         :: crenef
    integer, external                         :: getelt
    integer, external                         :: clsnef
    integer                                   :: ierror
    integer                                   :: fds
    integer, external                         :: inqmxi
    integer, external                         :: neferr
    integer                                   :: i
    integer                                   :: j
    integer                                   :: max_index
    integer, dimension(3,5)                   :: cuindex
    integer, dimension(3,5)                   :: uindex
    integer, dimension(:,:,:,:), pointer      :: ibuff
    real(fp)                                  :: dtm          ! time step in minutes (flexible precision)
    real(sp)                                  :: dtms         ! time step in minutes (single precision)
    real(sp), dimension(:,:,:), allocatable   :: roll_g
    real(sp), dimension(:,:,:,:), pointer     :: sbuff
    character(1024)                           :: error_string
    character(256)                            :: dat_file
    character(256)                            :: def_file
!
!! executable statements -------------------------------------------------------
!
    tstart              => gdp%gdexttim%tstart
    dt                  => gdp%gdexttim%dt
    restid              => gdp%gdrestart%restid
    mfg                 => gdp%gdparall%mfg
    mlg                 => gdp%gdparall%mlg
    nfg                 => gdp%gdparall%nfg
    nlg                 => gdp%gdparall%nlg
    mmaxgl              => gdp%gdparall%mmaxgl
    nmaxgl              => gdp%gdparall%nmaxgl    
    !    
    restid       = restid1
    nullify(ibuff)
    nullify(sbuff)
    error        = .false.
    error_string = ' '
    call remove_leading_spaces(restid    ,lrid      )
    !
    ! open NEFIS trim-<restid> file
    !
    dat_file = restid(1:lrid)//'.dat'
    def_file = restid(1:lrid)//'.def'
    !
    if (inode == master) then
       ierror = crenef(fds, dat_file, def_file, ' ', 'r')
       if (ierror/= 0) then
          error = .true.
          write(lundia, '(a)') 'Roller: opening restart file not possible'
          goto 9999
       endif
    endif

    write(lundia, '(a)') 'Roller: restarting from ' // trim(dat_file) // ' and ' // trim(def_file)
    !
    ! initialize group index time dependent data
    !
    uindex (3,1) = 1 ! increment in time
    uindex (1,1) = 1
    uindex (2,1) = 1
    !
    ! initialize group index constant data
    !
    cuindex (3,1) = 1 ! increment in time
    cuindex (1,1) = 1
    cuindex (2,1) = 1
    !
    ! the master opens and reads the grid file 
    ! 
    if ( inode /= master ) goto 50 
    
    ierror = getelt(fds, 'map-const', 'DT', cuindex, 1, 4, dtms)

    dtm = dtms
    if (ierror/= 0) then
       ierror = neferr(0,error_string)
       call prterr(lundia    ,'P004'    , error_string)
       error = .true.
       goto 9999
    endif
    ierror = inqmxi(fds, 'map-series', max_index)
    if (ierror/= 0) then
       ierror = neferr(0,error_string)
       call prterr(lundia    ,'P004'    , error_string)
       error = .true.
       goto 9999
    endif

    uindex (1,1) = i_restart
    uindex (2,1) = i_restart

    ! all 11 fields have dimension (nmaxgl, mmaxgl)

    allocate(sbuff(nmaxgl, mmaxgl, 1, 1), stat = ier1)
    allocate(roll_g(nmaxgl,mmaxgl,11), stat = ier2)
    if (ier1 /= 0 .or. ier2 /= 0) then
       call prterr(lundia, 'G020', 'roll_g')
       call d3stop(1, gdp)
    endif
    
    ierror = getelt( fds , 'map-rol-series', 'HS', uindex, 1, mmaxgl*nmaxgl*4, sbuff )  
    if (ierror/= 0) goto 9990
    roll_g(1:nmaxgl,1:mmaxgl,1) = sbuff(1:nmaxgl,1:mmaxgl,1,1)
    ierror = getelt( fds , 'map-rol-series', 'EWAVE1', uindex, 1, mmaxgl*nmaxgl*4, sbuff )       
    if (ierror/= 0) goto 9990
    roll_g(1:nmaxgl,1:mmaxgl,2) = sbuff(1:nmaxgl,1:mmaxgl,1,1)
    ierror = getelt( fds , 'map-rol-series', 'EROLL1', uindex, 1, mmaxgl*nmaxgl*4, sbuff )       
    if (ierror/= 0) goto 9990
    roll_g(1:nmaxgl,1:mmaxgl,3) = sbuff(1:nmaxgl,1:mmaxgl,1,1)
    ierror = getelt( fds , 'map-rol-series', 'QXKR', uindex, 1, mmaxgl*nmaxgl*4, sbuff )       
    if (ierror/= 0) goto 9990
    roll_g(1:nmaxgl,1:mmaxgl,4) = sbuff(1:nmaxgl,1:mmaxgl,1,1)
    ierror = getelt( fds , 'map-rol-series', 'QYKR', uindex, 1, mmaxgl*nmaxgl*4, sbuff )       
    if (ierror/= 0) goto 9990
    roll_g(1:nmaxgl,1:mmaxgl,5) = sbuff(1:nmaxgl,1:mmaxgl,1,1)
    ierror = getelt( fds , 'map-rol-series', 'QXKW', uindex, 1, mmaxgl*nmaxgl*4, sbuff )       
    if (ierror/= 0) goto 9990
    roll_g(1:nmaxgl,1:mmaxgl,6) = sbuff(1:nmaxgl,1:mmaxgl,1,1)
    ierror = getelt( fds , 'map-rol-series', 'QYKW', uindex, 1, mmaxgl*nmaxgl*4, sbuff )       
    if (ierror/= 0) goto 9990
    roll_g(1:nmaxgl,1:mmaxgl,7) = sbuff(1:nmaxgl,1:mmaxgl,1,1)          
    ierror = getelt( fds , 'map-rol-series', 'FXW', uindex, 1, mmaxgl*nmaxgl*4, sbuff )       
    if (ierror/= 0) goto 9990
    roll_g(1:nmaxgl,1:mmaxgl,8) = sbuff(1:nmaxgl,1:mmaxgl,1,1)          
    ierror = getelt( fds , 'map-rol-series', 'FYW', uindex, 1, mmaxgl*nmaxgl*4, sbuff )       
    if (ierror/= 0) goto 9990
    roll_g(1:nmaxgl,1:mmaxgl,9) = sbuff(1:nmaxgl,1:mmaxgl,1,1)          
    ierror = getelt( fds , 'map-rol-series', 'WSU', uindex, 1, mmaxgl*nmaxgl*4, sbuff )       
    if (ierror/= 0) goto 9990
    roll_g(1:nmaxgl,1:mmaxgl,10) = sbuff(1:nmaxgl,1:mmaxgl,1,1)          
    ierror = getelt( fds , 'map-rol-series', 'WSV', uindex, 1, mmaxgl*nmaxgl*4, sbuff )       
    if (ierror/= 0) goto 9990
    roll_g(1:nmaxgl,1:mmaxgl,11) = sbuff(1:nmaxgl,1:mmaxgl,1,1)          
              
    if (.not. nan_check(roll_g, 'roll_g (restart-file)', lundia)) call d3stop(1, gdp)        
    !
    !    end of master part
    !
    ! todo:
    ! - master should not jump to 9999 or call d3stop while reading data,
    !   but jump to 50 and scatter error-flag.
    ! - the same hold for the other nodes when allocate fails.
    ! - scattering all flags in one call is more efficient,
    !   but takes more lines of code.
    !
    ! scatter information to all nodes
50 continue
    !
    ! first scatter flags that are changed by the master
    ! (as ltur1, r1 etc) to all other domains!
    !
    
    call dfsync(gdp)
      
    if ( inode /= master ) then 
      
       allocate (roll_g(nmaxgl,mmaxgl,11), stat = ierr)
       if (ierr /= 0) then
          call prterr(lundia, 'G020', 'restart-data')
          call d3stop(1, gdp)
       endif
    endif
   
    !
    ! scatter arrays s1 etc to all nodes. Note: the broadc must use 'dfreal'
    ! since the arrays are single precision! Otherwise, intractable memory errors will occur. 
    !
    call dfbroadc_gdp ( roll_g, nmaxgl*mmaxgl*11, dfreal, gdp )
    !
    !
    ! 
    ! put copies of parts of hrms etc for each subdomain 
    ! 
    do j = mfg, mlg 
       do i = nfg, nlg 
          hrms  (i-nfg+1,j-mfg+1) = roll_g(i,j, 1) / sqrt(2.0_fp)     
          ewave1(i-nfg+1,j-mfg+1) = roll_g(i,j, 2)             
          eroll1(i-nfg+1,j-mfg+1) = roll_g(i,j, 3) 
          qxkr  (i-nfg+1,j-mfg+1) = roll_g(i,j, 4) * guu(i-nfg+1, j-mfg+1)
          qykr  (i-nfg+1,j-mfg+1) = roll_g(i,j, 5) * gvv(i-nfg+1, j-mfg+1)          
          qxkw  (i-nfg+1,j-mfg+1) = roll_g(i,j, 6) * guu(i-nfg+1, j-mfg+1)
          qykw  (i-nfg+1,j-mfg+1) = roll_g(i,j, 7) * gvv(i-nfg+1, j-mfg+1)          
          fxw   (i-nfg+1,j-mfg+1) = roll_g(i,j, 8) 
          fyw   (i-nfg+1,j-mfg+1) = roll_g(i,j, 9) 
          wsu   (i-nfg+1,j-mfg+1) = roll_g(i,j,10) 
          wsv   (i-nfg+1,j-mfg+1) = roll_g(i,j,11)                               
       enddo
    enddo   
       
    deallocate(roll_g, stat = ierr)
    if (ierr /= 0) then
       call prterr(lundia, 'U021', 'flow_nefis_restart: memory de-allocate error')
    endif
    goto 9999    
    
9990 continue
     ! this part handles problems concerning reading roller fields, if present.       
     ierror = neferr(0,error_string)
     call prterr(lundia    ,'P004'    , error_string)
     error = .true.
     goto 9999

                
9999 continue
    
    if (inode == master) then
      if (associated(sbuff)) deallocate (sbuff)
      ierror = clsnef(fds) 
    endif
    !
    ! todo:
    ! no communication since last call to dfsync, so it can be removed?
    !
end subroutine roller_nefis_restart
