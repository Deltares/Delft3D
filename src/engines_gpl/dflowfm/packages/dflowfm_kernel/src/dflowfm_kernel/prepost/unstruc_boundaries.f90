!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
! 
! 
module unstruc_boundaries
implicit none

integer, parameter :: max_registered_item_id = 128
integer            :: max_ext_bnd_items      = 64  ! Starting size, will grow dynamically when needed.
character(len=max_registered_item_id), allocatable :: registered_items(:)
integer            :: num_registered_items = 0

contains

subroutine findexternalboundarypoints()             ! find external boundary points
 use m_netw
 use m_flow, filetype_hide => filetype               ! Two stages: 1 = collect elsets for which data is provided
 use m_flowgeom                                      !             2 = add relations between elsets and their providers
 use unstruc_model                                   ! This routine is based upon the network admin only,
 use timespace                                       ! not on the flow admin.
 use m_sferic
 use m_alloc
 use unstruc_messages
 use m_ship
 use properties
 use m_transport
 use m_sobekdfm
 use m_sediment
 use m_partitioninfo
 use system_utils, only: split_filename
 use unstruc_files, only: resolvePath

 implicit none

 character(len=256)    :: filename
 integer               :: filetype
 integer, allocatable  :: kce(:)             ! kc edges (numl)
 integer, allocatable  :: ke(:)              ! kc edges (numl)
 logical               :: jawel
 integer               :: ja_ext_force
 logical               :: ext_force_bnd_used
 integer               :: ierr, method
 double precision      :: return_time
 integer               :: numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf
 integer               :: nx
 integer               :: ierror
 integer               :: num_bc_ini_blocks
 character(len=64)     :: varname

 jatimespace = 1

 return_time = 0
 ja_ext_force = 0
 ext_force_bnd_used = .false.

 if (len(trim(md_extfile)) > 0) then
    inquire (file = trim(md_extfile), exist = jawel)
    if (jawel) then
       if (mext /= 0) then
          ! Close first, if left open after prior flow_geominit().
          ! NOTE: AvD: this if-check relies on the fact that mext is *not* set to 0 in default_flowexternalforcings(), when reinitializing an already initialized model.
          call doclose(mext)
       end if

       call oldfil(mext,md_extfile)
       call split_filename(md_extfile, md_extfile_dir, filename) ! Remember base dir for this ext file
       ja_ext_force = 1
    else
       call qnerror( 'External forcing file '''//trim(md_extfile)//''' not found.', '  ', ' ')
       write(msgbuf, '(a,a,a)') 'External forcing file ''', trim(md_extfile), ''' not found.'
       call err_flush()
    endif
 endif
 if (len(trim(md_extfile_new)) > 0) then
    inquire (file = trim(md_extfile_new), exist = jawel)
    if (jawel) then
       ext_force_bnd_used = .true.
    else
       call qnerror( 'Boundary external forcing file '''//trim(md_extfile_new)//''' not found.', '  ', ' ')
       write(msgbuf, '(a,a,a)') 'Boundary external forcing file ''', trim(md_extfile_new), ''' not found.'
       call err_flush()
    endif
 endif

! if (ja_ext_force == 0 .and. .not. ext_force_bnd_used) then
!    return
! endif

 if ( allocated (xe) ) deallocate(xe, ye, xyen)     ! centre points of all net links, also needed for opening closed boundaries

 !mx1Dend = 0                                        ! count MAX nr of 1D endpoints
 !do L = 1,numl1D
 !   if ( kn(3,L) == 1) then                         ! zeker weten
 !      k1 = kn(1,L) ; k2 = kn(2,L)
 !      if (nmk(k1) == 1 .and. nmk(k2) == 2 .and. lne(1,L) < 0 .or. &
 !          nmk(k2) == 1 .and. nmk(k1) == 2 .and. lne(2,L) < 0 ) then
 !          mx1Dend = mx1Dend + 1
 !      endif
 !   endif
 !enddo
 !
 !
 !nx = numl + mx1Dend

! count number of 2D links and 1D endpoints
 call count_links(mx1Dend, Nx)


 allocate ( xe (nx) ,     stat=ierr ) ; xe = 0      ! used in findexternalboundarypoints
 call aerr('xe (nx)',     ierr, nx)
 allocate ( ye (nx) ,     stat=ierr ) ; ye = 0
 call aerr('ye (nx)',     ierr, nx)
 allocate ( xyen(2, nx) , stat=ierr ) ; xyen = 0d0
 call aerr('xyen(2, nx)', ierr, nx)

                                                    ! some temp arrays

 if (allocated(kez)) then
    ! If flow_geominit was called separately from a flow_modelinit:
    deallocate (             kez,     keu,     kes,     ketm,     kesd,     keuxy,     ket,     ken,     ke1d2d,     keg,     ked,     kep,     kedb,     keklep,     kevalv,     kegs,     kegen,     itpez,     itpenz,     itpeu,      itpenu,     kew)
 end if
 if (allocated(ftpet) ) then
    deallocate(ftpet)
 end if
 allocate ( kce(nx), ke(nx), kez(nx), keu(nx), kes(nx), ketm(nx), kesd(nx), keuxy(nx), ket(nx), ken(nx), ke1d2d(nx), keg(nx), ked(nx), kep(nx), kedb(nx), keklep(nx), kevalv(nx), kegs(nx), kegen(nx), itpez(nx), itpenz(nx), itpeu(nx) , itpenu(nx), kew(nx), ftpet(nx), stat=ierr )
 call aerr('kce(nx), ke(nx), kez(nx), keu(nx), kes(nx), ketm(nx), kesd(nx), keuxy(nx), ket(nx), ken(nx), ke1d2d(nx), keg(nx), ked(nx), kep(nx), kedb(nx), keklep(nx), kevalv(nx), kegs(nx), kegen(nx), itpez(nx), itpenz(nx), itpeu(nx) , itpenu(nx), kew(nx), ftpet(nx)',ierr, 17*nx)
            kce = 0; ke = 0; kez = 0; keu = 0; kes = 0; ketm = 0; kesd = 0; keuxy = 0; ket = 0; ken = 0; ke1d2d = 0; keg = 0; ked = 0; kep=  0; kedb=0  ; keklep=0  ; kevalv=0  ; kegen= 0; itpez = 0; itpenz = 0; itpeu = 0 ; itpenu = 0 ; kew = 0; ftpet = 1d6

 if (allocated(ketr) ) deallocate(ketr)
 allocate ( ketr(nx,1), stat = ierr )
 call aerr('ketr(nx,1)', ierr, nx)
            ketr = 0

 if ( allocated(nbndtr) ) deallocate(nbndtr)
 allocate ( nbndtr(1), stat = ierr )
 call aerr('nbndtr(1)', ierr, 1 )
            nbndtr = 0

 if ( allocated(trnames) ) deallocate(trnames)
 allocate ( trnames(1), stat = ierr )
 call aerr('trnames(1)', ierr, 1 )
            trnames(1) = ''
 numtracers = 0

 if (allocated(kesf) ) deallocate(kesf)
 allocate ( kesf(1,nx), stat = ierr )   ! would have been nice to have stmpar%lsedsus,
 call aerr('kesf(1,nx)', ierr, nx)      ! but no can do, jammer de bammer...
 kesf = 0

 if ( allocated(nbndsf) ) deallocate(nbndsf)
 allocate ( nbndsf(1), stat = ierr )
 call aerr('nbndsf(1)', ierr, 1 )
 nbndsf = 0

 if ( allocated(sfnames) ) deallocate(sfnames)
 allocate ( sfnames(1), stat = ierr )
 call aerr('sfnames(1)', ierr, 1 )
 sfnames = ''
 numfracs = 0

 call make_mirrorcells(Nx, xe, ye, xyen, kce, ke, ierror)

 if ( jampi.eq.1 ) then
! disable mirror cells that are not mirror cells in the whole model by setting kce=0
    call partition_reduce_mirrorcells(Nx, kce, ke, ierror)
 end if

 nbndz = 0                                           ! startindex waterlevel bnds
 nbndu = 0                                           ! startindex velocity   bnds
 nbnds = 0                                           ! startindex salinity   bnds
 nbndtm = 0                                          ! startindex temperature bnds
 nbndt = 0                                           ! startindex tangential vel. bnds
 nbnduxy = 0                                         ! startindex uxuy vel. bnds
 nbndn = 0                                           ! startindex normal     vel. bnds
 nbnd1d2d = 0                                        ! startindex 1d2d bnds
 ngate = 0                                           ! startindex gate links
 ncdam = 0                                           ! startindex cdam links
 npump = 0                                           ! startindex pump links
 nbndw  = 0                                          ! startindex wave energy bnds

 nqbnd   = 0                                         ! nr of q sections   or specified q bnd's
 nqhbnd  = 0                                         ! nr of qh boundary sections or specified qh bnd's
 ngatesg = 0                                         ! nr of gate signals or specified gates ! not in loop below because flow links not ready yet
 ncdamsg = 0                                         ! nr of controllable dam signals
 npumpsg = 0                                         ! nr of pump signals
 nshiptxy = 0                                        ! nr of ship xyt signals
 nwbnd    = 0                                        ! nr of wave-energy boundaries


 num_bc_ini_blocks = 0
 if (ext_force_bnd_used) then
    ! first read the bc file (new file format for boundary conditions)
    call readlocationfilesfromboundaryblocks(trim(md_extfile_new), nx, kce, num_bc_ini_blocks, &
                                         numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf)
 endif

 do while (ja_ext_force .eq. 1)                      ! read *.ext file

    call readprovider(mext,qid,filename,filetype,method,operand,transformcoef,ja_ext_force,varname)
    call resolvePath(filename, md_extfile_dir)

    if (num_bc_ini_blocks > 0 .and. qid(len_trim(qid)-2:len_trim(qid)) == 'bnd') then
       write(msgbuf, '(a)') 'Boundaries in BOTH external forcing and bound.ext.force file is not allowed'
       call msg_flush()
       call qnerror( 'Boundaries in two files: ', trim(md_extfile_new), ' and ' // trim(md_extfile) )
        ja_ext_force = 0
    endif

    if (ja_ext_force == 1) then

        jatimespace = 1                              ! module is to be used

        call processexternalboundarypoints(qid, filename, filetype, return_time,  nx, kce, numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf, 1d0, transformcoef)

    endif

 enddo

 deallocate(kce)
 deallocate(ke)

 if (mext /= 0) then
    rewind (mext)                                      ! prepare input file
 end if
 numbnp = nbndz + nbndu + nbnd1d2d                             ! nr of boundary points =

end subroutine findexternalboundarypoints



subroutine readlocationfilesfromboundaryblocks(filename, nx, kce, num_bc_ini_blocks, &
                                                numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf)
 use properties
 use timespace
 use tree_data_types
 use tree_structures
 use messageHandling
 use m_flowgeom, only: rrtol
 use m_flowexternalforcings, only: transformcoef
 use system_utils
 use unstruc_files, only: resolvePath
 use m_alloc
 use string_module, only: strcmpi
 use unstruc_model, only: ExtfileNewMajorVersion, ExtfileNewMinorVersion
 use m_missing, only: dmiss

 implicit none

 character(len=*)      , intent(in)    :: filename
 integer               , intent(in)    :: nx
 integer, dimension(nx), intent(inout) :: kce
 integer               , intent(out)   :: num_bc_ini_blocks
 integer               , intent(inout) :: numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf

 type(tree_data), pointer     :: bnd_ptr             !< tree of extForceBnd-file's [boundary] blocks
 type(tree_data), pointer     :: node_ptr            !
 integer                      :: filetype            !< possible values POLY_TIM: use polygon file as location reference, or NODE_ID: use nodeId as a location reference
 integer                      :: istat               !
 integer, parameter           :: ini_key_len   = 32  !
 integer, parameter           :: ini_value_len = 256 !
 character(len=ini_key_len)   :: groupname           !
 character(len=ini_value_len) :: quantity            !
 character(len=ini_value_len) :: locationfile        !< contains either the name of the polygon file (.pli) or the nodeId
 character(len=ini_value_len) :: forcingfile         !
 double precision             :: return_time         !
 double precision             :: tr_ws               ! Tracer fall velocity
 double precision             :: tr_decay_time       ! Tracer decay time
 double precision             :: rrtolb              ! Local, optional boundary tolerance value.
 double precision             :: width1D             ! Local, optional custom 1D boundary width
 double precision             :: blDepth             ! Local, optional custom boundary bed level depth below initial water level

 integer                      :: i                   !
 integer                      :: num_items_in_file   !
 logical                      :: file_ok             !
 logical                      :: group_ok            !
 logical                      :: property_ok         !
 character(len=256)           :: basedir, fnam
 integer                      :: major, minor

 call tree_create(trim(filename), bnd_ptr)
 call prop_file('ini',trim(filename),bnd_ptr,istat)
 if (istat /= 0) then
     call qnerror( 'Boundary external forcing file ', trim(filename), ' could not be read' )
     return
 end if

 ! check FileVersion
 major = 1
 minor = 0
 call prop_get_version_number(bnd_ptr, major = major, minor = minor, success = file_ok)
 if ((major /= ExtfileNewMajorVersion .and. major /= 1) .or. minor > ExtfileNewMinorVersion) then
    write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported format of new external forcing file detected in '''//trim(filename)//''': v', major, minor, '. Current format: v',ExtfileNewMajorVersion,ExtfileNewMinorVersion,'. Ignoring this file.'
    call err_flush()
    return
 end if

 call split_filename(filename, basedir, fnam) ! Remember base dir of input file, to resolve all refenced files below w.r.t. that base dir.

 num_items_in_file = 0
 if (associated(bnd_ptr%child_nodes)) then
     num_items_in_file = size(bnd_ptr%child_nodes)
 endif

 file_ok = .true.
 do i=1,num_items_in_file
    node_ptr => bnd_ptr%child_nodes(i)%node_ptr
    groupname = tree_get_name(bnd_ptr%child_nodes(i)%node_ptr)
    if (strcmpi(groupname, 'Boundary')) then
       quantity = ''
       locationfile = ''
       forcingfile = ''
       return_time = 0.0

       group_ok = .true.

       ! todo: read multiple quantities
       call prop_get_string(node_ptr, '', 'quantity', quantity, property_ok)
       if (.not. property_ok) then
          call qnerror( 'Expected property' , 'quantity', ' for boundary definition' )
       end if

       group_ok = group_ok .and. property_ok

       call prop_get_string(node_ptr, '', 'nodeId', locationfile, property_ok)
       if (property_ok)  then
          filetype = node_id
       else
          call prop_get_string(node_ptr, '', 'locationFile', locationfile, property_ok)
          filetype = poly_tim
       endif

       if (property_ok)  then
          call resolvePath(locationfile, basedir)
       else
          call qnerror( 'Expected property' , 'locationFile', ' for boundary definition' )
       end if

       group_ok = group_ok .and. property_ok

       call prop_get_string(node_ptr, '', 'forcingFile ', forcingfile , property_ok)
       if (property_ok)  then
          call resolvePath(forcingfile, basedir)
       else
          call qnerror( 'Expected property' , 'forcingFile', ' for boundary definition' )
       end if

       group_ok = group_ok .and. property_ok

       call prop_get_double(node_ptr, '', 'returnTime', return_time )
       call prop_get_double(node_ptr, '', 'return_time', return_time ) ! UNST-2386: Backwards compatibility reading.

       tr_ws = 0d0
       call prop_get_double(node_ptr, '', 'tracerFallVelocity', tr_ws)
       transformcoef(4) = tr_ws

       tr_decay_time = 0d0
       call prop_get_double(node_ptr, '', 'tracerDecayTime', tr_decay_time)
       transformcoef(5) = tr_decay_time

       rrtolb = 0d0
       call prop_get_double(node_ptr, '', 'openBoundaryTolerance', rrtolb)

       width1D = dmiss
       call prop_get_double(node_ptr, '', 'bndWidth1D', width1D)

       blDepth = dmiss
       call prop_get_double(node_ptr, '', 'bndBlDepth', blDepth)

       if (group_ok) then
          if (rrtolb > 0d0) then
             call processexternalboundarypoints(quantity, locationfile, filetype, return_time, nx, kce, numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf, rrtolrel = (1+2*rrtolb)/(1+2*rrtol), tfc = transformcoef, width1D = width1D, blDepth = blDepth)
          else
             call processexternalboundarypoints(quantity, locationfile, filetype, return_time, nx, kce, numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf, rrtolrel = 1d0, tfc = transformcoef, width1D = width1D, blDepth = blDepth)
          end if
          num_bc_ini_blocks = num_bc_ini_blocks + 1
       endif

       file_ok = file_ok .and. group_ok

    else
       ! warning: unknown group
    endif

 end do

 call tree_destroy(bnd_ptr)

end subroutine readlocationfilesfromboundaryblocks

subroutine appendrettime(qidfm, nbnd, rettime)

 use m_flowexternalforcings
 use m_alloc

 implicit none

 character(len=256), intent(in)  :: qidfm ! constituent index
 integer, intent(in)             :: nbnd    ! boundary cell index
 double precision, intent(in)    :: rettime ! return time (h)
 integer                         :: thrtlen ! temp array length

 if (allocated(thrtt)) then
    thrtlen = size(thrtt) + 1
 else
    thrtlen = 1
 endif

 call realloc(thrtq, thrtlen, keepExisting=.true., fill='')
 thrtq(thrtlen) = qidfm

 call realloc(thrtn, thrtlen, keepExisting=.true., fill=0)
 thrtn(thrtlen) = nbnd

 call realloc(thrtt, thrtlen, keepExisting=.true., fill=0d0)
 thrtt(thrtlen) = rettime
end subroutine appendrettime


!> helper routine finding external boundary points, called for both old and new-type ext file.
!! Also used for some none-boundary quantities that also need counting total nr of elements, *prior* to flow_initexternalforcings.
!! Two stages: 1 = collect elsets for which data is provided         <-- findexternalboundarypoints + processexternalboundarypoints
!!             2 = add relations between elsets and their providers  <-- flow_initexternalforcings
!! This routine is based upon the network admin only, not on the flow admin.
subroutine processexternalboundarypoints(qid, filename, filetype, return_time, nx, kce, &
                                         numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, &
                                         numqh, numw, numtr, numsf, rrtolrel, tfc, &
                                         width1D, blDepth) ! helper for finding external boundary points
 use m_netw
 use m_flow, qid_flow => qid, filetype_flow => filetype
 use m_flowgeom
 use unstruc_model
 use timespace
 use m_sferic
 use m_alloc
 use unstruc_messages
 use m_ship
 use properties
 use m_transport
 use m_meteo, qid_meteo => qid, filetype_meteo => filetype
 use m_sobekdfm
 use m_flowparameters, only: jawave
 use string_module
 use m_strucs, only: NUMGENERALKEYWRD
 use m_missing, only: dmiss

 implicit none

 character(len=256)    , intent(in)    :: qid                                 !
 character(len=256)    , intent(in)    :: filename                            !
 integer               , intent(in)    :: filetype
 integer               , intent(in)    :: nx                                  !
 integer, dimension(nx), intent(inout) :: kce                                 !
 double precision      , intent(in)    :: return_time
 integer               , intent(inout) :: numz, numu, nums, numtm, numsd, &   !
                                          numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf      !
 double precision      , intent(in)    :: rrtolrel !< To enable a more strict rrtolerance value than the global rrtol. Measured w.r.t. global rrtol.

 double precision, dimension(NUMGENERALKEYWRD), optional, intent(in) :: tfc
 double precision, optional, intent(in) :: width1D !< Optional custom width for boundary flow link.
 double precision, optional, intent(in) :: blDepth !< Optional custom bed level depths below water level boundaries's initial value for boundary points.

 character(len=256)                    :: qidfm                               !
 integer                               :: itpbn
 character (len=NAMTRACLEN)            :: tracnam, sfnam, qidnam
 character(len=20)                     :: tracunit
 integer                               :: itrac, isf
 integer, external                     :: findname
 integer                               :: janew
 character(len=:),allocatable          :: pliname

! call bndname_to_fm(qid,qidfm)
  qidfm = qid
  if (qidfm == 'waterlevelbnd'    .or. qidfm == 'neumannbnd'  .or. qidfm == 'riemannbnd' .or. qidfm == 'outflowbnd' .or. qidfm == 'qhbnd') then

     if (allocated(pliname)) deallocate(pliname)
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, kez(nbndz+1:nx), numz, usemask=.true., pliname=pliname) !numz=number cells found, plname=pliname
     write(msgbuf,'(a,x,a,i8,a)') trim (qid), trim( filename), numz, ' nr of open bndcells' ; call msg_flush()
     nzbnd = nzbnd + 1

     if (qidfm == 'waterlevelbnd')  itpbn = 1
     if (qidfm == 'neumannbnd'   )  itpbn = 2
     if (qidfm == 'riemannbnd'  )   then
        itpbn = 5
        if (present(tfc)) then
           ftpet(nbndz+1:nbndz+numz) = tfc(7)    ! relaxation time riemann from ext file
        end if
     end if
     if (qidfm == 'outflowbnd'   )  itpbn = 6

     if (qidfm == 'qhbnd') then
         itpbn = 7
         nqhbnd = nqhbnd + 1
         numqh  = numz
         if (filetype == poly_tim) then
         call realloc(qhpliname,nqhbnd)  ; qhpliname(nqhbnd) = pliname
         end if

         call realloc(L1qhbnd,nqhbnd) ; L1qhbnd(nqhbnd) = nbndz + 1
         call realloc(L2qhbnd,nqhbnd) ; L2qhbnd(nqhbnd) = nbndz + numz
         call realloc(atqh_all,nqhbnd); atqh_all(nqhbnd) = 0d0
         call realloc(atqh_sum,nqhbnd); atqh_sum(nqhbnd) = 0d0
         call realloc(qhbndz,nqhbnd)  ; qhbndz(nqhbnd)   = 0d0
         call realloc(qh_gamma, nqhbnd)
         qh_gamma = 0d0
         call realloc(qhbndz_min, nqhbnd)
         qhbndz_min = 0d0
         call realloc(qhbndz_plus, nqhbnd)
         qhbndz_plus = 0d0
         call realloc(q_org, nqhbnd)
         q_org = 0d0
     end if
     itpez(nbndz+1:nbndz+numz) =  itpbn

     call addopenbndsection(numz, kez(nbndz+1:nbndz+numz), filename, IBNDTP_ZETA)

     ! When present, set custom geometry for open boundaries (bed level for bndz and/or width1D for 1D bndz/u).
     ! Only for z:
     if (present(blDepth)) then
        call realloc(bndBlDepth, size(openbndtype), fill = dmiss)
        bndBlDepth(nopenbndsect) = blDepth
     end if
     ! For z and u:
     if (present(width1D)) then
        call realloc(bndWidth1D, size(openbndtype), fill = dmiss)
        bndWidth1D(nopenbndsect) = width1D
     end if

     itpenz(nbndz+1:nbndz+numz) = nopenbndsect
     nbndz = nbndz + numz

  else if (qidfm == 'velocitybnd' .or. qidfm == 'dischargebnd' .or. qidfm == 'qhubnd'.or. &
           qidfm == 'criticaloutflowbnd' .or. qidfm == 'weiroutflowbnd' .or. qidfm == 'absgenbnd') then
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, keu(nbndu+1:nx), numu, usemask=.true., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim (qid), trim( filename), numu, ' nr of open bndcells' ; call msg_flush()
     nubnd = nubnd + 1

     if (qidfm == 'velocitybnd' ) then
        itpbn = 3
     else if (qidfm == 'dischargebnd') then
        itpbn = 4
        nqbnd = nqbnd + 1
        call realloc(L1qbnd,nqbnd) ; L1qbnd(nqbnd) = nbndu + 1
        call realloc(L2qbnd,nqbnd) ; L2qbnd(nqbnd) = nbndu + numu
        call realloc(at_all,nqbnd);  at_all(nqbnd) = 0d0
        call realloc(at_sum,nqbnd);  at_sum(nqbnd) = 0d0
        call realloc(wwssav_all,(/2,nqbnd/), keepExisting=.true., fill=0d0)
        call realloc(wwssav_sum,(/2,nqbnd/), keepExisting=.true., fill=0d0)
        call realloc(huqbnd,L2qbnd(nqbnd)); huqbnd(L1qbnd(nqbnd):L2qbnd(nqbnd)) = 0d0
     else if ( qidfm == 'absgenbnd') then
        if (.not. (jawave.eq.4)) then                 ! Safety to avoid allocation errors later on
           call qnerror( 'Absorbing-generating boundary defined without activating surfbeat model. Please use appropriate wave model, or change the boundary condition type.', '  ', ' ')
           write(msgbuf, '(a)') 'Absorbing-generating boundary defined without activating surfbeat model. Please use appropriate wave model, or change the boundary condition type.'
           call err_flush()
        end if
        itpbn = 5
     else if ( qidfm == 'qhubnd') then
        itpbn = 6
     else if ( qidfm == 'criticaloutflowbnd') then
        itpbn = 8
     else if ( qidfm == 'weiroutflowbnd') then
        itpbn = 9
     endif

     itpeu(nbndu+1:nbndu+numu) = itpbn

     call addopenbndsection(numu, keu(nbndu+1:nbndu+numu), filename, IBNDTP_U)

     ! When present, set custom geometry for open boundaries (width1D for 1D bndz/u).
     ! For z and u:
     if (present(width1D)) then
        call realloc(bndWidth1D, size(openbndtype), fill = dmiss)
        bndWidth1D(nopenbndsect) = width1D
     end if

     itpenu(nbndu+1:nbndu+numu) = nopenbndsect
     nbndu = nbndu + numu

  else if (qidfm == 'salinitybnd' .and. jasal>0 ) then
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, kes(nbnds+1:nx), nums, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename), nums, ' nr of salinity bndcells' ; call msg_flush()
     if (nums>0) then
        call appendrettime(qidfm, nbnds + 1, return_time)
        nbnds = nbnds + nums
     end if

  else if (qidfm == 'waveenergybnd' ) then
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, kew(nbndw+1:nx), numw, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename), numw, ' nr of wave energy bndcells' ; call msg_flush()

     nwbnd = nwbnd + 1
     
     call realloc(L1wbnd,nwbnd) ; L1wbnd(nwbnd) = nbndw + 1
     call realloc(L2wbnd,nwbnd) ; L2wbnd(nwbnd) = nbndw + numw
     
     nbndw = nbndw + numw
     call realloc(fnamwbnd,nwbnd,fill='')
     fnamwbnd(nwbnd) = trim(filename)
     
  else if (qidfm == 'temperaturebnd' .and. jatem > 0 ) then
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, ketm(nbndtm+1:nx), numtm, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename), numtm, ' nr of temperature bndcells' ; call msg_flush()
     if (numtm>0) then
        call appendrettime(qidfm, nbndtm + 1, return_time)
        nbndtm = nbndtm + numtm
     end if

  else if (qidfm == 'sedimentbnd' ) then
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, kesd(nbndsd+1:nx), numsd, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename), numsd, ' nr of sediment bndcells' ; call msg_flush()
     if (numsd>0) then
        call appendrettime(qidfm, nbndsd + 1, return_time)
        nbndsd = nbndsd + numsd
     end if

  else if (qidfm(1:9) == 'tracerbnd' ) then
     call get_tracername(qidfm, tracnam, qidnam)
     tracunit = " "
     call add_bndtracer(tracnam, tracunit, itrac, janew)

     if ( janew.eq.1 ) then
!       realloc ketr
        call realloc(ketr, (/ Nx, numtracers /), keepExisting=.true., fill=0 )
     end if
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, ketr(nbndtr(itrac)+1:,itrac), numtr, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename) , numtr, ' nr of tracer bndcells' ; call msg_flush()
     if (numtr>0) then
        call appendrettime(qidfm, nbndtr(itrac) + 1, return_time)
        nbndtr(itrac) = nbndtr(itrac) + numtr
        nbndtr_all = maxval(nbndtr(1:numtracers))
     end if

  else if (qid(1:13) == 'initialtracer' ) then
     call get_tracername(qid, tracnam, qidnam)
     tracunit = " "
     call add_bndtracer(tracnam, tracunit, itrac, janew)

     if ( janew.eq.1 ) then
!       realloc ketr
        call realloc(ketr, (/ Nx, numtracers /), keepExisting=.true., fill=0 )
     end if

  else if (qidfm(1:10) == 'sedfracbnd' .and. stm_included) then
     call get_sedfracname(qidfm, sfnam, qidnam)
     isf = findname(numfracs, sfnames, sfnam)

     if ( isf.eq.0) then   ! add

        numfracs = numfracs+1
!       realloc
        call realloc(kesf, (/Nx, numfracs/), keepExisting=.true., fill=0 )
        call realloc(nbndsf, numfracs, keepExisting=.true., fill=0 )
        call realloc(sfnames, numfracs, keepExisting=.true., fill='')

        sfnames(numfracs) = trim(sfnam)
        isf = numfracs

     end if

     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, kesf(nbndsf(isf)+1:,isf), numsf, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(3a,i8,a)') trim(qid), ' ', trim(filename) , numsf, ' nr of sedfrac bndcells' ; call msg_flush()
     if (numsf > 0) then
        call appendrettime(qidfm, nbndsf(isf) + 1, return_time)
        nbndsf(isf) = nbndsf(isf) + numsf
        nbndsf_all = maxval(nbndsf(1:numfracs))
     endif

  else if (qidfm == 'tangentialvelocitybnd' ) then
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, ket(nbndt+1:nx), numt, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename) , numt, ' nr of tangentialvelocity bndcells' ; call msg_flush()

     nbndt = nbndt + numt

  else if (qidfm == 'uxuyadvectionvelocitybnd' ) then
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, keuxy(nbnduxy+1:nx), numuxy, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename) , numuxy, ' nr of uxuyadvectionvelocity bndcells' ; call msg_flush()

     nbnduxy = nbnduxy + numuxy

  else if (qidfm == 'normalvelocitybnd' ) then
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, ken(nbndn+1:nx), numn, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename) , numn, ' nr of normalvelocity bndcells' ; call msg_flush()

     nbndn = nbndn + numn

  else if (qidfm == '1d2dbnd' ) then ! SOBEK1D-FM2D
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, ke1d2d(nbnd1d2d+1:nx), num1d2d, usemask=.true., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename) , num1d2d, ' nr of SOBEK1D-FM2D bndcells' ; call msg_flush()

     call addopenbndsection(num1d2d, ke1d2d(nbnd1d2d+1:nbnd1d2d+num1d2d), filename, IBNDTP_1D2D)
     nbnd1d2d = nbnd1d2d + num1d2d

  else if (qidfm == 'shiptxy' ) then

     nshiptxy = nshiptxy + 1

  else if (qidfm == 'nudgetime' .or. qidfm == 'nudgerate' .or. qidfm == 'nudge_salinity_temperature' ) then

     janudge = 1

 endif

 end subroutine processexternalboundarypoints


!> Calls the ec_addtimespacerelation with all proper unstruc-specific target arrays and element set masks.
function addtimespacerelation_boundaries(qid, filename, filetype, method, operand, forcingfile, targetindex) result(success)
   use m_flowexternalforcings, no1=>qid, no2=>filetype, no3=>operand, no4 => success
   use m_meteo, no5=>qid, no6=>filetype, no7=>operand, no8 => success
   use m_flowparameters, only: jawave
   use m_flowtimes, only: dt_nodal

   implicit none

   character(len=*),            intent(inout) :: qid         !< Identifier of current quantity (i.e., 'waterlevelbnd')
   character(len=*),            intent(in)    :: filename    !< Name of data file for current quantity.
   integer,                     intent(in)    :: filetype    !< File type of current quantity.
   integer,                     intent(in)    :: method      !< Time-interpolation method for current quantity.
   character(len=1),            intent(in)    :: operand     !< Operand w.r.t. previous data ('O'verride or '+'Append)
   character(len=*),  optional, intent(in)    :: forcingfile !< Optional forcings file, if it differs from the filename (i.e., if filename=*.pli, and forcingfile=*.bc)
   integer,           optional, intent(in)    :: targetIndex !< target position or rank of (complete!) vector in target array

   logical                       :: success
   character(len=256)            :: tracnam, sfnam, qidnam
   integer                       :: itrac, isf
   integer, external             :: findname
   double precision, dimension(:), pointer     :: pzmin, pzmax

   success = .true.   ! initialization

   ! Special forcingsfile: if name equals 'REALTIME', do not do an ec_addtimespacerelation, just leave it to the external caller to fill zbnd* target value array.
   ! TODO: AVD: we now leave it to caller to fill array with length(zbnd*),
   ! instead of the number of polyline points. Cleaner alternative is to create
   ! a poly_tim provider, with the *underlying* point child providers being REALTIME.
   if (present(forcingfile)) then
      if (trim(forcingfile) == 'REALTIME') then
         call mess(LEVEL_DEBUG, 'addtimespacerelation_boundaries: leave empty timespacerelation for '''//trim(qid)//''' from locationfile '''//trim(filename)//''' (REALTIME data).')
         return
      end if
   end if

   kx = 1
   if (nbndz > 0 .and. (qid == 'waterlevelbnd' .or. qid == 'neumannbnd' .or. qid == 'riemannbnd' .or. qid == 'outflowbnd')) then
      success = ec_addtimespacerelation(qid, xbndz, ybndz, kdz, kx, filename, filetype, method, operand, xy2bndz, forcingfile=forcingfile, dtnodal=dt_nodal, targetindex=targetindex)

   else if (nbndz > 0 .and. nqhbnd > 0 .and. (qid == 'qhbnd')) then
      success = ec_addtimespacerelation(qid, xbndz, ybndz, kdz, kx, filename, filetype, method, operand, xy2bndz, forcingfile=forcingfile, targetindex=targetindex)

   else if (nbndu > 0 .and. (qid == 'dischargebnd' .or. qid == 'criticaloutflowbnd' .or. qid == 'weiroutflowbnd' .or. qid == 'absgenbnd' ) ) then
      if ( qid.eq.'absgenbnd' ) then
         jawave = 4
      end if
      success = ec_addtimespacerelation(qid, xbndu, ybndu, kdu, kx, filename, filetype, method, operand, xy2bndu, forcingfile=forcingfile, targetindex=targetindex)

   else if (nbndu > 0 .and. qid == 'velocitybnd' ) then
      pzmin => zminmaxu(1:nbndu)
      pzmax => zminmaxu(nbndu+1:2*nbndu)
      success = ec_addtimespacerelation(qid, xbndu, ybndu, kdu, kx, filename, filetype, method, operand,   &
                                           xy2bndu, z=sigmabndu, pzmin=pzmin, pzmax=pzmax, forcingfile=forcingfile, targetindex=targetindex)

   else if (nbnds > 0 .and. qid == 'salinitybnd' ) then ! 2D
      pzmin => zminmaxs(1:nbnds)
      pzmax => zminmaxs(nbnds+1:2*nbnds)
      success = ec_addtimespacerelation(qid, xbnds, ybnds, kds, kx, filename, filetype, method, operand, xy2bnds,    &
                                           z=sigmabnds, pzmin=pzmin, pzmax=pzmax, forcingfile=forcingfile, targetindex=targetindex)

   else if (nbndTM > 0 .and. qid == 'temperaturebnd') then
      pzmin => zminmaxtm(1:nbndTM)
      pzmax => zminmaxtm(nbndTM+1:2*nbndTM)
      success = ec_addtimespacerelation(qid, xbndTM, ybndTM, kdtm, kx, filename, filetype, method, operand, xy2bndtm,   &
                                           z=sigmabndtm, pzmin=pzmin, pzmax=pzmax, forcingfile=forcingfile, targetindex=targetindex)

   else if (nbndsd > 0 .and. (qid == 'sedimentbnd')) then
      pzmin => zminmaxsd(1:nbndsd)
      pzmax => zminmaxsd(nbndsd+1:2*nbndsd)
      success = ec_addtimespacerelation(qid, xbndsd, ybndsd, kdsd, kx, filename, filetype, method, operand, xy2bndsd,   &
                                           z=sigmabndsd, pzmin=pzmin, pzmax=pzmax, forcingfile=forcingfile, targetindex=targetindex)

   else if ( numtracers > 0 .and. (qid(1:9) == 'tracerbnd') ) then
      ! get tracer boundary condition number
      call get_tracername(qid, tracnam, qidnam)
      itrac = findname(numtracers, trnames, tracnam)

! for parallel runs, we always need to add the tracer, even if this subdomain has no tracer boundary conditions defined
!      call add_tracer(tracnam, iconst)
!      update: all tracers are counted first and allocated later

      if ( nbndtr(itrac).gt.0 ) then
         pzmin => bndtr(itrac)%zminmax(1:nbndtr(itrac))
         pzmax => bndtr(itrac)%zminmax(nbndtr(itrac)+1:2*nbndtr(itrac))
         success = ec_addtimespacerelation(qid, bndtr(itrac)%x, bndtr(itrac)%y, bndtr(itrac)%kd, kx, filename, filetype, method, operand, bndtr(itrac)%xy2,    &
                                           z=bndtr(itrac)%sigma, forcingfile=forcingfile, pzmin=pzmin, pzmax=pzmax, targetindex=targetindex)
      else
         success = .true.
      end if

   else if ( numfracs > 0 .and. (qid(1:10) == 'sedfracbnd') .and. stm_included) then

      call get_sedfracname(qid, sfnam, qidnam)
      isf = findname(numfracs, sfnames, sfnam)

      if (isf > 0) then
         if ( nbndsf(isf).gt.0 ) then
            pzmin => bndsf(isf)%zminmax(1:nbndsf(isf))
            pzmax => bndsf(isf)%zminmax(nbndsf(isf)+1:2*nbndsf(isf))
            success = ec_addtimespacerelation(qid, bndsf(isf)%x, bndsf(isf)%y, bndsf(isf)%kd, kx, filename, filetype, method, operand, bndsf(isf)%xy2,    &
                                              z=bndsf(isf)%sigma, forcingfile=forcingfile, pzmin=pzmin, pzmax=pzmax, targetindex=targetindex)
         else
            success = .true.
         end if
      else
         call mess(LEVEL_WARN, 'Initializing boundary block for file '''//trim(filename)//''', getting unknown sediment fraction '''//trim(sfnam)//''' from QUANTITY '''//trim(qid)//'''.')
         call qnerror('Initializing boundary block for file '''//trim(filename)//''', getting unknown sediment fraction '''//trim(sfnam)//''' from QUANTITY '''//trim(qid)//'''.',' ',' ')
      end if

   else if (nbndt > 0 .and. (qid == 'tangentialvelocitybnd')) then
      success = ec_addtimespacerelation(qid, xbndt, ybndt, kdt, kx, filename, filetype, method, operand, xy2bndt, forcingfile=forcingfile, targetindex=targetindex)

   else if (nbnduxy > 0 .and. (qid == 'uxuyadvectionvelocitybnd')) then
      kx = 2
      pzmin => zminmaxuxy(1:nbnduxy)
      pzmax => zminmaxuxy(nbnduxy+1:2*nbnduxy)
      success = ec_addtimespacerelation(qid, xbnduxy, ybnduxy, kduxy, kx, filename, filetype, method, operand, xy2bnduxy,   &
                                        z=sigmabnduxy, pzmin=pzmin, pzmax=pzmax, forcingfile=forcingfile)

   else if (nbndn > 0 .and. (qid == 'normalvelocitybnd')) then
      success = ec_addtimespacerelation(qid, xbndn, ybndn, kdn, kx, filename, filetype, method, operand, xy2bndn, forcingfile=forcingfile, targetindex=targetindex)

   else !There is some boundary that is not detected or recognized
!      success = .false.
! SPvdP: this is not an error, especially for parallel runs
   end if
end function addtimespacerelation_boundaries

logical function initboundaryblocksforcings(filename)
 use properties
 use tree_data_types
 use tree_structures
 use messageHandling
 use m_flowexternalforcings
 use m_flowgeom
 use timespace_data, only: weightfactors, poly_tim, uniform, spaceandtime, getmeteoerror
 use m_lateral, only : balat, qplat, lat_ids, n1latsg, n2latsg, ILATTP_1D, ILATTP_2D, ILATTP_ALL, kclat, numlatsg, nnlat, nlatnd
 use m_alloc
 use m_meteo, only: ec_addtimespacerelation
 use timespace
 use string_module, only: str_tolower, strcmpi
 use system_utils
 use unstruc_files, only: resolvePath
 use unstruc_model, only: ExtfileNewMajorVersion, ExtfileNewMinorVersion
 use m_missing
 use m_ec_parameters, only: provFile_uniform
 use m_partitioninfo, only: jampi, reduce_sum, is_ghost_node
 use m_lateral, only : apply_transport
 use m_flow, only: kmx

 implicit none

 character(len=*), intent(in) :: filename            !< file name for new external forcing boundary blocks
 type(tree_data), pointer     :: bnd_ptr             !< tree of extForceBnd-file's [boundary] blocks
 type(tree_data), pointer     :: node_ptr            !
 type(tree_data), pointer     :: block_ptr           !
 integer                      :: istat               !
 integer, parameter           :: ini_key_len   = 32  !
 integer, parameter           :: ini_value_len = 256 !
 character(len=ini_key_len)   :: groupname           !
 character(len=ini_value_len) :: property_name
 character(len=ini_value_len) :: property_value
 character(len=ini_value_len) :: quantity
 character(len=ini_value_len) :: locationfile        !
 character(len=ini_value_len) :: forcingfile         !
 character(len=ini_value_len) :: forcingfiletype     !
 character(len=ini_value_len) :: targetmaskfile      !
 integer                      :: i,j                 !
 integer                      :: num_items_in_file   !
 integer                      :: num_items_in_block
 logical                      :: retVal
 logical                      :: invertMask
 character(len=1)             :: oper                !
 character (len=300)          :: rec

 character(len=ini_value_len) :: nodeid
 character(len=ini_value_len) :: branchid

 character(len=ini_value_len) :: locid
 character(len=ini_value_len) :: itemtype
 character(len=256)           :: fnam
 character(len=256)           :: basedir
 double precision             :: chainage
 integer                      :: ierr
 integer                      :: ilattype, nlat
 integer                      :: k, n, k1, nini
 integer                      :: fmmethod
 integer, dimension(1)        :: targetindex
 integer                      :: ib, ibqh, ibt
 integer                      :: maxlatsg
 integer                      :: major, minor
 integer                      :: loc_spec_type
 integer                      :: numcoordinates
 double precision, allocatable :: xcoordinates(:), ycoordinates(:)
 double precision, allocatable :: xdum(:), ydum(:)
 integer, allocatable          :: kdum(:)
 integer, allocatable          :: itpenzr(:), itpenur(:)

 if (allocated(xdum  )) deallocate(xdum, ydum, kdum) !, xy2dum)
 allocate ( xdum(1), ydum(1), kdum(1)) !, xy2dum(2,1) , stat=ierr)
 !call aerr('xdum(1), ydum(1), kdum(1), xy2dum     ', ierr, 3)
 xdum = 1d0 ; ydum = 1d0; kdum = 1!; xy2dum = 0d0

 initboundaryblocksforcings = .true.

 call tree_create(trim(filename), bnd_ptr)
 call prop_file('ini',trim(filename),bnd_ptr,istat)

 ! check FileVersion
 major = 1
 minor = 0
 call prop_get_version_number(bnd_ptr, major = major, minor = minor, success = retVal)
 if ((major /= ExtfileNewMajorVersion .and. major /= 1) .or. minor > ExtfileNewMinorVersion) then
    write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported format of new external forcing file detected in '''//trim(filename)//''': v', major, minor, '. Current format: v',ExtfileNewMajorVersion,ExtfileNewMinorVersion,'. Ignoring this file.'
    call err_flush()
    initboundaryblocksforcings = .false.
    return
 end if

 call init_registered_items()

 call split_filename(filename, basedir, fnam) ! Remember base dir of input file, to resolve all refenced files below w.r.t. that base dir.

 num_items_in_file = tree_num_nodes(bnd_ptr)

 ! Build temporary reverse lookup table that maps boundary block # in file -> boundary condition nr in openbndsect (separate for u and z).
 allocate(itpenzr(num_items_in_file)); itpenzr = 0
 allocate(itpenur(num_items_in_file)); itpenur = 0
 do ibt=1,nbndz
    ib = itpenz(ibt)
    if (ib > 0) then
       itpenzr(ib) = ibt
    end if
 end do
 do ibt=1,nbndu
    ib = itpenu(ibt)
    if (ib > 0) then
       itpenur(ib) = ibt
    end if
 end do

 ! Allocate lateral provider array now, just once, because otherwise realloc's in the loop would destroy target arrays in ecInstance.
 maxlatsg = tree_count_nodes_byname(bnd_ptr, 'lateral')
 if (maxlatsg > 0) then
    call realloc(balat, maxlatsg, keepExisting = .false., fill = 0d0)
    call realloc(qplat, (/max(1,kmx),maxlatsg/), keepExisting = .false., fill = 0d0)
    call realloc(lat_ids, maxlatsg, keepExisting = .false.)
    call realloc(n1latsg, maxlatsg, keepExisting = .false., fill = 0)
    call realloc(n2latsg, maxlatsg, keepExisting = .false., fill = 0)
 end if

 ib = 0
 ibqh = 0
 do i=1,num_items_in_file

    node_ptr => bnd_ptr%child_nodes(i)%node_ptr
    groupname = tree_get_name(node_ptr)
    select case (str_tolower(trim(groupname)))
    case ('general')
       ! General block, was already read.
       cycle

    case ('boundary')

       ! First check for required input:
       call prop_get_string(node_ptr, '', 'quantity', quantity, retVal)
       if (.not. retVal) then
          initboundaryblocksforcings = .false.
          write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''quantity'' is missing.'
          call warn_flush()
          cycle
       end if
       ib = ib + 1

       call prop_get_string(node_ptr, '', 'nodeId', locationfile, retVal)
       if (retVal) then
          filetype = node_id
          fmmethod  = spaceandtime
       else
          filetype = poly_tim
          fmmethod  = weightfactors
          call prop_get_string(node_ptr, '', 'locationfile', locationfile, retVal)
       endif

       if (retVal) then
          call resolvePath(locationfile, basedir)
       else
          initboundaryblocksforcings = .false.
          write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''locationfile'' is missing.'
          call warn_flush()
          cycle
       end if

       call prop_get_string(node_ptr, '', 'forcingFile ', forcingfile , retVal)
       if (retVal) then
          call resolvePath(forcingfile, basedir)
       else
          initboundaryblocksforcings = .false.
          write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''forcingFile'' is missing.'
          call warn_flush()
          cycle
       end if

       oper = '-'
       call prop_get_string(node_ptr, '', 'operand ', oper , retVal)

       num_items_in_block = 0
       if (associated(node_ptr%child_nodes)) then
           num_items_in_block = size(node_ptr%child_nodes)
       endif

       ! Now loop over all key-value pairs, to support reading *multiple* lines with forcingfile=...
       do j=1,num_items_in_block
          block_ptr => node_ptr%child_nodes(j)%node_ptr
          ! todo: read multiple quantities
          property_name = tree_get_name(block_ptr)
          call tree_get_data_string(block_ptr, property_value, retVal)
          if (retVal) then
             if (property_name == 'quantity') then
                quantity = property_value ! We already knew this
             else if (property_name == 'locationfile') then
                locationfile = property_value ! We already knew this
                call resolvePath(locationfile, basedir)
             else if (property_name == 'forcingfile') then
                forcingfile = property_value
                call resolvePath(forcingfile, basedir)
                if ( oper /= 'O' .and. oper /= '+' ) then
	               oper = 'O'
                   if (quantity_pli_combination_is_registered(quantity, locationfile)) then
                      oper = '+'
                   endif
		        end if 
                call register_quantity_pli_combination(quantity, locationfile)
                if (filetype == node_id .or. quantity == 'qhbnd') then
                   select case(quantity)
                   case ('waterlevelbnd')
                     targetIndex = itpenzr(ib)
                   case ('qhbnd')
                      ibqh = ibqh + 1
                      targetindex = (/ibqh/)
                      if (filetype/=node_id) then
                          locationfile = qhpliname(ibqh)
                      end if
                   case ('dischargebnd')
                      targetIndex = itpenur(ib)
                   case default
                      targetindex = (/-1/)
                   end select

                   if (targetindex(1) <= 0) then
                      ! This boundary has been skipped in an earlier phase (findexternalboundarypoints),
                      ! so, also do *not* connect it as a spacetimerelation here.
                      retVal = .true. ! No failure: boundaries are allowed to remain disconnected.
                   else if (forcingfile == '-') then
                      retVal = addtimespacerelation_boundaries(quantity, locationfile, filetype=node_id, method=fmmethod, &
                             operand=oper, targetindex=targetindex(1))
                   else
                      retVal = addtimespacerelation_boundaries(quantity, locationfile, filetype=node_id, method=fmmethod, &
                             operand=oper, forcingfile = forcingfile, targetindex=targetindex(1))
                   endif
                else
                   if (forcingfile == '-') then
                      retVal = addtimespacerelation_boundaries(quantity, locationfile, filetype=filetype, method=fmmethod, &
                             operand=oper)
                   else
                      retVal = addtimespacerelation_boundaries(quantity, locationfile, filetype=filetype, method=fmmethod, &
                             operand=oper, forcingfile = forcingfile)
                   endif
                endif
                initboundaryblocksforcings = initboundaryblocksforcings .and. retVal ! Remember any previous errors.
                oper = '-'
             else if (property_name == 'operand') then
                continue
             else if (property_name == 'returntime' .or. property_name == 'return_time') then
                continue                   ! used elsewhere to set Thatcher-Harleman delay
             else if (property_name == 'openboundarytolerance') then
                continue                   ! used in findexternalboundarypoints/readlocationfiles... to set search distance. Not relevant here.
             else if (property_name == 'nodeid') then
                continue
             else if (property_name == 'bndwidth1d') then
                continue
             else if (property_name == 'bndbldepth') then
                continue
             else
                ! initboundaryblocksforcings remains unchanged: support ignored lines in ext file.
                write(msgbuf, '(9a)') 'Unrecognized line in file ''', trim(filename), ''' for block [', trim(groupname), ']: ', trim(property_name), ' = ', trim(property_value), '. Ignoring this line.'
                call warn_flush()
                cycle
             endif
          endif
       enddo	      
       if (.not. retVal) then ! This addtimespace was not successful
          rec = getmeteoerror()
          if (len_trim(rec)>0) then
             call mess(LEVEL_WARN, trim(rec))
          endif
          call mess(LEVEL_WARN, 'initboundaryblockforcings: Error while initializing quantity '''//trim(quantity)//'''. Check preceding log lines for details.')
       end if
       
    case ('lateral')
       ! [Lateral]
       ! Id = ...
       locid = ' '
       call prop_get(node_ptr, '', 'Id', locid, success)
       if (.not. success .or. len_trim(locid) == 0) then
          write(msgbuf, '(a,i0,a)') 'Required field ''Id'' missing in lateral (block #', i, ').'
          call warn_flush()
          cycle
       end if

       ! locationType = optional for lateral
       ! fileVersion >= 2: locationType = 1d | 2d | all
       ! fileVersion <= 1: Type         = 1d | 2d | 1d2d
       itemtype = ' '
       if (major >= 2) then
          call prop_get(node_ptr, '', 'locationType', itemtype, success)
       else
          call prop_get(node_ptr, '', 'Type',         itemtype, success)
       end if
       select case (str_tolower(trim(itemtype)))
       case ('1d')
          ilattype = ILATTP_1D
       case ('2d')
          ilattype = ILATTP_2D
       case ('1d2d', 'all')
          ilattype = ILATTP_ALL
       case default
          ilattype = ILATTP_ALL
       end select
       
       call reserve_sufficient_space(apply_transport, numlatsg+1, 0)
       call prop_get(node_ptr, '', 'applyTransport', apply_transport(numlatsg+1), success)

       ! [lateral]
       ! fileVersion >= 2: nodeId                  => location_specifier = LOCTP_NODEID
       !                   branchId+chainage       => location_specifier = LOCTP_BRANCH_CHAINAGE
       !                   numcoor+xcoors+ycoors   => location_specifier = LOCTP_XY_POLYGON
       ! fileVersion <= 1: LocationFile = test.pol => location_specifier = LOCTP_POLYGON_FILE
       loc_spec_type      = imiss
       nodeId             = ' '
       branchid           = ' '
       chainage           = dmiss
       numcoordinates     = imiss
       !
       if (major >= 2) then
          call prop_get_string(node_ptr, '', 'nodeId', nodeId, success)
          if (success) then
             loc_spec_type = LOCTP_NODEID
             ilattype = ILATTP_1D
          else
             call prop_get(node_ptr, '', 'branchId',         branchid, success)
             if (success) then
                call prop_get(node_ptr, '', 'chainage',         chainage, success)
             end if
             if (success) then
                if (len_trim(branchid)>0 .and. chainage /= dmiss .and. chainage >= 0.0d0) then
                   loc_spec_type = LOCTP_BRANCHID_CHAINAGE
                   ilattype = ILATTP_1D
                end if
             else
                call prop_get(node_ptr, '', 'numCoordinates',   numcoordinates, success)
                if (success .and. numcoordinates > 0) then
                   allocate(xcoordinates(numcoordinates), stat=ierr)
                   allocate(ycoordinates(numcoordinates), stat=ierr)
                   call prop_get_doubles(node_ptr, '', 'xCoordinates',     xcoordinates, numcoordinates, success)
                   call prop_get_doubles(node_ptr, '', 'yCoordinates',     ycoordinates, numcoordinates, success)
                   if (success) then
                      loc_spec_type = LOCTP_POLYGON_XY
                   end if
                end if
             end if
          end if
       else ! fileVersion <= 1
          loc_spec_type = LOCTP_POLYGON_FILE
          !
          locationfile = ''
          call prop_get(node_ptr, '', 'LocationFile', locationfile, success)
          if (.not. success .or. len_trim(locationfile) == 0) then
             write(msgbuf, '(a,a,a)') 'Required field ''LocationFile'' missing in lateral ''', trim(locid), '''.'
             call warn_flush()
             cycle
          else
             call resolvePath(locationfile, basedir)
          end if
       end if
       if (loc_spec_type == imiss) then
          write(msgbuf, '(a,a,a)') 'Unrecognized location specification in lateral ''', trim(locid), '''.'
          call warn_flush()
          cycle
       end if

       call ini_alloc_laterals()

       call prepare_lateral_mask(kclat, ilattype)

       numlatsg = numlatsg + 1
       call realloc(nnlat, max(2*ndxi, nlatnd+ndxi), keepExisting = .true., fill = 0)
       call selectelset_internal_nodes(xz, yz, kclat, ndxi, nnLat(nlatnd+1:), nlat, &
                                       loc_spec_type, locationfile, numcoordinates, xcoordinates, ycoordinates, branchid, chainage, nodeId)

       n1latsg(numlatsg) = nlatnd + 1
       n2latsg(numlatsg) = nlatnd + nlat

       nlatnd = nlatnd + nlat

       if (allocated(xcoordinates)) deallocate(xcoordinates, stat=ierr)
       if (allocated(ycoordinates)) deallocate(ycoordinates, stat=ierr)

       ! [lateral]
       ! Flow = 1.23 | test.tim | REALTIME
       kx = 1
       rec = ' '
       call prop_get(node_ptr, '', 'discharge', rec, success)
       if (.not. success .and. major <= 1) then ! Old pre-2.00 keyword 'flow'
          call prop_get(node_ptr, '', 'flow', rec, success)
       end if
       if (len_trim(rec) > 0) then
          call resolvePath(rec, basedir)
       else
          write(msgbuf, '(a,a,a)') 'Required field ''discharge'' missing in lateral ''', trim(locid), '''.'
          call warn_flush()
          cycle
       end if

       qid = 'lateral_discharge' ! New quantity name in .bc files
       success = adduniformtimerelation_objects(qid, '', 'lateral', trim(locid), 'discharge', trim(rec), numlatsg, kx, qplat(1,:))
       if (success) then
          jaqin = 1
          lat_ids(numlatsg) = locid
       end if

    case ('meteo')

       ! First check for required input:
       call prop_get_string(node_ptr, '', 'quantity', quantity, retVal)
       if (.not. retVal) then
          write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''quantity'' is missing.'
          call warn_flush()
          cycle
       end if

       call prop_get_string(node_ptr, '', 'forcingFileType', forcingfiletype, retVal)
       if (.not. retVal) then
          write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''forcingFileType'' is missing.'
          call warn_flush()
          cycle
       end if

       call prop_get_string(node_ptr, '', 'forcingFile', forcingfile , retVal)
       if (.not. retVal) then
          write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''forcingFile'' is missing.'
          call warn_flush()
          cycle
       else
          call resolvePath(forcingfile, basedir)
       end if
       oper = 'O'
       call prop_get_string(node_ptr, '', 'operand', oper , retVal)

       targetmaskfile = ''
       call prop_get_string(node_ptr, '', 'targetMaskFile', targetmaskfile, retVal)
       invertMask = .false.
       call prop_get_logical(node_ptr, '', 'targetMaskInvert', invertMask , retVal)

       call realloc(kcsini, ndx, keepExisting=.false., fill = 0)
       if (len_trim(targetMaskFile) > 0) then
          ! Mask flow nodes based on inside polygon(s), or outside.
          ! in: kcs, all flow nodes, out: kcsini: all masked flow nodes.
          call realloc(kdum, ndx, keepExisting=.false., fill = 0)
          call selectelset_internal_nodes(xz, yz, kcs, ndx, kdum, nini, &
                                       LOCTP_POLYGON_FILE, targetmaskfile)
          ! Transfer kdum(1:nini) into a 0-1 mask kcsini(1:ndx)
          do n=1,nini
             kcsini(kdum(n)) = 1
          end do

          if (invertMask) then
             kcsini = ieor(kcsini, 1)
          end if

       else
          ! 100% masking: accept all flow nodes that were already active in kcs.
          where(kcs /= 0) kcsini = 1
       end if

       select case (quantity)
          case ('rainfall','rainfall_rate') ! case is zeer waarschijnlijk overbodig 
             if (.not. allocated(rain) ) then
                allocate ( rain(ndx) , stat=ierr)
                call aerr('rain(ndx)', ierr, ndx)
                rain = 0d0
             endif
             kx = 1
          case ('qext')
             ! Only time-independent sample file supported for now: sets Qext initially and this remains constant in time.
             if (jaQext == 0) then
                write(msgbuf, '(a)') 'quantity '''// trim(quantity) //' in file ''', trim(filename), ''': [', trim(groupname), '] is missing QExt=1 in MDU. Ignoring this block.'
                call warn_flush()
                cycle
             end if
             if (strcmpi(forcingFileType, 'sample')) then
                filetype = triangulation
                fmmethod = 5 ! triangulation
!                transformcoef(4) = 2 ! Nearest-neighbour
             else
                write(msgbuf, '(a)') 'Unknown forcingFileType '''// trim(forcingfiletype) //' in file ''', trim(filename), ''': [', trim(groupname), '], quantity=', trim(quantity), '. Ignoring this block.'
                call warn_flush()
                cycle
             end if
             call prop_get(node_ptr, '', 'locationType', itemtype, success)
             select case (str_tolower(trim(itemtype)))
             case ('1d')
                ilattype = ILATTP_1D
             case ('2d')
                ilattype = ILATTP_2D
             case ('1d2d', 'all')
                ilattype = ILATTP_ALL
             case default
                ilattype = ILATTP_ALL
             end select


             call realloc(kcsini, ndx, keepExisting=.false., fill = 0)
             call prepare_lateral_mask(kcsini, ilattype)
             !kcsini(ndx2d+1:ndxi) = 1 ! Only 1D for now

             success = timespaceinitialfield(xz, yz, qext, ndx, forcingFile, filetype, fmmethod, oper, transformcoef, 2, kcsini) ! zie meteo module
             cycle ! This was a special case, don't continue with timespace processing below.
          case default
             write(msgbuf, '(a)') 'Unknown quantity '''// trim(quantity) //' in file ''', trim(filename), ''': [', trim(groupname), ']. Ignoring this block.'
             call warn_flush()
             cycle
       end select
       select case (trim(str_tolower(forcingfiletype)))
       case ('bcascii')
          filetype = bcascii
          fmmethod = spaceandtime
          ! NOTE: Currently, we only support name=global meteo in.bc files, later maybe station time series as well.
          success = ec_addtimespacerelation(quantity, xz(1:ndx), yz(1:ndx), kcsini, kx,  'global', filetype=filetype, forcingfile=forcingfile, method=fmmethod, operand=oper)
       case ('netcdf')
          filetype = ncgrid
          fmmethod = weightfactors
          success = ec_addtimespacerelation(quantity, xz(1:ndx), yz(1:ndx), kcsini, kx, forcingfile, filetype=filetype, method=fmmethod, operand=oper)
       case ('uniform')
          filetype = provFile_uniform
          fmmethod = spaceandtime
          success = ec_addtimespacerelation(quantity, xz(1:ndx), yz(1:ndx), kcsini, kx, forcingfile, filetype=filetype, method=fmmethod, operand=oper)
       case default
          write(msgbuf, '(a)') 'Unknown forcingFileType '''// trim(forcingfiletype) //' in file ''', trim(filename), ''': [', trim(groupname), ']. Ignoring this block.'
          call warn_flush()
          cycle
       end select
       if (success) then
          select case (quantity)
             case ('rainfall','rainfall_rate')
                jarain = 1
                jaqin = 1
             case ('windxy')
                jawind = 1
          end select
       endif

    case default       ! Unrecognized item in a ext block
       ! initboundaryblocksforcings remains unchanged: Not an error (support commented/disabled blocks in ext file)
       write(msgbuf, '(5a)') 'Unrecognized block in file ''', trim(filename), ''': [', trim(groupname), ']. Ignoring this block.'
       call warn_flush()
    end select
 end do

 if (allocated(itpenzr)) deallocate(itpenzr)
 if (allocated(itpenur)) deallocate(itpenur)
 if (numlatsg > 0) then
    do n = 1,numlatsg
       balat(n) = 0d0
       do k1=n1latsg(n),n2latsg(n)
          k = nnlat(k1)
          if (k > 0) then
             if (.not. is_ghost_node(k)) then
             balat(n) = balat(n) + ba(k)
             end if
          endif
       end do
    end do
    if (jampi > 0) then
       call reduce_sum(numlatsg, balat)
    end if
    if (allocated(kclat)) then
       deallocate(kclat)
    endif
 end if

 call tree_destroy(bnd_ptr)
 if (allocated(thrtt)) then
    call init_threttimes()
 endif

end function initboundaryblocksforcings


!> Initializes memory for laterals on flow nodes.
subroutine ini_alloc_laterals()
   use m_lateral, only : qqlat, kclat, nnlat
   use m_flowgeom, only: ndx2d, ndxi, ndx
   use m_alloc
   use m_flow, only: kmx
   integer :: ierr
   integer :: nlatndguess

   if (.not. allocated(QQlat) ) then                      ! just once
      nlatndguess = ndx2d+2*(ndxi-ndx2d)  ! first guess: all 2D + twice all 1D, nnlat *might* be bigger.
      allocate ( QQLat(max(1,kmx),ndx) , stat=ierr)
      call aerr('QQLAT(ndx)', ierr, ndx)
      QQLat = 0d0
      allocate ( nnLat(nlatndguess) , stat=ierr)
      call aerr('nnLat(nlatndguess)', ierr, nlatndguess)
      nnLat = 0
   endif
   if (.not. allocated(kcLat) ) then
      allocate ( kcLat(ndx) , stat=ierr)                  ! only if needed
      call aerr('kcLat(ndx)', ierr, ndx)
   endif
end subroutine ini_alloc_laterals


!> Prepare the 'kclat' mask array for a specific type of lateral.
subroutine prepare_lateral_mask(kc, ilattype)
   use m_flowgeom
   use m_lateral, only : ILATTP_1D, ILATTP_2D, ILATTP_ALL
   implicit none

   integer         , intent(inout) :: kc(:) !< (ndx) The mask array that is to be filled.
   integer         , intent(in)    :: ilattype !< Type of the new lateral (one of ILATTP_1D|2D|1D2D)

   integer                         :: L, k1, k2

   kc = 0
   select case (ilattype)
   case (ILATTP_1D)       ! in everything 1D
      do L = 1,lnx1D
         !if (abs(prof1D(3,L)) .ne. 1 .and. prof1D(3,L) > 0 ) then ! no pipes pos or neg, others only if pos
            k1 = ln(1,L)
            if (k1 > ndx2d) then
               kc(k1) = 1
            end if
            k2 = ln(2,L)
            if (k2 > ndx2d) then
               kc(k2) = 1
            end if
         !endif
      enddo
   case (ILATTP_2D)       ! in everything 2D
      do L = lnx1D+1,lnxi
         k1 = ln(1,L) ; kc(k1) = 1
         k2 = ln(2,L) ; kc(k2) = 1
      enddo
   case (ILATTP_ALL)      ! both to everything 2D, and 1D, except to 1D pipes
      do L = 1,lnx1D
         ! When is lateral allowed?
         ! * (X)YZ profiles pointering to profiles number: always allow
         ! * direct profiles (rect/circle, etc.):no pipes pos or neg, others only if pos (==non-closed)
         if (prof1D(1,L) < 0 .or. (abs(prof1D(3,L)) .ne. 1 .and. prof1D(3,L) > 0) ) then
            k1 = ln(1,L) ; kc(k1) = 1
            k2 = ln(2,L) ; kc(k2) = 1
         else
            continue
         endif
      enddo
      do L = lnx1D+1,lnxi
         k1 = ln(1,L) ; kc(k1) = 1
         k2 = ln(2,L) ; kc(k2) = 1
      enddo
   end select
end subroutine prepare_lateral_mask


!> Calls the ec_addtimespacerelation with all proper dflowfm-specific
!! target arrays and element set masks for object parameters with
!! spatially uniform time series.
!! Also handles inside one function the old-style *.ext quantities and
!! the new style *.ext and structures.ini quantities.
function adduniformtimerelation_objects(qid, locationfile, objtype, objid, paramname, paramvalue, targetindex, vectormax, targetarray) result(success)
   !use m_flowexternalforcings, no1=>qid, no2=>filetype, no3=>operand, no4 => success
   use m_meteo, no5=>qid, no6=>filetype, no7=>operand, no8 => success
   use string_module, only: strcmpi
   use timespace_parameters, only: uniform, bcascii, spaceandtime
   use unstruc_messages

   implicit none

   character(len=*), intent(in)    :: qid            !< Identifier of current quantity (i.e., 'waterlevelbnd')
   character(len=*), intent(in)    :: locationfile   !< Name of location file (*.pli or *.pol) for current quantity (leave empty when valuestring contains value or filename).
   character(len=*), intent(in)    :: objtype        !< Type name of the object for which this relation is set (e.g., 'lateral', for prettyprinting only).
   character(len=*), intent(in)    :: objid          !< Id of the object for which this relation is set (for prettyprinting only).
   character(len=*), intent(in)    :: paramname      !< Name of the parameter that is set in this relation (e.g., 'discharge', for prettyprinting only).
   character(len=*), intent(in)    :: paramvalue     !< String containing the parameter value (either a scalar double, or 'REALTIME', or a filename)
   integer,          intent(in)    :: targetindex    !< Target index in target value array (typically, the current count of this object type, e.g. numlatsg).
   integer,          intent(in)    :: vectormax      !< The number of values per object ('kx'), typically 1.
   logical                         :: success        !< Return value. Whether relation was added successfully.
   double precision, intent(inout), target :: targetarray(:) !< The target array in which the value(s) will be stored. Either now with scalar, or later via ec_gettimespacevalue() calls.

   character(len=256) :: valuestring, fnam
   double precision   :: valuedble
   double precision   :: xdum(1), ydum(1)
   integer            :: kdum(1)
   integer            :: ierr, L
   double precision, pointer  :: targetarrayptr(:)
   double precision, pointer  :: dbleptr(:)
   integer            :: tgtitem
   integer, pointer   :: intptr, multuniptr
   logical            :: file_exists

   success = .true.   ! initialization
   xdum = 1d0 ; ydum = 1d0; kdum = 1

   if (len_trim(paramvalue) > 0) then
      valuestring = paramvalue
   else if (len_trim(locationfile) > 0) then
      ! Old-style *.ext:
      ! Prepare time series relation, if the .pli file has an associated .tim file.
      L = index(locationfile,'.', back=.true.) - 1
      valuestring = locationfile(1:L)//'_0001.tim'
      inquire(file=valuestring, exist=file_exists)
      if ( .not. file_exists ) then
          valuestring = locationfile(1:L)//'.tim'
          inquire(file=valuestring, exist=file_exists)
          if ( .not. file_exists ) then
             call mess(LEVEL_WARN, 'Files '''//trim(valuestring)//''' and file '''//trim(locationfile(1:L)//'_0001.tim')//''' do not exist.')
          end if
      end if

   else
      ! TODO: AvD: error msg?
      success = .false.
   end if

   ! Now check the valuestring for either scalar/REALTIME/.tim filename
   read(valuestring, *, iostat = ierr) valuedble
   targetarrayptr => targetarray
   tgtitem = ec_undef_int

   if (ierr /= 0 .or. index(valuestring,'/') == 1) then ! No number or a string starting with '/': check for timeseries filename
      if (strcmpi(trim(valuestring), 'REALTIME')) then
         success = .true.
         ! targetarray(targetindex) should be filled via DLL's API
         write(msgbuf, '(a,a,a,a,a)') 'Control for ', trim(objtype), '''' // trim(objid) // ''', ', paramname, ' set to REALTIME.'
         call dbg_flush()
      else
         if (fm_ext_force_name_to_ec_item('','','', qid,multuniptr,intptr,intptr,intptr,dbleptr,dbleptr,dbleptr,dbleptr)) then
            success = .true.
         else
            success = .false.
            write(msgbuf, '(a)') 'Unknown quantity '''//trim(qid)//'''.'
            call warn_flush()
            return
         end if

         fnam = trim(valuestring)
         ! Time-interpolated value will be placed in target array (e.g., qplat(n)) when calling ec_gettimespacevalue.
         if (index(trim(fnam)//'|','.tim|')>0) then
            ! uniform=single time series vectormax = 1
            success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, vectormax, fnam,    &
                                               filetype    = uniform,                     &
                                               method      = spaceandtime,                &
                                               operand     = 'O',                         &
                                               tgt_data1   = targetarrayptr,              &
                                               tgt_item1   = tgtitem,                     &
                                               multuni1    = multuniptr,                  &
                                               targetIndex = targetindex)
         elseif (index(trim(fnam)//'|','.bc|')>0) then
            ! uniform=single time series vectormax = 1

            success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, vectormax, objid,   &
                                               filetype    = bcascii,                     &
                                               method      = spaceandtime,                &
                                               operand     = 'O',                         &
                                               tgt_data1   = targetarrayptr,              &
                                               tgt_item1   = tgtitem,                     &
                                               multuni1    = multuniptr,                  &
                                               targetIndex = targetindex,                 &
                                               forcingfile = fnam)
         endif
      end if
   else
      targetarray(targetindex) = valuedble ! Constant value for always, set it now already.
   end if
end function adduniformtimerelation_objects


subroutine register_quantity_pli_combination(quantity, locationfile)
   use m_alloc
   implicit none
   character(len=*), intent(in)          :: quantity
   character(len=*), intent(in)          :: locationfile
   character(len=max_registered_item_id) :: item_id

   item_id = trim(quantity) // '-' // trim(locationfile)

   if (num_registered_items >= max_ext_bnd_items) then
      max_ext_bnd_items = ceiling(1.2*num_registered_items)
      call realloc(registered_items, max_ext_bnd_items, keepExisting = .true., fill='')
   end if

   num_registered_items = num_registered_items + 1
   registered_items(num_registered_items) = item_id

end subroutine register_quantity_pli_combination

subroutine init_registered_items()
   implicit none
   num_registered_items = 0

   max_ext_bnd_items = 64 ! Default start size.
   if (allocated(registered_items)) deallocate(registered_items)
   allocate(registered_items(max_ext_bnd_items))

   registered_items(1:max_ext_bnd_items) = ''

end subroutine

function quantity_pli_combination_is_registered(quantity, locationfile) result(is_registered)
   implicit none
   logical                               :: is_registered
   character(len=*),intent(in)           :: quantity
   character(len=*),intent(in)           :: locationfile
   integer                               :: i
   character(len=max_registered_item_id) :: item_id

   item_id = trim(quantity) // '-' // trim(locationfile)

   is_registered = .false.

   do i = 1, num_registered_items
      if (item_id == registered_items(i)) then
         is_registered = .true.
         return
      endif
   enddo

end function quantity_pli_combination_is_registered

subroutine init_threttimes()

 use m_flow
 use m_flowgeom
 use m_flowexternalforcings
 use m_transport
 use m_sediment, only: stm_included
 use unstruc_messages
 use m_missing

 implicit none

 integer             :: thrtlen, i, j, nseg, itrac, ifrac, iconst, n, ierr
 character(len=256)  :: qidfm, tracnam, sedfracnam, qidnam
 integer, external   :: findname

 
 ! deallocation of TH arrays
 if(allocated(threttim)) then
    deallocate(threttim)
 endif

 if(nopenbndsect==0) then
    return
 endif

 allocate(threttim(NUMCONST,nopenbndsect), stat=ierr)
 call aerr('threttim(NUMCONST,nopenbndsect)', ierr, nopenbndsect)
 threttim = 0

 ! assign return times using temp arrays
 thrtlen = size(thrtt)
 do i = 1, thrtlen
    qidfm = thrtq(i)
    if(qidfm == 'salinitybnd' .and. allocated(kbnds)) then
       nseg = kbnds(5,thrtn(i))
       if (nseg /=i) cycle
       if (nseg == 0 .or. nseg > nopenbndsect) then
          write(msgbuf,'(i8,a)') thrtn(i), ' salinity boundary point is assigned to incorrect boundary segment' ; call err_flush()
          cycle
       endif
       threttim(ISALT,nseg) = thrtt(i)
    else if(qidfm == 'temperaturebnd' .and. allocated(kbndtm)) then
       nseg = kbndtm(5,thrtn(i))
       if (nseg /=i) cycle
       if (nseg == 0 .or. nseg > nopenbndsect) then
          write(msgbuf,'(i8,a)') thrtn(i), ' temperature boundary point is assigned to incorrect boundary segment' ; call err_flush()
          cycle
       endif
       threttim(ITEMP,nseg) = thrtt(i)
    else if(qidfm == 'sedimentbnd' .and. allocated(kbndsd) .and. .not. stm_included) then
       nseg = kbndsd(5,thrtn(i))
       if (nseg /=i) cycle
       if (nseg == 0 .or. nseg > nopenbndsect) then
          write(msgbuf,'(i8,a)') thrtn(i), ' sediment boundary point is assigned to incorrect boundary segment' ; call err_flush()
          cycle
       endif
       do j = ISED1, ISEDN
         threttim(j,nseg) = thrtt(i)
       enddo
    else if(qidfm(1:9) == 'tracerbnd') then
       call get_tracername(qidfm, tracnam, qidnam)
       itrac = findname(numtracers, trnames, tracnam)
       if (allocated(bndtr).and.thrtn(i)<=nbndtr(itrac) ) then
          nseg = bndtr(itrac)%k(5,thrtn(i))
          if (nseg /=i) cycle
          if (nseg == 0 .or. nseg > nopenbndsect) then
             write(msgbuf,'(i8,a)') thrtn(i), ' tracer boundary point is assigned to incorrect boundary segment' ; call err_flush()
             cycle
          endif
          iconst = itrac2const(itrac)
          threttim(iconst,nseg) = thrtt(i)
       endif
    else if(qidfm(1:10) == 'sedfracbnd') then
       ierr = 0
       call get_sedfracname(qidfm, sedfracnam, qidnam)
       ifrac = findname(numfracs, sfnames, sedfracnam)
       if (allocated(bndsf).and.thrtn(i)<=nbndsf(ifrac)) then      ! i      = no of TH boundaries (i.e. 1 per fraction bnd)
                                                                   ! thrtn  = no of boundaries per fraction
                                                                   ! nbndsf = total no of bnd links per fractions
          nseg = bndsf(ifrac)%k(5,thrtn(i))  ! 5, has open bnd section where TH bnd applies
          !if (nseg /=i) cycle
          if (nseg == 0 .or. nseg > nopenbndsect) then
             ierr = 1
          endif
          iconst = ifrac2const(ifrac)
          if (iconst==0) cycle
          threttim(iconst,nseg) = thrtt(i)
       else
           ierr = 1
       endif
       if( ierr /= 0 ) then
           write(msgbuf,'(i8,a)') thrtn(i), ' sedfrac boundary point is assigned to incorrect boundary segment' ; call err_flush()
           cycle
       endif
    endif
 enddo

 if(allocated(thtbnds)) deallocate(thtbnds)
 if(allocated(thzbnds)) deallocate(thzbnds)
 if(allocated(thtbndtm)) deallocate(thtbndtm)
 if(allocated(thzbndtm)) deallocate(thzbndtm)
 if(allocated(thtbndsd)) deallocate(thtbndsd)
 if(allocated(thzbndsd)) deallocate(thzbndsd)

 allocate(thtbnds(nbnds), thzbnds(nbnds*kmxd), thtbndtm(nbndtm), thzbndtm(nbndtm*kmxd), thtbndsd(nbndsd), thzbndsd(nbndsd*kmxd), stat=ierr)
 call aerr('thtbnds(nbnds), thzbnds(nbnds*kmxd), thtbndtm(nbndtm), thzbndtm(nbndtm*kmxd), thtbndsd(nbndsd), thzbndsd(nbndsd*kmxd)', ierr, (kmxd+1)*(nbnds+nbndtm+nbndsd))
 thzbnds = DMISS

 do i = 1,nbnds
    thtbnds(i) = threttim(ISALT,kbnds(5,i))
 enddo

 do i = 1,nbndtm
    thtbndtm(i) = threttim(ITEMP,kbndtm(5,i))
 enddo

 do i = 1,nbndsd
    thtbndsd(i) = threttim(ISED1,kbndsd(5,i))
 enddo

 if (allocated(bndtr)) then
    do itrac = 1, numtracers
       iconst = itrac2const(itrac)

       if(allocated(bndtr(itrac)%tht)) deallocate(bndtr(itrac)%tht)
       if(allocated(bndtr(itrac)%thz)) deallocate(bndtr(itrac)%thz)

       n = nbndtr(itrac)

       allocate (bndtr(itrac)%tht(n), bndtr(itrac)%thz(n*kmxd), stat=ierr)
       call aerr('bndtr(itrac)%tht(n), bndtr(itrac)%thz(n*kmxd)', ierr, n*(kmxd+1))

       bndtr(itrac)%thz = dmiss
       do i = 1,n
         bndtr(itrac)%tht(i) = threttim(iconst,bndtr(itrac)%k(5,i))
       enddo
    enddo
 endif

 if (allocated(bndsf)) then
    do ifrac = 1, numfracs
       if(allocated(bndsf(ifrac)%tht)) deallocate(bndsf(ifrac)%tht)
       if(allocated(bndsf(ifrac)%thz)) deallocate(bndsf(ifrac)%thz)

       n = nbndsf(ifrac)

       allocate(bndsf(ifrac)%tht(n), bndsf(ifrac)%thz(n*kmxd), stat=ierr)
       call aerr('bndsf(ifrac)%tht(n), bndsf(ifrac)%thz(n*kmxd)', ierr, n*(kmxd+1))

       bndsf(ifrac)%thz = dmiss
       ! mapping to constituents, just in case fracs do not map sequentially to ised1 and so on
       iconst = ifrac2const(ifrac)
       if (iconst==0) then
          bndsf(ifrac)%tht = 0d0
       else
          do i = 1,n
             bndsf(ifrac)%tht(i) = threttim(iconst,bndsf(ifrac)%k(5,i))
          enddo
       end if
    enddo
 endif

end subroutine init_threttimes

end module unstruc_boundaries
