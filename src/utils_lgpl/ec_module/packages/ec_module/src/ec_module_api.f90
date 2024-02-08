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
!  
!  


module ec_module_api

   use iso_c_binding
   use iso_c_utils
   use m_ec_module
   use m_ec_parameters
   
implicit none

   type(tEcInstance), pointer  :: instancePtr

   public :: averaging
   
   private
   
   contains
   
function triangulation(meshtwoddim, meshtwod, startIndex, c_sampleX, c_sampleY, c_sampleValues, numSamples, c_targetValues, locType, jsferic, jasfer3D) result(ierr) bind(C, name="triangulation")
    !DEC$ ATTRIBUTES DLLEXPORT :: triangulation

    !from ec_module
    use m_ec_basic_interpolation, only: triinterp2
    use m_missing
    use meshdata
    use precision_basics
    use network_data
    use m_alloc

    implicit none

    ! inputs
    type(c_t_ug_meshgeomdim), intent(in)    :: meshtwoddim         !< input 2d mesh dimensions
    type(c_t_ug_meshgeom), intent(in)       :: meshtwod            !< input 2d mesh 
    integer(c_int), intent(in)              :: startIndex          !< start index of index based arrays (might be needed for 1d interpolation)
    type(c_ptr), intent(in)                 :: c_sampleX           !< samples x
    type(c_ptr), intent(in)                 :: c_sampleY           !< samples y 
    type(c_ptr), intent(in)                 :: c_sampleValues      !< samples values
    integer(c_int), intent(in)              :: numSamples          !< number of samples
    type(c_ptr),    intent(inout)           :: c_targetValues      !< return values (ptr to double array)
    integer(c_int), intent(in)              :: locType             !< destination location type: 0: To flow nodes, 1: to zk net nodes, 2: to center of cell edges
    integer(c_int), intent(in)              :: jsferic             
    integer(c_int), intent(in)              :: jasfer3D

    ! local variables
    type(t_ug_meshgeom)                      :: meshgeom            !< fortran meshgeom
    real(c_double), pointer                  :: sampleX(:)
    real(c_double), pointer                  :: sampleY(:)
    real(c_double), pointer                  :: sampleValues(:)
    real(c_double), pointer                  :: targetValues(:)
    double precision, allocatable            :: targetX(:)
    double precision, allocatable            :: targetY(:)
    integer                                  :: numTargets
    integer                                  :: i    
      
    !From other modules
    integer                                  :: ierr 
    integer                                  :: jdla
    real(hp)                                 :: transformcoef(6)
    integer                                  :: shift, start_node, end_node 
    
    !fill meshgeom
    ierr = 0
    ierr = network_data_destructor()
    ierr = convert_cptr_to_meshgeom(meshtwod, meshtwoddim, meshgeom)
    
    !determine number of numTargets
    if (locType.eq.0) then
      numTargets = size(meshgeom%facex)
      allocate(targetX(numTargets))
      allocate(targetY(numTargets))
      targetX = meshgeom%facex
      targetY = meshgeom%facey
    else if (locType.eq.1) then
      numTargets = size(meshgeom%nodex)
      allocate(targetX(numTargets))
      allocate(targetY(numTargets))
      targetX = meshgeom%nodex
      targetY = meshgeom%nodey
    else if (locType.eq.2) then  
      shift = 1 - startIndex
      do i=1,size(meshgeom%edge_nodes,2)
         meshgeom%edge_nodes(1,i) = meshgeom%edge_nodes(1,i) + shift
         meshgeom%edge_nodes(2,i) = meshgeom%edge_nodes(2,i) + shift
      enddo
      allocate(targetX(size(meshgeom%edge_nodes,2)))
      allocate(targetY(size(meshgeom%edge_nodes,2)))      
      numTargets = 0
      do i=1,size(meshgeom%edge_nodes,2)
         start_node = meshgeom%edge_nodes(1,i)
         end_node = meshgeom%edge_nodes(2,i)
         if (start_node>=1 .and. start_node<=size(meshgeom%nodex,1) .and. end_node>=1 .and. end_node<=size(meshgeom%nodex,1)) then
            targetX(i) = (meshgeom%nodex(start_node) + meshgeom%nodex(end_node)) * 0.5d0
            targetY(i) = (meshgeom%nodey(start_node) + meshgeom%nodey(end_node)) * 0.5d0
            numTargets = numTargets + 1
         end if
      enddo
      if(numTargets.ne.size(meshgeom%edge_nodes,2)) then
         ierr = -1
         goto 1234  
      endif
    else
      !not valid location
      ierr = -1
      goto 1234  
    endif
    
    call c_f_pointer(c_sampleX, sampleX, (/numSamples/))
    call c_f_pointer(c_sampleY, sampleY, (/numSamples/))
    call c_f_pointer(c_sampleValues, sampleValues, (/numSamples/))
    call c_f_pointer(c_targetValues, targetValues, (/numTargets/))

    targetValues = dmiss
    transformcoef = 0.0d0
    jdla = 1
    
    ! (re)allocate polygon to 1
    call realloc(XPL, 1, keepExisting=.false.)
    call realloc(YPL, 1, keepExisting=.false.)
    call realloc(ZPL, 1, keepExisting=.false.)
      
    ! call triangulate (dres is the result)
    call triinterp2(XZ = targetX,& 
    YZ = targetY, &
    BL = targetValues,& 
    NDX = numTargets, &
    JDLA = jdla,& 
    XS = sampleX,& 
    YS = sampleY,&
    ZS = sampleValues,& 
    ns = numSamples,& 
    dmiss = dmiss,& 
    jsferic = jsferic,& 
    jins = 1,& 
    jasfer3D = jasfer3D, &
    NPL = 0,& 
    MXSAM = 0,& 
    MYSAM =0,& 
    XPL = XPL,& 
    YPL = YPL,& 
    ZPL = ZPL,& 
    transformcoef = transformcoef)
    
1234 continue

end function triangulation


function averaging(meshtwoddim, meshtwod, startIndex, c_sampleX, c_sampleY, c_sampleValues, numSamples, c_targetValues, locType, Wu1Duni, method, minNumSamples, relativeSearchSize, jsferic, jasfer3D) result(ierr) bind(C, name="averaging")
    !DEC$ ATTRIBUTES DLLEXPORT :: averaging
    use kdtree2Factory
    use m_ec_interpolationsettings
    use m_ec_basic_interpolation, only: averaging2, TerrorInfo
    use gridoperations
    use precision_basics
    use m_missing
    use meshdata
    use network_data
    use m_cell_geometry
    
    implicit none
    
    ! parameters
    type(c_t_ug_meshgeomdim), intent(in)    :: meshtwoddim         !< input 2d mesh dimensions
    type(c_t_ug_meshgeom), intent(in)       :: meshtwod            !< input 2d mesh 
    integer(c_int), intent(in)              :: startIndex          !< the start_index index of the arrays
    type(c_ptr),    intent(in)              :: c_sampleX           !< samples x
    type(c_ptr),    intent(in)              :: c_sampleY           !< sample  y
    type(c_ptr),    intent(in)              :: c_sampleValues      !< sample values
    integer(c_int), intent(in)              :: numSamples          !< number of samples
    type(c_ptr),    intent(inout)           :: c_targetValues      !< return values (ptr to double array)
    integer(c_int), intent(in)              :: locType             !< destination location type: 0: To flow nodes, 1: to net nodes, 2: to center of cell edges
    real(c_double), intent(in)              :: Wu1Duni
    integer(c_int), intent(in)              :: method              !< averaging method
    integer(c_int), intent(in)              :: minNumSamples       !< minimum nr of samples for avaraging
    real(c_double), intent(in)              :: relativeSearchSize  !< relative search cell size
    integer(c_int), intent(in)              :: jsferic
    integer(c_int), intent(in)              :: jasfer3D
    integer                                 :: ierr

    ! local variables
    type(t_ug_meshgeom)                     :: meshgeom            !< fortran meshgeom
    real(c_double), pointer                 :: sampleX(:)
    real(c_double), pointer                 :: sampleY(:)
    real(c_double), pointer                 :: sampleValuesTemp(:)
    real(c_double), pointer                 :: targetValues(:)
    double precision, allocatable           :: sampleValues(:,:)
    integer, allocatable                    :: ipsam(:)
    double precision, allocatable           :: interpolationResults(:,:)
    double precision, allocatable           :: cxx(:,:)
    double precision, allocatable           :: cyy(:,:)
    double precision, allocatable           :: targetX(:)
    double precision, allocatable           :: targetY(:)
    double precision, allocatable           :: rawTargetValues(:)
    integer                                 :: k, IAVtmp, NUMMINtmp, INTTYPEtmp, i, jakdtree, numTargets
    double precision                        :: RCELtmp, valFirstNode,valSecondNode
    integer                                 :: nMaxNodesPolygon
    real(hp), allocatable                   :: xx(:,:), yy(:,:), xxx(:), yyy(:)
    integer, allocatable                    :: nnn(:)
    integer                                 :: nNetCells, shift
    type(TerrorInfo)                        :: errorInfo

    !get and convert meshgeom to kn table
    ierr = network_data_destructor()
    ierr = convert_cptr_to_meshgeom(meshtwod, meshtwoddim, meshgeom)
    ierr = ggeo_convert(meshgeom, startIndex)
    
    !determine number of numTargets
    if (locType.eq.0) then
      !to net nodes and edges
      numTargets = size(meshgeom%facex)
      allocate(targetX(numTargets))
      allocate(targetY(numTargets))
      targetX = meshgeom%facex
      targetY = meshgeom%facey
    else if ((locType.eq.1).or.(locType.eq.2)) then
      !to flow nodes
       numTargets = size(meshgeom%nodex)
      allocate(targetX(numTargets))
      allocate(targetY(numTargets))
      targetX = meshgeom%nodex
      targetY = meshgeom%nodey  
    else
      !not valid location
      ierr = -1
      goto 1234  
    endif
        
    !allocate arrays and a dummy polygon for averaging2
    allocate (XPL(1), YPL(1), ZPL(1))

    ! cache interpolation settings
    IAVtmp = IAV
    NUMMINtmp = NUMMIN
    INTTYPEtmp = INTERPOLATIONTYPE
    RCELtmp = RCEL

    !sample arrays
    allocate(sampleValues(1, numSamples), ipsam(numSamples), interpolationResults(1,numTargets))
    call c_f_pointer(c_sampleX, sampleX, (/numSamples/))
    call c_f_pointer(c_sampleY, sampleY, (/numSamples/))
    call c_f_pointer(c_sampleValues, sampleValuesTemp, (/numSamples/))
    
    
    !assign values
    sampleValues(1,:)          = sampleValuesTemp(:)
    ipsam(:)                   = 1
    interpolationResults(1,:)  = dmiss
    
    !set interpolation settings
    if(method > 0 .and. method < 8) then
       IAV = method
    else
       goto 1234
    endif
    if(minNumSamples > 0) then
       NUMMIN = minNumSamples
    else
       goto 1234
    endif
    if(relativeSearchSize > 0 .and. relativeSearchSize < 10) then
       RCEL = relativeSearchSize
    else
       goto 1234
    endif
    INTERPOLATIONTYPE = 2

    !build kdtree
    jakdtree = 1
    call build_kdtree(treeglob, numSamples, sampleX, sampleY, ierr, jsferic, dmiss)
    
    !find cells
    ierr = 0
    call findcells(100000)

    if (locType.eq.0) then
       nNetCells = size(xzw)
       !to flow nodes
       nMaxNodesPolygon = maxval(netcell%n)
       allocate( xx(nMaxNodesPolygon,nNetCells), yy(nMaxNodesPolygon,nNetCells), nnn(nNetCells) , stat = ierr)
       do i = 1,nNetCells
          nnn(i) = netcell(i)%n
          do k=1,nnn(i)
             xx(k,i) = xzw(i) + rcel*(xk(netcell(i)%nod(k))-xzw(i))
             yy(k,i) = yzw(i) + rcel*(yk(netcell(i)%nod(k))-yzw(i))
          enddo
       enddo

    else if ((locType.eq.1).or.(locType.eq.2)) then
       !to net nodes
       nMaxNodesPolygon = 3*maxval(nmk)   ! 2: safe upper bound , 3 : even safer!
       allocate( xx(nMaxNodesPolygon,numk), yy(nMaxNodesPolygon,numk), nnn(numk), xxx(nMaxNodesPolygon), yyy(nMaxNodesPolygon) )
       do i = 1,numk
          call make_dual_cell(i, nMaxNodesPolygon, rcel, xxx, yyy, nnn(i), Wu1Duni)
          do k=1,nnn(i)
             xx(k,i) = xxx(k)
             yy(k,i) = yyy(k)
          enddo
       enddo

    endif

    !interpolation 
    call averaging2(1,&     !sample vector dimension
    numSamples,&            !number of samples
    sampleX,&               !sample x coordinate
    sampleY,&               !sample y coordinate
    sampleValues,&          !sample value
    ipsam,&                 !sample permutation array (increasing x-coordinate)
    targetX,&               !destination points x coordinates
    targetY,&               !destination points y coordinates
    interpolationResults,&  !interpolated values
    numTargets,&            !number of destination points
    xx,&                    !polygon points x coordinates
    yy,&                    !polygon points y coordinates
    nMaxNodesPolygon,&      !number of poligon vertices
    nnn,&                   !output variable from make_dual_cell
    jakdtree,&              !jakdtree
    dmiss,&
    jsferic,&
    jasfer3D,&
    1,&
    0,&
    XPL,&
    YPL,&
    ZPL, &
    errorInfo)

    !delete kdtree
    call delete_kdtree2(treeglob)

    if ( .not. errorInfo%success) then
       ierr = -1
       goto 1234
    end if

    !copy values back
    if (locType.eq.0) then
       !to flow nodes, we assume no re-mapping is needed
       call c_f_pointer(c_targetValues, targetValues, (/numTargets/))
       targetValues = interpolationResults(1,:)
    else if (locType.eq.1) then
       !to net node: use the permutation array
       call c_f_pointer(c_targetValues, targetValues, (/numTargets/))
       targetValues = interpolationResults(1,nodePermutation)
    else if(locType.eq.2) then
        shift = 1 - meshgeom%start_index
        do i=1,size(meshgeom%edge_nodes,2)
          meshgeom%edge_nodes(1,i) = meshgeom%edge_nodes(1,i) + shift
          meshgeom%edge_nodes(2,i) = meshgeom%edge_nodes(2,i) + shift
        enddo
       ! if the node have been permuted by findcell, restore the original ordering, supposing is the same as 
       ! the one in the edge_nodes       
       allocate(rawTargetValues(numTargets))
       rawTargetValues = interpolationResults(1,nodePermutation)
       !reset number of targets, associate c# and fortran pointers
       numTargets = size(meshgeom%edge_nodes,2);
       call c_f_pointer(c_targetValues, targetValues, (/numTargets/))
       targetValues = dmiss
       do i =1, numTargets
          valFirstNode  = rawTargetValues(meshgeom%edge_nodes(1,i))
          valSecondNode = rawTargetValues(meshgeom%edge_nodes(2,i))
          if ((valFirstNode.ne.dmiss).and.(valSecondNode.ne.dmiss)) then
             targetValues(i) = (valFirstNode + valSecondNode)/2.0d0
          endif
          if ((valFirstNode.eq.dmiss).and.(valSecondNode.ne.dmiss)) then
             targetValues(i) = valSecondNode
          endif
          if ((valFirstNode.ne.dmiss).and.(valSecondNode.eq.dmiss)) then
             targetValues(i) = valFirstNode
          endif
       enddo
    endif

1234 continue

    !unroll & cleanup
    IAV = IAVtmp
    NUMMIN = NUMMINtmp
    INTERPOLATIONTYPE = INTTYPEtmp
    RCEL = RCELtmp
    if (allocated(sampleValues)) deallocate(sampleValues)
    if (allocated(ipsam)) deallocate(ipsam)
    if (allocated(interpolationResults))  deallocate(interpolationResults)
    if (allocated(cxx)) deallocate(cxx)
    if (allocated(cyy)) deallocate(cyy)
    if (allocated(xx))  deallocate(xx)
    if (allocated(yy))  deallocate(yy)
    if (allocated(nnn)) deallocate(nnn)
    if (allocated(xxx)) deallocate(xxx)
    if (allocated(yyy)) deallocate(yyy) 

end function averaging
   
end module ec_module_api
