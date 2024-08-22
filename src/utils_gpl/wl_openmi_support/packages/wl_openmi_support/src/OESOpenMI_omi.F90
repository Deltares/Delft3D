!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! OESOpenMI.F90: DLL on OES - module
!!!
!!! (c) Deltares, may 2004
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!! File organization:
!!!
!!!   - GENERAL FUNCTIONS 
!!!     .  Create Oes Module
!!!
!!!   - INTERFACE TOWARDS WRAPPERS, LINKABLE COMPONENT PART
!!!     .  Relate Engine-QuantityName to Wrapper-QuantityName
!!!     .  Define Quantities on Elementsets from in/out Links
!!!     .  Put/Get vales fro Quantities on Elementsets from in/out Links
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!-----------------------------------------------------------------
! GENERAL FUNCTIONS 
!-----------------------------------------------------------------


function GetArgumentStringLength() result(argsStringLength)
    !DEC$ ATTRIBUTES DLLEXPORT :: GetArgumentStringLength
    use wl_open_mi_support
    integer :: argsStringLength
    argsStringLength = oes_id_len
end function GetArgumentStringLength

subroutine create_OES()
    !DEC$ ATTRIBUTES DLLEXPORT :: create_OES
    use wl_open_mi_support
    implicit none
    call OesCreate()
end subroutine create_OES

subroutine destroy_OES()
    !DEC$ ATTRIBUTES DLLEXPORT :: destroy_OES
    use wl_open_mi_support
    implicit none
    call OesDestroy()
end subroutine destroy_OES

subroutine create_OES_with_logging(logName)
    !DEC$ ATTRIBUTES DLLEXPORT :: create_OES_with_logging
    use wl_open_mi_support
    use dio_streams
    implicit none
    character(Len=*), intent(in) :: logName
    call OesCreate(logName)
end subroutine create_OES_with_logging


function initialized_OES() result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: initialized_OES
    use wl_open_mi_support
    implicit none
    integer :: retVal   ! 0 == initialized
    retVal = -99
    if ( OesInitialized() ) retVal = 0
end function initialized_OES


!-----------------------------------------------------------------
! INTERFACE TOWARDS WRAPPERS, LINKABLE COMPONENT PART
!-----------------------------------------------------------------


function ModelFindOrCreate_OES(componentID , schemID) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: ModelFindOrCreate_OES
    use wl_open_mi_support
    implicit none

    ! return value
    integer :: retVal ! retVal > 0 : success

    character(Len=*)      , intent(in) :: componentID
    character(Len=*)      , intent(in) :: schemID

    if ( .not. OesInitialized() ) then
        call OesCreate('D:\oes_from_wrapper.log')
    endif        

    retVal = OesModelFindOrCreate(trim(componentID)//'-Wrapper', schemID)

end function ModelFindOrCreate_OES

function DefineExchItemIdsElmset_OES(componentID , schemID         , &
                                     quantityID  , role            , &
                                     elementsetID, ElementIDs      , &
                                     elementCount, maxElementLength) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: DefineExchItemIdsElmset_OES
    use wl_open_mi_support
    implicit none

    ! return value
    integer :: retVal ! retVal == 0 : success

    ! arguments
    character(Len=*)      , intent(in) :: componentID
    character(Len=*)      , intent(in) :: schemID
    character(Len=*)      , intent(in) :: quantityID
    integer               , intent(in) :: role
    character(Len=*)      , intent(in) :: elementsetID
    integer               , intent(in) :: elementCount
    integer               , intent(in) :: maxElementLength
    character(Len=elementCount* &
                   maxElementLength), &
                            intent(in) :: ElementIDs

    ! locals
    character(Len=oes_id_len), &
             dimension(:), allocatable :: IDs2Oes     ! array for element id's
    integer                            :: modelHandle ! handle to computation
    integer                            :: esIndex     ! handle to created elem.set
    integer                            :: eiIndex     ! handle to created exch.item
    integer                            :: i           ! loop counter

    ! body

    retVal = -99
    ! WRITE(*,*) 'OES add M: ', trim(componentID)
    ! WRITE(*,*) 'OES add S: ', trim(schemID)
    ! WRITE(*,*) 'OES add Q: ', trim(quantityID)
    ! WRITE(*,*) 'OES add E: ', trim(elementsetID)
    modelHandle = OesModelFind(trim(componentID)//'-Wrapper', schemID)
    ! WRITE(*,*) 'OES add mh: ', modelHandle
    if ( modelHandle > 0 ) then
        allocate(IDs2Oes(elementCount))
        do i=1,elementCount
            IDs2Oes(i) = trim(ElementIDs((i-1)*maxElementLength+1:i*maxElementLength))
        end do
        esIndex = OesElmsetFindOrCreate(modelHandle, elementsetID, IDs2Oes)
        ! WRITE(*,*) 'OES add es: ', esIndex
        if ( esIndex > 0 ) then
            if ( role == oes_providing ) then
                eiIndex = OesExchItemCreate(modelHandle, quantityID, elementsetID, oes_accepting)
                ! WRITE(*,*) 'OES add EI: ', eiIndex
                if ( eiIndex > 0 ) then
                    call OesRelate( componentID, quantityID, elementsetID, &
                                                trim(componentID)//'-Wrapper' )
                    retVal = 0
                endif
            else
                ! WRITE(*,*) 'OES add ei: ', eiIndex
                eiIndex = OesExchItemCreate(modelHandle, quantityID, elementsetID, oes_providing)
                if ( eiIndex > 0 ) then
                    call OesRelate( trim(componentID)//'-Wrapper', quantityID, elementsetID, &
                                                 componentID)
                    retVal = 0
                endif
            endif
        endif
        deallocate(IDs2Oes)
    endif

end function DefineExchItemIdsElmset_OES

function DefineWrapperElmset_OES(componentID , schemID         , &
                                 quantityID  , role            , &
                                 elementsetID    ) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: DefineWrapperElmset_OES
    use wl_open_mi_support
    implicit none

    ! return value
    integer :: retVal ! retVal == 0 : success

    ! arguments
    character(Len=*), intent(in) :: componentID
    character(Len=*), intent(in) :: schemID
    character(Len=*), intent(in) :: quantityID
    integer         , intent(in) :: role
    character(Len=*), intent(in) :: elementsetID

    ! locals
    character(Len=oes_id_len), pointer, &
                  dimension(:) :: ids          ! element ids
    integer                    :: numElms      ! # element ids
    integer                    :: modelHandle  ! handle to computation
    integer, external          :: DefineExchItemIdsElmset_OES

    ! body

    retVal = -99
    modelHandle = OesModelFind(trim(componentID), schemID)

    if ( modelHandle > 0 ) then
        ids => OesElmsetGetElmIDs(componentID, schemID, elementsetID)
        if (.not. associated(ids)) then
            write(*,*) 'FOUTE BOEL'
            return
        endif
        numElms = size(ids)
        retVal = DefineExchItemIdsElmset_OES(componentID, schemID, quantityID, role, elementsetID, ids, size(ids,1), len(ids(1)))
        if (retVal == 0) then
            retVal = numElms
        endif
    endif

end function DefineWrapperElmset_OES

subroutine AnalyzeRelations_OES()
    !DEC$ ATTRIBUTES DLLEXPORT :: AnalyzeRelations_OES
    use wl_open_mi_support
    implicit none

    ! body
    call OesAnalyzeRelations(.true.)

end subroutine AnalyzeRelations_OES


!-----------------------------------------------------------------
! INTERFACE TOWARDS WRAPPERS, EXCHANGEMODEL PART
!-----------------------------------------------------------------


function GETQUANTITYCOUNT(model) result(retVal)

    !DEC$ ATTRIBUTES DLLEXPORT :: GETQUANTITYCOUNT
    use wl_open_mi_support

    ! return value

    integer                       :: retVal         ! >=0 : #items; <0 : Error

    ! arguments

    character(len=*), intent(in)  :: model          ! model identifier

    ! body

    retVal = OesQuantGetCount(model)
    
end function GETQUANTITYCOUNT


function GETQUANTITY(quantityIndex, model, quantity) result(retVal)

    !DEC$ ATTRIBUTES DLLEXPORT :: GETQUANTITY
    use wl_open_mi_support

    ! return value

    integer                       :: retVal         ! >=0 : Success; <0 : Error

    ! arguments

    integer         , intent(in)  :: quantityIndex  ! quantityIndex
    character(len=*), intent(in)  :: model          ! model identifier
    character(len=*), intent(out) :: quantity       ! quantity identifier

    ! body

    retVal = -1
    quantity = OesQuantGetID(model, quantityIndex + 1)
    if ( quantity /= ' ' ) then
        retVal = 0
    endif
    
end function GETQUANTITY


function GETELEMENTTYPECOUNT(model) result(retVal)

    !DEC$ ATTRIBUTES DLLEXPORT :: GETELEMENTTYPECOUNT
    use wl_open_mi_support

    ! return value

    integer                       :: retVal         ! >=0 : #items; <0 : Error

    ! arguments

    character(len=*), intent(in)  :: model          ! model identifier

    ! body

    retVal = OesElmtypeGetCount(model, anySchemString)
    
end function GETELEMENTTYPECOUNT


function GETELEMENTTYPE(elementTypeIndex, model, elementType) result(retVal)

    !DEC$ ATTRIBUTES DLLEXPORT :: GETELEMENTTYPE
    use wl_open_mi_support

    ! return value

    integer                       :: retVal         ! >=0 : Success; <0 : Error

    ! arguments

    integer         , intent(in)  :: elementTypeIndex   ! elementType identifierindex
    character(len=*), intent(in)  :: model          ! model identifier
    character(len=*), intent(out) :: elementType        ! elementType identifier

    ! body

    retVal = -1
    elementType = OesElmtypeGetID(model, anySchemString, elementTypeIndex + 1)
    if ( elementType /= ' ' ) then
        retVal = 0
    endif

end function GETELEMENTTYPE


function GETELEMENTSETCOUNT(model, schematization, elementType) result(retVal)

    !DEC$ ATTRIBUTES DLLEXPORT :: GETELEMENTSETCOUNT
    use wl_open_mi_support

    ! return value

    integer                       :: retVal         ! >=0 : #items; <0 : Error

    ! arguments

    character(len=*), intent(in)  :: model          ! model identifier
    character(len=*), intent(in)  :: schematization ! schematization identifier
    character(len=*), intent(in)  :: elementType    ! elementType identifier

    ! body

    retVal = OesElmsetGetCount(model, schematization, elementType)
    
end function GETELEMENTSETCOUNT


function GETELEMENTSET(elmSetIndex, model, schematization, elementType, elementSet) result(retVal)

    !DEC$ ATTRIBUTES DLLEXPORT :: GETELEMENTSET
    use wl_open_mi_support

    ! return value

    integer                       :: retVal         ! >=0 : Success; <0 : Error

    ! arguments

    integer         , intent(in)  :: elmSetIndex    ! elementset index
    character(len=*), intent(in)  :: model          ! model identifier
    character(len=*), intent(in)  :: schematization ! schematization identifier
    character(len=*), intent(in)  :: elementType    ! elementType identifier
    character(len=*), intent(out) :: elementSet     ! elementSet identifier

    ! body

    retVal = -1
    elementSet = OesElmsetGetID(model, schematization, elementType, elmSetIndex + 1)
    if ( elementSet /= ' ' ) then
        retVal = 0
    endif

end function GETELEMENTSET


function GETELEMENTINSETCOUNT(model, schematization, elementSet) result(retVal)

    !DEC$ ATTRIBUTES DLLEXPORT :: GETELEMENTINSETCOUNT
    use wl_open_mi_support

    ! return value

    integer                       :: retVal         ! >=0 : #items; <0 : Error

    ! arguments

    character(len=*), intent(in)  :: model          ! model identifier
    character(len=*), intent(in)  :: schematization ! elementSet identifier
    character(len=*), intent(in)  :: elementSet     ! quantity identifier

    ! body

    retVal = OesElmsetGetElementCount(model, schematization, elementSet)
    
end function GETELEMENTINSETCOUNT


function GETELEMENTINSET(elementIndex, model, schematization, elementSet, element) result(retVal)

    !DEC$ ATTRIBUTES DLLEXPORT :: GETELEMENTINSET
    use wl_open_mi_support

    ! return value

    integer                       :: retVal         ! >=0 : Success; <0 : Error

    ! arguments

    integer         , intent(in)  :: elementIndex   ! element index
    character(len=*), intent(in)  :: model          ! model identifier
    character(len=*), intent(in)  :: schematization ! schematization identifier
    character(len=*), intent(in)  :: elementSet     ! elementSet identifier
    character(len=*), intent(out) :: element        ! element identifier

    ! body

    retVal = -1
    element = OesElmsetGetElmID(model, schematization, elementSet, elementIndex + 1)
    if ( element /= ' ' ) then
        retVal = 0
    endif
    
end function GETELEMENTINSET


function GETELEMENTINSETVERTEXCOUNT(elementIndex, model, schematization, elementSet) result(retVal)

    !DEC$ ATTRIBUTES DLLEXPORT :: GETELEMENTINSETVERTEXCOUNT
    use wl_open_mi_support

    ! return value

    integer                       :: retVal         ! >=0 : #items; <0 : Error

    ! arguments

    integer         , intent(in)  :: elementIndex   ! element index in element set
    character(len=*), intent(in)  :: model          ! model identifier
    character(len=*), intent(in)  :: schematization ! schematization identifier
    character(len=*), intent(in)  :: elementSet     ! elementSet identifier

    ! body

    retVal = OesElmsetGetElementVertexCount(elementIndex, model, schematization, elementSet)
    
end function GETELEMENTINSETVERTEXCOUNT


function GETELEMENTINSETCOORDINATE(elementIndex, vertexIndex, model, schematization, elementSet, axis) result(retVal)

    !DEC$ ATTRIBUTES DLLEXPORT :: GETELEMENTINSETCOORDINATE
    use wl_open_mi_support

    ! return value
    double precision              :: retVal         ! coordinate on X, Y, or Z axis

    ! arguments
    integer         , intent(in)  :: elementIndex   ! element index in element set
    integer         , intent(in)  :: vertexIndex    ! vertex index
    character(len=*), intent(in)  :: model          ! model identifier
    character(len=*), intent(in)  :: schematization ! schematization identifier
    character(len=*), intent(in)  :: elementSet     ! elementSet identifier
    integer         , intent(in)  :: axis           ! axis index (X/Y/Z)


    ! body

    retVal = OesElmsetGetElementCoordinate(elementIndex, vertexIndex, model, schematization, elementSet, axis)
    
end function GETELEMENTINSETCOORDINATE


function CANACCEPTONSET(model, schematization, elementSet, quantity) result(retVal)

    !DEC$ ATTRIBUTES DLLEXPORT :: CANACCEPTONSET
    use wl_open_mi_support

    ! return value

    integer                       :: retVal         ! 0 : false, /=0 : true

    ! arguments

    character(len=*), intent(in)  :: model          ! model identifier
    character(len=*), intent(in)  :: schematization ! schematization identifier
    character(len=*), intent(in)  :: elementSet     ! elementSet identifier
    character(len=*), intent(in)  :: quantity       ! quantity identifier

    ! locals
    integer :: role

    ! body

    retVal = 0
    role = OesExchItemGetRole(model, schematization, quantity, elementSet)
    if ( role == oes_accepting .or. role == oes_both ) then
        retVal = 1
    endif

end function CANACCEPTONSET


function CANPROVIDEONSET(model, schematization, elementSet, quantity) result(retVal)

    !DEC$ ATTRIBUTES DLLEXPORT :: CANPROVIDEONSET
    use wl_open_mi_support

    ! return value

    integer                       :: retVal         ! 0: false; >0: true; <0: error

    ! arguments

    character(len=*), intent(in)  :: model          ! model identifier
    character(len=*), intent(in)  :: schematization ! schematization identifier
    character(len=*), intent(in)  :: elementSet     ! elementSet identifier
    character(len=*), intent(in)  :: quantity       ! quantity identifier

    ! locals
    integer :: role

    ! body

    retVal = 0
    role = OesExchItemGetRole(model, schematization, quantity, elementSet)
    if ( role == oes_providing .or. role == oes_both ) then
        retVal = 1
    endif

end function CANPROVIDEONSET


function GETERROR(error, errorDescription) result(retVal)

    !DEC$ ATTRIBUTES DLLEXPORT :: GETERROR
    use wl_open_mi_support

    ! return value

    integer                       :: retVal         ! >=0 : Success; <0 : Error

    ! arguments

    integer         , intent(in)  :: error            ! error index
    character(len=*), intent(out) :: errorDescription ! error description text

    ! body

    errorDescription = ''
    if ( error == -77 ) then
        errorDescription = 'WLDelft OpenMI Component Demo License expired'
    else
        errorDescription = OesGetLastError()
    endif

    retVal = 0
    
end function GETERROR

function OES_SetValues(componentID , schemID      , QuantityID, &
                      ElementsetID, ElementCount , Values      ) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: OES_SetValues
    
    use wl_open_mi_support
    implicit none

    ! result
    integer :: retVal                                ! retVal == 0 : success

    ! arguments
    character(Len=*) , intent(in)    :: componentID  ! RR, RTC, etc.
    character(Len=*) , intent(in)    :: schemID      ! schem. file (*.fnm)
    character(Len=*) , intent(in)    :: QuantityID   ! quant. identifier
    character(Len=*) , intent(in)    :: ElementsetID ! elem.set. identifier
    integer          , intent(in)    :: ElementCount ! #elems in elementset
    double precision , intent(in), &
           dimension(1:ElementCount) :: Values       ! values in elemenset

    ! locals
    integer                          :: modelHandle  ! handle to computation

    ! body

    retVal = OES_UNDEFINED

    modelHandle = OesModelFind(trim(componentID)//'-Wrapper', schemID)
    if ( modelHandle <= 0 ) then
        modelHandle = OesModelFind(trim(componentID), schemID) ! D-RR fix
        if ( modelHandle <= 0 ) return
    endif

    call OesPut(modelHandle, QuantityID, ElementsetID, Values)
    retVal = 0

end function OES_SetValues

function OES_ResetValues(componentID , schemID      , QuantityID, &
                        ElementsetID                             ) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: OES_ResetValues
    use wl_open_mi_support
    implicit none

    ! result
    integer :: retVal                                ! retVal == 0 : success

    ! arguments
    character(Len=*) , intent(in)    :: componentID  ! RR, RTC, etc.
    character(Len=*) , intent(in)    :: schemID      ! schem. file (*.fnm)
    character(Len=*) , intent(in)    :: QuantityID   ! quant. identifier
    character(Len=*) , intent(in)    :: ElementsetID ! elem.set. identifier

    ! locals
    integer                          :: modelHandle  ! handle to computation
    integer                          :: elementCount ! #elems in elementset
    double precision , pointer, &
           dimension(:)              :: values       ! values
    logical                          :: prevOverrule ! previous value of overrule

    ! body

    retVal = OES_UNDEFINED

    modelHandle = OesModelFind(trim(componentID)//'-Wrapper', schemID)
    if ( modelHandle <= 0 ) return

    elementCount = OesElmsetGetElementCount(modelHandle, ElementsetID)
    allocate(values(elementCount))
    values = oes_missingValue

    prevOverrule = overrule
    overrule = .true.
    call OesPut(modelHandle, QuantityID, ElementsetID, Values)
    overrule = prevOverrule
    deallocate(values)

    retVal = 0

end function OES_ResetValues

function OES_GetValuesElementCount(componentID, schemID, ElementsetID) result(elementCount)
    !DEC$ ATTRIBUTES DLLEXPORT :: OES_GetValuesElementCount
    use wl_open_mi_support
    implicit none

    ! result
    integer :: elementCount

    ! arguments
    character(Len=*), intent(in) :: componentID  ! RR, RTC, etc.
    character(Len=*), intent(in) :: schemID      ! schem. file (*.fnm)
    character(Len=*), intent(in) :: ElementsetID ! elem.set. identifier

    ! locals
    integer                      :: modelHandle  ! handle to computation

    ! body

    elementCount = OES_UNDEFINED

    modelHandle = OesModelFind(trim(componentID)//'-Wrapper', schemID)
    if ( modelHandle <= 0 ) return

    elementCount = OesElmsetGetElementCount(modelHandle, ElementsetID)

end function OES_GetValuesElementCount

