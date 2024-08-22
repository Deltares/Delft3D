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
!!! Oes.F90: OES - module (*O*penmi *E*ngine *S*upport)
!!!
!!! (c) Deltares, oct 2003
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!! File organization:
!!!
!!!   DECLARATIONS
!!!   - Data Type Definitions
!!!   - Overloaded Functions
!!!   - Oes Datastore
!!!   - Private Functions (Manage Oes Store)
!!!
!!!   IMPLEMENTATION
!!!
!!!   - Public Functions:
!!!
!!!     .  CREATE / DESTROY OES MODULE
!!!     .  CREATE / DESTROY / GET HANDLE TO ELEMENTSETS
!!!     .  CREATE / DESTROY / GET HANDLE TO QUANTITIES
!!!     .  PUT / GET VALUES FOR QUANT ON ELEMENTSET
!!!     .  INTERFACE TOWARDS WRAPPERS / ENGINES, PLT ADMINISTRATION FUNCTIONS
!!!     .  INTERFACE TOWARDS WRAPPERS / ENGINES, INPUT ADMINISTRATION FUNCTIONS
!!!
!!!   - Private Functions (Manage Oes Store):
!!!
!!!     .  Find / Add  Models
!!!     .  Find / Add  Quantities
!!!     .  Find / Add  Elementsets
!!!     .  Find / Add  ExchangeItems
!!!     .  Find / Add  Conversion Items
!!!     .  Support Functions
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module wl_open_mi_support

use wlopenmi_version_module

implicit none



integer, parameter :: level1 = 1          ! Top level functions and errors
integer, parameter :: level2 = 2          ! Initial quant / elmset / exchItem administration
integer, parameter :: level3 = 3          ! All Put/Get calls, detailed mapping information
integer, parameter :: level4 = 4          ! Actual Put/Get calls only
integer, parameter :: num_levels = 4

!logical, dimension(num_levels) :: doLoggingOnLevel = (/ .false., .false., .false., .false. /)
logical, dimension(num_levels) :: doLoggingOnLevel = (/ .true., .true., .true., .true. /)



integer, parameter :: oes_id_len   = 256
integer, parameter :: oes_path_len = 256


integer, parameter :: OES_UNDEFINED = -999


integer, parameter :: oes_providing = 1
integer, parameter :: oes_accepting = 2
integer, parameter :: oes_both      = 3


integer            :: oesLogHandle    = 0
character(Len=100) :: oesLogName      = 'OES_module.log'


character(Len=8), parameter  :: anySchemString = 'ANY_SCHEM'

character(Len=7), parameter :: idBasedEnumValue = 'IDBased'

character(Len=40)   :: indentedGetConnectedString = '<--OesGetConnected_Double mh ='

real               :: oes_missingValue = -999.999
logical            :: overrule = .FALSE.


!
! DATA TYPE DEFINITIONS
!


type oes_quant

    integer                                          :: qIndex

    character(Len=oes_id_len)                        :: quantID

end type oes_quant


type oes_elmset

    integer                                          :: esIndex

    ! TDR character(Len=oes_id_len)                        :: componentID
    character(Len=oes_id_len)                        :: elmsetID

    character(Len=oes_id_len), pointer, dimension(:) :: elmIDs
    double precision, dimension(:), pointer          :: Xcoords
    double precision, dimension(:), pointer          :: Ycoords

end type oes_elmset


type oes_exch_item

    integer                                 :: eiIndex

    character(Len=oes_id_len)               :: componentID  ! TODO: replace by index
    character(Len=oes_id_len)               :: quantID
    character(Len=oes_id_len)               :: elmsetID

    integer                                 :: role

    type(oes_quant) , pointer               :: quant
    type(oes_elmset), pointer               :: elmset

    integer                                 :: parentModelIndex
    integer                                 :: relatedModelIndex
    integer                                 :: relatedExchItemIndex
    integer, dimension(:), pointer          :: indexSet
    double precision, dimension(:), pointer :: inputValues

end type oes_exch_item


type oes_model

    integer                   :: mIndex

    character(Len=oes_id_len) :: componentID
    character(Len=oes_path_len) :: schemID

    type(oes_quant)     , pointer, dimension(:) :: quants
    type(oes_elmset)    , pointer, dimension(:) :: elmsets
    type(oes_exch_item) , pointer, dimension(:) :: exchItems
    integer :: numQuants
    integer :: numElmsets
    integer :: numExchItems

    logical :: core_initialized

end type oes_model


type oes_conv

    integer                        :: cIndex

    character(Len=oes_id_len)      :: sourceComponentID
    character(Len=oes_id_len)      :: sourceQuantID
    character(Len=oes_id_len)      :: sourceElmsetID

    character(Len=oes_id_len)      :: targetComponentID
    character(Len=oes_id_len)      :: targetQuantID
    character(Len=oes_id_len)      :: targetElmsetID

    type(oes_exch_item), pointer   :: sourceExchItem
    type(oes_exch_item), pointer   :: targetExchItem

    logical                        :: mapped

end type oes_conv


!
! OVERLOADED FUNCTIONS
!


interface OesElmsetFindOrCreate
    module procedure OesElmsetFindOrCreate_ByIDs
    module procedure OesElmsetFindOrCreate_ByID
    module procedure OesElmsetFindOrCreate_ByCoords
end interface


interface OesElmsetGetElementCount
    module procedure OesElmsetGetElementCount_ByHandle
    module procedure OesElmsetGetElementCount_ByID
end interface


interface OesPut
    module procedure OesPutOnID_Real
    module procedure OesPutOnID_Double
end interface


interface OesGet
    module procedure OesGetOnID_Real
    module procedure OesGetOnID_Double
end interface


interface OesGetConnected
    module procedure OesGetConnected_Real
    module procedure OesGetConnected_Double
end interface


interface OesRelate
    module procedure OesRelate_SameExchItem
    module procedure OesRelate_DiffExchItems
end interface


!
! OES DATASTORE
!

type T_OesStore

    type(oes_model)     , pointer, dimension(:) :: models
    integer :: numModels

    type(oes_conv)      , pointer, dimension(:) :: convs
    integer :: numConvs

    logical :: initialized = .false.

end type T_OesStore


type(T_OesStore), private :: OesStore


integer, parameter :: oesErrorLength = 512
character(Len=512) :: oesError


!
! PRIVATE FUNCTIONS (Manage OES store)
!

integer, parameter, private  :: MaxNumModels    =  3

integer, parameter, private  :: MaxNumQuants    = 50
integer, parameter, private  :: MaxNumElmsets   = 3000
integer, parameter, private  :: MaxNumExchItems = 12 * MaxNumElmsets
integer, parameter, private  :: MaxNumConvs     = MaxNumExchItems / 2


integer, parameter, private  :: StoreInitializedValue = 7654


private OesModelAdd
private OesModelFind_ByHandle
private OesModelFind_ByID
private OesModelCleanup

private OesQuantAdd
private OesQuantFind
private OesQuantCleanup

private OesElmsetAdd
private OesElmsetFind
private OesElmsetCleanup

private OesExchItemAdd
private OesExchItemFind
private OesExchItemCleanup

private OesConvAdd
private OesConvFind
private OesConvCleanup

private OesBuildIDBasedMapping
private OesElmsetContainsElement
private OesDoIDBasedMapping


contains

subroutine get_versionstring (versionstring)
    character(*) :: versionstring
    call getfullversionstring_wlopenmi(versionstring)
end subroutine get_versionstring

!-----------------------------------------------------------------
!-----------------------------------------------------------------
! PUBLIC FUNCTIONS
!-----------------------------------------------------------------
!-----------------------------------------------------------------


!-----------------------------------------------------------------
! CREATE / DESTROY OES MODULE
!-----------------------------------------------------------------


function OesInitialized() result(retVal)

    ! return value
    logical :: retVal

    ! body

    retVal = .false.
    if ( OESsTORE % initialized .eqv. (StoreInitializedValue > 0) ) retVal = .true.

end function OesInitialized


function doLog(level) result(doIndeed)

    ! return value
    logical :: doIndeed

    ! arguments
    integer :: level

    ! body

    doIndeed = .false.
    if (oesLogHandle .ne. 0) then
       doIndeed = doLoggingOnLevel(level)
    endif

end function doLog


function OesGetLastError() result(lastError)

    ! return value
    character(len=oesErrorLength) :: lastError

    lastError = ' ' ; if ( .not. OesInitialized() ) return

    lastError = oesError
    oesError = ' '

end function OesGetLastError


subroutine OesCreate(logName)

    ! arguments
    character(Len=*), intent(in), optional :: logName

    ! locals
    integer :: ierr

    if ( present(logName) ) then
       if (oesLogHandle == 0) then
#if (defined(DO_LOG))
          oesLogName = logName
          open(newunit=oesLogHandle, file=oesLogName, iostat=ierr)
          if ( ierr .ne. 0 ) oesLogHandle = 0
#endif
       endif
    endif

    if ( OesInitialized() ) return

    if(doLog(level1)) write(oesLogHandle, '(A)') 'OesCreate'

    allocate(oesStore % models(MaxNumModels) )
    allocate(oesStore % convs (MaxNumConvs ) )

    oesStore % models(:) % mIndex = OES_UNDEFINED
    oesStore % convs (:) % cIndex = OES_UNDEFINED

    oesStore % numModels    = 0
    oesStore % numConvs     = 0

    oesStore % initialized = StoreInitializedValue > 0

end subroutine OesCreate


subroutine OesDestroy()
    ! locals
    integer :: i

    ! body

    if ( .not. OesInitialized() ) return

    if(doLog(level1)) write(oesLogHandle, '(A)') 'OesDestroy'
    if(doLog(level1)) call flush(oesLogHandle)

    do i = 1 , oesStore % numModels
        call OesModelCleanup(oesStore % models(i))
    enddo
    deallocate(oesStore % models)

    do i = 1 , oesStore % numConvs
        call OesConvCleanup(oesStore % convs(i))
    enddo
    deallocate(oesStore % convs )

    oesStore % numModels      = 0
    oesStore % numConvs       = 0

    OesStore % initialized    = .false.

    if (oesLogHandle .ne. 0) then
        call flush(oesLogHandle)
        close(oesLogHandle)
        oesLogHandle = 0
    endif

end subroutine OesDestroy


!-----------------------------------------------------------------
! CREATE / DESTROY / GET HANDLE TO MODEL-(=COMP./SCHEM.COMBI
!-----------------------------------------------------------------


function OesModelFindOrCreate(componentID, schemID) result(modelHandle)

    ! return value
    integer                      :: modelHandle

    ! arguments
    character(Len=*), intent(in) :: componentID
    character(Len=*), intent(in) :: schemID

    ! locals
    type(oes_model), pointer     :: model

    ! body

    ! provide valid handle by default
    modelHandle = 1

    if ( .not. OesInitialized() ) return

    ! find or create model handle
    modelHandle = OES_UNDEFINED
    model => OesModelFind_ByID(componentID, schemID)

    if ( .not. associated(model) ) then
        model => OesModelAdd(componentID, schemID)
    endif

    if ( associated(model) ) modelHandle = model % mIndex

end function OesModelFindOrCreate


function OesModelFind(componentID, schemID) result(modelHandle)

    ! return value
    integer                      :: modelHandle

    ! arguments
    character(Len=*), intent(in) :: componentID
    character(Len=*), intent(in) :: schemID

    ! locals
    type(oes_model), pointer     :: model

    ! body

    modelHandle = OES_UNDEFINED

    if ( .not. OesInitialized() ) return

    model => OesModelFind_ByID(componentID, schemID)
    if ( associated(model) ) then
        modelHandle = model % mIndex
    endif

end function OesModelFind


!-----------------------------------------------------------------
! CREATE / DESTROY / GET HANDLE TO QUANTITIES
!-----------------------------------------------------------------


function OesQuantFindOrCreate(modelHandle, quantID) result(qIndex)

    ! return value
    integer :: qIndex

    ! arguments
    integer         , intent(in) :: modelHandle
    character(Len=*), intent(in) :: quantID

    ! locals
    type(oes_model), pointer     :: model
    type(oes_quant), pointer     :: quant

    ! body

    qIndex = OES_UNDEFINED ; if ( .not. OesInitialized() ) return

    model => OesModelFind_ByHandle(modelHandle)
    if ( associated(model) ) then
        quant => OesQuantFind(model, quantID)
        if ( .not. associated(quant) ) then
            quant => OesQuantAdd(model, quantID)
        endif
    endif

    if ( associated(quant) ) qIndex = quant % qIndex

end function OesQuantFindOrCreate


function OesQuantGetCount(componentID) result(qCount)

    ! return value
    integer :: qCount

    ! arguments
    character(Len=*), intent(in) :: componentID

    ! locals
    type(oes_model), pointer :: model

    ! body

    qCount = OES_UNDEFINED ; if ( .not. OesInitialized() ) return

    model => OesModelFind_ByID(componentID, anySchemString )
    if ( associated(model) ) then
        qCount = model % numQuants
    endif

end function OesQuantGetCount


function OesQuantGetID(componentID, qIndex) result(quantID)

    ! return value
    character(Len=oes_id_len)    :: quantID

    ! arguments
    integer, intent(in)          :: qIndex
    character(Len=*), intent(in) :: componentID

    ! locals
    type(oes_model), pointer     :: model

    ! body

    quantID = ' ' ; if ( .not. OesInitialized() ) return

    model => OesModelFind_ByID(componentID, anySchemString)
    if ( associated(model) ) then
        if ( qIndex > 0 .and. qIndex <= model % numQuants ) then
            quantID = model % quants (qIndex) % quantID
        endif
    endif

end function OesQuantGetID


subroutine OesQuantDestroy(modelHandle, quantID)

    ! arguments
    integer         , intent(in) :: modelHandle
    character(Len=*), intent(in) :: quantID

    ! locals
    type(oes_model), pointer     :: model
    type(oes_quant), pointer     :: quant

    ! body

    if ( .not. OesInitialized() ) return

    model => OesModelFind_ByHandle(modelHandle)

    if ( associated(model) ) then
        quant => OesQuantFind(model, quantID)
        if ( associated(quant) ) then
            call OesQuantCleanup(quant)
        endif
    endif

end subroutine OesQuantDestroy


!-----------------------------------------------------------------
! CREATE / DESTROY / GET HANDLE TO ELEMENTSETS
!-----------------------------------------------------------------


function OesElmsetFindOrCreate_ByIDs(modelHandle, elmsetID, elmIDs) result(esIndex)

    ! return value
    integer :: esIndex

    ! arguments
    integer,                        intent(in)  :: modelHandle
    character(Len=*),               intent(in)  :: elmsetID
    character(Len=*), dimension(:), intent(in)  :: elmIDs

    ! locals
    type(oes_model) , pointer                   :: model
    type(oes_elmset), pointer                   :: elmset
    integer                                     :: i

    ! body

    esIndex = OES_UNDEFINED ; if ( .not. OesInitialized() ) return

    nullify(elmset)

    model => OesModelFind_ByHandle(modelHandle)
    if ( associated(model) ) then
        if ( size(elmIDS) > 0 ) then
            elmset => OesElmsetFind(model, elmsetID)
            if ( .not. associated(elmset) ) then
                elmset => OesElmsetAdd(model, elmsetID)
                if ( associated(elmset) ) then
                    allocate(elmSet % elmIDs(size(elmIDs)))
                    elmSet % elmIDs  = elmIDs
                    if(doLog(level2)) then
                        write(oesLogHandle, '(A,I2,2A)') 'OesElmsetFindOrCreate_ByIDs:  mh =', &
                                                    modelHandle, ': ', trim(elmsetID)
                        do i = 1, size(elmIDs)
                            write(oesLogHandle, '(A,I5,1X,A)') '   ', i, trim(elmIDs(i))
                        enddo
                        call flush(oesLogHandle)
                    endif
                endif
            endif
        endif
    endif

    if ( associated(elmset) ) esIndex = elmset % esIndex

end function OesElmsetFindOrCreate_ByIDs


function OesElmsetFindOrCreate_ByID(modelHandle, elmsetID) result(esIndex)

    ! return value
    integer :: esIndex

    ! arguments
    integer         , intent(in)  :: modelHandle
    character(Len=*), intent(in)  :: elmsetID

    ! locals
    type(oes_model) , pointer                   :: model
    type(oes_elmset), pointer                   :: elmset

    ! body

    esIndex = OES_UNDEFINED ; if ( .not. OesInitialized() ) return

    nullify(elmset)

    model => OesModelFind_ByHandle(modelHandle)
    if ( associated(model) ) then
        elmset => OesElmsetFind(model, elmsetID)
        if ( .not. associated(elmset) ) then
            elmset => OesElmsetAdd(model, elmsetID)
            if ( associated(elmset) ) then
                allocate(elmSet % elmIDs(1))
                elmSet % elmIDs(1) = elmsetID
                if(doLog(level2)) then
                    write(oesLogHandle, '(A,I2,2A)') 'OesElmsetFindOrCreate_ByID:  mh =', &
                                                modelHandle, ': ', trim(elmsetID)
                    write(oesLogHandle, '(A,I5,1X,A)') '   ', 1, trim(elmsetID)
                    call flush(oesLogHandle)
                endif
            endif
        endif
    endif

    if ( associated(elmset) ) esIndex = elmset % esIndex

end function OesElmsetFindOrCreate_ByID



function OesElmsetFindOrCreate_ByCoords(modelHandle, elmsetID, XCoords, YCoords) result(esIndex)

    ! return value
    integer :: esIndex

    ! arguments
    integer         ,               intent(in)  :: modelHandle
    character(Len=*),               intent(in)  :: elmsetID
    double precision, dimension(:), intent(in)  :: XCoords
    double precision, dimension(:), intent(in)  :: YCoords

    ! locals
    type(oes_model) , pointer                   :: model
    type(oes_elmset), pointer                   :: elmset

    ! body

    esIndex = OES_UNDEFINED ; if ( .not. OesInitialized() ) return

    nullify(elmset)

    model => OesModelFind_ByHandle(modelHandle)
    if ( associated(model) ) then
        elmset => OesElmsetFind(model, elmsetID)
        if ( .not. associated(elmset) ) then
            elmset => OesElmsetAdd(model, elmsetID)
            if ( associated(elmset) ) then
                allocate(elmSet % Xcoords(size(XCoords)))
                allocate(elmSet % Ycoords(size(YCoords)))
                elmSet % Xcoords = XCoords
                elmSet % Ycoords = YCoords
            endif
        endif
    endif

    if ( associated(elmset) ) esIndex = elmset % esIndex

end function OesElmsetFindOrCreate_ByCoords


function OesElmtypeGetCount(componentID, schemID) result(etCount)

    ! return value
    integer :: etCount

    ! arguments
    character(Len=*), intent(in) :: componentID
    character(Len=*), intent(in) :: schemID

    ! locals
    type(oes_model), pointer     :: model

    ! body

    etCount = OES_UNDEFINED ; if ( .not. OesInitialized() ) return

    model => OesModelFind_ByID(componentID, schemID)
    if ( associated(model) ) then
        etCount = 1
    endif


end function OesElmtypeGetCount


function OesElmtypeGetID(componentID, schemID, etIndex) result(elmTypeAsString)

    ! return value
    character(Len=oes_id_len)    :: elmTypeAsString

    ! arguments
    character(Len=*), intent(in) :: componentID
    character(Len=*), intent(in) :: schemID
    integer, intent(in)          :: etIndex

    ! locals
    type(oes_model), pointer     :: model

    ! body

    elmTypeAsString = ' ' ; if ( .not. OesInitialized() ) return

    model => OesModelFind_ByID(componentID, schemID)
    if ( associated(model) ) then
        if ( etIndex == 1) then
            elmTypeAsString = idBasedEnumValue
        endif
    endif

end function OesElmtypeGetID


function OesElmsetGetCount(componentID, schemID, elementType) result(esCount)

    ! return value
    integer :: esCount

    ! arguments
    character(Len=*), intent(in) :: componentID
    character(Len=*), intent(in) :: schemID
    character(len=*), intent(in) :: elementType    ! elementType identifier

    ! locals
    type(oes_model), pointer     :: model

    ! body

    esCount = OES_UNDEFINED ; if ( .not. OesInitialized() ) return

   model => OesModelFind_ByID(componentID, schemID)
   if ( associated(model) ) then
      if ( elementType == idBasedEnumValue ) then
         esCount = model % numElmsets
      endif
   endif

end function OesElmsetGetCount


function OesElmsetGetID(componentID, schemID, elementType, esIndex) result(elmsetID)

    ! return value
    character(Len=oes_id_len)    :: elmsetID

    ! arguments
    character(Len=*), intent(in) :: componentID
    character(Len=*), intent(in) :: schemID
    character(len=*), intent(in) :: elementType    ! elementType identifier
    integer, intent(in)          :: esIndex

    ! locals
    type(oes_model), pointer     :: model

    ! body

    elmsetID = ' ' ; if ( .not. OesInitialized() ) return

   model => OesModelFind_ByID(componentID, schemID)
   if ( associated(model) ) then
      if ( elementType == idBasedEnumValue ) then
         if ( esIndex > 0 .and. esIndex <= model % numElmsets ) then
            elmsetID = model % elmsets(esIndex) % elmsetID
         endif
      endif
   endif

end function OesElmsetGetID


function OesElmsetGetElementCount_ByHandle(modelHandle, elmsetID) result(numElems)

    ! return value
    integer :: numElems

    ! arguments
    integer         , intent(in) :: modelHandle
    character(Len=*), intent(in) :: elmsetID

    ! locals
    type(oes_model) , pointer    :: model
    type(oes_elmset), pointer    :: elmset

    ! body

    numElems = OES_UNDEFINED ; if ( .not. OesInitialized() ) return

    model => OesModelFind_ByHandle(modelHandle)
    if ( associated(model) ) then
        elmset => OesElmsetFind(model, elmsetID)
        if ( associated(elmset) ) then
            if ( associated( elmset % elmIDs ) ) then
                numElems = size(elmset % elmIDs)
            else if ( associated(elmset % XCoords) ) then
                if ( associated(elmset % YCoords) ) then
                    if ( size(elmset % XCoords) == size(elmset % YCoords) ) then
                        numElems = size(elmset % XCoords)
                    endif
                endif
            else
                numElems = 0
            endif
        endif
    endif

end function OesElmsetGetElementCount_ByHandle


function OesElmsetGetElementCount_ByID(componentID, schemID, elmsetID) result(numElems)

    ! return value
    integer :: numElems

    ! arguments
    character(Len=*), intent(in) :: componentID
    character(Len=*), intent(in) :: schemID
    character(Len=*), intent(in) :: elmsetID

    ! locals
    type(oes_model) , pointer    :: model
    type(oes_elmset), pointer    :: elmset

    ! body

    numElems = OES_UNDEFINED ; if ( .not. OesInitialized() ) return

    model => OesModelFind_ByID(componentID, schemID)
    if ( associated(model) ) then
        elmset => OesElmsetFind(model, elmsetID)
        if ( associated(elmset) ) then
            if ( associated( elmset % elmIDs ) ) then
                numElems = size(elmset % elmIDs)
            else if ( associated(elmset % XCoords) ) then
                if ( associated(elmset % YCoords) ) then
                    if ( size(elmset % XCoords) == size(elmset % YCoords) ) then
                        numElems = size(elmset % XCoords)
                    endif
                endif
            else
                numElems = 0
            endif
        endif
    endif

end function OesElmsetGetElementCount_ByID


function OesElmsetGetElmID(componentID, schemID, elmsetID, elementIndex) result(elementID)

    ! return value
    character(Len=oes_id_len)    :: elementID

    ! arguments
    character(Len=*), intent(in) :: componentID
    character(Len=*), intent(in) :: schemID
    character(Len=*), intent(in) :: elmsetID
    integer         , intent(in) :: elementIndex

    ! locals
    character(Len=oes_id_len), pointer, dimension(:) :: elements

    ! body

    elementID = ' ' ; if ( .not. OesInitialized() ) return

    elements => OesElmsetGetElmIDs(componentID, schemID, elmsetID)
    if ( associated(elements) ) then
        if ( elementIndex > 0 .and. elementIndex <= size(elements) ) then
            elementID = elements(elementIndex)
        else
            write(oesError,'(A,I0)') 'OesElmsetGetElmID(' // trim(elmsetID) // '): invalid index ', elementIndex
        endif
    else
         write(oesError,'(A,I0)') 'OesElmsetGetElmID(' // trim(elmsetID) // '): has no elements'
    endif

end function OesElmsetGetElmID


function OesElmsetGetElmIDs(componentID, schemID, elmsetID) result(elems)

    ! return value
    character(Len=oes_id_len), pointer, dimension(:) :: elems

    ! arguments
    character(Len=*), intent(in) :: componentID
    character(Len=*), intent(in) :: schemID
    character(Len=*), intent(in) :: elmsetID

    ! locals
    type(oes_model) , pointer    :: model
    type(oes_elmset), pointer    :: elmset

    ! body

    nullify(elems) ; if ( .not. OesInitialized() ) return

    model => OesModelFind_ByID(componentID, schemID)
    if ( associated(model) ) then
        elmset => OesElmsetFind(model, elmsetID)
        if ( associated(elmset) ) then
            if ( associated( elmset % elmIDs ) ) then
                elems => elmset % elmIDs
            else
                oesError = 'OesElmsetGetElmIDs: ' // trim(elmsetID) // ' has no elements'
            endif
        else
            oesError = 'OesElmsetGetElmIDs: ' // trim(elmsetID) // ' not found'
        endif
    endif

end function OesElmsetGetElmIDs


function OesElmsetGetElementVertexCount(elementIndex, model, schematization, elementSet) result(retVal)

    ! return value

    integer                       :: retVal         ! >=0 : #items; <0 : Error

    ! arguments

    integer         , intent(in)  :: elementIndex   ! element index in element set
    character(len=*), intent(in)  :: model          ! model identifier
    character(len=*), intent(in)  :: schematization ! schematization identifier
    character(len=*), intent(in)  :: elementSet     ! elementSet identifier

    ! body

    ! write(oesError, '(A)') 'OesElmsetGetElementVertexCount(' // &
    !                        trim(model) // ', ' // trim(schematization) // ', ' &
    !                        // trim(elementSet) //  ', ' //  trim(element) // '): not implemented'
    retVal = 0 ! TODO handle as error???

end function OesElmsetGetElementVertexCount


function OesElmsetGetElementCoordinate(elementIndex, vertexIndex, model, schematization, elementSet, axis) result(retVal)

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

    write(oesError, '(A,I0,A,I0,A,I0,A)') 'OesElmsetGetElementCoordinate(' // &
                                   trim(model) // ', ' // trim(schematization) // ', '  &
                                   // trim(elementSet) // ', ', elementIndex , ', '  &
                                   , vertexIndex, ', ', axis, '): not implemented'
    retVal = -1

end function OesElmsetGetElementCoordinate


subroutine OesElmsetDestroy(modelHandle, elmsetID)

    ! arguments
    integer         , intent(in) :: modelHandle
    character(Len=*), intent(in) :: elmsetID

    ! locals
    type(oes_model) , pointer    :: model
    type(oes_elmset), pointer    :: elmset

    ! body

    if ( .not. OesInitialized() ) return

    model => OesModelFind_ByHandle(modelHandle)
    if ( associated(model) ) then
        elmset => OesElmsetFind(model, elmsetID)
        if ( associated(elmset) ) then
            call OesElmsetCleanup(elmset)
        endif
    endif

end subroutine OesElmsetDestroy


!-----------------------------------------------------------------
! CREATE / DESTROY / GET HANDLE TO EXCHANGEITEMS
!-----------------------------------------------------------------


function OesExchItemCreate(modelHandle, quantID, elmsetID, role) result(eiIndex)

    ! return value
    integer :: eiIndex

    ! arguments
    integer         , intent(in) :: modelHandle
    character(Len=*), intent(in) :: quantID
    character(Len=*), intent(in) :: elmsetID
    integer,          intent(in) :: role     !(oes_providing|oes_accepting)

    ! locals
    type(oes_model)    , pointer :: model
    type(oes_exch_item), pointer :: exchItem

    ! body

    eiIndex = OES_UNDEFINED ; if ( .not. OesInitialized() ) return

    nullify(exchItem)

    model => OesModelFind_ByHandle(modelHandle)
    if ( associated(model) ) then
        exchItem => OesExchItemFind(model, quantID, elmsetID, role)
        if ( associated(exchItem) ) then
            if(doLog(level1)) then
                write(oesLogHandle, '(A,I0,5A,I2,A,I2)') 'ERROR: OesExchItem mh= ', &
                                        modelHandle, ' (', &
                                        trim(quantID), '/', trim(elmsetID), ') role=', role, &
                                        ' already defined for modelHandle=', modelHandle
                call flush(oesLogHandle)
            endif
            ! TODO: produce error
        else
            exchItem => OesExchItemAdd(model, quantID, elmsetID, role)
            if(doLog(level2)) write(oesLogHandle, '(A,I0,5A,I2)') 'OesExchItem mh= ', &
                                    modelHandle, ' (', &
                                    trim(quantID), '/', trim(elmsetID), ') role=', role
                call flush(oesLogHandle)
        endif
    endif

    if ( associated(exchItem) ) then
        eiIndex = exchItem % eiIndex
        call OesAnalyzeRelations(.false.)
    endif

end function OesExchItemCreate


function OesExchItemGetCount(componentID, schemID) result(eiCount)

    ! return value
    integer :: eiCount

    ! arguments
    character(Len=*), intent(in) :: componentID
    character(Len=*), intent(in) :: schemID

    ! locals
    type(oes_model), pointer     :: model

    ! body

    eiCount = OES_UNDEFINED ; if ( .not. OesInitialized() ) return

    model => OesModelFind_ByID(componentID, schemID)
    if ( associated(model) ) then
        eiCount = model % numExchItems
    endif

end function OesExchItemGetCount


function OesExchItemGetRole(componentID, schemID, quantID, elmsetID) result(role)

    ! return value
    integer                       :: role     !(oes_providing|oes_accepting)

    ! arguments
    character(Len=*), intent(in) :: componentID
    character(Len=*), intent(in) :: schemID
    character(Len=*), intent(in) :: quantID
    character(Len=*), intent(in) :: elmsetID

    ! locals
    type(oes_model), pointer      :: model
    type(oes_exch_item), pointer  :: exchItem
    logical                       :: accepts, provides

    ! body

    role = OES_UNDEFINED ; if ( .not. OesInitialized() ) return

    accepts  = .false.
    provides = .false.

    model => OesModelFind_ByID(componentID, schemID)
    if ( associated(model) ) then
        exchItem => OesExchItemFind(model, quantID, elmsetID, oes_accepting)
        if ( associated(exchItem) ) accepts  = .true.
        exchItem => OesExchItemFind(model, quantID, elmsetID, oes_providing)
        if ( associated(exchItem) ) provides = .true.

        if ( provides .and. accepts ) then
            role = oes_both
        else if ( provides ) then
            role = oes_providing
        else if ( accepts ) then
            role = oes_accepting
        endif
    endif

end function OesExchItemGetRole


subroutine OesExchItemDestroy(modelHandle, quantID, elmsetID, role)

    ! arguments
    integer,          intent(in) :: modelHandle
    character(Len=*), intent(in) :: quantID
    character(Len=*), intent(in) :: elmsetID
    integer,          intent(in) :: role     !(oes_providing|oes_accepting)

    ! locals
    type(oes_model)    , pointer   :: model
    type(oes_exch_item), pointer   :: exchItem


    ! body

    if ( .not. OesInitialized() ) return

    model => OesModelFind_ByHandle(modelHandle)
    if ( associated(model) ) then
        exchItem => OesExchItemFind(model, quantID, elmsetID, role)
        if ( associated(exchItem) ) then
            call OesExchItemCleanup(exchItem)
        endif
    endif

end subroutine OesExchItemDestroy


!-----------------------------------------------------------------
! PUT / GET VALUES FOR QUANT ON ELEMENTSET
!-----------------------------------------------------------------


subroutine OesPutOnID_Real(modelHandle, quantID, elmsetID, values)

    ! arguments
    integer         ,   intent(in) :: modelHandle
    character(Len=*),   intent(in) :: quantID
    character(Len=*),   intent(in) :: elmsetID
    real, dimension(:), intent(in) :: values

    ! locals
    double precision, dimension(:), allocatable:: dValues

    ! body

    if ( .not. OesInitialized() ) return

    if(doLog(level3)) write(oesLogHandle, '(A,I2, 5A)') '->OesPutOnID_Real mh =', &
                     modelHandle, ' (',  trim(quantID), '/', trim(elmsetID), ')'
    if(doLog(level3)) call flush(oesLogHandle)

    allocate(dValues(size(values)))
    dValues=values
    call OesPutOnID_Double(modelHandle, quantID, elmsetID, dValues)
    deallocate(dValues)

end subroutine OesPutOnID_Real


subroutine OesPutOnID_Double(modelHandle, quantID, elmsetID, values)

    ! arguments
    integer         , intent(in)               :: modelHandle
    character(Len=*), intent(in)               :: quantID
    character(Len=*), intent(in)               :: elmsetID
    double precision, dimension(:), intent(in) :: values

    ! locals
    type(oes_model)    , pointer :: model
    type(oes_exch_item), pointer :: sourceExchItem
    integer                      :: i, numValues

    ! body

    if ( .not. OesInitialized() ) return

    if(doLog(level3)) write(oesLogHandle, '(A,I2,5A)') '-->OesPutOnID_Double mh =',  &
                            modelHandle, ' (',  trim(quantID), '/', trim(elmsetID), ')'
    if(doLog(level3)) call flush(oesLogHandle)

    model => OesModelFind_ByHandle(modelHandle)
    if ( associated(model) ) then
        sourceExchItem => OesExchItemFind(model, quantID, elmsetID, oes_providing)
        if ( associated(sourceExchItem) ) then
            if ( associated(sourceExchItem % inputValues ) ) then
                numValues = size(values)
                if ( numValues == size(sourceExchItem % inputValues) ) then
                   if(doLog(level4).and. .not. doLog(level3))                         &
                       write(oesLogHandle, '(A,I2,5A)') '-->OesPut mh =',  &
                             modelHandle, ' (',  trim(quantID), '/', trim(elmsetID), ')'
                   do i = 1, numValues
                       if ( ( .not. OesDoublesEqual (values(i), -999.0D+0  ) ) .and. &
                            ( .not. OesDoublesEqual (values(i), -9.9999D+0 ) ) .or.  &
                            ( overrule )    ) then
                           sourceExchItem % inputValues(i) = values(i)
                           if(doLog(level4)) write(oesLogHandle, *) '     ', i, ': ', values(i)
                       endif
                   enddo
                    if(doLog(level4)) call flush(oesLogHandle)
                else
                    if(doLog(level1)) write(oesLogHandle, '(A)') &
                            '      OesPutOnID_Double: inputValues wrong size'
                    if(doLog(level1)) call flush(oesLogHandle)
                endif
            else
                if(doLog(level3)) write(oesLogHandle, '(A)') &
                        '      OesPutOnID_Double: parameter not required by other process(es)'
                if(doLog(level3)) call flush(oesLogHandle)
            endif
        else
            if(doLog(level1) .and. .not. doLog(level3))                              &
                      write(oesLogHandle, '(A,I2,5A)') '-->OesPutOnID_Double mh =',  &
                            modelHandle, ' (',  trim(quantID), '/', trim(elmsetID), ')'
            if(doLog(level1)) write(oesLogHandle, '(A)') &
                    '      OesPutOnID_Double: sourceExchItem not found'
        endif
    else
        if(doLog(level1)) write(oesLogHandle, '(A)') &
                '      OesPutOnID_Double: model not found'
        if(doLog(level1)) call flush(oesLogHandle)
    endif

end subroutine OesPutOnID_Double


function OesGetOnID_Real(modelHandle, quantID, elmsetID, values) result(success) ! , mask) result(success)

    ! return value
    logical                         :: success

    ! arguments
    integer           , intent(in)  :: modelHandle
    character(Len=*)  , intent(in)  :: quantID
    character(Len=*)  , intent(in)  :: elmsetID
    real, dimension(:), intent(out) :: values

    ! locals
    double precision, dimension(:), allocatable:: dValues

    ! body

    success = .false. ; values = oes_missingValue; if ( .not. OesInitialized() ) return

    allocate(dValues(size(values)))
    success = OesGetOnID_Double(modelHandle, quantID, elmsetID, dValues)
    if (success) values = dValues
    deallocate(dValues)

    if(doLog(level3)) write(oesLogHandle, '(A,I2,5A)') '<-OesGetOnID_Real mh =', &
                     modelHandle, ' (',  trim(quantID), '/', trim(elmsetID), ')'
    if(doLog(level3)) call flush(oesLogHandle)

end function OesGetOnID_Real


function OesGetOnID_Double(modelHandle, quantID, elmsetID, values) result(success)

    ! return value
    logical                                     :: success

    ! arguments
    integer         , intent(in)                :: modelHandle
    character(Len=*), intent(in)                :: quantID
    character(Len=*), intent(in)                :: elmsetID
    double precision, dimension(:), intent(out) :: values

    ! locals
    logical, dimension(:), allocatable          :: mask
    integer                                     :: numElems

    ! body

    success = .false. ; values = oes_missingValue; if ( .not. OesInitialized() ) return

    allocate(mask(size(values)))
    indentedGetConnectedString = '  <--OesGetConnected_Double mh ='
    numElems = OesGetConnected_Double(modelHandle, quantID, elmsetID, values, mask)
    indentedGetConnectedString = '<--OesGetConnected_Double mh ='
    deallocate(mask)
    if ( numElems == size(values) ) then
        success = .true.
    else
        ! write(*,*) '      OesGetOnID_Double, ERROR: #retrieved /= size(values)'
        success = .true.  ! TODO: Check
    endif

    if(doLog(level3)) write(oesLogHandle, '(A,I2,5A)') '<--OesGetOnID_Double mh =', &
                     modelHandle, ' (',  trim(quantID), '/', trim(elmsetID), ')'
    if(doLog(level3)) call flush(oesLogHandle)

end function OesGetOnID_Double


!-----------------------------------------------------------------
! INTERFACE TOWARDS WRAPPERS / ENGINES, ADMINISTRATION FUNCTIONS
!-----------------------------------------------------------------


subroutine OesRelate_SameExchItem(  sourceComponentID, &
                                    sourceQuantID    , &
                                    sourceElmsetID   , &
                                    targetComponentID   )

    ! arguments
    character(Len=*), intent(in) :: sourceComponentID
    character(Len=*), intent(in) :: sourceQuantID
    character(Len=*), intent(in) :: sourceElmsetID
    character(Len=*), intent(in) :: targetComponentID

    ! body

    if ( .not. OesInitialized() ) return

    call OesRelate_DiffExchItems(   sourceComponentID, sourceQuantID, sourceElmsetID, &
                                    targetComponentID, sourceQuantID, sourceElmsetID  )

end subroutine OesRelate_SameExchItem


subroutine OesRelate_DiffExchItems( sourceComponentID, &
                                    sourceQuantID    , &
                                    sourceElmsetID   , &
                                    targetComponentID, &
                                    targetQuantID    , &
                                    targetElmsetID      )


    ! arguments
    character(Len=*), intent(in) :: sourceComponentID
    character(Len=*), intent(in) :: sourceQuantID
    character(Len=*), intent(in) :: sourceElmsetID
    character(Len=*), intent(in) :: targetComponentID
    character(Len=*), intent(in) :: targetQuantID
    character(Len=*), intent(in) :: targetElmsetID

    ! locals
    type(oes_conv), pointer :: conv

    ! body

    if ( .not. OesInitialized() ) return

    conv => OesConvAdd(sourceComponentID, sourceQuantID, sourceElmsetID, &
                                targetComponentID, targetQuantID, targetElmsetID )

    if ( associated(conv) ) then
        call OesAnalyzeRelations(.false.)
    endif

end subroutine OesRelate_DiffExchItems


!-----------------------------------------------------------------
! INTERFACE TOWARDS WRAPPERS / ENGINES, INPUT ADMINISTRATION FUNCTIONS
!-----------------------------------------------------------------


function OesGetConnected_Real(modelHandle, quantID, elmsetID, values, mask) result(numElems)

    ! returnvalue
    integer :: numElems

    ! arguments
    integer         , intent(in)       :: modelHandle
    character(Len=*), intent(in)       :: quantID
    character(Len=*), intent(in)       :: elmsetID
    real, intent(out), dimension(:)    :: values
    logical, intent(out), dimension(:) :: mask

    ! locals
    double precision, dimension(:), allocatable :: dValues

    ! body

    numElems = 0
    values   = oes_missingValue
    mask     = .false.

    if ( .not. OesInitialized() ) return

    allocate(dValues(size(values)))
    numElems = OesGetConnected_Double(modelHandle, quantID, elmsetID, dValues, mask)
    if ( numElems > 0 ) values = dValues
    deallocate(dValues)

    if(doLog(level3)) write(oesLogHandle, '(A,I2,5A,I4)') '<-OesGetConnected_Real mh =',    &
                     modelHandle, ' (',  trim(quantID), '/', trim(elmsetID), &
                     '): #numElems:' , numElems

    if((doLog(level3)) .and. numElems > 0 ) write(oesLogHandle, *) '       mask: ',  mask
    if(doLog(level3)) call flush(oesLogHandle)

end function OesGetConnected_Real


function OesGetConnected_Double(modelHandle, quantID, elmsetID, values, mask) result(numElems)

    ! returnvalue
    integer :: numElems

    ! arguments
    integer         , intent(in)                :: modelHandle
    character(Len=*), intent(in)                :: quantID
    character(Len=*), intent(in)                :: elmsetID
    double precision, intent(out), dimension(:) :: values
    logical, intent(out), dimension(:)          :: mask

    ! locals
    type(oes_model)    , pointer                :: model
    type(oes_exch_item), pointer                :: targetExchItem

    ! body

    numElems = 0
    values   = 0.0D+00
    mask     = .false.

    if ( .not. OesInitialized() ) return

    model => OesModelFind_ByHandle(modelHandle)
    if ( associated(model) ) then
        targetExchItem => OesExchItemFind(model, quantID, elmsetID, oes_accepting)
        if ( associated( targetExchItem) ) then
            if ( associated( targetExchItem % elmset ) ) then
                if ( targetExchItem % elmset % elmsetID == elmsetID ) then
                    if ( ( targetExchItem % relatedModelIndex    /= OES_UNDEFINED ) .and. &
                         ( targetExchItem % relatedExchItemIndex /= OES_UNDEFINED )       ) then
                        numElems = OesDoIDBasedMapping(modelHandle, targetExchItem,             &
                            oesStore % models(targetExchItem % relatedModelIndex) %             &
                                            exchItems(targetExchItem % relatedExchItemIndex), &
                                            values, mask)
                    endif
                endif
            endif
        endif
    endif

    if(doLog(level3)) write(oesLogHandle, '(A,I2,5A,I4)') '<--OesGetConnected_Double mh =', &
                     modelHandle, ' (',  trim(quantID), '/', trim(elmsetID), &
                     '): #numElems:' , numElems
    if(doLog(level3)) call flush(oesLogHandle)

end function OesGetConnected_Double


!-----------------------------------------------------------------
!-----------------------------------------------------------------
! PRIVATE FUNCTIONS (Manage OES store)
!-----------------------------------------------------------------
!-----------------------------------------------------------------


!
! Find / add models
!

function OesModelAdd(componentID, schemID) result(model)

    ! return value
    type(oes_model) , pointer    :: model

    ! arguments
    character(Len=*), intent(in) :: componentID
    character(Len=*), intent(in) :: schemID

    nullify(model) ; if ( .not. OesInitialized() ) return

    if ( oesStore % numModels >= MaxNumModels ) then

        write(oesError, '(A,I4,A)') 'modelCreate: max #models (', MaxNumModels, ') exceeded'
        if(doLog(level1)) write(oesLogHandle, '(A)') OesGetLastError()
        if(doLog(level1)) call flush(oesLogHandle)

    else

        oesStore % numModels = oesStore % numModels + 1
        model => oesStore % models(oesStore % numModels)
        model % mIndex  = oesStore % numModels

        
        model % componentID = componentID
        model % schemID     = schemID
        
        call big(model%componentID)
        call small(model%schemID)

        allocate(model % quants    (MaxNumQuants)    )
        allocate(model % elmsets   (MaxNumElmsets)    )
        allocate(model % exchItems (MaxNumExchItems) )

        model % quants (:)    % qIndex    = OES_UNDEFINED
        model % elmsets(:)    % esIndex   = OES_UNDEFINED
        model % exchItems (:) % eiIndex   = OES_UNDEFINED

        model % numQuants        = 0
        model % numElmsets       = 0
        model % numExchItems     = 0

        model % core_initialized = .false.

        if(doLog(level1)) write(oesLogHandle, '(A,I2,4A)') &
                    'modelCreate (handle=', model % mIndex, '): ', &
                    trim(componentID), '-', trim(schemID)
        if(doLog(level1)) call flush(oesLogHandle)

    endif

end function OesModelAdd


function OesModelFind_ByHandle(modelHandle)  result(model)

    ! return value
    type(oes_model) , pointer  :: model

    ! arguments
    integer, intent(in)        :: modelHandle

    ! body

    nullify(model) ; if ( .not. OesInitialized() ) return

    if ( ( modelHandle > 0 ) .and. ( modelHandle <= oesStore % numModels) ) then
        if ( oesStore % models(modelHandle) % mIndex == modelHandle ) then
            model => oesStore % models(modelHandle)
        endif
    endif

end function OesModelFind_ByHandle


function OesModelFind_ByID(componentID, schemID)  result(model)

    ! return value
    type(oes_model) , pointer    :: model

    ! arguments
    character(Len=*), intent(in) :: componentID
    character(Len=*), intent(in) :: schemID

    ! locals
    integer :: m
    character(Len=oes_id_len)   :: component_id
    character(Len=oes_path_len) :: schem_id

    ! body

    nullify(model)
    if ( .not. OesInitialized() ) return
    
    component_id = componentID
    schem_id     = schemID
    
    call big(component_id)
    call small(schem_id)

    do m = 1 , oesStore % numModels
        if ( oesStore % models(m) % mIndex /= OES_UNDEFINED ) then
            if ( ( component_id == oesStore % models(m) % componentID ) .and. &
                 ( schem_id     == oesStore % models(m) % schemId    )        ) then
                model => oesStore % models(m)
                exit
            else if ( ( component_id == oesStore % models(m) % componentId ) .and. &
                      ( schem_id     == anySchemString                     )        ) then
                model => oesStore % models(m)
                exit
            endif
        endif
    enddo

end function OesModelFind_ByID


subroutine OesModelCleanup(model)

    ! arguments
    type(oes_model), intent(inout) :: model

    ! locals
    integer :: i

    ! body

    if ( .not. OesInitialized() ) return

    if(doLog(level1)) write(oesLogHandle, '(A)') 'OesDestroy'
    if(doLog(level1)) call flush(oesLogHandle)

    do i = 1 , model % numQuants
        call OesQuantCleanup(model % quants(i))
    enddo
    deallocate(model % quants)

    do i = 1 , model % numElmsets
        call OesElmsetCleanup(model % elmsets(i))
    enddo
    deallocate(model % elmsets )

    do i = 1 , model % numExchItems
        call OesExchItemCleanup(model % exchItems(i))
    enddo
    deallocate(model % exchItems )

    model % numQuants    = 0
    model % numElmsets   = 0
    model % numExchItems = 0

    model % mIndex       = 0

end subroutine OesModelCleanup


!
! Find / add exchange quantities
!


function OesQuantAdd(model, quantID)  result(quant)

    ! return value
    type(oes_quant) , pointer       :: quant

    ! arguments
    type(oes_model) , intent(inout) :: model
    character(Len=*), intent(in)    :: quantID

    nullify(quant) ; if ( .not. OesInitialized() ) return

    if ( model % numQuants < MaxNumQuants ) then

        model % numQuants = model % numQuants  + 1
        quant => model % quants(model % numQuants)
        quant % qIndex    = model % numQuants

        quant % quantID     = quantID

    else
        if(doLog(level1)) write(oesLogHandle, '(A)') 'OesQuantAdd: MaxNumQuants exceeded'
        if(doLog(level1)) call flush(oesLogHandle)
    endif

end function OesQuantAdd


function OesQuantFind(model, quantID)  result(quant)

    ! return value
    type(oes_quant) , pointer       :: quant

    ! arguments
    type(oes_model) , intent(inout) :: model
    character(Len=*), intent(in)    :: quantID

    ! locals
    integer :: q

    ! body

    nullify(quant) ; if ( .not. OesInitialized() ) return

    do q = 1 , model % numQuants
        if ( model % quants(q) % qIndex /= OES_UNDEFINED ) then
            if ( model % quants(q) % quantID == quantID ) then
                quant => model % quants(q)
                exit
            endif
        endif
    enddo

end function OesQuantFind


subroutine OesQuantCleanup(quant)

    ! arguments
    type(oes_quant), intent(inout) :: quant

    ! body

    quant % qIndex = OES_UNDEFINED

end subroutine OesQuantCleanup


!
! Find / add elementsets
!

function OesElmsetAdd(model, elmsetID) result(elmset)

    ! return value
    type(oes_elmset), pointer       :: elmset

    ! arguments
    type(oes_model) , intent(inout) :: model
    character(Len=*), intent(in)    :: elmsetID

    ! body

    nullify(elmset) ; if ( .not. OesInitialized() ) return

    if ( model % numElmsets < MaxNumElmsets ) then

        model % numElmsets = model % numElmsets  + 1
        elmset => model % elmsets(model % numElmsets)
        elmset % esIndex = model % numElmsets

        elmset % elmsetID    = elmsetID

        nullify(elmset % elmIDs )
        nullify(elmset % Xcoords)
        nullify(elmset % Ycoords)

    else
        if(doLog(level1)) write(oesLogHandle, '(A)') 'OesElmsetAdd: MaxNumElmsets exceeded'
        if(doLog(level1)) call flush(oesLogHandle)
    endif

end function OesElmsetAdd


function OesElmsetFind(model, elmsetID)  result(elmset)

    ! return value
    type(oes_elmset), pointer    :: elmset

    ! arguments
    type(oes_model),  intent(in) :: model
    character(Len=*), intent(in) :: elmsetID

    ! locals
    integer :: es  ! element set loop counter
    character(Len=oes_id_len) :: localElmsetID   ! SH, 09-01-21: local copy if incoming elementset ID
                                 ! NOTE: this is done because of the fact that the elementset id
                                 ! comparison below,  if (elmsetID .eq. model % elmsets(es) % elmsetID ),
                                 ! fails for elmsetID's that come in from DATools through com.sun.jna
                                 ! As soon as this is repaired in DATools (nl.wldelft.da.models.delftflow.DLLWrapper),
                                 ! the localElmsetID copy can be removed.

    ! body

    nullify(elmset) ; if ( .not. OesInitialized() ) return

    localElmsetID = elmsetID
    do es = 1 , model % numElmsets
        if ( model % elmsets(es) % esIndex /= OES_UNDEFINED ) then
            if ( localElmsetID .eq. model % elmsets(es) % elmsetID ) then
                elmset => model % elmsets(es)
                exit
            endif
        endif
    enddo

end function OesElmsetFind


subroutine OesElmsetCleanup(elmset)

    ! arguments
    type(oes_elmset), intent(inout) :: elmset

    ! body
    if ( associated(elmset % elmIDs ) ) then
      deallocate(elmset % elmIDs )
      elmset%elmIDs => null()
    endif
    if ( associated(elmset % XCoords) ) then
        deallocate(elmset % XCoords)
        elmset%XCoords => null()
    endif
    if ( associated(elmset % YCoords) ) then
        deallocate(elmset % YCoords)
        elmset % YCoords => null()
    endif

    elmset % esIndex = OES_UNDEFINED

end subroutine OesElmsetCleanup


!
! Find / add exchange items
!


function OesExchItemAdd(model, quantID, elmsetID, role) result(exchItem)

    ! return value
    type(oes_exch_item), pointer    :: exchItem

    ! arguments
    type(oes_model),  intent(inout) :: model
    character(Len=*), intent(in)    :: quantID
    character(Len=*), intent(in)    :: elmsetID
    integer         , intent(in)    :: role          !(oes_providing|oes_accepting)

    ! locals
    type(oes_quant ), pointer     :: quant
    type(oes_elmset), pointer     :: elmset

    ! body

    nullify(exchItem) ; if ( .not. OesInitialized() ) return

    quant  => OesQuantFind(model, quantID )
    if ( .not. associated(quant) ) quant  => OesQuantAdd(model, quantID )
    elmset => OesElmsetFind(model, elmsetID)

    if ( associated(quant) .and. associated(elmset) ) then

        if ( model % numExchItems < MaxNumExchItems ) then

            model % numExchItems = model % numExchItems  + 1
            exchItem => model % exchItems(model % numExchItems)
            exchItem % eiIndex = model % numExchItems

            exchItem % componentID  = model % componentID
            exchItem % quantID      = quantID
            exchItem % elmsetID     = elmsetID
            exchItem % role         = role

            exchItem % quant  => quant
            exchItem % elmset => elmset

            nullify(exchItem % inputValues)
            nullify(exchItem % indexSet)

            exchItem % parentModelIndex     = model % mIndex
            exchItem % relatedModelIndex    = OES_UNDEFINED
            exchItem % relatedExchItemIndex = OES_UNDEFINED


        else
            if(doLog(level1)) write(oesLogHandle, '(A)') 'OesExchItemAdd: MaxNumExchItems exceeded'
            if(doLog(level1)) call flush(oesLogHandle)
        endif

    else
        if(doLog(level1)) write(oesLogHandle, '(A)') 'OesExchItemAdd: quant or elmset NULL'
        if(doLog(level1)) call flush(oesLogHandle)
    endif

end function OesExchItemAdd


function OesExchItemFind(model, quantID, elmsetID, role)  result(exchItem)

    ! return value
    type(oes_exch_item), pointer :: exchItem

    ! arguments
    type(oes_model),  intent(in) :: model
    character(Len=*), intent(in) :: quantID
    character(Len=*), intent(in) :: elmsetID
    integer         , intent(in) :: role          !(oes_providing|oes_accepting)

    ! locals
    integer :: ei

    ! body

    nullify(exchItem) ; if ( .not. OesInitialized() ) return

    do ei = 1 , model % numExchItems
        if ( model % exchItems(ei) % eiIndex /= OES_UNDEFINED ) then
            if ( ( model % exchItems(ei) % quantID     == quantID     ) .and. &
                 ( model % exchItems(ei) % elmsetID    == elmsetID    ) .and. &
                 ( model % exchItems(ei) % role        == role        )        ) then
                exchItem => model % exchItems(ei)
                exit
            endif
        endif
    enddo

end function OesExchItemFind


subroutine OesExchItemCleanup(exchItem)

    ! arguments
    type(oes_exch_item), intent(inout) :: exchItem

    ! body

    if ( associated( exchItem % inputValues   ) ) &
                    deallocate( exchItem % inputValues   )
    if ( associated( exchItem % indexSet ) ) &
                    deallocate( exchItem % indexSet )

    exchItem % eiIndex = OES_UNDEFINED

end subroutine OesExchItemCleanup


!
! Find / add Conversion (= related) items
!

function OesConvAdd(sourceComponentID, sourceQuantID, sourceElmsetID, &
                         targetComponentID, targetQuantID, targetElmsetID   ) result(conv)

    ! return value
    type(oes_conv), pointer      :: conv

    ! arguments
    character(Len=*), intent(in) :: sourceComponentID
    character(Len=*), intent(in) :: sourceQuantID
    character(Len=*), intent(in) :: sourceElmsetID
    character(Len=*), intent(in) :: targetComponentID
    character(Len=*), intent(in) :: targetQuantID
    character(Len=*), intent(in) :: targetElmsetID

    ! body

    nullify(conv) ; if ( .not. OesInitialized() ) return

    if ( oesStore % numConvs < MaxNumConvs ) then

       if(doLog(level1)) then
          write(oesLogHandle, '(13A)') 'OesConvAdd ', &
               trim(sourceComponentID), ' (', &
               trim(sourceQuantID), '/', &
               trim(sourceElmsetID), ')', '->', &
               trim(targetComponentID), ' (', &
               trim(targetQuantID), '/', &
               trim(targetElmsetID), ')'
       end if
        if(doLog(level1)) call flush(oesLogHandle)

        oesStore % numConvs = oesStore % numConvs  + 1
        conv => oesStore % convs(oesStore % numConvs)
        conv % cIndex = oesStore % numConvs

        conv % sourceComponentID = sourceComponentID
        conv % sourceQuantID     = sourceQuantID
        conv % sourceElmsetID    = sourceElmsetID

        conv % targetComponentID = targetComponentID
        conv % targetQuantID     = targetQuantID
        conv % targetElmsetID    = targetElmsetID

        nullify(conv % sourceExchItem)
        nullify(conv % targetExchItem)

        conv % mapped    = .false.

    else
        if(doLog(level1)) write(oesLogHandle, '(A)') 'OesConvAdd: MaxNumConvs exceeded'
        if(doLog(level1)) call flush(oesLogHandle)
    endif

end function OesConvAdd


function OesConvFind(sourceComponentID, sourceQuantID, sourceElmsetID, &
                          targetComponentID, targetQuantID, targetElmsetID   ) result(conv)

    ! return value
    type(oes_conv), pointer      :: conv

    ! arguments
    character(Len=*), intent(in) :: sourceComponentID
    character(Len=*), intent(in) :: sourceQuantID
    character(Len=*), intent(in) :: sourceElmsetID
    character(Len=*), intent(in) :: targetComponentID
    character(Len=*), intent(in) :: targetQuantID
    character(Len=*), intent(in) :: targetElmsetID

    ! locals
    integer                      :: ci

    ! body

    nullify(conv) ; if ( .not. OesInitialized() ) return

    do ci = 1 , oesStore % numConvs
        if ( oesStore % convs(ci) % cIndex /= OES_UNDEFINED ) then
            if ( ( oesStore % convs(ci) % sourceComponentID == sourceComponentID ) .and. &
                 ( oesStore % convs(ci) % sourceQuantID     == sourceQuantID     ) .and. &
                 ( oesStore % convs(ci) % sourceElmsetID    == sourceElmsetID    ) .and. &
                 ( oesStore % convs(ci) % targetComponentID == targetComponentID ) .and. &
                 ( oesStore % convs(ci) % targetQuantID     == targetQuantID     ) .and. &
                 ( oesStore % convs(ci) % targetElmsetID    == targetElmsetID    )        ) then
                conv => oesStore % convs(ci)
                exit
            endif
        endif
    enddo

end function OesConvFind


subroutine OesConvCleanup(oesConv)

    ! arguments
    type(oes_conv), intent(inout) :: oesConv

    ! body: no action (yet)
    oesConv % cIndex = OES_UNDEFINED
    nullify(oesConv % sourceExchItem)
    nullify(oesConv % targetExchItem)

end subroutine OesConvCleanup


!
! Mapping functions
!

subroutine OesAnalyzeRelations(final)

    ! arguments
    logical, intent(in)      :: final         ! final call? i.e. from extern?

    ! locals
    type(oes_conv) , pointer :: oesConv
    type(oes_model), pointer :: model
    integer                  :: cnv, m, ei

    ! body

    if ( .not. OesInitialized() ) return

    if(doLog(level1)) write(oesLogHandle, '(A,L,A)') '  OesAnalyzeRelations (', final, ')'
    if(doLog(level1)) call flush(oesLogHandle)

    do cnv = 1, oesStore % numConvs

        oesConv => oesStore % Convs(cnv)

        if ( .not. oesConv % mapped ) then

           if(doLog(level3)) then
              write(oesLogHandle, '(13A)') '    ', &
                   trim(oesConv % sourceComponentID), ' (', &
                   trim(oesConv % sourceQuantID), '/', &
                   trim(oesConv % sourceElmsetID), ')', ')->', &
                   trim(oesConv % targetComponentID), ' (', &
                   trim(oesConv % targetQuantID), '/',  &
                   trim(oesConv % targetElmsetID), ')'
           end if

            do m = 1 , oesStore % numModels

                model => oesStore % models(m)

                do ei = 1 , model % numExchItems

                    if ( .not. oesConv % mapped ) then

                        if(doLog(level3)) write(oesLogHandle, '(A,I5,A,I5,5A)')                         &
                            '      ei ', ei, ' of ', model % numExchItems, ' in ', trim(model % componentID) , &
                                   '(', trim(model % exchItems(ei) % quantID), ')'

                        if ( .not. associated(oesConv % sourceExchItem) ) then
                            if ( ( model % exchItems(ei) % role == oes_providing                       ) .and. &
                                 ( oesConv % sourceComponentID  ==  model % exchItems(ei) % componentID ) .and. &
                                 ( oesConv % sourceQuantID      ==  model % exchItems(ei) % quantID     )       ) then
                                if ( associated(model % exchItems(ei) % elmset ) ) then
                                    if ( oesConv % sourceElmsetID ==  model % exchItems(ei) % elmset % elmsetID ) then
                                        oesConv % sourceExchItem => model % exchItems(ei)
                                        if(doLog(level3)) write(oesLogHandle, '(A)') '        source index set'
                                    else if ( final ) then
                                        if ( associated( oesConv % targetExchItem ) ) then
                                            if ( OesElmsetContainsElement( oesConv % targetExchItem % elmset, &
                                                                           oesConv % sourceElmsetID           ) ) then
                                                oesConv % sourceExchItem => model % exchItems(ei)
                                            endif
                                        endif
                                    endif
                                endif
                            endif
                        endif

                        if ( .not. associated(oesConv % targetExchItem) ) then
                            if ( ( model % exchItems(ei) % role == oes_accepting                       ) .and. &
                                 ( oesConv % targetComponentID  ==  model % exchItems(ei) % componentID ) .and. &
                                 ( oesConv % targetQuantID      ==  model % exchItems(ei) % quantID     )       ) then
                                if ( associated(model % exchItems(ei) % elmset ) ) then
                                    if ( oesConv % targetElmsetID ==  model % exchItems(ei) % elmset % elmsetID ) then
                                        oesConv % targetExchItem => model % exchItems(ei)
                                        if(doLog(level3)) write(oesLogHandle, '(A)') '        target index set'
                                    else if ( final ) then
                                        if ( associated( oesConv % sourceExchItem ) ) then
                                            if ( OesElmsetContainsElement( oesConv % sourceExchItem % elmset, &
                                                                           oesConv % targetElmsetID           ) ) then
                                                oesConv % targetExchItem => model % exchItems(ei)
                                            endif
                                        endif
                                    endif
                                endif
                            endif
                        endif

                        if ( associated( oesConv % sourceExchItem ) .and. &
                             associated( oesConv % targetExchItem ) .and. &
                                         ( .not. oesConv % mapped )       )  then

                            call OesBuildIDBasedMapping( oesConv )

                        endif

                    endif

                enddo

            enddo

        endif

    enddo

    if(doLog(level1)) call flush(oesLogHandle)

end subroutine OesAnalyzeRelations


subroutine OesBuildIDBasedMapping(oesConv)

    ! arguments
    type(oes_conv)            :: oesConv

    ! locals
    integer                   :: sourceElm, nSourceElms
    integer                   :: targetElm, nTargetElms
    type(oes_elmset), pointer :: sourceElmset, targetElmset
    logical                   :: allElements
    character(Len=oes_id_len) :: sourceElmsetID
    character(Len=oes_id_len) :: targetElmsetID

    ! body

    if ( .not. OesInitialized() ) return

    if(doLog(level3)) then
       write(oesLogHandle, '(13A)') '      OesBuildIDBasedMapping ', &
            trim(oesConv % sourceComponentID), '(',  &
            trim(oesConv % sourceQuantID), '/', &
            trim(oesConv % sourceElmsetID), ')' , '->',  &
            trim(oesConv % targetComponentID), '(', &
            trim(oesConv % targetQuantID), '/', &
            trim(oesConv % targetElmsetID), ')'
    end if
    if(doLog(level3)) call flush(oesLogHandle)

    sourceElmset => oesConv % sourceExchItem % elmset
    targetElmset => oesConv % targetExchItem % elmset

    if ( .not. associated( sourceElmset % elmIDs ) ) then
        if(doLog(level1)) write(oesLogHandle, '(A)') '        NO SOURCE ELMSET-IDS'
    else
        if ( .not. associated( targetElmset % elmIDs ) ) then
            if(doLog(level1)) write(oesLogHandle, '(A)') '        NO TARGET ELMSET-IDS'
        else
            nSourceElms = size( sourceElmset % elmIDs )
            nTargetElms = size( targetElmset % elmIDs )

            if(doLog(level3)) write(oesLogHandle, '(5A,I3)') '        Mapping ', &
                        trim(oesConv % sourceQuantID), '->', &
                        trim(oesConv % targetQuantID), ', nElms: ', nTargetElms

            allElements = .false.
            if ( nTargetElms == 1 .and. targetElmset % elmIDs(1) == 'AllElements' ) then
                nTargetElms = nSourceElms
                deallocate(targetElmset % elmIDs)
                allocate  (targetElmset % elmIDs(nTargetElms))
                allElements = .true.
                if(doLog(level3)) write(oesLogHandle, '(A)') '          ALL ELEMENTS'
            endif

            if ( .not. associated(oesConv % targetExchItem % indexSet) ) then
                allocate(oesConv % targetExchItem % indexSet(nTargetElms))
                oesConv % targetExchItem % indexSet = OES_UNDEFINED
            endif

            do targetElm = 1, nTargetElms

                if ( allElements ) then

                    oesConv % targetExchItem % indexSet(targetElm) = targetElm
                    targetElmset  % elmIDs(targetElm) = sourceElmset % elmIDs(targetElm)
                    oesConv % mapped = .true.

                else
                    do sourceElm = 1, nSourceElms
                        sourceElmsetID = sourceElmset % elmIDs(sourceElm)
                        targetElmsetID = targetElmset % elmIDs(targetElm)
                        if ( nTargetElms == 1 ) then
                            targetElmsetID = OesGetChildElmSetID(targetElmsetID)
                        endif
                        if ( nSourceElms == 1 ) then
                            sourceElmsetID = OesGetChildElmSetID(sourceElmsetID)
                        endif
                        if ( sourceElmsetID == targetElmsetID ) then

                            oesConv % targetExchItem % indexSet(targetElm) = sourceElm
                            if(doLog(level3)) write(oesLogHandle, '(4A)') '          MAPPED ' , &
                                                trim(sourceElmset % elmIDs(sourceElm)), ' -> ',&
                                                trim(targetElmset % elmIDs(targetElm))
                            oesConv % mapped = .true.

                            oesConv % targetExchItem % relatedModelIndex    = oesConv % sourceExchItem % parentModelIndex
                            oesConv % targetExchItem % relatedExchItemIndex = oesConv % sourceExchItem % eiIndex
                        endif
                    enddo
                endif
            enddo
            if ( oesConv % mapped ) then

                oesConv % targetExchItem % relatedModelIndex    = oesConv % sourceExchItem % parentModelIndex
                oesConv % targetExchItem % relatedExchItemIndex = oesConv % sourceExchItem % eiIndex

                if ( associated(oesConv % sourceExchItem % inputValues) ) then
                    if ( size(oesConv % sourceExchItem % inputValues) /= nSourceElms ) then
                        write(oesError, '(7A)') &
                            'ERROR: OesBuildIDBasedMapping: inconsistent inputValues size for ', &
                                        trim(oesConv % sourceComponentID) ,     &
                                        '(', trim(oesConv % sourceQuantID),     &
                                        '/', trim(oesConv % sourceElmsetID), ')'
                        if(doLog(level3)) write(oesLogHandle, '(A)') oesError
                    endif
                else
                    allocate(oesConv % sourceExchItem % inputValues(nSourceElms))
                endif
                oesConv % sourceExchItem % inputValues = -9.9999D+00
            else
                if(doLog(level3)) write(oesLogHandle, '(A)') &
                    '          NO MAPPING, NULLIFYING'
                deallocate(oesConv % targetExchItem % indexSet)
                nullify(oesConv % targetExchItem % indexSet)
                oesConv % mapped = .false.
            endif
        endif
    endif

    if(doLog(level3)) call flush(oesLogHandle)

end subroutine OesBuildIDBasedMapping


function OesElmsetContainsElement( elmset, elmID ) result(doesContain)

    ! returnvalue
    logical :: doesContain

    ! arguments
    type(oes_elmset), intent(in) :: elmset
    character(Len=*), intent(in) :: elmID

    ! locals
    integer  :: i
    character(Len=oes_id_len) :: elementID     ! elm. or child-element id

    doesContain = .false.

    elementID = OesGetChildElmSetID(elmID)
    do i = 1 , size( elmset % elmIDs )
        if ( elmset % elmIDs(i) == elementID ) then
            doesContain = .true.
            exit
        endif
    enddo

    if(doLog(level3)) write(oesLogHandle, '(5A,L)') '        OesElmsetContainsElement: ', &
                        trim(elmset % elmsetID), ' contains ', trim(elmID), ': ', doesContain
    if(doLog(level3)) call flush(oesLogHandle)

end function OesElmsetContainsElement


function OesDoIDBasedMapping(modelHandle, targetExchItem, sourceExchItem, values, mask) result(numElems)

    ! returnvalue
    integer :: numElems

    ! arguments
    integer            , intent(in)              :: modelHandle
    type(oes_exch_item), intent(in)              :: targetExchItem
    type(oes_exch_item), intent(in)              :: sourceExchItem
    double precision, intent(out), dimension(:)  :: values
    logical, intent(out), dimension(:), optional :: mask

    ! locals
    integer          :: targetElm, numTargetElms
    double precision :: value

    ! body

    numElems = OES_UNDEFINED
    values   = -999.0D+00
    if ( present(mask) ) mask = .false.

    if ( .not. OesInitialized() ) return

    if(doLog(level3)) write(oesLogHandle, '(/,4A)') '           <-OesDoIDBasedMapping: ',    &
                     trim(sourceExchItem % quantID), '->',  trim(targetExchItem % quantID)
    numElems = 0
    if ( associated(sourceExchItem % inputValues) ) then
        numTargetElms = size(targetExchItem % indexSet)
        if(doLog(level4).and. .not. doLog(level3))                        &
            write(oesLogHandle, '(A,I2,5A)') '<--OesGet mh =',  &
                  modelHandle, ' (',  trim(targetExchItem % quantID), '/', trim(targetExchItem % elmsetID), ')'
        do targetElm = 1, numTargetElms
            if ( targetExchItem % indexSet(targetElm) /= OES_UNDEFINED ) then
                value = sourceExchItem % inputValues(targetExchItem % indexSet(targetElm))
                if ( ( .not. OesDoublesEqual (value, -999.0D+0 ) ) .and. &
                     ( .not. OesDoublesEqual (value, -9.9999D+0 ) )       ) then
                    values(targetElm) = sourceExchItem % inputValues(targetExchItem % indexSet(targetElm))
                    if ( present(mask) ) mask(targetElm) = .true.
                    numElems = numElems + 1
                    if(doLog(level4)) write(oesLogHandle, '(A,I4,A,E13.6)') &
                           '      ', targetElm, ':', values(targetElm)
                else
                    if(doLog(level4)) write(oesLogHandle, '(A,I4,A)') &
                           '      ', targetElm, ' not provided'
                endif
            else
                if(doLog(level3)) write(oesLogHandle, '(A,I4,A)') &
                       '      ', targetElm, ' not mapped'
            endif
        enddo
    endif

    if(doLog(level3)) write(oesLogHandle, '(A,I4)') '    #elems: ',  numElems
    if(doLog(level3)) call flush(oesLogHandle)

end function OesDoIDBasedMapping


!-----------------------------------------------------------------
! SUPPORT FUNCTIONS
!-----------------------------------------------------------------


subroutine OesBaseName(itemName, baseName)
    character(Len=*), intent(IN) :: itemName  ! ds name
    character(Len=*), intent(OUT):: baseName  ! ds name without path
    integer                      :: slashPos  ! pos. of last slash
    character(Len=1)             :: slash     ! slash or backslash

#if (defined(WIN32))
    slash = '\'
#else
    slash = '/'
#endif

    ! value in case of no slash

    baseName = itemName

    ! find slash pos. from end. If found, skip path.

    slashPos = index(itemName, slash, .true.)
    if ( slashPos .ne. 0 ) then
        baseName = itemName(slashPos+1:)
    endif

end subroutine OesBaseName


subroutine OesDirName(itemName, dirName)
    character(Len=*), intent(IN) :: itemName  ! ds name
    character(Len=*), intent(OUT):: dirName   ! ds name without path
    integer                      :: slashPos  ! pos. of last slash
    character(Len=1)             :: slash     ! slash or backslash

#if (defined(WIN32))
    slash = '\'
#else
    slash = '/'
#endif

    ! value in case of no slash

    dirName = '.'

    ! find slash pos. from end. If found, skip path.

    slashPos = index(itemName, slash, .true.)
    if ( slashPos .ne. 0 ) then
        dirName = itemName(1:slashPos-1)
    endif

end subroutine OesDirName


!-----------------------------------------------------------------
! TEMPORARY FUNCTIONS (will become obsolete when all engines
!                      are 'fully wrapped')
!-----------------------------------------------------------------


function OesModelCoreIsInitialized(componentID, schemID) result(core_initialized)

    ! return value
    logical                      :: core_initialized

    ! arguments
    character(Len=*), intent(in) :: componentID
    character(Len=*), intent(in) :: schemID

    ! locals
    type(oes_model), pointer     :: model

    ! body

    core_initialized = .false.

    if ( .not. OesInitialized() ) return

    model => OesModelFind_ByID(componentID, schemID)
    if ( associated(model) ) then
        core_initialized = model % core_initialized
    endif

end function OesModelCoreIsInitialized


subroutine OesModelSetCoreInitialized(componentID, schemID, core_initialized)

    ! return value

    ! arguments
    character(Len=*), intent(in) :: componentID
    character(Len=*), intent(in) :: schemID
    logical         , intent(in) :: core_initialized

    ! locals
    type(oes_model), pointer     :: model

    ! body

    if ( .not. OesInitialized() ) return

    model => OesModelFind_ByID(componentID, schemID)
    if ( associated(model) ) then
        model % core_initialized = core_initialized
    endif

end subroutine OesModelSetCoreInitialized


function OesDoublesEqual(double_1, double_2) result(equal)

    ! return value
    logical :: equal ! .true.  : double_1 and double_2 are equal
                     ! .false. : double_1 and double_2 not equal

    ! arguments
    double precision, intent(in) :: double_1 ! first double
    double precision, intent(in) :: double_2 ! second double

    ! local
    double precision, parameter:: bo_double_epsilon = 1.0D-7

    ! body

    equal = .false.
    if ( ( double_1 + bo_double_epsilon ) > double_2 .and. &
         ( double_1 - bo_double_epsilon ) < double_2       ) then
        equal = .true.
    endif

end function OesDoublesEqual


function OesGetParentElmSetID( elementSetID ) result(parentID)

    ! returnvalue
    character(Len=oes_id_len)    :: parentID

    ! arguments
    character(Len=*), intent(in) :: elementSetID

    ! locals
    integer                      :: dotPos ! position of '.' in elementset ID

    ! body

    parentID = elementSetID
    dotPos = index(elementSetID, '.')
    if ( dotPos > 2 ) then
        parentID = elementSetID(:dotPos-1)
    endif

end function OesGetParentElmSetID


function OesGetChildElmSetID( elementSetID ) result(childID)

    ! returnvalue
    character(Len=oes_id_len)    :: childID

    ! arguments
    character(Len=*), intent(in) :: elementSetID

    ! locals
    integer                      :: dotPos ! position of '.' in elementset ID

    ! body

    childID = elementSetID
    dotPos = index(elementSetID, '.')
    if ( dotPos > 2 ) then
        childID = elementSetID(dotPos+1:)
    endif

end function OesGetChildElmSetID

   subroutine small(string)
      implicit none
      character(len=*)           :: string
      integer       i, j, newlen, lenstr, biga, bigz, smalla, offset
!
      biga   = ICHAR('A')
      bigz   = ICHAR('Z')
      smalla = ICHAR('a')

      offset = smalla - biga

      newlen = len(string)
      do i = 1, newlen
         j = ichar (string(i:i))
         if ( (j .ge. biga) .and. (j .le. bigz) ) then
            j = j + offset
            string(i:i) = char (j)
         endif
      enddo

   end subroutine small

   subroutine big(string)
      implicit none
      character(len=*)           :: string
      integer       i, j, newlen, lenstr, smalla, smallz, biga, offset
!
      smalla = ICHAR('a')
      smallz = ICHAR('z')
      biga   = ICHAR('A')

      offset = smalla - biga

      newlen = len(string)
      do i = 1, newlen
         j = ichar (string(i:i))
         if ( (j .ge. smalla) .and. (j .le. smallz) ) then
            j = j - offset
            string(i:i) = char (j)
         endif
      enddo

   end subroutine big


end module wl_open_mi_support
