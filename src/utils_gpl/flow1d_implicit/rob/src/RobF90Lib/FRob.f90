! Copyright © 2004, Rijkswaterstaat/RIZA & ®HKV Consultants, All Rights Reserved.
!
! DESCRIPTION
!   Module interface for RIZA OpenMI Buffer
!
!   Called from Fortran
!
! AUTHOR
!   Johan Ansink, HKV lijn in water
!
! HISTORY
!   R. Terveer, RIZA : june 2005 changes due to introduction Quantityset & QuantityProperties
!
! $Header: $
! $NoKeywords: $
!
!
module frob

  use irob

  IMPLICIT NONE

  character(len=ROBFILELEN+1) :: CFileName

  character(len=ROBIDLEN+1)   :: CmodelID
  character(len=ROBIDLEN+1)   :: CmodelDescription

  character(len=ROBIDLEN+1)   :: CunitID
  character(len=ROBIDLEN+1)   :: CunitDescription

  character(len=ROBIDLEN+1)   :: CquantityID
  character(len=ROBIDLEN+1)   :: CquantityDescription

  character(len=ROBIDLEN+1)   :: CelementID
  character(len=ROBIDLEN+1)   :: CelementDescription
  character(len=ROBIDLEN+1)   :: CelementsetID
  character(len=ROBIDLEN+1)   :: CelementsetDescription

  character(len=ROBIDLEN+1)   :: CquantitysetID
  character(len=ROBIDLEN+1)   :: CquantitysetDescription
  character(len=ROBIDLEN+1)   :: CpropertyID
  character(len=ROBIDLEN+1)   :: CpropertyValue

  public FRobElementSetAddIDBasedSingle
  public FRobElementSetAddIDBasedMultiple
  
  CONTAINS

  FUNCTION FRobInitialize (logFileName) result (rv)
    character (len=*),intent(in), optional :: logFileName
    integer :: rv

    call MakeEmptyCstring(CFileName)
    if ( present(logFileName) ) then
        CFileName = trim(logFileName)//char(0)
    endif
    rv = RobInitialize(CFileName)
  END FUNCTION 

  FUNCTION FRobUnitAdd (unitID, unitDescription, conversionFactor, conversionOffset) result (rv)
    character (len=*),intent(in) :: unitID
    character (len=*),intent(in) :: unitDescription
    real(8)          ,intent(in) :: conversionFactor
    real(8)          ,intent(in) :: conversionOffset
    integer                      :: rv
        
    CunitID           = trim(unitID)//char(0)
    CunitDescription  = trim(unitDescription)//char(0)

    rv =  RobUnitAdd (CunitID, CunitDescription, conversionFactor, conversionOffset)

  END FUNCTION

  FUNCTION FRobUnitGetInfo (unitID, unitDescription, conversionFactor, conversionOffset) result (rv)
    character (len=*),intent(in) :: unitID
    character (len=*),intent(out) :: unitDescription
    real(8)          ,intent(out) :: conversionFactor
    real(8)          ,intent(out) :: conversionOffset
    integer                      :: rv

    CunitID           = trim(unitID)//char(0)
    call MakeEmptyCstring(CunitDescription)

    rv =  RobUnitGetInfo (CunitID, CunitDescription, conversionFactor, conversionOffset)

    unitDescription = Cstring2Fstring(CunitDescription)

  END FUNCTION

  FUNCTION FRobQuantityAdd (quantityID, quantityDescription, unitID) result (rv)
    character (len=*),intent(in) :: quantityID
    character (len=*),intent(in) :: quantityDescription
    character (len=*),intent(in) :: unitID
    integer                      :: rv

    CquantityID          = trim(quantityID)//char(0)
    CquantityDescription = trim(quantityDescription)//char(0)
    CunitID              = trim(unitID)//char(0)

    rv =  RobQuantityAdd (CquantityID, CquantityDescription, CunitID)

  END FUNCTION


  FUNCTION FRobQuantityAddQuantity (quantitysetID, quantityID, quantityDescription, unitID) result (rv)
    character (len=*),intent(in) :: quantitysetID
    character (len=*),intent(in) :: quantityID
    character (len=*),intent(in) :: quantityDescription
    character (len=*),intent(in) :: unitID
    integer                      :: rv

    CquantitysetID       = trim(quantitysetID)//char(0)
    CquantityID          = trim(quantityID)//char(0)
    CquantityDescription = trim(quantityDescription)//char(0)
    CunitID              = trim(unitID)//char(0)

    rv =  RobQuantityAddQuantity (CquantitysetID, CquantityID, CquantityDescription, CunitID)
  END FUNCTION


  FUNCTION FRobQuantitysetRemoveQuantity (quantitysetID, quantityID) result (rv)
    character (len=*),intent(in) :: quantitysetID
    character (len=*),intent(in) :: quantityID
    integer                      :: rv

    CquantitysetID       = trim(quantitysetID)//char(0)
    CquantityID          = trim(quantityID)//char(0)

    rv =  RobQuantitysetRemoveQuantity (CquantitysetID, CquantityID)
  END FUNCTION

  FUNCTION FRobQuantitysetRemoveAllQuantities (quantitysetID) result (rv)
    character (len=*),intent(in) :: quantitysetID
    integer                      :: rv

    CquantitysetID       = trim(quantitysetID)//char(0)
    rv =  RobQuantitysetRemoveAllQuantities (CquantitysetID)
  END FUNCTION

  function FRobQuantityAddProperty(quantitysetID, quantityID, propertyID, propertyValue) result(rv)
    character (len=*),intent(in) :: quantitysetID,quantityID, propertyID, propertyValue
    integer                      :: rv

    CquantitysetID = trim(quantitysetID)//char(0)
    CquantityID    = trim(quantityID)//char(0)
    CpropertyID    = trim(propertyID)//char(0)
    CpropertyValue = trim(propertyValue)//char(0)

    rv = RobQuantityAddProperty(CquantitysetID, CquantityId,CpropertyID,CpropertyValue)
  end function FRobQuantityAddProperty

  function FRobQuantityPropertySetValue(quantitysetID, quantityID, propertyID, propertyValue) result(rv)
    character (len=*),intent(in) :: quantitysetID,quantityID, propertyID, propertyValue
    integer                      :: rv

    CquantitysetID = trim(quantitysetID)//char(0)
    CquantityID    = trim(quantityID)//char(0)
    CpropertyID    = trim(propertyID)//char(0)
    CpropertyValue = trim(propertyValue)//char(0)

    rv = RobQuantityPropertySetValue(CquantitysetID, CquantityId,CpropertyID,CpropertyValue)
  end function FRobQuantityPropertySetValue


  function FRobQuantityPropertyValue(quantitysetID, quantityID, propertyID) result(propertyValue)
    character (len=*),intent(in)       :: quantitysetID, quantityID, propertyID
    character(len=ROBIDLEN+1)          :: propertyValue

    CquantitysetID = trim(quantitysetID)//char(0)
    CquantityID    = trim(quantityID)//char(0)
    CpropertyID    = trim(propertyID)//char(0)
    call MakeEmptyCstring(CpropertyValue)

    if( RobQuantityProperty(CquantitysetID, CquantityId,CpropertyID, CpropertyValue) == 1) then
       propertyValue = Cstring2Fstring(CpropertyValue)
    else
       propertyValue = ''
    endif      
    !rv = RobQuantityPropertyValue(CquantitysetID, CquantityId,CpropertyID)
  end function FRobQuantityPropertyValue

  function FRobQuantitysetGetQuantityID(quantitysetID, quantityOrderNr) result(quantityID)
    character (len=*),intent(in)       :: quantitysetID
    integer,intent(in)                 :: quantityOrderNr
    character(len=ROBIDLEN+1)          :: quantityID

    CquantitysetID = trim(quantitysetID)//char(0)
    call MakeEmptyCstring(CquantityID)

    if(RobQuantitysetQuantityID(CquantitysetID,quantityOrderNr,CquantityID) == 1) then
      quantityID=Cstring2Fstring(CquantityID)
    else
      quantityID = ''
    endif    
  end function FRobQuantitysetGetQuantityID

  FUNCTION FRobQuantityGetInfo (quantityID, quantityDescription, unitID, NumberOfQuantities) result (rv)
    character (len=*),intent(in)  :: quantityID
    character (len=*),intent(out) :: quantityDescription
    character (len=*),intent(out) :: unitID
    integer          ,intent(out) :: NumberOfQuantities
    integer                       :: rv

    CquantityID = trim(quantityID)//char(0)

    call MakeEmptyCstring(CquantityDescription)
    call MakeEmptyCstring(CunitID)

    rv =  RobQuantityGetInfo (CquantityID, CquantityDescription, CunitID, NumberOfQuantities)

    quantityDescription = Cstring2Fstring(CquantityDescription)
    unitID = Cstring2Fstring(CunitID)

  END FUNCTION


  FUNCTION FRobElementSetAdd (modelID, elementSetID, elementDescription, elementType) result (rv)
    character (len=*),intent(in) :: modelID
    character (len=*),intent(in) :: elementSetID
    character (len=*),intent(in) :: elementDescription
    integer          ,intent(in) :: elementType
    integer                      :: rv

    CmodelID            = trim(modelID)//char(0)
    CelementSetID       = trim(elementSetID)//char(0)
    CelementDescription = trim(elementDescription)//char(0)

    rv = RobElementSetAdd(CmodelID, CelementSetID, CelementDescription, elementType)
  END FUNCTION FRobElementSetAdd

  
  FUNCTION FRobElementSetAddIDBasedMultiple (modelID, elementSetID, elementDescription, elementIDs) result (rv)
    character (len=*),intent(in) :: modelID
    character (len=*),intent(in) :: elementSetID
    character (len=*),intent(in) :: elementDescription
    character (len=*), dimension(:), intent(in) :: elementIDs
    integer                      :: rv

    integer                      :: e, elementCount, elementType

    CmodelID            = trim(modelID)//char(0)
    CelementSetID       = trim(elementSetID)//char(0)
    CelementDescription = trim(elementDescription)//char(0)

    elementCount = size(elementIDs)
    elementType  = 0 ! ID-based
    rv = RobElementSetAdd(CmodelID, CelementSetID, CelementDescription, elementType)
    do e = 1, elementCount
      CelementID = trim(elementIDs(e))//char(0)
      rv = RobElementSetAddElementIDBased(CmodelID, CelementSetID, CelementID)
    enddo

  END FUNCTION FRobElementSetAddIDBasedMultiple

  
  FUNCTION FRobElementSetAddIDBasedSingle (modelID, elementSetID, elementDescription) result (rv)
    character (len=*),intent(in) :: modelID
    character (len=*),intent(in) :: elementSetID
    character (len=*),intent(in) :: elementDescription
    integer                      :: rv

    CmodelID            = trim(modelID)//char(0)
    CelementSetID       = trim(elementSetID)//char(0)
    CelementDescription = trim(elementDescription)//char(0)

    rv = RobElementSetAddIDBasedSingle(CmodelID, CelementSetID, CelementDescription)
  END FUNCTION FRobElementSetAddIDBasedSingle

  
  FUNCTION FRobElementsetElementCount (modelID, elementSetID) result (rv)
    character (len=*),intent(in) :: modelID
    character (len=*),intent(in) :: elementSetID
    integer                      :: rv

    CmodelID      = trim(modelID)//char(0)
    CelementSetID = trim(elementSetID)//char(0)
    
    rv = RobElementsetElementCount (CmodelID, CelementSetID)

  END FUNCTION

  FUNCTION FRobElementsetGetInfo (modelID, elementsetID, elementsetDescription, elementsetType, nElements) result (rv)
    character (len=*),intent(in)  :: modelID
    character (len=*),intent(in)  :: elementsetID
    character (len=*),intent(out) :: elementsetDescription
    integer          ,intent(out) :: elementsetType
    integer          ,intent(out) :: nElements
    integer                       :: rv

    CmodelID      = trim(modelID)//char(0)
    CelementsetID = trim(elementsetID)//char(0)
    call MakeEmptyCstring(CelementSetDescription)

    rv =  RobElementsetGetInfo (CmodelID, CelementsetID, CelementsetDescription, elementSetType,nElements)
 
    elementsetDescription = Cstring2Fstring(CelementSetDescription)

  END FUNCTION

  FUNCTION FRobModelAdd (modelID, modelDescription) result (rv)
    character (len=*),intent(in) :: modelID
    character (len=*),intent(in) :: modelDescription
    integer                      :: rv

    CmodelID          = trim(modelID)//char(0)
    CmodelDescription = trim(modelDescription)//char(0)
 
    rv =  RobModelAdd (CmodelID, CmodelDescription)
 
  END FUNCTION

  FUNCTION FRobModelCount () result (rv)
    integer                      :: rv

    rv =  RobModelCount () 

  END FUNCTION

  FUNCTION FRobModelGetInfo (index, modelID, modelDescription) result (rv)
    integer          ,intent(in)  :: index
    character (len=*),intent(out) :: modelID
    character (len=*),intent(out) :: modelDescription
    integer                       :: rv
    
    call MakeEmptyCstring(CmodelID)
    call MakeEmptyCstring(CmodelDescription)

    rv =  RobModelGetInfo (index, CmodelID, CmodelDescription)
 
    modelID = Cstring2Fstring(CmodelID)
    modelDescription = Cstring2Fstring(CmodelDescription)
  END FUNCTION

  FUNCTION FRobExchangeitemAdd (modelID, quantityID, elementsetID, role) result (rv)
    character (len=*),intent(in) :: modelID
    character (len=*),intent(in) :: quantityID
    character (len=*),intent(in) :: elementsetID
    integer          ,intent(in) :: role
    integer                      :: rv

    CmodelID      = trim(modelID)//char(0)
    CquantityID   = trim(quantityID)//char(0)
    CelementsetID = trim(elementsetID)//char(0)

    rv =  RobExchangeitemAdd (CmodelID, CquantityID, CelementsetID, role)
 
  END FUNCTION


  FUNCTION FRobExchangeitemCount (modelID) result (rv)
    character (len=*),intent(in) :: modelID
    integer                      :: rv

    CmodelID      = trim(modelID)//char(0)

    rv =  RobExchangeitemCount (CmodelID) 

  END FUNCTION

  FUNCTION FRobExchangeitemGetInfo (modelID, index, quantityID, elementsetID, role) result (rv)
    character (len=*),intent(in)  :: modelID
    integer          ,intent(in)  :: index
    character (len=*),intent(out) :: quantityID
    character (len=*),intent(out) :: elementsetID
    integer          ,intent(out) :: role
    integer                       :: rv

    CmodelID      = trim(modelID)//char(0)
    
    call MakeEmptyCstring(CquantityID)
    call MakeEmptyCstring(CelementsetID)

    rv = RobExchangeitemGetInfo (CmodelID, index, CquantityID, CelementsetID, role)
    
    quantityID   = Cstring2Fstring(CquantityID)
    elementsetID = Cstring2Fstring(CelementsetID)

  END FUNCTION

  SUBROUTINE FRobExchangeitemsResize()

    call RobExchangeitemsResize()

  END SUBROUTINE

  FUNCTION FRobPutInt (modelID, quantitysetId, quantityID, elementsetID, role, nElements, iValues) result (rv)
    character (len=*),intent(in) :: modelID
    character (len=*),intent(in) :: quantitysetID, quantityID
    character (len=*),intent(in) :: elementsetID
    integer          ,intent(in) :: role
    integer          ,intent(in) :: nElements
    integer          ,intent(in) :: iValues(nElements)
    integer                      :: rv

    CmodelID       = trim(modelID)//char(0)
    CquantitysetID = trim(quantitysetID)//char(0)
    CquantityID    = trim(quantityID)//char(0)
    CelementsetID  = trim(elementsetID)//char(0)
    
    rv =  RobPutInt (CmodelID, CquantitysetID, CquantityID, CelementsetID, role, nElements, iValues)

  END FUNCTION


  FUNCTION FRobPutReal (modelID, quantitysetID, quantityID, elementsetID, role, nElements, fValues) result (rv)
    character (len=*),intent(in) :: modelID
    character (len=*),intent(in) :: quantityID,quantitysetID
    character (len=*),intent(in) :: elementsetID
    integer          ,intent(in) :: role
    integer          ,intent(in) :: nElements
    real(4)          ,intent(in) :: fValues(nElements)
    integer                      :: rv

    CmodelID       = trim(modelID)//char(0)
    CquantitysetID = trim(quantitysetID)//char(0)
    CquantityID    = trim(quantityID)//char(0)
    CelementsetID  = trim(elementsetID)//char(0)
    
    rv =  RobPutReal (CmodelID, CquantitysetID, CquantityID, CelementsetID, role, nElements, fValues)

  END FUNCTION

  
  FUNCTION FRobPutDouble (modelID, quantitysetID, quantityID, elementsetID, role, nElements, dValues) result (rv)
    character (len=*),intent(in) :: modelID
    character (len=*),intent(in) :: quantityID,quantitysetID
    character (len=*),intent(in) :: elementsetID
    integer          ,intent(in) :: role
    integer          ,intent(in) :: nElements
    real(8)          ,intent(in) :: dValues(nElements)
    integer                      :: rv

    CmodelID       = trim(modelID)//char(0)
    CquantitysetID = trim(quantitysetID)//char(0)
    CquantityID    = trim(quantityID)//char(0)
    CelementsetID  = trim(elementsetID)//char(0)
    
    rv =  RobPutDouble (CmodelID, CquantitysetID, CquantityID, CelementsetID, role, nElements, dValues)

  END FUNCTION


  FUNCTION FRobGetDouble(modelID, quantitysetID, quantityID, elementsetID, role, nElements, dValues) result (rv)
    character (len=*),intent(in) :: modelID
    character (len=*),intent(in) :: quantityID,quantitysetID
    character (len=*),intent(in) :: elementsetID
    integer          ,intent(in) :: role
    integer          ,intent(in)  :: nElements
    real(8)          ,intent(out) :: dValues(nElements)
    integer                      :: rv

    CmodelID       = trim(modelID)//char(0)
    CquantitysetID = trim(quantitysetID)//char(0)
    CquantityID    = trim(quantityID)//char(0)
    CelementsetID  = trim(elementsetID)//char(0)
    
    rv =  RobGetDouble (CmodelID, CquantitysetID, CquantityID, CelementsetID, role, nElements, dValues)

  END FUNCTION

  
  SUBROUTINE FRobFinalize
    CALL RobFinalize
  END SUBROUTINE

  subroutine FRobExchangeItemsTrim()
    call RobExchangeItemsTrim()
  END SUBROUTINE


  SUBROUTINE FRobDump (dumpFileName)
    character (len=*),intent(in):: dumpFileName

    CFileName = trim(dumpFileName)//char(0)

    call RobDump(CFileName)

  END SUBROUTINE 


  subroutine MakeEmptyCstring (string)

  character*(*), intent(inout) :: string
  integer   ::i

  do i = 1, len(string)-1
     STRING(I:I) = ' '
  end do

  string(len(string):) = char(0)

  end subroutine

  function Cstring2Fstring (string) result(rv)

  character*(*), intent(in) :: string
  character(len=ROBIDLEN)   ::rv
  
  integer         :: i
  logical         :: found

  i = 1
  found = .false.

  do while ( i <= len(string) .AND. .not. found )
    if (string(i:i) .ne. char(0)) then
      rv(i:i) = string(i:i)
    else
      found = .true.
    endif
    i=i+1
  enddo

  rv(i-1:) = ' '
  
  end function
END MODULE
