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
!   R. Terveer, RIZA : june 2005 changes due to introduction QuantitySet & QuantityProperties
!
! $Header: $
! $NoKeywords: $
!
!
module frob

  use irob

  IMPLICIT NONE

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
  character(len=ROBIDLEN+1)   :: CquantitySetID
  character(len=ROBIDLEN+1)   :: CquantitySetDescription
  character(len=ROBIDLEN+1)   :: CpropertyID
  character(len=ROBIDLEN+1)   :: CpropertyValue

  
  CONTAINS

  FUNCTION FRobInitialize () result (rv)
    integer :: rv
    rv = RobInitialize()
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


  FUNCTION FRobQuantityAddQuantity (quantitySetID, quantityID, quantityDescription, unitID) result (rv)
    character (len=*),intent(in) :: quantitySetID
    character (len=*),intent(in) :: quantityID
    character (len=*),intent(in) :: quantityDescription
    character (len=*),intent(in) :: unitID
    integer                      :: rv

    CquantitySetID       = trim(quantitySetID)//char(0)
    CquantityID          = trim(quantityID)//char(0)
    CquantityDescription = trim(quantityDescription)//char(0)
    CunitID              = trim(unitID)//char(0)

    rv =  RobQuantityAddQuantity (CquantitySetID, CquantityID, CquantityDescription, CunitID)
  END FUNCTION


  FUNCTION FRobQuantitySetRemoveQuantity (quantitySetID, quantityID) result (rv)
    character (len=*),intent(in) :: quantitySetID
    character (len=*),intent(in) :: quantityID
    integer                      :: rv

    CquantitySetID       = trim(quantitySetID)//char(0)
    CquantityID          = trim(quantityID)//char(0)

    rv =  RobQuantitySetRemoveQuantity (CquantitySetID, CquantityID)
  END FUNCTION

  FUNCTION FRobQuantitySetRemoveAllQuantities (quantitySetID) result (rv)
    character (len=*),intent(in) :: quantitySetID
    integer                      :: rv

    CquantitySetID       = trim(quantitySetID)//char(0)
    rv =  RobQuantitySetRemoveAllQuantities (CquantitySetID)
  END FUNCTION

  function FRobQuantityAddProperty(quantitySetID, quantityID, propertyID, propertyValue) result(rv)
    character (len=*),intent(in) :: quantitySetID,quantityID, propertyID, propertyValue
    integer                      :: rv

    CquantitySetID = trim(quantitySetID)//char(0)
    CquantityID    = trim(quantityID)//char(0)
    CpropertyID    = trim(propertyID)//char(0)
    CpropertyValue = trim(propertyValue)//char(0)

    rv = RobQuantityAddProperty(CquantitySetID, CquantityId,CpropertyID,CpropertyValue)
  end function FRobQuantityAddProperty

  function FRobQuantitySetPropertyValue(quantitySetID, quantityID, propertyID, propertyValue) result(rv)
    character (len=*),intent(in) :: quantitySetID,quantityID, propertyID, propertyValue
    integer                      :: rv

    CquantitySetID = trim(quantitySetID)//char(0)
    CquantityID    = trim(quantityID)//char(0)
    CpropertyID    = trim(propertyID)//char(0)
    CpropertyValue = trim(propertyValue)//char(0)

    rv = RobQuantitySetPropertyValue(CquantitySetID, CquantityId,CpropertyID,CpropertyValue)
  end function FRobQuantitySetPropertyValue


  function FRobQuantityPropertyValue(quantitySetID, quantityID, propertyID) result(propertyValue)
    character (len=*),intent(in)       :: quantitySetID, quantityID, propertyID
    character(len=ROBIDLEN+1)          :: propertyValue

    CquantitySetID = trim(quantitySetID)//char(0)
    CquantityID    = trim(quantityID)//char(0)
    CpropertyID    = trim(propertyID)//char(0)
    call MakeEmptyCstring(CpropertyValue)

    if( RobQuantityProperty(CquantitySetID, CquantityId,CpropertyID, CpropertyValue) == 1) then
       propertyValue = Cstring2Fstring(CpropertyValue)
    else
       propertyValue = ''
    endif      
    !rv = RobQuantityPropertyValue(CquantitySetID, CquantityId,CpropertyID)
  end function FRobQuantityPropertyValue

  function FRobQuantitySetGetQuantityID(quantitySetID, quantityOrderNr) result(quantityID)
    character (len=*),intent(in)       :: quantitySetID
    integer,intent(in)                 :: quantityOrderNr
    character(len=ROBIDLEN+1)          :: quantityID

    CquantitySetID = trim(quantitySetID)//char(0)
    call MakeEmptyCstring(CquantityID)

    if(RobQuantitySetQuantityID(CquantitySetID,quantityOrderNr,CquantityID) == 1) then
      quantityID=Cstring2Fstring(CquantityID)
    else
      quantityID = ''
    endif    
  end function FRobQuantitySetGetQuantityID

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


  FUNCTION FRobElementAdd (elementID) result (rv)
    character (len=*),intent(in) :: elementID
    integer                      :: rv

    CelementID          = trim(elementID)//char(0)

    rv =  RobElementAdd (CelementID)
  END FUNCTION

  FUNCTION FRobElementSetAdd (elementSetID, elementDescription, elementType, elementID) result (rv)
    character (len=*),intent(in) :: elementSetID
    character (len=*),intent(in) :: elementDescription
    integer          ,intent(in) :: elementType
    character (len=*),intent(in) :: elementID
    integer                      :: rv

    CelementSetID       = trim(elementSetID)//char(0)
    CelementDescription = trim(elementDescription)//char(0)
    CelementID          = trim(elementID)//char(0)

    rv = RobElementSetAdd(CelementSetID, CelementDescription, elementType, CelementID)
  END FUNCTION FRobElementSetAdd

  
  FUNCTION FRobElementsetCount (elementsetID) result (rv)
    character (len=*),intent(in) :: elementsetID
    integer                      :: rv

    CelementsetID = trim(elementsetID)//char(0)
    
    rv = RobElementsetCount (elementsetID)

  END FUNCTION

  FUNCTION FRobElementsetGetInfo (elementsetID, elementsetDescription, elementsetType, nElements) result (rv)
    character (len=*),intent(in)  :: elementsetID
    character (len=*),intent(out) :: elementsetDescription
    integer          ,intent(out) :: elementsetType
    integer          ,intent(out) :: nElements
    integer                       :: rv

    CelementsetID = trim(elementsetID)//char(0)
    call MakeEmptyCstring(CelementSetDescription)

    rv =  RobElementsetGetInfo (CelementsetID, CelementsetDescription, elementSetType,nElements)
 
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

  FUNCTION FRobPutInt (modelID, quantitySetId, quantityID, elementsetID, role, nElements, iValues) result (rv)
    character (len=*),intent(in) :: modelID
    character (len=*),intent(in) :: quantitySetID, quantityID
    character (len=*),intent(in) :: elementsetID
    integer          ,intent(in) :: role
    integer          ,intent(in) :: nElements
    integer          ,intent(in) :: iValues(nElements)
    integer                      :: rv

    CmodelID       = trim(modelID)//char(0)
    CquantitySetID = trim(quantitySetID)//char(0)
    CquantityID    = trim(quantityID)//char(0)
    CelementsetID  = trim(elementsetID)//char(0)
    
    rv =  RobPutInt (CmodelID, CquantitySetID, CquantityID, CelementsetID, role, nElements, iValues)

  END FUNCTION


  FUNCTION FRobPutReal (modelID, quantitySetID, quantityID, elementsetID, role, nElements, fValues) result (rv)
    character (len=*),intent(in) :: modelID
    character (len=*),intent(in) :: quantityID,quantitySetID
    character (len=*),intent(in) :: elementsetID
    integer          ,intent(in) :: role
    integer          ,intent(in) :: nElements
    real(4)          ,intent(in) :: fValues(nElements)
    integer                      :: rv

    CmodelID       = trim(modelID)//char(0)
    CquantitySetID = trim(quantitySetID)//char(0)
    CquantityID    = trim(quantityID)//char(0)
    CelementsetID  = trim(elementsetID)//char(0)
    
    rv =  RobPutReal (CmodelID, CquantitySetID, CquantityID, CelementsetID, role, nElements, fValues)

  END FUNCTION

  
  FUNCTION FRobPutDouble (modelID, quantitySetID, quantityID, elementsetID, role, nElements, dValues) result (rv)
    character (len=*),intent(in) :: modelID
    character (len=*),intent(in) :: quantityID,quantitySetID
    character (len=*),intent(in) :: elementsetID
    integer          ,intent(in) :: role
    integer          ,intent(in) :: nElements
    real(8)          ,intent(in) :: dValues(nElements)
    integer                      :: rv

    CmodelID       = trim(modelID)//char(0)
    CquantitySetID = trim(quantitySetID)//char(0)
    CquantityID    = trim(quantityID)//char(0)
    CelementsetID  = trim(elementsetID)//char(0)
    
    rv =  RobPutDouble (CmodelID, CquantitySetID, CquantityID, CelementsetID, role, nElements, dValues)

  END FUNCTION


  FUNCTION FRobGetDouble(modelID, quantitySetID, quantityID, elementsetID, role, nElements, dValues) result (rv)
    character (len=*),intent(in) :: modelID
    character (len=*),intent(in) :: quantityID,quantitySetID
    character (len=*),intent(in) :: elementsetID
    integer          ,intent(in) :: role
    integer          ,intent(in)  :: nElements
    real(8)          ,intent(out) :: dValues(nElements)
    integer                      :: rv

    CmodelID       = trim(modelID)//char(0)
    CquantitySetID = trim(quantitySetID)//char(0)
    CquantityID    = trim(quantityID)//char(0)
    CelementsetID  = trim(elementsetID)//char(0)
    
    rv =  RobGetDouble (CmodelID, CquantitySetID, CquantityID, CelementsetID, role, nElements, dValues)

  END FUNCTION

  
  SUBROUTINE FRobFinalize
    CALL RobFinalize
  END SUBROUTINE

  subroutine FRobExchangeItemsTrim()
    call RobExchangeItemsTrim()
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
