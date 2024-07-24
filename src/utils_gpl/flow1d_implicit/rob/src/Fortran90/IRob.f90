! Copyright © 2004, Rijkswaterstaat/RIZA & ®HKV Consultants, All Rights Reserved.
!
! DESCRIPTION
!   Interface RIZA OpenMI Buffer implementation
!
! AUTHOR
!   Johan Ansink, HKV lijn in water

! HISTORY
!   R. Terveer, RIZA : june 2005 changes due to introduction QuantitySet

! $Header: $
! $NoKeywords: $
!
!
module irob

  IMPLICIT NONE

  integer,parameter           :: ROBIDLEN     = 255           ! max length of ID strings

  integer,parameter           :: PROVIDING_ROLE=1
  integer,parameter           :: ACCEPTING_ROLE=2

  INTEGER,PARAMETER           :: ELEMENTTYPE_IDBASED=0
  INTEGER,PARAMETER           :: ELEMENTTYPE_XYPOINT=1
  INTEGER,PARAMETER           :: ELEMENTTYPE_XYLINE=2
  INTEGER,PARAMETER           :: ELEMENTTYPE_XYPOLYLINE=3
  INTEGER,PARAMETER           :: ELEMENTTYPE_XYPOLYGON=4
  INTEGER,PARAMETER           :: ELEMENTTYPE_XYZPOINT=5
  INTEGER,PARAMETER           :: ELEMENTTYPE_XYZLINE=6
  INTEGER,PARAMETER           :: ELEMENTTYPE_XYZPOLYLINE=7
  INTEGER,PARAMETER           :: ELEMENTTYPE_XYZPOLYGON=8
  INTEGER,PARAMETER           :: ELEMENTTYPE_XYZPOLYHEDRON=9


  INTERFACE  
    FUNCTION RobInitialize () result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobInitialize' :: RobInitialize
      integer :: rv
    END FUNCTION 
  END INTERFACE

  INTERFACE
    SUBROUTINE RobFinalize ()
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobFinalize' :: RobFinalize
    END SUBROUTINE 
  END INTERFACE

  INTERFACE
    subroutine RobExchangeitemsTrim ()
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobExchangeitemsTrim' :: RobExchangeitemsTrim
    END subroutine RobExchangeitemsTrim 
  END INTERFACE
  
  INTERFACE
    FUNCTION RobUnitAdd (unitID, unitDescription, conversionFactor, conversionOffset) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobUnitAdd' :: RobUnitAdd
      character (len=*),intent(in) :: unitID
      !DEC$ ATTRIBUTES REFERENCE :: unitID
      character (len=*),intent(in) :: unitDescription
      !DEC$ ATTRIBUTES REFERENCE :: unitDescription
      real(8)          ,intent(in) :: conversionFactor
      real(8)          ,intent(in) :: conversionOffset
      integer                      :: rv
    END FUNCTION
  END INTERFACE
  
  INTERFACE
    FUNCTION RobUnitGetInfo (unitID, unitDescription, conversionFactor, conversionOffset) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobUnitGetInfo' :: RobUnitGetInfo
      character (len=*),intent(in) :: unitID
      !DEC$ ATTRIBUTES REFERENCE :: unitID
      character (len=*),intent(out) :: unitDescription
      !DEC$ ATTRIBUTES REFERENCE :: unitDescription
      real(8)          ,intent(out) :: conversionFactor
      !DEC$ ATTRIBUTES REFERENCE :: conversionFactor
      real(8)          ,intent(out) :: conversionOffset
      !DEC$ ATTRIBUTES REFERENCE :: conversionOffset
      integer                      :: rv
    END FUNCTION
  END INTERFACE
  
  INTERFACE
    FUNCTION RobQuantityAdd (quantityID, quantityDescription, unitID) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobQuantityAdd' :: RobQuantityAdd
      character (len=*),intent(in) :: quantityID
      !DEC$ ATTRIBUTES REFERENCE :: quantityID
      character (len=*),intent(in) :: quantityDescription
      !DEC$ ATTRIBUTES REFERENCE :: quantityDescription
      character (len=*),intent(in) :: unitID
      !DEC$ ATTRIBUTES REFERENCE :: unitID
      integer                      :: rv
    END FUNCTION
  END INTERFACE

  INTERFACE
    FUNCTION RobQuantityAddQuantity (quantitySetID, quantityID, quantityDescription, unitID) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobQuantityAddQuantity' :: RobQuantityAddQuantity
      character (len=*),intent(in) :: quantitySetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitySetID
      character (len=*),intent(in) :: quantityID
      !DEC$ ATTRIBUTES REFERENCE :: quantityID
      character (len=*),intent(in) :: quantityDescription
      !DEC$ ATTRIBUTES REFERENCE :: quantityDescription
      character (len=*),intent(in) :: unitID
      !DEC$ ATTRIBUTES REFERENCE :: unitID
      integer                      :: rv
    END FUNCTION
  END INTERFACE

  INTERFACE
    FUNCTION RobQuantitySetRemoveQuantity (quantitySetID, quantityID) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobQuantitySetRemoveQuantity' :: RobQuantitySetRemoveQuantity
      character (len=*),intent(in) :: quantitySetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitySetID
      character (len=*),intent(in) :: quantityID
      !DEC$ ATTRIBUTES REFERENCE :: quantityID
      integer                      :: rv
    END FUNCTION
  END INTERFACE

  INTERFACE
    FUNCTION RobQuantitySetRemoveAllQuantities (quantitySetID) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobQuantitySetRemoveAllQuantities' :: RobQuantitySetRemoveAllQuantities
      character (len=*),intent(in) :: quantitySetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitySetID
      integer                      :: rv
    END FUNCTION
  END INTERFACE

  INTERFACE
    FUNCTION RobQuantityAddProperty (quantitySetID, quantityID, propertyID, propertyValue) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobQuantityAddProperty' :: RobQuantityAddProperty
      character (len=*),intent(in) :: quantitySetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitySetID
      character (len=*),intent(in) :: quantityID
      !DEC$ ATTRIBUTES REFERENCE :: quantityID
      character (len=*),intent(in) :: propertyID
      !DEC$ ATTRIBUTES REFERENCE :: propertyID
      character (len=*),intent(in) :: propertyValue
      !DEC$ ATTRIBUTES REFERENCE :: propertyValue
      integer                      :: rv
    END FUNCTION
  END INTERFACE

  INTERFACE
    FUNCTION RobQuantitySetPropertyValue (quantitySetID, quantityID, propertyID, propertyValue) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobQuantitySetPropertyValue' :: RobQuantitySetPropertyValue
      character (len=*),intent(in) :: quantitySetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitySetID
      character (len=*),intent(in) :: quantityID
      !DEC$ ATTRIBUTES REFERENCE :: quantityID
      character (len=*),intent(in) :: propertyID
      !DEC$ ATTRIBUTES REFERENCE :: propertyID
      character (len=*),intent(in) :: propertyValue
      !DEC$ ATTRIBUTES REFERENCE :: propertyValue
      integer                      :: rv
    END FUNCTION
  END INTERFACE

  INTERFACE             
    function RobQuantityProperty (quantitySetID, quantityID, propertyID,propertyValue) result(rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobQuantityProperty' :: RobQuantityProperty
      character (len=*),intent(in) :: quantitySetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitySetID
      character (len=*),intent(in) :: quantityID
      !DEC$ ATTRIBUTES REFERENCE :: quantityID
      character (len=*),intent(in) :: propertyID
      !DEC$ ATTRIBUTES REFERENCE :: propertyID
      character (len=*),intent(in) :: propertyValue
      !DEC$ ATTRIBUTES REFERENCE :: propertyValue
      integer                      :: rv
    END function
  END INTERFACE

  INTERFACE
    function RobQuantitySetQuantityID (quantitySetID, quantityOrderNr, QuantityID) result(rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobQuantitySetQuantityID' :: RobQuantitySetQuantityID
      character (len=*),intent(in) :: quantitySetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitySetID
      integer,intent(in)           :: quantityOrderNr
      !$DEC$ ATTRIBUTES VALUE    :: quantityOrderNr
      character (len=*),intent(out) :: quantityID
      !DEC$ ATTRIBUTES REFERENCE :: quantityID
      integer                      :: rv
    END function
  END INTERFACE

  INTERFACE
    FUNCTION RobIsQuantitySet(quantitySetID) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobIsQuantitySet' :: RobIsQuantitySet
      character (len=*),intent(in) :: quantitySetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitySetID
      integer                      :: rv
    END FUNCTION
  END INTERFACE

  INTERFACE
    FUNCTION RobQuantityGetInfo (quantityID, quantityDescription, unitID, NumberOfQuantities) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobQuantityGetInfo' :: RobQuantityGetInfo
      character (len=*),intent(in) :: quantityID
      !DEC$ ATTRIBUTES REFERENCE :: quantityID
      character (len=*),intent(out) :: quantityDescription
      !DEC$ ATTRIBUTES REFERENCE :: quantityDescription
      character (len=*),intent(out) :: unitID
      !DEC$ ATTRIBUTES REFERENCE :: unitID
      integer          ,intent(out) :: NumberOfQuantities
      !DEC$ ATTRIBUTES REFERENCE :: NumberOfQuantities
      integer                       :: rv
    END FUNCTION
  END INTERFACE


  INTERFACE
    !FUNCTION RobElementAdd (elementID, elementDescription, elementType) result (rv)
    FUNCTION RobElementAdd (elementID) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobElementAdd' :: RobElementAdd
      character (len=*),intent(in) :: elementID
      !DEC$ ATTRIBUTES REFERENCE :: elementID
      !character (len=*),intent(in) :: elementDescription
      !!DEC$ ATTRIBUTES REFERENCE :: elementDescription
      !integer          ,intent(in) :: elementType
      integer                      :: rv
    END FUNCTION
  END INTERFACE
  
  INTERFACE
    FUNCTION RobElementsetAdd (elementSetID, elementDescription, elementType, elementID) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobElementsetAdd' :: RobElementsetAdd
      character (len=*),intent(in) :: elementSetID
      !DEC$ ATTRIBUTES REFERENCE :: elementSetID
      character (len=*),intent(in) :: elementDescription
      !DEC$ ATTRIBUTES REFERENCE :: elementDescription
      integer          ,intent(in) :: elementType
      !DEC$ ATTRIBUTES VALUE:: elementType
      character (len=*),intent(in) :: elementID
      !DEC$ ATTRIBUTES REFERENCE :: elementID
      integer                      :: rv
    END FUNCTION
  END INTERFACE
  
  INTERFACE
    FUNCTION RobModelAdd (modelID, modelDescription) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobModelAdd' :: RobModelAdd
      character (len=*),intent(in) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      character (len=*),intent(in) :: modelDescription
      !DEC$ ATTRIBUTES REFERENCE :: modelDescription
      integer                      :: rv
    END FUNCTION
  END INTERFACE
  
  INTERFACE
    FUNCTION RobModelCount () result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobModelCount' :: RobModelCount
      integer                      :: rv
    END FUNCTION
  END INTERFACE
  
  INTERFACE
    FUNCTION RobModelGetInfo (index, modelID,  modelDescription) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobModelGetInfo' :: RobModelGetInfo
      integer          ,intent(in) :: index
      character (len=*),intent(out) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      character (len=*),intent(out) :: modelDescription
      !DEC$ ATTRIBUTES REFERENCE :: modelDescription
      integer                      :: rv
    END FUNCTION
  END INTERFACE
  
  
  INTERFACE
    FUNCTION RobElementsetCount (elementsetID) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobElementsetCount' :: RobElementsetCount
      character (len=*),intent(in) :: elementsetID
      !DEC$ ATTRIBUTES REFERENCE :: elementsetID
      integer                      :: rv
    END FUNCTION
  END INTERFACE
  
  INTERFACE

    FUNCTION RobElementsetGetInfo (elementsetID, elementsetDescription, elementsetType, nElements) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobElementsetGetInfo' :: RobElementsetGetInfo
      character (len=*),intent(in) :: elementsetID
      !DEC$ ATTRIBUTES REFERENCE :: elementsetID
      character (len=*),intent(out) :: elementsetDescription
      !DEC$ ATTRIBUTES REFERENCE :: elementsetDescription
      integer,intent(out)          :: elementSetType
      !DEC$ ATTRIBUTES REFERENCE :: elementsetType
      integer,intent(out)          :: nElements
      !DEC$ ATTRIBUTES REFERENCE :: nElements
      integer                      :: rv
    END FUNCTION
  END INTERFACE
  
  INTERFACE
    FUNCTION RobExchangeitemAdd (modelID, quantityID, elementsetID, role) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobExchangeitemAdd' :: RobExchangeitemAdd
      character (len=*),intent(in) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      character (len=*),intent(in) :: quantityID
      !DEC$ ATTRIBUTES REFERENCE :: quantityID
      character (len=*),intent(in) :: elementsetID
      !DEC$ ATTRIBUTES REFERENCE :: elementsetID
      integer          ,intent(in) :: role
      integer                      :: rv
    END FUNCTION
  END INTERFACE

  
  INTERFACE
    FUNCTION RobExchangeitemCount (modelID) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobExchangeitemCount' :: RobExchangeitemCount
      character (len=*),intent(in) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      integer                      :: rv
    END FUNCTION
  END INTERFACE
  
  INTERFACE
    FUNCTION RobExchangeitemGetInfo (modelID, index, quantityID, elementsetID, role) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobExchangeitemGetInfo' :: RobExchangeitemGetInfo
      character (len=*),intent(in) :: modelID
      integer          ,intent(in) :: index
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      character (len=*),intent(out) :: quantityID
      !DEC$ ATTRIBUTES REFERENCE :: quantityID
      character (len=*),intent(out) :: elementsetID
      !DEC$ ATTRIBUTES REFERENCE :: elementsetID
      integer          ,intent(out) :: role
      !DEC$ ATTRIBUTES REFERENCE :: role
      integer                      :: rv
    END FUNCTION
  END INTERFACE
  
  INTERFACE
    FUNCTION RobPutInt (modelID, quantitySetID, quantityID, elementsetID, role, nElements, iValues) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobPutInt' :: RobPutInt
      character (len=*),intent(in) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      character (len=*),intent(in) :: quantitySetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitySetID
      character (len=*),intent(in) :: quantityID
      !DEC$ ATTRIBUTES REFERENCE :: quantityID
      character (len=*),intent(in) :: elementsetID
      !DEC$ ATTRIBUTES REFERENCE :: elementsetID
      integer          ,intent(in) :: role
      integer          ,intent(in) :: nElements
      integer          ,intent(in) :: iValues(nElements)
      integer                      :: rv
    END FUNCTION
  END INTERFACE

  
  INTERFACE
    FUNCTION RobPutReal (modelID, quantitySetID, quantityID, elementsetID, role, nElements, rValues) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobPutReal' :: RobPutReal
      character (len=*),intent(in) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      character (len=*),intent(in) :: quantitySetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitySetID
      character (len=*),intent(in) :: quantityID
      !DEC$ ATTRIBUTES REFERENCE :: quantityID
      character (len=*),intent(in) :: elementsetID
      !DEC$ ATTRIBUTES REFERENCE :: elementsetID
      integer          ,intent(in) :: role
      integer          ,intent(in) :: nElements
      real(4)          ,intent(in) :: rValues(nElements)
      integer                      :: rv
    END FUNCTION
  END INTERFACE

  
  INTERFACE
    FUNCTION RobPutDouble (modelID, quantitySetID, quantityID, elementsetID, role, nElements, dValues) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobPutDouble' :: RobPutDouble
      character (len=*),intent(in) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      character (len=*),intent(in) :: quantitySetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitySetID
      character (len=*),intent(in) :: quantityID
      !DEC$ ATTRIBUTES REFERENCE :: quantityID
      character (len=*),intent(in) :: elementsetID
      !DEC$ ATTRIBUTES REFERENCE :: elementsetID
      integer          ,intent(in) :: role
      integer          ,intent(in) :: nElements
      real(8)          ,intent(in) :: dValues(nElements)
      integer                      :: rv
    END FUNCTION
  END INTERFACE

  
  INTERFACE
    FUNCTION RobGetDouble (modelID, quantitySetID, quantityID, elementsetID, role, nElements, dValues) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobGetDouble' :: RobGetDouble
      character (len=*),intent(in) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      character (len=*),intent(in) :: quantitySetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitySetID
      character (len=*),intent(in) :: quantityID
      !DEC$ ATTRIBUTES REFERENCE :: quantityID
      character (len=*),intent(in) :: elementsetID
      !DEC$ ATTRIBUTES REFERENCE :: elementsetID
      integer          ,intent(in) :: role
      integer          ,intent(in) :: nElements
      real(8)          ,intent(out) :: dValues(nElements)
      !DEC$ ATTRIBUTES REFERENCE :: dValues
      integer                      :: rv
    END FUNCTION
  END INTERFACE
 
END MODULE
