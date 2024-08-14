! Copyright © 2004, Rijkswaterstaat/RIZA & ®HKV Consultants, All Rights Reserved.
!
! DESCRIPTION
!   Interface RIZA OpenMI Buffer implementation
!
! AUTHOR
!   Johan Ansink, HKV lijn in water

! HISTORY
!   R. Terveer, RIZA : june 2005 changes due to introduction Quantityset

! $Header: $
! $NoKeywords: $
!
!
module irob

  IMPLICIT NONE

  real(kind=8),parameter      :: MISSING_VALUE = huge(MISSING_VALUE)
  integer,parameter           :: ROBFILELEN   = 255           ! max length of file name
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
    FUNCTION RobInitialize (logFileName) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobInitialize' :: RobInitialize
      character (len=*),intent(in) :: logFileName
      !DEC$ ATTRIBUTES REFERENCE :: logFileName
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
    FUNCTION RobQuantityAddQuantity (quantitysetID, quantityID, quantityDescription, unitID) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobQuantityAddQuantity' :: RobQuantityAddQuantity
      character (len=*),intent(in) :: quantitysetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitysetID
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
    FUNCTION RobQuantitysetRemoveQuantity (quantitysetID, quantityID) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobQuantitysetRemoveQuantity' :: RobQuantitysetRemoveQuantity
      character (len=*),intent(in) :: quantitysetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitysetID
      character (len=*),intent(in) :: quantityID
      !DEC$ ATTRIBUTES REFERENCE :: quantityID
      integer                      :: rv
    END FUNCTION
  END INTERFACE

  INTERFACE
    FUNCTION RobQuantitysetRemoveAllQuantities (quantitysetID) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobQuantitysetRemoveAllQuantities' :: RobQuantitysetRemoveAllQuantities
      character (len=*),intent(in) :: quantitysetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitysetID
      integer                      :: rv
    END FUNCTION
  END INTERFACE

  INTERFACE
    FUNCTION RobQuantityAddProperty (quantitysetID, quantityID, propertyID, propertyValue) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobQuantityAddProperty' :: RobQuantityAddProperty
      character (len=*),intent(in) :: quantitysetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitysetID
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
    FUNCTION RobQuantityPropertySetValue (quantitysetID, quantityID, propertyID, propertyValue) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobQuantityPropertySetValue' :: RobQuantityPropertySetValue
      character (len=*),intent(in) :: quantitysetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitysetID
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
    function RobQuantityProperty (quantitysetID, quantityID, propertyID,propertyValue) result(rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobQuantityProperty' :: RobQuantityProperty
      character (len=*),intent(in) :: quantitysetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitysetID
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
    function RobQuantitysetQuantityID (quantitysetID, quantityOrderNr, QuantityID) result(rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobQuantitysetQuantityID' :: RobQuantitysetQuantityID
      character (len=*),intent(in) :: quantitysetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitysetID
      integer,intent(in)           :: quantityOrderNr
      !$DEC$ ATTRIBUTES VALUE    :: quantityOrderNr
      character (len=*),intent(out) :: quantityID
      !DEC$ ATTRIBUTES REFERENCE :: quantityID
      integer                      :: rv
    END function
  END INTERFACE

  INTERFACE
    FUNCTION RobIsQuantityset(quantitysetID) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobIsQuantityset' :: RobIsQuantityset
      character (len=*),intent(in) :: quantitysetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitysetID
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
    FUNCTION RobElementsetAdd (modelID, elementSetID, elementDescription, elementType) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobElementsetAdd' :: RobElementsetAdd
      character (len=*),intent(in) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      character (len=*),intent(in) :: elementSetID
      !DEC$ ATTRIBUTES REFERENCE :: elementSetID
      character (len=*),intent(in) :: elementDescription
      !DEC$ ATTRIBUTES REFERENCE :: elementDescription
      integer          ,intent(in) :: elementType
      !DEC$ ATTRIBUTES VALUE:: elementType
      integer                      :: rv
    END FUNCTION
  END INTERFACE
  
  INTERFACE
    FUNCTION RobElementsetAddIDBasedMultiple (modelID, elementSetID, elementDescription, elementCount, elementIDs) result (rv)
      character (len=*),intent(in) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobElementsetAddIDBasedMultiple' :: RobElementsetAddIDBasedMultiple
      character (len=*),intent(in) :: elementSetID
      !DEC$ ATTRIBUTES REFERENCE :: elementSetID
      character (len=*),intent(in) :: elementDescription
      !DEC$ ATTRIBUTES REFERENCE :: elementDescription
      integer          ,intent(in) :: elementCount
      !DEC$ ATTRIBUTES VALUE:: elementCount
      character (len=*), dimension(:), intent(in) :: elementIDs
      !DEC$ ATTRIBUTES REFERENCE :: elementIDs
      integer                      :: rv
    END FUNCTION
  END INTERFACE
  
  INTERFACE
    FUNCTION RobElementsetAddIDBasedSingle (modelID, elementSetID, elementDescription) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobElementsetAddIDBasedSingle' :: RobElementsetAddIDBasedSingle
      character (len=*),intent(in) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      character (len=*),intent(in) :: elementSetID
      !DEC$ ATTRIBUTES REFERENCE :: elementSetID
      character (len=*),intent(in) :: elementDescription
      !DEC$ ATTRIBUTES REFERENCE :: elementDescription
      integer                      :: rv
    END FUNCTION
  END INTERFACE
  
  INTERFACE
    FUNCTION RobElementsetAddElementIDBased (modelID, elementSetID, elementID) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobElementsetAddElementIDBased' :: RobElementsetAddElementIDBased
      character (len=*),intent(in) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      character (len=*),intent(in) :: elementSetID
      !DEC$ ATTRIBUTES REFERENCE :: elementSetID
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
    FUNCTION RobElementsetElementCount (modelID,  elementsetID) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobElementsetElementCount' :: RobElementsetElementCount
      character (len=*),intent(in) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      character (len=*),intent(in) :: elementsetID
      !DEC$ ATTRIBUTES REFERENCE :: elementsetID
      integer                      :: rv
    END FUNCTION
  END INTERFACE
  
  INTERFACE

    FUNCTION RobElementsetGetInfo (modelID, elementsetID, elementsetDescription, elementsetType, nElements) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobElementsetGetInfo' :: RobElementsetGetInfo
      character (len=*),intent(in) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
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
    SUBROUTINE RobExchangeitemsResize()
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobExchangeitemsResize' :: RobExchangeitemsResize
    END SUBROUTINE
  END INTERFACE
  
  INTERFACE
    FUNCTION RobPutInt (modelID, quantitysetID, quantityID, elementsetID, role, nElements, iValues) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobPutInt' :: RobPutInt
      character (len=*),intent(in) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      character (len=*),intent(in) :: quantitysetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitysetID
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
    FUNCTION RobPutReal (modelID, quantitysetID, quantityID, elementsetID, role, nElements, rValues) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobPutReal' :: RobPutReal
      character (len=*),intent(in) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      character (len=*),intent(in) :: quantitysetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitysetID
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
    FUNCTION RobPutDouble (modelID, quantitysetID, quantityID, elementsetID, role, nElements, dValues) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobPutDouble' :: RobPutDouble
      character (len=*),intent(in) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      character (len=*),intent(in) :: quantitysetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitysetID
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
    FUNCTION RobGetDouble (modelID, quantitysetID, quantityID, elementsetID, role, nElements, dValues) result (rv)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobGetDouble' :: RobGetDouble
      character (len=*),intent(in) :: modelID
      !DEC$ ATTRIBUTES REFERENCE :: modelID
      character (len=*),intent(in) :: quantitysetID
      !DEC$ ATTRIBUTES REFERENCE :: quantitysetID
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
 
  INTERFACE  
    SUBROUTINE RobDump (dumpFileName)
      !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'RobDump' :: RobDump
      character (len=*),intent(in) :: dumpFileName
      !DEC$ ATTRIBUTES REFERENCE :: dumpFileName
    END SUBROUTINE 
  END INTERFACE

END MODULE
