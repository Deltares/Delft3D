INTERFACE
  FUNCTION gdaldatatypeunion(etype1, etype2) BIND(C,name='GDALDataTypeUnion')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALDataTypeUnion
  INTEGER(kind=c_int),VALUE :: etype1 ! GDALDataType
  INTEGER(kind=c_int),VALUE :: etype2 ! GDALDataType
  INTEGER(kind=c_int) :: gdaldatatypeunion ! GDALDataType
  END FUNCTION gdaldatatypeunion
END INTERFACE

INTERFACE
  FUNCTION gdalgetdatatypesize(edatatype) BIND(C,name='GDALGetDataTypeSize')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetDataTypeSize
  INTEGER(kind=c_int),VALUE :: edatatype ! GDALDataType
  INTEGER(kind=c_int) :: gdalgetdatatypesize
  END FUNCTION gdalgetdatatypesize
END INTERFACE

INTERFACE
  FUNCTION gdaldatatypeiscomplex(edatatype) BIND(C,name='GDALDataTypeIsComplex')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALDataTypeIsComplex
  INTEGER(kind=c_int),VALUE :: edatatype ! GDALDataType
  INTEGER(kind=c_int) :: gdaldatatypeiscomplex
  END FUNCTION gdaldatatypeiscomplex
END INTERFACE

INTERFACE
  FUNCTION gdalgetdatatypename(edatatype) BIND(C,name='GDALGetDataTypeName')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetDataTypeName
  INTEGER(kind=c_int),VALUE :: edatatype ! GDALDataType
  TYPE(c_ptr) :: gdalgetdatatypename ! char*
  END FUNCTION gdalgetdatatypename
END INTERFACE

INTERFACE
  FUNCTION gdalgetdatatypebyname(pszname) BIND(C,name='GDALGetDataTypeByName')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetDataTypeByName
  CHARACTER(kind=c_char),INTENT(in) :: pszname(*)
  INTEGER(kind=c_int) :: gdalgetdatatypebyname ! GDALDataType
  END FUNCTION gdalgetdatatypebyname
END INTERFACE

INTERFACE
  FUNCTION gdalgetpaletteinterpretationname(einterp) BIND(C,name='GDALGetPaletteInterpretationName')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetPaletteInterpretationName
  INTEGER(kind=c_int),VALUE :: einterp ! GDALPaletteInterp
  TYPE(c_ptr) :: gdalgetpaletteinterpretationname ! char*
  END FUNCTION gdalgetpaletteinterpretationname
END INTERFACE

INTERFACE
  FUNCTION gdalgetcolorinterpretationname(einterp) BIND(C,name='GDALGetColorInterpretationName')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetColorInterpretationName
  INTEGER(kind=c_int),VALUE :: einterp ! GDALColorInterp
  TYPE(c_ptr) :: gdalgetcolorinterpretationname ! char*
  END FUNCTION gdalgetcolorinterpretationname
END INTERFACE

INTERFACE
  FUNCTION gdalgetcolorinterpretationbyname(pszname) BIND(C,name='GDALGetColorInterpretationByName')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetColorInterpretationByName
  CHARACTER(kind=c_char),INTENT(in) :: pszname(*)
  INTEGER(kind=c_int) :: gdalgetcolorinterpretationbyname ! GDALColorInterp
  END FUNCTION gdalgetcolorinterpretationbyname
END INTERFACE

INTERFACE
  SUBROUTINE gdalallregister() BIND(C,name='GDALAllRegister')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALAllRegister
  END SUBROUTINE gdalallregister
END INTERFACE

INTERFACE
  FUNCTION gdalcreate(hdriver, pszfilename, nxsize, nysize, nbands, ebandtype, papszoptions) BIND(C,name='GDALCreate')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALCreate
  TYPE(gdaldriverh),VALUE :: hdriver
  CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*)
  INTEGER(kind=c_int),VALUE :: nxsize
  INTEGER(kind=c_int),VALUE :: nysize
  INTEGER(kind=c_int),VALUE :: nbands
  INTEGER(kind=c_int),VALUE :: ebandtype ! GDALDataType
  TYPE(c_ptr),VALUE :: papszoptions ! TYPE(c_ptr_ptr)
  TYPE(gdaldataseth) :: gdalcreate
  END FUNCTION gdalcreate
END INTERFACE

INTERFACE
  FUNCTION gdalcreatecopy(hdriver, pszfilename, hsrcds, bstrict, papszoptions, pfnprogress, pprogressdata) &
   BIND(C,name='GDALCreateCopy')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALCreateCopy
  TYPE(gdaldriverh),VALUE :: hdriver
  CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*)
  TYPE(gdaldataseth),VALUE :: hsrcds
  INTEGER(kind=c_int),VALUE :: bstrict
  TYPE(c_ptr),VALUE :: papszoptions ! TYPE(c_ptr_ptr)
  TYPE(c_ptr),VALUE :: pfnprogress
  TYPE(c_ptr),VALUE :: pprogressdata ! void*
  TYPE(gdaldataseth) :: gdalcreatecopy
  END FUNCTION gdalcreatecopy
END INTERFACE

INTERFACE
  FUNCTION gdalidentifydriver(pszfilename, papszfilelist) BIND(C,name='GDALIdentifyDriver')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALIdentifyDriver
  CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*)
  TYPE(c_ptr),VALUE :: papszfilelist ! TYPE(c_ptr_ptr)
  TYPE(gdaldriverh) :: gdalidentifydriver
  END FUNCTION gdalidentifydriver
END INTERFACE

INTERFACE
  FUNCTION gdalopen(pszfilename, eaccess) BIND(C,name='GDALOpen')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALOpen
  CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*)
  INTEGER(kind=c_int),VALUE :: eaccess ! GDALAccess
  TYPE(gdaldataseth) :: gdalopen
  END FUNCTION gdalopen
END INTERFACE

INTERFACE
  FUNCTION gdalopenshared(pszfilename, eaccess) BIND(C,name='GDALOpenShared')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALOpenShared
  CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*)
  INTEGER(kind=c_int),VALUE :: eaccess ! GDALAccess
  TYPE(gdaldataseth) :: gdalopenshared
  END FUNCTION gdalopenshared
END INTERFACE

INTERFACE
  FUNCTION gdalgetdriverbyname(pszname) BIND(C,name='GDALGetDriverByName')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetDriverByName
  CHARACTER(kind=c_char),INTENT(in) :: pszname(*)
  TYPE(gdaldriverh) :: gdalgetdriverbyname
  END FUNCTION gdalgetdriverbyname
END INTERFACE

INTERFACE
  FUNCTION gdalgetdrivercount() BIND(C,name='GDALGetDriverCount')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetDriverCount
  INTEGER(kind=c_int) :: gdalgetdrivercount
  END FUNCTION gdalgetdrivercount
END INTERFACE

INTERFACE
  FUNCTION gdalgetdriver(idriver) BIND(C,name='GDALGetDriver')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetDriver
  INTEGER(kind=c_int),VALUE :: idriver
  TYPE(gdaldriverh) :: gdalgetdriver
  END FUNCTION gdalgetdriver
END INTERFACE

INTERFACE
  SUBROUTINE gdaldestroydriver(hdriver) BIND(C,name='GDALDestroyDriver')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALDestroyDriver
  TYPE(gdaldriverh),VALUE :: hdriver
  END SUBROUTINE gdaldestroydriver
END INTERFACE

INTERFACE
  FUNCTION gdalregisterdriver(hdriver) BIND(C,name='GDALRegisterDriver')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALRegisterDriver
  TYPE(gdaldriverh),VALUE :: hdriver
  INTEGER(kind=c_int) :: gdalregisterdriver
  END FUNCTION gdalregisterdriver
END INTERFACE

INTERFACE
  SUBROUTINE gdalderegisterdriver(hdriver) BIND(C,name='GDALDeregisterDriver')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALDeregisterDriver
  TYPE(gdaldriverh),VALUE :: hdriver
  END SUBROUTINE gdalderegisterdriver
END INTERFACE

INTERFACE
  SUBROUTINE gdaldestroydrivermanager() BIND(C,name='GDALDestroyDriverManager')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALDestroyDriverManager
  END SUBROUTINE gdaldestroydrivermanager
END INTERFACE

INTERFACE
  FUNCTION gdaldeletedataset(hdriver, pszfilename) BIND(C,name='GDALDeleteDataset')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALDeleteDataset
  TYPE(gdaldriverh),VALUE :: hdriver
  CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*)
  INTEGER(kind=c_int) :: gdaldeletedataset ! CPLErr
  END FUNCTION gdaldeletedataset
END INTERFACE

INTERFACE
  FUNCTION gdalrenamedataset(hdriver, psznewname, pszoldname) BIND(C,name='GDALRenameDataset')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALRenameDataset
  TYPE(gdaldriverh),VALUE :: hdriver
  CHARACTER(kind=c_char),INTENT(in) :: psznewname(*)
  CHARACTER(kind=c_char),INTENT(in) :: pszoldname(*)
  INTEGER(kind=c_int) :: gdalrenamedataset ! CPLErr
  END FUNCTION gdalrenamedataset
END INTERFACE

INTERFACE
  FUNCTION gdalcopydatasetfiles(hdriver, psznewname, pszoldname) BIND(C,name='GDALCopyDatasetFiles')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALCopyDatasetFiles
  TYPE(gdaldriverh),VALUE :: hdriver
  CHARACTER(kind=c_char),INTENT(in) :: psznewname(*)
  CHARACTER(kind=c_char),INTENT(in) :: pszoldname(*)
  INTEGER(kind=c_int) :: gdalcopydatasetfiles ! CPLErr
  END FUNCTION gdalcopydatasetfiles
END INTERFACE

INTERFACE
  FUNCTION gdalvalidatecreationoptions(hdriver, papszcreationoptions) BIND(C,name='GDALValidateCreationOptions')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALValidateCreationOptions
  TYPE(gdaldriverh),VALUE :: hdriver
  TYPE(c_ptr),VALUE :: papszcreationoptions ! TYPE(c_ptr_ptr)
  INTEGER(kind=c_int) :: gdalvalidatecreationoptions
  END FUNCTION gdalvalidatecreationoptions
END INTERFACE

INTERFACE
  FUNCTION gdalgetdrivershortname(hdriver) BIND(C,name='GDALGetDriverShortName')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetDriverShortName
  TYPE(gdaldriverh),VALUE :: hdriver
  TYPE(c_ptr) :: gdalgetdrivershortname ! char*
  END FUNCTION gdalgetdrivershortname
END INTERFACE

INTERFACE
  FUNCTION gdalgetdriverlongname(hdriver) BIND(C,name='GDALGetDriverLongName')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetDriverLongName
  TYPE(gdaldriverh),VALUE :: hdriver
  TYPE(c_ptr) :: gdalgetdriverlongname ! char*
  END FUNCTION gdalgetdriverlongname
END INTERFACE

INTERFACE
  FUNCTION gdalgetdriverhelptopic(hdriver) BIND(C,name='GDALGetDriverHelpTopic')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetDriverHelpTopic
  TYPE(gdaldriverh),VALUE :: hdriver
  TYPE(c_ptr) :: gdalgetdriverhelptopic ! char*
  END FUNCTION gdalgetdriverhelptopic
END INTERFACE

INTERFACE
  FUNCTION gdalgetdrivercreationoptionlist(hdriver) BIND(C,name='GDALGetDriverCreationOptionList')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetDriverCreationOptionList
  TYPE(gdaldriverh),VALUE :: hdriver
  TYPE(c_ptr) :: gdalgetdrivercreationoptionlist ! char*
  END FUNCTION gdalgetdrivercreationoptionlist
END INTERFACE

INTERFACE
  FUNCTION gdalinvgeotransform(padfgeotransformin, padfinvgeotransformout) BIND(C,name='GDALInvGeoTransform')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALInvGeoTransform
  REAL(kind=c_double) :: padfgeotransformin(*)
  REAL(kind=c_double) :: padfinvgeotransformout(*)
  INTEGER(kind=c_int) :: gdalinvgeotransform
  END FUNCTION gdalinvgeotransform
END INTERFACE

INTERFACE
  FUNCTION gdalgetmetadata(hobject, pszdomain) BIND(C,name='GDALGetMetadata')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetMetadata
  TYPE(gdalmajorobjecth),VALUE :: hobject
  CHARACTER(kind=c_char),INTENT(in) :: pszdomain(*)
  TYPE(c_ptr) :: gdalgetmetadata ! TYPE(c_ptr_ptr)
  END FUNCTION gdalgetmetadata
END INTERFACE

INTERFACE
  FUNCTION gdalsetmetadata(hobject, papszmd, pszdomain) BIND(C,name='GDALSetMetadata')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSetMetadata
  TYPE(gdalmajorobjecth),VALUE :: hobject
  TYPE(c_ptr),VALUE :: papszmd ! TYPE(c_ptr_ptr)
  CHARACTER(kind=c_char),INTENT(in) :: pszdomain(*)
  INTEGER(kind=c_int) :: gdalsetmetadata ! CPLErr
  END FUNCTION gdalsetmetadata
END INTERFACE

INTERFACE
  FUNCTION gdalgetmetadataitem(hobject, pszname, pszdomain) BIND(C,name='GDALGetMetadataItem')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetMetadataItem
  TYPE(gdalmajorobjecth),VALUE :: hobject
  CHARACTER(kind=c_char),INTENT(in) :: pszname(*)
  CHARACTER(kind=c_char),INTENT(in) :: pszdomain(*)
  TYPE(c_ptr) :: gdalgetmetadataitem ! char*
  END FUNCTION gdalgetmetadataitem
END INTERFACE

INTERFACE
  FUNCTION gdalsetmetadataitem(hobject, pszname, pszvalue, pszdomain) BIND(C,name='GDALSetMetadataItem')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSetMetadataItem
  TYPE(gdalmajorobjecth),VALUE :: hobject
  CHARACTER(kind=c_char),INTENT(in) :: pszname(*)
  CHARACTER(kind=c_char),INTENT(in) :: pszvalue(*)
  CHARACTER(kind=c_char),INTENT(in) :: pszdomain(*)
  INTEGER(kind=c_int) :: gdalsetmetadataitem ! CPLErr
  END FUNCTION gdalsetmetadataitem
END INTERFACE

INTERFACE
  FUNCTION gdalgetdescription(hobject) BIND(C,name='GDALGetDescription')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetDescription
  TYPE(gdalmajorobjecth),VALUE :: hobject
  TYPE(c_ptr) :: gdalgetdescription ! char*
  END FUNCTION gdalgetdescription
END INTERFACE

INTERFACE
  SUBROUTINE gdalsetdescription(hobject, psznewdesc) BIND(C,name='GDALSetDescription')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSetDescription
  TYPE(gdalmajorobjecth),VALUE :: hobject
  CHARACTER(kind=c_char),INTENT(in) :: psznewdesc(*)
  END SUBROUTINE gdalsetdescription
END INTERFACE

INTERFACE
  FUNCTION gdalgetdatasetdriver(hdataset) BIND(C,name='GDALGetDatasetDriver')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetDatasetDriver
  TYPE(gdaldataseth),VALUE :: hdataset
  TYPE(gdaldriverh) :: gdalgetdatasetdriver
  END FUNCTION gdalgetdatasetdriver
END INTERFACE

INTERFACE
  FUNCTION gdalgetfilelist(hds) BIND(C,name='GDALGetFileList')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetFileList
  TYPE(gdaldataseth),VALUE :: hds
  TYPE(c_ptr) :: gdalgetfilelist ! TYPE(c_ptr_ptr)
  END FUNCTION gdalgetfilelist
END INTERFACE

INTERFACE
  SUBROUTINE gdalclose(hds) BIND(C,name='GDALClose')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALClose
  TYPE(gdaldataseth),VALUE :: hds
  END SUBROUTINE gdalclose
END INTERFACE

INTERFACE
  FUNCTION gdalgetrasterxsize(hdataset) BIND(C,name='GDALGetRasterXSize')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterXSize
  TYPE(gdaldataseth),VALUE :: hdataset
  INTEGER(kind=c_int) :: gdalgetrasterxsize
  END FUNCTION gdalgetrasterxsize
END INTERFACE

INTERFACE
  FUNCTION gdalgetrasterysize(hdataset) BIND(C,name='GDALGetRasterYSize')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterYSize
  TYPE(gdaldataseth),VALUE :: hdataset
  INTEGER(kind=c_int) :: gdalgetrasterysize
  END FUNCTION gdalgetrasterysize
END INTERFACE

INTERFACE
  FUNCTION gdalgetrastercount(hdataset) BIND(C,name='GDALGetRasterCount')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterCount
  TYPE(gdaldataseth),VALUE :: hdataset
  INTEGER(kind=c_int) :: gdalgetrastercount
  END FUNCTION gdalgetrastercount
END INTERFACE

INTERFACE
  FUNCTION gdalgetrasterband(hds, nbandid) BIND(C,name='GDALGetRasterBand')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterBand
  TYPE(gdaldataseth),VALUE :: hds
  INTEGER(kind=c_int),VALUE :: nbandid
  TYPE(gdalrasterbandh) :: gdalgetrasterband
  END FUNCTION gdalgetrasterband
END INTERFACE

INTERFACE
  FUNCTION gdaladdband(hds, etype, papszoptions) BIND(C,name='GDALAddBand')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALAddBand
  TYPE(gdaldataseth),VALUE :: hds
  INTEGER(kind=c_int),VALUE :: etype ! GDALDataType
  TYPE(c_ptr),VALUE :: papszoptions ! TYPE(c_ptr_ptr)
  INTEGER(kind=c_int) :: gdaladdband ! CPLErr
  END FUNCTION gdaladdband
END INTERFACE

INTERFACE
  FUNCTION gdalbeginasyncreader(hds, nxoff, nyoff, nxsize, nysize, pbuf, nbufxsize, nbufysize, ebuftype, nbandcount, &
   panbandmap, npixelspace, nlinespace, nbandspace, papszoptions) BIND(C,name='GDALBeginAsyncReader')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALBeginAsyncReader
  TYPE(gdaldataseth),VALUE :: hds
  INTEGER(kind=c_int),VALUE :: nxoff
  INTEGER(kind=c_int),VALUE :: nyoff
  INTEGER(kind=c_int),VALUE :: nxsize
  INTEGER(kind=c_int),VALUE :: nysize
  TYPE(c_ptr),VALUE :: pbuf ! void*
  INTEGER(kind=c_int),VALUE :: nbufxsize
  INTEGER(kind=c_int),VALUE :: nbufysize
  INTEGER(kind=c_int),VALUE :: ebuftype ! GDALDataType
  INTEGER(kind=c_int),VALUE :: nbandcount
  INTEGER(kind=c_int) :: panbandmap(*)
  INTEGER(kind=c_int),VALUE :: npixelspace
  INTEGER(kind=c_int),VALUE :: nlinespace
  INTEGER(kind=c_int),VALUE :: nbandspace
  TYPE(c_ptr),VALUE :: papszoptions ! TYPE(c_ptr_ptr)
  TYPE(c_ptr) :: gdalbeginasyncreader
  END FUNCTION gdalbeginasyncreader
END INTERFACE

INTERFACE
  SUBROUTINE gdalendasyncreader(hds, hasynchreaderh) BIND(C,name='GDALEndAsyncReader')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALEndAsyncReader
  TYPE(gdaldataseth),VALUE :: hds
  TYPE(c_ptr),VALUE :: hasynchreaderh
  END SUBROUTINE gdalendasyncreader
END INTERFACE

INTERFACE
  FUNCTION gdaldatasetrasterio(hds, erwflag, ndsxoff, ndsyoff, ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, ebdatatype, &
   nbandcount, panbandcount, npixelspace, nlinespace, nbandspace) BIND(C,name='GDALDatasetRasterIO')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALDatasetRasterIO
  TYPE(gdaldataseth),VALUE :: hds
  INTEGER(kind=c_int),VALUE :: erwflag ! GDALRWFlag
  INTEGER(kind=c_int),VALUE :: ndsxoff
  INTEGER(kind=c_int),VALUE :: ndsyoff
  INTEGER(kind=c_int),VALUE :: ndsxsize
  INTEGER(kind=c_int),VALUE :: ndsysize
  TYPE(c_ptr),VALUE :: pbuffer ! void*
  INTEGER(kind=c_int),VALUE :: nbxsize
  INTEGER(kind=c_int),VALUE :: nbysize
  INTEGER(kind=c_int),VALUE :: ebdatatype ! GDALDataType
  INTEGER(kind=c_int),VALUE :: nbandcount
  INTEGER(kind=c_int) :: panbandcount(*)
  INTEGER(kind=c_int),VALUE :: npixelspace
  INTEGER(kind=c_int),VALUE :: nlinespace
  INTEGER(kind=c_int),VALUE :: nbandspace
  INTEGER(kind=c_int) :: gdaldatasetrasterio ! CPLErr
  END FUNCTION gdaldatasetrasterio
END INTERFACE

INTERFACE
  FUNCTION gdaldatasetadviseread(hds, ndsxoff, ndsyoff, ndsxsize, ndsysize, nbxsize, nbysize, ebdatatype, nbandcount, &
   panbandcount, papszoptions) BIND(C,name='GDALDatasetAdviseRead')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALDatasetAdviseRead
  TYPE(gdaldataseth),VALUE :: hds
  INTEGER(kind=c_int),VALUE :: ndsxoff
  INTEGER(kind=c_int),VALUE :: ndsyoff
  INTEGER(kind=c_int),VALUE :: ndsxsize
  INTEGER(kind=c_int),VALUE :: ndsysize
  INTEGER(kind=c_int),VALUE :: nbxsize
  INTEGER(kind=c_int),VALUE :: nbysize
  INTEGER(kind=c_int),VALUE :: ebdatatype ! GDALDataType
  INTEGER(kind=c_int),VALUE :: nbandcount
  INTEGER(kind=c_int) :: panbandcount(*)
  TYPE(c_ptr),VALUE :: papszoptions ! TYPE(c_ptr_ptr)
  INTEGER(kind=c_int) :: gdaldatasetadviseread ! CPLErr
  END FUNCTION gdaldatasetadviseread
END INTERFACE

INTERFACE
  FUNCTION gdalgetprojectionref(hds) BIND(C,name='GDALGetProjectionRef')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetProjectionRef
  TYPE(gdaldataseth),VALUE :: hds
  TYPE(c_ptr) :: gdalgetprojectionref ! char*
  END FUNCTION gdalgetprojectionref
END INTERFACE

INTERFACE
  FUNCTION gdalsetprojection(hds, pszprojection) BIND(C,name='GDALSetProjection')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSetProjection
  TYPE(gdaldataseth),VALUE :: hds
  CHARACTER(kind=c_char),INTENT(in) :: pszprojection(*)
  INTEGER(kind=c_int) :: gdalsetprojection ! CPLErr
  END FUNCTION gdalsetprojection
END INTERFACE

INTERFACE
  FUNCTION gdalgetgeotransform(hds, padftransform) BIND(C,name='GDALGetGeoTransform')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetGeoTransform
  TYPE(gdaldataseth),VALUE :: hds
  REAL(kind=c_double) :: padftransform(*)
  INTEGER(kind=c_int) :: gdalgetgeotransform ! CPLErr
  END FUNCTION gdalgetgeotransform
END INTERFACE

INTERFACE
  FUNCTION gdalsetgeotransform(hds, padftransform) BIND(C,name='GDALSetGeoTransform')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSetGeoTransform
  TYPE(gdaldataseth),VALUE :: hds
  REAL(kind=c_double) :: padftransform(*)
  INTEGER(kind=c_int) :: gdalsetgeotransform ! CPLErr
  END FUNCTION gdalsetgeotransform
END INTERFACE

INTERFACE
  FUNCTION gdalgetgcpcount(hds) BIND(C,name='GDALGetGCPCount')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetGCPCount
  TYPE(gdaldataseth),VALUE :: hds
  INTEGER(kind=c_int) :: gdalgetgcpcount
  END FUNCTION gdalgetgcpcount
END INTERFACE

INTERFACE
  FUNCTION gdalgetgcpprojection(hds) BIND(C,name='GDALGetGCPProjection')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetGCPProjection
  TYPE(gdaldataseth),VALUE :: hds
  TYPE(c_ptr) :: gdalgetgcpprojection ! char*
  END FUNCTION gdalgetgcpprojection
END INTERFACE

INTERFACE
  FUNCTION gdalgetinternalhandle(hds, pszrequest) BIND(C,name='GDALGetInternalHandle')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetInternalHandle
  TYPE(gdaldataseth),VALUE :: hds
  CHARACTER(kind=c_char),INTENT(in) :: pszrequest(*)
  TYPE(c_ptr) :: gdalgetinternalhandle ! void*
  END FUNCTION gdalgetinternalhandle
END INTERFACE

INTERFACE
  FUNCTION gdalreferencedataset(hdataset) BIND(C,name='GDALReferenceDataset')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALReferenceDataset
  TYPE(gdaldataseth),VALUE :: hdataset
  INTEGER(kind=c_int) :: gdalreferencedataset
  END FUNCTION gdalreferencedataset
END INTERFACE

INTERFACE
  FUNCTION gdaldereferencedataset(hdataset) BIND(C,name='GDALDereferenceDataset')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALDereferenceDataset
  TYPE(gdaldataseth),VALUE :: hdataset
  INTEGER(kind=c_int) :: gdaldereferencedataset
  END FUNCTION gdaldereferencedataset
END INTERFACE

INTERFACE
  FUNCTION gdalbuildoverviews(hdataset, pszresampling, noverviews, panoverviewlist, nlistbands, panbandlist, pfnprogress, &
   pprogressdata) BIND(C,name='GDALBuildOverviews')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALBuildOverviews
  TYPE(gdaldataseth),VALUE :: hdataset
  CHARACTER(kind=c_char),INTENT(in) :: pszresampling(*)
  INTEGER(kind=c_int),VALUE :: noverviews
  INTEGER(kind=c_int) :: panoverviewlist(*)
  INTEGER(kind=c_int),VALUE :: nlistbands
  INTEGER(kind=c_int) :: panbandlist(*)
  TYPE(c_ptr),VALUE :: pfnprogress
  TYPE(c_ptr),VALUE :: pprogressdata ! void*
  INTEGER(kind=c_int) :: gdalbuildoverviews ! CPLErr
  END FUNCTION gdalbuildoverviews
END INTERFACE

INTERFACE
  SUBROUTINE gdalgetopendatasets(hds, pncount) BIND(C,name='GDALGetOpenDatasets')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetOpenDatasets
  TYPE(c_ptr),VALUE :: hds ! TYPE(c_ptr_ptr)
  INTEGER(kind=c_int),INTENT(inout) :: pncount
  END SUBROUTINE gdalgetopendatasets
END INTERFACE

INTERFACE
  FUNCTION gdalgetaccess(hds) BIND(C,name='GDALGetAccess')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetAccess
  TYPE(gdaldataseth),VALUE :: hds
  INTEGER(kind=c_int) :: gdalgetaccess
  END FUNCTION gdalgetaccess
END INTERFACE

INTERFACE
  SUBROUTINE gdalflushcache(hds) BIND(C,name='GDALFlushCache')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALFlushCache
  TYPE(gdaldataseth),VALUE :: hds
  END SUBROUTINE gdalflushcache
END INTERFACE

INTERFACE
  FUNCTION gdalcreatedatasetmaskband(hds, nflags) BIND(C,name='GDALCreateDatasetMaskBand')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALCreateDatasetMaskBand
  TYPE(gdaldataseth),VALUE :: hds
  INTEGER(kind=c_int),VALUE :: nflags
  INTEGER(kind=c_int) :: gdalcreatedatasetmaskband ! CPLErr
  END FUNCTION gdalcreatedatasetmaskband
END INTERFACE

INTERFACE
  FUNCTION gdaldatasetcopywholeraster(hsrcds, hdstds, papszoptions, pfnprogress, pprogressdata) &
   BIND(C,name='GDALDatasetCopyWholeRaster')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALDatasetCopyWholeRaster
  TYPE(gdaldataseth),VALUE :: hsrcds
  TYPE(gdaldataseth),VALUE :: hdstds
  TYPE(c_ptr),VALUE :: papszoptions ! TYPE(c_ptr_ptr)
  TYPE(c_ptr),VALUE :: pfnprogress
  TYPE(c_ptr),VALUE :: pprogressdata ! void*
  INTEGER(kind=c_int) :: gdaldatasetcopywholeraster ! CPLErr
  END FUNCTION gdaldatasetcopywholeraster
END INTERFACE

INTERFACE
  FUNCTION gdalrasterbandcopywholeraster(hsrcband, hdstband, papszoptions, pfnprogress, pprogressdata) &
   BIND(C,name='GDALRasterBandCopyWholeRaster')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALRasterBandCopyWholeRaster
  TYPE(gdalrasterbandh),VALUE :: hsrcband
  TYPE(gdalrasterbandh),VALUE :: hdstband
  TYPE(c_ptr),VALUE :: papszoptions ! TYPE(c_ptr_ptr)
  TYPE(c_ptr),VALUE :: pfnprogress
  TYPE(c_ptr),VALUE :: pprogressdata ! void*
  INTEGER(kind=c_int) :: gdalrasterbandcopywholeraster ! CPLErr
  END FUNCTION gdalrasterbandcopywholeraster
END INTERFACE

INTERFACE
  FUNCTION gdalregenerateoverviews(hsrcband, noverviewcount, pahoverviewbands, pszresampling, pfnprogress, pprogressdata) &
   BIND(C,name='GDALRegenerateOverviews')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALRegenerateOverviews
  TYPE(gdalrasterbandh),VALUE :: hsrcband
  INTEGER(kind=c_int),VALUE :: noverviewcount
  TYPE(gdalrasterbandh),VALUE :: pahoverviewbands
  CHARACTER(kind=c_char),INTENT(in) :: pszresampling(*)
  TYPE(c_ptr),VALUE :: pfnprogress
  TYPE(c_ptr),VALUE :: pprogressdata ! void*
  INTEGER(kind=c_int) :: gdalregenerateoverviews ! CPLErr
  END FUNCTION gdalregenerateoverviews
END INTERFACE

INTERFACE
  FUNCTION gdalgetrasterdatatype(hband) BIND(C,name='GDALGetRasterDataType')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterDataType
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int) :: gdalgetrasterdatatype ! GDALDataType
  END FUNCTION gdalgetrasterdatatype
END INTERFACE

INTERFACE
  FUNCTION gdalrasteradviseread(hrb, ndsxoff, ndsyoff, ndsxsize, ndsysize, nbxsize, nbysize, ebdatatype, papszoptions) &
   BIND(C,name='GDALRasterAdviseRead')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALRasterAdviseRead
  TYPE(gdalrasterbandh),VALUE :: hrb
  INTEGER(kind=c_int),VALUE :: ndsxoff
  INTEGER(kind=c_int),VALUE :: ndsyoff
  INTEGER(kind=c_int),VALUE :: ndsxsize
  INTEGER(kind=c_int),VALUE :: ndsysize
  INTEGER(kind=c_int),VALUE :: nbxsize
  INTEGER(kind=c_int),VALUE :: nbysize
  INTEGER(kind=c_int),VALUE :: ebdatatype ! GDALDataType
  TYPE(c_ptr),VALUE :: papszoptions ! TYPE(c_ptr_ptr)
  INTEGER(kind=c_int) :: gdalrasteradviseread ! CPLErr
  END FUNCTION gdalrasteradviseread
END INTERFACE

INTERFACE
  FUNCTION gdalrasterio(hrband, erwflag, ndsxoff, ndsyoff, ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, ebdatatype, &
   npixelspace, nlinespace) BIND(C,name='GDALRasterIO')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALRasterIO
  TYPE(gdalrasterbandh),VALUE :: hrband
  INTEGER(kind=c_int),VALUE :: erwflag ! GDALRWFlag
  INTEGER(kind=c_int),VALUE :: ndsxoff
  INTEGER(kind=c_int),VALUE :: ndsyoff
  INTEGER(kind=c_int),VALUE :: ndsxsize
  INTEGER(kind=c_int),VALUE :: ndsysize
  TYPE(c_ptr),VALUE :: pbuffer ! void*
  INTEGER(kind=c_int),VALUE :: nbxsize
  INTEGER(kind=c_int),VALUE :: nbysize
  INTEGER(kind=c_int),VALUE :: ebdatatype ! GDALDataType
  INTEGER(kind=c_int),VALUE :: npixelspace
  INTEGER(kind=c_int),VALUE :: nlinespace
  INTEGER(kind=c_int) :: gdalrasterio ! CPLErr
  END FUNCTION gdalrasterio
END INTERFACE

INTERFACE
  FUNCTION gdalgetrasterbandxsize(hband) BIND(C,name='GDALGetRasterBandXSize')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterBandXSize
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int) :: gdalgetrasterbandxsize
  END FUNCTION gdalgetrasterbandxsize
END INTERFACE

INTERFACE
  FUNCTION gdalgetrasterbandysize(hband) BIND(C,name='GDALGetRasterBandYSize')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterBandYSize
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int) :: gdalgetrasterbandysize
  END FUNCTION gdalgetrasterbandysize
END INTERFACE

INTERFACE
  FUNCTION gdalgetrasteraccess(hband) BIND(C,name='GDALGetRasterAccess')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterAccess
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int) :: gdalgetrasteraccess ! GDALAccess
  END FUNCTION gdalgetrasteraccess
END INTERFACE

INTERFACE
  FUNCTION gdalgetbandnumber(hband) BIND(C,name='GDALGetBandNumber')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetBandNumber
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int) :: gdalgetbandnumber
  END FUNCTION gdalgetbandnumber
END INTERFACE

INTERFACE
  FUNCTION gdalgetbanddataset(hband) BIND(C,name='GDALGetBandDataset')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetBandDataset
  TYPE(gdalrasterbandh),VALUE :: hband
  TYPE(gdaldataseth) :: gdalgetbanddataset
  END FUNCTION gdalgetbanddataset
END INTERFACE

INTERFACE
  FUNCTION gdalgetrastercolorinterpretation(hband) BIND(C,name='GDALGetRasterColorInterpretation')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterColorInterpretation
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int) :: gdalgetrastercolorinterpretation ! GDALColorInterp
  END FUNCTION gdalgetrastercolorinterpretation
END INTERFACE

INTERFACE
  FUNCTION gdalsetrastercolorinterpretation(hband, ecolorinterp) BIND(C,name='GDALSetRasterColorInterpretation')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSetRasterColorInterpretation
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),VALUE :: ecolorinterp ! GDALColorInterp
  INTEGER(kind=c_int) :: gdalsetrastercolorinterpretation ! CPLErr
  END FUNCTION gdalsetrastercolorinterpretation
END INTERFACE

INTERFACE
  FUNCTION gdalgetrastercolortable(hband) BIND(C,name='GDALGetRasterColorTable')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterColorTable
  TYPE(gdalrasterbandh),VALUE :: hband
  TYPE(gdalcolortableh) :: gdalgetrastercolortable
  END FUNCTION gdalgetrastercolortable
END INTERFACE

INTERFACE
  FUNCTION gdalsetrastercolortable(hband, hct) BIND(C,name='GDALSetRasterColorTable')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSetRasterColorTable
  TYPE(gdalrasterbandh),VALUE :: hband
  TYPE(gdalcolortableh),VALUE :: hct
  INTEGER(kind=c_int) :: gdalsetrastercolortable ! CPLErr
  END FUNCTION gdalsetrastercolortable
END INTERFACE

INTERFACE
  FUNCTION gdalhasarbitraryoverviews(hband) BIND(C,name='GDALHasArbitraryOverviews')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALHasArbitraryOverviews
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int) :: gdalhasarbitraryoverviews
  END FUNCTION gdalhasarbitraryoverviews
END INTERFACE

INTERFACE
  FUNCTION gdalgetoverviewcount(hband) BIND(C,name='GDALGetOverviewCount')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetOverviewCount
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int) :: gdalgetoverviewcount
  END FUNCTION gdalgetoverviewcount
END INTERFACE

INTERFACE
  FUNCTION gdalgetoverview(hband, i) BIND(C,name='GDALGetOverview')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetOverview
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),VALUE :: i
  TYPE(gdalrasterbandh) :: gdalgetoverview
  END FUNCTION gdalgetoverview
END INTERFACE

INTERFACE
  FUNCTION gdalgetrasternodatavalue(hband, pbsuccess) BIND(C,name='GDALGetRasterNoDataValue')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterNoDataValue
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),INTENT(inout) :: pbsuccess
  REAL(kind=c_double) :: gdalgetrasternodatavalue
  END FUNCTION gdalgetrasternodatavalue
END INTERFACE

INTERFACE
  FUNCTION gdalsetrasternodatavalue(hband, dfvalue) BIND(C,name='GDALSetRasterNoDataValue')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSetRasterNoDataValue
  TYPE(gdalrasterbandh),VALUE :: hband
  REAL(kind=c_double),VALUE :: dfvalue
  INTEGER(kind=c_int) :: gdalsetrasternodatavalue ! CPLErr
  END FUNCTION gdalsetrasternodatavalue
END INTERFACE

INTERFACE
  FUNCTION gdalgetrastercategorynames(hband) BIND(C,name='GDALGetRasterCategoryNames')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterCategoryNames
  TYPE(gdalrasterbandh),VALUE :: hband
  TYPE(c_ptr) :: gdalgetrastercategorynames ! TYPE(c_ptr_ptr)
  END FUNCTION gdalgetrastercategorynames
END INTERFACE

INTERFACE
  FUNCTION gdalgetrasterminimum(hband, pbsuccess) BIND(C,name='GDALGetRasterMinimum')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterMinimum
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),INTENT(inout) :: pbsuccess
  REAL(kind=c_double) :: gdalgetrasterminimum
  END FUNCTION gdalgetrasterminimum
END INTERFACE

INTERFACE
  FUNCTION gdalgetrastermaximum(hband, pbsuccess) BIND(C,name='GDALGetRasterMaximum')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterMaximum
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),INTENT(inout) :: pbsuccess
  REAL(kind=c_double) :: gdalgetrastermaximum
  END FUNCTION gdalgetrastermaximum
END INTERFACE

INTERFACE
  FUNCTION gdalgetrasterstatistics(hband, bapproxok, bforce, pdfmin, pdfmax, pdfmean, pdfstddev) &
   BIND(C,name='GDALGetRasterStatistics')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterStatistics
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),VALUE :: bapproxok
  INTEGER(kind=c_int),VALUE :: bforce
  REAL(kind=c_double) :: pdfmin(*)
  REAL(kind=c_double) :: pdfmax(*)
  REAL(kind=c_double) :: pdfmean(*)
  REAL(kind=c_double) :: pdfstddev(*)
  INTEGER(kind=c_int) :: gdalgetrasterstatistics ! CPLErr
  END FUNCTION gdalgetrasterstatistics
END INTERFACE

INTERFACE
  FUNCTION gdalcomputerasterstatistics(hband, bapproxok, pdfmin, pdfmax, pdfmean, pdfstddev, pfnprogress, pprogressdata) &
   BIND(C,name='GDALComputeRasterStatistics')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALComputeRasterStatistics
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),VALUE :: bapproxok
  REAL(kind=c_double) :: pdfmin(*)
  REAL(kind=c_double) :: pdfmax(*)
  REAL(kind=c_double) :: pdfmean(*)
  REAL(kind=c_double) :: pdfstddev(*)
  TYPE(c_ptr),VALUE :: pfnprogress
  TYPE(c_ptr),VALUE :: pprogressdata ! void*
  INTEGER(kind=c_int) :: gdalcomputerasterstatistics ! CPLErr
  END FUNCTION gdalcomputerasterstatistics
END INTERFACE

INTERFACE
  FUNCTION gdalsetrasterstatistics(hband, dfmin, dfmax, dfmean, dfstddev) BIND(C,name='GDALSetRasterStatistics')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSetRasterStatistics
  TYPE(gdalrasterbandh),VALUE :: hband
  REAL(kind=c_double),VALUE :: dfmin
  REAL(kind=c_double),VALUE :: dfmax
  REAL(kind=c_double),VALUE :: dfmean
  REAL(kind=c_double),VALUE :: dfstddev
  INTEGER(kind=c_int) :: gdalsetrasterstatistics ! CPLErr
  END FUNCTION gdalsetrasterstatistics
END INTERFACE

INTERFACE
  FUNCTION gdalgetrasterunittype(hband) BIND(C,name='GDALGetRasterUnitType')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterUnitType
  TYPE(gdalrasterbandh),VALUE :: hband
  TYPE(c_ptr) :: gdalgetrasterunittype ! char*
  END FUNCTION gdalgetrasterunittype
END INTERFACE

INTERFACE
  FUNCTION gdalsetrasterunittype(hband, psznewvalue) BIND(C,name='GDALSetRasterUnitType')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSetRasterUnitType
  TYPE(gdalrasterbandh),VALUE :: hband
  CHARACTER(kind=c_char),INTENT(in) :: psznewvalue(*)
  INTEGER(kind=c_int) :: gdalsetrasterunittype ! CPLErr
  END FUNCTION gdalsetrasterunittype
END INTERFACE

INTERFACE
  FUNCTION gdalgetrasteroffset(hband, pbsuccess) BIND(C,name='GDALGetRasterOffset')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterOffset
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),INTENT(inout) :: pbsuccess
  REAL(kind=c_double) :: gdalgetrasteroffset
  END FUNCTION gdalgetrasteroffset
END INTERFACE

INTERFACE
  FUNCTION gdalsetrasteroffset(hband, dfnewoffset) BIND(C,name='GDALSetRasterOffset')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSetRasterOffset
  TYPE(gdalrasterbandh),VALUE :: hband
  REAL(kind=c_double),VALUE :: dfnewoffset
  INTEGER(kind=c_int) :: gdalsetrasteroffset ! CPLErr
  END FUNCTION gdalsetrasteroffset
END INTERFACE

INTERFACE
  FUNCTION gdalgetrasterscale(hband, pbsuccess) BIND(C,name='GDALGetRasterScale')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterScale
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),INTENT(inout) :: pbsuccess
  REAL(kind=c_double) :: gdalgetrasterscale
  END FUNCTION gdalgetrasterscale
END INTERFACE

INTERFACE
  FUNCTION gdalsetrasterscale(hband, dfnewoffset) BIND(C,name='GDALSetRasterScale')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSetRasterScale
  TYPE(gdalrasterbandh),VALUE :: hband
  REAL(kind=c_double),VALUE :: dfnewoffset
  INTEGER(kind=c_int) :: gdalsetrasterscale ! CPLErr
  END FUNCTION gdalsetrasterscale
END INTERFACE

INTERFACE
  SUBROUTINE gdalcomputerasterminmax(hband, bapproxok, adfminmax) BIND(C,name='GDALComputeRasterMinMax')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALComputeRasterMinMax
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),VALUE :: bapproxok
  REAL(kind=c_double),VALUE :: adfminmax
  END SUBROUTINE gdalcomputerasterminmax
END INTERFACE

INTERFACE
  FUNCTION gdalflushrastercache(hband) BIND(C,name='GDALFlushRasterCache')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALFlushRasterCache
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int) :: gdalflushrastercache ! CPLErr
  END FUNCTION gdalflushrastercache
END INTERFACE

INTERFACE
  FUNCTION gdalgetrasterhistogram(hband, dfmin, dfmax, nbuckets, panhistogram, bincludeoutofrange, bapproxok, pfnprogress, &
   pprogressdata) BIND(C,name='GDALGetRasterHistogram')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterHistogram
  TYPE(gdalrasterbandh),VALUE :: hband
  REAL(kind=c_double),VALUE :: dfmin
  REAL(kind=c_double),VALUE :: dfmax
  INTEGER(kind=c_int),VALUE :: nbuckets
  INTEGER(kind=c_int) :: panhistogram(*)
  INTEGER(kind=c_int),VALUE :: bincludeoutofrange
  INTEGER(kind=c_int),VALUE :: bapproxok
  TYPE(c_ptr),VALUE :: pfnprogress
  TYPE(c_ptr),VALUE :: pprogressdata ! void*
  INTEGER(kind=c_int) :: gdalgetrasterhistogram ! CPLErr
  END FUNCTION gdalgetrasterhistogram
END INTERFACE

INTERFACE
  FUNCTION gdalgetdefaulthistogram(hband, pdfmin, pdfmax, pnbuckets, ppanhistogram, bforce, pfnprogress, pprogressdata) &
   BIND(C,name='GDALGetDefaultHistogram')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetDefaultHistogram
  TYPE(gdalrasterbandh),VALUE :: hband
  REAL(kind=c_double) :: pdfmin(*)
  REAL(kind=c_double) :: pdfmax(*)
  INTEGER(kind=c_int) :: pnbuckets(*)
  TYPE(c_ptr),VALUE :: ppanhistogram ! TYPE(c_ptr_ptr)
  INTEGER(kind=c_int),VALUE :: bforce
  TYPE(c_ptr),VALUE :: pfnprogress
  TYPE(c_ptr),VALUE :: pprogressdata ! void*
  INTEGER(kind=c_int) :: gdalgetdefaulthistogram ! CPLErr
  END FUNCTION gdalgetdefaulthistogram
END INTERFACE

INTERFACE
  FUNCTION gdalsetdefaulthistogram(hband, dfmin, dfmax, nbuckets, panhistogram) BIND(C,name='GDALSetDefaultHistogram')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSetDefaultHistogram
  TYPE(gdalrasterbandh),VALUE :: hband
  REAL(kind=c_double),VALUE :: dfmin
  REAL(kind=c_double),VALUE :: dfmax
  INTEGER(kind=c_int),VALUE :: nbuckets
  INTEGER(kind=c_int) :: panhistogram(*)
  INTEGER(kind=c_int) :: gdalsetdefaulthistogram ! CPLErr
  END FUNCTION gdalsetdefaulthistogram
END INTERFACE

INTERFACE
  FUNCTION gdalgetrandomrastersample(hband, nsamples, pafsamplebuf) BIND(C,name='GDALGetRandomRasterSample')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRandomRasterSample
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),VALUE :: nsamples
  REAL(kind=c_float) :: pafsamplebuf(*)
  INTEGER(kind=c_int) :: gdalgetrandomrastersample
  END FUNCTION gdalgetrandomrastersample
END INTERFACE

INTERFACE
  FUNCTION gdalgetrastersampleoverview(hband, t) BIND(C,name='GDALGetRasterSampleOverview')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetRasterSampleOverview
  TYPE(gdalrasterbandh),VALUE :: hband
  TYPE(c_ptr),VALUE :: t
  TYPE(gdalrasterbandh) :: gdalgetrastersampleoverview
  END FUNCTION gdalgetrastersampleoverview
END INTERFACE

INTERFACE
  FUNCTION gdalfillraster(hband, dfrealvalue, dfimaginaryvalue) BIND(C,name='GDALFillRaster')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALFillRaster
  TYPE(gdalrasterbandh),VALUE :: hband
  REAL(kind=c_double),VALUE :: dfrealvalue
  REAL(kind=c_double),VALUE :: dfimaginaryvalue
  INTEGER(kind=c_int) :: gdalfillraster ! CPLErr
  END FUNCTION gdalfillraster
END INTERFACE

INTERFACE
  FUNCTION gdalcomputebandstats(hband, nsamplestep, pdfmean, pdfstddev, pfnprogress, pprogressdata) &
   BIND(C,name='GDALComputeBandStats')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALComputeBandStats
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),VALUE :: nsamplestep
  REAL(kind=c_double) :: pdfmean(*)
  REAL(kind=c_double) :: pdfstddev(*)
  TYPE(c_ptr),VALUE :: pfnprogress
  TYPE(c_ptr),VALUE :: pprogressdata ! void*
  INTEGER(kind=c_int) :: gdalcomputebandstats ! CPLErr
  END FUNCTION gdalcomputebandstats
END INTERFACE

INTERFACE
  FUNCTION gdaloverviewmagnitudecorrection(hbaseband, noverviewcount, pahoverviews, pfnprogress, pprogressdata) &
   BIND(C,name='GDALOverviewMagnitudeCorrection')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALOverviewMagnitudeCorrection
  TYPE(gdalrasterbandh),VALUE :: hbaseband
  INTEGER(kind=c_int),VALUE :: noverviewcount
  TYPE(gdalrasterbandh),VALUE :: pahoverviews
  TYPE(c_ptr),VALUE :: pfnprogress
  TYPE(c_ptr),VALUE :: pprogressdata ! void*
  INTEGER(kind=c_int) :: gdaloverviewmagnitudecorrection ! CPLErr
  END FUNCTION gdaloverviewmagnitudecorrection
END INTERFACE

INTERFACE
  FUNCTION gdalgetdefaultrat(hband) BIND(C,name='GDALGetDefaultRAT')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetDefaultRAT
  TYPE(gdalrasterbandh),VALUE :: hband
  TYPE(gdalrasterattributetableh) :: gdalgetdefaultrat
  END FUNCTION gdalgetdefaultrat
END INTERFACE

INTERFACE
  FUNCTION gdalsetdefaultrat(hband, h) BIND(C,name='GDALSetDefaultRAT')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSetDefaultRAT
  TYPE(gdalrasterbandh),VALUE :: hband
  TYPE(c_ptr),VALUE :: h
  INTEGER(kind=c_int) :: gdalsetdefaultrat ! CPLErr
  END FUNCTION gdalsetdefaultrat
END INTERFACE

INTERFACE
  FUNCTION gdaladdderivedbandpixelfunc(pszname, pfnpixelfunc) BIND(C,name='GDALAddDerivedBandPixelFunc')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALAddDerivedBandPixelFunc
  CHARACTER(kind=c_char),INTENT(in) :: pszname(*)
  TYPE(c_ptr),VALUE :: pfnpixelfunc
  INTEGER(kind=c_int) :: gdaladdderivedbandpixelfunc ! CPLErr
  END FUNCTION gdaladdderivedbandpixelfunc
END INTERFACE

INTERFACE
  FUNCTION gdalgetmaskband(hband) BIND(C,name='GDALGetMaskBand')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetMaskBand
  TYPE(gdalrasterbandh),VALUE :: hband
  TYPE(gdalrasterbandh) :: gdalgetmaskband
  END FUNCTION gdalgetmaskband
END INTERFACE

INTERFACE
  FUNCTION gdalgetmaskflags(hband) BIND(C,name='GDALGetMaskFlags')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetMaskFlags
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int) :: gdalgetmaskflags
  END FUNCTION gdalgetmaskflags
END INTERFACE

INTERFACE
  FUNCTION gdalcreatemaskband(hband, nflags) BIND(C,name='GDALCreateMaskBand')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALCreateMaskBand
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),VALUE :: nflags
  INTEGER(kind=c_int) :: gdalcreatemaskband ! CPLErr
  END FUNCTION gdalcreatemaskband
END INTERFACE

INTERFACE
  FUNCTION gdalargetnextupdatedregion(hario, dftimeout, pnxbufoff, pnybufoff, pnxbufsize, pnybufsize) &
   BIND(C,name='GDALARGetNextUpdatedRegion')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALARGetNextUpdatedRegion
  TYPE(c_ptr),VALUE :: hario
  REAL(kind=c_double),VALUE :: dftimeout
  INTEGER(kind=c_int) :: pnxbufoff(*)
  INTEGER(kind=c_int) :: pnybufoff(*)
  INTEGER(kind=c_int) :: pnxbufsize(*)
  INTEGER(kind=c_int) :: pnybufsize(*)
  INTEGER(kind=c_int) :: gdalargetnextupdatedregion ! GDALAsyncStatusType
  END FUNCTION gdalargetnextupdatedregion
END INTERFACE

INTERFACE
  FUNCTION gdalarlockbuffer(hario, dftimeout) BIND(C,name='GDALARLockBuffer')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALARLockBuffer
  TYPE(c_ptr),VALUE :: hario
  REAL(kind=c_double),VALUE :: dftimeout
  INTEGER(kind=c_int) :: gdalarlockbuffer
  END FUNCTION gdalarlockbuffer
END INTERFACE

INTERFACE
  SUBROUTINE gdalarunlockbuffer(hario) BIND(C,name='GDALARUnlockBuffer')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALARUnlockBuffer
  TYPE(c_ptr),VALUE :: hario
  END SUBROUTINE gdalarunlockbuffer
END INTERFACE

INTERFACE
  SUBROUTINE gdalswapwords(pdata, nwordsize, nwordcount, nwordskip) BIND(C,name='GDALSwapWords')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSwapWords
  TYPE(c_ptr),VALUE :: pdata ! void*
  INTEGER(kind=c_int),VALUE :: nwordsize
  INTEGER(kind=c_int),VALUE :: nwordcount
  INTEGER(kind=c_int),VALUE :: nwordskip
  END SUBROUTINE gdalswapwords
END INTERFACE

INTERFACE
  SUBROUTINE gdalcopywords(psrcdata, esrctype, nsrcpixeloffset, pdstdata, edsttype, ndstpixeloffset, nwordcount) &
   BIND(C,name='GDALCopyWords')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALCopyWords
  TYPE(c_ptr),VALUE :: psrcdata ! void*
  INTEGER(kind=c_int),VALUE :: esrctype ! GDALDataType
  INTEGER(kind=c_int),VALUE :: nsrcpixeloffset
  TYPE(c_ptr),VALUE :: pdstdata ! void*
  INTEGER(kind=c_int),VALUE :: edsttype ! GDALDataType
  INTEGER(kind=c_int),VALUE :: ndstpixeloffset
  INTEGER(kind=c_int),VALUE :: nwordcount
  END SUBROUTINE gdalcopywords
END INTERFACE

INTERFACE
  SUBROUTINE gdalcopybits(pabysrcdata, nsrcoffset, nsrcstep, pabydstdata, ndstoffset, ndststep, nbitcount, nstepcount) &
   BIND(C,name='GDALCopyBits')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALCopyBits
  TYPE(c_ptr),VALUE :: pabysrcdata
  INTEGER(kind=c_int),VALUE :: nsrcoffset
  INTEGER(kind=c_int),VALUE :: nsrcstep
  TYPE(c_ptr),VALUE :: pabydstdata
  INTEGER(kind=c_int),VALUE :: ndstoffset
  INTEGER(kind=c_int),VALUE :: ndststep
  INTEGER(kind=c_int),VALUE :: nbitcount
  INTEGER(kind=c_int),VALUE :: nstepcount
  END SUBROUTINE gdalcopybits
END INTERFACE

INTERFACE
  FUNCTION gdalloadworldfile(pszfilename, padfgeotransform) BIND(C,name='GDALLoadWorldFile')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALLoadWorldFile
  CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*)
  REAL(kind=c_double) :: padfgeotransform(*)
  INTEGER(kind=c_int) :: gdalloadworldfile
  END FUNCTION gdalloadworldfile
END INTERFACE

INTERFACE
  FUNCTION gdalreadworldfile(pszbasefilename, pszextension, padfgeotransform) BIND(C,name='GDALReadWorldFile')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALReadWorldFile
  CHARACTER(kind=c_char),INTENT(in) :: pszbasefilename(*)
  CHARACTER(kind=c_char),INTENT(in) :: pszextension(*)
  REAL(kind=c_double) :: padfgeotransform(*)
  INTEGER(kind=c_int) :: gdalreadworldfile
  END FUNCTION gdalreadworldfile
END INTERFACE

INTERFACE
  FUNCTION gdalwriteworldfile(pszbasefilename, pszextension, padfgeotransform) BIND(C,name='GDALWriteWorldFile')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALWriteWorldFile
  CHARACTER(kind=c_char),INTENT(in) :: pszbasefilename(*)
  CHARACTER(kind=c_char),INTENT(in) :: pszextension(*)
  REAL(kind=c_double) :: padfgeotransform(*)
  INTEGER(kind=c_int) :: gdalwriteworldfile
  END FUNCTION gdalwriteworldfile
END INTERFACE

INTERFACE
  FUNCTION gdalloadozimapfile(pszfilename, padfgeotransform, ppszwkt, pngcpcount, ppasgcps) BIND(C,name='GDALLoadOziMapFile')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALLoadOziMapFile
  CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*)
  REAL(kind=c_double) :: padfgeotransform(*)
  TYPE(c_ptr),VALUE :: ppszwkt ! TYPE(c_ptr_ptr)
  INTEGER(kind=c_int) :: pngcpcount(*)
  TYPE(c_ptr),VALUE :: ppasgcps ! TYPE(c_ptr_ptr)
  INTEGER(kind=c_int) :: gdalloadozimapfile
  END FUNCTION gdalloadozimapfile
END INTERFACE

INTERFACE
  FUNCTION gdalreadozimapfile(pszbasefilename, padfgeotransform, ppszwkt, pngcpcount, ppasgcps) &
   BIND(C,name='GDALReadOziMapFile')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALReadOziMapFile
  CHARACTER(kind=c_char),INTENT(in) :: pszbasefilename(*)
  REAL(kind=c_double) :: padfgeotransform(*)
  TYPE(c_ptr),VALUE :: ppszwkt ! TYPE(c_ptr_ptr)
  INTEGER(kind=c_int) :: pngcpcount(*)
  TYPE(c_ptr),VALUE :: ppasgcps ! TYPE(c_ptr_ptr)
  INTEGER(kind=c_int) :: gdalreadozimapfile
  END FUNCTION gdalreadozimapfile
END INTERFACE

INTERFACE
  FUNCTION gdalloadtabfile(pszfilename, padfgeotransform, ppszwkt, pngcpcount, ppasgcps) BIND(C,name='GDALLoadTabFile')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALLoadTabFile
  CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*)
  REAL(kind=c_double) :: padfgeotransform(*)
  TYPE(c_ptr),VALUE :: ppszwkt ! TYPE(c_ptr_ptr)
  INTEGER(kind=c_int) :: pngcpcount(*)
  TYPE(c_ptr),VALUE :: ppasgcps ! TYPE(c_ptr_ptr)
  INTEGER(kind=c_int) :: gdalloadtabfile
  END FUNCTION gdalloadtabfile
END INTERFACE

INTERFACE
  FUNCTION gdalreadtabfile(pszbasefilename, padfgeotransform, ppszwkt, pngcpcount, ppasgcps) BIND(C,name='GDALReadTabFile')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALReadTabFile
  CHARACTER(kind=c_char),INTENT(in) :: pszbasefilename(*)
  REAL(kind=c_double) :: padfgeotransform(*)
  TYPE(c_ptr),VALUE :: ppszwkt ! TYPE(c_ptr_ptr)
  INTEGER(kind=c_int) :: pngcpcount(*)
  TYPE(c_ptr),VALUE :: ppasgcps ! TYPE(c_ptr_ptr)
  INTEGER(kind=c_int) :: gdalreadtabfile
  END FUNCTION gdalreadtabfile
END INTERFACE

INTERFACE
  FUNCTION gdalloadrpbfile(pszfilename, papszsiblingfiles) BIND(C,name='GDALLoadRPBFile')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALLoadRPBFile
  CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*)
  TYPE(c_ptr),VALUE :: papszsiblingfiles ! TYPE(c_ptr_ptr)
  TYPE(c_ptr) :: gdalloadrpbfile ! TYPE(c_ptr_ptr)
  END FUNCTION gdalloadrpbfile
END INTERFACE

INTERFACE
  FUNCTION gdalloadrpcfile(pszfilename, papszsiblingfiles) BIND(C,name='GDALLoadRPCFile')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALLoadRPCFile
  CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*)
  TYPE(c_ptr),VALUE :: papszsiblingfiles ! TYPE(c_ptr_ptr)
  TYPE(c_ptr) :: gdalloadrpcfile ! TYPE(c_ptr_ptr)
  END FUNCTION gdalloadrpcfile
END INTERFACE

INTERFACE
  FUNCTION gdalwriterpbfile(pszfilename, papszmd) BIND(C,name='GDALWriteRPBFile')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALWriteRPBFile
  CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*)
  TYPE(c_ptr),VALUE :: papszmd ! TYPE(c_ptr_ptr)
  INTEGER(kind=c_int) :: gdalwriterpbfile ! CPLErr
  END FUNCTION gdalwriterpbfile
END INTERFACE

INTERFACE
  FUNCTION gdalloadimdfile(pszfilename, papszsiblingfiles) BIND(C,name='GDALLoadIMDFile')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALLoadIMDFile
  CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*)
  TYPE(c_ptr),VALUE :: papszsiblingfiles ! TYPE(c_ptr_ptr)
  TYPE(c_ptr) :: gdalloadimdfile ! TYPE(c_ptr_ptr)
  END FUNCTION gdalloadimdfile
END INTERFACE

INTERFACE
  FUNCTION gdalwriteimdfile(pszfilename, papszmd) BIND(C,name='GDALWriteIMDFile')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALWriteIMDFile
  CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*)
  TYPE(c_ptr),VALUE :: papszmd ! TYPE(c_ptr_ptr)
  INTEGER(kind=c_int) :: gdalwriteimdfile ! CPLErr
  END FUNCTION gdalwriteimdfile
END INTERFACE

INTERFACE
  FUNCTION gdaldectodms(dfangle, pszaxis, nprecision) BIND(C,name='GDALDecToDMS')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALDecToDMS
  REAL(kind=c_double),VALUE :: dfangle
  CHARACTER(kind=c_char),INTENT(in) :: pszaxis(*)
  INTEGER(kind=c_int),VALUE :: nprecision
  TYPE(c_ptr) :: gdaldectodms ! char*
  END FUNCTION gdaldectodms
END INTERFACE

INTERFACE
  FUNCTION gdalpackeddmstodec(dfpacked) BIND(C,name='GDALPackedDMSToDec')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALPackedDMSToDec
  REAL(kind=c_double),VALUE :: dfpacked
  REAL(kind=c_double) :: gdalpackeddmstodec
  END FUNCTION gdalpackeddmstodec
END INTERFACE

INTERFACE
  FUNCTION gdaldectopackeddms(dfdec) BIND(C,name='GDALDecToPackedDMS')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALDecToPackedDMS
  REAL(kind=c_double),VALUE :: dfdec
  REAL(kind=c_double) :: gdaldectopackeddms
  END FUNCTION gdaldectopackeddms
END INTERFACE

INTERFACE
  FUNCTION gdalextractrpcinfo(papszmd, psrpc) BIND(C,name='GDALExtractRPCInfo')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALExtractRPCInfo
  TYPE(c_ptr),VALUE :: papszmd ! TYPE(c_ptr_ptr)
  TYPE(gdalrpcinfo) :: psrpc
  INTEGER(kind=c_int) :: gdalextractrpcinfo
  END FUNCTION gdalextractrpcinfo
END INTERFACE

INTERFACE
  FUNCTION gdalcreatecolortable(einterp) BIND(C,name='GDALCreateColorTable')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALCreateColorTable
  INTEGER(kind=c_int),VALUE :: einterp ! GDALPaletteInterp
  TYPE(gdalcolortableh) :: gdalcreatecolortable
  END FUNCTION gdalcreatecolortable
END INTERFACE

INTERFACE
  SUBROUTINE gdaldestroycolortable(htable) BIND(C,name='GDALDestroyColorTable')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALDestroyColorTable
  TYPE(gdalcolortableh),VALUE :: htable
  END SUBROUTINE gdaldestroycolortable
END INTERFACE

INTERFACE
  FUNCTION gdalclonecolortable(htable) BIND(C,name='GDALCloneColorTable')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALCloneColorTable
  TYPE(gdalcolortableh),VALUE :: htable
  TYPE(gdalcolortableh) :: gdalclonecolortable
  END FUNCTION gdalclonecolortable
END INTERFACE

INTERFACE
  FUNCTION gdalgetpaletteinterpretation(htable) BIND(C,name='GDALGetPaletteInterpretation')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetPaletteInterpretation
  TYPE(gdalcolortableh),VALUE :: htable
  INTEGER(kind=c_int) :: gdalgetpaletteinterpretation ! GDALPaletteInterp
  END FUNCTION gdalgetpaletteinterpretation
END INTERFACE

INTERFACE
  FUNCTION gdalgetcolorentrycount(htable) BIND(C,name='GDALGetColorEntryCount')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetColorEntryCount
  TYPE(gdalcolortableh),VALUE :: htable
  INTEGER(kind=c_int) :: gdalgetcolorentrycount
  END FUNCTION gdalgetcolorentrycount
END INTERFACE

INTERFACE
  FUNCTION gdalgetcolorentry(htable, i) BIND(C,name='GDALGetColorEntry')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetColorEntry
  TYPE(gdalcolortableh),VALUE :: htable
  INTEGER(kind=c_int),VALUE :: i
  TYPE(c_ptr) :: gdalgetcolorentry ! GDALColorEntry*
  END FUNCTION gdalgetcolorentry
END INTERFACE

INTERFACE
  FUNCTION gdalgetcolorentryasrgb(htable, i, poentry) BIND(C,name='GDALGetColorEntryAsRGB')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetColorEntryAsRGB
  TYPE(gdalcolortableh),VALUE :: htable
  INTEGER(kind=c_int),VALUE :: i
  TYPE(gdalcolorentry) :: poentry
  INTEGER(kind=c_int) :: gdalgetcolorentryasrgb
  END FUNCTION gdalgetcolorentryasrgb
END INTERFACE

INTERFACE
  SUBROUTINE gdalsetcolorentry(htable, i, poentry) BIND(C,name='GDALSetColorEntry')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSetColorEntry
  TYPE(gdalcolortableh),VALUE :: htable
  INTEGER(kind=c_int),VALUE :: i
  TYPE(gdalcolorentry),INTENT(in) :: poentry
  END SUBROUTINE gdalsetcolorentry
END INTERFACE

INTERFACE
  SUBROUTINE gdalcreatecolorramp(htable, nstartindex, psstartcolor, nendindex, psendcolor) BIND(C,name='GDALCreateColorRamp')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALCreateColorRamp
  TYPE(gdalcolortableh),VALUE :: htable
  INTEGER(kind=c_int),VALUE :: nstartindex
  TYPE(gdalcolorentry),INTENT(in) :: psstartcolor
  INTEGER(kind=c_int),VALUE :: nendindex
  TYPE(gdalcolorentry),INTENT(in) :: psendcolor
  END SUBROUTINE gdalcreatecolorramp
END INTERFACE

INTERFACE
  SUBROUTINE gdalsetcachemax(nbytes) BIND(C,name='GDALSetCacheMax')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSetCacheMax
  INTEGER(kind=c_int),VALUE :: nbytes
  END SUBROUTINE gdalsetcachemax
END INTERFACE

INTERFACE
  FUNCTION gdalgetcachemax() BIND(C,name='GDALGetCacheMax')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetCacheMax
  INTEGER(kind=c_int) :: gdalgetcachemax
  END FUNCTION gdalgetcachemax
END INTERFACE

INTERFACE
  FUNCTION gdalgetcacheused() BIND(C,name='GDALGetCacheUsed')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetCacheUsed
  INTEGER(kind=c_int) :: gdalgetcacheused
  END FUNCTION gdalgetcacheused
END INTERFACE

INTERFACE
  SUBROUTINE gdalsetcachemax64(nbytes) BIND(C,name='GDALSetCacheMax64')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALSetCacheMax64
  TYPE(c_ptr),VALUE :: nbytes
  END SUBROUTINE gdalsetcachemax64
END INTERFACE

INTERFACE
  FUNCTION gdalgetcachemax64() BIND(C,name='GDALGetCacheMax64')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetCacheMax64
  TYPE(c_ptr) :: gdalgetcachemax64
  END FUNCTION gdalgetcachemax64
END INTERFACE

INTERFACE
  FUNCTION gdalgetcacheused64() BIND(C,name='GDALGetCacheUsed64')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALGetCacheUsed64
  TYPE(c_ptr) :: gdalgetcacheused64
  END FUNCTION gdalgetcacheused64
END INTERFACE

INTERFACE
  FUNCTION gdalflushcacheblock() BIND(C,name='GDALFlushCacheBlock')
  IMPORT
!!GCC$ ATTRIBUTES STDCALL :: GDALFlushCacheBlock
  INTEGER(kind=c_int) :: gdalflushcacheblock
  END FUNCTION gdalflushcacheblock
END INTERFACE

