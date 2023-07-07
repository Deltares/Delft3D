GDALDataType GDALDataTypeUnion(GDALDataType eType1,GDALDataType eType2);
int GDALGetDataTypeSize(GDALDataType eDataType);
int GDALDataTypeIsComplex(GDALDataType eDataType);
const char *GDALGetDataTypeName(GDALDataType eDataType);
GDALDataType GDALGetDataTypeByName(const char *pszName);

const char *GDALGetPaletteInterpretationName(GDALPaletteInterp eInterp);
const char *GDALGetColorInterpretationName(GDALColorInterp eInterp);
GDALColorInterp GDALGetColorInterpretationByName(const char *pszName);

void GDALAllRegister();
GDALDatasetH GDALCreate(GDALDriverH hDriver,const char *pszFilename,int nXSize,int nYSize,int nBands,GDALDataType eBandType,char **papszOptions);
GDALDatasetH GDALCreateCopy(GDALDriverH hDriver,const char *pszFilename,GDALDatasetH hSrcDS,int bStrict,char **papszOptions,GDALProgressFunc pfnProgress,void *pProgressData);

GDALDriverH GDALIdentifyDriver(const char *pszFilename, char **papszFileList);

GDALDatasetH GDALOpen(const char *pszFilename, GDALAccess eAccess);
GDALDatasetH GDALOpenShared(const char *pszFilename,GDALAccess eAccess);

GDALDriverH GDALGetDriverByName(const char *pszName);

int GDALGetDriverCount();

GDALDriverH GDALGetDriver(int iDriver);

void GDALDestroyDriver(GDALDriverH hDriver);

int GDALRegisterDriver(GDALDriverH hDriver);

void GDALDeregisterDriver(GDALDriverH hDriver);

void GDALDestroyDriverManager();

CPLErr GDALDeleteDataset(GDALDriverH hDriver,const char *pszFilename);
CPLErr GDALRenameDataset(GDALDriverH hDriver,const char *pszNewName,const char *pszOldName);
CPLErr GDALCopyDatasetFiles(GDALDriverH hDriver,const char *pszNewName,const char *pszOldName);

int GDALValidateCreationOptions(GDALDriverH hDriver, char **papszCreationOptions);

const char *GDALGetDriverShortName(GDALDriverH hDriver);

const char * GDALGetDriverLongName(GDALDriverH hDriver);

const char * GDALGetDriverHelpTopic(GDALDriverH hDriver);

const char * GDALGetDriverCreationOptionList(GDALDriverH hDriver);

int GDALInvGeoTransform(double *padfGeoTransformIn, double *padfInvGeoTransformOut);

char **GDALGetMetadata(GDALMajorObjectH hObject,const char *pszDomain);
CPLErr GDALSetMetadata(GDALMajorObjectH hObject,char **papszMD,const char *pszDomain);
const char *GDALGetMetadataItem(GDALMajorObjectH hObject,const char *pszName,const char *pszDomain);
CPLErr GDALSetMetadataItem(GDALMajorObjectH hObject,const char *pszName,const char *pszValue,const char *pszDomain);
const char * GDALGetDescription(GDALMajorObjectH hObject);
void GDALSetDescription(GDALMajorObjectH hObject, const char *pszNewDesc);
GDALDriverH GDALGetDatasetDriver(GDALDatasetH hDataset);
char ** GDALGetFileList(GDALDatasetH hDS);
void GDALClose(GDALDatasetH hDS);
int GDALGetRasterXSize(GDALDatasetH hDataset);
int GDALGetRasterYSize(GDALDatasetH hDataset);
int GDALGetRasterCount(GDALDatasetH hDataset);
GDALRasterBandH GDALGetRasterBand(GDALDatasetH hDS,int nBandId);
CPLErr GDALAddBand(GDALDatasetH hDS, GDALDataType eType, char **papszOptions);

GDALAsyncReaderH GDALBeginAsyncReader(GDALDatasetH hDS, int nXOff, int nYOff, int nXSize, int nYSize, void *pBuf, int nBufXSize, int nBufYSize, GDALDataType eBufType, int nBandCount, int *panBandMap, int nPixelSpace, int nLineSpace, int nBandSpace, char **papszOptions);
void GDALEndAsyncReader(GDALDatasetH hDS, GDALAsyncReaderH hAsynchReaderH);
CPLErr GDALDatasetRasterIO(GDALDatasetH hDS, GDALRWFlag eRWFlag, int nDSXOff, int nDSYOff, int nDSXSize, int nDSYSize, void *pBuffer, int nBXSize, int nBYSize, GDALDataType eBDataType, int nBandCount, int *panBandCount, int nPixelSpace, int nLineSpace, int nBandSpace);
CPLErr GDALDatasetAdviseRead(GDALDatasetH hDS, int nDSXOff, int nDSYOff, int nDSXSize, int nDSYSize, int nBXSize, int nBYSize, GDALDataType eBDataType, int nBandCount, int *panBandCount, char **papszOptions);
const char * GDALGetProjectionRef(GDALDatasetH hDS);
CPLErr GDALSetProjection(GDALDatasetH hDS, const char *pszProjection);
CPLErr GDALGetGeoTransform(GDALDatasetH hDS,double *padfTransform);
CPLErr GDALSetGeoTransform(GDALDatasetH hDS,double *padfTransform);
int GDALGetGCPCount(GDALDatasetH hDS);
const char * GDALGetGCPProjection(GDALDatasetH hDS);
void *GDALGetInternalHandle(GDALDatasetH hDS,const char *pszRequest);
int GDALReferenceDataset(GDALDatasetH hDataset);
int GDALDereferenceDataset(GDALDatasetH hDataset);
CPLErr GDALBuildOverviews(GDALDatasetH hDataset,const char *pszResampling,int nOverviews,int *panOverviewList,int nListBands,int *panBandList,GDALProgressFunc pfnProgress,void *pProgressData);
void GDALGetOpenDatasets(GDALDatasetH **hDS, int *pnCount);/*out*/
int GDALGetAccess(GDALDatasetH hDS);
void GDALFlushCache(GDALDatasetH hDS);
CPLErr GDALCreateDatasetMaskBand(GDALDatasetH hDS, int nFlags);

CPLErr GDALDatasetCopyWholeRaster(GDALDatasetH hSrcDS, GDALDatasetH hDstDS, char **papszOptions, GDALProgressFunc pfnProgress, void *pProgressData);
CPLErr GDALRasterBandCopyWholeRaster(GDALRasterBandH hSrcBand, GDALRasterBandH hDstBand, char **papszOptions, GDALProgressFunc pfnProgress, void *pProgressData);
CPLErr GDALRegenerateOverviews(GDALRasterBandH hSrcBand, int nOverviewCount, GDALRasterBandH *pahOverviewBands, const char *pszResampling, GDALProgressFunc pfnProgress, void *pProgressData);
GDALDataType GDALGetRasterDataType(GDALRasterBandH hband);


CPLErr GDALRasterAdviseRead(GDALRasterBandH hRB, int nDSXOff, int nDSYOff, int nDSXSize, int nDSYSize, int nBXSize, int nBYSize, GDALDataType eBDataType, char **papszOptions);

CPLErr GDALRasterIO(GDALRasterBandH hRBand, GDALRWFlag eRWFlag, int nDSXOff, int nDSYOff, int nDSXSize, int nDSYSize, void *pBuffer, int nBXSize, int nBYSize, GDALDataType eBDataType, int nPixelSpace, int nLineSpace);

int GDALGetRasterBandXSize(GDALRasterBandH hBand);
int GDALGetRasterBandYSize(GDALRasterBandH hBand);
GDALAccess GDALGetRasterAccess(GDALRasterBandH hBand);

int GDALGetBandNumber(GDALRasterBandH hBand);

GDALDatasetH GDALGetBandDataset(GDALRasterBandH hBand);

GDALColorInterp GDALGetRasterColorInterpretation(GDALRasterBandH hBand);

CPLErr GDALSetRasterColorInterpretation(GDALRasterBandH hBand, GDALColorInterp eColorInterp);

GDALColorTableH GDALGetRasterColorTable(GDALRasterBandH hBand);

CPLErr GDALSetRasterColorTable(GDALRasterBandH hBand, GDALColorTableH hCT);

int GDALHasArbitraryOverviews(GDALRasterBandH hBand);

int GDALGetOverviewCount(GDALRasterBandH hBand);

GDALRasterBandH GDALGetOverview(GDALRasterBandH hBand, int i);

double GDALGetRasterNoDataValue(GDALRasterBandH hBand, int *pbSuccess);/*out*/

CPLErr GDALSetRasterNoDataValue(GDALRasterBandH hBand, double dfValue);

char ** GDALGetRasterCategoryNames(GDALRasterBandH hBand);

CPLErr GDALSetRasterCategoryNames(GDALRasterBandH hBand,char **papsz
Names);

double GDALGetRasterMinimum(GDALRasterBandH hBand, int *pbSuccess);/*out*/

double GDALGetRasterMaximum(GDALRasterBandH hBand, int *pbSuccess);/*out*/

CPLErr GDALGetRasterStatistics(GDALRasterBandH hBand, int bApproxOK, int bForce, double *pdfMin, double *pdfMax, double *pdfMean, double *pdfStdDev);

CPLErr GDALComputeRasterStatistics(GDALRasterBandH hBand, int bApproxOK, double *pdfMin, double *pdfMax, double *pdfMean, double *pdfStdDev, GDALProgressFunc pfnProgress, void *pProgressData);

CPLErr GDALSetRasterStatistics(GDALRasterBandH hBand, double dfMin, double dfMax, double dfMean, double dfStdDev);

const char * GDALGetRasterUnitType(GDALRasterBandH hBand);

CPLErr GDALSetRasterUnitType(GDALRasterBandH hBand, const char *pszNewValue);

double GDALGetRasterOffset(GDALRasterBandH hBand, int *pbSuccess);/*out*/

CPLErr GDALSetRasterOffset(GDALRasterBandH hBand, double dfNewOffset);

double GDALGetRasterScale(GDALRasterBandH hBand, int *pbSuccess);/*out*/

CPLErr GDALSetRasterScale(GDALRasterBandH hBand, double dfNewOffset);

void GDALComputeRasterMinMax(GDALRasterBandH hBand, int bApproxOK, double adfMinMax[2]);

CPLErr GDALFlushRasterCache(GDALRasterBandH hBand);

CPLErr GDALGetRasterHistogram(GDALRasterBandH hBand, double dfMin, double dfMax, int nBuckets, int *panHistogram, int bIncludeOutOfRange, int bApproxOK, GDALProgressFunc pfnProgress, void *pProgressData);

CPLErr GDALGetDefaultHistogram(GDALRasterBandH hBand, double *pdfMin, double *pdfMax, int *pnBuckets, int **ppanHistogram, int bForce, GDALProgressFunc pfnProgress, void *pProgressData);

CPLErr GDALSetDefaultHistogram(GDALRasterBandH hBand, double dfMin, double dfMax, int nBuckets, int *panHistogram);

int GDALGetRandomRasterSample(GDALRasterBandH hBand,int nSamples,float *pafSampleBuf);
GDALRasterBandH GDALGetRasterSampleOverview(GDALRasterBandH hBand, int);

CPLErr GDALFillRaster(GDALRasterBandH hBand, double dfRealValue, double dfImaginaryValue);

CPLErr GDALComputeBandStats(GDALRasterBandH hBand, int nSampleStep, double *pdfMean, double *pdfStdDev, GDALProgressFunc pfnProgress, void *pProgressData);
CPLErr GDALOverviewMagnitudeCorrection(GDALRasterBandH hBaseBand, int nOverviewCount, GDALRasterBandH *pahOverviews, GDALProgressFunc pfnProgress, void *pProgressData);
GDALRasterAttributeTableH GDALGetDefaultRAT(GDALRasterBandH hBand);

CPLErr GDALSetDefaultRAT(GDALRasterBandH hBand, GDALRasterAttributeTableH);

CPLErr GDALAddDerivedBandPixelFunc(const char *pszName, GDALDerivedPixelFunc pfnPixelFunc);

GDALRasterBandH GDALGetMaskBand(GDALRasterBandH hBand);

int GDALGetMaskFlags(GDALRasterBandH hBand);

CPLErr GDALCreateMaskBand(GDALRasterBandH hBand, int nFlags);

GDALAsyncStatusType GDALARGetNextUpdatedRegion(GDALAsyncReaderH hARIO, double dfTimeout, int *pnXBufOff, int *pnYBufOff, int *pnXBufSize, int *pnYBufSize);
int GDALARLockBuffer(GDALAsyncReaderH hARIO, double dfTimeout);
void GDALARUnlockBuffer(GDALAsyncReaderH hARIO);

void GDALSwapWords(void *pData, int nWordSize, int nWordCount, int nWordSkip);

void GDALCopyWords(void *pSrcData, GDALDataType eSrcType, int nSrcPixelOffset, void *pDstData, GDALDataType eDstType, int nDstPixelOffset, int nWordCount);
void GDALCopyBits(const GByte *pabySrcData, int nSrcOffset, int nSrcStep, GByte *pabyDstData, int nDstOffset, int nDstStep, int nBitCount, int nStepCount);


int GDALLoadWorldFile(const char *pszFilename,double *padfGeoTransform);
int GDALReadWorldFile(const char *pszBaseFilename,const char *pszExtension,double *padfGeoTransform);
int GDALWriteWorldFile(const char *pszBaseFilename,const char *pszExtension,double *padfGeoTransform);
int GDALLoadOziMapFile(const char *pszFilename,double *padfGeoTransform,char **ppszWKT,int *pnGCPCount,GDAL_GCP **ppasGCPs);
int GDALReadOziMapFile(const char *pszBaseFilename,double *padfGeoTransform,char **ppszWKT,int *pnGCPCount,GDAL_GCP **ppasGCPs);
int GDALLoadTabFile(const char *pszFilename,double *padfGeoTransform,char **ppszWKT,int *pnGCPCount,GDAL_GCP **ppasGCPs);
int GDALReadTabFile(const char *pszBaseFilename,double *padfGeoTransform,char **ppszWKT,int *pnGCPCount,GDAL_GCP **ppasGCPs);



char ** GDALLoadRPBFile(const char *pszFilename, char **papszSiblingFiles);
char ** GDALLoadRPCFile(const char *pszFilename, char **papszSiblingFiles);
CPLErr GDALWriteRPBFile(const char *pszFilename, char **papszMD);
char ** GDALLoadIMDFile(const char *pszFilename, char **papszSiblingFiles);
CPLErr GDALWriteIMDFile(const char *pszFilename, char **papszMD);

const char *GDALDecToDMS(double dfAngle,const char *pszAxis,int nPrecision);
double GDALPackedDMSToDec(double dfPacked);
double GDALDecToPackedDMS(double dfDec);


int GDALExtractRPCInfo(char **papszMD,GDALRPCInfo *psRPC);

GDALColorTableH GDALCreateColorTable(GDALPaletteInterp eInterp);

void GDALDestroyColorTable(GDALColorTableH hTable);

GDALColorTableH GDALCloneColorTable(GDALColorTableH hTable);

GDALPaletteInterp GDALGetPaletteInterpretation(GDALColorTableH hTable);

int GDALGetColorEntryCount(GDALColorTableH hTable);

const GDALColorEntry * GDALGetColorEntry(GDALColorTableH hTable, int i);

int GDALGetColorEntryAsRGB(GDALColorTableH hTable, int i, GDALColorEntry *poEntry);

void GDALSetColorEntry(GDALColorTableH hTable, int i, const GDALColorEntry *poEntry);

void GDALCreateColorRamp(GDALColorTableH hTable, int nStartIndex, const GDALColorEntry *psStartColor, int nEndIndex, const GDALColorEntry *psEndColor);

void GDALSetCacheMax(int nBytes);

int GDALGetCacheMax();

int GDALGetCacheUsed();

void GDALSetCacheMax64(GIntBig nBytes);

GIntBig GDALGetCacheMax64();

GIntBig GDALGetCacheUsed64();

int GDALFlushCacheBlock();

