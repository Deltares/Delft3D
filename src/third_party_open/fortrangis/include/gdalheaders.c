
const char *GDALVersionInfo(const char *pszRequest);
int GDALCheckVersion(int nVersionMajor,int nVersionMinor,const char *pszCallingComponentName);
int GDALGeneralCmdLineProcessor(int nArgc,char ***ppapszArgv,int nOptions);
GDALDataset *GDALFindAssociatedAuxFile(const char *pszBasename,GDALAccess eAccess,GDALDataset *poDependentDS);

CPL_C_START void *GDALCreateProjDef(const char *);
CPLErr GDALReprojectToLongLat(void *,double *,double *);
CPLErr GDALReprojectFromLongLat(void *,double *,double *);
void GDALDestroyProjDef(void *);
CPL_C_END CPL_C_START int GDALCheckDatasetDimensions(int nXSize,int nYSize);
int GDALCheckBandCount(int nBands,int bIsZeroAllowed);
