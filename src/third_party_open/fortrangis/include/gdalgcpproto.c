void GDALInitGCPs(int nCount,GDAL_GCP *psGCP);
void GDALDeinitGCPs(int nCount,GDAL_GCP *psGCP);
GDAL_GCP *GDALDuplicateGCPs(int nCount,const GDAL_GCP *pasGCPList);
int GDALGCPsToGeoTransform(int nGCPCount,const GDAL_GCP *pasGCPs,double *padfGeoTransform,int bApproxOK);
const GDAL_GCP * GDALGetGCPs(GDALDatasetH hDS);
CPLErr GDALSetGCPs(GDALDatasetH hDS,int nGCPCount,const GDAL_GCP *pasGCPList,const char *pszGCPProjection);
