PROGRAM gdal_test
USE,INTRINSIC :: iso_c_binding
USE fortranc
USE gdal
IMPLICIT none

TYPE(gdaldriverh) :: driver
TYPE(gdaldataseth) :: ds
TYPE(gdalrasterbandh) :: band
CHARACTER(len=512) :: file
REAL(kind=c_double) :: x1, y1, x2, y2, gt(6)
INTEGER(kind=c_int) :: i1, j1, k1, i2, j2, k2, i, j, k, ierr
REAL,ALLOCATABLE :: z(:,:), z3(:,:,:), zr3(:,:,:)


! register all available gdal drivers
CALL gdalallregister()
! file name to work on
file = 'gdal_test.tiff'

! ==== How to create a gdal dataset and export it to a file ====

! get a specific driver (see e.g. gdalinfo --formats)
PRINT*,'Getting GeoTIFF driver'
driver = gdalgetdriverbyname('GTiff'//CHAR(0))
IF (.NOT.gdalassociated(driver)) THEN
  PRINT*,'Error getting GeoTIFF driver from gdal'
  STOP 1
ENDIF

! create the dataset with i1xj1 points and 1 raster band of byte data
PRINT*,'Creating a GeoTIFF gdal dataset'
i1 = 120
j1 = 80
k1 = 3
! pass a couple of options (BIGTIFF and COMPRESS) to the create function,
! the strings must have the same legth (possibly compiler-dependent),
! they will be trimmed by the c_ptr_ptr_new function
ds = gdalcreate(driver, TRIM(file)//CHAR(0), i1, j1, k1, gdt_byte, &
 c_ptr_ptr_getobject(c_ptr_ptr_new((/'BIGTIFF=YES     ','COMPRESS=DEFLATE'/))))
! (/('',i=1,0)/) is a trick to define a zero-length array on the fly,
! if we do not want to pass any specific option
!ds = gdalcreate(driver, TRIM(file)//CHAR(0), i1, j1, k1, gdt_byte, &
! c_ptr_ptr_getobject(c_ptr_ptr_new((/('',i=1,0)/))))
IF (.NOT.gdalassociated(ds)) THEN
  PRINT*,'Error creating a GeoTIFF dataset on file ',TRIM(file)
  STOP 1
ENDIF

! this seems to be useless here, depends on file type
PRINT*,'Setting color interpretation to RGB'
ierr =  GDALSetRasterColorInterpretation(gdalgetrasterband(ds, 1), GCI_RedBand)
ierr =  GDALSetRasterColorInterpretation(gdalgetrasterband(ds, 2), GCI_GreenBand)
ierr =  GDALSetRasterColorInterpretation(gdalgetrasterband(ds, 3), GCI_BlueBand)

! create a dummy array of real data, we stick to integer <= 255 in
! order to fit in a byte and not to lose precision in read test
ALLOCATE(z3(i1,j1,k1))
DO k = 1, k1
  DO j = 1, j1
    DO i = 1, i1
      z3(i,j,k) = i/2+j*MOD(k,2)+(j1-j)*(1-MOD(k,2))
    ENDDO
  ENDDO
ENDDO

! write data to dataset with the simplified Fortran interface, one
! raster band is written starting from the upper(?) left corner, the
! conversion from the type of z3 (real) to the gdt_byte type specified
! with gdalcreate is done internally by gdal
PRINT*,'Writing data to dataset'
PRINT*,'using the simplified Fortran interface'
ierr = gdaldatasetrasterio_f(ds, GF_Write, 0, 0, z3)
IF (ierr /= 0) THEN
  PRINT*,'Error writing data to GeoTIFF dataset on file ',TRIM(file)
  STOP 1
ENDIF

CALL gdalclose(ds)

! ==== How to read a gdal dataset from a file, simplified Fortran interface ====

PRINT*,'Opening a GeoTIFF gdal dataset for reading'
PRINT*,'using the simplified Fortran interface'
ds = gdalopen(TRIM(file)//CHAR(0), GA_ReadOnly)
IF (.NOT.gdalassociated(ds)) THEN
  PRINT*,'Error opening dataset on file ',TRIM(file)
  STOP 1
ENDIF

PRINT*,'Getting the affine transformation'
ierr = gdalgetgeotransform(ds, gt)
! an error is acceptable since no transformation had been defined
!IF (ierr /= 0) THEN
!  PRINT*,'Error getting the affine transformation from dataset on file ',TRIM(file)
!  STOP 1
!ENDIF
PRINT*,'The affine transformation matrix is: ',gt

PRINT*,'Getting the dataset size'
i2 = gdalgetrasterxsize(ds)
j2 = gdalgetrasterysize(ds)
k2 = gdalgetrastercount(ds)
IF (i1 /= i2 .OR. j1 /= j2) THEN
  PRINT*,'Error, wrong dataset x or y size ',i1,i2,j1,j2
  STOP 1
ENDIF
PRINT*,'The x/y size of the raster is: ',i2,j2

IF (k1 /= k2) THEN
  PRINT*,'Error, wrong raster band number ',k1,k2
  STOP 1
ENDIF
PRINT*,'The number of raster bands is: ',k2

! apply the affine transformation to some coordinates
! original gdal version
CALL gdalapplygeotransform(gt, 0.5_c_double, 0.5_c_double, x1, y1)
! Fortran elemental version
CALL gdalapplygeotransform_f(gt, 0.5_c_double, 0.5_c_double, x2, y2)
IF (x1 /= x2 .OR. y1 /= y2) THEN ! this check should be relaxed
  PRINT*,'Error gdal and Fortran version of GDALApplyGeoTransform'
  PRINT*,'give different results'
  STOP 1
ENDIF

CALL gdalapplygeotransform_f(gt, 0.5_c_double, 0.5_c_double, x1, y1)
CALL gdalapplygeotransform_f(gt, &
 REAL(i1, kind=c_double)-0.5_c_double, &
 REAL(j1, kind=c_double)-0.5_c_double, x2, y2)
PRINT*,'The raster coordinates of the corners are: ',x1,y1,x2,y2

ALLOCATE(z(i2,j2))

! get the first raster band
PRINT*,'Getting a raster band'
band = gdalgetrasterband(ds, 1)
IF (.NOT.gdalassociated(band)) THEN
  PRINT*,'Error getting raster band from GeoTIFF dataset on file ',TRIM(file)
  STOP 1
ENDIF

! read data from first raster band with the simplified Fortran interface,
! raster band is read starting from the upper(?) left corner
PRINT*,'Reading data from dataset'
ierr = gdalrasterio_f(band, GF_Read, 0, 0, z)
IF (ierr /= 0) THEN
  PRINT*,'Error reading data from GeoTIFF dataset on file ',TRIM(file)
  PRINT*,'with simplified Fortran interface'
  STOP 1
ENDIF

PRINT*,'The sum of the buffer read is: ',SUM(z)
PRINT*,'Average error after write/read process: ',&
 SUM(ABS(z(:,:) - z3(:,:,1)))/(i2*j2)

CALL gdalclose(ds)

! ==== How to read a gdal dataset from a file, even more simplified Fortran interface ====

PRINT*,'Opening a GeoTIFF gdal dataset for reading'
PRINT*,'using the even more simplified Fortran interface'
ds = gdalopen(TRIM(file)//CHAR(0), GA_ReadOnly)
IF (.NOT.gdalassociated(ds)) THEN
  PRINT*,'Error opening dataset on file ',TRIM(file)
  STOP 1
ENDIF

! read data from first raster band with the even more simplified Fortran interface,
! raster band is read starting from the upper(?) left corner
PRINT*,'Reading data from dataset'
CALL gdaldatasetsimpleread_f(ds, 5._c_double, 5._c_double, 20._c_double, 15._c_double, &
 zr3, x1, y1, x2, y2)

IF (.NOT.ALLOCATED(zr3)) THEN
  PRINT*,'Error reading data from GeoTIFF dataset on file ',TRIM(file)
  PRINT*,'with even more simplified Fortran interface'
  STOP 1
ENDIF

PRINT*,'The shape of the buffer read is: ',SHAPE(zr3)
PRINT*,'The sum of the buffer read is: ',SUM(zr3)
PRINT*,'The raster coordinates of the corners are: ',x1,y1,x2,y2

CALL gdalclose(ds)

DEALLOCATE(z3,z,zr3)

END PROGRAM gdal_test

