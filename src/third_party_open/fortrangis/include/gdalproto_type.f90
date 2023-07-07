TYPE,BIND(C) :: GDALDatasetH
  PRIVATE
  TYPE(c_ptr) :: ptr = C_NULL_PTR
END TYPE GDALDatasetH

TYPE,BIND(C) :: GDALMajorObjectH
  PRIVATE
  TYPE(c_ptr) :: ptr = C_NULL_PTR
END TYPE GDALMajorObjectH

TYPE,BIND(C) :: GDALDriverH
  PRIVATE
  TYPE(c_ptr) :: ptr = C_NULL_PTR
END TYPE GDALDriverH

TYPE,BIND(C) :: GDALRasterAttributeTableH
  PRIVATE
  TYPE(c_ptr) :: ptr = C_NULL_PTR
END TYPE GDALRasterAttributeTableH

TYPE,BIND(C) :: GDALColorTableH
  PRIVATE
  TYPE(c_ptr) :: ptr = C_NULL_PTR
END TYPE GDALColorTableH

TYPE,BIND(C) :: GDALRasterBandH
  PRIVATE
  TYPE(c_ptr) :: ptr = C_NULL_PTR
END TYPE GDALRasterBandH

INTERFACE gdalassociated
  MODULE PROCEDURE gdalassociated_gdaldataseth
  MODULE PROCEDURE gdalassociated_gdalmajorobjecth
  MODULE PROCEDURE gdalassociated_gdaldriverh
  MODULE PROCEDURE gdalassociated_gdalrasterattributetableh
  MODULE PROCEDURE gdalassociated_gdalcolortableh
  MODULE PROCEDURE gdalassociated_gdalrasterbandh
END INTERFACE
INTERFACE gdalnullify
  MODULE PROCEDURE gdalnullify_gdaldataseth
  MODULE PROCEDURE gdalnullify_gdalmajorobjecth
  MODULE PROCEDURE gdalnullify_gdaldriverh
  MODULE PROCEDURE gdalnullify_gdalrasterattributetableh
  MODULE PROCEDURE gdalnullify_gdalcolortableh
  MODULE PROCEDURE gdalnullify_gdalrasterbandh
END INTERFACE
