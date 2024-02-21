PROGRAM proj_test6
USE,INTRINSIC :: iso_c_binding
USE fortranc
USE proj6
IMPLICIT none

! declare projection objects
TYPE(pj_object) :: pj, pjf, pjt
! declare coordinate objects
TYPE(pj_coord_object) :: coordg, coordp, coordgc, &
 vcoordg(4), vcoordp(4), vcoordgc(4)
! declare area object
TYPE(pj_area_object) :: area

INTEGER :: res
CHARACTER(len=512) :: proj_stringf, proj_stringt

! from EPSG:4326 ("latlon") to UTM
proj_stringf = 'EPSG:4326'
proj_stringt = '+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs' ! EPSG:32632
PRINT*,'from: ',TRIM(proj_stringf)
PRINT*,'to: ',TRIM(proj_stringt)

! initialize a projection object from two projection strings, remember //char(0)!
PRINT*,'Defining a projection object'
pj = proj_create_crs_to_crs(pj_default_ctx, TRIM(proj_stringf)//CHAR(0), &
 TRIM(proj_stringt)//CHAR(0), area)

PRINT*,'Converting through a pj_coord object'
! define a coordinate set, %z and %t keep the default value of 0;
! EPSG:4326 expects values in degrees, lat lon order
coordg%x = 45.0D0
coordg%y = 11.0D0

! make a forward projection to UTM, result in meters x,y order
coordp = proj_trans(pj, pj_fwd, coordg)
! return back to polar coordinates in degrees
coordgc = proj_trans(pj, pj_inv, coordp)

PRINT*,'Original:',coordg
PRINT*,'Projected:',coordp
PRINT*,'Returned:',coordgc

! check whether they are the same within 10-8 degrees
IF (ABS(coordgc%x-coordg%x) > 1.0D-8 .OR. ABS(coordgc%y-coordg%y) > 1.0D-8) THEN
  PRINT*,'Error pj_inv*pj_fwd failed or /= identity'
  CALL EXIT(1)
ENDIF

pj = proj_destroy(pj)

! initialize a projection transformation object from two projection objects
pjf = proj_create(pj_default_ctx, TRIM(proj_stringf)//CHAR(0))
IF (.NOT.proj_associated(pjf)) THEN
  PRINT*,'Failing to define projection object from ',TRIM(proj_stringf)
  CALL print_error(pj_default_ctx)
  CALL EXIT(1)
ENDIF
CALL print_info(proj_pj_info(pjf))

pjt = proj_create(pj_default_ctx, TRIM(proj_stringt)//CHAR(0))
IF (.NOT.proj_associated(pjt)) THEN
  PRINT*,'Failing to define projection object from ',TRIM(proj_stringt)
  CALL print_error(pj_default_ctx)
  CALL EXIT(1)
ENDIF
CALL print_info(proj_pj_info(pjt))

PRINT*,'Defining a projection object'
pj = proj_create_crs_to_crs_from_pj(pj_default_ctx, pjf, pjt, area, C_NULL_PTR)
IF (.NOT.proj_associated(pjt)) THEN
  PRINT*,'Failing to define projection object'
  CALL print_error(pj_default_ctx)
  CALL EXIT(1)
ENDIF

PRINT*,'Converting through a pj_coord array object'
! define a coordinate set, values in degree, %z and %t keep the
! default value of 0
vcoordg(:)%x = (/45.0D0,45.0D0,46.0D0,46.0D0/)
vcoordg(:)%y = (/11.0D0,12.0D0,11.0D0,12.0D0/)

vcoordp = vcoordg ! array transformation overwrites
! make a forward projection to UTM, result in meters
res = proj_trans_f(pj, pj_fwd, vcoordp)
IF (res /= 0) THEN
  PRINT*,'Failure in forward array transformation',res
  CALL print_error(pj_default_ctx)
ENDIF
vcoordgc = vcoordp ! array transformation overwrites

! return back to polar coordinates in degrees
res = proj_trans_f(pj, pj_inv, vcoordgc)
IF (res /= 0) THEN
  PRINT*,'Failure in inverse array transformation',res
  CALL print_error(pj_default_ctx)
  CALL EXIT(1)
ENDIF

PRINT*,'Original:',vcoordg%x,vcoordg%y
PRINT*,'Projected:',vcoordp%x,vcoordp%y
PRINT*,'Returned:',vcoordgc%x,vcoordgc%y

! check whether they are the same within 10-8 degrees
IF (MAXVAL(ABS(vcoordgc%x-vcoordg%x)) > 1.0D-8 .OR. &
 MAXVAL(ABS(vcoordgc%y-vcoordg%y)) > 1.0D-8) THEN
  PRINT*,'Error pj_inv*pj_fwd failed or /= identity'
  CALL EXIT(1)
ENDIF

pj = proj_destroy(pj)

CONTAINS

SUBROUTINE print_info(info)
TYPE(pj_proj_info_object),INTENT(in) :: info

PRINT*,'==== Object information ===='
PRINT*,'id:',TRIM(strtofchar(info%id,256))
PRINT*,'desc:',TRIM(strtofchar(info%description,256))
PRINT*,'def:',TRIM(strtofchar(info%definition,256))
PRINT*,'has inv:',info%has_inverse
PRINT*,'accuracy:',info%accuracy
PRINT*,'============================'

END SUBROUTINE print_info

SUBROUTINE print_error(ctx)
TYPE(pj_context_object),INTENT(in) :: ctx

IF (proj_context_errno(ctx) /= 0) THEN
  ! switch to proj_context_errno_string
  PRINT*,TRIM(strtofchar(proj_errno_string(proj_context_errno(ctx)), 256))
ENDIF

END SUBROUTINE print_error

END PROGRAM proj_test6
