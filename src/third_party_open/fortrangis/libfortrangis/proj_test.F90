PROGRAM proj_test
USE,INTRINSIC :: iso_c_binding
USE fortranc
USE proj
IMPLICIT none

! declare projection objects
TYPE(pj_object) :: pj, pjll
! declare coordinate objects
TYPE(pjuv_object) :: coordg, coordp, coordgc
REAL(kind=c_double) :: x(4), y(4), z(4), xorig(4), yorig(4)
INTEGER :: res
CHARACTER(len=512) :: proj_string

proj_string = &
 '+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0'
! initialize a projection object from a string, remember //char(0)!
PRINT*,TRIM(proj_string)
pj = pj_init_plus(TRIM(proj_string)//CHAR(0))
PRINT*,'Associated:',pj_associated(pj)
IF (.NOT.pj_associated(pj)) CALL EXIT(1)
PRINT*,'Geographic:',pj_is_latlong(pj)

! build a corresponding latlon projection
PRINT*,'Corresponding latlon projection'
pjll = pj_latlong_from_proj(pj)
IF (.NOT.pj_associated(pjll)) CALL EXIT(1)
PRINT*,'Associated:',pj_associated(pjll)
PRINT*,'Geographic:',pj_is_latlong(pjll)
! retrieve the projection string defined by proj, the trick described
! in the fortranc library is used
proj_string = strtofchar(pj_get_def(pjll, 0), LEN(proj_string))
PRINT*,TRIM(proj_string)

PRINT*,'Converting through a pjuv object'
! define a coordinate set, it is latlong so convert to radians
coordg = pjuv_object(11.0D0*pj_deg_to_rad, 45.0D0*pj_deg_to_rad)

! make a forward projection, result in meters
coordp = pj_fwd(coordg, pj)
! return back to polar coordinates in radians
coordgc = pj_inv(coordp, pj)
PRINT*,'Original:',coordg
PRINT*,'Projected:',coordp
PRINT*,'Returned:',coordgc

! check whether they are the same within 10-6 radians
IF (ABS(coordgc%u-coordg%u) > 1.0D-6 .OR. ABS(coordgc%v-coordg%v) > 1.0D-6) THEN
  PRINT*,'Error pj_inv*pj_fwd failed or /= identity'
  CALL EXIT(1)
ENDIF

xorig(:) = (/11.0D0,12.0D0,11.0D0,12.0D0/)*pj_deg_to_rad
yorig(:) = (/45.0D0,45.0D0,46.0D0,46.0D0/)*pj_deg_to_rad

PRINT*,'Converting through pj_transform_f with z'
x(:) = xorig(:)
y(:) = yorig(:)
z(:) = 0.0D0
PRINT*,'Original:',x(1),y(1)
res = pj_transform_f(pjll, pj, x, y, z)
PRINT*,'Projected:',x(1),y(1)
IF (res == 0) res = pj_transform_f(pj, pjll, x, y, z)
IF (res == 0) THEN
  PRINT*,'Returned:',x(1),y(1)
ELSE
  PRINT*,'pj_transform with z failed'
  CALL EXIT(1)
ENDIF
IF (MAXVAL(ABS(xorig-x)) > 1.0D-6 .OR. MAXVAL(ABS(yorig-y)) > 1.0D-6) THEN
  PRINT*,'Error pj_transform*pj_transform**-1 /= identity'
  CALL EXIT(1)
ENDIF

PRINT*,'Converting through pj_transform_f without z'
x(:) = xorig(:)
y(:) = yorig(:)
PRINT*,'Original:',x(1),y(1)
res = pj_transform_f(pjll, pj, x, y)
PRINT*,'Projected:',x(1),y(1)
IF (res == 0) res = pj_transform_f(pj, pjll, x, y)
IF (res == 0) THEN
  PRINT*,'Returned:',x(1),y(1)
ELSE
  PRINT*,'pj_transform without z failed'
  CALL EXIT(1)
ENDIF
IF (MAXVAL(ABS(xorig-x)) > 1.0D-6 .OR. MAXVAL(ABS(yorig-y)) > 1.0D-6) THEN
  PRINT*,'Error pj_transform*pj_transform**-1 /= identity'
  CALL EXIT(1)
ENDIF

CALL pj_free(pj)
CALL pj_free(pjll)

END PROGRAM proj_test

