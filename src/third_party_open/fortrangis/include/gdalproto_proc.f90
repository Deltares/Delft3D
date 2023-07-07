FUNCTION gdalassociated_gdaldataseth(arg1, arg2) RESULT(associated_)
TYPE(gdaldataseth),INTENT(in) :: arg1
TYPE(gdaldataseth),INTENT(in),OPTIONAL :: arg2
LOGICAL :: associated_
IF(PRESENT(arg2)) THEN
  associated_ = C_ASSOCIATED(arg1%ptr, arg2%ptr)
ELSE
  associated_ = C_ASSOCIATED(arg1%ptr)
ENDIF
END FUNCTION gdalassociated_gdaldataseth


SUBROUTINE gdalnullify_gdaldataseth(arg1)
TYPE(gdaldataseth),INTENT(inout) :: arg1
arg1%ptr = C_NULL_PTR
END SUBROUTINE gdalnullify_gdaldataseth
FUNCTION gdalassociated_gdalmajorobjecth(arg1, arg2) RESULT(associated_)
TYPE(gdalmajorobjecth),INTENT(in) :: arg1
TYPE(gdalmajorobjecth),INTENT(in),OPTIONAL :: arg2
LOGICAL :: associated_
IF(PRESENT(arg2)) THEN
  associated_ = C_ASSOCIATED(arg1%ptr, arg2%ptr)
ELSE
  associated_ = C_ASSOCIATED(arg1%ptr)
ENDIF
END FUNCTION gdalassociated_gdalmajorobjecth


SUBROUTINE gdalnullify_gdalmajorobjecth(arg1)
TYPE(gdalmajorobjecth),INTENT(inout) :: arg1
arg1%ptr = C_NULL_PTR
END SUBROUTINE gdalnullify_gdalmajorobjecth
FUNCTION gdalassociated_gdaldriverh(arg1, arg2) RESULT(associated_)
TYPE(gdaldriverh),INTENT(in) :: arg1
TYPE(gdaldriverh),INTENT(in),OPTIONAL :: arg2
LOGICAL :: associated_
IF(PRESENT(arg2)) THEN
  associated_ = C_ASSOCIATED(arg1%ptr, arg2%ptr)
ELSE
  associated_ = C_ASSOCIATED(arg1%ptr)
ENDIF
END FUNCTION gdalassociated_gdaldriverh


SUBROUTINE gdalnullify_gdaldriverh(arg1)
TYPE(gdaldriverh),INTENT(inout) :: arg1
arg1%ptr = C_NULL_PTR
END SUBROUTINE gdalnullify_gdaldriverh
FUNCTION gdalassociated_gdalrasterattributetableh(arg1, arg2) RESULT(associated_)
TYPE(gdalrasterattributetableh),INTENT(in) :: arg1
TYPE(gdalrasterattributetableh),INTENT(in),OPTIONAL :: arg2
LOGICAL :: associated_
IF(PRESENT(arg2)) THEN
  associated_ = C_ASSOCIATED(arg1%ptr, arg2%ptr)
ELSE
  associated_ = C_ASSOCIATED(arg1%ptr)
ENDIF
END FUNCTION gdalassociated_gdalrasterattributetableh


SUBROUTINE gdalnullify_gdalrasterattributetableh(arg1)
TYPE(gdalrasterattributetableh),INTENT(inout) :: arg1
arg1%ptr = C_NULL_PTR
END SUBROUTINE gdalnullify_gdalrasterattributetableh
FUNCTION gdalassociated_gdalcolortableh(arg1, arg2) RESULT(associated_)
TYPE(gdalcolortableh),INTENT(in) :: arg1
TYPE(gdalcolortableh),INTENT(in),OPTIONAL :: arg2
LOGICAL :: associated_
IF(PRESENT(arg2)) THEN
  associated_ = C_ASSOCIATED(arg1%ptr, arg2%ptr)
ELSE
  associated_ = C_ASSOCIATED(arg1%ptr)
ENDIF
END FUNCTION gdalassociated_gdalcolortableh


SUBROUTINE gdalnullify_gdalcolortableh(arg1)
TYPE(gdalcolortableh),INTENT(inout) :: arg1
arg1%ptr = C_NULL_PTR
END SUBROUTINE gdalnullify_gdalcolortableh
FUNCTION gdalassociated_gdalrasterbandh(arg1, arg2) RESULT(associated_)
TYPE(gdalrasterbandh),INTENT(in) :: arg1
TYPE(gdalrasterbandh),INTENT(in),OPTIONAL :: arg2
LOGICAL :: associated_
IF(PRESENT(arg2)) THEN
  associated_ = C_ASSOCIATED(arg1%ptr, arg2%ptr)
ELSE
  associated_ = C_ASSOCIATED(arg1%ptr)
ENDIF
END FUNCTION gdalassociated_gdalrasterbandh


SUBROUTINE gdalnullify_gdalrasterbandh(arg1)
TYPE(gdalrasterbandh),INTENT(inout) :: arg1
arg1%ptr = C_NULL_PTR
END SUBROUTINE gdalnullify_gdalrasterbandh
