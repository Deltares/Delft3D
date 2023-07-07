#ifndef _TECUTILM_H
#define _TECUTILM_H
/*
******************************************************************
******************************************************************
*******                                                   ********
******             (C) 1988-2010 Tecplot, Inc.             *******
*******                                                   ********
******************************************************************
******************************************************************
*/

/* CORE SOURCE CODE REMOVED */

/**
 * @file
 */
#if defined EXTERN
#undef EXTERN
#endif
#if defined TECUTILMMODULE
#define EXTERN
#else
#define EXTERN extern
#endif

/*{{<exclude_fglue>
        TecUtilArgListGetArgCount
        TecUtilArgListGetArgTypeByIndex
        TecUtilArgListGetArgNameByIndex
        TecUtilArgListGetArbParamPtrByIndex
        TecUtilArgListGetDoublePtrByIndex
        TecUtilArgListGetArbParamByIndex
        TecUtilArgListGetDoubleByIndex
        TecUtilArgListGetArrayByIndex
        TecUtilArgListGetFunctionByIndex
        TecUtilArgListGetIntByIndex
        TecUtilArgListGetSetByIndex
        TecUtilArgListGetStringByIndex
        TecUtilArgListGetStringListByIndex
        TecUtilSortUInt32ItemArray UInt32ItemComparator_pf
        TecUtilSortUInt64ItemArray UInt64ItemComparator_pf
  </exclude_fglue> }}*/

/*{{<exclude_tcl>
        TecUtilImageIndexedBitmapCreate
        TecUtilImageRGBGetScanLine
        TecUtilImageIndexedGetScanLine
        TecUtilImageGetColorTable
        TecUtilArgListGetArgCount
        TecUtilArgListGetArgTypeByIndex
        TecUtilArgListGetArgNameByIndex
        TecUtilArgListGetArbParamPtrByIndex
        TecUtilArgListGetDoublePtrByIndex
        TecUtilArgListGetArbParamByIndex
        TecUtilArgListGetDoubleByIndex
        TecUtilArgListGetArrayByIndex
        TecUtilArgListGetFunctionByIndex
        TecUtilArgListGetIntByIndex
        TecUtilArgListGetSetByIndex
        TecUtilArgListGetStringByIndex
        TecUtilArgListGetStringListByIndex
        TecUtilSortUInt32ItemArray UInt32ItemComparator_pf
        TecUtilSortUInt64ItemArray UInt64ItemComparator_pf
  </exclude_tcl>}}*/

/*{{<exclude_python>
        TecUtil3DViewGetNearZPlane
        TecUtilAnimateXYMaps
        TecUtilAnimateXYMapsX
        TecUtilContourLabelAdd
        TecUtilContourLabelDeleteAll
        TecUtilContourLevelDeleteRange
        TecUtilContourLevelDelNearest
        TecUtilContourLevelNew
        TecUtilContourLevelReset
        TecUtilCreateSimpleXYZone
        TecUtilCreateSliceZone
        TecUtilPickAddXYMaps
        TecUtilPickSetMouseMode
        TecUtilPolarToRectangular
        TecUtilRotate2D
        TecUtilViewRotate
        TecUtilViewScale
        TecUtilWriteCurveInfo
        TecUtilXYMapCopy
        TecUtilXYMapCreate
        TecUtilXYMapDelete
        TecUtilXYMapShiftToBottom
        TecUtilXYMapShiftToTop
        TecUtilZoneDelete
        TecUtilImageIndexedBitmapCreate
        TecUtilImageRGBGetScanLine
        TecUtilImageIndexedGetScanLine
        TecUtilImageGetColorTable
        TecUtilArgListGetArgCount
        TecUtilArgListGetArgTypeByIndex
        TecUtilArgListGetArgNameByIndex
        TecUtilArgListGetArbParamPtrByIndex
        TecUtilArgListGetDoublePtrByIndex
        TecUtilArgListGetArbParamByIndex
        TecUtilArgListGetDoubleByIndex
        TecUtilArgListGetArrayByIndex
        TecUtilArgListGetFunctionByIndex
        TecUtilArgListGetIntByIndex
        TecUtilArgListGetSetByIndex
        TecUtilArgListGetStringByIndex
        TecUtilArgListGetStringListByIndex
        TecUtilSortUInt32ItemArray UInt32ItemComparator_pf
        TecUtilSortUInt64ItemArray UInt64ItemComparator_pf
  </exclude_python>}}*/

/*{{<windows_only>
        TecUtilWinCopyToClipboard
    </windows_only>}}*/

/*{{<exclude_all>
  TecUtilIsBusy
  </exclude_all>}}*/


/**
 * Allocate an argument list. Argument lists are used with all of the "X"
 * (extended) functions in Tecplot.
 *
 * @return
 *   Returns an ArgList object if successful, otherwise NULL.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilArgListAlloc(ResultPtr)
 *    POINTER (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ArgLists
 *
 */
LINKTOADDON TP_GIVES ArgList_pa STDCALL TecUtilArgListAlloc(void);
/**
 * Clear all arguments from an argument list.
 *
 *
 * @pre <em>ArgListIsValid(ArgList)</em>
 *   Argument list must be valid.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilArgListClear(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ArgLists
 *
 */
LINKTOADDON void       STDCALL TecUtilArgListClear(ArgList_pa ArgList);
/**
 * Append an integer parameter to an argument list.
 *
 * @param ArgList
 *   The argument list.
 *
 * @param Name
 *   Parameter name. Use the SV_constants listed with the extended function.
 *
 * @param Value
 *   Value associated with the parameter name.
 *
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 * @pre <em>Name</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendInt(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   Value)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    INTEGER*4       Value
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ArgLists
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilArgListAppendInt(ArgList_pa   ArgList,
                                                       const char  *Name,
                                                       LgIndex_t    Value);
/**
 * Append an ArbParam_t type parameter to an argument list
 *
 * @param ArgList
 *   The argument list.
 *
 * @param Name
 *   Parameter name. Use the SV_constants listed with the extended function.
 *
 * @param Value
 *   Value associated with the parameter name.
 *
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 * @pre <em>Name</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendArbParam(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   ValuePtr)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    POINTER         (ValuePtr, Value)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ArgLists
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilArgListAppendArbParam(ArgList_pa  ArgList,
                                                            const char *Name,
                                                            ArbParam_t  Value);
/**
 * Appends an ArbParam_t pointer parameter to an argument list.
 *
 * @param ArgList
 *   The argument list
 *
 * @param Name
 *   Parameter Name. Use the SV_ constants listed with the extended function.
 *
 * @param Value
 *   Pointer value associated with the parameter name
 *
 * @return
 *   Returns TRUE is append is successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 * @pre <em>Name</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>Value</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendArbParamPtr(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   ValuePtr)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    POINTER         (ValuePtr, Value)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ArgLists
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilArgListAppendArbParamPtr(ArgList_pa            ArgList,
                                                              const char*           Name,
                                                              TP_IN_OUT ArbParam_t* Value);
/**
 * Append a double parameter to an argument list.
 *
 * @param ArgList
 *   The argument list.
 *
 * @param Name
 *   Parameter name. Use the SV_constants listed with the extended function.
 *
 * @param Value
 *   Value associated with the parameter name.
 *
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 * @pre <em>Name</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendDouble(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   Value)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    REAL*8          Value
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ArgLists
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilArgListAppendDouble(ArgList_pa  ArgList,
                                                          const char *Name,
                                                          double      Value);
/**
 * Appends a double pointer parameter to an argument list.
 *
 * @param ArgList
 *   The argument list
 *
 * @param Name
 *   Parameter Name. Use the SV_ constants listed with the extended function
 *
 * @param Value
 *   Pointer value associated with the parameter name
 *
 * @return
 *   Returns TRUE is append is successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 * @pre <em>Name</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>Value</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendDoublePtr(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   Value)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    REAL*8          Value
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * See TecUtilStyleGetLowLevelX() for a complete example.
 *
 * @ingroup ArgLists
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilArgListAppendDoublePtr(ArgList_pa        ArgList,
                                                             const char*       Name,
                                                             TP_IN_OUT double* Value);
/**
 * Append a string to an argument list.
 *
 * @param ArgList
 *   The argument list.
 *
 * @param Name
 *   Parameter name. Use the SV_constants listed with the extended function.
 *
 * @param Value
 *   String value associated with the parameter name.
 *
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 * @pre <em>Name</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>Value</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendString(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   Value)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    CHARACTER*(*)   Value
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ArgLists
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilArgListAppendString(ArgList_pa  ArgList,
                                                          const char *Name,
                                                          const char *Value);
/**
 * Appends a named array reference to the argument list.
 *
 * @param ArgList
 *   The argument list.
 *
 * @param Name
 *   Parameter name. Use the SV_constants listed with the extended function.
 *
 * @param Value
 *   Array reference associated with the parameter name. Tecplot does not
 *   make a copy of the array so the client must maintain the array until
 *   the ArgList is cleared or deallocated at which point the array can
 *   be deallocated.
 *
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 * @pre <em>Name</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>Value</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendArray(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   Value)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    POINTER         (ValuePtr, Value)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ArgLists
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilArgListAppendArray(ArgList_pa  ArgList,
                                                         const char *Name,
                                                         const void *Value);
/**
 * Append a named set reference to an argument list.
 *
 * @param ArgList
 *   The argument list.
 *
 * @param Name
 *   Parameter name. Use the SV_constants listed with the extended function.
 *
 * @param Value
 *   Set reference associated with the parameter name. Tecplot does not
 *   make a copy of the set so the client must maintain the set until
 *   the ArgList is cleared or deallocated at which point the set can
 *   be deallocated.
 *
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 * @pre <em>Name</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>IMPLICATION(Set != NULL,VALID_REF(Set))</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendSet(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   ValuePtr)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    POINTER         (ValuePtr, Value)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ArgLists
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilArgListAppendSet(ArgList_pa  ArgList,
                                                       const char *Name,
                                                       Set_pa      Value);
/**
 * Append a named string list reference to an argument list.
 *
 * @param ArgList
 *   The argument list.
 *
 * @param Name
 *   Parameter name. Use the SV_constants listed with the extended function.
 *
 * @param StringList
 *   String list reference associated with the parameter name. Tecplot does not
 *   make a copy of the list so the client must maintain the string list until
 *   the ArgList is cleared or deallocated at which point the string list can
 *   be deallocated.
 *
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 * @pre <em>Name</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>StringList</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendStringList(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   StringListPtr)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    POINTER         (StringListPtr, StringList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ArgLists
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilArgListAppendStringList(ArgList_pa     ArgList,
                                                             const char    *Name,
                                                             StringList_pa  StringList);
/**
 * Append a named function pointer to an argument list.
 *
 * @param ArgList
 *   The argument list.
 *
 * @param Name
 *   Parameter name. Use the SV_constants listed with the extended function.
 *
 * @param Value
 *   Function pointer associated with the parameter name.
 *
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 * @pre <em>Name</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>Value</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendFunction(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   Value)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    POINTER         (ValuePtr, Value)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ArgLists
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilArgListAppendFunction(ArgList_pa  ArgList,
                                                           const char *Name,
                                                           const void *Value);
/**
 * Deallocate an argument list.
 *
 * @param ArgList
 *   The argument list.
 *
 *
 * @pre <em>ArgList</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid or NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilArgListDealloc(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ArgLists
 *
 */
LINKTOADDON void STDCALL TecUtilArgListDealloc(TP_RECEIVES_GIVES ArgList_pa* ArgList);

/**
 * Returns the number of arguments in the ArgList.
 *
 * @since 12.0.1
 *
 * @param ArgList
 *     The argument list.
 *
 * @return
 *     The number of items in the argument list.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * @ingroup ArgLists
 *   
 * #internalattributes exclude_alldoc 
 */
LINKTOADDON LgIndex_t STDCALL TecUtilArgListGetArgCount(ArgList_pa ArgList);

/**
 * Returns the specified argument's type.
 *
 * @since 12.0.1
 *
 * @param ArgList
 *     The argument list.
 * @param Index
 *     Index of the argument in question.
 *
 * @return
 *     The specified argument's type.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * @ingroup ArgLists
 *   
 * #internalattributes exclude_alldoc
 */
LINKTOADDON ArgListArgType_e STDCALL TecUtilArgListGetArgTypeByIndex(ArgList_pa ArgList,
                                                                     LgIndex_t  Index);

/**
 * Returns an allocated copy of the specified argument's name.
 *
 * @since 12.0.1
 *
 * @param ArgList
 *     The argument list.
 * @param Index
 *     Index of the argument in question.
 *
 * @return
 *     Allocated character string representing the specified argument's name.
 *     The client is responsible for deallocating the result when it is no
 *     longer needed using TecUtilStringDealloc().
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * @ingroup ArgLists
 *   
 * #internalattributes exclude_alldoc
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilArgListGetArgNameByIndex(ArgList_pa ArgList,
                                                                   LgIndex_t  Index);

/**
 * Returns the ArbParam_t pointer value reference of the specified argument.
 * The argument's type must match the request.
 *
 * @since 12.0.1
 *
 * @param ArgList
 *     The argument list.
 * @param Index
 *     Index of the argument in question.
 *
 * @return
 *     The ArbParam_t pointer value reference of the specified argument.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * @ingroup ArgLists
 *   
 * #internalattributes exclude_alldoc
 */
LINKTOADDON ArbParam_t* STDCALL TecUtilArgListGetArbParamPtrByIndex(ArgList_pa ArgList,
                                                                    LgIndex_t  Index);

/**
 * Returns the double pointer value reference of the specified argument. The
 * argument's type must match the request.
 *
 * @since 12.0.1
 *
 * @param ArgList
 *     The argument list.
 * @param Index
 *     Index of the argument in question.
 *
 * @return
 *     The double pointer value reference of the specified argument.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * @ingroup ArgLists
 *   
 * #internalattributes exclude_alldoc
 */
LINKTOADDON double* STDCALL TecUtilArgListGetDoublePtrByIndex(ArgList_pa ArgList,
                                                              LgIndex_t  Index);

/**
 * Returns the ArbParam_t value of the specified argument. The argument's type
 * must match the request.
 *
 * @since 12.0.1
 *
 * @param ArgList
 *     The argument list.
 * @param Index
 *     Index of the argument in question.
 *
 * @return
 *     The ArbParam_t value of the specified argument.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * @ingroup ArgLists
 *   
 * #internalattributes exclude_alldoc
 */
LINKTOADDON ArbParam_t STDCALL TecUtilArgListGetArbParamByIndex(ArgList_pa ArgList,
                                                                LgIndex_t  Index);

/**
 * Returns the double value of the specified argument. The argument's type must
 * match the request.
 *
 * @since 12.0.1
 *
 * @param ArgList
 *     The argument list.
 * @param Index
 *     Index of the argument in question.
 *
 * @return
 *     The double value of the specified argument.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * @ingroup ArgLists
 *   
 * #internalattributes exclude_alldoc
 */
LINKTOADDON double STDCALL TecUtilArgListGetDoubleByIndex(ArgList_pa ArgList,
                                                          LgIndex_t  Index);

/**
 * Returns the array value reference of the specified argument. The argument's
 * type must match the request.
 *
 * @since 12.0.1
 *
 * @param ArgList
 *     The argument list.
 * @param Index
 *     Index of the argument in question.
 *
 * @return
 *     The array value reference of the specified argument.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * @ingroup ArgLists
 *   
 * #internalattributes exclude_alldoc
 */
LINKTOADDON void* STDCALL TecUtilArgListGetArrayByIndex(ArgList_pa ArgList,
                                                        LgIndex_t  Index);

/**
 * Returns the function value reference of the specified argument. The
 * argument's type must match the request.
 *
 * @since 12.0.1
 *
 * @param ArgList
 *     The argument list.
 * @param Index
 *     Index of the argument in question.
 *
 * @return
 *     The function value reference of the specified argument.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * @ingroup ArgLists
 *   
 * #internalattributes exclude_alldoc
 */
LINKTOADDON void* STDCALL TecUtilArgListGetFunctionByIndex(ArgList_pa ArgList,
                                                           LgIndex_t  Index);

/**
 * Returns the integer value of the specified argument. The argument's type
 * must match the request.
 *
 * @since 12.0.1
 *
 * @param ArgList
 *     The argument list.
 * @param Index
 *     Index of the argument in question.
 *
 * @return
 *     The integer value of the specified argument.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * @ingroup ArgLists
 *   
 * #internalattributes exclude_alldoc
 */
LINKTOADDON int STDCALL TecUtilArgListGetIntByIndex(ArgList_pa ArgList,
                                                    LgIndex_t  Index);

/**
 * Returns the Set_pa value reference of the specified argument. The argument's
 * type must match the request.
 *
 * @since 12.0.1
 *
 * @param ArgList
 *     The argument list.
 * @param Index
 *     Index of the argument in question.
 *
 * @return
 *     The Set_pa value reference of the specified argument.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * @ingroup ArgLists
 *   
 * #internalattributes exclude_alldoc
 */
LINKTOADDON Set_pa STDCALL TecUtilArgListGetSetByIndex(ArgList_pa ArgList,
                                                       LgIndex_t  Index);

/**
 * Returns an allocated copy of the string value of the specified argument. The
 * argument's type must match the request.
 *
 * @since 12.0.1
 *
 * @param ArgList
 *     The argument list.
 * @param Index
 *     Index of the argument in question.
 *
 * @return
 *     Allocated character string representing the specified argument's value.
 *     The client is responsible for deallocating the result when it is no
 *     longer needed using TecUtilStringDealloc().
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * @ingroup ArgLists
 *   
 * #internalattributes exclude_alldoc
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilArgListGetStringByIndex(ArgList_pa ArgList,
                                                                  LgIndex_t  Index);

/**
 * Returns the string list value reference of the specified argument. The
 * argument's type must match the request.
 *
 * @since 12.0.1
 *
 * @param ArgList
 *     The argument list.
 * @param Index
 *     Index of the argument in question.
 *
 * @return
 *     The string list value reference of the specified argument.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * @ingroup ArgLists
 *   
 * #internalattributes exclude_alldoc
 */
LINKTOADDON StringList_pa STDCALL TecUtilArgListGetStringListByIndex(ArgList_pa ArgList,
                                                                     LgIndex_t  Index);

/**
 * Redistribute the control points for the currently active color map so they
 * are evenly spaced across the color spectrum. This is equivalent to clicking
 * Redistribute Control Points in the Workspace/Color Map dialog. Note that
 * this does not change the RGB values assigned at each control point.
 *
 * Note that this function does nothing if the currently active colormap is a standard colormap,
 * since the standard color maps cannot be changed.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilColorMapRedistControlPts()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ColorMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilColorMapRedistControlPts(void);
/**
 *
 * Preset either the user-defined color map or the raw user-defined color map
 * to be a copy of one of the standard color maps. Tecplot must currently be
 * using either the user-defined color map or the raw user-defined color map in
 * order to use the function.
 *
 * @param ColorMap
 *   Color map type to use as the source. The possible values are: ContourColorMap_SmRainbow,
 *   ContourColorMap_LgRainbow, ContourColorMap_Modern, ContourColorMap_GrayScale or ContourColorMap_TwoColor
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilColorMapCopyStandard(ColorMap)
 *    INTEGER*4 ColorMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Reset the user-defined color map to be a copy of the modern color map.
 *
 * @code
 *   TecUtilColorMapSetBase(ContourColorMap_UserDef);
 *   TecUtilColorMapCopyStandard(ContourColorMap_Modern);
 * @endcode
 *
 * @ingroup ColorMap
 *
 */

LINKTOADDON Boolean_t STDCALL TecUtilColorMapCopyStandard(ContourColorMap_e ColorMap);
/* - NO DOXYGEN COMMENT GENERATION -
 * @deprecated
 *  Not necessary in the new colormap system.  
 *
 * Redistribute the control points and reset the RGB values for the currently
 * active color map. This is equivalent to clicking Reset on the
 * Workspace/ColorMap dialog.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilColorMapResetToFactory()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ColorMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilColorMapResetToFactory(void);

/**
 * Check to see if the supplied frame needs to be redrawn
 *
 * @param FrameID
 *   Unique ID of the frame for which the query is made.
 *
 * @pre
 *  There must be at least one page and one frame.
 *
 * @return
 *   TRUE if the frame associated with the supplied FrameID exists
 *   and the frame needs to be redrawn, FALSE otherwise.
 *
 * @pre Must have one or more pages.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameNeedsRedraw()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Drawing
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameNeedsRedraw(UniqueID_t FrameID);

/**
 * Redraw the current frame.
 *
 * @param DoFullDrawing
 *   Set to FALSE to only draw a trace of the data in the current frame. Set to
 *   TRUE to do a full redraw.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilRedraw(DoFullDrawing)
 *    INTEGER*4 DoFullDrawing
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Do a complete redraw of the current frame:
 *
 * @code
 *   TecUtilRedraw(TRUE);
 * @endcode
 *
 * @ingroup Drawing
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilRedraw(Boolean_t DoFullDrawing);

/**
 * Redraw all frames.
 *
 * @param DoFullDrawing
 *   Set to FALSE to only draw a trace of the data in all frames. Set to TRUE
 *   to do a full redraw.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilRedrawAll(DoFullDrawing)
 *    INTEGER*4 DoFullDrawing
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Do a complete redraw of all frames:
 *
 * @code
 *   TecUtilRedrawAll(TRUE);
 * @endcode
 *
 * @ingroup Drawing
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilRedrawAll(Boolean_t DoFullDrawing);
/**
 *   Implements a double buffer action as described below by the DoubleBufferAction parameter
 *   definitions.
 *
 * @param DoubleBufferAction
 *   Action to take. The possible values are:\n
 *
 *   - DoubleBufferAction_Off (turns off double buffering. Use this command
 *     once at the end of a sequence of using the double buffer).
 *   - DoubleBufferAction_On (turns on double buffering.  Use this command once
 *     at the beginning of a sequence of using the double buffer. While double
 *     buffering is turned on, all drawing is sent to the back buffer).
 *   - DoubleBufferAction_Swap (swaps the back buffer to the front.  In
 *     other words, copy the image in the back buffer to the front).
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDoubleBuffer(DoubleBufferAction)
 *    INTEGER*4 DoubleBufferAction
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDoubleBuffer(DoubleBufferAction_e DoubleBufferAction);

/**
 * Suspends or un-suspends updates to the work area only. Updates to the
 * sidebar, menu, and status line are still preformed unless suspended via a
 * call to TecUtilDrawGraphics(), TecUtilInterfaceSuspend() or
 * TecUtilStatusSuspend().
 *
 * @since
 *   11.0-3-010
 *
 * @param DoSuspend
 *    Indicates if Tecplot should suspend or un-suspend making updates to the
 *    work area.
 *
 * @sa TecUtilDrawGraphics, TecUtilInterfaceSuspend, TecUtilStatusSuspend
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup WorkArea
 */
LINKTOADDON void STDCALL TecUtilWorkAreaSuspend(Boolean_t DoSuspend);

/**
 * Suspends or un-suspends updates to the interface only (not including the
 * status line). Updates to the work area and status line are still preformed
 * unless suspended via a call to TecUtilDrawGraphics(),
 * TecUtilWorkAreaSuspend() or TecUtilStatusSuspend().
 *
 * @since
 *   11.0-3-010
 *
 * @param DoSuspend
 *    Indicates if Tecplot should suspend or un-suspend making updates to the
 *    interface.
 *
 * @sa TecUtilDrawGraphics, TecUtilWorkAreaSuspend, TecUtilStatusSuspend
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilInterfaceSuspend(Boolean_t DoSuspend);

/**
 * Suspends or un-suspends updates to the status line only. Updates to the work
 * area, sidebar, and menu are still preformed unless suspended via a call to
 * TecUtilDrawGraphics(), TecUtilWorkAreaSuspend() or
 * TecUtilInterfaceSuspend().
 *
 * @since
 *   11.0-3-010
 *
 * @param DoSuspend
 *    Indicates if Tecplot should suspend or un-suspend making updates to the
 *    status line.
 *
 * @sa TecUtilDrawGraphics, TecUtilWorkAreaSuspend, TecUtilInterfaceSuspend
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup StatusBar
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilStatusSuspend(Boolean_t DoSuspend);

/**
 * Turn on or off all graphics drawing. Updates to the work area, sidebar,
 * menu, and status line are suspended unless un-suspended via a call to
 * TecUtilWorkAreaSuspend(), TecUtilInterfaceSuspend() or
 * TecUtilStatusSuspend().
 *
 * @param DoDrawing
 *   TRUE to turn graphics drawing on, FALSE to turn off
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDrawGraphics(DoDrawing)
 *    INTEGER*4 DoDrawing
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Drawing
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDrawGraphics(Boolean_t DoDrawing);

/**
 * Terminate the execution of the Tecplot program. Note that program control
 * returns to your application before quitting. Calling TecUtilQuit() merely
 * sends a suggestion to Tecplot to quit. The only code that follows a call to
 * TecUtilQuit() should be one or more calls to TecUtilLockFinish().
 *
 * @return
 *   Currently TRUE is always returned.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQuit()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Exit Tecplot:
 * @code
 *   TecUtilLockStart(AddOnID);
 *   TecUtilQuit();
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Utilities
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilQuit(void);

/**
 * Creates a new page.
 *
 * @return TRUE is pages created successfully.
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPageCreateNew()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since
 *   11.0-5-014
 *
 * @ingroup PageManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPageCreateNew(void);

/**
 * Changes the current page to the next one in the internal
 * linked list of pages.
 *
 * @pre Must have one or more pages.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPageSetCurrentToNext()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since
 *   11.0-5-014
 *
 * @ingroup PageManagement
 *
 */
LINKTOADDON void STDCALL TecUtilPageSetCurrentToNext(void);

/**
 * Changes the current page to the previous one in the internal
 * linked list of pages.
 *
 * @pre Must have one or more pages.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPageSetCurrentToPrev()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since
 *   11.0-5-014
 *
 * @ingroup PageManagement
 *
 */
LINKTOADDON void STDCALL TecUtilPageSetCurrentToPrev(void);

/**
 * Sets the current page to one matching the passed name.
 *
 * @param PageName the name of the page to be set as the current.
 *
 * @return TRUE is a page was found for the passed name.
 *
 *
 * @pre <em>PageName</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPageSetCurrentByName(PageName)
 *    CHARACTER*(*) PageName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since
 *   11.0-5-014
 *
 * @ingroup PageManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPageSetCurrentByName(const char * PageName);

/**
 * Sets the current page to one matching the passed unique ID.
 *
 * @param UniqueID ID of the page to be set as the current.
 *
 * @return TRUE is a page was found for the passed ID.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPageSetCurrentByUniqueID(UniqueID)
 *    POINTER         (ValuePtr, UniqueID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since
 *   11.0-5-014
 *
 * @ingroup PageManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPageSetCurrentByUniqueID(UniqueID_t UniqueID);

/**
 * Deletes the current page. Will set the next page from the internal linked list
 * to be current.
 *
 * @pre Must have one or more pages.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPageDelete()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since
 *   11.0-5-014
 *
 * @ingroup PageManagement
 *
 */
LINKTOADDON void STDCALL TecUtilPageDelete(void);

/**
 * Deletes all frames in the current page.
 *
 * @pre Must have one or more pages.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPageClear()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since
 *   11.0-5-014
 *
 * @ingroup PageManagement
 *
 */
LINKTOADDON void STDCALL TecUtilPageClear(void);

/**
 *   Creates a new frame.
 *
 * @param UseSuppliedFrameSize
 *   If set to TRUE, then XYPos, Width, and Height are used to position and size the new frame.
 *   Otherwise, the frame is created with the position and size of the factory default frame
 *
 * @param XPos
 *   X-Position (in inches) of the frame relative to the left edge of the paper. The frame is anchored
 *   in its upper left corner.
 *
 * @param YPos
 *   Y-Position (in inches) of the frame relative to the top edge of the paper.
 *
 * @param Width
 *   Width of the frame in inches.
 *
 * @param Height
 *   Height of the frame in inches. Note that the Y-direction is positive in the down direction on the
 *   paper
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameCreateNew(
 *   &                   UseSuppliedFrameSize,
 *   &                   XPos,
 *   &                   YPos,
 *   &                   Width,
 *   &                   Height)
 *    INTEGER*4       UseSuppliedFrameSize
 *    REAL*8          XPos
 *    REAL*8          YPos
 *    REAL*8          Width
 *    REAL*8          Height
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create two new frames:
 *
 * @code
 *   // let Tecplot position the frame
 *   Boolean_t IsOk1 = TecUtilFrameCreateNew(FALSE,0,0,0,0);
 *
 *   // use an absolute position
 *   Boolean_t IsOk2 = TecUtilFrameCreateNew(TRUE, 1,1,7,7);
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameCreateNew(Boolean_t UseSuppliedFrameSize,
                                                    double    XPos,
                                                    double    YPos,
                                                    double    Width,
                                                    double    Height);

/**
 * @deprecated
 *   Please use TecUtilFrameMoveToTopByName() followed by TecUtilFrameActivateTop() instead.
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFramePopByName(const char *Name);
/**
 * @deprecated
 *   Please use TecUtilFrameMoveToBottomByName() followed by TecUtilFrameActivateTop() instead.
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFramePushByName(const char *Name);
/**
 * @deprecated
 *   Please use TecUtilFrameMoveToTopByUniqueID() followed by TecUtilFrameActivateTop() instead.
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFramePopByUniqueID(UniqueID_t UniqueID);
/**
 * @deprecated
 *   Please use TecUtilFrameMoveToBottomByUniqueID() followed by TecUtilFrameActivateTop() instead.
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFramePushByUniqueID(UniqueID_t UniqueID);
/**
 * @deprecated
 *   Please use TecUtilFrameMoveToBottomByNumber() followed by TecUtilFrameActivateTop() instead.
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFramePushTop(void);
/**
 * @deprecated
 *   Please use TecUtilFrameMoveToBottomByNumber() followed by TecUtilFrameActivateTop() instead.
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFramePush(int FrameNum);

/**
 * @deprecated
 *   Please use TecUtilFrameMoveToTopByNumber() followed by TecUtilFrameActivateTop() instead.
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFramePop(int FrameNum);

/**
 * Pop the frame on top at a specified position on the paper.
 *
 * @param X
 *   X-Coordinates to specify a position on the paper of the frame to be popped. X is in inches from
 *   the left edge of the paper.
 *
 * @param Y
 *   Y-Coordinates to specify a position on the paper of the frame to be popped. Y is in inches from
 *   the top edge of the paper.
 *
 * @return
 *   Always returns TRUE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFramePopAtPosition(
 *   &                   X,
 *   &                   Y)
 *    REAL*8          X
 *    REAL*8          Y
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Pop the frame currently visible at one inch from the left, two inches from the right of the paper,
 *   and make that frame the current frame:
 *
 * @code
 *   TecUtilFramePopAtPosition(1.0, 2.0);
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFramePopAtPosition(double X,
                                                        double Y);
/**
 * @deprecated
 *   Please use TecUtilFrameDeleteActive() instead.
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameDeleteTop(void);

/**
 *   Activate the top frame.
 *
 * @since
 *   11.4-1-1120
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameActivateTop()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameActivateTop(void);
/**
 *   Activate a frame (specified by name).
 *
 * @since
 *   11.4-1-1120
 *
 * @param Name
 *   Name of the frame to activate. Use TecUtilFrameGetName() to get the name of the
 *   current frame.
 *
 * @return
 *   TRUE if Name is a valid frame name (and thus the frame is activated).
 *
 *
 * @pre <em>Name</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameActivateByName(Name)
 *    CHARACTER*(*) Name
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Activate the frame with the name "Frame 001":
 *
 * @code
 *   TecUtilFrameActivateByName("Frame 001");
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameActivateByName(const char *Name);
/**
 *   Activate a frame.
 *
 * @since
 *   11.4-1-1120
 *
 * @param FrameNum
 *   Frame to be activated. Frames are numbered 1 to numframes with frame 1 drawn
 *   first and the highest numbered frame drawn last when RedrawAll is
 *   executed. See TecUtilFrameGetCount() to get the number of frames currently
 *   defined.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameActivateByNumber(FrameNum)
 *    INTEGER*4 FrameNum
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Activate the frame underneath the top frame assuming that there are at least two frames.
 *
 * @code
 *   TecUtilFrameActivateByNumber(TecUtilFrameGetCount() - 1);
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameActivateByNumber(int FrameNum);
/**
 *   Activates a frame identified by a unique ID.
 *
 * @since
 *   11.4-1-1120
 *
 * @param UniqueID
 *   Unique ID of the frame.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameActivateByUniqueID(UniqueID)
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Activate the current frame using a unique id.
 *
 * @code
 *   {
 *     UniqueID_t id;
 *     TecUtilLockStart(AddOnID);
 *     id = TecUtilFrameGetUniqueID();
 *     TecUtilFrameActivateByUniqueID(id);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameActivateByUniqueID(UniqueID_t UniqueID);
/**
 *   Activate the frame on top at a specified position on the paper.
 *
 * @since
 *   11.4-1-1120
 *
 * @param X
 *   X-Coordinates to specify a position on the paper of the frame to be activated. X is in inches from
 *   the left edge of the paper.
 *
 * @param Y
 *   Y-Coordinates to specify a position on the paper of the frame to be activated. Y is in inches from
 *   the top edge of the paper.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameActivateAtPosition(
 *   &                   X,
 *   &                   Y)
 *    REAL*8          X
 *    REAL*8          Y
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Activate the frame currently visible at one inch from the left, two inches from the right of the paper,
 *   and make that frame the current frame:
 *
 * @code
 *   TecUtilFrameActivateAtPosition(1.0, 2.0);
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameActivateAtPosition(double X,
                                                             double Y);
/**
 *   Delete the active frame. If there is only one frame when this is called, a new empty frame
 *   is automatically created after this command is executed. (Thus, you can never have a workspace
 *   without at least one frame.)
 *
 * @since
 *   11.4-1-1120
 *
 * @return
 *   TRUE is successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameDeleteActive()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameDeleteActive(void);
/**
 *   Pop a frame (specified by name) to the top, but do not activate it.
 *
 * @since
 *   11.4-1-1120
 *
 * @param Name
 *   Name of the frame to pop. Use TecUtilFrameGetName() to get the name of the
 *   active frame.
 *
 * @return
 *   TRUE if Name is a valid frame name (and thus the frame is moved to the top).
 *
 *
 * @pre <em>Name</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameMoveToTopByName(Name)
 *    CHARACTER*(*) Name
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Pop the frame with the name "Frame 001", but do not make that frame the active frame:
 *
 * @code
 *   TecUtilFrameMoveToTopByName("Frame 001");
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameMoveToTopByName(const char *Name);
/**
 *   Pop a frame to the top, but do not make it the active frame.
 *
 * @since
 *   11.4-1-1120
 *
 * @param FrameNum
 *   Frame to be popped. Frames are numbered 1 to numframes with frame 1 drawn
 *   first and the highest numbered frame drawn last when RedrawAll is
 *   executed. See TecUtilFrameGetCount() to get the number of frames currently
 *   defined.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameMoveToTopByNumber(FrameNum)
 *    INTEGER*4 FrameNum
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Pop the frame underneath the current frame assuming that there are at least two frames,
 *   but do not activate the frame:
 *
 * @code
 *   TecUtilFrameMoveToTopByNumber(TecUtilFrameGetCount() - 1);
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameMoveToTopByNumber(int FrameNum);
/**
 *   Pops a frame identified by a unique ID, but does not activate the frame.
 *
 * @since
 *   11.4-1-1120
 *
 * @param UniqueID
 *   Unique ID of the frame.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameMoveToTopByUniqueID(UniqueID)
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Pop the active frame using a unique id:
 *
 * @code
 *   {
 *     UniqueID_t id;
 *     TecUtilLockStart(AddOnID);
 *     id = TecUtilFrameGetUniqueID();
 *     TecUtilFrameMoveToTopByUniqueID(id);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameMoveToTopByUniqueID(UniqueID_t UniqueID);
/**
 *   Push a frame (specified by name) to the bottom. Does not affect which frame is active.
 *
 * @since
 *   11.4-1-1120
 *
 * @param Name
 *   Name of the frame to push. Use TecUtilFrameGetName() to get the name of the
 *   active frame
 *
 * @return
 *   TRUE if Name is a valid frame name (and thus the frame is pushed), otherwise FALSE.
 *
 *
 * @pre <em>Name</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameMoveToBottomByName(Name)
 *    CHARACTER*(*) Name
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Push the frame with the name "Frame 001" to the bottom, but do not change which frame
 *   is currently the active frame:
 *
 * @code
 *   TecUtilFrameMoveToBottomByName("Frame 001");
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameMoveToBottomByName(const char *Name);
/**
 * Push a frame to the bottom of the frame stack. In other words it is given
 * the frame number 1 and therefore drawn first. Do not alter which frame
 * is the active frame.
 *
 * @since
 *   11.4-1-1120
 *
 * @param FrameNum
 *   Frame to be pushed. Frames are numbered 1 to numframes with frame 1 drawn
 *   first and the highest numbered frame drawn last when Redraw All is
 *   executed. See TecUtilFrameGetCount() to get the number of frames currently
 *   defined.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameMoveToBottomByNumber(FrameNum)
 *    INTEGER*4 FrameNum
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Assuming that there are at least two frames, push the frame underlying the
 * top frame to the bottom of the frame stack. Do not modify which frame is
 * the active frame.
 *
 * @code
 *   TecUtilFrameMoveToBottomByNumber(TecUtilFrameGetCount() - 1);
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameMoveToBottomByNumber(int FrameNum);

/**
 *   Pushes a frame identified by a unique ID. Does not affect which frame
 *   is the active frame.
 *
 * @since
 *   11.4-1-1120
 *
 * @param UniqueID
 *   Unique ID of the frame.
 *
 * @return
 *   TRUE if successful, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameMoveToBottomByUniqueID(UniqueID)
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *  Push the active frame using a unique id
 *
 * @code
 *   {
 *     UniqueID_t id;
 *     TecUtilLockStart(AddOnID);
 *     id = TecUtilFrameGetUniqueID();
 *     TecUtilFrameMoveToBottomByUniqueID(id);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameMoveToBottomByUniqueID(UniqueID_t UniqueID);


/**
 * Moves a frame identified by an unique ID to a new location in the frame stack. Does not affect
 * which frame is the active frame
 *
 * @since
 *   14.1
 *
 * @param UniqueID
 *   Unique ID of the frame.
 * @param NewNumber
 *   The number of the new position of the frame in the frame stack
 *
 * @return
 *   TRUE if successful, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameSetNumberByUniqueID(UniqueID, NewNumber)
 *    INTEGER*4 UniqueID
 *    INTEGER*4 NewNumber
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameSetNumberByUniqueID(UniqueID_t UniqueID, int NewNumber);

/**
 *   Resize all frames so that they fit inside the hardclip limits of the paper.
 *
 * @return
 *   TRUE is successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameFitAllToPaper()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameFitAllToPaper(void);

/**
 *   This function gets the current background color of the frame.
 *
 * @return
 *   Current background color of frame. The possible values are: Black_C, Blue_C, Red_C, Green_C,
 *   Cyan_C, Purple_C, Yellow_C, White_C. CustomXX_C (where XX can be any number between 1 and 64.)
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameGetBackgroundColor()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON ColorIndex_t STDCALL TecUtilFrameGetBackgroundColor(void);

/**
 * Sets the frame background color to the specified color and surveys all basic color assignments
 * in Tecplot 360, converting the all basic colors using the following rules to achieve the
 * best contrast:
 *  1. For all line type basic colors that match the new basic frame color, set the basic line
 *     color to the best show color of the basic frame color.
 * 2. For all fill type basic colors that match the best show color of the new basic frame
 *    color, set the fill color to the new frame color.
 * Exceptions:
 * 1. For geometries and text boxes if the line and fill colors are the same and filling is
 *    active then both lines and fill follow the fill rules above.
 * 2. For zone, slice, iso-surface, and streamtrace object types the basic color shading (i.e.
 *    fill) only follows the fill rules above if lighting effects are not being used.
 *
 * @since
 *   11.3-5-002
 * @param color
 *   index of the color. The possible values are: Black_C, Blue_C, Red_C, Green_C,
 *   Cyan_C, Purple_C, Yellow_C, White_C. CustomXX_C (where XX can be any number between 1 and 64.)
 *
 * @return
 *   SetValueReturnCode_Ok if suceeded or other SetValueReturnCode_e values if failed.
 *
 * @ingroup FrameManagement
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilFrameSetBackgroundColor(ColorIndex_t color);

/**
 * Get the name of the current frame.
 *
 * @param Name
 *   Address of character string. Space for the new name will be allocated for you. You must later free
 *   the string by using TecUtilStringDealloc().
 *
 * @return
 *   Returns TRUE if the frame was successfully retrieved.
 *
 *
 * @pre <em>Name</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameGetName(
 *   &                   Name,
 *   &                   NameLength)
 *    CHARACTER*(*)   Name
 *    INTEGER*4       NameLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the name of the current frame:
 *
 * @code
 *   char *name = NULL;
 *   if ( TecUtilFrameGetName(&name) )
 *     {
 *       // do something
 *       TecUtilStringDealloc(&name);
 *     }
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameGetName(TP_GIVES char** Name);
/**
 *   Set the frame style defaults used by Tecplot when creating new frames. During normal operation,
 *   Tecplot bases the style of a new frame on the factory defaults plus any changes assigned in the
 *   Tecplot configuration file. Layout files and stylesheet files, however, rely on Tecplot basing
 *   new frames only on the factory defaults. This command is typically not used by the casual user.
 *
 * @param StyleBase
 *   Style base used by Tecplot for all future frames. This may be set to StyleBase_Factory or
 *   StyleBase_Config
 *
 * @return
 *   TRUE if successful, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStyleSetBase(StyleBase)
 *    INTEGER*4 StyleBase
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Instruct Tecplot to create all future frames based upon the factory defaults, ignoring any
 *   modifications specified in the user's tecplot.cfg file.
 *
 * @code
 *   TecUtilStyleSetBase(StyleBase_Factory);
 * @endcode
 *
 * @ingroup StyleValue
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStyleSetBase(StyleBase_e StyleBase);


/**
 * @deprecated
 *   Please use TecUtilDataSetDeleteZone() instead.
 *
 * @ingroup DataServices
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneDelete(Set_pa ZoneList);


/**
 * Deletes the specified set of zones.
 *
 * @param ZoneList
 *   Set of zones to delete.
 *
 * @return
 *   Returns TRUE if zones were deleted, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetDeleteZone(ZoneListPtr)
 *    POINTER (ZoneListPtr, ZoneList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Delete zones 1,3, and 9
 * @code
 *
 *   Set_pa ZoneList;
 *
 *   Set_pa ZoneList = TecUtilSetAlloc(FALSE);
 *   if (ZoneList)
 *     {
 *       TecUtilSetAddMember(ZoneList,1,FALSE);
 *       TecUtilSetAddMember(ZoneList,3,FALSE);
 *       TecUtilSetAddMember(ZoneList,9,FALSE);
 *       if (TecUtilDataSetDeleteZone(ZoneList))
 *         {
 *           ... zones deleted successfully.   Take approp. action.
 *         }
 *       TecUtilSetDealloc(&ZoneList);
 *     }
 * @endcode
 *
 * @ingroup DataServices
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetDeleteZone(Set_pa ZoneList);






/**
 *   Deletes the specified set of variables.
 *
 * @param VarList
 *   Set of variables to delete.
 *
 * @return
 *   Returns TRUE if variables were deleted, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetDeleteVar(VarListPtr)
 *    POINTER (VarListPtr, VarList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Delete variables 1,3, and 9
 * @code
 *
 *   Set_pa VarList;
 *
 *   Set_pa VarList = TecUtilSetAlloc(FALSE);
 *   if (VarList)
 *     {
 *       TecUtilSetAddMember(VarList,1,FALSE);
 *       TecUtilSetAddMember(VarList,3,FALSE);
 *       TecUtilSetAddMember(VarList,9,FALSE);
 *       if (TecUtilDataSetDeleteVar(VarList))
 *         {
 *           ... variables deleted successfully.   Take approp. action.
 *         }
 *       TecUtilSetDealloc(&VarList);
 *     }
 * @endcode
 *
 * @ingroup DataServices
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetDeleteVar(Set_pa VarList);






/**
 * Read one or more data files into Tecplot to form a new data set in the
 * current frame.
 *
 * @param ReadDataOption
 *   Determine how to handle the situation where a data set already exists in
 *   the current frame. The possible values are: ReadDataOption_NewData (Remove
 *   the data set from the current frame before loading the new data set),
 *   ReadDataOption_AppendData (Append the new data to the current data set),
 *   and ReadDataOption_ReplaceData (Replace the data set in the current frame
 *   and in all frames which share the data set with the new data).
 *
 * @param ResetStyle
 *   TRUE if you want to reset the style of the current frame, FALSE if you
 *   want to keep the same style. Only used if ReadDataOption is
 *   ReadDataOption_NewData or ReadDataOption_ReplaceData.FileNamesOr
 *
 * @param FileNamesOrInstructions
 *   A string list containing the file names to load or the instructions to
 *   send to the data set reader (converter or loader)
 *
 * @param DataSetReader
 *   Name of the data set reader (converter or loader). To let Tecplot load the
 *   data, use "TECPLOT."
 *
 * @param InitialPlotType
 *   Initial PlotType for the data. Only used if ResetStyle is TRUE. To have
 *   Tecplot determine the most appropriate frame mode for the data, use
 *   Frame_Empty. The possible values are: PlotType_Automatic:
 *   PlotType_Cartesian3D, PlotType_Cartesian2D, PlotType_XYLine,
 *   PlotType_PolarLine, and PlotType_Sketch. PlotType_Automatic instructs
 *   Tecplot to choose the best frame mode.
 *
 * @param IncludeText
 *   Set to TRUE to load any text in the data files.
 *
 * @param IncludeGeom
 *   Set to TRUE to load any geometries in the data files.
 *
 * @param IncludeCustomLabels
 *   Set to TRUE to load any custom labels in the data files
 *
 * @param IncludeData
 *   Set to TRUE to load the data from the data files. Set to FALSE to only
 *   load text, geometries and/or custom labels, depending on IncludeText,
 *   IncludeGeom, and IncludeCustomLabels
 *
 * @param CollapseZonesAndVars
 *   Set to TRUE to renumber zones and variables if any are disabled. For more
 *   information on collapsing zones and variables, see Section 5.1.1.7, "Zone
 *   and Variable List Collapsing," in the Tecplot User's Manual
 *
 * @param ZonesToRead
 *   Set of zones to load from the data files. Use NULL to load all zones
 *
 * @param VarLoadMode
 *   Choose to load variables by name or by their position in the data file.
 *   See Section 5.1.1.6, "Variable Loading by Position," in the Tecplot User's
 *   Manual for more information about loading variables by name or by
 *   position. If ReadDataOption is ReadDataOption_AppendData, this must be
 *   same as the mode of the data set in the current frame. Use
 *   TecUtilDataSetGetVarLoadMode() to get this information. The possible
 *   values are: VarLoadMode_ByName and VarLoadMode_ByPosition.
 *
 * @param VarPositionList
 *   Set of variables to load from the data files. Use NULL to load all
 *   variables. Ignored if VarLoadMode is VarLoadMode_ByName
 *
 * @param VarNameList
 *   Set of variable names to load from the data files. Use NULL to load only
 *   variable names common to all data files. Must be NULL if VarLoadMode is
 *   VarLoadMode_ByPosition. When appending to the existing data set you must
 *   supply a new VarNameList where the new VarNameList is a superset of the
 *   existing one. A VarNameList that is a superset is one that contains all
 *   the variable names currently in use Tecplot. They must be in the same
 *   position. You can add new names either at the end of the list, or as
 *   aliases in the already established positions. Use a newline character to
 *   separate aliased names.If you do not create a VarNameList that is a
 *   superset then it is indeterminant which variable is in which position for
 *   the original data
 *
 * @param ISkip
 *   Set to 1 to load every data point in the I-direction; 2 to load every
 *   other data point, and so forth.
 *
 * @param JSkip
 *   Same as ISkip but for J-direction.
 *
 * @param KSkip
 *   Same as ISkip but for K-direction.
 *
 * @return
 *   TRUE if the input parameters are valid and the data was successfully
 *   loaded, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>DataSetReader</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>ZonesToRead</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>VarPositionList</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilReadDataSet(
 *   &                   ReadDataOption,
 *   &                   ResetStyle,
 *   &                   FileNamesOrInstructionsPtr,
 *   &                   DataSetReader,
 *   &                   InitialPlotType,
 *   &                   IncludeText,
 *   &                   IncludeGeom,
 *   &                   IncludeCustomLabels,
 *   &                   IncludeData,
 *   &                   CollapseZonesAndVars,
 *   &                   ZonesToReadPtr,
 *   &                   VarLoadMode,
 *   &                   VarPositionListPtr,
 *   &                   VarNameListPtr,
 *   &                   ISkip,
 *   &                   JSkip,
 *   &                   KSkip)
 *    INTEGER*4       ReadDataOption
 *    INTEGER*4       ResetStyle
 *    POINTER         (FileNamesOrInstructionsPtr, FileNamesOrInstructions)
 *    CHARACTER*(*)   DataSetReader
 *    INTEGER*4       InitialPlotType
 *    INTEGER*4       IncludeText
 *    INTEGER*4       IncludeGeom
 *    INTEGER*4       IncludeCustomLabels
 *    INTEGER*4       IncludeData
 *    INTEGER*4       CollapseZonesAndVars
 *    POINTER         (ZonesToReadPtr, ZonesToRead)
 *    INTEGER*4       VarLoadMode
 *    POINTER         (VarPositionListPtr, VarPositionList)
 *    POINTER         (VarNameListPtr, VarNameList)
 *    INTEGER*4       ISkip
 *    INTEGER*4       JSkip
 *    INTEGER*4       KSkip
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Good: Var names currently in Tecplot = "A" "B" "C"
 *   VarNameList is appended to read = "A" "B\nR" "C" "D"
 *
 *   Bad: Var names currently in Tecplot = "A" "B" "C"
 *   VarNameList is appended to read "A" "C" "B" "D"
 *
 *   In the good example the integrity of the original data is always maintained, that is,
 *   the new VarNameList works for the original data as well as the new data.  Variable 2 can
 *   be either "B" or "R".
 *
 *   In the bad example, the new VarNameList states that variable 2 must contain "C"
 *   but "C" has already been assigned to variable 3 with the original data.
 *   Keep in mind that once variables are in Tecplot all style assignements are made based on
 *   variable position and not by name. Thus the variable positioning is important.
 *
 * @code
 *   StringList_pa FileNames, VarNames;
 *   FileNames = TecUtilStringListAlloc();
 *   TecUtilStringListAppendString(FileNames, "file1.plt");
 *   TecUtilStringListAppendString(FileNames, "file2.plt");
 *   VarNames  = TecUtilStringListAlloc();
 *   TecUtilStringListAppendString(VarNames,  "X");
 *   TecUtilStringListAppendString(VarNames,  "Y");
 *   TecUtilStringListAppendString(VarNames,  "P\nPress");
 *   IsOk = TecUtilReadDataSet(ReadDataOption_NewData,
 *                             TRUE,
 *                             FileNames,
 *                             "TECPLOT",
 *                             PlotType_Automatic,
 *                             TRUE, TRUE, TRUE, TRUE,
 *                             FALSE,
 *                             (Set_pa)NULL,
 *                             VarLoadMode_ByName,
 *                             (Set_pa)NULL,
 *                             VarNames,
 *                             1, 1, 1))
 *   TecUtilStringListDealloc(&FileNames);
 *   TecUtilStringListDealloc(&VarNames);
 * @endcode
 *
 *   Read a data set consisting of file1.plt and file2.plt. Load the variables named "X", "Y", and
 *   either "P" or "Press."
 *
 * @code
 *   StringList_pa FileNames, VarNames;
 *   FileNames = TecUtilStringListAlloc();
 *   TecUtilStringListAppendString(FileNames, "file1.plt");
 *   TecUtilStringListAppendString(FileNames, "file2.plt");
 *   VarNames  = TecUtilStringListAlloc();
 *   TecUtilStringListAppendString(VarNames,  "X");
 *   TecUtilStringListAppendString(VarNames,  "Y");
 *   TecUtilStringListAppendString(VarNames,  "P\nPress");
 *   IsOk = TecUtilReadDataSet(ReadDataOption_NewData,
 *                             TRUE,
 *                             FileNames,
 *                             "TECPLOT",
 *                             PlotType_Automatic,
 *                             TRUE, TRUE, TRUE, TRUE,
 *                             FALSE,
 *                             (Set_pa)NULL,
 *                             VarLoadMode_ByName,
 *                             (Set_pa)NULL,
 *                             VarNames,
 *                             1, 1, 1))
 *   TecUtilStringListDealloc(&FileNames);
 *   TecUtilStringListDealloc(&VarNames);
 * @endcode
 *
 * @ingroup DataFileSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilReadDataSet(ReadDataOption_e  ReadDataOption,
                                                 Boolean_t         ResetStyle,
                                                 StringList_pa     FileNamesOrInstructions,
                                                 const char       *DataSetReader,
                                                 PlotType_e        InitialPlotType,
                                                 Boolean_t         IncludeText,
                                                 Boolean_t         IncludeGeom,
                                                 Boolean_t         IncludeCustomLabels,
                                                 Boolean_t         IncludeData,
                                                 Boolean_t         CollapseZonesAndVars,
                                                 Set_pa            ZonesToRead,
                                                 VarLoadMode_e     VarLoadMode,
                                                 Set_pa            VarPositionList,
                                                 StringList_pa     VarNameList,
                                                 LgIndex_t         ISkip,
                                                 LgIndex_t         JSkip,
                                                 LgIndex_t         KSkip);
/**
 *   Read one or more data files into Tecplot to form a new data set in the current frame.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_READDATAOPTION
 * Type:
 *   ReadDataOption_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   ReadDataOption_NewData
 * Required:
 *   No
 * Notes:
 *   Determine how to handle the situation where a data set already exists in the current frame.
 *   The possible values are: ReadDataOption_NewData, ReadDataOption_AppendData and
 *   ReadDataOption_ReplaceData.
 *
 * Name:
 *   SV_RESETSTYLE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   TRUE if you want to reset the style of the current frame, FALSE if you want to keep the same style.
 *
 * Name:
 *   SV_FILENAMESORINSTRUCTIONS
 * Type:
 *   StringList_pa
 * Arg Function:
 *   TecUtilArgListAppendStringList()
 * Required:
 *   Yes
 * Notes:
 *   A string list containing the file names to load or the instructions to send to the data set reader (converter or loader).
 *
 * Name:
 *   SV_DATASETREADER
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   "TECPLOT"
 * Required:
 *   No
 * Notes:
 *   Name of the data set reader (converter or loader). To let Tecplot load the data, use NULL.
 *
 * Name:
 *   SV_INITIALPLOTFIRSTZONEONLY
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Informs Tecplot that after the data is loaded it only needs to activate the first
 *   enabled zone for the initial plot. This option is particularly usefull if you have many
 *   zones and want to get the data into Tecplot and the first zone drawn as fast as
 *   possible. The inactive zones can always be activated when needed.
 *
 * Name:
 *   SV_INITIALPLOTTYPE
 * Type:
 *   PlotType_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   PlotType_Automatic
 * Required:
 *   No
 * Notes:
 *   Initial frame plot type for the data. Only used if SV_RESETSTYLE is TRUE. To have
 *   Tecplot determine the most appropriate frame plot type for the data, use
 *   PlotType_Automatic (the default).
 *
 * Name:
 *   SV_INCLUDETEXT
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to load any text, geometries, or custom labels in the data files.
 *
 * Name:
 *   SV_INCLUDEGEOM
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 *
 * Name:
 *   SV_INCLUDECUSTOMLABL
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 *
 * Name:
 *   SV_INCLUDEDATA
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to load the data from the data files. Set to FALSE to only load text,
 *   geometries and/or customer labels, depending on SV_INCLUDETEXT, SV_INCLUDEGEOM,
 *   and SV_INCLUDECUSTOMLABELS
 *
 * Name:
 *   SV_COLLAPSEZONESANDVARS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to renumber zones and variables if any are disabled. For more information
 *   on collapsing zones and variables, see Section 5.1.1.7, "Zone and Variable List
 *   Collapsing," in the Tecplot User's Manual.
 *
 * Name:
 *   SV_ASSIGNSTRANDIDS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to request that Tecplot assign strand ID's to zones that hava
 *   a strand ID of -1.
 *
 * Name:
 *   SV_ADDZONESTOEXISTINGSTRANDS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to request that Tecplot add the zones to matching strands, if
 *   they exist. Otherwise, if the new data specifies strands, Tecplot will
 *   create new strands beginning after the last strand in the dataset.
 *
 * Name:
 *   SV_ZONELIST
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Set of zones to load from the data files. Use NULL to load all zones.
 *
 * Name:
 *   SV_VARLOADMODE
 * Type:
 *   VarLoadMode_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   VarLoadMode_ByPosition
 * Required:
 *   No
 * Notes:
 *   Choose to load variables by name or by their position in the data file.
 *
 * Name:
 *   SV_VARPOSITIONLIST
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Set of variables to load from the data files. Use NULL to load all
 *   variables. Must be NULL if SV_VARLOADMODE is VarLoadMode_ByName.
 *
 * Name:
 *   SV_VARNAMELIST
 * Type:
 *   StringList_pa
 * Arg Function:
 *   TecUtilArgListAppendStringList()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Set of variable names to load from the data files.
 *
 * Name:
 *   SV_ISKIP
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   The default value of 1 loads every data point in the I-, J-, or K-directions. A value
 *   of 2 for each loads every other data point and so forth. These values only apply
 *   to ordered dat
 *
 * Name:
 *   SV_JSKIP
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 *
 * Name:
 *   SV_KSKIP
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * </ArgListTable>
 *
 * @return
 *   TRUE if the input parameters are valid and the data was successfully loaded, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetReadX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Read in a the file newdata.plt.  Note that some arglist entries are shown
 *   here using their default values as an example only as they
 *   normally would not have to be provided.
 * @code
 *   StringList_pa Instructions = TecUtilStringListAlloc();
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *
 *   TecUtilStringListAppendString(Instructions, "newdata.plt");
 *
 *   TecUtilArgListAppendInt(ArgList,       SV_READDATAOPTION,  ReadDataOption_NewData);
 *   TecUtilArgListAppendInt(ArgList,       SV_RESETSTYLE,      TRUE);
 *   TecUtilArgListAppendStringList(ArgList,SV_FILENAMESORINSTRUCTIONS, Instructions);
 *   TecUtilArgListAppendInt(ArgList,       SV_INITIALPLOTFIRSTZONEONLY,FALSE);
 *   TecUtilArgListAppendInt(ArgList,       SV_INITIALPLOTTYPE,PlotType_Automatic);
 *   TecUtilArgListAppendInt(ArgList,       SV_INCLUDETEXT,      TRUE);
 *   TecUtilArgListAppendInt(ArgList,       SV_INCLUDEGEOM,     TRUE);
 *   TecUtilArgListAppendInt(ArgList,       SV_INCLUDECUSTOMLABELS,  TRUE);
 *   TecUtilArgListAppendInt(ArgList,       SV_INCLUDEDATA,     TRUE);
 *   TecUtilArgListAppendInt(ArgList,       SV_COLLAPSEZONESANDVARS,    FALSE);
 *   TecUtilArgListAppendInt(ArgList,       SV_VARLOADMODE,     VarLoadMode_ByPosition);
 *   TecUtilArgListAppendArray(ArgList,     SV_VARPOSITIONLIST, NULL);
 *   TecUtilArgListAppendInt(ArgList,       SV_ISKIP, 1);
 *   TecUtilArgListAppendInt(ArgList,       SV_JSKIP, 1);
 *   TecUtilArgListAppendInt(ArgList,       SV_KSKIP, 1);
 *
 *   TecUtilDataSetReadX(ArgList);
 *
 *   TecUtilStringListDealloc(&Instructions);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup DataLoad
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetReadX(ArgList_pa ArgList);

/**
 *   Write the specified components of the current frame's data set to a file.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   Name of the data file to write.
 *
 * Name:
 *   SV_INCLUDETEXT
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to write any text, geometries, or custom labels to the data files.
 *
 * Name:
 *   SV_INCLUDEGEOM
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 *
 * Name:
 *   SV_INCLUDEDATA
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to write the data to the data files. Set to FALSE to only write text and
 *   geometries, depending on SV_INCLUDETEXT and SV_INCLUDEGEOM
 *
 * Name:
 *   SV_INCLUDEDATASHARELINKAGE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to conserve space and write the variable and connectivity linkage to the
 *   data file wherever possible. Set to FALSE to write all the data to the data file and
 *   lose the variable and connectivity sharing linkage for future dataset reads of the
 *   file.
 *
 * Name:
 *   SV_INCLUDEAUTOGENFACENEIGHBORS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to save the face neighbor connectivity to the data file. This may produce
 *   very large data files.
 *
 * Name:
 *   SV_USEPOINTFORMAT
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to write the data file in point format and FALSE for block format. For
 *   binary files, only block format is supported, and this parameter will be ignored.
 *
 * Name:
 *   SV_BINARY
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to write the data in binary format and FALSE for ASCII.
 *
 * Name:
 *   SV_ASSOCIATELAYOUTWITHDATAFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to associate this data file with the current layout. Set to FALSE to
 *   write the datafile without modifying Tecplot's current data file to layout
 *   association. If SV_TECPLOTVERSIONTOWRITE is set to anything other than
 *   BinaryFileVersion_Current, this association is not possible, and this parameter
 *   will be ignored.
 *
 * Name:
 *   SV_ZONELIST
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Set of zones to write to the data file. Use NULL to write all zones.
 *
 * Name:
 *   SV_VARLIST
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Set of variables to write to the data file. Use NULL to write all variables.
 *
 * Name:
 *   SV_PRECISION
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   12
 * Required:
 *   No
 * Notes:
 *   Sets the ASCII decimal precision if the data file is ASCII format.
 *
 * Name:
 *   SV_TECPLOTVERSIONTOWRITE
 * Type:
 *   BinaryFileVersion_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   BinaryFileVersion_Current
 * Required:
 *   No
 * Notes:
 *   Specifies the file version to write. This is only applicable to binary files; for
 *   ASCII files, the parameter is ignored. Note that some data may be excluded from the
 *   file if it cannot be supported in the specified version.
 * </ArgListTable>
 *
 * @return
 *   TRUE if the input parameters are valid and the data was successfully written, FALSE
 *   otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetWriteX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataFileSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetWriteX(ArgList_pa ArgList);


/**
 *   Write the data set attached to the current frame to a file.
 *
 * @param FName
 *   File name. Must not be NULL
 *
 * @param IncludeText
 *   Set to TRUE to include text.
 *
 * @param IncludeGeom
 *   Set to TRUE to include geometries
 *
 * @param IncludeCustomLabels
 *   Set to TRUE to include custom labels
 *
 * @param IncludeData
 *   Set to TRUE to include data
 *
 * @param ZonesToWrite
 *   Set of zones to write. Pass NULL to write all zones
 *
 * @param VarsToWrite
 *   Set of vars to write. Pass NULL to write all variables
 *
 * @param WriteBinary
 *   Set to TRUE to write a binary file, FALSE to write an ASCII file
 *
 * @param UsePointFormat
 *   Valid only if WriteBinary is FALSE, ignored otherwise
 *
 * @param AsciiPrecision
 *   Valid only if WriteBinary is FALSE, ignored otherwise
 *
 *
 * @pre <em>FName</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>ZonesToWrite</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>VarsToWrite</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWriteDataSet(
 *   &                   FName,
 *   &                   IncludeText,
 *   &                   IncludeGeom,
 *   &                   IncludeCustomLabels,
 *   &                   IncludeData,
 *   &                   ZonesToWritePtr,
 *   &                   VarsToWritePtr,
 *   &                   WriteBinary,
 *   &                   UsePointFormat,
 *   &                   AsciiPrecision)
 *    CHARACTER*(*)   FName
 *    INTEGER*4       IncludeText
 *    INTEGER*4       IncludeGeom
 *    INTEGER*4       IncludeCustomLabels
 *    INTEGER*4       IncludeData
 *    POINTER         (ZonesToWritePtr, ZonesToWrite)
 *    POINTER         (VarsToWritePtr, VarsToWrite)
 *    INTEGER*4       WriteBinary
 *    INTEGER*4       UsePointFormat
 *    INTEGER*4       AsciiPrecision
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Write out only zone 3 to a file called zone3.plt:
 *
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set,3,FALSE);
 *   TecUtilWriteDataSet("zone3.plt",FALSE,FALSE,FALSE,TRUE,set,NULL,
 *                       TRUE,FALSE,0);
 *   TecUtilSetDealloc(&set);
 * @endcode
 *
 * @ingroup DataFileSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilWriteDataSet(const char  *FName,
                                                  Boolean_t    IncludeText,
                                                  Boolean_t    IncludeGeom,
                                                  Boolean_t    IncludeCustomLabels,
                                                  Boolean_t    IncludeData,
                                                  Set_pa       ZonesToWrite,
                                                  Set_pa       VarsToWrite,
                                                  Boolean_t    WriteBinary,
                                                  Boolean_t    UsePointFormat,
                                                  SmInteger_t  AsciiPrecision);
/**
 *   Create a rectangular zone. If no data set exists when this command is executed, a data set is
 *   created with variables X, Y (and Z, if KMAX>1). If a data set exists prior to this command, the
 *   non-coordinate variables for the zone created are initialized to zero.
 *
 * @param IMax
 *   I-Dimension of the zone to create.
 *
 * @param JMax
 *   J-Dimension of the zone to create.
 *
 * @param KMax
 *   K-Dimension of the zone to create.
 *
 * @param XMin
 *   X min (occurs at I = 1) for the recangular zone.
 *
 * @param YMin
 *   Y min (occurs at J = 1) for the recangular zone.
 *
 * @param ZMin
 *   Z min (occurs at K = 1) for the recangular zone.
 *
 * @param XMax
 *   X max (occurs at I = I-Max) for the recangular zone.
 *
 * @param YMax
 *   Y max (occurs at J = J-Max) for the recangular zone.
 *
 * @param ZMax
 *   Z max (occurs at K = K-Max) for the recangular zone.
 *
 * @param FieldDataType
 *   Data type for the variables in the new zone. The possible choices are: FieldDataType_Float,
 *   FieldDataType_Double, FieldDataType_Int32, FieldDataType_Int16, FieldDataType_Bit,
 *   FieldDataType_Byte, or FieldDataType_Invalid.  If set to FieldDataType_Invalid, Tecplot
 *   will choose the type for you.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateRectangularZone(
 *   &                   IMax,
 *   &                   JMax,
 *   &                   KMax,
 *   &                   XMin,
 *   &                   YMin,
 *   &                   ZMin,
 *   &                   XMax,
 *   &                   YMax,
 *   &                   ZMax,
 *   &                   FieldDataType)
 *    INTEGER*4       IMax
 *    INTEGER*4       JMax
 *    INTEGER*4       KMax
 *    REAL*8          XMin
 *    REAL*8          YMin
 *    REAL*8          ZMin
 *    REAL*8          XMax
 *    REAL*8          YMax
 *    REAL*8          ZMax
 *    INTEGER*4       FieldDataType
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a 16 by 8 by 2 zone that is specified from zero to one in all directions:
 *
 * @code
 *   Boolean_t ISOK;
 *   ISOK = TecUtilCreateRectangularZone(16,  // imax
 *                                     8,     // jmax
 *                                     2,     // kmax
 *                                     0,0,0, // x,y,z min
 *                                     1,1,1, // x,y,z max
 *                                     FieldDataType_Float);
 * @endcode
 *
 *   FORTRAN EXAMPLE:  Create 10x10x10 zone:
 *
 * @code
 *   IErr = TecUtilCreateRectangularZone(10,10,10,
 *  &                                    0.0D0,0.0D0,0.0D0,
 *  &                                    1.0D0,1.0D0,1.0D0,
 *  &
 * @endcode
 *
 * @ingroup DataCreate
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateRectangularZone(LgIndex_t       IMax,
                                                           LgIndex_t       JMax,
                                                           LgIndex_t       KMax,
                                                           double          XMin,
                                                           double          YMin,
                                                           double          ZMin,
                                                           double          XMax,
                                                           double          YMax,
                                                           double          ZMax,
                                                           FieldDataType_e FieldDataType);
/**
 *   Create a circular (or cylindrical) IJ- or IJK-ordered zone. If no data set exists when this
 *   command is executed, a data set is created with variables X, Y (and Z, if KMAX>1). If a data set
 *   exists prior to this command, the non-coordinate variables for the zone created are initialized
 *   to zero.
 *
 * @param IMax
 *   I-Dimension of the zone to create. I is in the radial direction.
 *
 * @param JMax
 *   J-Dimension of the zone to create. J is in the circumferential direction.
 *
 * @param KMax
 *   K-Dimension of the zone to create. K is in the Z-direction. Set K to 1 to create a circle
 *
 * @param XOrigin
 *   X-Origin for the circle or cylinder.
 *
 * @param YOrigin
 *   Y-Origin for the circle or cylinder.
 *
 * @param Radius
 *   Radius of the circle or cylinder
 *
 * @param ZMin
 *   Z-Min value used when creating a cylinder
 *
 * @param ZMax
 *   Z-Max value used when creating a cylinder
 *
 * @param FieldDataType
 *   Data type for the variables in the new zone. The possible choices are: FieldDataType_Float,
 *   FieldDataType_Double, FieldDataType_Int32, FieldDataType_Int16, FieldDataType_Bit,
 *   FieldDataType_Byte, or FieldDataType_Invalid.  If set to FieldDataType_Invalid, Tecplot
 *   will choose the type for you.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateCircularZone(
 *   &                   IMax,
 *   &                   JMax,
 *   &                   KMax,
 *   &                   XOrigin,
 *   &                   YOrigin,
 *   &                   Radius,
 *   &                   ZMin,
 *   &                   ZMax,
 *   &                   FieldDataType)
 *    INTEGER*4       IMax
 *    INTEGER*4       JMax
 *    INTEGER*4       KMax
 *    REAL*8          XOrigin
 *    REAL*8          YOrigin
 *    REAL*8          Radius
 *    REAL*8          ZMin
 *    REAL*8          ZMax
 *    INTEGER*4       FieldDataType
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a circular 10 by 20 IJ-ordered zone centered at (5, 5) with a radius of two:
 *
 * @code
 *   Boolean_t IsOk;
 *
 *   IsOk = TecUtilCreateCircularZone(10,20,1,
 *                                    5.0,5.0,2.0,
 *                                    0.0,0.0,
 *                                    FieldDataType_Float);
 *
 * @endcode
 *
 *   Create a cylindrical 5 by 6 by 8 IJK-ordered zone with the bottom centered at (4, 4, 0) and the
 *   top centered at (4, 4, 7) and a radius of three:
 *
 * @code
 *
 *   IsOk = TecUtilCreateCircularZone(5,6,8,
 *                                    4.0,4.0,3.0,
 *                                    0.0,7.0,
 *                                    FieldDataType_Float);
 * @endcode
 *
 * @ingroup DataCreate
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateCircularZone(LgIndex_t       IMax,
                                                        LgIndex_t       JMax,
                                                        LgIndex_t       KMax,
                                                        double          XOrigin,
                                                        double          YOrigin,
                                                        double          Radius,
                                                        double          ZMin,
                                                        double          ZMax,
                                                        FieldDataType_e FieldDataType);
/**
 * Create a Spherical IJ-ordered zone. If no data set exists when this command
 * is executed, a data set is created with variables X, Y and Z. If a data set
 * exists prior to this command, the non-coordinate variables for the zone
 * created are initialized to zero.
 *
 * @since
 *   11.2-0-500
 *
 * @param IMax
 *   I-Dimension of the zone to create. I is in the psi direction, where psi
     represents the angle from the Z-axis.
 *
 * @param JMax
 *   J-Dimension of the zone to create. J is in the theta direction, where
     theta represents the angle in the XY-plane.
 *
 * @param XOrigin
 *   X-Origin for the sphere.
 *
 * @param YOrigin
 *   Y-Origin for the sphere.
 *
 * @param ZOrigin
 *   Y-Origin for the sphere.
 *
 * @param Radius
 *   Radius of the sphere.
 *
 * @param FieldDataType
 *   Data type for the variables in the new zone. The possible choices are:
 *   FieldDataType_Float, FieldDataType_Double, FieldDataType_Int32,
 *   FieldDataType_Int16, FieldDataType_Bit, FieldDataType_Byte, or
 *   FieldDataType_Invalid.  If set to FieldDataType_Invalid, Tecplot will
 *   choose the type for you.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateSphericalZone(
 *   &                   IMax,
 *   &                   JMax,
 *   &                   XOrigin,
 *   &                   YOrigin,
 *   &                   ZOrigin,
 *   &                   Radius,
 *   &                   FieldDataType)
 *    INTEGER*4       IMax
 *    INTEGER*4       JMax
 *    REAL*8          XOrigin
 *    REAL*8          YOrigin
 *    REAL*8          ZOrigin
 *    REAL*8          Radius
 *    INTEGER*4       FieldDataType
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Create a Spherical 10 by 20 IJ-ordered zone centered at (5,6,7) with a
 * radius of two:
 *
 * @code
 *   Boolean_t IsOk;
 *
 *   IsOk = TecUtilCreateSphericalZone(10,20,
 *                                     5.0,6.0,7.0,
 *                                     2.0,
 *                                     FieldDataType_Float);
 *
 * @endcode
 *
 * @ingroup DataCreate
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateSphericalZone(LgIndex_t       IMax,
                                                         LgIndex_t       JMax,
                                                         double          XOrigin,
                                                         double          YOrigin,
                                                         double          ZOrigin,
                                                         double          Radius,
                                                         FieldDataType_e FieldDataType);
/**
 * @deprecated
 *   Please use TecUtilCreateSimpleZone() instead.
 *
 * @ingroup DataCreate
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateSimpleXYZone(LgIndex_t        NumPoints,
                                                        const double    *XValues,
                                                        const double    *YValues,
                                                        FieldDataType_e  FieldDataType);
/**
 *   Create a new zone by specifying only a list of XY pairs of data. If other zones exist prior to
 *   using this function and there are more than two variables, then the additional variables are also
 *   created and set to zero.
 *
 * @param NumPoints
 *   Number of XY pairs of data.
 *
 * @param V1Values
 *   Array of X (or theta) values for the zone.
 *
 * @param V2Values
 *   Array of Y (or R) values for the zone.
 *
 * @param FieldDataType
 *   Data type for the variables in the new zone. The possible choices are: FieldDataType_Float,
 *   FieldDataType_Double, FieldDataType_Int32, FieldDataType_Int16, FieldDataType_Bit,
 *   FieldDataType_Byte, or FieldDataType_Invalid.  If set to FieldDataType_Invalid, Tecplot
 *   will choose the type for you.
 *
 * @return
 *   TRUE if the zone could be created, FALSE if not.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>V1Values</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>V2Values</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateSimpleZone(
 *   &                   NumPoints,
 *   &                   V1Values,
 *   &                   V2Values,
 *   &                   FieldDataType)
 *    INTEGER*4       NumPoints
 *    REAL*8          V1Values
 *    REAL*8          V2Values
 *    INTEGER*4       FieldDataType
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a zone containing three points: (0.0, 1.0), (1.0, 1.1), and (2.0, 1.3):
 *
 * @code
 *   double xarray[3] = {0.0, 1.0, 2.0};
 *   double yarray[3] = {1.0, 1.1, 1.3);
 *   Boolean_t IsOk;
 *   IsOk = TecUtilCreateSimpleZone(3, xarray, yarray,
 *                                  FieldDataType_Float);
 * @endcode
 *
 * @ingroup DataCreate
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateSimpleZone(LgIndex_t        NumPoints,
                                                      const double    *V1Values,
                                                      const double    *V2Values,
                                                      FieldDataType_e  FieldDataType);

/**
 * Get the base path of a file name. This function allocates space for and
 * creates a new string of the base path.
 *
 * @param FName
 *   File name. A NULL file name is allowed in which case the startup directory
 *   is returned
 *
 * @return
 *   The base path of the file name including the trailing '/' or '\.' You must
 *   call TecUtilStringDealloc() on the returned string.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGetBasePath(
 *   &           FName,
 *   &           Result,
 *   &           ResultLength)
 *    CHARACTER*(*)   FName
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the start up directory and the base path of the current layout file name:
 *
 * @code
 *   char *FName = TecUtilGetCurLayoutFName();
 *   if (FName)
 *     {
 *       char *StartUpDir = TecUtilGetBasePath(NULL);
 *       char *BasePath = TecUtilGetBasePath(FName);
 *       //  BasePath is something like "C:\TEC75\DEMO\LAY\"
 *       if (BasePath)
 *         {
 *           .
 *           .
 *             TecUtilStringDealloc(&BasePath);
 *         }
 *       TecUtilStringDealloc(&FName);
 *       if (StartUpDir)
 *         TecUtilStringDealloc(&StartUpDir);
 *     }
 * @endcode
 *
 * @ingroup Utilities
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilGetBasePath(const char *FName);


/**
 *   Write out an html file and related image files. The resulting html file, when viewed will show the
 *   current tecplot session.
 *
 * @param FName
 *   Name of the publish file to create
 *
 * @param IncludeLayoutPackage
 *   If TRUE, write out a layout package file as well and include a link to the file in the HTML
 *   document
 *
 * @param ImageSelection
 *   Choose the image types to reference in the HTML document. Choose from one of
 *   ImageSelection_OnePerFrame or ImageSelection_WorkspaceOnly
 *
 * @return
 *   Returns TRUE if publish file could be written, FALSE otherwise.
 *
 *
 * @pre <em>FName</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPublish(
 *   &                   FName,
 *   &                   IncludeLayoutPackage,
 *   &                   ImageSelection)
 *    CHARACTER*(*)   FName
 *    INTEGER*4       IncludeLayoutPackage
 *    INTEGER*4       ImageSelection
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Write out a publish file to mydoc.htm. Include the package file and include a separate image per
 *   frame.
 *
 * @code
 *      TecUtilPublish("mydoc.htm",
 *                     TRUE,
 *                     ImageSelection_OnePerFrame);
 * @endcode
 *
 * @ingroup Export
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilPublish(const char      *FName,
                                             Boolean_t        IncludeLayoutPackage,
                                             ImageSelection_e ImageSelection);
/**
 *   Delete all frames and data sets (clear the current layout). A blank default frame will be created
 *   for you.
 *
 * @return
 *   Currently, TRUE is always returned.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilNewLayout()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Clear the current layout:
 *
 * @code
 *   Boolean_t IsOk = TecUtilNewLayout();
 * @endcode
 *
 * @ingroup LayoutSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilNewLayout(void);
/**
 * Open and read in a new layout file.
 *
 * @param FName
 *   The name of the layout file to open. This should be the full
 *   path to the layout file. If the full path is not specified,
 *   any relative paths in your layout may not be correctly resolved.
 *
 * @param AltInstructions
 *   Alternate instructions for the layout file. In most cases, this will be a string list containing
 *   file names which will override the data files listed in the layout file. Use NULL to load the
 *   data referenced in the layout file
 *
 * @param Append
 *   TRUE to append the new layout file to the current layout or FALSE to replace the current layout
 *
 * @return
 *   TRUE if successfull, FALSE otherwise.
 *
 *
 * @pre <em>FName</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilOpenLayout(
 *   &                   FName,
 *   &                   AltInstructionsPtr,
 *   &                   Append)
 *    CHARACTER*(*)   FName
 *    POINTER         (AltInstructionsPtr, AltInstructions)
 *    INTEGER*4       Append
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Open a layout file called "experiment.lay." Then, append a layout called "calculate.lay,"
 * overriding the first data file referenced in that layout with a file called "newdata.plt":
 *
 * @code
 *   StringList_pa AltInst = TecUtilStringListAlloc();
 *   TecUtilStringListAppendString(AltInst, "newdata.plt");
 *   TecUtilOpenLayout("C:\\experiment.lay", (StringList_pa)NULL, FALSE);
 *   TecUtilOpenLayout("C:\\calculate.lay", AltInst, TRUE);
 *   TecUtilStringListDealloc(&AltInst);
 * @endcode
 *
 * @ingroup LayoutSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilOpenLayout(const char*   FName,
                                                StringList_pa AltInstructions,
                                                Boolean_t     Append);
/**
 * Open and read in a new layout file.
 *
 * @since
 *   11.3.29.396
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   The name of the layout file to open. This should be the full path to the
 *   layout file. If the full path is not specified, any relative paths in your
 *   layout may not be correctly resolved. The file name reference need not be
 *   allocated however the string must not be deallocated until it is no longer
 *   referenced by the argument list.
 *
 * Name:
 *   SV_ALTINSTRUCTIONS
 * Type:
 *   StringList_pa
 * Arg Function:
 *   TecUtilArgListAppendStringList()
 * Required:
 *   No
 * Default:
 *   NULL
 * Notes:
 *   Alternate instructions for the layout file. In most cases, this will be a
 *   string list containing file names which will override the data files
 *   listed in the layout file. Use NULL to load the data referenced in the
 *   layout file.
 *
 * Name:
 *   SV_APPEND
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   TRUE to append the new layout file to the current layout or FALSE to
 *   replace the current layout.
 *
 * Name:
 *   SV_APPENDTOCURPAGE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   When appending a value of TRUE tells Tecplot to append the layout to the
 *   contents of the current page while a value of FALSE instructs Tecplot to
 *   append the layout to a new page.
 * </ArgListTable>
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSaveLayoutX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Open a layout file called "experiment.lay." Then, append a layout called "calculate.lay,"
 * overriding the first data file referenced in that layout with a file called "newdata.plt":
 *
 * @code
 *   TecUtilArgListClear(ArgList);
 *   TecUtilArgListAppendString(ArgList, SV_FNAME, "C:\\experiment.lay");
 *   TecUtilOpenLayoutX(ArgList);
 *
 *   StringList_pa AltInst = TecUtilStringListAlloc();
 *   TecUtilStringListAppendString(AltInst, "newdata.plt");
 *
 *   TecUtilArgListClear(ArgList);
 *   TecUtilArgListAppendString(ArgList,     SV_FNAME,           "C:\\calculate.lay");
 *   TecUtilArgListAppendStringList(ArgList, SV_ALTINSTRUCTIONS, AltInst);
 *   TecUtilArgListAppendInt(ArgList,        SV_APPEND,          TRUE);
 *   TecUtilOpenLayoutX(ArgList);
 *
 *   TecUtilStringListDealloc(&AltInst);
 *   ...
 * @endcode
 *
 * @ingroup LayoutSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilOpenLayoutX(ArgList_pa ArgList);

/**
 *   Save the current layout to a file. You must supply the file name.
 *
 * @param FName
 *   The name of the layout file to save
 *
 * @param UseRelativePaths
 *   Set to TRUE to make all of the files referenced by the layout file use paths relative to the
 *   current directory. Set to FALSE to make all of the files referenced by absolute paths
 *
 * @return
 *   TRUE if the current layout was saved, FALSE otherwise.
 *
 * @pre Must have one or more pages.
 *
 * @pre <em>FName</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSaveLayout(
 *   &                   FName,
 *   &                   UseRelativePaths)
 *    CHARACTER*(*)   FName
 *    INTEGER*4       UseRelativePaths
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Notes:
 *   Saving a layout requires that all the frame's journals are valid. If a journal is invalid
 *   you can write out the data (in its entirety) thus validating the journal for that dataset.
 *   You can verify the journal's integrity before calling this function as follows:
 * @code
 *   bool result = true;
 *   TecUtilFrameLightweightLoopStart();
 *   do
 *   {
 *       result = TecUtilDataSetJournalIsValid();
 *   }
 *   while (TecUtilFrameLightweightLoopNext() && result);
 *   TecUtilFrameLightweightLoopEnd();
 *
 *   if (result)
 *       TecUtilSaveLayout(fname,userelativepaths)
 * @endcode
 *
 *   Save a layout file called temp.lay, using absolute paths:
 *
 * @code
 *   Boolean_t IsOk = TecUtilSaveLayout("temp.lay", FALSE);
 * @endcode
 *
 * @ingroup LayoutSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilSaveLayout(const char *FName,
                                                Boolean_t   UseRelativePaths);
/**
 *   Save the current layout to a file.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   Associated value is the file name of the file in which to save the layout.
 *   The file name must be a reference to a non-NULL string who's length is
 *   greater than zero. The file name reference need not be allocated however
 *   the string must not be deallocated until it is no longer referenced by the
 *   argument list.
 *
 * Name:
 *   SV_INCLUDEDATA
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the layout should be saved as a layout
 *   package where the data is included with the style information or if it
 *   should reference linked data
 *
 * Name:
 *   SV_INCLUDEPREVIEW
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the layout package should also include a
 *   preview image. This argument only applies if the include data option is
 *   TRUE
 *
 * Name:
 *   SV_USERELATIVEPATHS
 * Type:
 *   Boolean_t
 * Arg Function:
 *  TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the layout should be saved using relative
 *   paths. This argument only applies if the include data option is FALSE.
 *
 * Name:
 *   SV_POSTLAYOUTCOMMANDS
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   A character string containing a set of Tecplot macro commands that are
 *   appended to the layout or layout package file.  These can be almost
 *   anything and are generally used to store add-on specific state information
 *   (using $!EXTENDEDCOMMAND commands).
 *
 * Name:
 *   SV_PAGELIST
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   If NULL all pages are written to the layout otherwise the specified subset
 *   of pages are written. The set contains the 1 based page positions relative
 *   to the current page where the current page has a position value of 1, the
 *   next page 2, the page after that 3, and so on.
 *   See example below.
 * </ArgListTable>
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more pages.
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSaveLayoutX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Notes:
 *   Saving a layout requires that all the frame's journals are valid. If a journal is invalid
 *   you can write out the data (in its entirety) thus validating the journal for that dataset.
 *   You can verify the journal's integrity before calling this function as follows:
 * @code
 *   bool result = true;
 *   TecUtilFrameLightweightLoopStart();
 *   do
 *   {
 *       result = TecUtilDataSetJournalIsValid();
 *   }
 *   while (TecUtilFrameLightweightLoopNext() && result);
 *   TecUtilFrameLightweightLoopEnd();
 *
 *   if (result)
 *       TecUtilSaveLayoutX(ArgList)
 * @endcode
 *
 * Save a layout package named temp.lpk, without a preview image:
 * @code
 *   TecUtilArgListClear(ArgList);
 *   TecUtilArgListAppendString(ArgList, SV_FNAME,   "temp.lpk");
 *   TecUtilArgListAppendInt(ArgList,    SV_INCLUDEDATA,    TRUE);
 *   TecUtilArgListAppendInt(ArgList,    SV_INCLUDEPREVIEW, FALSE);
 *   IsOk = TecUtilSaveLayoutX(ArgList);
 * @endcode
 *
 * Save a layout named temp.lay using relative path names:
 * @code
 *   TecUtilArgListClear(ArgList);
 *   TecUtilArgListAppendString(ArgList, SV_FNAME,  "temp.lay");
 *   TecUtilArgListAppendInt(ArgList,    SV_USERELATIVEPATHS, TRUE);
 *   IsOk = TecUtilSaveLayoutX(ArgList);
 * @endcode
 *
 * Assume that we have a layout consisting of half a dozen pages all identified
 * by their unique page IDs. Save a partial layout containing only a sub-set of
 * the layout's pages. In this example we assume My2DPlotPageID and My3DPlotPageID
 * where acquired by earlier calls to TecUtilPageGetUniqueID().
 * @code
 *   Set_pa PageList = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(PageList, TecUtilPageGetPosByUniqueID(My2DPlotPageID)), FALSE);
 *   TecUtilSetAddMember(PageList, TecUtilPageGetPosByUniqueID(My3DPlotPageID)), FALSE);
 *
 *   ArgList_pa ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendString(ArgList, SV_FNAME,  "partial.lay");
 *   TecUtilArgListAppendInt(ArgList,    SV_USERELATIVEPATHS, TRUE);
 *   TecUtilArgListAppendSet(ArgList,    SV_PAGELIST, PageList);
 *   IsOk = TecUtilSaveLayoutX(ArgList);
 *
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilSetDealloc(&PageList);
 * @endcode
 *
 * @ingroup LayoutSupport
 */
LINKTOADDON Boolean_t STDCALL TecUtilSaveLayoutX(ArgList_pa ArgList);

/**
 *   Read a stylesheet file into the current frame.
 *
 * @param FName
 *   The name of the stylesheet file to read
 *
 * @param IncludePlotStyle
 *   Set to TRUE to process commands related to the style of the plot (that is, mesh color, vector
 *   type, etc.)
 *
 * @param IncludeText
 *   Set to TRUE to load any text in the stylesheet
 *
 * @param IncludeGeom
 *   Set to TRUE to load any geometries in the stylesheet
 *
 * @param IncludeStreamPositions
 *   Set to TRUE to load any streamtrace starting positions in the stylesheet
 *
 * @param IncludeContourLevels
 *   Set to TRUE to load any contour level information in the stylesheet.
 *
 * @param MergeStyle
 *   Set to TRUE to merge the current frame style with the new stylesheet. Set to FALSE to reset the
 *   style of the current frame back to factory defaults before reading in the stylesheet
 *
 * @param IncludeFrameSizeAndPosition
 *   Set to TRUE to resize and position the current frame to the specifications of the stylesheet file.
 *   Set to FALSE to keep the current frame's size and position
 *
 * @return
 *   TRUE if the input parameters are valid and the specified style was successfully loaded, FALSE
 *   otherwise.
 *
 *
 * @pre <em>FName</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilReadStylesheet(
 *   &                   FName,
 *   &                   IncludePlotStyle,
 *   &                   IncludeText,
 *   &                   IncludeGeom,
 *   &                   IncludeStreamPositions,
 *   &                   IncludeContourLevels,
 *   &                   MergeStyle,
 *   &                   IncludeFrameSizeAndPosition)
 *    CHARACTER*(*)   FName
 *    INTEGER*4       IncludePlotStyle
 *    INTEGER*4       IncludeText
 *    INTEGER*4       IncludeGeom
 *    INTEGER*4       IncludeStreamPositions
 *    INTEGER*4       IncludeContourLevels
 *    INTEGER*4       MergeStyle
 *    INTEGER*4       IncludeFrameSizeAndPosition
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Load in the stylesheet file1.sty, choosing to reset the style of the current frame back to factory
 *   defaults first. Next, load only text and geometries from the stylesheet file2.sty, adding them to
 *   the current style:
 *
 * @code
 *   Boolean_t IsOk;
 *   IsOk = TecUtilReadStylesheet("file1.sty", TRUE, TRUE, TRUE,
 *                                TRUE, TRUE, FALSE, TRUE);
 *   IsOk = TecUtilReadStylesheet("file2.sty", FALSE, TRUE, TRUE,
 *                                FALSE, FALSE, TRUE, FALSE);
 * @endcode
 *
 * @ingroup Stylesheets
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilReadStylesheet(const char *FName,
                                                    Boolean_t   IncludePlotStyle,
                                                    Boolean_t   IncludeText,
                                                    Boolean_t   IncludeGeom,
                                                    Boolean_t   IncludeStreamPositions,
                                                    Boolean_t   IncludeContourLevels,
                                                    Boolean_t   MergeStyle,
                                                    Boolean_t   IncludeFrameSizeAndPosition);

/**
 *   Write the style for the current frame to a file.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *
 * Name:
 *   SV_INCLUDECONTOURLEVELS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to include contour levels
 *
 * Name:
 *   SV_INCLUDETEXT
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to include text
 *
 * Name:
 *   SV_INCLUDEAUXDATA
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to include auxiliary data
 *
 * Name:
 *   SV_INCLUDEGEOM
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to include geometries
 *
 * Name:
 *   SV_INCLUDEPLOTSTYLE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   --------------------------
 *
 * Name:
 *   SV_COMPRESS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to compress the stylesheet file
 *
 * Name:
 *   SV_INCLUDESTREAMPOSITIONS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to include stream positions
 *
 * Name:
 *   SV_INCLUDEFACTORYDEFAULTS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to include factory defaults
 *
 * Name:
 *   SV_USERELITIVEPATHS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to save with relative paths for any image geometries in the frame
 * </ArgListTable>
 *
 * @return
 *   TRUE if successfully, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWriteStylesheetX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Save a stylesheet "temp.sty" with no text objects included:
 *
 * @code
 *   TecUtilArgListClear(ArgList);
 *   TecUtilArgListAppendString(ArgList,SV_FNAME,"temp.sty");
 *   TecUtilArgListAppendString(ArgList,SV_INCLUDETEXT,FALSE);
 *   IsOk = TecUtilWriteStyleSheetX(ArgList);
 * @endcode
 *
 * @ingroup Stylesheets
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilWriteStylesheetX(ArgList_pa ArgList);


/**
 *   Write the style for the current frame to a file.
 *
 * @param FName
 *   File name. Must not be NULL
 *
 * @param IncludePlotStyle
 *   Set to TRUE to include the plot style
 *
 * @param IncludeText
 *   Set to TRUE to include text
 *
 * @param IncludeGeom
 *   Set to TRUE to include geometries
 *
 * @param IncludeStreamPositions
 *   Set to TRUE to include stream positions
 *
 * @param IncludeContourLevels
 *   Set to TRUE to include contour levels
 *
 * @param IncludeFactoryDefaults
 *   Set to TRUE to include factory defaults
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 *
 * @pre <em>FName</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWriteStylesheet(
 *   &                   FName,
 *   &                   IncludePlotStyle,
 *   &                   IncludeText,
 *   &                   IncludeGeom,
 *   &                   IncludeStreamPositions,
 *   &                   IncludeContourLevels,
 *   &                   IncludeFactoryDefaults)
 *    CHARACTER*(*)   FName
 *    INTEGER*4       IncludePlotStyle
 *    INTEGER*4       IncludeText
 *    INTEGER*4       IncludeGeom
 *    INTEGER*4       IncludeStreamPositions
 *    INTEGER*4       IncludeContourLevels
 *    INTEGER*4       IncludeFactoryDefaults
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Write the style for the current frame to the file f1.sty:
 *
 * @code
 *   TecUtilWriteStylesheet("f1.sty",TRUE,TRUE,TRUE,TRUE,TRUE,TRUE);.
 * @endcode
 *
 * @ingroup Stylesheets
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilWriteStylesheet(const char *FName,
                                                     Boolean_t   IncludePlotStyle,
                                                     Boolean_t   IncludeText,
                                                     Boolean_t   IncludeGeom,
                                                     Boolean_t   IncludeStreamPositions,
                                                     Boolean_t   IncludeContourLevels,
                                                     Boolean_t   IncludeFactoryDefaults);
/**
 *   Load a color map file.
 *
 * @param FName
 *   Name of the color map file to load
 *
 * @return
 *   Returns TRUE if the FName was successfully loaded as a color map, otherwise, FALSE.
 *
 *
 * @pre <em>FName</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilReadColorMap(FName)
 *    CHARACTER*(*) FName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Load the color map file temp.map.
 *
 * @code
 *   TecUtilReadColorMap("temp.map");
 * @endcode
 *
 * @ingroup ColorMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilReadColorMap(const char *FName);
/**
 *   Assign the RGB values that define the Raw User-Defined color map. This
 *   does not set the color map to use the Raw User-Defined color map. Use
 *   TecUtilColorMapSetBase() to assign the color map.
 *
 * @param NumRawRGBValues
 *   The number of RGB values to use to define the Raw User-Defined color map.
 *   This must be less than MaxRawColorMapEntries.
 *
 * @param RawRValues_Array
 *   An array of size NumRawRGBValues which contains the red values to define
 *   the Raw User-Defined color map.
 *
 * @param RawGValues_Array
 *   An array of size NumRawRGBValues which contains the green values to define
 *   the Raw UserDefined color map.
 *
 * @param RawBValues_Array
 *   An array of size NumRawRGBValues which contains the blue values to define
 *   the Raw User-Defined color map.
 *
 * @return
 *   TRUE if successful, FALSE if an invalid number of RGB values was requested.
 *
 *
 * @pre <em>VALID_REF(RawRValues)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(RawGValues)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(RawBValues)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilRawColorMap(
 *   &                   NumRawRGBValues,
 *   &                   RawRValues_Array,
 *   &                   RawGValues_Array,
 *   &                   RawBValues_Array)
 *    INTEGER*4       NumRawRGBValues
 *    INTEGER*4       RawRValues_Array
 *    INTEGER*4       RawGValues_Array
 *    INTEGER*4       RawBValues_Array
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Define the Raw User-Defined color map to be five RGB values: blue, cyan, green, yellow, and red
 *   and assign Tecplot to use the Raw User-Defined color map.
 *
 * @code
 *
 *   ColorIndex_t RValues[5];
 *   ColorIndex_t GValues[5]
 *   ColorIndex_t BValues[5];
 *
 *   RValues[0] = 0; GValues[0] = 0; BValues[0] = 255;
 *   RValues[1] = 0; GValues[1] = 255; BValues[1] = 255;
 *   RValues[2] = 0; GValues[2] = 255; BValues[2] = 0;
 *   RValues[3] = 255; GValues[3] = 255; BValues[3] = 0;
 *   RValues[4] = 255; GValues[4] = 0; BValues[4] = 0;
 *
 *   TecUtilRawColorMap(5, RValues, GValues, BValues);
 *   TecUtilColorMapSetBase(ContourColorMap_RawUserDef);
 * @endcode
 *
 * @ingroup ColorMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilRawColorMap(int           NumRawRGBValues,
                                                 ColorIndex_t *RawRValues_Array,
                                                 ColorIndex_t *RawGValues_Array,
                                                 ColorIndex_t *RawBValues_Array);
/**
 *   Write the current color map to a file.
 *
 * @param FName
 *   File name. Must not be NULL
 *
 * @return
 *   TRUE if the color map was successfully written, FALSE otherwise.
 *
 *
 * @pre <em>FName</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWriteColorMap(FName)
 *    CHARACTER*(*) FName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Write the current color map to a file. Mycolors.map is the file name:
 *
 * @code
 *   TecUtilWriteColorMap("mycolors.map");
 * @endcode
 *
 * @ingroup ColorMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilWriteColorMap(const char *FName);

#if defined MSWIN || defined DOCUMENT_BUT_DONT_COMPILE
/**
 * Copy the current plot to the Windows system clipboard.
 *
 * @since 11.2-0-384
 *
 * Call TecUtilExportSetup() first to specify the type and options of an image
 * that will be inserted into the clipboard.
 *
 * Supported export formats are @ref ExportFormat_WindowsMetafile and
 * @ref ExportFormat_BMP
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWinCopyToClipboard()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *   TecUtilExportSetup(SV_EXPORTFORMAT, NULL,
 *                      0.0, (ArbParam_t)ExportFormat_WindowsMetafile);
 *   TecUtilExportSetup(SV_EXPORTREGION, NULL,
 *                      0.0, (ArbParam_t)ExportRegion_AllFrames);
 *   TecUtilWinCopyToClipboard();
 *
 *   TecUtilExportSetup(SV_EXPORTFORMAT, NULL,
 *                      0.0, (ArbParam_t)ExportFormat_BMP);
 *   TecUtilExportSetup(SV_EXPORTREGION, NULL,
 *                      0.0, (ArbParam_t)ExportRegion_CurrentFrame);
 *   TecUtilExportSetup(SV_IMAGEWIDTH, NULL,
 *                      0.0, (ArbParam_t)640);
 *   TecUtilWinCopyToClipboard();
 * @endcode
 *
 * @ingroup Export
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilWinCopyToClipboard(void);
#endif /* defined MSWIN */

/**
 * Export an image file from Tecplot. See TecUtilExportSetup() for details on
 * settup the exported image type, file name and so forth.
 *
 * @param Append
 *   Append the image data. This can only be set to TRUE if the export format
 *   is a Raster Metafile or AVI. However, it is recommended that you use
 *   TecUtilExportStart(), TecUtilExportNextFrame(), and TecUtilExportFinish()
 *   for Raster Metafile and AVI formats. This value must be FALSE for all
 *   other formats.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilExport(Append)
 *    INTEGER*4 Append
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Export
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilExport(Boolean_t Append);

/**
 *   Change the view in the workspace so the currently selected frames (that
 *   is, the frames with picked handles) are fit just inside the edges of the
 *   workspace.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWorkViewFitSelectFrames()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup WorkArea
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilWorkViewFitSelectFrames(void);

/**
 *   Change the view in the workspace so all frames are fit just inside the edges of the workspace.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWorkViewFitAllFrames()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup WorkArea
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilWorkViewFitAllFrames(void);
/**
 *   Change the view in the workspace so the entire paper is fit just inside the edges of the
 *   workspace.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWorkViewFitPaper()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup WorkArea
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilWorkViewFitPaper(void);
/**
 *   Temporarily expand the workspace as large as possible. In the Motif version of Tecplot, the
 *   maximized workspace occupies the entire Tecplot process window. In the Windows version, the
 *   maximized workspace expands to fill the entire screen.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWorkViewMaximize()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup WorkArea
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilWorkViewMaximize(void);
/**
 *   Return to the previous workspace view.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWorkViewLastView()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup WorkArea
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilWorkViewLastView(void);
/**
 *   Change the view into the workspace. This has no effect on the local view within any frame in your
 *   layout.
 *
 * @param X1
 *   X min Corner of the rectangle on the paper to be viewed.
 *
 * @param Y1
 *   Y min Corner of the rectangle on the paper to be viewed.
 *
 * @param X2
 *   X max Corner of the rectangle on the paper to be viewed.
 *
 * @param Y2
 *   Y max Corner of the rectangle on the paper to be viewed.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWorkViewZoom(
 *   &                   X1,
 *   &                   Y1,
 *   &                   X2,
 *   &                   Y2)
 *    REAL*8          X1
 *    REAL*8          Y1
 *    REAL*8          X2
 *    REAL*8          Y2
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Make the region in the lower left corner of an 8.5 by 11 paper viewable in the work area. The
 *   paper is in portrait orientation:
 *
 * @code
 *   TecUtilWorkViewZoom(0.0,5.5,4.25,9.75);
 * @endcode
 *
 * @ingroup WorkArea
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilWorkViewZoom(double X1,
                                                  double Y1,
                                                  double X2,
                                                  double Y2);
/**
 *   Shift the view of the workspace. This has no effect on the local view within any frame in your
 *   layout.
 *
 * @param X
 *   Amount to translate in the X-Direction. Value is in inches.
 *
 * @param Y
 *   Amount to translate in the Y-Direction. Value is in inches.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWorkViewTranslate(
 *   &                   X,
 *   &                   Y)
 *    REAL*8          X
 *    REAL*8          Y
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Shift the workspace view to the left by two inches (as measured by the workspace ruler):
 *
 * @code
 *   TecUtilWorkViewTranslate(-2.0,0.0);
 * @endcode
 *
 * @ingroup WorkArea
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilWorkViewTranslate(double X,
                                                       double Y);

/**
 * Instruct Tecplot to push the current view onto the view stack associated
 * with the frame mode for the current frame. A view will not be pushed if the
 * current view is the same as the top view on the stack. Functions
 * TecUtilViewAxisFit(), TecUtilViewCenter(), TecUtilViewDataFit(),
 * TecUtilViewFit(), and TecUtilViewZoom() automatically push a view onto the
 * stack. Tecplot automatically pushes the current view onto the stack when a
 * $!Redraw command or TecUtilRedraw() is issued and the current view is
 * different from the top view on the view stack.
 *
 * @par Note:
 *   Views pushed with this command are only retrieved using TecUtilViewLast().
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewPush()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewPush(void);
/**
 *   Retrieve the view from the view paste buffer and assign it to the current frame.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewPaste()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewPaste(void);
/**
 * Copy the view from the current frame to the view paste buffer. This view can
 * later be pasted into any frame that uses the same frame mode as the current
 * frame.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewCopy()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilViewPaste()
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewCopy(void);
/**
 *   Retrieve the previous view from the view stack. Each frame mode within each frame maintains its
 *   own view stack.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewLast()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewLast(void);
/**
 *   Change the view by "zooming" into the image. Tecplot will adjust the ranges on the axis to view
 *   the region defined by the rectangle with corners at (X1, Y1) and (X2, Y2). X1, Y1, X2, and Y2 are
 *   defined in the units of the X- and Y-axis (that is, grid coordinates).
 *
 * @param X1
 *   X min Corner of the rectangle to be viewed.
 *
 * @param Y1
 *   Y min Corner of the rectangle to be viewed.
 *
 * @param X2
 *   X max Corner of the rectangle to be viewed.
 *
 * @param Y2
 *   Y max Corner of the rectangle to be viewed.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewZoom(
 *   &                   X1,
 *   &                   Y1,
 *   &                   X2,
 *   &                   Y2)
 *    REAL*8          X1
 *    REAL*8          Y1
 *    REAL*8          X2
 *    REAL*8          Y2
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Zoom so the rectangular region with corners at (1, 0) and (7, 9) are in view:
 *
 * @code
 *   TecUtilViewZoom(1.0,0.0,7.0,9.0);
 * @endcode
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewZoom(double X1,
                                              double Y1,
                                              double X2,
                                              double Y2);
/**
 *   Shift the image in the X- and/or Y-direction. The amount translated is in frame units.
 *
 * @param X
 *   Amount to shift the data in the X-direction.
 *
 * @param Y
 *   Amount to shift the data in the Y-direction.
 *
 * @return
 *   TRUE if successful, FALSE if not
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewTranslate(
 *   &                   X,
 *   &                   Y)
 *    REAL*8          X
 *    REAL*8          Y
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Translate the view ten percent of the frame width to the right:
 *
 * @code
 *   TecUtilViewTranslate(10.0,0.0);
 * @endcode
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewTranslate(double X,
                                                   double Y);

/**
 *   Center the data within the axis grid area.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewCenter()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewCenter(void);

/**
 * @deprecated
 *   Please use TecUtilViewSetMagnification() instead.
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewScale(double Scale);

/**
 *   Scale (magnify) the view by a given value. Set the magnification factor for the current view
 *   relative to a view where the data fit to full frame size.
 *
 * @param Magnification
 *   Set the magnification to this value.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewSetMagnification(Magnification)
 *    REAL*8 Magnification
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Scale the view to ten percent of the size at which the data would fit the full frame:
 *
 * @code
 *   TecUtilViewSetMagnification(0.10);
 * @endcode
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewSetMagnification(double Magnification);

/**
 *   Reset the range of a polar axis to fit the entire circle.
 *
 * @param Axis
 *   Valid values are 'T' or 'R'.
 *
 * @param AxisNum
 *   Should be always 1. For future use.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewAxisFitToEntireCircle(
 *   &                   Axis)
 *    CHARACTER*(*)   Axis
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Reset the range on the Y-axis to fit the data being plotted:
 *
 * @code
 *   TecUtilViewAxisFitToEntireCircle('T');
 * @endcode
 *
 * @since 11.3-13-007
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewAxisFitToEntireCircle(char Axis,
                                                               short AxisNum);

/**
 *   Reset the range on a specific axis so that it equals the minimum and maximum of the data being
 *   plotted. If the axis dependency is not independent then this action may also affect the range on
 *   another axis.
 *
 * @param Axis
 *   Valid values are 'X' or 'Y', 'Z', 'T' or 'R'.
 *
 * @param AxisNum
 *   If the frame mode is XY, then this can be a number between one and five. Otherwise, this must be
 *   set to one
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewAxisFit(
 *   &                   Axis,
 *   &                   AxisNum)
 *    CHARACTER*(*)   Axis
 *    INTEGER*4       AxisNum
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Reset the range on the Y-axis to fit the data being plotted:
 *
 * @code
 *   TecUtilViewAxisFit('Y',1);
 * @endcode
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewAxisFit(char Axis,
                                                 short AxisNum);

/**
 *   Fit the data being plotted within the axis grid area. This does not take into consideration text
 *   or geometries.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewDataFit()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewDataFit(void);

/**
 *   Resizes the plot so that all surfaces are included in the frame, excluding any volume zones.
 *   The Fit Surfaces operation is performed when your dataset is first displayed.
 * Notes:
 *   TecUtilViewDataFit and TecUtilViewFitSurfaces are only different in 3D plot type, for all other
 *   plot types this function behaves exactly as TecUtilViewDataFit.
 *
 * @since 14.2
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewFitSurfaces()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewFitSurfaces(void);

/**
 *   Fit the data being plotted within the axis grid area. This also takes into consideration text and
 *   geometries that are plotted using the grid coordinate system.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewFit()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewFit(void);

/**
 *   Fit the data being plotted within the axis grid area leaving a determined border around the edge
 *   of the plot to make in more visually appealing. This also takes into consideration text and
 *   geometries that are plotted using the grid coordinate system.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewNiceFit()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewNiceFit(void);

/**
 *   Give a border to the current view.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewMakeCurViewNice()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewMakeCurViewNice(void);

/**
 *   Reset the range on a specific axis so that it equals a little more than the minimum and maximum of
 *   the data being plotted allowing a small border to enhance visual appeal. If the axis dependency
 *   is not independent then this action may also affect the range on another axis.
 *
 * @param Axis
 *   Valid values are 'X' or 'Y', 'Z', 'T' or 'R'.
 *
 * @param AxisNum
 *   If the frame mode is XY, then this can be a number between one and five. Otherwise, this must be
 *   set to one
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewAxisNiceFit(
 *   &                   Axis,
 *   &                   AxisNum)
 *    CHARACTER*(*)   Axis
 *    INTEGER*4       AxisNum
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewAxisNiceFit(char  Axis,
                                                     short AxisNum);
/**
 *   Makes the curently viewed axis values visually appealing.
 *
 * @param Axis
 *   Valid values are 'X' or 'Y', 'Z', 'T' or 'R'.
 *
 * @param AxisNum
 *   If the frame mode is XY, then this can be a number between one and five. Otherwise, this must be
 *   set to one
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewAxisMakeCurValsNice(
 *   &                   Axis,
 *   &                   AxisNum)
 *    CHARACTER*(*)   Axis
 *    INTEGER*4       AxisNum
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewAxisMakeCurValsNice(char  Axis,
                                                             short AxisNum);
/**
 *   Reset the ranges on the 3-D axes. The current frame mode must be 3D to use this function.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilReset3DAxes()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Axis
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilReset3DAxes(void);

/**
 *
 * Rotate to specific angles. The current frame must be in 3D frame mode to use this function.
 *
 * @since
 *   14.2
 *
 * @param Psi
 *   Tilt, in degrees, of the eye origin ray away from the Z-axis.
 *
 * @param Theta
 *   Rotation, in degrees, of the eye origin ray about the Z-axis.
 *
 * @param Alpha
 *   Twist, in degrees, about the eye origin ray.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE FUNCTION TecUtilReset3DAngles(
 *   &                   Psi,
 *   &                   Theta,
 *   &                   Alpha)
 *    REAL*8          Psi
 *    REAL*8          Theta
 *    REAL*8          Alpha
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Axis
 *
 */
LINKTOADDON void STDCALL TecUtilRotateToSpecificAngles(double Psi,
                                                       double Theta,
                                                       double Alpha);

/**
 *
 * Reset angles to default value. The current frame must be in 3D frame mode to use this function.
 *
 * @since
 *   14.2
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>VALID_REF(FB)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE FUNCTION TecUtilReset3DAngles()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Axis
 *
 */
LINKTOADDON void STDCALL TecUtilReset3DAngles(void);

/**
 *   Recalculate the scale factors for the 3-D axes. Aspect ratio limits are taken into account. The
 *   current frame must be in 3D frame mode to use this function.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilReset3DScaleFactors()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Axis
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilReset3DScaleFactors(void);
/**
 * Print the current layout to a printer or send the print instructions to a
 * file. Use the TecUtilPrintSetup() function to configure printing.
 *
 * @return
 *   Returns TRUE if the printing completed successfully. A FALSE return value
 *   indicates that the user pressed cancel during printing or that the
 *   printing could not complete.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPrint()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Setup to print to a grayscale PostScript file called "print.ps" with black flooding appearing as a
 *   light gray and then do the printing:
 *
 * @code
 *   TecUtilPrintSetup("PALETTE", NULL,
 *                     0.0, (ArbParam_t)Palette_Monochrome);
 *   TecUtilPrintSetup("DRIVER", NULL,
 *                     0.0, (ArbParam_t)PD_PS);
 *   TecUtilPrintSetup("PRINTFNAME", NULL,
 *                     0.0, (ArbParam_t)"print.ps");
 *   TecUtilPrintSetup("MONOFLOODMAP", "BLACKSHADE",
 *                     90.0, (ArbParam_t)0);
 *   TecUtilPrint();
 * @endcode
 *
 * @ingroup Print
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPrint(void);

/**
 * @deprecated
 *   Please use TecUtilPickAddAtPositionX() instead.
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickAtPosition(double    X,
                                                    double    Y,
                                                    Boolean_t CollectingObjects,
                                                    Boolean_t DiggingForObjects);

/**
 * @deprecated
 *   Please use TecUtilPickAddAtPositionX() instead.
 *
 * Pick an object at a specified location. The location is defined as the (X,Y) location in inches
 * relative to the top-left edge of the paper. See Section 17.4, "The Pick List," in the ADK User's
 * Manual for a discussion of pick lists.
 *
 * @since
 *   11.4-1-1120
 *
 * @param X
 *   X-location (in inches) relative to the left edge of the paper
 *
 * @param Y
 *   Y-location (in inches) relative to the top edge of the paper
 *
 * @param CollectingObjects
 *   If FALSE, the list of picked objects is cleared before the attempt is made to add a new object. If
 *   TRUE, the new object is added to the list of picked objects. (This is like holding Shift down
 *   while selecting objects in the workspace.) If CollectingObjects is TRUE, DiggingForObjects must
 *   be FALSE
 *
 * @param DiggingForObjects
 *   If TRUE, attempt to pick objects below any currently picked objects at this location. If FALSE,
 *   pick the top object at this location. (This is like holding Ctrl down while selecting objects in
 *   the workspace. It allows you to select objects which are beneath other objects.) If
 *   DiggingForObjects is TRUE, CollectingObjects must be FALSE
 *
 * @param ConsiderStyle
 *   If TRUE, attempt to pick a visible object, checking for invisible objects only if no visible objects
 *   are found at this location. If FALSE, pick the top object (or next object, if DiggingForObjects is
 *   TRUE), whether visible or not. Visible objects include flooded or shaded surfaces, slices, isosurfaces
 *   and stream rods and ribbons.
 *
 * @return
 *   Currently, TRUE is always returned.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickAddAtPosition(
 *   &                   X,
 *   &                   Y,
 *   &                   CollectingObjects,
 *   &                   DiggingForObjects,
 *   &                   ConsiderStyle)
 *    REAL*8          X
 *    REAL*8          Y
 *    INTEGER*4       CollectingObjects
 *    INTEGER*4       DiggingForObjects
 *    INTEGER*4       ConsiderStyle
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Select a visible object at the location (1.5, 3.2). Then select the visible object underneath that object
 * instead, and add the visible object at (5.5, 5.4) to the list of picked objects:
 *
 * @code
 *   Boolean_t IsOk;
 *   IsOk = TecUtilPickAddAtPosition(1.5, 3.2, FALSE, FALSE, TRUE);
 *   IsOk = TecUtilPickAddAtPosition(1.5, 3.2, FALSE, TRUE,  TRUE);
 *   IsOk = TecUtilPickAddAtPosition(5.5, 5.4, TRUE,  FALSE, TRUE);
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickAddAtPosition(double    X,
                                                       double    Y,
                                                       Boolean_t CollectingObjects,
                                                       Boolean_t DiggingForObjects,
                                                       Boolean_t ConsiderStyle);

/**
 * Pick an object at a specified location. The location is defined as the (X,Y) location in inches
 * relative to the top-left edge of the paper. See Section 17.4, "The Pick List," in the ADK User's
 * Manual for a discussion of pick lists.
 *
 * @since
 *   11.4-1-1120
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_X
 * Type:
 *   Double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   X-location (in inches) relative to the left edge of the paper
 *
 * Name:
 *   SV_Y
 * Type:
 *   Double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   Y-location (in inches) relative to the top edge of the paper
 *
 * Name:
 *   SV_COLLECTINGOBJECTSMODE
 * Type:
 *   PickCollectMode_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   PickCollectMode_ExclusiveSelect
 * Required:
 *   No
 * Notes:
 *   If PickCollectMode_ExclusiveSelect, the list of picked objects is cleared before the attempt 
 *   is made to add a new object. If PickCollectMode_InvertingAdd, the new object is added to the
 *   list of picked objects. (This is like holding Shift down while selecting objects in the 
 *   workspace.) PickCollectMode_AlwaysAdd never removes items from picklist, and will add new items
 *   to picklist if new item is under cursor and not currently picked. If CollectingObjects is 
 *   PickCollectMode_InvertingAdd, DiggingForObjects must be FALSE.
 *
 * Name:
 *   SV_DIGGINGFOROBJECTS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   False
 * Required:
 *   No
 * Notes:
 *   If TRUE, attempt to pick objects below any currently picked objects at this location. If FALSE,
 *   pick the top object at this location. (This is like holding Ctrl down while selecting objects in
 *   the workspace. It allows you to select objects which are beneath other objects.) If
 *   DiggingForObjects is TRUE, CollectingObjects must be FALSE
 *
 * Name:
 *   SV_CONSIDERSTYLE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   True
 * Required:
 *   No
 * Notes:
 *   If TRUE, attempt to pick a visible object. If FALSE, pick the top object (or next object, if DiggingForObjects
 *   is TRUE), whether visible or not. Visible objects include flooded or shaded surfaces, slices, isosurfaces
 *   and stream rods and ribbons.
 * </ArgListTable>
 *
 * @return
 *   Currently, TRUE is always returned.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickAddAtPositionX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Select a visible object at the location (1.5, 3.2):
 *
 * @code
 * bool result = false;
 * ArgList_pa ArgList = TecUtilArgListAlloc();
 *
 * // X,Y Position
 * TecUtilArgListAppendDouble(ArgList, SV_X,  1.5);
 * TecUtilArgListAppendDouble(ArgList, SV_Y,  3.2);
 * 
 * // Collect Mode    
 * PickCollectMode_e collectMode = PickCollectMode_AlwaysAdd;
 * TecUtilArgListAppendInt(ArgList, SV_COLLECTINGOBJECTSMODE, collectMode);
 * 
 * // Digging for objects * 
 * TecUtilArgListAppendInt(ArgList, SV_DIGGINGFOROBJECTS, TRUE);
 * 
 * result = TecUtilPickAddAtPositionX(ArgList) == TRUE;
 * TecUtilArgListDealloc(&ArgList);
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickAddAtPositionX(ArgList_pa ArgList);

/**
 *   Deselect all selected objects. See Section 17.4, "The Pick List," in the ADK User's Manual for a
 *   discussion of pick lists.
 *
 * @return
 *   Currently TRUE is always returned.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickDeselectAll()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickDeselectAll(void);

/**
* Deselect a single pick list item. See Section 17.4, "The Pick List," in the ADK User's Manual for a
* discussion of pick lists.
*
* @param PickListItem
*   The index in the Pick List of the item to be removed.  This value must be
*   >= 1 and <= TecUtilPickListGetCount()
*
* @return
*   Currently TRUE is always returned.
*
* @sa TecUtilPickListGetCount()
*
* <FortranSyntax>
*    INTEGER*4 FUNCTION TecUtilPickDeselect()
* </FortranSyntax>
*
* @ingroup Pick
*/
LINKTOADDON Boolean_t STDCALL TecUtilPickDeselect(int PickListItem);

/**
 * Adds the frame identified by the unique identifier to the pick list. Each
 * Tecplot frame is assigned a unique ID that can be obtained for the current
 * frame by calling TecUtilFrameGetUniqueID().
 *
 * @param CollectingObjects
 *   Set to TRUE to add frames identified by the unique ID to the pick list. If
 *   FALSE, all objects previously picked are first removed from the pick list.
 *
 * @param UniqueID
 *   Unique ID of the frame.
 *
 * @return
 *   TRUE if the pick was successful, FALSE otherwise.
 *
 * @sa TecUtilFrameGetUniqueID()
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickAddFrameByUniqueID(
 *   &                   CollectingObjects,
 *   &                   UniqueID)
 *    INTEGER*4 CollectingObjects
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Add the current frame to a new pick list.
 *
 * @code
 *   {
 *     UniqueID_t FrameID;
 *     Boolean_t Picked;
 *     TecUtilLockStart(AddOnID);
 *     FrameID = TecUtilFrameGetUniqueID();
 *     Picked  = TecUtilPickAddFrameByUniqueID(FALSE, FrameID);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickAddFrameByUniqueID(Boolean_t  CollectingObjects,
                                                            UniqueID_t UniqueID);

/**
 *   Adds the specified Line-maps to the pick object list. In addition, you can control whether the
 *   Line-maps are added to the list of already picked objects or if the pick object list is cleared
 *   prior to adding them.
 *
 * @param CollectingObjects
 *   Set to TRUE to add identified linemaps to the pick list. If FALSE, all objects previously picked
 *   are first removed from the pick list.
 *
 * @param ZoneSet
 *   Set of zones to add to the pick list.
 *
 * @return
 *   Returns TRUE if the zones could be picked, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>ZoneSet</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickAddZones(
 *   &                   CollectingObjects,
 *   &                   ZoneSetPtr)
 *    INTEGER*4       CollectingObjects
 *    POINTER         (ZoneSetPtr, ZoneSet)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Pick zones 1 and 3
 *
 * @code
 *   Set_pa ZoneSet;
 *   ZoneSet = TecUtilSetAlloc(TRUE);
 *   TecUtilSetAddMember(ZoneSet,1,TRUE);
 *   TecUtilSetAddMember(ZoneSet,3,TRUE);
 *   TecUtilPickAddZones(TRUE,ZoneSet);
 *   TecUtilSetDealloc(&ZoneSet);
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickAddZones(Boolean_t CollectingObjects,
                                                  Set_pa    ZoneSet);


/**
 * @deprecated
 *   Please use TecUtilPickAddLineMaps() instead.
 *
 * @ingroup Pick
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickAddXYMaps(Boolean_t CollectingObjects,
                                                   Set_pa    XYMapsSet);



/**
 *   Adds the specified Line-maps to the pick object list. In addition, you can control whether the
 *   Line-maps are added to the list of already picked objects or if the pick object list is cleared
 *   prior to adding them.
 *
 * @param CollectingObjects
 *   Set to TRUE to add identified linemaps to the pick list. If FALSE, all objects previously picked
 *   are removed from the pick list
 *
 * @param LineMapSet
 *   Set of Line-maps to add to the pick list
 *
 * @return
 *   Returns TRUE if the linemaps could be picked, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>LineMapSet</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickAddLineMaps(
 *   &                   CollectingObjects,
 *   &                   LineMapSetPtr)
 *    INTEGER*4       CollectingObjects
 *    POINTER         (LineMapSetPtr, LineMapSet)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Pick Line-maps 1 and 3
 *
 * @code
 *   Set_pa LineMapSet;
 *   LineMapSet = TecUtilSetAlloc(TRUE);
 *   TecUtilSetAddMember(LineMapSet,1,TRUE);
 *   TecUtilSetAddMember(LineMapSet,3,TRUE);
 *   TecUtilPickAddLineMaps(TRUE,LineMapSet);
 *   TecUtilSetDealloc(&LineMapSet);
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickAddLineMaps(Boolean_t CollectingObjects,
                                                     Set_pa    LineMapSet);



/**
 *   Add all objects of a certain type to the pick list. See Section 17.4, "The Pick List," in the ADK
 *   User's Manual for a discussion of pick lists.
 *
 * @param ObjectType
 *   The type of object to select. Except for PickObjects_Frame, all objects are selected in the current
 *   frame. The possible values are: PickObjects_Text (Text) PickObjects_Geom (Geometries)
 *   PickObjects_Frame (Frames) PickObjects_Zone (Zones) PickObjects_LineMapping (X-Y Mappings)
 *   PickObjects_StreamtracePosition (Streamtraces starting positions)
 *
 * @return
 *   Returns FALSE if there is any error during processing otherwise TRUE. Note that a TRUE return value
 *   doesn't necessarily mean that something was added to the picklist. You must check the picklist
 *   contents before and after the call to determine if there was a change.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickAddAll(ObjectType)
 *    INTEGER*4 ObjectType
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Select all text and geometries in the current frame:
 *
 * @code
 *   Boolean_t TextsExist, GeomsExist;
 *   TecUtilPickDeselectAll();
 *   TextsExist = TecUtilPickAddAll(PickObjects_Text);
 *   GeomsExist = TecUtilPickAddAll(PickObjects_Geom);
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickAddAll(PickObjects_e ObjectType);



/**
 * @deprecated
 *   Please use TecUtilPickAddAllInRectX() instead.
 *
 * @ingroup Pick
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickAddAllInRect(double         X1,
                                                      double         Y1,
                                                      double         X2,
                                                      double         Y2,
                                                      PickObjects_e  ObjectType,
                                                      const char    *Filter);

/**
 * Add all objects defined within a specified rectangle to the pick list. This function operates
 * on the contents on the active frame unless SV_SELECTFRAMES is specified, where all frames that
 * are within the selection region will be selected, and the other options will only be applied to
 * the active frame.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_X1
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   X-Value for Top left corner of the rectangle defining the region in which to select objects.
 *   (Specify in inches relative to the top left corner of the paper.)
 *
 * Name:
 *   SV_Y1
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   Y-Value for Top left corner of the rectangle defining the region in which to select objects.
 *   (Specify in inches relative to the top left corner of the paper.)
 *
 * Name:
 *   SV_X2
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   X-Value for Lower right corner of the rectangle defining the region in which to select objects.
 *   (Specify in inches relative to the top left corner of the paper.)
 *
 * Name:
 *   SV_Y2
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   Y-Value for Lower right corner of the rectangle defininf the region in which to select objects.
 *   (Specify in inches relative to the top left corner of the paper.)
 *
 * Name:
 *   SV_SELECTTEXT
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   No
 * Default:
 *   FALSE
 * Notes:
 *   Set to TRUE if Text Geometries are to be selected.
 *
 * Name:
 *   SV_SELECTCONTOURLABELS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   No
 * Default:
 *   FALSE
 * Notes:
 *   Set to TRUE if Contour Labels are to be selected.
 *
 * Name:
 *   SV_SELECTFRAMES
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   No
 * Default:
 *   FALSE
 * Notes:
 *   Set to TRUE if frames are to be selected.
 *
 * Name:
 *   SV_SELECTGEOMS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   No
 * Default:
 *   FALSE
 * Notes:
 *   Set to TRUE if Geometries are to be selected.
 *
 * Name:
 *   SV_SELECTGRIDAREA
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   No
 * Default:
 *   FALSE
 * Notes:
 *   Set to TRUE if the grid area is to be selected.
 *
 * Name:
 *   SV_SELECTMAPS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   No
 * Default:
 *   FALSE
 * Notes:
 *   Set to TRUE if Line maps are to be selected. This option is mutually exclusive with
 *   SV_SELECTZONES.
 *
 * Name:
 *   SV_SELECTSTREAMTRACES
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   No
 * Default:
 *   FALSE
 * Notes:
 *   Set to TRUE if streamtraces are to be selected.
 *
 * Name:
 *   SV_SELECTZONES
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   No
 * Default:
 *   FALSE
 * Notes:
 *   Set to TRUE if zones are to be selected. This option is mutually exclusive with
 *   SV_SELECTMAPS.
 *
 * Name:
 *   SV_GEOMFILTER
 * Type:
 *   GeomType_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   No
 * Notes:
 *   This parameter will be used to filter the type of geometry selected. This
 *   parameter requires SV_SELECTGEOMS to be set to TRUE.
 *
 * Name:
 *   SV_ISITALIC
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   No
 * Default:
 *   FALSE
 * Notes:
 *   This parameter will be used to filter the text geometries selected and will
 *   only select text geometries with italic text. This parameter requires
 *   SV_FONTFILTER or SV_FONTFAMILYFILTER to be set to TRUE.
 *
 * Name:
 *   SV_ISBOLD
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   No
 * Default:
 *   FALSE
 * Notes:
 *   This parameter will be used to filter the text geometries selected and will
 *   only select text geometries with bold text. This parameter requires
 *   SV_FONTFILTER or SV_FONTFAMILYFILTER to be set to TRUE. 
 *
 * Name:
 *   SV_FONTFAMILYFILTER
 * Type:
 *   char*
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   No
 * Notes:
 *   This parameter will be used to filter the text geometries selected and will
 *   only select text geometries with the specified SV_FONTFAMILYFILTER. This parameter
 *   requires SV_SELECTTEXT to be set to TRUE and is mutually exclusive with SV_FONTFILTER.
 *
 * Name:
 *   SV_FONTFILTER
 * Type:
 *   Font_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   No
 * Notes:
 *   This parameter will be used to filter the text geometries selected and will
 *   only select text geometries with the specified SV_FONTFILTER. This parameter
 *   requires SV_SELECTTEXT to be set to TRUE and is mutually exclusive with
 *   SV_FONTFAMILYFILTER.
 *
 * Name:
 *   SV_LINEPATTERNFILTER
 * Type:
 *   LinePattern_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   No
 * Notes:
 *   This parameter will be used to filter the geometries selected and will only
 *   select geometries with the specified SV_LINEPATTERNFILTER. This parameter
 *   requires SV_SELECTGEOMS to be set to TRUE.
 *
 * Name:
 *   SV_COLORFILTER
 * Type:
 *   ColorIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   No
 * Notes:
 *   This parameter will be used to filter the objects and will only select with
 *   the specified SV_COLORFILTER. This parameter requires that at least one of
 *   SV_SELECTTEXT, SV_SELECTGEOMS, SV_SELECTMAPS or SV_SELECTZONES is set to TRUE.
 *
 * Name:
 *   SV_CONSIDERSTYLE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   No
 * Default:
 *   FALSE
 * Notes:
 *   This parameter will be used to filter the objects based on their style. If TRUE,
 *   only field or line maps that are visible will be selected, otherwise even invisible
 *   field or line maps will be selected. This parameter requires that either SV_SELECTMAPS
 *   or SV_SELECTZONES is set to TRUE.
 *
 * @return
 *   Returns FALSE if there is any error during processing otherwise TRUE. Note that a TRUE return value
 *   doesn't necessarily mean that something was added to the picklist. You must check the picklist
 *   contents before and after the call to determine if there was a change.
 *
 *
 * @pre <em>ArgListIsValid(argList)</em>
 *   Argument list must be valid.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickAddAllInRectX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Select visible zones in a rectangle comprised by (0.0;0.0) and (9;9)
 *
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList, SV_SELECTZONES,   TRUE);
 *   TecUtilArgListAppendInt(ArgList, SV_CONSIDERSTYLE, TRUE);
 *   TecUtilArgListAppendDouble(ArgList, SV_X1,            0.0);
 *   TecUtilArgListAppendDouble(ArgList, SV_Y1,            0.0);
 *   TecUtilArgListAppendDouble(ArgList, SV_X2,            9.0);
 *   TecUtilArgListAppendDouble(ArgList, SV_Y2,            9.0);
 *   IsOk = TecUtilPickAddAllInRectX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Pick
 *
 * @since 14.3
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickAddAllInRectX(ArgList_pa argList);

/**
 * Perform a global edit operation on the currently picked objects. Only one
 * edit operation is allowed per call to TecUtilPickEdit(). Objects are edited
 * only if the supplied parameter is relevant. See Section 17.4, "The Pick
 * List," in the ADK User's Manual for a discussion of pick lists.
 *
 * @param Action
 *   The action to perform on the currently picked objects. The action uses the
 *   same text which is used in the macro language for the $!PickEdit macro
 *   command. See the Tecplot Reference Manual for more information about
 *   available keywords
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 *
 * @pre <em>Action</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickEdit(Action)
 *    CHARACTER*(*) Action
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the color for all picked objects to yellow.
 *   Set the contour type for all picked zones to be "Flood".
 * @code
 *   TecUtilPickEdit("COLOR = YELLOW");
 *   TecUtilPickEdit("CONTOUR {COUNTOURTYPE = FLOOD}");
 * @endcode
 *
 * @code
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickEdit(const char *Action);
/**
 *   Copy all objects that are currently picked to the paste buffer and then delete them from the plot.
 *
 * @return
 *   TRUE if successful, FALSE if not or if no objects are currently picked.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickCut()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickCut(void);
/**
 *   Copy all objects that are currently picked to the paste buffer.
 *
 * @return
 *   TRUE if successful, FALSE if not or if no objects are currently picked.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickCopy()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickCopy(void);
/**
 *   Clear (that is, delete) all objects that are currently picked. These objects cannot be retrieved.
 *
 * @return
 *   TRUE if any objects were deleted.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickClear()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickClear(void);
/**
 *   Paste the currently picked objects from the paste buffer to the workspace.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickPaste()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickPaste(void);
/**
 *   Shift the currently picked objects. Objects are shifted relative to their
 *   starting position. X and Y shift amounts are in paper units (inches). If
 *   snapping is in effect then it is applied after shifting in X and Y.
 *
 * @param DXPaper
 *   Shift amount in the X-direction. Units are in inches. Positive numbers
 *   will shift objects to the right. Ignored if PointerStyle is
 *   PointerStyle_BottomBracket, PointerStyle_TopBracket, or
 *   PointerStyle_UpDown
 *
 * @param DYPaper
 *   Shift amount in the Y-direction. Units are in inches. Positive numbers
 *   will shift objects down.  Ignored if PointerStyle is
 *   PointerStyle_LeftBracket, PointerStyle_RightBracket, or
 *   PointerStyle_LeftRight.
 *
 * @param PointerStyle
 *   The type of shift to perform on the picked objects. This is ignored if not
 *   appropriate for the currently selected objects. PointerStyle_AllDirections
 *   is appropriate for all types of objects. Currently, the other options are
 *   only available for Axis and Frame objects.  See PointerStyle_e.
 *
 * @return
 *   TRUE if successful, FALSE if no items are selected or if a zone or
 *   Line-mapping is selected and the current mouse mode is not the Adjustor
 *   tool. Objects can only be selected if the current mouse mode is the
 *   selector or the Adjustor tool.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickShift(
 *   &                   DXPaper,
 *   &                   DYPaper,
 *   &                   PointerStyle)
 *    REAL*8          DXPaper
 *    REAL*8          DYPaper
 *    INTEGER*4       PointerStyle
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Select all frames and resize their upper-left corners to be 2.5 inches to
 *   the right and 3.5 inches down from their previous location. Then, select
 *   all text in the current frame and move them 1.5 inches to the left and
 *   0.25 inches up from their previous locations:
 * @code
 *   TecUtilPickSetMouseMode(MouseButtonMode_Select);
 *   if (TecUtilPickAddAll(PickObjects_Frame))
 *     TecUtilPickShift(2.5, 3.5, PointerStyle_UpperLeftBracket);
 *   TecUtilPickDeselectAll();
 *   if (TecUtilPickAddAll(PickObjects_Text))
 *     TecUtilPickShift(-1.5, -0.25, PointerStyle_AllDirections);
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickShift(double         DXPaper,
                                               double         DYPaper,
                                               PointerStyle_e PointerStyle);
/**
 *   Magnify all picked objects. The first object picked will remain in place; the other picked objects
 *   will move proportionally as the image is magnified. Only frames, text, and geometries are allowed
 *   to be magnified.
 *
 * @param MagFactor
 *   Amount to magnify
 *
 * @return
 *   TRUE if picked objects could be magnified, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickMagnify(MagFactor)
 *    REAL*8 MagFactor
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Double the size of all picked objects.
 *
 * @code
 *   Boolean_t IsOk = TecUtilPickMagnify(2.0);
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickMagnify(double MagFactor);
/**
 *   Change the order in which objects are drawn by pushing the currently picked objects back. Only
 *   frames, text, geometries, and the grid area for 2-D plots are allowed to be pushed.
 *
 * @return
 *   TRUE if all currently picked objects are allowed to be pushed, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickPush()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickPush(void);
/**
 *   Change the order in which objects are drawn by popping the currently picked objects to the front.
 *   Only frames, text, geometries, and the grid area for 2-D plots are allowed to be popped.
 *
 * @return
 *   TRUE if all currently picked objects are allowed to be popped, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickPop()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickPop(void);


/**
 * @deprecated
 *   Please use TecUtilMouseSetMode() instead.
 *
 * @ingroup Pick
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickSetMouseMode(MouseButtonMode_e MouseMode);






/**
 * @deprecated
 *   Please use TecUtilLineMapCopy() instead.
 *
 * @ingroup LineMap
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilXYMapCopy(EntIndex_t SourceMap,
                                               EntIndex_t DestMap);
/**
 *   Copy attributes from an existing Line-mapping to another.
 *
 * @param SourceMap
 *   Existing Line-map to copy. Must be greater than or equal to one and less than or equal to the
 *   current number of maps
 *
 * @param DestMap
 *   Destination map. Must be greater than or equal to one and less than or equal to the current number
 *   of maps plus one
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapCopy(
 *   &                   SourceMap,
 *   &                   DestMap)
 *    INTEGER*4       SourceMap
 *    INTEGER*4       DestMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Duplicate the first Line-map:
 *
 * @code
 *   Boolean_t IsOk = TecUtilLineMapCopy(1,2);
 *   if (!IsOk)
 *   {
 *    // function NOT successful
 *   }
 * @endcode
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapCopy(EntIndex_t SourceMap,
                                                 EntIndex_t DestMap);


/**
 * @deprecated
 *   Please use TecUtilLineMapCreate() instead.
 *
 * @ingroup LineMap
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilXYMapCreate(void);



/**
 *   Create a new Line-mapping.
 *
 * @return
 *   TRUE if successful, FALSE otherwise .
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapCreate()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapCreate(void);



/**
 * @deprecated
 *   Please use TecUtilLineMapDelete() instead.
 *
 * @ingroup LineMap
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilXYMapDelete(Set_pa MapsToDelete);



/**
 *   Delete one or more Line-maps.
 *
 * @param MapsToDelete
 *   Set of maps to delete
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 *
 * @pre <em>MapsToDelete</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapDelete(MapsToDeletePtr)
 *    POINTER (MapsToDeletePtr, MapsToDelete)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Delete maps 1 and 3:
 *
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   if (set)
 *   {
 *     TecUtilSetAddMember(set,1,TRUE);
 *     TecUtilSetAddMember(set,3,TRUE);
 *     TecUtilLineMapDelete(set);
 *     TecUtilSetDealloc(&set);
 *   }
 * @endcode
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapDelete(Set_pa MapsToDelete);



/**
 * @deprecated
 *   Please use TecUtilLineMapShiftToTop() instead.
 *
 * @ingroup LineMap
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilXYMapShiftToTop(Set_pa MapsToShift);



/**
 *   Shift a list of Line-maps to the top of the Line-map list. This in effect causes the selected
 *   Line-maps to be drawn first.
 *
 * @param MapsToShift
 *   Set of maps to shift. Must not be NULL
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 *
 * @pre <em>MapsToShift</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapShiftToTop(MapsToShiftPtr)
 *    POINTER (MapsToShiftPtr, MapsToShift)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Shift Line-maps 2 and 4 to the top:
 *
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set,2,FALSE);
 *   TecUtilSetAddMember(set,4,FALSE);
 *   TecUtilLineMapShiftToBottom(set);
 *   TecUtilSetDealloc(&set);
 * @endcode
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapShiftToTop(Set_pa MapsToShift);


/**
 * @deprecated
 *   Please use TecUtilLineMapShiftToBottom() instead.
 *
 * @ingroup LineMap
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilXYMapShiftToBottom(Set_pa MapsToShift);



/**
 *   Shift a list of Line-maps to the bottom of the Line-map list. This in effect causes the selected
 *   Line-maps to be drawn last.
 *
 * @param MapsToShift
 *   Set of maps to shift. Must not be NULL
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 *
 * @pre <em>MapsToShift</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapShiftToBottom(MapsToShiftPtr)
 *    POINTER (MapsToShiftPtr, MapsToShift)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Shift Line-maps 2 and 4 to the bottom:
 *
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set,2,FALSE);
 *   TecUtilSetAddMember(set,4,FALSE);
 *   TecUtilLineMapShiftToBottom(set);
 *   TecUtilSetDealloc(&set);
 * @endcode
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapShiftToBottom(Set_pa MapsToShift);

/**
 * @deprecated
 *   Please use TecUtilViewRotate3D() instead.
 *
 * @ingroup View
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewRotate(RotateAxis_e  RotateAxis,
                                                double        RotateAmountInDegrees);




/**
 *   Do a 3-D rotation about a given axis.
 *
 * @param RotateAxis
 *   Axis to rotate about. Valid values are: RotateAxis_X, RotateAxis_Y, RotateAxis_Z, RotateAxis_Psi,
 *   RotateAxis_Theta, RotateAxis_Alpha, RotateAxis_Twist, RotateAxis_VertRollerBall,
 *   RotateAxis_HorzRollerBall, RotateAxis_AboutVector
 *
 * @param RotateAmountInDegrees
 *   Amount to rotate in degrees
 *
 * @param VectorX
 *   X-Component for the vector to rotate about when RotateAxis_AboutVector is specified. At least one
 *   of VectorX, VectorY, or VectorZ must be non-zero.
 *
 * @param VectorY
 *   Y-Component for the vector to rotate about when RotateAxis_AboutVector is specified.
 *
 * @param VectorZ
 *   Z-Component for the vector to rotate about when RotateAxis_AboutVector is specified.
 *
 * @param RotateOriginLocation
 *   Specifies the location of the rotation origin for the rotation. Valid values are
 *   RotateOriginLocation_DefinedOrigin or RotateOriginLocation_Viewer. Use
 *   RotateOriginLocation_DefinedOrigin to rotate about the currently defined rotation origin. Use
 *   RotateOriginLocation_Viewer to rotate about the currently defined viewer position
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewRotate3D(
 *   &                   RotateAxis,
 *   &                   RotateAmountInDegrees,
 *   &                   VectorX,
 *   &                   VectorY,
 *   &                   VectorZ,
 *   &                   RotateOriginLocation)
 *    INTEGER*4       RotateAxis
 *    REAL*8          RotateAmountInDegrees
 *    REAL*8          VectorX
 *    REAL*8          VectorY
 *    REAL*8          VectorZ
 *    INTEGER*4       RotateOriginLocation
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Rotate about the Psi-Axis by 25 degrees at the currently defined rotation origin.
 *
 * @code
 *   TecUtilViewRotate3D(RotateAxis_Psi,
 *                                 25.0,
 *                                 0.0,
 *                                 0.0,
 *                                 0.0,
 *                                 RotateOriginLocation_DefinedOrigin);
 * @endcode
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewRotate3D(RotateAxis_e           RotateAxis,
                                                  double                 RotateAmountInDegrees,
                                                  double                 VectorX,
                                                  double                 VectorY,
                                                  double                 VectorZ,
                                                  RotateOriginLocation_e RotateOriginLocation);
/**
 *   Reposition the rotation origin in 3-D to be at the centroid of the data. The current frame must be
 *   in 3D mode to use this function.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilReset3DOrigin()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Axis
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilReset3DOrigin(void);
/**
 *   Sets the distance the viewer is from the plane that is parallel to the screen and passes through
 *   the 3-D rotation origin.
 *
 * @param EyeDistance
 *   The distance from the 3-D rotation origin to set the viewer
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSet3DEyeDistance(EyeDistance)
 *    REAL*8 EyeDistance
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilSet3DEyeDistance(double EyeDistance);
/**
 *   Reposition the rotation origin in 3-D to the specified location. The current frame must be in 3D
 *   frame mode to use this function.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_ORIGINRESETLOCATION
 * Type:
 *   OriginResetLocation_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   OriginResetLocation_DataCenter
 * Required:
 *   No
 * Notes:
 *   Associated value indicates the location to which the origin should be
 *   reset. Valid values are: OriginResetLocation_DataCenter and
 *   OriginResetLocation_ViewCenter
 * </ArgListTable>
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilReset3DOriginX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Reset the origin location to the center of the current view.
 *
 * @code
 *   Boolean_t IsOk = TRUE;
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList,
 *                           SV_ORIGINRESETLOCATION,
 *                           OriginResetLocation_ViewCenter);
 *   IsOk = TecUtilReset3DOriginX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Axis
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilReset3DOriginX(ArgList_pa ArgList);

/**
 * Reset the length of the vectors. Tecplot will find the vector with the
 * largest magnitude and set the scaling factor so it will appear on the screen
 * using the default length as specified in the configuration file with the
 * $!FRAMESETUP VECTDEFLEN macro command. The current frame must have a plot
 * type of cartesian 2D or 3D to use this function.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilResetVectorLength()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Vector
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilResetVectorLength(void);

/**
 * Reset the relative scatter sizing. Tecplot will find the scatter with the
 * largest size and set the scaling factor so it will appear on the screen
 * using the default length as specified in the configuration file with the
 * $!BASICSIZE SYMBOLSIZE { MEDIUM = &lt;double&gt; } macro command. The current
 * frame must have a plot type of cartesian 2D or 3D to use this function.
 *
 * @since
 *   10.0-4-23
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilScatterResetRelSize()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Scatter
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilScatterResetRelSize(void);


/**
 * Modify the contour levels.
 *
 * @param ArgList
 *   Set of Arglist entries. This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_CONTOURLEVELACTION
 * Type:
 *   ContourLevelAction_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The contour level action you want to take.  See Table below for
 *   more details.
 *
 * Name:
 *   SV_CONTOURGROUP
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   Number of the contour group to be modified.
 *
 * Name:
 *   SV_NUMVALUES
 * Type:
 *   int
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   Number of contour levels added, or number of new contour levels.
 *
 * Name:
 *   SV_APPROXNUMVALUES
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The approx number of new contour levels. Must be non-zero. The actual
 *   number will be adjusted slightly to give "nice" values of the contour
 *   variable at the levels. Use when SV_CONTOURLEVELACTION is set to
 *   ContourLevelAction_ResetToNice.
 *   Required for ContourLevelAction_ResetToNice.
 *
 * Name:
 *   SV_RAWDATA
 * Type:
 *   double *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Required:
 *   Yes
 * Notes:
 *   A pointer to an aray containing the values of the contour variable at each
 *   of the added levels or at each of the new levels.
 *   Required for ContourLevelAction_Add and ContourLevelAction_New. 
 *
 * Name:
 *   SV_RANGEMIN
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 * Notes:
 *   The value of the contour variable at the minimum end of the range to be
 *   deleted, or the value of the contour variable near the level to be
 *   deleted. Use when SV_CONTOURLEVELACTION is set to
 *   ContourLevelAction_DeleteRange.
 *
 * Name:
 *   SV_RANGEMAX
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 * Notes:
 *   The value of the contour variable at the maximum end of the range to be
 *   deleted.  Use when SV_CONTOURLEVELACTION is set to
 *   ContourLevelAction_DeleteRange.
 * </ArgListTable>
 *
 * @par SV_CONTOURLEVELACTION
   @verbatim
        SV_CONTOURLEVELACTION      Description
        ------------------------------------------------------------------------
        ContourLevelAction_Add     Add a new set of contour levels to the existing
                                   set of contour levels. You may specify the number
                                   of the contour group being modified (SV_CONTOURGROUP)
                                   and the number of contour levels being added
                                   (SV_ NUMVALUES). You must specify the value of the
                                   contour variable at each of the added levels
                                   (SV_RAWDATA).

        ContourLevelAction_New     Replace the current set of contour levels with a
                                   new set. You may specify the number of the
                                   contour group being modified (SV_CONTOURGROUP) and
                                   the number of contours levels in the new set
                                   (SV_NUMVALUES). You must specify the value of the
                                   contour variable at each of the new levels
                                   (SV_RAWDATA).

        ContourLevelAction_
        DeleteRange                Delete all contour levels between the specified
                                   minimum and maximum values of the contour variable
                                   (inclusive). You may specify the number of the
                                   contour group being modified (SV_CONTOURGROUP),
                                   the value of the contour variable at the minimum
                                   end of the range (SV_RANGEMIN), and the value of
                                   the contour variable at the maximum end of the
                                   range (SV_RANGEMAX).

        ContourLevelAction_Reset   Reset the contour levels to a set of evenly
                                   distributed values spanning the entire range of
                                   the currently selected contouring variable. You may
                                   specify the number of the contour group being
                                   modified (SV_CONTOURGROUP) and the number of
                                   contours levels in the new set (SV_NUMVALUES).

        ContourLevelAction_
        ResetToNice                Reset the contour levels to a set of evenly
                                   distributed values that approximately spans the
                                   range of the currently selected contouring variable.
                                   Exact range and number of levels will be adjusted
                                   to make contour levels have "nice" values. You
                                   may specify the number of the contour group being
                                   modified (SV_CONTOURGROUP) and the approximate
                                   number of contours levels in the new set
                                   (SV_APPROXNUMVALUES).

        ContourLevelAction_
        DeleteNearest              Delete the contour level with the value nearest
                                   the supplied value. You may specify the number of
                                   the contour group being modified (SV_CONTOURGROUP)
                                   and the value of the contour variable to be deleted
                                   (SV_RANGEMIN).

   @endverbatim
 *
 * @return
 *   TRUE if modification was successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilContourLevelX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Add contour levels 0.1, 0.2, and 0.3 to the current set of contour levels for contour group 2:
 *
 * @code
 *   Boolean_t IsOk = TRUE;
 *   ArgList_pa ArgList;
 *   double levels[3] = {0.1, 0.2, 0.3};
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList, SV_CONTOURLEVELACTION, (LgIndex_t) ContourLevelAction_Add);
 *   TecUtilArgListAppendInt(ArgList, SV_CONTOURGROUP, 2);
 *   TecUtilArgListAppendInt(ArgList, SV_NUMVALUES, 3);
 *   TecUtilArgListAppendArray(ArgList, SV_RAWDATA, (void *)levels);
 *   IsOk = TecUtilContourLevelX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Contour
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLevelX(ArgList_pa ArgList);


/**
 * @deprecated
 *   Please use TecUtilContourLevelX() instead.
 *
 * @ingroup Contour
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLevelAdd(int           NumEntries,
                                                     const double *RawData_Array,
                                                     Boolean_t     ShowTrace);

/**
 * @deprecated
 *   Please use TecUtilContourLevelX() instead.
 *
 * @ingroup Contour
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLevelNew(int           NumEntries,
                                                     const double *RawData_Array,
                                                     Boolean_t     ShowTrace);

/**
 * @deprecated
 *   Please use TecUtilContourLevelX() instead.
 *
 * @ingroup Contour
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLevelDeleteRange(double    RangeMin,
                                                             double    RangeMax,
                                                             Boolean_t ShowTrace);

/**
 * @deprecated
 *   Please use TecUtilContourLevelX() instead.
 *
 * @ingroup Contour
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLevelReset(int NumEntries);

/**
 * @deprecated
 *   Please use TecUtilContourLevelX() instead.
 *
 * @ingroup Contour
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLevelDelNearest(double    Level,
                                                            Boolean_t ShowTrace);

/**
 * Manage contour line labels in your plot.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 *
 * <ArgListTable>
 * Name:
 *   SV_CONTOURLABELACTION
 * Type:
 *   ContourLabelAction_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The possible values are: \ref ContourLabelAction_Add and
 *   \ref ContourLabelAction_DeleteAll
 *
 * Name:
 *   SV_CONTOURGROUP
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   Indicates which contour is affected.
 *
 * Name:
 *   SV_X
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 * Notes:
 *   This option is only applicable for SV_CONTOURLABELACTION set to
 *   \ref ContourLabelAction_Add.
 *
 * Name:
 *   SV_Y
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 * Notes:
 *   This option is only applicable for SV_CONTOURLABELACTION set to
 *   \ref ContourLabelAction_Add.
 *
 * Name:
 *   SV_Z
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 * Notes:
 *   This option is only applicable for SV_CONTOURLABELACTION set to
 *   \ref ContourLabelAction_Add in a 3D plot.
 *
 * Name:
 *   SV_ISALIGNED
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   If TRUE the contour label is aligned along the contour line otherwise it
 *   is drawn horizontally.  This option is only applicable for
 *   SV_CONTOURLABELACTION set to \ref ContourLabelAction_Add.
 * </ArgListTable>
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilContourLabelX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Add a contour label at position (0.5, 0.25) in a 2-D plot. Make it align
 * itself to the nearest contour line.
 *
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID)
 *   ArgList = TecUtilArgListAlloc();
 *
 *   TecUtilArgListAppendInt(ArgList, SV_CONTOURLABELACTION, ContourLabelAction_Add);
 *   TecUtilArgListAppendDouble(ArgList, SV_X, 0.5);
 *   TecUtilArgListAppendDouble(ArgList, SV_Y, 0.25);
 *   TecUtilArgListAppendInt(ArgList, SV_ISALIGNED, TRUE);
 *
 *   TecUtilContourLabelX(ArgList);
 *
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Contour
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLabelX(ArgList_pa ArgList);



/**
 * @deprecated
 *   Please use TecUtilContourLabelX() instead.
 *
 * @ingroup Contour
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLabelAdd(double    X,
                                                     double    Y,
                                                     double    Z,
                                                     Boolean_t IsAligned);


/**
 * @deprecated
 *   Please use TecUtilContourLabelX() instead.
 *
 * @ingroup Contour
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLabelDeleteAll(void);

/**
 * Add one or more streamtraces to the plot of the current frame. The frame must be either a
 * Cartesian 2D or 3D field plot.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 *
 * <ArgListTable>
 * Name:
 *   SV_STREAMTYPE
 * Type:
 *   Streamtrace_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   Type of streamtraces that are being added. The possible values are: \ref Streamtrace_SurfaceLine,
 *   \ref Streamtrace_VolumeLine, \ref Streamtrace_VolumeRibbon, \ref Streamtrace_VolumeRod, and
 *   \ref Streamtrace_TwoDLine.
 *
 * Name:
 *   SV_DISTRIBUTIONREGION
 * Type:
 *   DistributionRegion_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   Specifies the region for distributing the streamtraces. The possible values are:
 *   \ref DistributionRegion_Point, \ref DistributionRegion_Rake,
 *   \ref DistributionRegion_SurfacesOfActiveZones, and \ref DistributionRegion_SurfacesOfSelectedObjects.
 *
 * Name:
 *   SV_STREAMDIRECTION
 * Type:
 *   StreamDir_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   StreamDir_Both
 * Required:
 *   No
 * Notes:
 *   Direction of propagation of the streamtraces being added. The possible values are:
 *   \ref StreamDir_Forward, \ref StreamDir_Reverse, and \ref StreamDir_Both.
 *
 * Name:
 *   SV_NUMPTS
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1 for point distribution, $!FrameSetup NumStreamRakePoints for rake, and $!FrameSetup NumStreamSurfacePoints for surfaces.
 * Required:
 *   No
 * Notes:
 *   Number of seed points for distributing along a rake or on defined surfaces.
 *
 * Name:
 *   SV_X1
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   X ordinate of seed point or starting rake point.
 *   Required for distribution regions of \ref DistributionRegion_Point or \ref DistributionRegion_Rake.
 *
 * Name:
 *   SV_Y1
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   Y ordinate of seed point or starting rake point.
 *   Required for distribution regions of \ref DistributionRegion_Point or \ref DistributionRegion_Rake.
 *
 * Name:
 *   SV_Z1
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   Z ordinate of seed point or starting rake point.
 *   Required for distribution regions of \ref DistributionRegion_Point or \ref DistributionRegion_Rake for
 *   Cartesian 3D plots.
 *
 * Name:
 *   SV_X2
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   X ordinate of ending rake point.
 *   Required for distribution region of \ref DistributionRegion_Point if the streamtrace type is a rod
 *   or ribbon or for a region of \ref DistributionRegion_Rake. For DistributionRegion_Point the value
 *   is used to orient the streamtrace.
 *
 * Name:
 *   SV_Y2
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   Y ordinate of ending rake point.
 *   Required for distribution region of \ref DistributionRegion_Point if the streamtrace type is a rod
 *   or ribbon or for a region of \ref DistributionRegion_Rake. For DistributionRegion_Point the value
 *   is used to orient the streamtrace.
 *
 * Name:
 *   SV_Z2
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   Z ordinate of ending rake point.
 *   Required for distribution region of \ref DistributionRegion_Point if the streamtrace type is a rod
 *   or ribbon or for a region of \ref DistributionRegion_Rake. For DistributionRegion_Point the value
 *   is used to orient the streamtrace. Only applicable to Cartesian 3D plots.
 * </ArgListTable>
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * Distribute a rake of 10 volume streamtrace rods at the specified location (absent of any error
 * handling).
 *
 * @code
 *   ArgList_pa argList;
 *   argList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(argList,SV_NUMPTS,10);
 *   TecUtilArgListAppendInt(argList,SV_STREAMTYPE, (LgIndex_t)(Streamtrace_VolumeRod));
 *   TecUtilArgListAppendInt(argList,SV_STREAMDIRECTION, (LgIndex_t)(StreamDir_Both));
 *   TecUtilArgListAppendInt(argList,SV_DISTRIBUTIONREGION, (LgIndex_t)(DistributionRegion_Rake));
 *   TecUtilArgListAppendDouble(argList,SV_X1, 0.5);
 *   TecUtilArgListAppendDouble(argList,SV_Y1, 0.3311);
 *   TecUtilArgListAppendDouble(argList,SV_Z1, 0.4310);
 *   TecUtilArgListAppendDouble(argList,SV_X2, 0.5);
 *   TecUtilArgListAppendDouble(argList,SV_Y2, 0.3311);
 *   TecUtilArgListAppendDouble(argList,SV_Z2, 0.4312);
 *   isOk = TecUtilStreamtraceAddX(argList);
 *   TecUtilArgListDealloc(&argList);
 *   ...
 * @endcode
 *
 * @pre Must have one or more pages.
 * @pre Must have one or more frames.
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStreamtraceAddX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since
 *   13.4.0
 *
 * @sa TecUtilStreamtraceAdd
 *
 * @ingroup Streamtraces
 */
LINKTOADDON Boolean_t STDCALL TecUtilStreamtraceAddX(ArgList_pa ArgList);

/**
 * Add a single streamtrace or a rake of streamtraces to the current frame. The frame must be a 2-D
 * or 3-D field plot.
 *
 * @param NumRakePoints
 *   The number of points in the rake of streamtraces which are being added. Use one if just one
 *   streamtrace is being added. The number must be greater than zero
 *
 * @param StreamType
 *   The type of streamtraces which are being added. The possible values are: Streamtrace_SurfaceLine,
 *   Streamtrace_VolumeLine, Streamtrace_VolumeRibbon, Streamtrace_VolumeRod, Streamtrace_TwoDLine
 *
 * @param Direction
 *   The direction of the streamtraces which are being added. The possible values are:
 *   StreamDir_Forward, StreamDir_Reverse, or StreamDir_Both
 *
 * @param StartXPos
 *   X-Coordinate for the starting position of the streamtraces being added. If NumRakePoints is
 *   greater than one, then this is the starting position of the rake of streamtraces which are being
 *   added. StartZPos is only used for 3-D streamtraces
 *
 * @param StartYPos
 *   Y-Coordinate for the starting position of the streamtraces being added.
 *
 * @param StartZPos
 *   Z-Coordinate for the starting position of the streamtraces being added. Only used streamtraces are
 *   3-D.
 *
 * @param AltStartXPos
 *   X-Coordinate for the alternate starting position of the streamtraces which are being added. If
 *   NumRakePoints is one, this value is ignored (except in the case of streamtraces of type ribbon or
 *   rod, in which case the alternate starting position defines the streamtrace orientation).
 *   Otherwise, these values define the ending position of the rake of streamtraces which are being
 *   added.
 *
 * @param AltStartYPos
 *   Y-Coordinate for the alternate starting position of the streamtraces which are being added. See
 *   AltStartXPos.
 *
 * @param AltStartZPos
 *   Z-Coordinate for the alternate starting position of the streamtraces which are being added. See
 *   AltStartXPos.
 *
 * @return
 *   TRUE if successful, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStreamtraceAdd(
 *   &                   NumRakePoints,
 *   &                   StreamType,
 *   &                   Direction,
 *   &                   StartXPos,
 *   &                   StartYPos,
 *   &                   StartZPos,
 *   &                   AltStartXPos,
 *   &                   AltStartYPos,
 *   &                   AltStartZPos)
 *    INTEGER*4       NumRakePoints
 *    INTEGER*4       StreamType
 *    INTEGER*4       Direction
 *    REAL*8          StartXPos
 *    REAL*8          StartYPos
 *    REAL*8          StartZPos
 *    REAL*8          AltStartXPos
 *    REAL*8          AltStartYPos
 *    REAL*8          AltStartZPos
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Add a single 2-D line streamtrace to the current frame:
 *
 * @code
 *   Boolean_t IsOk = FALSE;
 *   double    Xp = 0.0;
 *   double    Yp = 0.0;
 *
 *   Xp = 10.0;
 *   Yp = 15.0;
 *   IsOk = TecUtilStreamtraceAdd(1, Streamtrace_TwoDLine,
 *                                StreamDir_Forward, Xp, Yp, 0.0,
 *                                0.0, 0.0, 0.0);
 *   if (IsOk)
 *     {
 *       // do more processing
 *         .
 *         .
 *         .
 *     }
 * @endcode
 *
 * @ingroup Streamtraces
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStreamtraceAdd(EntIndex_t    NumRakePoints,
                                                    Streamtrace_e StreamType,
                                                    StreamDir_e   Direction,
                                                    double        StartXPos,
                                                    double        StartYPos,
                                                    double        StartZPos,
                                                    double        AltStartXPos,
                                                    double        AltStartYPos,
                                                    double        AltStartZPos);
/**
 *   Delete all streamtraces in the current frame. Streamtraces are numbered sequentially.
 *
 * @return
 *   TRUE if successful, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStreamtraceDeleteAll()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Streamtraces
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStreamtraceDeleteAll(void);
/**
 *   Delete a range of streamtraces. Streamtraces are numbered sequentially in the order they were
 *   created.
 *
 * @param Start
 *   Starting stream in the range of Streamtraces to delete.
 *
 * @param End
 *   Ending stream in the range of Streamtraces to delete.
 *
 * @return
 *   TRUE if successful, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStreamtraceDeleteRange(
 *   &                   Start,
 *   &                   End)
 *    INTEGER*4       Start
 *    INTEGER*4       End
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Delete streamtraces 1 through 5.
 *
 * @code
 *   Boolean_t IsOk = FALSE;
 *   IsOk = TecUtilStreamtraceDeleteRange(1, 5);
 *   if (IsOk)
 *     {
 *       // do more processing
 *         .
 *         .
 *         .
 *     }
 * @endcode
 *
 * @ingroup Streamtraces
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStreamtraceDeleteRange(EntIndex_t Start,
                                                            EntIndex_t End);
/**
 *   Set the position of the termination line for streamtraces.
 *
 * @param NumPoints
 *   The number of XY-termination line points supplied. The number must be greater than zero, and less
 *   than or equal to the number of XY-termination points supplied
 *
 * @param XTermLinePts_Array
 *   Array of X-termination line ordinates
 *
 * @param YTermLinePts_Array
 *   Array of Y-termination line ordinates
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 *
 * @pre <em>XTermLinePts_Array</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>YTermLinePts_Array</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStreamtraceSetTermLine(
 *   &                   NumPoints,
 *   &                   XTermLinePts_Array,
 *   &                   YTermLinePts_Array)
 *    INTEGER*4       NumPoints
 *    REAL*8          XTermLinePts_Array
 *    REAL*8          YTermLinePts_Array
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Define a line for terminating streamtraces.
 *
 * @code
 *   Boolean_t IsOk = FALSE;
 *   double    Xp[2];
 *   double    Yp[2];
 *
 *   Xp[0] = 0.0;
 *   Yp[0] = 0.0;
 *   Xp[1] = 5.0;
 *   Yp[1] = 8.0;
 *   IsOk = TecUtilStreamtraceSetTermLine(2, Xp, Yp);
 *   if (IsOk)
 *     {
 *       // do more processing
 *         .
 *         .
 *         .
 *     }
 * @endcode
 *
 * @ingroup Streamtraces
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStreamtraceSetTermLine(EntIndex_t    NumPoints,
                                                            const double *XTermLinePts_Array,
                                                            const double *YTermLinePts_Array);
/**
 *   Determines if streamtraces have the terminating line.
 *   This function is \ref threadsafe.
 *
 * @return
 *   Returns TRUE if the terminating line exists.
 *   The terminating line is visible if it is active.
 *   To activate the termination line use the
 *   SV_STREAMATTRIBUTES, SV_TERMLINE, SV_ISACTIVE style.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStreamtraceHasTermLine()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since 11.3-17-015
 *
 * @ingroup Streamtraces
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStreamtraceHasTermLine(void);

/**
 *   Resets the time interval between stream markers.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStreamtraceResetDelta()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Streamtraces
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStreamtraceResetDelta(void);

/**
 *   Get the number of active streamtraces.
 *   This function is \ref threadsafe.
 *
 * @return
 *   Returns the number of streamtraces currently defined.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStreamtraceGetCount()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Streamtraces
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilStreamtraceGetCount(void);


/**
 * @deprecated
 *   Please use TecUtilQueryStreamtracesAreActive instead. 
 * 
 *   Determines if streamtraces are active.
 *   This function is \ref threadsafe.
 *
 * @return
 *   Returns TRUE if streamtraces are active.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStreamtracesAreActive()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Streamtraces
 *
 * @since 11.3-17-015
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStreamtracesAreActive(void);

/**
 *   Get the starting position for a given streamtrace.
 *   This function is \ref threadsafe.
 *
 * @param StreamNumber
 *   Number of the streamtrace
 *
 * @param X
 *   References to the Variable in which to store the X-position of the streamtrace.
 *
 * @param Y
 *   References to the Variable in which to store the Y-position of the streamtrace.
 *
 * @param Z
 *   References to the Variable in which to store the Z-position of the streamtrace.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>X</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Y</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Z</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStreamtraceGetPos(
 *   &           StreamNumber,
 *   &           X,
 *   &           Y,
 *   &           Z)
 *    INTEGER*4       StreamNumber
 *    REAL*8          X
 *    REAL*8          Y
 *    REAL*8          Z
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the position of streamtrace number 3.
 *
 * @code
 *     double X,Y,Z;
 *     TecUtilStreamtraceGetPos(3,&X,&Y,&Z);
 * @endcode
 *
 * @ingroup Streamtraces
 *
 */
LINKTOADDON void STDCALL TecUtilStreamtraceGetPos(EntIndex_t     StreamNumber,
                                                  TP_OUT double* X,
                                                  TP_OUT double* Y,
                                                  TP_OUT double* Z);

/**
 * Get the type of a given streamtrace.
 * This function is @ref threadsafe.
 *
 * @param StreamNumber
 *   Number of the streamtrace. The valid range is between 1 and the return value of
 *   the @ref TecUtilStreamtraceGetCount function.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStreamtraceGetType(
 *   &           StreamNumber)
 *    INTEGER*4       StreamNumber
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the type of streamtrace number 3.
 *
 * @code
 *     Streamtrace_e streamTraceType = TecUtilStreamtraceGetType(3);
 * @endcode
 *
 * @ingroup Streamtraces
 *
 * @since 11.3-17-015
 *
 */
LINKTOADDON Streamtrace_e STDCALL TecUtilStreamtraceGetType(EntIndex_t StreamNumber);

/**
 * Assign a value to a field variable at a specific position. This function
 * does not require you to obtain the handle to the field data as does
 * TecUtilDataValueSetByRef(), however this function is not very efficient.
 * Use TecUtilDataValueSetByRef() if you are setting multiple values in the
 * same zone. If the zone referenced is IJ- or IJK-ordered then the position is
 * calculated by treating the two- or three-dimensional array as a
 * one-dimensional array. TecUtilStateChanged() need not be called after
 * changing data in this way since Tecplot will handle that for you.
 *
 * @param Zone
 *   The zone number.
 *
 * @param Var
 *   The variable number.
 *
 * @param PointIndex
 *   Position in the array of field data values. Position starts at one.
 *
 * @param Value
 *   New value for the position in the field data.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueSetByZoneVar(
 *   &                   Zone,
 *   &                   Var,
 *   &                   PointIndex,
 *   &                   Value)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 *    INTEGER*4       PointIndex
 *    REAL*8          Value
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Set the first value of the second variable of zone 5 to be 0.0.
 *
 * @code
 *   TecUtilDataValueSetByZoneVar(5, 2, 1, 0.0);
 * @endcode
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueSetByZoneVar(EntIndex_t Zone,
                                                           EntIndex_t Var,
                                                           LgIndex_t  PointIndex,
                                                           double     Value);

/**
 * Assign a value to a field variable at a specific position. This function
 * does not require you to obtain the handle to the field data as does
 * TecUtilDataValueSetByRef(), however this function is not very efficient.
 * Use TecUtilDataValueSetByRef() if you are setting multiple values in the
 * same zone. If the zone referenced is IJ- or IJK-ordered then the position is
 * calculated by treating the two- or three-dimensional array as a
 * one-dimensional array. TecUtilStateChanged() need not be called after
 * changing data in this way since Tecplot will handle that for you.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 *
 * @return 
 *   Returns TRUE if successful, FALSE otherwise.
 *
 * <ArgListTable>
 * Name:
 *   SV_ZONE
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The zone number.
 *
 * Name:
 *   SV_VAR
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The variable number.
 *
 * Name:
 *   SV_INDEX
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   Position in the array of field data values. Position starts at one.
 *
 * Name:
 *   SV_VALUE
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   New value for the position in the field data.
 *
 * Name:
 *   SV_AUTOBRANCH
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendceInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Branch variable automatically if it is currently shared.
 * </ArgListTable>
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueSetByZoneVarX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Set the first value of the second variable of zone 5 to be 0.0.
 *
 * @ingroup DataValue
 *
 * @since 14.2
 *
 * @code
 *   ArgList_pa ArgList;
 *
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *
 *   TecUtilArgListAppendInt(ArgList, SV_ZONE, 1);
 *   TecUtilArgListAppendInt(ArgList, SV_VAR, 4);
 *   TecUtilArgListAppendInt(ArgList, SV_INDEX, 10);
 *   TecUtilArgListAppendDouble(ArgList, SV_VALUE, 123.4);
 *   TecUtilArgListAppendInt(ArgList, SV_AUTOBRANCH, (LgIndex_t)TRUE);
 *
 *   TecUtilDataValueSetByZoneVarX(ArgList);
 *
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueSetByZoneVarX(ArgList_pa ArgList);

/**
 *   Operates on a data set within Tecplot using FORTRAN-like equations. See Section 8.2, "Data
 *   Alteration through Equations," in the Tecplot User's Manual for more information on using
 *   equations in Tecplot.
 *
 * @param Equation
 *   String containing the equation
 *
 * @param ZoneSet
 *   Set of zones to operate on. Pass NULL to operate on all zones
 *
 * @param IMin
 *   Operate on values starting at an I-Index range of IMin. Set to 1 when operating on the entire
 *   range. For nodal finite-element data, the starting node index on which to operate. Not used
 *   for finite-element cell-centered data.
 *
 * @param IMax
 *   Operate on values ending at an I-Index range of IMax. Set to 0 to specify the maximum I index.
 *   Not used for finite-element cell-centered data.
 *
 * @param ISkip
 *   Operate on values skipping by ISkip in the I-Direction. Set to 0 to specify one less than the
 *   maximum I index range. Not used for finite-element cell-centered data.
 *
 * @param JMin
 *   Operate on values starting at an J-Index range of JMin. Set to 1 when operating on the entire
 *   range. For cell-centered finite-element data, the starting cell index on which to operate. Not
 *   used for finite-element nodal data.
 *
 * @param JMax
 *   See IMax. Not used for finite-element nodal data.
 *
 * @param JSkip
 *   See ISkip. Not used for finite-element nodal data.
 *
 * @param KMin
 *   See IMin. Not used for finite-element data.
 *
 * @param KMax
 *   See IMax. Not used for finite-element data.
 *
 * @param KSkip
 *   See ISkip. Not used for finite-element data.
 *
 * @param DestDataType
 *   Data type for the variable on the left hand side. This is used only if this variable is being
 *   created for the first time. The possible choices are: FieldDataType_Float,
 *   FieldDataType_Double, FieldDataType_Int32, FieldDataType_Int16, FieldDataType_Bit,
 *   FieldDataType_Byte, or FieldDataType_Invalid.  If set to FieldDataType_Invalid, Tecplot
 *   will choose the type for you.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>Equation</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>ZoneSet</em>
 *   Pointer must be a valid address or NULL.
 * @pre I range arguments <em>(IMin, IMax, ISkip)</em> must be within range -MAXINDEX ... MAXINDEX.
 * @pre J range arguments <em>(JMin, JMax, JSkip)</em> must be within range -MAXINDEX ... MAXINDEX.
 * @pre K range arguments <em>(KMin, KMax, KSkip)</em> must be within range -MAXINDEX ... MAXINDEX.
 *
 * @pre <em>DestDataType</em>
 *   Must specify a valid data type.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataAlter(
 *   &                   Equation,
 *   &                   ZoneSetPtr,
 *   &                   IMin,
 *   &                   IMax,
 *   &                   ISkip,
 *   &                   JMin,
 *   &                   JMax,
 *   &                   JSkip,
 *   &                   KMin,
 *   &                   KMax,
 *   &                   KSkip,
 *   &                   DestDataType)
 *    CHARACTER*(*)   Equation
 *    POINTER         (ZoneSetPtr, ZoneSet)
 *    INTEGER*4       IMin
 *    INTEGER*4       IMax
 *    INTEGER*4       ISkip
 *    INTEGER*4       JMin
 *    INTEGER*4       JMax
 *    INTEGER*4       JSkip
 *    INTEGER*4       KMin
 *    INTEGER*4       KMax
 *    INTEGER*4       KSkip
 *    INTEGER*4       DestDataType
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Add 2 to X in zones 1 and 3. Operate only data points where J is greater than or equal to three
 * and less than or equal to ten:
 *
 * @code
 *   if (TecUtilDataSetIsAvailable())
 *     {
 *       Set_pa    ZoneSet;
 *       Boolean_t IsOk = FALSE;
 *       ZoneSet = TecUtilSetAlloc(TRUE);
 *
 *       if (ZoneSet)
 *         {
 *           TecUtilSetAddMember(ZoneSet,1,TRUE);
 *           TecUtilSetAddMember(ZoneSet,3,TRUE);
 *
 *           IsOk = TecUtilDataAlter("X = X + 2",
 *                                   ZoneSet,
 *                                   1,0,1,  // All I-Values
 *                                   3,10,1  // 3 <= J <= 10
 *                                   1,0,1,  // All K Values
 *                                   FieldDataType_Float);
 *           TecUtilSetDealloc(&ZoneSet);
 *         }
 *     }
 * @endcode
 *
 * Fortran Example.  Execute X = X + 2 on all points in all zones:
 *
 * @code
 *      POINTER(NullPtr, Null)
 *      INTEGER*4 IsOk
 *
 *      NullPtr = 0
 *
 *      if (TecUtilDataSetIsAvailable().EQ.TRUE) then
 *        IsOk = TecUtilDataAlter('X = X + 2'//char(0),
 *     &                          NullPtr,
 *     &                          1,0,1,
 *     &                          1,0,1,
 *     &                          1,0,1,
 *     &                          FieldDataType_Float)
 *      Endif
 * @endcode
 *
 * @ingroup DataManipulation
 * @sa TecUtilDataAlterX
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataAlter(const char     *Equation,
                                               Set_pa          ZoneSet,
                                               LgIndex_t       IMin,
                                               LgIndex_t       IMax,
                                               LgIndex_t       ISkip,
                                               LgIndex_t       JMin,
                                               LgIndex_t       JMax,
                                               LgIndex_t       JSkip,
                                               LgIndex_t       KMin,
                                               LgIndex_t       KMax,
                                               LgIndex_t       KSkip,
                                               FieldDataType_e DestDataType);

/**
 *   Operates on a data set within Tecplot using FORTRAN-like equations. See Section 8.2, "Data
 *   Alteration through Equations," in the Tecplot User's Manual for more information on using
 *   equations in Tecplot.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_EQUATION
 * Type:
 *   const char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   String containing the equation.
 *
 * Name:
 *   SV_ZONESET
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Set of zones to operate on. If the zone set is not specified, the equation will be applied to all zones.
 *
 * Name:
 *   SV_IMIN
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   Operate on values starting at an I-Index range of IMin. Set to 1 when operating on the entire
 *   range. For nodal finite-element data, the starting node index on which to operate. Not used
 *   for finite-element cell-centered data.
 *
 * Name:
 *   SV_JMIN
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   Operate on values starting at an J-Index range of JMin. Set to 1 when operating on the entire
 *   range. For cell-centered finite-element data, the starting cell index on which to operate. Not
 *   used for finite-element nodal data.
 *
 * Name:
 *   SV_KMIN
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   See IMin. Not used for finite-element data.
 *
 * Name:
 *   SV_IMAX
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0 (Mx)
 * Required:
 *   No
 * Notes:
 *   Operate on values ending at an I-Index range of IMax. Set to 0 to specify the maximum I index.
 *   Not used for finite-element cell-centered data.
 *
 * Name:
 *   SV_JMAX
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0 (Mx)
 * Required:
 *   No
 * Notes:
 *   See IMax. Not used for finite-element nodal data.
 *
 * Name:
 *   SV_KMAX
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0 (Mx)
 * Required:
 *   No
 * Notes:
 *   See IMax. Not used for finite-element data.
 *
 * Name:
 *   SV_ISKIP
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   Operate on values skipping by ISkip in the I-Direction. Set to 0 to specify one less than the
 *   maximum I index range. Not used for finite-element cell-centered data.
 *
 * Name:
 *   SV_JSKIP
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   See ISkip. Not used for finite-element nodal data.
 *
 * Name:
 *   SV_KSKIP
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   See ISkip. Not used for finite-element nodal data.
 *
 * Name:
 *   SV_VALUELOCATION
 * Type:
 *   ValueLocation_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   Auto
 * Required:
 *   No
 * Notes:
 *   Variable value location for the variable on the left hand side.
 *   This is used only if this variable is being created for the first time.
 *   If unspecified, Tecplot will choose the location for you.
 *
 * Name:
 *   SV_VARDATATYPE
 * Type:
 *   FieldDataType_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   Auto
 * Required:
 *   No
 * Notes:
 *   Data type for the variable on the left hand side.
 *   This is used only if this variable is being created for the first time.
 *   If unspecified, Tecplot will choose the type for you.
 *
 * Name:
 *   SV_IGNOREDIVIDEBYZERO
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Boolean value which instructs Tecplot to ignore divide by zero errors. The result is clamped
 *   such that 0/0 is clamped to zero and (+/-N)/0 where N != 0 clamps to +/-maximum value for the
 *   given type.
 * </ArgListTable>
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataAlterX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataManipulation
 * @sa TecUtilDataAlter
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataAlterX(ArgList_pa ArgList);

/**
 *   Smooth data (that is, reduce the spikes) for selected variables in selected zones.
 *
 * @param Zone
 *   The number of the zone to smooth. The zone must be an ordered zone (not a finite-element zone)
 *
 * @param SmoothVar
 *   The number of the variable to smooth. This cannot be a variable which is assigned to an axis
 *
 * @param NumSmoothPasses
 *   The number of smoothing passes to perform. The normal default value is on
 *
 * @param SmoothWeight
 *   The relaxation factor for each pass of smoothing. Must be a number between zero and one
 *   (exclusively). Higher numbers indicate a greater smoothing effect. The normal default value is
 *   0.8
 *
 * @param SmoothBndryCond
 *   The boundary condition by which to smooth.
 *
 * @return
 *   TRUE if the input parameters are valid and the current frame mode is XY, 2D, or 3D. Otherwise,
 *   FALSE.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSmooth(
 *   &                   Zone,
 *   &                   SmoothVar,
 *   &                   NumSmoothPasses,
 *   &                   SmoothWeight,
 *   &                   SmoothBndryCond)
 *    INTEGER*4       Zone
 *    INTEGER*4       SmoothVar
 *    INTEGER*4       NumSmoothPasses
 *    REAL*8          SmoothWeight
 *    INTEGER*4       SmoothBndryCond
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Perform one smoothing pass on variable 3 in zone 2:
 *
 * @code
 *   TecUtilSmooth(2, 3, 1, 0.8, BoundaryCondition_Fixed);
 * @endcode
 *
 * @ingroup DataManipulation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilSmooth(EntIndex_t          Zone,
                                            EntIndex_t          SmoothVar,
                                            LgIndex_t           NumSmoothPasses,
                                            double              SmoothWeight,
                                            BoundaryCondition_e SmoothBndryCond);



/**
 * @deprecated
 *   Please use TecUtilCurveWriteInfo() instead.
 *
 * @ingroup Export
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilWriteCurveInfo(const char     *FName,
                                                    EntIndex_t      XYMapNum,
                                                    ProcessXYMode_e ProcessXYMode);



/**
 * Write out the coefficients or the calculated data points for the equations
 * used to draw the curve for a selected line-mapping.
 *
 * @param FileName
 *   File name. Must not be NULL
 *
 * @param LineMap
 *   The number of a line-mapping that does some type of curve fit or spline
 *
 * @param CurveInfoMode
 *   The possible values are:
 *     CurveInfoMode_Coefficients,
 *     CurveInfoMode_RawData, and
 *     CurveInfoMode_Macro
 *
 * @return
 *   TRUE if the curve information wrote, FALSE otherwise.
 *
 *
 * @pre <em>FileName</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCurveWriteInfo(
 *   &                   FileName,
 *   &                   LineMap,
 *   &                   CurveInfoMode)
 *    CHARACTER*(*)   FileName
 *    INTEGER*4       LineMap
 *    INTEGER*4       CurveInfoMode
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Write out the coefficients for line-mapping number 3 to the file map3.out:
 *
 * @code
 *   Boolean_t IsOk;
 *   IsOk = TecUtilCurveWriteInfo("map3.out", 3,CurveInfoMode_Coefficients);
 * @endcode
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilCurveWriteInfo(const char      *FileName,
                                                    EntIndex_t       LineMap,
                                                    CurveInfoMode_e  CurveInfoMode);
/**
 *   Make a copy of a zone or zones.
 *
 *  @note The variables in the duplicate zone initially share data with the source zone;
 *  use TecUtilDataValueBranchShared() to branch any variables you do not want to be shared.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_SOURCEZONE
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The source zone. This can be only used if SV_SOURCEZONES is not set.
 *
 * Name:
 *   SV_SOURCEZONES
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Required:
 *   Yes
 * Notes:
 *   The source zones. This can be only used if SV_SOURCEZONE is not set.
 *
 * Name:
 *   SV_DESTINATIONZONE
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   The number of Zones in the DataSet plus one.  TecUtilDataSetGetNumZones()+1.
 * Required:
 *   No
 * Notes:
 *   The destination zone number for the duplicated zone. If more than one zone is
 *   specified for the source zones then zone numbers are assigned sequentially using
 *   this value for the first zone, this value plus one for the next zone and so on.
 *   Specifying an existing zone number will replace that zone with a copy of SourceZone.
 *   SourceZone and DestinationZone cannot be the same value.  Destination zone will
 *   be assigned a new UniqueID.
 *
 * Name:
 *   SV_IMIN
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 *
 * Name:
 *   SV_IMAX
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   0 = Max
 *
 * Name:
 *   SV_ISKIP
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 *
 * Name:
 *   SV_JMIN
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 *
 * Name:
 *   SV_JMAX
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   0 = Max
 *
 * Name:
 *   SV_JSKIP
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 *
 * Name:
 *   SV_KMIN
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 *
 * Name:
 *   SV_KMAX
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   0 = Max
 *
 * Name:
 *   SV_KSKIP
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * </ArgListTable>
 *
 * @return
 *   Returns TRUE if successful, otherwise FALSE.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneCopyX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Zone
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneCopyX(ArgList_pa ArgList);
/**
 *   Make a copy of an existing zone. You can use index ranges to create a new zone which is a
 *   subset of the source zone.
 *   
 *  @note The variables in the duplicate zone initially share data with the source zone; 
 *  use TecUtilDataValueBranchShared() to branch any variables you do not want to be shared.
 *
 * @param ZoneUsed
 *   Source zone. Must be greater than or equal to one
 *
 * @param IMin
 *   Minimum I-index. Set to one to duplicate the entire zone
 *
 * @param IMax
 *   Maximum I-index. Set to zero to duplicate the entire zone
 *
 * @param ISkip
 *   I skip value. Set to one to duplicate the entire zone
 *
 * @param JMin
 *   Minimum J-index. Set to one to duplicate the entire zone
 *
 * @param JMax
 *   Maximum J-index. Set to zero to duplicate the entire zone
 *
 * @param JSkip
 *   J skip value. Set to one to duplicate the entire zone.
 *
 * @param KMin
 *   Minimum K-index. Set to one to duplicate the entire zone.
 *
 * @param KMax
 *   Maximum K-index. Set to zero to duplicate the entire zone
 *
 * @param KSkip
 *   K skip value. Set to one to duplicate the entire zone
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneCopy(
 *   &                   ZoneUsed,
 *   &                   IMin,
 *   &                   IMax,
 *   &                   ISkip,
 *   &                   JMin,
 *   &                   JMax,
 *   &                   JSkip,
 *   &                   KMin,
 *   &                   KMax,
 *   &                   KSkip)
 *    INTEGER*4       ZoneUsed
 *    INTEGER*4       IMin
 *    INTEGER*4       IMax
 *    INTEGER*4       ISkip
 *    INTEGER*4       JMin
 *    INTEGER*4       JMax
 *    INTEGER*4       JSkip
 *    INTEGER*4       KMin
 *    INTEGER*4       KMax
 *    INTEGER*4       KSkip
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Duplicate zone 3:
 *
 * @code
 *   Boolean_t IsOK = TecUtilZoneCopy(3,1,0,1,1,0,1,1,0,1);
 * @endcode
 *
 * @ingroup Zone
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneCopy(EntIndex_t ZoneUsed,
                                              LgIndex_t  IMin,
                                              LgIndex_t  IMax,
                                              LgIndex_t  ISkip,
                                              LgIndex_t  JMin,
                                              LgIndex_t  JMax,
                                              LgIndex_t  JSkip,
                                              LgIndex_t  KMin,
                                              LgIndex_t  KMax,
                                              LgIndex_t  KSkip);
/**
 *   Create new zones that are mirror images of the source zones.
 *
 * @param SourceZones
 *   Set of zones to mirror. A separate new zone is created for each zone in the set. Pass NULL to
 *   mirror all zones
 *
 * @param MirrorVar
 *   Variable to negate. Use the following table to determine the value of MirrorVarin 2-D: Mirror
 *   about: MirrorVarX-Axis 'Y'Y-Axis 'X'in 3-D: Mirror about: MirrorVarX-Y Plane 'Z'Y-Z Plane 'X'X-Z
 *   Plane 'Y'
 *
 * @return
 *   TRUE if successfull, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>SourceZones</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateMirrorZones(
 *   &                   SourceZonesPtr,
 *   &                   MirrorVar)
 *    POINTER         (SourceZonesPtr, SourceZones)
 *    CHARACTER*(*)   MirrorVar
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Create two mirror zones about the X-axis:
 *
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   if (set)
 *   {
 *     TecUtilSetAddMember(set,1,FALSE); // mirror zones 1 and 2
 *     TecUtilSetAddMember(set,2,FALSE);
 *
 *     TecUtilCreateMirrorZones(set,'X');
 *     TecUtilSetDealloc(&set);
 *   }
 * @endcode
 *
 * @ingroup DataCreate
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateMirrorZones(Set_pa SourceZones,
                                                       char   MirrorVar);
/**
 *   Create one or more zones out of the currently defined streamtraces. The new zones have the same
 *   number of variables per data point as the other zones in the data set with all non-coordinate
 *   variables interpolated at the positions along the streamtrace.
 *
 * @param ConcatenateStreams
 *   Set to TRUE to create a single zone out of all common streamtraces. The cell that connects the end
 *   of one streamtrace with the beginning of the next can later be turned off using value-blanking
 *
 * @return
 *   TRUE if the zone could be created, FALSE if not.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateStreamZones(ConcatenateStreams)
 *    INTEGER*4 ConcatenateStreams
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataCreate
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateStreamZones(Boolean_t ConcatenateStreams);
/**
 *   Create finite element surface zones out of iso-surfaces in volume zones. One zone will be made for
 *   each contour level.
 *
 * @return
 *   TRUE if successfull, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateIsoZones()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataCreate
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateIsoZones(void);
/**
 *   Create surface zones out of all slices currently defined in volume zones. One zone is made for
 *   each defined slice.
 *
 * @return
 *   TRUE if the zone is created successfully, otherwise FALSE.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateSliceZones()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataCreate
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateSliceZones(void);
/**
 *   Create zones from contour lines. Contour lines will only come from those zones with a contour
 *   style set to contour lines or lines and flood. One zone will be made from each contour level. The
 *   resulting zones are of type FE-Triangle.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateContourLineZones()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataCreate
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateContourLineZones(void);
/**
 *   Create zones from contour lines.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_CONTLINECREATEMODE
 * Type:
 *   ContCreateMode_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   ContLineCreateMode_OneZonePerContourLevel
 * Required:
 *   No
 * Notes:
 *   Possible values: ContLineCreateMode_OneZonePerCountourLevel or ContLineCreateMode_OneZonePerIndependentPolyline.
 * </ArgListTable>
 *
 * Name:
 *   SV_AUTOSTRANDTRANSIENTDATA
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Boolean value which instructs Tecplot to automatically assign strand IDs to data extracted
 *   from transient sources.
 *
 * @return
 *   TRUE if create zone successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateContourLineZonesX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Create zones from contour lines.
 *
 * @code
 *   ArgList_pa ArgList;
 *
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *
 *   TecUtilArgListAppendInt(ArgList, SV_CONTLINECREATEMODE, (LgIndex_t)ContLineCreateMode_OneZonePerContourLevel);
 *
 *   TecUtilCreateContourLineZonesX(ArgList);
 *
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup DataCreate
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateContourLineZonesX(ArgList_pa ArgList);
/**
 * Finite element volume boundaries cannot be turned on or off using the edge
 * plot layer in Tecplot. You can, however, create a separate zone which is a
 * FE surface zone of the volume boundary. This new zone can then be turned on
 * or off. One requirement for this function to work correctly is that adjacent
 * cells must share the same node points along their common boundary.
 *
 * @param SourceZone
 *   Zone to use. This must be a finite-element zone.
 *
 * @param RemoveBlankedSurfaces
 *   If blanking is turned on, do not include faces of cells that border on blanked regions in the set
 *   of faces that become the resulting finite-element boundary zone.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateFEBoundary(
 *   &                   SourceZone,
 *   &                   RemoveBlankedSurfaces)
 *    INTEGER*4       SourceZone
 *    INTEGER*4       RemoveBlankedSurfaces
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Create a separate zone which is the boundary of the first zone:
 *
 * @code
 *    TecUtilCreateFEBoundary(1,TRUE);
 *   // zone 1 must be finite element
 * @endcode
 *
 * @ingroup DataCreate
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateFEBoundary(EntIndex_t SourceZone,
                                                      Boolean_t  RemoveBlankedSurfaces);



/**
 * @since 11.2-0-200
 * Allow activation of arbitrary slice identifier (i.e. The translucent slicing plane) when preparing to extract.
 * Typical use is to call with TRUE on launch of dialog used to extract arb slices and call with FALSE on dialog drop.
 *
 * @param DoShow TRUE to allow arbitrary slice identifier. FALSE, otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataCreate
 *
 */
LINKTOADDON void STDCALL TecUtilCreateSliceZoneShowTrace(Boolean_t DoShow);


/**
 * @deprecated
 *   Please use TecUtilCreateSliceZoneFromPlane() instead.
 *
 * @ingroup DataCreate
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateSliceZone(double OriginX,
                                                     double OriginY,
                                                     double OriginZ,
                                                     double NormalX,
                                                     double NormalY,
                                                     double NormalZ);




/**
 *   Create a new zone as a slice through the currently active zones that match the specified slice
 *   source and are intersected by a slice plane having the specified origin and normal.
 *
 *   Slicing data while in 2D may yield different results than slicing in 3D.  In 2D all
 *   Z-values are treated as zero, in which case there may be collapsed cells which are not
 *   sliced.  This is especially true for volume data.
 *   
 * @param SliceSource
 *   Source for slicing. The possible choices
 *   are:SliceSource_SurfaceZones, SliceSource_VolumeZones, and SliceSource_SurfacesOfVolumeZones
 *
 * @param OriginX
 *   X-Coordinate for the slicing plane origin.
 *
 * @param OriginY
 *   Y-Coordinate for the slicing plane origin.
 *
 * @param OriginZ
 *   Z-Coordinate for the slicing plane origin.
 *
 * @param NormalX
 *   X-Component for the vector normal to the slicing plane
 *
 * @param NormalY
 *   Y-Component for the vector normal to the slicing plane
 *
 * @param NormalZ
 *   Z-Component for the vector normal to the slicing plane
 *
 * @return
 *   TRUE if the zone is created successfully, FALSE if not.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateSliceZoneFromPlane(
 *   &                   SliceSource,
 *   &                   OriginX,
 *   &                   OriginY,
 *   &                   OriginZ,
 *   &                   NormalX,
 *   &                   NormalY,
 *   &                   NormalZ)
 *    INTEGER*4       SliceSource
 *    REAL*8          OriginX
 *    REAL*8          OriginY
 *    REAL*8          OriginZ
 *    REAL*8          NormalX
 *    REAL*8          NormalY
 *    REAL*8          NormalZ
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Slice 3-D volume zones with a the plane X=2.5:
 *
 * @code
 *   TecUtilCreateSliceZoneFromPlane(SliceSource_VolumeZones,
 *                                   2.5, 0.0, 0.0,
 *                                   1.0, 0.0, 0.0);
 * @endcode
 *
 * @ingroup DataCreate
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateSliceZoneFromPlane(SliceSource_e SliceSource,
                                                              double        OriginX,
                                                              double        OriginY,
                                                              double        OriginZ,
                                                              double        NormalX,
                                                              double        NormalY,
                                                              double        NormalZ);
/**
 *   Create a zone from a slice taken from a 2D or 3D plot.
 *
 *   Slicing data while in 2D may yield different results than slicing in 3D.  In 2D all
 *   Z-values are treated as zero, in which case there may be collapsed cells which are not
 *   sliced.  This is especially true for volume data.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_SLICESOURCE
 * Type:
 *   SliceSource_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   SliceSource_VolumeZones
 * Required:
 *   No
 *
 * Name:
 *   SV_FORCEEXTRACTIONTOSINGLEZONE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Boolean value which forces Tecplot to make the taken slice into a single
 *   zone if made to be TRUE.  If FALSE, one zone per contiguous region is created.
 *
 * Name:
 *   SV_COPYCELLCENTEREDVALUES
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   If TRUE, cell-center values will be copied when possible to the extracted slice plane.
 *   Cell-centers are copied when a variable is cell-centered for all the source zones
 *   through which the slice passes. Otherwise, extracted planes use node-centered data,
 *   which is calculated by interpolation.
 *
 * Name:
 *   SV_AUTOSTRANDTRANSIENTDATA
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Boolean value which instructs Tecplot to automatically assign strand IDs to data extracted
 *   from transient sources.
 *
 * Name:
 *   SV_ORIGINX
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   Sets the origin values of the created slice zone
 *
 * Name:
 *   SV_ORIGINY
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 *
 * Name:
 *   SV_ORIGINZ
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 *
 * Name:
 *   SV_NORMALX
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   Sets the normal values of the created slice zone
 *
 * Name:
 *   SV_NORMALY
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 *
 * Name:
 *   SV_NORMALZ
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * </ArgListTable>
 *
 * @return
 *   TRUE if extraction was successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateSliceZoneFromPlneX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *
 *   TecUtilArgListAppendInt(ArgList,    SV_SLICESOURCE,                 (LgIndex_t)SliceSource_SurfaceZones);
 *   TecUtilArgListAppendInt(ArgList,    SV_FORCEEXTRACTIONTOSINGLEZONE, (LgIndex_t)TRUE);
 *   TecUtilArgListAppendInt(ArgList,    SV_COPYCELLCENTEREDVALUES,      (LgIndex_t)TRUE);
 *   TecUtilArgListAppendDouble(ArgList, SV_ORIGINX,                     0.0);
 *   TecUtilArgListAppendDouble(ArgList, SV_ORIGINY,                     0.0);
 *   TecUtilArgListAppendDouble(ArgList, SV_ORIGINZ,                     0.0);
 *   TecUtilArgListAppendDouble(ArgList, SV_NORMALX,                     0.9);
 *   TecUtilArgListAppendDouble(ArgList, SV_NORMALY,                     0.9);
 *   TecUtilArgListAppendDouble(ArgList, SV_NORMALZ,                     0.9);
 *
 *   TecUtilCreateSliceZoneFromPlneX(ArgList);
 *
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup DataCreate
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateSliceZoneFromPlneX(ArgList_pa ArgList);


/**
 *   Extract data from a 2-D or 3-D field plot
 *
 * @param PolylineXPts_Array
 *   X-array defining the polyline used to extract the data.
 *
 * @param PolylineYPts_Array
 *   Y-array defining the polyline used to extract the data.
 *
 * @param PolylineZPts_Array
 *   Z-array defining the polyline used to extract the data.
 *
 * @param NumPtsInPolyline
 *   Number of points in the supplied polyline
 *
 * @param ExtractThroughVolume
 *   If this is TRUE and the current frame mode is 3D then this will extract data from within any
 *   active volume zones in the data set. If the frame mode is 3D and this is FALSE then a projection
 *   is made from each extraction point onto the nearest surface away from your eye with respect to
 *   the eye coordinate system. If the frame mode is 2D then this parameter is ignored
 *
 * @param ExtractOnlyPointsOnPolyline
 *   Extract only from the points that define the polyline
 *
 * @param IncludeDistanceVariable
 *   Include the distance variable in the resulting extracted data. This is only available if
 *   ExtractToFile is TRUE.
 *
 * @param NumPtsToExtractAlongPolyline
 *   Number of points to evenly distribute along the polyline. Data is extracted from these points if
 *   ExtractOnlyPointsOnPolyline is FALSE
 *
 * @param ExtractToFile
 *   If set to TRUE the data is sent to the file ExtractFName in the form of a valid Tecplot ASCII data
 *   file. If FALSE then a new zone is created within Tecplot
 *
 * @param ExtractFName
 *   Name of the file where extracted data is sent if ExtractToFile is TRUE
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>VALID_REF(PolylineXPts)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(PolylineYPts)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(PolylineZPts)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilExtractFromPolyline(
 *   &                   PolylineXPts_Array,
 *   &                   PolylineYPts_Array,
 *   &                   PolylineZPts_Array,
 *   &                   NumPtsInPolyline,
 *   &                   ExtractThroughVolume,
 *   &                   ExtractOnlyPointsOnPolyline,
 *   &                   IncludeDistanceVariable,
 *   &                   NumPtsToExtractAlongPolyline,
 *   &                   ExtractToFile,
 *   &                   ExtractFName)
 *    REAL*8          PolylineXPts_Array
 *    REAL*8          PolylineYPts_Array
 *    REAL*8          PolylineZPts_Array
 *    INTEGER*4       NumPtsInPolyline
 *    INTEGER*4       ExtractThroughVolume
 *    INTEGER*4       ExtractOnlyPointsOnPolyline
 *    INTEGER*4       IncludeDistanceVariable
 *    INTEGER*4       NumPtsToExtractAlongPolyline
 *    INTEGER*4       ExtractToFile
 *    CHARACTER*(*)   ExtractFName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Extract ten points from specific locations in a field plot. Create a zone with the extracted data:
 *
 * @code
 *   // raw data
 *   double X[10] = { 0, 1, 2, 3, 3, 4, 4, 3, 4, 5 };
 *   double Y[10] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
 *   double Z[10] = { 4, 3, 2, 5, 3, 1, 2, 3, 4, 3 };
 *
 *   TecUtilExtractFromPolyline(X,Y,Z,10,
 *                             TRUE,TRUE,FALSE,10,FALSE,NULL);
 * @endcode
 *
 * @ingroup DataCreate
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilExtractFromPolyline(const double *PolylineXPts_Array,
                                                         const double *PolylineYPts_Array,
                                                         const double *PolylineZPts_Array,
                                                         LgIndex_t     NumPtsInPolyline,
                                                         Boolean_t     ExtractThroughVolume,
                                                         Boolean_t     ExtractOnlyPointsOnPolyline,
                                                         Boolean_t     IncludeDistanceVariable,
                                                         LgIndex_t     NumPtsToExtractAlongPolyline,
                                                         Boolean_t     ExtractToFile,
                                                         const char   *ExtractFName);
/**
 *   Extract data from a 2D or 3D field plot. The locations at which to extract the data come from a
 *   polyline geometry that must be picked prior to issuing this command.  When the frame mode is 3D
 *   and the ExtractThroughVolume parameter is false, the coordinates must be provided in the eye
 *   coordinate system, rather than in 3D grid coordinates.
 *
 * @param ExtractOnlyPointsOnPolyline
 *   Extract only from the points that define the polyline
 *
 * @param IncludeDistanceVariable
 *   Include the distance variable in the resulting data extracted. This is only available if
 *   ExtractToFile is TRUE
 *
 * @param NumPtsToExtractAlongPolyline
 *   Number of points to evenly distribute along the polyline. Data is extracted from these points if
 *   ExtractOnlyPointsOnPolyline is FALSE
 *
 * @param ExtractToFile
 *   If set to TRUE the data is sent to the file ExtractFName in the form of a valid Tecplot ASCII data
 *   file. If FALSE then a new zone is created within Tecplot
 *
 * @param ExtractFName
 *   Name of the file where extracted data is sent if ExtractToFile is TRUE.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilExtractFromGeom(
 *   &                   ExtractOnlyPointsOnPolyline,
 *   &                   IncludeDistanceVariable,
 *   &                   NumPtsToExtractAlongPolyline,
 *   &                   ExtractToFile,
 *   &                   ExtractFName)
 *    INTEGER*4       ExtractOnlyPointsOnPolyline
 *    INTEGER*4       IncludeDistanceVariable
 *    INTEGER*4       NumPtsToExtractAlongPolyline
 *    INTEGER*4       ExtractToFile
 *    CHARACTER*(*)   ExtractFName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Extract 20 points from along the currently picked geometry. Send the result to a file called
 *   "extract.dat":
 *
 * @code
 *   TecUtilExtractFromGeom(FALSE,FALSE,20,TRUE,"extract.dat");
 * @endcode
 *
 * @ingroup DataCreate
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilExtractFromGeom(Boolean_t   ExtractOnlyPointsOnPolyline,
                                                     Boolean_t   IncludeDistanceVariable,
                                                     LgIndex_t   NumPtsToExtractAlongPolyline,
                                                     Boolean_t   ExtractToFile,
                                                     const char *ExtractFName);



/**
 * @deprecated
 *   Please use TecUtilTransformCoordinatesX() instead.
 *
 * @ingroup DataManipulation
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t  STDCALL TecUtilPolarToRectangular(Set_pa ZoneSet);

/**
 * Performs a Fourier transform for each dependent variable for each source zone. A new zone
 * containing the resulting frequency, amplitude, and phase variables (three for each dependent
 * variable) is created for each source zone. If the independent variable is non-uniform the
 * resulting frequency is a modification of the original data (see discussion below for the
 * independentVar and obeySourceZoneBlanking parameters). Resulting zones are assigned new time
 * strands using the same groupings as the source zones if they belong to time stands, otherwise
 * the resulting zones are made static.
 *
 * Zones created by the Fourier transform are added with the name "Fourier Transform" followed by
 * the source zone name in parenthesis. Similarly, the three variables created are added with the
 * names, "Frequency", "Amplitude", and "Phase". The frequency variable name is followed by the
 * independent variable name in parenthesis to the -1 power. The amplitude variable name is followed
 * by the dependent variable name in parenthesis. Newly created zones are assigned passive variables
 * for all variables that previously existed in the dataset and all previously existing zones are
 * assigned passive variables for all new variables created by the Fourier transform.
 *
 * @param independentVar
 *     Independent variable used as the frequency domain. If the spacing is non-uniform this
 *     variable is used in conjunction with each dependent variable for interpolation to create a
 *     uniform frequency for the Fourier transform.
 * @param windowFunction
 *     Window function or tapering function applied to the dependent data before performing the
 *     Fourier transform but after performing the non-uniform interpolation. For details see
 *     http://en.wikipedia.org/wiki/Window_function
 * @param dependentVars
 *     Set of dependent variables on which to perform a Fourier transform. The variables must not be
 *     the same as the independent variable. This must not be NULL or an empty set and must be a
 *     sub-set of Tecplot's variables.
 * @param sourceZones
 *     Set of source zones containing the dependent variables on which to perform the Fourier
 *     transform. This must not be NULL or an empty set and must be a sub-set of Tecplot's zones.
 * @param includeConjugates
 *     For purely real numbers the Fourier transform output satisfies the "Hermitian" redundancy
 *     where out[i] is the conjugate of out[n-i]. If includeConjugates is TRUE the conjugates are
 *     included otherwise they are not and approximately half of the output values, n/2+1, is computed.
 * @param obeySourceZoneBlanking
 *     If value blanking is active and this value is TRUE, value blanking is applied to the data
 *     values of both the independent and dependent variables before the data is interpolated for
 *     non-uniformity. If data values eliminated from the independent or dependent variables cause
 *     the data to be non-uniform the values are interpolated appropriately. Additionally, all
 *     blanked data values up to the first non-blanked data value and all blanked data values after
 *     the last non-blanked data value are ignored, providing a mechanism to constrain the domain
 *     over which the Fourier transformation is performed.
 *
 * @return
 *     TRUE if the Fourier transform succeeded, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>independentVar</em>
 *   Must specify a valid variable.
 *
 * @pre <em>dependentVars</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>sourceZones</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>includeConjugates</em>
 *   Value must be TRUE or FALSE.
 *
 * @pre <em>obeySourceZoneBlanking</em>
 *   Value must be TRUE or FALSE.
 *
 *
 * @since
 *   13.0.0.14113
 *
 * @sa TecUtilFourierTransformX
 *
 * @ingroup DataManipulation
 */
LINKTOADDON Boolean_t STDCALL TecUtilFourierTransform(EntIndex_t       independentVar,
                                                      WindowFunction_e windowFunction,
                                                      Set_pa           dependentVars,
                                                      Set_pa           sourceZones,
                                                      Boolean_t        includeConjugates,
                                                      Boolean_t        obeySourceZoneBlanking);

/**
 * Performs a Fourier transform for each dependent variable for each source zone. A new zone
 * containing the resulting frequency, amplitude, and phase variables (three for each dependent
 * variable) is created for each source zone. If the user instructs to replace zones and/or variables,
 * the ones created by the latest Fourier trasform will be used. If the independent variable is 
 * non-uniform the resulting frequency is a modification of the original data (see discussion below 
 * for the independentVar and obeySourceZoneBlanking parameters). Resulting zones are assigned new
 * time strands using the same groupings as the source zones if they belong to time stands, otherwise
 * the resulting zones are made static.
 *
 * Zones created by the Fourier transform are added with the name "Fourier Transform" followed by
 * the source zone name in parenthesis. Similarly, the three variables created are added with the
 * names, "Frequency", "Amplitude", and "Phase". The frequency variable name is followed by the
 * independent variable name in parenthesis to the -1 power. The amplitude variable name is followed
 * by the dependent variable name in parenthesis. Newly created zones are assigned passive variables
 * for all variables that previously existed in the dataset and all previously existing zones are
 * assigned passive variables for all new variables created by the Fourier transform.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_INDEPENDENTVAR
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   Associated value is the independent variable used as the frequency domain. If the spacing is 
 *   non-uniform this variable is used in conjunction with each dependent variable for interpolation
 *   to create a uniform frequency for the Fourier transform.
 *
 * Name:
 *   SV_WINDOWFUNCTION
 * Type:
 *   WindowFunction_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   WindowFunction_Rectangular
 * Required:
 *   No
 * Notes:
 *   Associated value indicates window function or tapering function applied to the dependent data
 *   before performing the Fourier transform but after performing the non-uniform interpolation. For
 *   details see http://en.wikipedia.org/wiki/Window_function
 *
 * Name:
 *   SV_DEPENDENTVARS
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Required:
 *   Yes
 * Notes:
 *   Associated value contains the set of dependent variables on which to perform a Fourier transform.
 *   The variables must not be the same as the independent variable. This must not be NULL or an empty
 *   set and must be a sub-set of Tecplot's variables.
 *
 * Name:
 *   SV_SOURCEZONES
 * Type:
 *   Set_pa
 * Arg Function:
 *  TecUtilArgListAppendSet()
 * Required:
 *   Yes
 * Notes:
 *   Associated value contains the set of source zones containing the dependent variables on which to
 *     perform the Fourier transform. This must not be NULL or an empty set and must be a sub-set of
 *     Tecplot's zones.
 *
 * Name:
 *   SV_INCLUDECONJUGATES
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   For purely real numbers the Fourier transform output satisfies the "Hermitian" redundancy
 *   where out[i] is the conjugate of out[n-i]. If includeConjugates is TRUE the conjugates are
 *   included otherwise they are not and approximately half of the output values, n/2+1, is computed.
 *
 * Name:
 *   SV_OBEYSOURCEZONEBLANKING
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   If value blanking is active and this value is TRUE, value blanking is applied to the data
 *   values of both the independent and dependent variables before the data is interpolated for
 *   non-uniformity. If data values eliminated from the independent or dependent variables cause
 *   the data to be non-uniform the values are interpolated appropriately. Additionally, all
 *   blanked data values up to the first non-blanked data value and all blanked data values after
 *   the last non-blanked data value are ignored, providing a mechanism to constrain the domain
 *   over which the Fourier transformation is performed.
 *
 * Name:
 *   SV_REPLACEMATHINGRESULTZONES
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE 
 * Required:
 *   No
 * Notes:
 *   If this value is TRUE, then the zones created by the latest Fourier transform will be reused.
 *   If no zones have been created previously, then this setting is disregarded and new zones are
 *   created.
 *
 * Name:
 *   SV_REPLACEMATHINGRESULTVARIABLES
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   If this value is TRUE, then the variables created by the latest Fourier transform will be
 *   reused. If no variables have been created previously, then this setting is disregarded and new
 *   variables are created.
 *
 * Name:
 *   SV_FOURIERTRANSFORMRESULTINGZONES
 * Type:
 *   Set_pa*
 * Arg Function:
 *   TecUtilArgListAppendArbParam
 * Required:
 *   No
 * Notes:
 *   Receives the 1-based zone numbers of any new zones added.
 *
 * Name:
 *   SV_FOURIERTRANSFORMRESULTINGVARS
 * Type:
 *   Set_pa*
 * Arg Function:
 *   TecUtilArgListAppendArbParam
 * Required:
 *   No
 * Notes:
 *   Receives the 1-based variable numbers of any new variables added.
 * </ArgListTable>
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more pages.
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>VarIsValid(dataSet, independentVar)</em>
 *   Must specify a valid variable.
 *
 * @pre <em>VALID_REF(dependentVars)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(sourceZones)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_BOOLEAN(includeConjugates)</em>
 *   Value must be TRUE or FALSE.
 *
 * @pre <em>VALID_BOOLEAN(obeySourceZoneBlanking)</em>
 *   Value must be TRUE or FALSE.
 *
 * @pre <em>VALID_BOOLEAN(replaceMatchingResultZones)</em>
 *   Value must be TRUE or FALSE.
 *
 * @pre <em>VALID_BOOLEAN(replaceMatchingResultVariables)</em>
 *   Value must be TRUE or FALSE.
 *
 *
 * @since
 *   13.0.0.15770
 *
 * @sa TecUtilFourierTransform
 *
 * @ingroup DataManipulation
 */
LINKTOADDON Boolean_t STDCALL TecUtilFourierTransformX(ArgList_pa ArgList);

/**
 *   Transform Coordinates.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_CREATENEWVARIABLES
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   If TRUE, the transform creates new variables for the transformed
 *   coordinates.  If FALSE, the transformed coordinates are placed in the
 *   specified variable numbers.  Default is FALSE
 *
 * Name:
 *   SV_THETAVAR
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The variable numbers of the theta and r coordinate variables in polar of
 *   spherical coordinates.  They are required source variable numbers for
 *   polar-to-rectangular transformations.  If SV_CREATENEWVARIABLES is FALSE,
 *   they are required destination variable numbers for the
 *   rectangular-to-polar and rectangular-to-spherical transformation
 *
 * Name:
 *   SV_RVAR
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 *
 * Name:
 *   SV_PSIVAR
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The variable number of the psi variable in spherical coordinates. This is
 *   a required source variable for spherical-to-rectangular coordinate
 *   transformations. If SV_CREATENEWVARIABLES is FALSE, it is a required
 *   destinations variable number for the rectangular-to-spherical coordinate
 *   transformation.
 *
 * Name:
 *   SV_XVAR
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 *  Notes:
 *   The variable numbers of the X and Y coordinate variables in rectangular
 *   coordinates. The are required source variable numbers for the
 *   rectangular-to-polar or rectangular-to-spherical coordinate
 *   transformations.  If SV_CREATENEWVARIABLES is FALSE, they are required
 *   destination variable numbers for the polar-to-rectangular or
 *   spherical-to-rectangular transformations
 *
 * Name:
 *   SV_YVAR
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 *
 * Name:
 *   SV_ZVAR
 * Type:
 *   entIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The variable number of the z-coordinate variable in rectangular
 *   coordinates.  It is required source variable number for the
 *   rectangular-to-spherical coordinate transformation.  If
 *   SV_CREATENEWVARIABLES is FALSE, it is required a required destination
 *   variable number for the spherical-to-polar transformation
 *
 * Name:
 *   SV_ZONELIST
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   The set of zones to operate on.  If not supplied then all active zones will be operated on
 *
 * Name:
 *   SV_ANGLESPEC
 * Type:
 *   ThetaMode_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   ThetaMode_Radians
 * Required:
 *   No
 * Notes:
 *   Units of angle variables (optional).  Possible values are:
 *   ThetaMode_Degrees, ThetaMode_Radians, ThetaMode_Arbitrary.
 *
 * Name:
 *   SV_TRANSFORMATION
 * Type:
 *   Transform_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The desired transformation (required). Possible values are:
 *   Transform_PolarToRect, Transform_SphericalToRect, Transform_RectToPolar,
 *   Transform_RectToSpherical.
 * </ArgListTable>
 *
 * @return
 *   TRUE if transformation was successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTransformCoordinatesX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Transform spherical coordinate variables (varibles 1, 2 and 3) to rectangular (cartesian)
 *   coordinate variables.
 *
 * @code
 *   TecUtilLockStart(AddOnID);
 *   ArgList_pa ArgList;
 *   Set_pa ZoneSet = TecUtilSetAlloc(TRUE);
 *   TecUtilSetAddMember(ZoneSet, 1, TRUE);
 *
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList, SV_THETAVAR,       1);
 *   TecUtilArgListAppendInt(ArgList, SV_RVAR,           2);
 *   TecUtilArgListAppendInt(ArgList, SV_PSIVAR,         3);
 *   TecUtilArgListAppendInt(ArgList, SV_XVAR,           4);
 *   TecUtilArgListAppendInt(ArgList, SV_YVAR,           5);
 *   TecUtilArgListAppendInt(ArgList, SV_ZVAR,           6);
 *   TecUtilArgListAppendSet(ArgList, SV_ZONELIST,       ZoneSet);
 *   TecUtilArgListAppendInt(ArgList, SV_ANGLESPEC,      ThetaMode_Arbitrary);
 *   TecUtilArgListAppendInt(ArgList, SV_TRANSFORMATION, Transform_SphericalToRect);
 *
 *   TecUtilTransformCoordinatesX(ArgList);
 *
 *   TecUtilArgListDealloc(&ArgList);
 *
 *   TecUtilSetDealloc(&ZoneSet);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup DataManipulation
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilTransformCoordinatesX(ArgList_pa ArgList);




/**
 * @deprecated
 *   Please use TecUtilDataRotate2D() instead.
 *
 * @ingroup View
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t  STDCALL TecUtilRotate2D(Set_pa ZoneSet,
                                               double RotateAmountInDegrees,
                                               double XOrigin,
                                               double YOrigin);
/**
 *   Rotate field data in 2-D about any point.
 *
 * @param ZoneSet
 *   Zones to rotate
 *
 * @param RotateAmountInDegrees
 *   Angle of rotation in degrees
 *
 * @param XOrigin
 *   X-origin about which to rotate
 *
 * @param YOrigin
 *   Y-origin about which to rotate
 *
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>ZoneSet</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataRotate2D(
 *   &                   ZoneSetPtr,
 *   &                   RotateAmountInDegrees,
 *   &                   XOrigin,
 *   &                   YOrigin)
 *    POINTER         (ZoneSetPtr, ZoneSet)
 *    REAL*8          RotateAmountInDegrees
 *    REAL*8          XOrigin
 *    REAL*8          YOrigin
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataManipulation
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilDataRotate2D(Set_pa ZoneSet,
                                                   double RotateAmountInDegrees,
                                                   double XOrigin,
                                                   double YOrigin);

/**
 *   Most plotting in Tecplot is based on using values at the corners (nodes)
 *   of each cell. If you have some field values that are cell-centered then
 *   you need a way to interpolate those values to cell corners.
 *   TecUtilAverageCellCenterData() is used to interpolate cell centered data
 *   in IJ- or IJK-ordered datasets from the cell-center to the nodes. This
 *   same objective can be accomplished by using one of the interpolation
 *   options in Tecplot but that is usually more involved and requires more
 *   processing time. In preparation for using this function, you must: \n
 *      -# Determine which variables are cell-centered values in your data.
 *      -# Construct the Tecplot input data file as follows;
 *           -# Treat all node data in the usual manner.
 *           -# Assign the cell-centered data to the lowest \n
 *              indexed node of the cell it represents.
 *           -# Assign dummy values (0.0) to all nodes at I=IMAX, J=JMAX,\n
 *              (and K=KMAX, for IJK-ordered data).
 *      -# Load your data into Tecplot and call this function.\n
 *
 *   The data from the lowest indexed corners of each cell are treated as if
 *   they are positioned in the cell center.TecUtilAverageCellCenterData() then
 *   takes these values and replaces the values at the nodes with the new
 *   interpolated values.
 *
 *   If your objective is only to view a flooded contour plot where each cell
 *   is filled with a single color representing the cell-centered value, then
 *   do steps 1 and 2 above and set the contour plot type to Corner.  Only call
 *   this routine if the current frame is in 2D or 3D frame mode.
 *
 * @param ZoneSet
 *   Set of zones to operate on. NULL will do all zones.
 *
 * @param VarSet
 *   Set of variables to shift from the cell center to the nodes. NULL will do all variables not
 *   assigned to the X-, Y- or Z-axis.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 *
 * @pre <em>ZoneSet</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>VarSet</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAverageCellCenterData(
 *   &                   ZoneSetPtr,
 *   &                   VarSetPtr)
 *    POINTER         (ZoneSetPtr, ZoneSet)
 *    POINTER         (VarSetPtr, VarSet)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Average the cell-centered values for variables 4, 7, 8, and 9 in zones 1-3:
 *
 * @code
 *   if (TecUtilFrameGetMode() == Frame_TwoD ||
 *       TecUtilFrameGetMode() == Frame_ThreeD)
 *     {
 *       Set_pa    ZoneSet;
 *       Set_pa    VarSet;
 *       Boolean_t IsOk = FALSE;
 *
 *       ZoneSet = TecUtilSetAlloc(TRUE);
 *       VarSet  = TecUtilSetAlloc(TRUE);
 *       if (ZoneSet && VarSet)
 *         {
 *           TecUtilSetAddMember(ZoneSet,1,TRUE);
 *           TecUtilSetAddMember(ZoneSet,2,TRUE);
 *           TecUtilSetAddMember(ZoneSet,3,TRUE);
 *           TecUtilSetAddMember(VarSet,4,TRUE);
 *           TecUtilSetAddMember(VarSet,7,TRUE);
 *           TecUtilSetAddMember(VarSet,8,TRUE);
 *           TecUtilSetAddMember(VarSet,9,TRUE);
 *           IsOk = TecUtilAverageCellCenterData(ZoneSet,VarSet);
 *           TecUtilSetDealloc(&ZoneSet);
 *           TecUtilSetDealloc(&VarSet);
 *         }
 *     }
 * @endcode
 *
 * @ingroup DataManipulation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAverageCellCenterData(Set_pa ZoneSet,
                                                           Set_pa VarSet);

/**
 *   Interpolate selected variables from a set of source zones to a destination zone using linear
 *   interpolation. The source zones cannot be I-ordered. Values assigned to the destination zone are
 *   equivalent to the results of using the Probe tool in Tecplot. See Section 8.8.3, "Linear
 *   Interpolation," in the Tecplot User's Manual for more information about linear interpolation and
 *   its available options.
 *
 * @param SourceZones
 *   Set of zones used to obtain the field values for interpolation. Use NULL to specify all zones
 *   except DestZone.
 *
 * @param DestZone
 *   Destination zone for interpolation
 *
 * @param VarList
 *   Set of variables to interpolate. Use NULL to specify all variables except those assigned to the
 *   axes
 *
 * @param LinearInterpConst
 *   Constant value to which all points outside the data field are set. Only used if LinearInterpMode
 *   is LinearInterpMode_SetToConst. (Normal default value is 0.0.)
 *
 * @param LinearInterpMode
 *   How to deal with points that are outside the source zones' data field. (Normal default value is
 *   LinearInterpMode_SetToConst.) The possible values are: LinearInterpMode_DontChange (Preserves the
 *   points values). LinearInterpMode_SetToConst (Sets all points to LinearInterpConst).
 *
 * @return
 *   Returns TRUE if the interpolation completes successfully. A FALSE return value indicates that the
 *   user pressed cancel during the interpolation process or that the input parameters are not valid.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>SourceZones</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>IMPLICATION(VarSet != NULL,VALID_REF(VarSet))</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLinearInterpolate(
 *   &                   SourceZonesPtr,
 *   &                   DestZone,
 *   &                   VarListPtr,
 *   &                   LinearInterpConst,
 *   &                   LinearInterpMode)
 *    POINTER         (SourceZonesPtr, SourceZones)
 *    INTEGER*4       DestZone
 *    POINTER         (VarListPtr, VarList)
 *    REAL*8          LinearInterpConst
 *    INTEGER*4       LinearInterpMode
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Interpolate all of the variables (except those assigned to the axes) from zones 1-3 to zone 4
 *   using linear interpolation:
 *
 * @code
 *   Boolean_t IsOk;
 *   Set_pa Zones = TecUtilSetAlloc(TRUE);
 *   TecUtilSetAddMember(Zones, 1, TRUE);
 *   TecUtilSetAddMember(Zones, 2, TRUE);
 *   TecUtilSetAddMember(Zones, 3, TRUE);
 *   IsOk = TecUtilLinearInterpolate(
 *            Zones,
 *            4,
 *            (Set_pa)NULL,
 *            0.0,
 *            LinearInterpMode_SetToConst);
 *   TecUtilSetDealloc(&Zones);
 * @endcode
 *
 * @ingroup DataManipulation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilLinearInterpolate(Set_pa             SourceZones,
                                                       EntIndex_t         DestZone,
                                                       Set_pa             VarList,
                                                       double             LinearInterpConst,
                                                       LinearInterpMode_e LinearInterpMode);

/**
 *   Interpolate selected variables from one or more zones onto a destination zone using the inverse
 *   distance method. See Section 8.8.1, "Inverse-Distance Interpolation," in the Tecplot User's
 *   Manual for more information about inverse distance interpolation and its available options.
 *
 * @param SourceZones
 *   Set of zones used to obtain the field values from for the interpolation. Use NULL to specify all
 *   zones except DestZone.
 *
 * @param DestZone
 *   Destination zone for the interpolation
 *
 * @param VarList
 *   Set of variables to interpolate. Use NULL to specify all variables except those assigned to the
 *   axes
 *
 * @param InvDistExponent
 *   Exponent for the inverse-distance weighting. (Normal default value is 3.5.)
 *
 * @param InvDistMinRadius
 *   Minimum distance used for the inverse-distance weighting. (Normal default value is 0.0.)
 *
 * @param InterpPtSelection
 *   Method for determining which source points to consider for each destination data point. (Normal
 *   default value is PtSelection_OctantN.) The possible values are: PtSelection_All (All points in
 *   the source zone). PtSelection_NearestN (Closest N points to the destination point).
 *   PtSelection_OctantN (Closest N points selected by coordinate-system octants)
 *
 * @param InterpNPoints
 *   Number of source points to consider for each destination data point. Only used if
 *   InterpPtSelection is PtSelection_NearestN or PtSelection_OctantN. (Normal default value is
 *   eight.) Must be greater than zero
 *
 * @return
 *   Returns TRUE if the interpolation completes successfully. A FALSE return value indicates that the
 *   user pressed cancel during the interpolation process or that the input parameters are not valid.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>SourceZones</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>IMPLICATION(VarSet != NULL,VALID_REF(VarSet))</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilInverseDistInterpolation(
 *   &                   SourceZonesPtr,
 *   &                   DestZone,
 *   &                   VarListPtr,
 *   &                   InvDistExponent,
 *   &                   InvDistMinRadius,
 *   &                   InterpPtSelection,
 *   &                   InterpNPoints)
 *    POINTER         (SourceZonesPtr, SourceZones)
 *    INTEGER*4       DestZone
 *    POINTER         (VarListPtr, VarList)
 *    REAL*8          InvDistExponent
 *    REAL*8          InvDistMinRadius
 *    INTEGER*4       InterpPtSelection
 *    INTEGER*4       InterpNPoints
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Interpolate all of the variables (except those assigned to the axes) from zones 1-3 to zone 4
 *   using inverse-distance.
 *
 * @code
 *   Boolean_t IsOk;
 *   Set_pa Zones = TecUtilSetAlloc(TRUE);
 *   TecUtilSetAddMember(Zones, 1, TRUE);
 *   TecUtilSetAddMember(Zones, 2, TRUE);
 *   TecUtilSetAddMember(Zones, 3, TRUE);
 *   IsOk = TecUtilInverseDistInterpolation(
 *            Zones,
 *            4,
 *            (Set_pa)NULL,
 *            3.5,
 *            0.0,
 *            PtSelection_OctantN,
 *            8);
 *   TecUtilSetDealloc(&Zones);
 * @endcode
 *
 * @ingroup DataManipulation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilInverseDistInterpolation(Set_pa          SourceZones,
                                                              EntIndex_t      DestZone,
                                                              Set_pa          VarList,
                                                              double          InvDistExponent,
                                                              double          InvDistMinRadius,
                                                              PtSelection_e   InterpPtSelection,
                                                              LgIndex_t       InterpNPoints);




/**
 *   Interpolate selected variables from a set of source zones to a destination zone using the kriging
 *   method. See Section 8.8.2, "Kriging," in the Tecplot User's Manual for more information about
 *   kriging and its available options.
 *
 * @param SourceZones
 *   Set of zones used to obtain the field values for interpolation. Use NULL to specify all zones
 *   except DestZone
 *
 * @param DestZone
 *   Destination zone for interpolation
 *
 * @param VarList
 *   Set of variables to interpolate. Use NULL to specify all variables except those assigned to the
 *   axes
 *
 * @param KrigRange
 *   Distance beyond which source points become insignificant. (Normal default value is 0.3.) Must be
 *   between zero and one, inclusive
 *
 * @param KrigZeroValue
 *   Semi-variance at each source data point on a normalized scale from zero to one. (Normal default
 *   value is 0.0.)
 *
 * @param KrigDrift
 *   Overall trend for the data. (Normal default value is Drift_Linear.) The possible values are:
 *   Drift_None (No trend). Drift_Linear (Linear trend). Drift_Quad (Quadratic trend).
 *
 * @param InterpPtSelection
 *   Method for determining which source points to consider for each destination data point. The
 *   possible values are: PtSelection_All (All points in the source zone). PtSelection_NearestN
 *   (Closest N points to the destination point). PtSelection_OctantN (Closest N points selected by
 *   coordinate-system octants)
 *
 * @param InterpNPoints
 *   Number of source points to consider for each destination data point. Only used if
 *   InterpPtSelection is PtSelection_NearestN or PtSelection_OctantN. (Normal default value is
 *   eight.) Must be greater than zero
 *
 * @return
 *   Returns TRUE if the interpolation completes successfully. A FALSE return value indicates that the
 *   user pressed cancel during the interpolation process or that the input parameters are not valid.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>SourceZones</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>IMPLICATION(VarSet != NULL,VALID_REF(VarSet))</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilKrig(
 *   &                   SourceZonesPtr,
 *   &                   DestZone,
 *   &                   VarListPtr,
 *   &                   KrigRange,
 *   &                   KrigZeroValue,
 *   &                   KrigDrift,
 *   &                   InterpPtSelection,
 *   &                   InterpNPoints)
 *    POINTER         (SourceZonesPtr, SourceZones)
 *    INTEGER*4       DestZone
 *    POINTER         (VarListPtr, VarList)
 *    REAL*8          KrigRange
 *    REAL*8          KrigZeroValue
 *    INTEGER*4       KrigDrift
 *    INTEGER*4       InterpPtSelection
 *    INTEGER*4       InterpNPoints
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Interpolate all of the variables (except those assigned to the axes) from zones 1-3 to zone 4
 *   using kriging:
 *
 * @code
 *   Boolean_t IsOk;
 *   Set_pa Zones = TecUtilSetAlloc(TRUE);
 *   TecUtilSetAddMember(Zones, 1, TRUE);
 *   TecUtilSetAddMember(Zones, 2, TRUE);
 *   TecUtilSetAddMember(Zones, 3, TRUE);
 *   IsOk = TecUtilKrig(
 *            Zones,
 *            4,
 *            (Set_pa)NULL,
 *            0.3,
 *            0.0,
 *            Drift_Linear,
 *            PtSelection_OctantN,
 *            8);
 *   TecUtilSetDealloc(&Zones);
 * @endcode
 *
 * @ingroup DataManipulation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilKrig(Set_pa         SourceZones,
                                          EntIndex_t     DestZone,
                                          Set_pa         VarList,
                                          double         KrigRange,
                                          double         KrigZeroValue,
                                          Drift_e        KrigDrift,
                                          PtSelection_e  InterpPtSelection,
                                          LgIndex_t      InterpNPoints);
/**
 *   Create a new zone by forming triangles from data points in existing zones.
 *
 * @param SourceZones
 *   Set of zones to triangulate
 *
 * @param DoBoundary
 *   If TRUE, BoundaryZones must specify one or more I-ordered zones that define the boundaries across
 *   which no triangles can be created
 *
 * @param BoundaryZones
 *   Set of zones for DoBoundary. Required if DoBoundary is TRUE, ignored otherwise
 *
 * @param IncludeBoundaryPts
 *   TRUE if you also want the boundary points to be used to create triangles
 *
 * @param NumCoincidentPts
 *   Returns the number of coincident points
 *
 * @param TriangleKeepFactor
 *   A number between zero and 0.5. The smaller the number, the more likely it will be that highly
 *   obtuse triangles will be created opening toward the outside of the triangulated zone
 *
 * @return
 *   TRUE if successful, FALSE otherwise
 *
 *
 * @pre <em>SourceZones</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>BoundaryZones</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>NumCoincidentPts</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTriangulate(
 *   &                   SourceZonesPtr,
 *   &                   DoBoundary,
 *   &                   BoundaryZonesPtr,
 *   &                   IncludeBoundaryPts,
 *   &                   NumCoincidentPts,
 *   &                   TriangleKeepFactor)
 *    POINTER         (SourceZonesPtr, SourceZones)
 *    INTEGER*4       DoBoundary
 *    POINTER         (BoundaryZonesPtr, BoundaryZones)
 *    INTEGER*4       IncludeBoundaryPts
 *    INTEGER*4       NumCoincidentPts
 *    REAL*8          TriangleKeepFactor
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a zone by triangulating data points from zones 1 and 2:
 *
 * @code
 *   LgIndex_t NumCoincidentPts;
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set,1,FALSE);
 *   TecUtilSetAddMember(set,2,FALSE);
 *   TecUtilTriangulate(set,FALSE,NULL,FALSE,&NumCoincidentPts,0.25);
 *   TecUtilSetDealloc(&set);
 * @endcode
 *
 * @ingroup DataManipulation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilTriangulate(Set_pa            SourceZones,
                                                 Boolean_t         DoBoundary,
                                                 Set_pa            BoundaryZones,
                                                 Boolean_t         IncludeBoundaryPts,
                                                 TP_OUT LgIndex_t* NumCoincidentPts,
                                                 double            TriangleKeepFactor);

/**
 * Animate zones.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_ZONEANIMATIONMODE
 * Type:
 *   ZoneAnimationMode_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   ZoneAnimationMode_StepByNumber
 * Required:
 *   No
 * Notes:
 *   Used to select how zones are animated. If "StepByNumber" then individual
 *   zones are animated by successively activating/deactivating one zone at a
 *   time, skipped by the specified skip value. If "GroupStepByNumber" then
 *   groups of zones are animated by successively activating/deactivating each
 *   zone group. Tecplot uses the skip value to delineate groups of zones. If
 *   "StepByTime" groups of zones by time are animated by successively
 *   activating/deactivating each time group. Tecplot uses the auxiliary zone
 *   data scalar named "Common.Time" to group zones by time.
 *
 * Name:
 *   SV_START
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   These are the start, end and skip for the zones to animate.
 *
 * Name:
 *   SV_END
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 *
 * Name:
 *   SV_SKIP
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 *
 * Name:
 *   SV_CREATEMOVIEFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the animation should be saved to a file
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *
 * Name:
 *   SV_EXPORTFORMAT
 * Type:
 *   ExportFormat_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   The format used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the format used for the export.
 *
 * Name:
 *   SV_ANIMATIONSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The animation speed used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the animation speed used for the export.
 * </ArgListTable>
 *
 * @return
 *   TRUE if animation was successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateZonesX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Animate zones 1-10. Write the results to an AVI file named Animzones.avi.
 *
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList,    SV_EXPORTFORMAT,    ExportFormat_AVI);
 *   TecUtilArgListAppendString(ArgList, SV_FNAME,           "Animzones.avi");
 *   TecUtilArgListAppendInt(ArgList,    SV_START,           1);
 *   TecUtilArgListAppendInt(ArgList,    SV_END,             10);
 *   TecUtilArgListAppendInt(ArgList,    SV_CREATEMOVIEFILE, TRUE);
 *   IsOk = TecUtilAnimateZonesX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Animation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateZonesX(ArgList_pa ArgList);

/**
 * @deprecated
 *   Please use TecUtilAnimateLineMapsX() instead.
 *
 * @ingroup Animation
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateXYMapsX(ArgList_pa ArgList);

/**
 * Animate Line-mapping.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_START
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   These are the start, end and skip for the Line-mappings to animate.
 *
 * Name:
 *   SV_END
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 *
 * Name:
 *   SV_SKIP
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 *
 * Name:
 *   SV_CREATEMOVIEFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the animation should be saved to a file.
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *
 * Name:
 *   SV_EXPORTFORMAT
 * Type:
 *   ExportFormat_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   The format used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the format used for the export.
 *
 * Name:
 *   SV_ANIMATIONSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The animation speed used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the animation speed used for the export.
 * </ArgListTable>
 *
 * @return
 *   TRUE if animation was successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateLineMapsX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Animate Line-maps 5-10. Write the results to an AVI file named Animxy.avi.
 *
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList,    SV_EXPORTFORMAT,    ExportFormat_AVI);
 *   TecUtilArgListAppendString(ArgList, SV_FNAME,           "Animxy.avi");
 *   TecUtilArgListAppendInt(ArgList,    SV_START,           5);
 *   TecUtilArgListAppendInt(ArgList,    SV_END,             10);
 *   TecUtilArgListAppendInt(ArgList,    SV_CREATEMOVIEFILE, TRUE);
 *   IsOk = TecUtilAnimateLineMapsX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Animation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateLineMapsX(ArgList_pa ArgList);




/**
 * Animate contour levels.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_START
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   These are the start, end and skip for the contour levels to animate.
 *
 * Name:
 *   SV_END
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 *
 * Name:
 *   SV_SKIP
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 *
 * Name:
 *   SV_CREATEMOVIEFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the animation should be saved to a file
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *
 * Name:
 *   SV_EXPORTFORMAT
 * Type:
 *   ExportFormat_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   The format used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the format used for the export.
 *
 * Name:
 *   SV_ANIMATIONSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The animation speed used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the animation speed used for the export.
 * </ArgListTable>
 *
 * @return
 *   TRUE if animation was successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateContourLevelsX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Animate contour levels 1-10. Write the results to an AVI file named animc.avi.
 *
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc()
 *   TecUtilArgListAppendInt(ArgList,    SV_EXPORTFORMAT,    ExportFormat_AVI);
 *   TecUtilArgListAppendString(ArgList, SV_FNAME,           "animc.avi");
 *   TecUtilArgListAppendInt(ArgList,    SV_START,           1);
 *   TecUtilArgListAppendInt(ArgList,    SV_END,             10);
 *   TecUtilArgListAppendInt(ArgList,    SV_CREATEMOVIEFILE, TRUE);
 *   IsOk = TecUtilAnimateContourLevelsX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Animation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateContourLevelsX(ArgList_pa ArgList);


/**
 * Animate iso-surfaces.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_STARTVALUE
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 * Notes:
 *   Associated value is the first iso-surface to draw during the animation.
 *
 * Name:
 *   SV_ENDVALUE
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 * Notes:
 *   Associated value is the last iso-surface to draw during the animation.
 *
 * Name:
 *   SV_NUMSTEPS
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   Associated value is the number of iso-surfaces to draw during the animation.
 *
 * Name:
 *   SV_GROUP
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   Associated value is the iso-surface group number on which to perform the animation.
 *
 * Name:
 *   SV_CREATEMOVIEFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the animation should be saved to a file.
 *
 * Name:
 *   SV_LIMITSCREENSPEED
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the MAXSCREENSPEED value should be used.
 *
 * Name:
 *   SV_MAXSCREENSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   12
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the maximum frames per second display during
 *   the animation. This is ignored if CREATEMOVIEFILE is TRUE
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *
 * Name:
 *   SV_EXPORTFORMAT
 * Type:
 *   ExportFormat_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   The format used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the format used for the export.
 *
 * Name:
 *   SV_ANIMATIONSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The animation speed used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the animation speed used for the export.
 * </ArgListTable>
 *
 * @return
 *   TRUE if animation was successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateIsoSurfacesX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Make an animation with two cycles and ten steps per cycle. Create an AVI movie file.
 *
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList,    SV_EXPORTFORMAT,     ExportFormat_AVI);
 *   TecUtilArgListAppendString(ArgList, SV_FNAME,            "Iso.avi");
 *   TecUtilArgListAppendDouble(ArgList, SV_STARTVALUE,       0.1);
 *   TecUtilArgListAppendDouble(ArgList, SV_ENDVALUE,         2.9);
 *   TecUtilArgListAppendInt(ArgList,    SV_NUMSTEPS,         20);
 *   TecUtilArgListAppendInt(ArgList,    SV_GROUP,            1);
 *   TecUtilArgListAppendDouble(ArgList, SV_ANIMATIONSPEED,   15.3);
 *   TecUtilArgListAppendInt(ArgList,    SV_CREATEMOVIEFILE,  TRUE);
 *   TecUtilAnimateIsoSurfacesX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Animation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateIsoSurfacesX(ArgList_pa ArgList);



/**
 * Animate Time.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_STARTTIME
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The Solution Time at the first time step for active strands
 * Required:
 *   No
 * Notes:
 *   Associated value is the first solution time to draw during the animation.
 *
 * Name:
 *   SV_ENDTIME
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The Solution Time at the last time step for active strands
 * Required:
 *   No
 * Notes:
 *   Associated value is the last solution time to draw during the animation.
 *
 * Name:
 *   SV_SKIP
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   A value of 1 will show every time step from STARTTIME to ENDTIME. A value of 2
 *   will show every other time step, a value of 3 will show every third time step,
 *   and so on.
 *
 * Name:
 *   SV_CREATEMOVIEFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the animation should be saved to a file.
 *
 * Name:
 *   SV_LIMITSCREENSPEED
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the MAXSCREENSPEED value should be used.
 *
 * Name:
 *   SV_MAXSCREENSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   12
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the maximum frames per second display during
 *   the animation. This is ignored if CREATEMOVIEFILE is TRUE
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *
 * Name:
 *   SV_EXPORTFORMAT
 * Type:
 *   ExportFormat_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   The format used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the format used for the export.
 *
 * Name:
 *   SV_ANIMATIONSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The animation speed used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the animation speed used for the export.
 * </ArgListTable>
 *
 * @return
 *   TRUE if animation was successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateIsoSurfacesX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Make an animation with two cycles and ten steps per cycle. Create an AVI movie file.
 *
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList,    SV_EXPORTFORMAT,     ExportFormat_AVI);
 *   TecUtilArgListAppendString(ArgList, SV_FNAME,            "Time.avi");
 *   TecUtilArgListAppendDouble(ArgList, SV_STARTTIME ,       0.1);
 *   TecUtilArgListAppendDouble(ArgList, SV_ENDTIME,          2.9);
 *   TecUtilArgListAppendInt(ArgList,    SV_SKIP,             1);
 *   TecUtilArgListAppendDouble(ArgList, SV_ANIMATIONSPEED,   15.3);
 *   TecUtilArgListAppendInt(ArgList,    SV_CREATEMOVIEFILE,  TRUE);
 *   TecUtilAnimateTimeX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Animation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateTimeX(ArgList_pa ArgList);



/**
 * Animate I-, J-, or K-planes.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_START
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   These are the start, end and skip for the planes to animate.
 *
 * Name:
 *   SV_END
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 *
 * Name:
 *   SV_SKIP
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 *
 * Name:
 *   SV_PLANES
 * Type:
 *   short
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   `I'
 * Required:
 *   No
 * Notes:
 *   Associated value indicates which plane to animate. Use I, J, or K
 *   (typecast to LgIndex_t)
 *
 * Name:
 *   SV_CREATEMOVIEFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the animation should be saved to a file.
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *
 * Name:
 *   SV_EXPORTFORMAT
 * Type:
 *   ExportFormat_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   The format used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the format used for the export.
 *
 * Name:
 *   SV_ANIMATIONSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The animation speed used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the animation speed used for the export.
 * </ArgListTable>
 *
 * @return
 *   TRUE if animation was successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateIJKPlanesX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Animate J-planes 5-10.
 *
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList,    SV_EXPORTFORMAT,    ExportFormat_AVI);
 *   TecUtilArgListAppendString(ArgList, SV_FNAME,           "IJKPlanes.avi");
 *   TecUtilArgListAppendInt(ArgList,    SV_START,           5);
 *   TecUtilArgListAppendInt(ArgList,    SV_END,             10);
 *   TecUtilArgListAppendInt(ArgList,    SV_SKIP,            1);
 *   TecUtilArgListAppendInt(ArgList,    SV_PLANES,          (LgIndex_t)'J');
 *   TecUtilArgListAppendInt(ArgList,    SV_CREATEMOVIEFILE, TRUE);
 *   TecUtilAnimateIJKPlanesX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Animation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateIJKPlanesX(ArgList_pa ArgList);




/**
 * Animate IJK-blanking.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_IMINFRACT
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.1
 * Required:
 *   No
 * Notes:
 *   Assoc. values are the minimum fractions for blanking at the start of
 *   animation. Actual I-index is minfract*IMAX. This is similiar for J and K.
 *
 * Name:
 *   SV_JMINFRACT
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.1
 * Required:
 *   No
 *
 * Name:
 *   SV_KMINFRACT
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.1
 * Required:
 *   No
 *
 * Name:
 *   SV_IMAXFRACT
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   1.0
 * Required:
 *   No
 * Notes:
 *   Assoc. values are the maximum fractions for blanking at the start of
 *   animation. Actual I-index is maxfract*IMAX. This is similiar for J and K.
 *
 * Name:
 *   SV_JMAXFRACT
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   1.0
 * Required:
 *   No
 *
 * Name:
 *   SV_KMAXFRACT
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   1.0
 * Required:
 *   No
 *
 * Name:
 *   SV_IMINFRACT2
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.8
 * Required:
 *   No
 * Notes:
 *   Assoc. values are the minimum fractions for blanking at the start of
 *   animation. Actual I-index is minfract*IMAX. This is similiar for J and K.
 *
 * Name:
 *   SV_JMINFRACT2
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.8
 * Required:
 *   No
 *
 * Name:
 *   SV_KMINFRACT2
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.8
 * Required:
 *   No
 *
 * Name:
 *   SV_IMAXFRACT2
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   1.0
 * Required:
 *   No
 * Notes:
 *   Assoc. values are the maximum fractions for blanking at the start
 *   of animation. Actual I-index is maxfract*IMAX. This is similiar for J and K.
 *
 * Name:
 *   SV_JMAXFRACT2
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   1.0
 * Required:
 *   No
 *
 * Name:
 *   SV_KMAXFRACT2
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   1.0
 * Required:
 *   No
 *
 * Name:
 *   SV_NUMSTEPS
 * Type:
 *   int
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   Number of intermediate steps for the animation. This parameter is required
 *
 * Name:
 *   SV_CREATEMOVIEFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the animation should be saved to a file
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *
 * Name:
 *   SV_EXPORTFORMAT
 * Type:
 *   ExportFormat_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   The format used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the format used for the export.
 *
 * Name:
 *   SV_ANIMATIONSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The animation speed used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the animation speed used for the export.
 * </ArgListTable>
 *
 * @return
 *   TRUE if animation was successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateIJKBlankingX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Animate IJK-blanking starting with fractions 0, 0.1, 0.1 (the defaults) for
 * the minimum fractions, 1.0, 1.0, 1.0 for the maximum fractions. End with
 * 0.7, 0.7, 0.7 for the minimum fractions and 0.75, 0.75, 0.75 for the maximum
 * fractions. Do animation in ten steps.
 *
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList,    SV_EXPORTFORMAT,   ExportFormat_RM);
 *   TecUtilArgListAppendString(ArgList, SV_FNAME,          "IJKBlanking.rm");
 *   TecUtilArgListAppendDouble(ArgList, SV_IMAXFRACT,      1.0);
 *   TecUtilArgListAppendDouble(ArgList, SV_JMAXFRACT,      1.0);
 *   TecUtilArgListAppendDouble(ArgList, SV_KMAXFRACT,      1.0);
 *   TecUtilArgListAppendDouble(ArgList, SV_IMINFRACT2,     0.7);
 *   TecUtilArgListAppendDouble(ArgList, SV_JMINFRACT2,     0.7);
 *   TecUtilArgListAppendDouble(ArgList, SV_KMINFRACT2,     0.7);
 *   TecUtilArgListAppendDouble(ArgList, SV_IMAXFRACT2,     0.75);
 *   TecUtilArgListAppendDouble(ArgList, SV_JMAXFRACT2,     0.75);
 *   TecUtilArgListAppendDouble(ArgList, SV_KMAXFRACT2,     0.75);
 *   TecUtilArgListAppendInt(ArgList,    SV_NUMSTEPS,       10);
 *   TecUtilArgListAppendInt(ArgList,    SV_CREATEMOVIEFILE,TRUE);
 *   TecUtilAnimateIJKBlankingX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Animation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateIJKBlankingX(ArgList_pa ArgList);



/**
 * Animate streamtraces.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_NUMCYCLES
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   4
 * Required:
 *   No
 * Notes:
 *   Associated value is the number of complete time cycles to animate.
 *
 * Name:
 *   SV_STEPSPERCYCLE
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   10
 * Required:
 *   No
 * Notes:
 *   Associated value is the number of steps per cycle.
 *
 * Name:
 *   SV_CREATEMOVIEFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the animation should be saved to a file.
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *
 * Name:
 *   SV_EXPORTFORMAT
 * Type:
 *   ExportFormat_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   The format used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the format used for the export.
 *
 * Name:
 *   SV_ANIMATIONSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The animation speed used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the animation speed used for the export.
 * </ArgListTable>
 *
 * @return
 *   TRUE if animation was successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateStreamX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Make an animation with two cycles and ten steps per cycle. Create an AVI movie file.
 *
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList,    SV_EXPORTFORMAT,    ExportFormat_AVI);
 *   TecUtilArgListAppendString(ArgList, SV_FNAME,           "Stream.avi");
 *   TecUtilArgListAppendInt(ArgList,    SV_NUMCYCLES,       2);
 *   TecUtilArgListAppendInt(ArgList,    SV_STEPSPERCYCLE,   10);
 *   TecUtilArgListAppendInt(ArgList,    SV_CREATEMOVIEFILE, TRUE);
 *   TecUtilAnimateStreamX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Animation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateStreamX(ArgList_pa ArgList);




/**
 * Animates slices.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_START
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   Start and end indices are based on NumSlices. EndSlice cannot be greater than
 *   NumSlices. In most cases, StartSlice will be 1, and EndSlice will be equal to NumSlices.
 *
 * Name:
 *   SV_END
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   2
 * Required:
 *   No
 *
 * Name:
 *   SV_NUMSLICES
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   2
 * Required:
 *   No
 * Notes:
 *   The number of slices to distribute between the currently defined Slice 1 and Slice 2 locations.
 *
 * Name:
 *   SV_GROUP
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   Associated value is the slice group number on which to perform the animation.
 *
 * Name:
 *   SV_LIMITSCREENSPEED
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the MAXSCREENSPEED value should be used.
 *
 * Name:
 *   SV_MAXSCREENSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   12
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the maximum frames per second display during
 *   the animation. This is ignored if CREATEMOVIEFILE is TRUE
 *
 * Name:
 *   SV_CREATEMOVIEFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the animation should be saved to a file.
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *
 * Name:
 *   SV_EXPORTFORMAT
 * Type:
 *   ExportFormat_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   The format used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the format used for the export.
 *
 * Name:
 *   SV_ANIMATIONSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The animation speed used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the animation speed used for the export.
 * </ArgListTable>
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateSlicesX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Animate 50 slices, using the currently defined Slice 1 and Slice 2 positions
 * as the beginning and end of the animation. Send the animation to a movie
 * file.
 *
 * @code
 *   Boolean_t IsOk = TRUE;
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList,    SV_EXPORTFORMAT,     ExportFormat_AVI);
 *   TecUtilArgListAppendString(ArgList, SV_FNAME,            "AnimateSlice.avi");
 *   TecUtilArgListAppendInt(ArgList,    SV_START,            1);
 *   TecUtilArgListAppendInt(ArgList,    SV_END,              50);
 *   TecUtilArgListAppendInt(ArgList,    SV_NUMSLICES,        50);
 *   TecUtilArgListAppendInt(ArgList,    SV_GROUP,            1);
 *   TecUtilArgListAppendDouble(ArgList, SV_ANIMATIONSPEED,   15.3);
 *   TecUtilArgListAppendInt(ArgList,    SV_CREATEMOVIEFILE,  TRUE);
 *   IsOk = TecUtilAnimateSlicesX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Animation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateSlicesX(ArgList_pa ArgList);

/**
 *   Produce an animation showing one zone at a time.
 *
 * @param StartZone
 *   Starting zone for the animation.
 *
 * @param EndZone
 *   Ending zone for the animation.
 *
 * @param ZoneSkip
 *   Zone skip for the animation.
 *
 * @param CreateMovieFile
 *   Set to TRUE if you want Tecplot to create a movie file.
 *
 * @param MovieFName
 *   Name of the file to create if CreateMovieFile is TRUE.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateZones(
 *   &                   StartZone,
 *   &                   EndZone,
 *   &                   ZoneSkip,
 *   &                   CreateMovieFile,
 *   &                   MovieFName)
 *    INTEGER*4       StartZone
 *    INTEGER*4       EndZone
 *    INTEGER*4       ZoneSkip
 *    INTEGER*4       CreateMovieFile
 *    CHARACTER*(*)   MovieFName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create an animation showing (in sequence) zones 2, 4, 6, 8, and 10:
 *
 * @code
 *   TecUtilAnimateZones(2, 10, 2, FALSE, NULL);
 * @endcode
 *
 * @ingroup Animation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateZones(EntIndex_t  StartZone,
                                                  EntIndex_t  EndZone,
                                                  EntIndex_t  ZoneSkip,
                                                  Boolean_t   CreateMovieFile,
                                                  const char *MovieFName);



/**
 * @deprecated
 *   Please use TecUtilAnimateLineMapsX() instead.
 *
 * @ingroup Animation
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateXYMaps(EntIndex_t  StartMap,
                                                   EntIndex_t  EndMap,
                                                   EntIndex_t  MapSkip,
                                                   Boolean_t   CreateMovieFile,
                                                   const char *MovieFName);



/**
 *   Produce an animation of a contour line plot by showing a single level at a time. The animation
 *   varies according to the currently defined contour levels.
 *
 * @param StartLevel
 *   The starting contour level for the animation.
 *
 * @param EndLevel
 *   The ending contour levels for the animation.
 *
 * @param LevelSkip
 *   The level skip.
 *
 * @param CreateMovieFile
 *   Set to TRUE to create a Raster Metafile of the animation.
 *
 * @param MovieFName
 *   String specifying the file name for the movie file. This parameter is used only if CreateMovieFile
 *   is TRUE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateContourLevels(
 *   &                   StartLevel,
 *   &                   EndLevel,
 *   &                   LevelSkip,
 *   &                   CreateMovieFile,
 *   &                   MovieFName)
 *    INTEGER*4       StartLevel
 *    INTEGER*4       EndLevel
 *    INTEGER*4       LevelSkip
 *    INTEGER*4       CreateMovieFile
 *    CHARACTER*(*)   MovieFName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Animate contour levels 1-10:
 *
 * @code
 *   TecUtilAnimateContourLevels(1,10,1,FALSE,(char *)NULL);
 * @endcode
 *
 * @ingroup Animation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateContourLevels(SmInteger_t StartLevel,
                                                          SmInteger_t EndLevel,
                                                          SmInteger_t LevelSkip,
                                                          Boolean_t   CreateMovieFile,
                                                          const char *MovieFName);
/**
 *   Produce an animation that cycles through I-, J-, or K-planes in an IJK-ordered data set.
 *
 * @param IJOrK
 *   This can be set to `I', `J' or `K'
 *
 * @param StartIndex
 *   The starting plane index for the animation.
 *
 * @param EndIndex
 *   The ending plane index for the animation.
 *
 * @param IndexSkip
 *   The plane index skip for the animation.
 *
 * @param CreateMovieFile
 *   Set to TRUE if you want Tecplot to create a movie file.
 *
 * @param MovieFName
 *   Name of the file to create if CreateMovieFile is TRUE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateIJKPlanes(
 *   &                   IJOrK,
 *   &                   StartIndex,
 *   &                   EndIndex,
 *   &                   IndexSkip,
 *   &                   CreateMovieFile,
 *   &                   MovieFName)
 *    CHARACTER*(*)   IJOrK
 *    INTEGER*4       StartIndex
 *    INTEGER*4       EndIndex
 *    INTEGER*4       IndexSkip
 *    INTEGER*4       CreateMovieFile
 *    CHARACTER*(*)   MovieFName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Produce an animation that shows every other J-plane from J=1 to J=11:
 *
 * @code
 *   TecUtilAnimateIJKPlanes('J',
 *                           1,  // Start at J=1
 *                           11, // End at J=11
 *                           2,  // Skip  every other J-Plane
 *                           FALSE,
 *                           (char *)NULL);
 * @endcode
 *
 * @ingroup Animation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateIJKPlanes(char        IJOrK,
                                                      LgIndex_t   StartIndex,
                                                      LgIndex_t   EndIndex,
                                                      LgIndex_t   IndexSkip,
                                                      Boolean_t   CreateMovieFile,
                                                      const char *MovieFName);
/**
 *   Produce an animation of different IJK-blankings in your plot. The animation starts at one
 *   IJK-blanking setting and marches through intermediate steps to a second setting.
 *
 * @param StartIMinFract
 *   Min I-Index fraction for the start of the animation. For example, to start the animation with
 *   blanking in the I direction at I=20 when IMax is 200 set StartIMinFract to 0.10.
 *
 * @param StartJMinFract
 *   Min J-Index fraction for the start of the animation.
 *
 * @param StartKMinFract
 *   Min K-Index fraction for the start of the animation.
 *
 * @param StartIMaxFract
 *   Max I-Index fraction for the start of the animation. For example, to start the animation with
 *   blanking in the I direction at I=20 when IMax is 200 set StartIMinFract to 0.10.
 *
 * @param StartJMaxFract
 *   Max J-Index fraction for the start of the animation.
 *
 * @param StartKMaxFract
 *   Max K-Index fraction for the start of the animation.
 *
 * @param EndIMinFract
 *   Min I-Index fraction for the end of the animation.
 *
 * @param EndJMinFract
 *   Min J-Index fraction for the end of the animation.
 *
 * @param EndKMinFract
 *   Min K-Index fraction for the end of the animation.
 *
 * @param EndIMaxFract
 *   Max I-Index fraction for the end of the animation.
 *
 * @param EndJMaxFract
 *   Max J-Index fraction for the end of the animation.
 *
 * @param EndKMaxFract
 *   Max K-Index fraction for the end of the animation.
 *
 * @param NumSteps
 *   Number of steps or frames to create. The index blanking ranges are linearly interpolated based on
 *   the step and the Start and End fractions.
 *
 * @param CreateMovieFile
 *   Set to TRUE if you want Tecplot to create a movie file.
 *
 * @param MovieFName
 *   Name of the file to create if CreateMovieFile is TRUE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateIJKBlanking(
 *   &                   StartIMinFract,
 *   &                   StartJMinFract,
 *   &                   StartKMinFract,
 *   &                   StartIMaxFract,
 *   &                   StartJMaxFract,
 *   &                   StartKMaxFract,
 *   &                   EndIMinFract,
 *   &                   EndJMinFract,
 *   &                   EndKMinFract,
 *   &                   EndIMaxFract,
 *   &                   EndJMaxFract,
 *   &                   EndKMaxFract,
 *   &                   NumSteps,
 *   &                   CreateMovieFile,
 *   &                   MovieFName)
 *    REAL*8          StartIMinFract
 *    REAL*8          StartJMinFract
 *    REAL*8          StartKMinFract
 *    REAL*8          StartIMaxFract
 *    REAL*8          StartJMaxFract
 *    REAL*8          StartKMaxFract
 *    REAL*8          EndIMinFract
 *    REAL*8          EndJMinFract
 *    REAL*8          EndKMinFract
 *    REAL*8          EndIMaxFract
 *    REAL*8          EndJMaxFract
 *    REAL*8          EndKMaxFract
 *    INTEGER*4       NumSteps
 *    INTEGER*4       CreateMovieFile
 *    CHARACTER*(*)   MovieFName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Produce an animation showing a band of I-planes traversing the entire data field using six steps:
 *
 * @code
 *   TecUtilAnimateIJKBlanking(0.1,0.0,0.0,
 *                             1.0,1.0,1.0,
 *                             1.0,0.0,0.0,
 *                             1.0,1.0,1.0,
 *                             6,
 *                             FALSE,
 *                             (char *)NULL);
 * @endcode
 *
 * @ingroup Animation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateIJKBlanking(double      StartIMinFract,
                                                        double      StartJMinFract,
                                                        double      StartKMinFract,
                                                        double      StartIMaxFract,
                                                        double      StartJMaxFract,
                                                        double      StartKMaxFract,
                                                        double      EndIMinFract,
                                                        double      EndJMinFract,
                                                        double      EndKMinFract,
                                                        double      EndIMaxFract,
                                                        double      EndJMaxFract,
                                                        double      EndKMaxFract,
                                                        int         NumSteps,
                                                        Boolean_t   CreateMovieFile,
                                                        const char *MovieFName);
/**
 *   Produce an animation of stream markers or dashes, moving along the currently defined streamtrace
 *   paths.
 *
 * @param NumStepsPerCycle
 *   This is the number of steps to take to complete one full cycle in time where one cycle is defined
 *   as the time it takes to complete one dash on and one dash off. The larger this number, the
 *   smoother and slower the animation.
 *
 * @param NumCycles
 *   The number of cycles to complete during the animation.
 *
 * @param CreateMovieFile
 *   Set to TRUE if you want Tecplot to create a movie file.
 *
 * @param MovieFName
 *   Name of the file to create if CreateMovieFile is TRUE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateStream(
 *   &                   NumStepsPerCycle,
 *   &                   NumCycles,
 *   &                   CreateMovieFile,
 *   &                   MovieFName)
 *    INTEGER*4       NumStepsPerCycle
 *    INTEGER*4       NumCycles
 *    INTEGER*4       CreateMovieFile
 *    CHARACTER*(*)   MovieFName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Animate streamtraces for three cycles with each cycle using 20 steps:
 *
 * @code
 *   TecUtilAnimateStream(20,3,FALSE,(char *)NULL);
 * @endcode
 *
 * @ingroup Animation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateStream(int         NumStepsPerCycle,
                                                   int         NumCycles,
                                                   Boolean_t   CreateMovieFile,
                                                   const char *MovieFName);
/**
 *   Animates the currently defined 3-D slices.
 *
 * @param StartSlice
 *   Starting slice for the animation. Must be less than or equal to NumSlices. This is most often set
 *   to 1.
 *
 * @param EndSlice
 *   Ending slice for the animation. EndSlice must be greator than or equal to StartSlice.
 *
 * @param NumSlices
 *   The number of slices to distribute between the currently defined Slice 1 and Slice 2 locations.
 *
 * @param CreateMovieFile
 *   Set to TRUE if you want Tecplot to create a movie file.
 *
 * @param MovieFName
 *   Name of the file to create if CreateMovieFile is TRUE.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateSlices(
 *   &                   StartSlice,
 *   &                   EndSlice,
 *   &                   NumSlices,
 *   &                   CreateMovieFile,
 *   &                   MovieFName)
 *    INTEGER*4       StartSlice
 *    INTEGER*4       EndSlice
 *    INTEGER*4       NumSlices
 *    INTEGER*4       CreateMovieFile
 *    CHARACTER*(*)   MovieFName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Animate 50 slices, using the currently defined Slice 1 and Slice 2 positions as the beginning and
 *   end of the animation. Send the animation to a movie file.
 *
 * @code
 *   SmInteger_t NumSlices = 50;
 *             TecUtilAnimateSlices(1,
 *                                  NumSlices,
 *                                  NumSlices,
 *                                  TRUE,
 *                                  "SliceAnimation.rm");
 * @endcode
 *
 * @ingroup Animation
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateSlices(SmInteger_t  StartSlice,
                                                   SmInteger_t  EndSlice,
                                                   SmInteger_t  NumSlices,
                                                   Boolean_t    CreateMovieFile,
                                                   const char  *MovieFName);
/**
 *   Delay Tecplot execution for a specified time.
 *
 * @param Seconds
 *   Number of seconds to delay
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDelay(Seconds)
 *    INTEGER*4 Seconds
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDelay(LgIndex_t Seconds);
/**
 *   Run a macro function. See the Tecplot Reference Manual for details about Tecplot macro functions.
 *
 * @param QuickMacroName
 *   The name of the macro function to run
 *
 * @param MacroParameters
 *   Any parameters which QuickMacroName requires. Pass NULL for macro functions which require no
 *   parameters
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 *
 * @pre <em>QuickMacroName</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>MacroParameters</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroRunFunction(
 *   &                   QuickMacroName,
 *   &                   MacroParameters)
 *    CHARACTER*(*)   QuickMacroName
 *    CHARACTER*(*)   MacroParameters
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Run a macro function called "Calculate" which takes no parameters and another macro function
 *   called "Display" which takes the name of a layout file and an integer as parameters:
 *
 * @code
 *   TecUtilMacroRunFunction("Calculate", (char *)NULL);
 *   TecUtilMacroRunFunction("Display", "(\"contour.lay\", 2)");
 * @endcode
 *
 * @ingroup ScriptSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroRunFunction(const char *QuickMacroName,
                                                      const char *MacroParameters);

/**
 * Set the title for the current data set.
 *
 * @param DataSetTitle
 *   New title for the current frame's data set. The name will be trimmed of any
 *   leading or trailing whitespace and truncated to be no more than 128
 *   characters in length.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 *
 * @pre <em>DataSetTitle</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetSetTitle(DataSetTitle)
 *    CHARACTER*(*) DataSetTitle
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the current frame's data set title to be "Tecplot Data Set #1":
 *
 * @code
 *   TecUtilDataSetSetTitle("Tecplot Data Set #1");
 * @endcode
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetSetTitle(const char *DataSetTitle);

/**
 * Rename a data set variable in Tecplot.
 *
 * @param VarNum
 *   The number of the variable to be renamed. The variable must be greater than zero and the variable
 *   must be enabled
 *
 * @param VarName
 *   A string containing the new variable name. The name will be trimmed of any
 *   leading or trailing whitespace and truncated to be no more than 128
 *   characters in length.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarRename(
 *   &                   VarNum,
 *   &                   VarName)
 *    INTEGER*4       VarNum
 *    CHARACTER*(*)   VarName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Rename the first variable (assumed to be enabled for this example):
 *
 * @code
 *   TecUtilVarRename(1,"NewNameForVariable1");
 * @endcode
 *
 * @ingroup Variables
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilVarRename(EntIndex_t  VarNum,
                                               const char *VarName);

/**
 * Rename a data set variable of the dataset attached to the specified frame in Tecplot.
 *
 * @param FrameID
 *   Unique ID of the frame that is attached to the dataset that should be changed.
 * @param VarNum
 *   The number of the variable to be renamed. The variable must be greater than zero and the variable
 *   must be enabled
 *
 * @param VarName
 *   A string containing the new variable name. The name will be trimmed of any
 *   leading or trailing whitespace and truncated to be no more than 128
 *   characters in length.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarRenameForFrame(
 *   &                   FrameID,
 *   &                   VarNum,
 *   &                   VarName)
 *    INTEGER*4       FrameID 
 *    INTEGER*4       VarNum
 *    CHARACTER*(*)   VarName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Rename the first variable (assumed to be enabled for this example):
 *
 * @code
 *   TecUtilVarRenameForFrame(1,1,"NewNameForVariable1");
 * @endcode
 *
 * @since 14.1
 *
 * @ingroup Variables
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilVarRenameForFrame(UniqueID_t FrameID,
                                                       EntIndex_t  VarNum,
                                                       const char *VarName);

/**
 * Rename a data set zone in Tecplot.
 *
 * @param Zone
 *   The number of the zone to be renamed. The first zone in Tecplot is at position 1
 *
 * @param ZoneName
 *   A string containing the new zone name. The name will be trimmed of any
 *   leading or trailing whitespace and truncated to be no more than 128
 *   characters in length.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneRename(
 *   &                   Zone,
 *   &                   ZoneName)
 *    INTEGER*4       Zone
 *    CHARACTER*(*)   ZoneName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Rename the first zone:
 *
 * @code
 *   TecUtilZoneRename(1,"New Zone Name");
 * @endcode
 *
 * @sa TecUtilZoneRenameForFrame
 *
 * @ingroup Zone
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneRename(EntIndex_t  Zone,
                                                const char *ZoneName);
/**
 * Rename a data set zone of the dataset attached to the specified frame in Tecplot.
 *
 * @param FrameID
 *   Unique ID of the frame that is attached to the dataset that should be changed.
 * @param Zone
 *   The number of the zone to be renamed. The first zone in Tecplot is at position 1
 *
 * @param ZoneName
 *   A string containing the new zone name. The name will be trimmed of any
 *   leading or trailing whitespace and truncated to be no more than 128
 *   characters in length.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneRenameForFrame(
 *   &                   FrameID,
 *   &                   Zone,
 *   &                   ZoneName)
 *    INTEGER*4       FrameID
 *    INTEGER*4       Zone
 *    CHARACTER*(*)   ZoneName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Rename the first zone:
 *
 * @code
 *   TecUtilZoneRenameForFrame(1,1,"New Zone Name");
 * @endcode
 *
 * @since 14.1
 *
 * @ingroup Zone
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneRenameForFrame(UniqueID_t FrameID,
                                                        EntIndex_t Zone,
                                                        const char *ZoneName);
/**
 *   Instruct Tecplot to submit and execute an operating system specific command.
 *
 * @param Command
 *   Operating system specific command to execute
 *
 * @param Wait
 *   If TRUE then Tecplot will wait for the command to finish processing. If FALSE then the command is
 *   run concurrently in another process and Tecplot returns immediately
 *
 * @return
 *   On UNIX platforms, TRUE is returned if there is a shell to execute (such as /bin/ksh). On Windows
 *   platforms, TRUE is returned if the process is successfully created. Otherwise, FALSE.
 *
 *
 * @pre <em>Command</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSystem(
 *   &                   Command,
 *   &                   Wait)
 *    CHARACTER*(*)   Command
 *    INTEGER*4       Wait
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Use the polygrid utility supplied with Tecplot to translate a PolyGrid file to a Tecplot ASCII
 *   input data file.
 *
 * @code
 *   TecUtilSystem("polygrid < sphere.pol > sphere.dat", TRUE);
 * @endcode
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilSystem(const char *Command,
                                            Boolean_t   Wait);

/**
 * @deprecated
 *   This method does nothing and always returns TRUE.
 *
 * @ingroup ScriptSupport
 *
 * #internalattributes exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroPanelAddTitle(const char *Title);

/**
 * Enables or disables a dialog's ability to be launched via a context
 * sensitive double click.
 *
 * @par Note:
 *   Not all dialogs can be launched via a double click. This function is
 *   primarily used for disabling and re-enabling dialogs that can be launched
 *   via a double click.
 *
 * @since
 *   11.0-3-006
 *
 * @param Dialog
 *   Dialog to enable or disable double click launching.
 * @param DoAllow
 *   If the dialog can be launched via a double click this control whether this
 *   ability is enabled or disabled.
 *
 * @sa TecUtilDialogLaunch, TecUtilDialogDrop
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilDialogAllowDoubleClickLaunch(Dialog_e  Dialog,
                                                             Boolean_t DoAllow);
/**
 * Launch a Tecplot interface dialog.
 *
 * @par Note:
 *   This function cannot be called when Tecplot is running in batch mode.
 *
 * @param DialogToLaunch
 *   Dialog to launch. See \ref Dialog_e for list of dialogs that you may launch.
 *
 * @return
 *   For modeless dialogs TRUE is returned if the dialog was launched or FALSE if
 *   the dialog could not be launched programmatically in the given context.
 *   For modal dialogs TRUE is returned, if the dialog was launched and the
 *   action was performed, or FALSE, if the user canceled the operation or the
 *   dialog could not be launched programmatically in the given context.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogLaunch(DialogToLaunch)
 *    INTEGER*4 DialogToLaunch
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Launch the Color Map dialog:
 * @code
 *   TecUtilDialogLaunch(Dialog_ColorMap);
 * @endcode
 *
 * @sa TecUtilDialogDrop
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogLaunch(Dialog_e DialogToLaunch);

/**
 *   Drop a Tecplot interface dialog.
 *
 * @param DialogToDrop
 *   To remove Dialog from the screen. See Dialog in GLOBAL.h (or FGLUE.INC) for possible values.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogDrop(DialogToDrop)
 *    INTEGER*4 DialogToDrop
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Drop the Color Map dialog:
 *
 * @code
 *   TecUtilDialogDrop(Dialog_ColorMap);
 * @endcode
 *
 * @sa TecUtilDialogLaunch
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogDrop(Dialog_e DialogToDrop);

/**
 *   Run a macro file. See the Tecplot Reference Manual for details about Tecplot macro files.
 *
 * @param FName
 *   The name of the macro file to run. This should be the full
 *   path to the macro file. If the full path is not specified,
 *   any relative paths in your macro file may not be correctly resolved.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 *
 * @pre <em>FName</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroRunFile(FName)
 *    CHARACTER*(*) FName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Load and run the macro file mymacro.mcr:
 *
 * @code
 *   TecUtilMacroRunFile("C:\\mymacro.mcr");
 * @endcode
 *
 * @ingroup ScriptSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroRunFile(const char *FName);

/**
 * Allow or disallow the unloading of an add-on on Tecplot exit. Some add-ons
 * have demonstrated crashing problems if unloaded before Tecplot exits. Often
 * this is due to a 3rd party library linked with the add-on that registers
 * with the operating system what is typically called an "atexit" function to
 * automatically perform cleanup operations when the loading program exits if
 * the client neglects to perform them. This is a very bad thing for a
 * dynamically loaded/unloaded library to do. Well written libraries usually
 * provide an API for cleaning up and preventing the registration of an
 * "atexit" function. In the event that a 3rd party library does not provide an
 * API you may have to prevent the add-on from being unloaded by Tecplot with
 * this function.
 *
 * @param AddOnID
 *   ID of the add-on
 *
 * @param DoAllowUnload
 *   Set to FALSE to disable unloading of the add-on on Tecplot exit. Set to
 *   TRUE to allow unloading (the default).
 *
 *
 * @pre <em>AddOnID</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAddOnAllowUnload(
 *   &           AddOnIDPtr,
 *   &           DoAllowUnload)
 *    POINTER         (AddOnIDPtr, AddOnID)
 *    INTEGER*4       DoAllowUnload
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Instruct Tecplot to no unload the add-on, identified by its AddOnID, before
 * Tecplot exits.
 * @code
 *   TecUtilAddOnAllowUnload(AddonID,FALSE);
 * @endcode
 *
 * @ingroup AddOnManagement
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void      STDCALL TecUtilAddOnAllowUnload(AddOn_pa   AddOnID,
                                                      Boolean_t  DoAllowUnload);
/**
 * Load an add-on into Tecplot.
 *
 * @param LibName
 *   Name of add-on file.
 *
 * @param NotUsed
 *   Pass NULL for this parameter.
 *
 * @param NotUsed2
 *   Pass 0 for this parameter.
 *
 * @return
 *   Returns TRUE if the add-on was successfully loaded, otherwise, FALSE.
 *
 *
 * @pre <em>LibName</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAddOnLoad(
 *   &                   LibName,
 *   &                   NotUsed,
 *   &                   NotUsed2)
 *    CHARACTER*(*)   LibName
 *    CHARACTER*(*)   NotUsed
 *    INTEGER*4       NoUsed2
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * To load the add-on cstream use:
 * @code
 *   if (!TecTuilAddOnLoad("cstream", NULL, 0))
 *     {
 *       // failed to load cstream add-on
 *     }
 * @endcode
 *
 * @ingroup AddOnManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAddOnLoad(const char   *LibName,
                                               const char   *NotUsed,
                                               int           NotUsed2);


/**
 *   Checks to see if Tecplot is currently recording an animation.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilExportIsRecording()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Export
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilExportIsRecording(void);
/**
 *   This function assembles all of the exported animation frames into the final animation file. Note
 *   that the final *.avi or *.rm file is not created until you call this function. This function will
 *   assert if there is no current animation.
 *
 * @return
 *   TRUE if Tecplot is currently recording an animation, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilExportFinish()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Export
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilExportFinish(void);

/**
 * This functions starts a new animation and exports the first frame. Use
 * TecUtilExportIsRecording() to make sure an export session is not in progress
 * before calling this function.
 *
 * @return
 *   TRUE if successful, FALSE otherwise. The return value will be FALSE only
 *   if Tecplot is unable to allocate a small amount of memory.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilExportStart()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   This example makes use of the following export functions: \n
 *              TecUtilExportIsRecording() \n
 *              TecUtilExportSetup() \n
 *              TecUtilExportStart() \n
 *              TecUtilExportCancel() \n
 *              TecUtilExportNextFrame() \n
 *              TecUtilExportFinish() \n
 *
 * @code
 *   This example may be used as a template when creating an animation routine.
 *   {
 *     int ii;
 *     Boolean_t IsOk;
 *     TecUtilLockStart(AddOnID);
 *
 *     // Make sure there is no current export session before starting a new one.
 *     if ( TecUtilExportIsRecording() )
 *       {
 *         TecUtilDialogErrMsg("An export session is alread in progress.");
 *         IsOk = FALSE;
 *       }
 *     else
 *       {
 *         // Setup the export format and the file name.
 *         TecUtilExportSetup(SV_EXPORTFNAME, NULL,
 *                            0.0, (ArbParam_t)"export.avi");
 *         TecUtilExportSetup(SV_EXPORTFORMAT, NULL,
 *                            0.0, (ArbParam_t)ExportFormat_AVI);
 *         // Start the export session.
 *         IsOk = TecUtilExportStart();
 *       }
 *
 *     if ( IsOk )
 *       {
 *         for (ii=0; ii<4; ii++)
 *           {
 *             // Rotate the plot around the PSI axis by 5 degrees.
 *             IsOk = TecUtilViewRotate3D(RotateAxis_Psi,
 *                                        5.0,
 *                                        0.0,
 *                                        0.0,
 *                                        0.0,
 *                                        RotateOriginLocation_DefinedOrigin);
 *
 *             // Export the new image.
 *             if ( IsOk )
 *               {
 *                 IsOk = TecUtilExportNextFrame();
 *               }
 *
 *             // If the rotation or the exporting of the next frame
 *             // was not sucessful, abandon the export session.
 *
 *             if ( !IsOk )
 *               {
 *                 TecUtilExportCancel();
 *                 break;
 *               }
 *           }
 *       }
 *     // If everything was successful, finish the export session.
 *     if ( IsOk )
 *       IsOk = TecUtilExportFinish();
 *
 *     if ( !IsOk )
 *       TecUtilDialogErrMsg("Error exporting, file may not be complete.");
 *
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup Export
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilExportStart(void);
/**
 * Export the next frame of a movie animation.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilExportNextFrame()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Export
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilExportNextFrame(void);
/**
 * Cancel a movie export which is in progress. This function is only valid
 * after you have called TecUtilExportStart() but before you have called
 * TecUtilExportFinish(). It ensures that any resources used by the animation
 * are properly released and that no partial files are created or left on the
 * user's drive.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilExportCancel()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Export
 *
 */
LINKTOADDON void STDCALL TecUtilExportCancel(void);

/**
 * Queries the 3-D viewer angles.
 *
 * @param PsiAngle
 *   Receives the Psi angle. May be NULL
 *
 * @param ThetaAngle
 *   Recevies the Theta angle. May be NULL
 *
 * @param AlphaAngle
 *   Receives the Alpha angle. May be NULL
 *
 *
 * @pre <em>PsiAngle</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>ThetaAngle</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>AlphaAngle</em>
 *   Pointer must be a valid address or NULL.
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilThreeDViewGetViewerAngle(
 *   &           PsiAngle,
 *   &           ThetaAngle,
 *   &           AlphaAngle)
 *    REAL*8          PsiAngle
 *    REAL*8          ThetaAngle
 *    REAL*8          AlphaAngle
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Query the 3-D view angles.
 *
 * @code
 *   {
 *     double P,T,A;
 *     TecUtilLockStart(AddOnID);
 *     TecUtilThreeDViewGetAngle(&P,&T,&A);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup View
 *
 */
LINKTOADDON void STDCALL TecUtilThreeDViewGetViewerAngle(TP_OUT double* PsiAngle,
                                                         TP_OUT double* ThetaAngle,
                                                         TP_OUT double* AlphaAngle);
/**
 * Queries the 3-D viewer angles.
 *
 * @param XPos
 *   Receives the viewer X-location
 *
 * @param YPos
 *   Recevies the viewer Y-location
 *
 * @param ZPos
 *   Receives the viewer Z-location
 *
 *
 * @pre <em>XPos</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>YPos</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>ZPos</em>
 *   Pointer must be a valid address or NULL.
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilThreeDViewGetViewerPos(
 *   &           XPos,
 *   &           YPos,
 *   &           ZPos)
 *    REAL*8          XPos
 *    REAL*8          YPos
 *    REAL*8          ZPos
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Query the 3-D viewer position.
 *
 * @code
 *   {
 *     double XPos,YPos,ZPos;
 *     TecUtilLockStart(AddOnID);
 *     TecUtilThreeDViewGetViewerPos(&XPos,&YPos,&ZPos);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup View
 *
 */
LINKTOADDON void STDCALL TecUtilThreeDViewGetViewerPos(TP_OUT double* XPos,
                                                       TP_OUT double* YPos,
                                                       TP_OUT double* ZPos);

/**
 * Queries the distance btween the viewer position and then
 * plane of rotation origin.
 *
 * @pre Must have one or more frames.
 *
 *
 * @return
 *   Distance from the viewer position to the rotation origin plane
 * <FortranSyntax>
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Query the distnace from the viewer position to the rotation
 *   origin plane.
 *
 * @code
 *   {
 *     double DistanceToRotateOriginPlane = TecUtilThreeDViewGetViewerPos(&XPos,&YPos,&ZPos);
 *   }
 * @endcode
 *
 * @since 11.3-17-025
 *
 * @ingroup View
 *
 */
LINKTOADDON double STDCALL TecUtilThreeDViewGetDistanceToRotateOriginPlane(void);

/**
 * Queries the location of the middle plane of view.
 *
 * @pre Must have one or more frames.
 *
 *
 * @return
 *   Location of the middle plane of view.
 * <FortranSyntax>
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 * @endcode
 *
 * @since 11.3-17-025
 *
 * @ingroup View
 *
 */
LINKTOADDON double STDCALL TecUtilThreeDViewGetMidZPlane(void);

/**
 * Queries the location of the minimum and maximum view planes.
 *
 * @param ZMin
 *   Receives the minimum view plane location.
 * @param ZMax
 *   Recevies the maximum view plane location.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>ZMin</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>ZMax</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 * @endcode
 *
 * @since 11.3-17-025
 *
 * @ingroup View
 *
 */
LINKTOADDON void STDCALL TecUtilThreeDViewGetMinMaxPanes(TP_OUT double* ZMin,
                                                         TP_OUT double* ZMax);

/**
 * Queries the 3-D view attributes.
 *
 * @param FieldOfView
 *   Receives the amount of the plot (in terms of spherical arc) in front of the viewer which may be
 *   seen. You may pass NULL for this parameter
 *
 * @param ViewWidth
 *   Receives the amount of the plot (in X-axis units) in front of the viewer
 *   which may be seen. You may pass NULL for this parameter. Zooming in or out
 *   of a 3-D orthographic plot changes this number, but not the viewer's
 *   position.
 *
 * @param IsInPerspective
 *   Receives TRUE if Tecplot is drawing the plot with perspective projection, FALSE otherwise. You may
 *   pass NULL for this parameter
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilThreeDViewGetProjection(
 *   &           FieldOfView,
 *   &           ViewWidth,
 *   &           IsInPerspective)
 *    REAL*8          FieldOfView
 *    REAL*8          ViewWidth
 *    INTEGER*4       IsInPerspective
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Query the 3-D view attributes.
 *
 * @code
 *   {
 *     double FieldOfView,WidthWidth;
 *     Boolean_t IsInPerspective;
 *
 *     TecUtilLockStart(AddOnID);
 *     TecUtilThreeDViewGetProjection(&FieldOfView,
 *    &ViewWidth,
 *    &IsInPerspective);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup View
 *
 */
LINKTOADDON void STDCALL TecUtilThreeDViewGetProjection(TP_OUT double*    FieldOfView,
                                                        TP_OUT double*    ViewWidth,
                                                        TP_OUT Boolean_t* IsInPerspective);

/**
 * Converts from 3-D world coordinates to Tecplot's 3-D grid coordinates. Note
 * that these 3-D grid coordinates are only to be used for zoom and 2-D grid
 * mode text and geometries in 3-D. Converted grid coordinates should be used
 * immediately. This transformation may change in future versions of Tecplot.
 * You must call TecUtilSetupTransformations() before calling this function,
 * although TecUtilSetupTransformations() need only be called once for each
 * sequence of calls to TecUtilConvert3DPositionToGrid().
 *
 * @param XPosition
 *   X-position in world coordinates.
 *
 * @param YPosition
 *   Y-position in world coordinates.
 *
 * @param ZPosition
 *   Z-position in world coordinates.
 *
 * @param XGridPosition
 *   Receives X-grid position. You may pass NULL for this parameter.
 *
 * @param YGridPosition
 *   Receives Y-grid position. You may pass NULL for this parameter.
 *
 * @param ZGridPosition
 *   Receives Z-grid position. You may pass NULL for this parameter.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>XGridPosition</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>YGridPosition</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>ZGridPosition</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilConvert3DPositionToGrid(
 *   &           XPosition,
 *   &           YPosition,
 *   &           ZPosition,
 *   &           XGridPosition,
 *   &           YGridPosition,
 *   &           ZGridPosition)
 *    REAL*8          XPosition
 *    REAL*8          YPosition
 *    REAL*8          ZPosition
 *    REAL*8          XGridPosition
 *    REAL*8          YGridPosition
 *    REAL*8          ZGridPosition
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Convert from 3-D world coordinates to Tecplot's 3-D grid coordinates.
 *
 * @code
 *   {
 *     double ZPlaneFloor;
 *     double X,Y,Z;
 *
 *     TecUtilLockStart(AddOnID);
 *     TecUtilSetupTransformations();
 *     ZPlaneFloor = TecUtil3DViewGetNearZPlane();
 *
 *     TecUtilConvert3DPositionToGrid(1.0,1.0,ZPlaneFloor,&X,&Y,&Z);
 *
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup Axis
 *
 */
LINKTOADDON void STDCALL TecUtilConvert3DPositionToGrid(double         XPosition,
                                                        double         YPosition,
                                                        double         ZPosition,
                                                        TP_OUT double* XGridPosition,
                                                        TP_OUT double* YGridPosition,
                                                        TP_OUT double* ZGridPosition);

/**
 * Converts from Tecplot's 3-D "Grid" or view coordinates to world coordinates.
 * Note that these 3-D grid coordinates are taken from 2-D grid mode text and
 * geometries when the frame is in a 3D view. This transformation may change in
 * future versions of Tecplot. You must call TecUtilSetupTransformations()
 * before calling this function, although TecUtilSetupTransformations() need
 * only be called once for each sequence of calls to
 * TecUtilConvertGridTo3DPosition().
 *
 * @since
 *   11.2-0-093
 *
 * @param XGridPosition
 *   X position in eye coordinates.
 *
 * @param YGridPosition
 *   Y position in eye coordinates.
 *
 * @param ZGridPosition
 *   Z position in eye coordinates.
 *
 * @param XPosition
 *   Receives X position in world coordinates. You may pass NULL for this parameter.
 *
 * @param YPosition
 *   Receives Y position in world coordinates. You may pass NULL for this parameter.
 *
 * @param ZPosition
 *   Receives Z position in world coordinates. You may pass NULL for this parameter.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>XPosition</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>YPosition</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>ZPosition</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilConvertGridTo3DPosition(
 *   &           XGridPosition,
 *   &           YGridPosition,
 *   &           ZGridPosition,
 *   &           XPosition,
 *   &           YPosition,
 *   &           ZPosition)
 *    REAL*8          XGridPosition
 *    REAL*8          YGridPosition
 *    REAL*8          ZGridPosition
 *    REAL*8          XPosition
 *    REAL*8          YPosition
 *    REAL*8          ZPosition
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Convert from Tecplot's 3-D eye (view) coordinates to 3-D world coordinates.
 *
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *
 *     Geom_ID   GeomId  = 0;
 *     double    xView   = 0.0;
 *     double    yView   = 0.0;
 *     double    zView   = 0.0;
 *     double    xPos    = 0.0;
 *     double    yPos    = 0.0;
 *     double    zPos    = 0.0;
 *
 *     TecUtilSetupTransformations();
 *     for (GeomId = TecUtilGeomGetBase();
 *          GeomId != TECUTILBADID;
 *          GeomId = TecUtilGeomGetNext(GeomId))
 *       {
 *          TecUtilGeomGetAnchorPos(geomId, &xView, &yView, &zView);
 *          TecUtilConvertGridTo3DPosition(xView, yView, zView, &xPos, &yPos, &zPos);
 *          ...
 *       }
 *
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup Axis
 *
 */
LINKTOADDON void STDCALL TecUtilConvertGridTo3DPosition(double         XGridPosition,
                                                        double         YGridPosition,
                                                        double         ZGridPosition,
                                                        TP_OUT double* XPosition,
                                                        TP_OUT double* YPosition,
                                                        TP_OUT double* ZPosition);

/**
 * Returns the Z-value below which you can no longer trust any values of
 * TecUtilConvert3DPositionToGrid().
 *
 * @return
 *   Returns the Z-value below which you can no longer trust any values of
 *   TecUtilConvert3DPositionToGrid().
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilThreeDViewGetNearZPlane()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Convert from 3-D world coordinates to Tecplot's 3-D grid coordinates.
 *
 * @code
 *   {
 *     double ZPlaneFloor;
 *     double X,Y,Z;
 *
 *     TecUtilLockStart(AddOnID);
 *     TecUtilSetupTransformations();
 *     ZPlaneFloor = TecUtilThreeDViewGetNearZPlane();
 *
 *     TecUtilConvert3DPositionToGrid(1.0,1.0,ZPlaneFloor,&X,&Y,&Z);
 *
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup View
 *
 */
LINKTOADDON double STDCALL TecUtilThreeDViewGetNearZPlane(void);




/**
 * @deprecated
 *   Please use TecUtilThreeDViewGetNearZPlane() instead.
 *
 * @ingroup View
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON double STDCALL TecUtil3DViewGetNearZPlane(void);




/**
 * Initializes transformation information so that coordinate conversion
 * functions can be used. This function must be called before any sequence of
 * calls to TecUtilConvert3DPositionToGrid().
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilSetupTransformations()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Convert from 3-D world coordinates to Tecplot's 3-D grid coordinates.
 *
 * @code
 *   {
 *     double ZPlaneFloor;
 *     double X,Y,Z;
 *
 *     TecUtilLockStart(AddOnID);
 *     TecUtilSetupTransformations();
 *     ZPlaneFloor = TecUtil3DViewGetNearZPlane();
 *
 *     TecUtilConvert3DPositionToGrid(1.0,1.0,ZPlaneFloor,&X,&Y,&Z);
 *
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON void STDCALL TecUtilSetupTransformations(void);
/**
 * @deprecated
 *   Please use TecUtilFrameLightweightLoopStart() instead.
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON void STDCALL TecUtilFrameLightweightPopStart(void);
/**
 * @deprecated
 *   Please use TecUtilFrameLightweightLoopNext() instead.
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameLightweightPopNext(void);
/**
 * @deprecated
 *   Please use TecUtilFrameLightweightLoopEnd() instead.
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON void STDCALL TecUtilFrameLightweightPopEnd(void);

/**
 * Initialize lightweight frame activation loop. Lightweight frame activations have much less
 * overhead compared to TecUtilFrameActivateByNumber(), but you are more restricted in what
 * you can do after each activation. Thus this function can be used when:
 *
 *   * Speed is important
 *   * You plan to cycle through and activate all of the frames
 *   * You will not do anything which will cause a redraw of the frame after the frame activation.
 *
 *  Call this function before calling TecUtilFrameLightweightLoopNext().
 *
 * @since
 *   11.4-1-1120
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilFrameLightweightLoopStart()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the name of all frames to Hi Mom.
 *
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     TecUtilFrameLightweightLoopStart();
 *     do
 *       {
 *         TecUtilFrameSetName("Hi Mom");
 *       }
 *     while ( TecUtilFrameLightweightLoopNext() );
 *     TecUtilFrameLightweightLoopEnd();
 *     // Don't redraw until TecUtilFrameLightweightLoopEnd() is called
 *     TecUtilRedraw(TRUE);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON void STDCALL TecUtilFrameLightweightLoopStart(void);
/**
 * Activate the next frame. See TecUtilFrameLightweightLoopStart() for more information on
 * lightweight frame activating.
 *
 * @since
 *   11.4-1-1120
 *
 * @return
 *   TRUE if there are more frames to activate, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameLightweightLoopNext()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the name of all frames to Hi Mom.
 *
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     TecUtilFrameLightweightLoopStart();
 *     do
 *       {
 *         TecUtilFrameSetName("Hi Mom");
 *       }
 *     while ( TecUtilFrameLightweightLoopNext() );
 *     TecUtilFrameLightweightLoopEnd();
 *     // Don't redraw until TecUtilFrameLightweightLoopEnd() is called
 *     TecUtilRedraw(TRUE);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameLightweightLoopNext(void);
/**
 * Ends a sequence of lightweight frame activations. See TecUtilFrameLightweightLoopStart()
 * for more information on lightweight frame activating.
 *
 * @since
 *   11.4-1-1120
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilFrameLightweightLoopEnd()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the name of all frames to Hi Mom.
 *
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     TecUtilFrameLightweightLoopStart();
 *     do
 *       {
 *         TecUtilFrameSetName("Hi Mom");
 *       }
 *     while ( TecUtilFrameLightweightLoopNext() );
 *     TecUtilFrameLightweightLoopEnd();
 *     // Don't redraw until TecUtilFrameLightweightLoopEnd() is called
 *     TecUtilRedraw(TRUE);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON void STDCALL TecUtilFrameLightweightLoopEnd(void);

/**
 * Initialize lightweight frame activation loop across all pages. Lightweight frame activations have much less
 * overhead compared to TecUtilFrameActivateByNumber(), but you are more restricted in what
 * you can do after each activation. Thus this function can be used when:
 *
 *   * Speed is important
 *   * You plan to cycle through and activate all of the frames
 *   * You will not do anything which will cause a redraw of the frame after the frame activation.
 *
 *  Call this function before calling TecUtilFrameLightweightForAllPagesLoopNext().
 *
 * @since
 *   14.2
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilFrameLightweightForAllPagesLoopStart()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the name of all frames to Hi Mom.
 *
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     TecUtilFrameLightweightForAllPagesLoopStart();
 *     do
 *       {
 *         TecUtilFrameSetName("Hi Mom");
 *       }
 *     while ( TecUtilFrameLightweightForAllPagesLoopNext() );
 *     TecUtilFrameLightweightForAllPagesLoopEnd();
 *     // Don't redraw until TecUtilFrameLightweightForAllPagesLoopEnd() is called
 *     TecUtilRedraw(TRUE);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON void STDCALL TecUtilFrameLightweightForAllPagesLoopStart(void);

/**
 * Activate the next frame across all pages. See TecUtilFrameLightweightForAllPagesLoopStart() for more information on
 * lightweight frame activating.
 *
 * @since
 *   14.2
 *
 * @return
 *   TRUE if there are more frames to activate, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameLightweightForAllPagesLoopNext()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the name of all frames to Hi Mom.
 *
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     TecUtilFrameLightweightForAllPagesLoopStart();
 *     do
 *       {
 *         TecUtilFrameSetName("Hi Mom");
 *       }
 *     while ( TecUtilFrameLightweightForAllPagesLoopNext() );
 *     TecUtilFrameLightweightForAllPagesLoopEnd();
 *     // Don't redraw until TecUtilFrameLightweightForAllPagesLoopEnd() is called
 *     TecUtilRedraw(TRUE);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */

LINKTOADDON Boolean_t STDCALL TecUtilFrameLightweightForAllPagesLoopNext(void);
/**
 * Ends a sequence of lightweight frame activations across all pages.. See TecUtilFrameLightweightForAllPagesLoopStart()
 * for more information on lightweight frame activating.
 *
 * @since
 *   14.2
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilFrameLightweightForAllPagesLoopEnd()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the name of all frames to Hi Mom.
 *
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     TecUtilFrameLightweightForAllPagesLoopStart();
 *     do
 *       {
 *         TecUtilFrameSetName("Hi Mom");
 *       }
 *     while ( TecUtilFrameLightweightForAllPagesLoopNext() );
 *     TecUtilFrameLightweightForAllPagesLoopEnd();
 *     // Don't redraw until TecUtilFrameLightweightForAllPagesLoopEnd() is called
 *     TecUtilRedraw(TRUE);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON void STDCALL TecUtilFrameLightweightForAllPagesLoopEnd(void);

/* Custom Exporters */
/**
 * Call this function to initialize the export state if you are exporting using
 * the RGB functions.  Must be called before calling
 * TecUtilImageRGBGetScanLine() and TecUtilImageGetDimensions().
 *
 * @param Region
 *   Region to export. Must be a valid region
 *
 * @return
 *   TRUE if the export state was successfully initialized, FALSE otherwise
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImageRGBBitmapCreate(Region)
 *    INTEGER*4 Region
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create an RGB (24-bit) bitmap.
 *
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     if (TecUtilImageRGBBitmapCreate(BitDumpRegion_CurrentFrame))
 *       {
 *         // Do the export
 *         TecUtilImageBitmapDestroy();
 *       }
 *
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 *   See the Example for TecUtilImageRGBBitmapCreate().
 *
 * @code
 * @endcode
 *
 * @ingroup Image
 */
LINKTOADDON Boolean_t STDCALL TecUtilImageRGBBitmapCreate(BitDumpRegion_e Region);

/**
 * Call this function to initialize the export state if you are exporting using
 * the index functions.  Must be called before calling
 * TecUtilImageIndexedGetScanLine() and TecUtilImageGetDimensions().
 *
 * @param Region
 *   Region to export. Must be a valid region
 *
 * @param RedColorTable_Array
 *   Receives the color table Red component. Caller must allocate this array
 *   and they must have 256 elements. You can also pass NULL for this arrays,
 *   using TecUtilImageGetColorTable() to get the color table. Note that in
 *   Version 9.0 or higher, the indices are guaranteed to be less than 256
 *
 * @param GreenColorTable_Array
 *   Receives the color table Green component.
 *
 * @param BlueColorTable_Array
 *   Receives the color table Blue component.
 *
 * @return
 *   TRUE if successful, FALSE otherwise
 *
 *
 * @pre <em>IMPLICATION(RedColorTableArray != NULL,VALID_REF(RedColorTableArray))</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>IMPLICATION(GreenColorTableArray != NULL,VALID_REF(GreenColorTableArray))</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>IMPLICATION(BlueColorTableArray != NULL,VALID_REF(BlueColorTableArray))</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImageIndexedBitmapCreate(
 *   &                   Region,
 *   &                   RedColorTable_Array,
 *   &                   GreenColorTable_Array,
 *   &                   BlueColorTable_Array)
 *    INTEGER*4       Region
 *    INTEGER*4       RedColorTable_Array
 *    INTEGER*4       GreenColorTable_Array
 *    INTEGER*4       BlueColorTable_Array
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create an indexed bitmap.
 *
 * @code
 *   {
 *     short RedColorTable[256];
 *     short BlueColorTable[256];
 *     short GreenColorTable[256];
 *
 *     TecUtilLockStart(AddOnID);
 *     if (TecUtilImageIndexedBitmapCreate(BitDumpRegion_CurrentFrame,
 *                                         RedColorTable,
 *                                         GreenColorTable,
 *                                         BlueColorTable))
 *       {
 *         // Bitmap has been created
 *         TecUtilImageBitmapDestroy(); // When you're finished with it
 *       }
 *
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup Image
 *
 * #internalattributes exclude_python, exclude_sdkdoc, exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilImageIndexedBitmapCreate(BitDumpRegion_e     Region,
                                                              TP_ARRAY_OUT short* RedColorTable_Array,
                                                              TP_ARRAY_OUT short* GreenColorTable_Array,
                                                              TP_ARRAY_OUT short* BlueColorTable_Array);
/**
 *   Destroy the bitmap buffer.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilImageBitmapDestroy()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Image
 */
LINKTOADDON void      STDCALL TecUtilImageBitmapDestroy(void);

/**
 * Gets the image dimensions. You must call TecUtilImageRGBBitmapCreate()
 * before using this function.
 *
 * @param Width
 *   Receives the width of the image in pixels. May be NULL.
 *
 * @param Height
 *   Receives the height of the image in scan lines. May be NULL
 *
 * @return
 *   TRUE if successful, FALSE otherwise
 *
 *
 * @pre <em>Width</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>Height</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImageGetDimensions(
 *   &                   Width,
 *   &                   Height)
 *    INTEGER*4       Width
 *    INTEGER*4       Height
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create an indexed bitmap and get the dimensions.
 *
 * @code
 *   {
 *     short Width,Height;
 *     short ColorTable[256]; // Must be at least 256
 *
 *     TecUtilLockStart(AddOnID);
 *
 *     if (TecUtilImageIndexedBitmapCreate(BitDumpRegion_CurrentFrame,
 *                                         ColorTable))
 *       {
 *         TecUtilImageGetDimensions(&Width,&Height);
 *         // Dimensions are now in the Width and Height variables
 *         TecUtilImageBitmapDestroy(); // Through with the bitmap
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup Image
 */
LINKTOADDON Boolean_t STDCALL TecUtilImageGetDimensions(TP_OUT short* Width,
                                                        TP_OUT short* Height);
/**
 * Gets the RGB values of a scan line. The calling application must
 * allocate/free the arrays and ensure that they have enough space.
 *
 * @param ScanLine
 *   One-based scan line to get the RGB values from. Must be between one and
 *   the height of the image
 *
 * @param Red_Array
 *   Array which receives the RED components. Caller must allocate this array.
 *   May not be NULL.
 *
 * @param Green_Array
 *   Array which receives the GREEN components. Caller must allocate this
 *   array. May not be NULL
 *
 * @param Blue_Array
 *   Array which receives the BLUE components. Caller must allocate this array.
 *   May not be NULL
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 *
 * @pre <em>VALID_REF(RedArray)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(GreenArray)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(BlueArray)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImageRGBGetScanLine(
 *   &                   ScanLine,
 *   &                   Red_Array,
 *   &                   Green_Array,
 *   &                   Blue_Array)
 *    INTEGER*4       ScanLine
 *    INTEGER*4       Red_Array
 *    INTEGER*4       Green_Array
 *    INTEGER*4       Blue_Array
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the first scan line.
 *
 * @code
 *   {
 *     // 24-bit color
 *     short *Red;
 *     short *Green;
 *     short *Blue;
 *     short Width,Height;
 *
 *     TecUtilLockStart(AddOnID);
 *
 *     if (TecUtilImageRGBBitmapCreate(BitDumpRegion_CurrentFrame))
 *      {
 *        TecUtilImageGetDimensions(&Width,&Height);
 *        Red = (short*) malloc(Width * sizeof(short));
 *        Green = (short*) malloc(Width * sizeof(short));
 *        Blue = (short*) malloc(Width * sizeof(short));
 *
 *        TecUtilImageRGBGetScanLine(1,Red,Green,Blue);
 *
 *        TecUtilImageBitmapDestroy();
 *      }
 *
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup Image
 *
 * #internalattributes exclude_python, exclude_sdkdoc, exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilImageRGBGetScanLine(short               ScanLine,
                                                         TP_ARRAY_OUT short* Red_Array,
                                                         TP_ARRAY_OUT short* Green_Array,
                                                         TP_ARRAY_OUT short* Blue_Array);
/**
 *   Gets the color table indices for a scan line. The calling application must
 *   allocate/free the RGBIndex array and ensure that it has enough space.
 *
 * @param ScanLine
 *   One-based scan line to get the RGB values from. Must be between one and
 *   the height of the image
 *
 * @param RGBIndex_Array
 *   Array which receives the indicies. These values can be indexed into the
 *   color table returned by TecUtilImageIndexedBitmapCreate().
 *
 * @return
 *   TRUE if successful, FALSE otherwise
 *
 *
 * @pre <em>VALID_REF(RGBIndex)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImageIndexedGetScanLine(
 *   &                   ScanLine,
 *   &                   RGBIndex_Array)
 *    INTEGER*4       ScanLine
 *    INTEGER*4       RGBIndex_Array
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the color table indices for the first scan line line.
 *
 * @code
 *   {
 *     short Width,Height;
 *     short *RGBIndex = NULL;
 *     short ColorTableR[256]; // Must be at least 256
 *     short ColorTableG[256];
 *     short ColorTableB[256];
 *
 *     TecUtilLockStart(AddOnID);
 *
 *     if  (TecUtilImageIndexedBitmapCreate(BitDumpRegion_CurrentFrame,
 *                                          ColorTableR)
 *                                          ColorTableG)
 *                                          ColorTableB)
 *       {
 *         TecUtilImageGetDimensions(&Width,&Height);
 *         RGBIndex = (short*) malloc(Width * sizeof(short));
 *         TecUtilImageIndexedGetScanLine(1, // scan lines are 1-based
 *                                        RGBIndex);
 *         TecUtilImageBitmapDestroy();
 *       }
 *
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup Image
 *
 * #internalattributes exclude_python, exclude_sdkdoc, exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilImageIndexedGetScanLine(short               ScanLine,
                                                             TP_ARRAY_OUT short* RGBIndex_Array);

/**
 *   Get the color table, that is, the palette, of a color-reduced image. You must call either
 *   TecUtilImageIndexedBitmapCreate() or TecUtilImageBitmapCreateX() before calling this function.
 *   The caller of this function must allocate three arrays of at least 256 bytes and pass them to
 *   this function.
 *
 * @param Red_Array
 *   Pointer to 256-byte array which will receive the red values of the color table. Must not be NULL
 *
 * @param Green_Array
 *   Pointer to 256-byte array which will receive the green values of the color table. Must not be NULL
 *
 * @param Blue_Array
 *   Pointer to 256-byte array which will receive the blue values of the color table. Must not be NULL.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilImageGetColorTable(
 *   &           Red_Array,
 *   &           Green_Array,
 *   &           Blue_Array)
 *    INTEGER*4       Red_Array
 *    INTEGER*4       Green_Array
 *    INTEGER*4       Blue_Array
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *   {
 *     Byte_t Red[256];
 *     Byte_t Green[256];
 *     Byte_t Blue[256];
 *
 *     TecUtilLockStart(AddOnID);
 *     TecUtilImageBitmapCreateX(NULL);
 *     // Will create a 256-color image.
 *     TecUtilImageGetColorTable(Red,Green,Blue);
 *     // Export the image.
 *     TecUtilImageBitmapDestroy();
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup Image
 *
 * #internalattributes exclude_python, exclude_sdkdoc, exclude_tcl
 */
LINKTOADDON void STDCALL TecUtilImageGetColorTable(TP_ARRAY_OUT Byte_t* Red_Array,
                                                   TP_ARRAY_OUT Byte_t* Green_Array,
                                                   TP_ARRAY_OUT Byte_t* Blue_Array);

/**
 * Create a true color or color-reduced bitmap of a specified width.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_CONVERTTO256COLORS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set this to TRUE to create a color-reduced bitmap. A 256-color indexed bitmap will
 *   be created. After rendering the image using this function, call
 *   TecUtilImageGetColorTable() to get the palette, and TecUtilImageIndexedGetScanLine()
 *   to retrieve the palette indices for a scan line.  Set this to FALSE to create a true
 *   color image. After rendering the image using this function, call
 *   TecUtilImageRGBGetScanLine() to get the red, green, and blue values for a scan line.
 *
 * Name:
 *   SV_IMAGEWIDTH
 * Type:
 *   ScreenDim_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   512
 * Required:
 *   No
 * Notes:
 *   This value specifies the resolution in pixels of the created image. The
 *   height is automatically calculated based on the export region and the
 *   width. The approximate amount of memory required to generate an image,
 *   either color-reduced or true color, is Width by Height by three. Thus,
 *   very large values for this parameter will require a large amount of
 *   memory. This function will return FALSE if there is insufficient memory to
 *   render an image of the requested size
 *
 * Name:
 *   SV_EXPORTREGION
 * Type:
 *   DumpRegion_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   ExportRegion_CurrentFrame
 * Required:
 *   No
 * Notes:
 *   Export region. Set this ExportRegion_Currentframe, ExportRegion_AllFrames,
 *   or ExportRegion_WorkArea
 * </ArgListTable>
 *
 * @return
 *   TRUE if the image was successfully created and rendered, otherwise FALSE.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImageBitmapCreateX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList,SV_CONVERTTO256COLORS,FALSE);
 *   // Create a true color image.
 *   TecUtilArgListAppendInt(ArgList,SV_IMAGEWIDTH,1000);
 *   // The image will be rendered with a width of 1000 pixels.
 *   TecUtilArgListAppendInt(ArgList,SV_EXPORTREGION,(LgIndex_t)ExportRegion_WorkArea);
 *   TecUtilImageBitmapCreateX(ArgList);
 *   // Export the image.
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Image
 */
LINKTOADDON Boolean_t STDCALL TecUtilImageBitmapCreateX(ArgList_pa ArgList);


/**
 * @deprecated
 *   This routine is no longer used or necessary.
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON void STDCALL TecUtilMemoryChangeNotify(Int64_t ChangeInKBytes);


/**
 * Test to see if a zone is linear.
 *
 * @param Zone
 *     Zone of interest. This zone must be an enabled zone.
 *
 * @return
 *     TRUE if the zone data is linear,
 *     FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneIsLinear(
 *    INTEGER*4       Zone
 * </FortranSyntax>
 *
 * @since 11.3-17-013
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilZoneIsLinearForFrame
 *
 * @ingroup Zone
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneIsLinear(EntIndex_t Zone);


/**
 * Test to see if a zone of the dataset attached to the specified frame is linear.
 *
 * @param FrameID
 *   Unique ID of the frame that is attached to the dataset for which the query is made.
 * @param Zone
 *     Zone of interest. This zone must be an enabled zone.
 *
 * @return
 *     TRUE if the zone data is linear,
 *     FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneIsLinearForFrame(
 *    INTEGER*4       FrameID
 *    INTEGER*4       Zone
 * </FortranSyntax>
 *
 * @since 14.1
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Zone
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneIsLinearForFrame(UniqueID_t FrameID, 
                                                          EntIndex_t Zone);

/**
 * Test to see if a variable is spatial
 * A spatial variable is X,Y, or Z in 3D, X or Y in 2D or assigned to
 * one of the axis in XY or Polar line maps.
 *
 *
 * @param Var
 *     Variable of interest. This variable must be an enabled variable.
 *
 * @return
 *     TRUE if variable is spatial
 *     FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarIsSpatial(
 *    INTEGER*4       Var
 * </FortranSyntax>
 *
 * @since 11.3-17-013
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilVarIsSpatialForFrame
 *
 * @ingroup Variables
 */
LINKTOADDON Boolean_t STDCALL TecUtilVarIsSpatial(EntIndex_t Var);

/**
 * Test to see if a variable of the the dataset attached to specified frame is spatial.
 * A spatial variable is X,Y, or Z in 3D, X or Y in 2D or assigned to
 * one of the axis in XY or Polar line maps.
 *
 * @param FrameID
 *   Unique ID of the frame that is attached to the dataset for which the query is made.
 *
 * @param Var
 *     Variable of interest. This variable must be an enabled variable.
 *
 * @return
 *     TRUE if variable is spatial
 *     FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarIsSpatialForFrame(
 *    INTEGER*4      FrameID 
 *    INTEGER*4      Var
 * </FortranSyntax>
 *
 * @since 14.1
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Variables
 */
LINKTOADDON Boolean_t STDCALL TecUtilVarIsSpatialForFrame(UniqueID_t FrameID, 
                                                          EntIndex_t Var);
/**
 * Formats the data value according to the specified format and precision.
 * Precision is only used for \ref NumberFormat_FixedFloat, \ref NumberFormat_SuperScript,
 * and \ref NumberFormat_Exponential. Please use TecUtilStringFormatTimeDate()
 * for \ref NumberFormat_TimeDate.
 *
 * @param Value
 *     Value to format.
 * @param Format
 *     Number format to apply to the value.
 * @param Precision
 *     Precision to use when using \ref NumberFormat_FixedFloat,
 *     \ref NumberFormat_SuperScript, or \ref NumberFormat_Exponential.
 * @return
 *     Allocated string containing the value formatted using the specified
 *     formatting. The client is responsible for deallocating the result when
 *     it is no longer needed using TecUtilStringDealloc().
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringFormatValue(
 *   &                Value,
 *   &                Format,
 *   &                Precision,
 *   &                Result,
 *   &                ResultLength)
 *    REAL*8        Value
 *    INTEGER*4     Format,
 *    INTEGER*4     Precision,
 *    CHARACTER*(*) Result
 *    INTEGER*4     ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilStringFormatTimeDate(), TecUtilAxisLabelGetNumberFormat(), TecUtilAxisLabelGetPrecisionFormat()
 * @since 12.2.1.10292
 * @ingroup Utilities
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilStringFormatValue(double         Value,
                                                            NumberFormat_e Format,
                                                            SmInteger_t    Precision);

/**
 * Formats the data value according to the specified time/date format.
 *
 * @param TimeDateValue
 *     Time/date value to format.
 * @param TimeDateFormat
 *     Time/date format to apply to the value or NULL for the default.
 * @return
 *     Allocated string containing the time/date value formatted using the
 *     specified formatting. The client is responsible for deallocating the
 *     result when it is no longer needed using TecUtilStringDealloc().
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringFormatTimeDate(
 *   &                TimeDateValue,
 *   &                TimeDateFormat,
 *   &                Result,
 *   &                ResultLength)
 *    REAL*8        TimeDateValue
 *    CHARACTER*(*) TimeDateFormat
 *    CHARACTER*(*) Result
 *    INTEGER*4     ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilStringFormatValue(), TecUtilAxisLabelGetNumberFormat(), TecUtilAxisLabelGetTimeDateFormat()
 * @since 12.2.1.10292
 * @ingroup Utilities
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilStringFormatTimeDate(double      TimeDateValue,
                                                               const char* TimeDateFormat);

/**
 * Sorts the array of unsigned 32 bit items concurrently using the using the compare function.
 *
 * @param ItemArray
 *     Array of items to sort. Note that this array of unsigned 32 bit items can be either be used
 *     as indirection offsets or as items cast into a 32 bit word. Either way the compare function
 *     is responsible for interpreting the values correctly.
 * @param ItemCount
 *     Number of items in the item array to sort.
 * @param ItemComparator
 *     Function used to compare two items. It must return TRUE if the first parameter is less than
 *     the second parameter.
 * @param ClientData
 *     Any client data required by the ItemComparator.
 *
 * @sa TecUtilSortUInt64ItemArray
 * @since 14.1
 * @ingroup Utilities
 */
LINKTOADDON void STDCALL TecUtilSortUInt32ItemArray(TP_ARRAY_IN_OUT UInt32_t* ItemArray,
                                                    UInt64_t                  ItemCount,
                                                    UInt32ItemComparator_pf   ItemComparator,
                                                    ArbParam_t                ClientData);
/**
 * Sorts the array of unsigned 64 bit items concurrently using the using the compare function.
 *
 * @param ItemArray
 *     Array of items to sort. Note that this array of unsigned 64 bit items can be either be used
 *     as indirection offsets or as items cast into a 64 bit word. Either way the compare function
 *     is responsible for interpreting the values correctly.
 * @param ItemCount
 *     Number of items in the item array to sort.
 * @param ItemComparator
 *     Function used to compare two items. It must return TRUE if the first parameter is less than
 *     the second parameter.
 * @param ClientData
 *     Any client data required by the ItemComparator.
 *
 * @sa TecUtilSortUInt32ItemArray
 * @since 14.1
 * @ingroup Utilities
 */
LINKTOADDON void STDCALL TecUtilSortUInt64ItemArray(TP_ARRAY_IN_OUT UInt64_t* ItemArray,
                                                    UInt64_t                  ItemCount,
                                                    UInt64ItemComparator_pf   ItemComparator,
                                                    ArbParam_t                ClientData);

/**
 * Queries whether the Tecplot engine is busy performing an operation.
 *
 * @return TRUE if the Tecplot engine is busy, FALSE otherwise
 *
 * @since 14.1
 * @ingroup Utilities
 */
LINKTOADDON Boolean_t STDCALL TecUtilIsBusy(void);

/**
 * Signal that a V3 loader has finished loading a dataset.
 * A V3 loader must call this function even if it was not able to 
 * successfully load the data. In other words, calls to
 * TecUtilDataLoadStart() and TecUtilDataLoadFinishX() must always be balanced.
 *
 * Before calling this function, the V3 loader must do the following in the loader callback:
 *
 *  1. Call TecUtilDataLoadStart()
 *  2. Call TecUtilDataSetCreate()
 *  3. If the data file was loaded successfully, Call TecUtilImportSetLoaderInstr().
 *     Calling TecUtilImportSetLoaderInstr is required if the data was loaded successfully.
 *
 * Note:
 *  The loader MUST call this function even if there was an error in loading the data file.
 *  An error in loading the data file is signaled by NOT calling TecUtilImportSetLoaderInstr().
 *  Otherwise calling TecUtilImportSetLoaderInstr() indicates that the data was loaded sucessfully.
 *
 * @return
 *  TRUE if successfully, FALSE otherwise.
 *  If there was an error in loading (that is, the loader did not call TecUtilImportSetLoaderInstr), this function will return FALSE).
 *  
 * @param ArgList
 *   Set of ArgList entries. This is built using calls to
 *   TecUtilArListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *  SV_INITIALPLOTTYPE
 * Type:
 *  InitialPlotType_e
 * Arg Function:
 *  TecUtilArgListAppendInt()
 * Default:
 *   AUTOMATIC
 * Required:
 *  No
 * Notes:
 *  Sets the plot type after loading the appended dataset.
 *
 * Name:
 *  SV_ASSIGNSTRANDIDS
 * Type:
 *  Boolean_t
 * Arg Function:
 *  TecUtilArgListAppendInt()
 * Default:
 *  FALSE
 * Required:
 *  No
 * Notes:
 *  Set to TRUE to automatically assign strand id's.
 *
 * Name:
 *  SV_ADDZONESTOEXISTINGSTRANDS
 * Type:
 *  Boolean_t
 * Default:
 *  FALSE
 * Required:
 *  No
 * Notes:
 *  Set to TRUE to automatically assign zones to existing strands.
 *
 * Name:
 *  SV_INITIALPLOTFIRSTZONEONLY
 * Type:
 *  Boolean_t
 * Default:
 *  FALSE
 * Required:
 *  No
 * Notes:
 *  Set to TRUE to plot first zone only after loading the appended dataset
 *
 * Name:
 *   SV_RESETSTYLE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *  Set to TRUE if you want to reset the style of the current frame, FALSE if you want to keep the same style.
 *
 * </ArgListTable>
 *
 * @param varUniqueIdArray
 *   Pre-allocated array of UniqueId_t. The Unique Ids for all variables created by the loader will be placed
 *   in this array. This array is managed by the caller, which must allocate and de-allocate it. This array
 *   should be dimensioned by the number of variables that the caller creates.
 *   May be NULL.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataLoadFinishX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 *    POINTER (varUniqueIdArray, varUniqueIdArray)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilDataLoadStart()
 * @since 14.2
 */
 LINKTOADDON Boolean_t STDCALL TecUtilDataLoadFinishX(
    ArgList_pa  argList,
    UniqueID_t* varUniqueIdArray);


/**
 * Start staged data load for V3 loaders.
 *
 * A loader add-on should call this function to indicate that it is about to
 * begin loading a dataset to the staging area.
 *
 * When the loader finishes loading the dataset, it must call TecUtilDataLoadFinishX().
 * The V3 loader must call TecUtilDataLoadFinishX() even if the data was not loaded sucessfully.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataLoadStart()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilDataLoadFinishX()
 * @since 14.2
 */
LINKTOADDON void STDCALL TecUtilDataLoadStart(void);
#endif /* _TECUTILM_H */
