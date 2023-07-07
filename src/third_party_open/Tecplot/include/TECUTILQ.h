#ifndef _TECUTIL_Q_H_
#define _TECUTIL_Q_H_
/*
******************************************************************
******************************************************************
*******                                                   ********
******  (C) 1988-2010 Tecplot, Inc.                        *******
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
#if defined TECUTILQMODULE
#define EXTERN
#else
#define EXTERN extern
#endif


/*{{ <motif_only> TecUtilInterfaceGetMotifHandles
                  TecUtilCheckActiveAllocs
                  </motif_only> }}*/

/*
 * Exclude all functions that were deprecated
 *  at the time of the initial Python release
 */
/*{{<exclude_python>
        TecUtilDataFaceNbrFaceIsObscured
        TecUtilDataFaceNbrGetByRef
        TecUtilDataFaceNbrGetByZone
        TecUtilDataFaceNbrGetRawPtr
        TecUtilDataSetRequiresSaving
        TecUtilDataValueGetRawPtr
        TecUtilDataValueGetRef
        TecUtilFrameGetLinking
        TecUtilFrameGetMode
        TecUtilGetTecplotVersion
        TecUtilGetTempFileName
        TecUtilPickListGetXYMapIndex
        TecUtilPickListGetXYMapNumber
        TecUtilProbeAtPosition
        TecUtilXYMapGetActive
        TecUtilXYMapGetAssignment
        TecUtilXYMapGetCount
        TecUtilXYMapGetName
        TecUtilXYMapIsActive
        TecUtilXYMapStyleGetArbValue
        TecUtilXYMapStyleGetDoubleValue
  </exclude_python>}}*/


/*{{<exclude_fglue>
                    TecUtilDataValueGetReadableNativeRefByUniqueID
                    TecUtilDataValueGetWritableNativeRefByUniqueID
                    TecUtilDataValueRefGetGetFunc
                    TecUtilDataValueRefGetSetFunc
                    TecUtilInterfaceGetMotifHandles
                    TecUtilTecplotGetAppMode
  </exclude_fglue> }}*/

/*
 * Exclusion notes:
 *   - TecUtilTecplotGetAppMode is out because we won't be launching OEM apps
 *     from Tcl scripts.
 *   - We had to remove TecUtilZoneGetInfo because calling the function alway
 *     returns writable field data handles and I don't think we really want to
 *     do this. Instead we should deprecate this function and create one to
 *     fetch the [IJK]Max and others to get the Readable/Writeable [UVW]Var,
 *     and so on.
 *   - Omit TecUtilAuxDataPageGetRef for now so tclinterp addon can build.
 *     It can be added back in as soon as ifdefs around TecUtilAuxDataPageGetRef
 *     are removed.
 *
 *
 */
/*{{<exclude_tcl>
                    TecUtilInterfaceGetMotifHandles
                    TecUtilDataValueRefGetGetFunc
                    TecUtilDataValueRefGetSetFunc
                    TecUtilTecplotGetAppMode
                    TecUtilDataValueGetReadableRawPtrByRef
                    TecUtilDataValueGetReadableRawPtr
                    TecUtilDataValueGetWritableRawPtrByRef
                    TecUtilDataValueGetWritableRawPtr
                    TecUtilDataValueGetRawPtr
                    TecUtilDataNodeGetRawPtr
                    TecUtilDataFaceNbrGetRawPtr
                    TecUtilZoneGetInfo
                    TecUtilZoneGetInfoForFrame
                    TecUtilContourGetLevels
  </exclude_tcl> }}*/


/**
 * Gets the number of loader instruction lists available for retrieval by
 * TecUtilImportGetLoaderInstrByNum. You must call this function before calling
 * TecUtilImportGetLoaderInstrByNum, to determine the maximum index
 * of the dataset reader instruction index parameter.
 * This function will return 0 if there are no dataset read commands
 * in the journal. You must call this function before calling
 * TecUtilImportGetLoaderInstrCount() in order to verify that at least
 * one dataset read command has been executed.
 *
 * @return
 *    The number of instruction lists (read commands) available for retrieval by
 *    TecUtilImportGetLoaderInstrByNum. Returns 0 if there have
 *    been no dataset read commands.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImportGetLoaderInstrCount()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *   if (TecUtilDataSetIsAvailable())
 *     {
 *       LgIndex_t MaxInstr = TecUtilImportGetLoaderInstrCount();
 *       if ( MaxInstr > 0 )
 *         {
 *           // only call TecUtilImportGetLoaderInstrByNum if
 *           // at least one dataset read instruction list exists
 *
 *           StringList_pa *Instr = NULL;
 *           char *LoaderName = NULL;
 *           // Get the last instruction list available
 *           TecUtilImportGetLoaderInstrByNum(MaxInstr, // 1-based
 *                                            &LoaderName,
 *                                            &Instr);
 *
 *           // release when finished
 *           TecUtilStringDealloc(&LoaderName);
 *           TecUtilStringListDealloc(&Instr);
 *         }
 *     }
 * @endcode
 *
 * @sa TecUtilImportGetLoaderInstr TecUtilImportGetLoaderInstrByNum
 *
 * @ingroup AddOnLoaders
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilImportGetLoaderInstrCount(void);

/**
 * Retrieves the instructions of the n'th data loader used to load the data into the
 * data set attached to the current frame. Use 1 for the first data loader,
 * or call TecUtilImportGetLoaderInstrCount(), and use this value to get the most
 * recent data loader.
 * If a foreign data set loader addon
 * was used to load the data, then the instruction string passed to the loader
 * is returned. If the data was loaded by Tecplot, then the DataSetReaderName
 * returned is "TECPLOT" and each file name in the data set is returned in the
 * DataSetLoaderInstructions stringlist parameter. The current frame must have
 * an attached data set when this function is used.
 * You MUST call TecUtilImportGetLoaderInstrCount before calling this function
 * in order to verify that at least 1 load command exists in the data journal.
 * This function will assert if the Index parameter is invalid.
 *
 * @param Index
 *    Index of the loader instruction list to retrieve
 *
 * @param DataSetReaderName
 *    Receives the DataSet reader name of the selected instruction list
 *    This parameter must be released after use with
 *    TecUtilStringDealloc. This parameter may be NULL.
 *
 * @param DataSetLoaderInstructions
 *    Receives the instruction string list selected
 *    This parameter must be released after use with
 *    TecUtilStringListDealloc. This parameter may be NULL.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set.
 *
 * @pre <em>DataSetReaderName</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>DataSetLoaderInstructions</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilImportGetLoaderInstrByNum
 *   &           Index,
 *   &           DataSetReaderName
 *   &           DataSetReaderNameLength
 *   &           DataSetLoaderInstructions,
 *    INTEGER*4       Index
 *    CHARACTER*(*)   DataSetReaderName
 *    INTEGER*4       DataSetReaderNameLength
 *    POINTER         (DataSetLoaderInstructions, StringList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *   if (TecUtilDataSetIsAvailable())
 *     {
 *       LgIndex_t MaxInstr = TecUtilImportGetLoaderInstrCount();
 *       if ( MaxInstr > 0 )
 *         {
 *           // only call TecUtilImportGetLoaderInstrByNum if
 *           // at least one dataset read instruction list exists
 *
 *           StringList_pa Instr = NULL;
 *           char *LoaderName = NULL;
 *           // Get the last instruction list available
 *           TecUtilImportGetLoaderInstrByNum(MaxInstr, // 1-based
 *                                            &LoaderName,
 *                                            &Instr);
 *
 *           // release when finished
 *           TecUtilStringDealloc(&LoaderName);
 *           TecUtilStringListDealloc(&Instr);
 *         }
 *     }
 * @endcode
 *
 * @sa TecUtilImportGetLoaderInstr TecUtilImportGetLoaderInstrCount
 *
 * @ingroup AddOnLoaders
 *
 */
LINKTOADDON void STDCALL TecUtilImportGetLoaderInstrByNum(LgIndex_t               Index,
                                                          TP_GIVES char**         DataSetReaderName,
                                                          TP_GIVES StringList_pa* DataSetLoaderInstructions);

/**
 * Indicates if the field layer of interest is active or not in the specified frame.
 *
 * @param FrameID
 *   An ID of the frame for which the query is made.
 * @param LayerShowFlag
 *   The show flag for the field layer of interest. Possible values are:
   @verbatim
     SV_SHOWMESH
     SV_SHOWCONTOUR
     SV_SHOWVECTOR
     SV_SHOWSCATTER
     SV_SHOWSHADE
     SV_SHOWEDGE
     SV_USELIGHTINGEFFECT
     SV_USETRANSLUCENCY
   @endverbatim
 *
 * @return
 *   TRUE if the specified field layer is active.
 *
 *
 * @pre <em>LayerShowFlag</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldLayerIsActiveForFrame(FrameID, LayerShowFlag)
 *    INTEGER*4 FrameID
 *    CHARACTER*(*) LayerShowFlag
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Determine if the field's contour layer is on for the frame with ID=1:
 *
 * @code
 *   if (TecUtilFieldLayerIsActiveForFrame(1, SV_SHOWCONTOUR))
 *     {
 *       // do something with the field's contour level
 *
 *     }
 * @endcode
 *
 * @since 14.1
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFieldLayerIsActiveForFrame(UniqueID_t   FrameID,
                                                                const char *LayerShowFlag);

/**
 * Indicates if the field layer of interest is active or not.
 *
 * @param LayerShowFlag
 *   The show flag for the field layer of interest. Possible values are:
   @verbatim
     SV_SHOWMESH
     SV_SHOWCONTOUR
     SV_SHOWVECTOR
     SV_SHOWSCATTER
     SV_SHOWSHADE
     SV_SHOWEDGE
     SV_USELIGHTINGEFFECT
     SV_USETRANSLUCENCY
   @endverbatim
 *
 * @return
 *   TRUE if the specified field layer is active.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>LayerShowFlag</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldLayerIsActive(LayerShowFlag)
 *    CHARACTER*(*) LayerShowFlag
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Determine if the field's contour layer is on:
 *
 * @code
 *   if (TecUtilFieldLayerIsActive(SV_SHOWCONTOUR))
 *     {
 *       // do something with the field's contour level
 *
 *     }
 * @endcode
 *
 * @sa TecUtilFieldLayerIsActiveForFrame
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFieldLayerIsActive(const char *LayerShowFlag);

/**
 * Indicates if the line plot layer of interest is active or not in the specified frame.
 *
 * @param FrameID
 *   An ID of the frame for which the query is made.
 * @param LayerShowFlag
 *   Show flag for the line plot layer of interest.
 *
 * @return
 *   TRUE if the specific layer in a line plot is active, FALSE otherwise.
 *
 *
 * @pre <em>LayerShowFlag</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLinePlotLayerIsActiveForFrame(FrameID, LayerShowFlag)
 *    INTEGER*4 FrameID
 *    CHARACTER*(*) LayerShowFlag
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Determine if the line plot's symbol layer is on for the frame with ID=1:
 *
 * @code
 *   if (TecUtilLinePlotLayerIsActiveForFrame(1, SV_SHOWSYMBOLS))
 *     {
 *       // Do something with the line plot's symbols
 *     }
 * @endcode
 *
 * @since 14.1
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilLinePlotLayerIsActiveForFrame(UniqueID_t FrameID, 
                                                                   const char *LayerShowFlag);

/**
 *   Indicates if the line plot layer of interest is active or not.
 *
 * @param LayerShowFlag
 *   Show flag for the line plot layer of interest.
 *
 * @return
 *   TRUE if the specific layer in a line plot is active, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>LayerShowFlag</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLinePlotLayerIsActive(LayerShowFlag)
 *    CHARACTER*(*) LayerShowFlag
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Determine if the line plot's symbol layer is on:
 *
 * @code
 *   if (TecUtilLinePlotLayerIsActive(SV_SHOWSYMBOLS))
 *     {
 *       // Do something with the line plot's symbols
 *     }
 * @endcode
 *
 * @sa TecUtilLinePlotLayerIsActiveForFrame
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilLinePlotLayerIsActive(const char *LayerShowFlag);



/**
 * @deprecated
 *   Please use TecUtilLinkingGetValue() instead.
 *
 * @ingroup FrameManagement
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON ArbParam_t STDCALL TecUtilFrameGetLinking(const char *Attribute);
/**
 *   Gets frame linking attributes.
 *
 * @param Attribute
 *   Valid values: SV_BETWEENFRAMES and SV_WITHINFRAME.
 *
 * @param SubAttribute
 *   Attribute to set. For Attribute SV_BETWEENFRAMES the subattribute must be one of
 *   SV_LINKCONTOURLEVELS, SV_LINKFRAMESIZEANDPOSITION, SV_LINKSOLUTIONTIME, SV_LINKXAXISRANGE,
 *   SV_LINKYAXISRANGE, SV_LINK3DVIEW, SV_LINKGROUP. For Attribute SV_WITHINFRAME the subattribute
 *   must be one of SV_LINKAXISSTYLE, SV_LINKGRIDLINESTYLE, SV_LINKLAYERLINECOLOR,
 *   SV_LINKLAYERLINEPATTERN
 *
 * @return
 *   The type of return value is dependent upon the attribute parameter. If the
 *   subattribute is SV_LINKGROUP, the return value is the Group Number and
 *   should be cast to a SmInteger_t, otherwise the return value is TRUE or
 *   FALSE and should be cast to a Boolean_t.
 *
 *
 * @pre <em>Attribute</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>SubAttribute</em>
 *   Pointer must be a valid address and non-NULL.
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilLinkingGetValue(
 *   &           Attribute,
 *   &           SubAttribute,
 *   &           ResultPtr)
 *    CHARACTER*(*)   Attribute
 *    CHARACTER*(*)   SubAttribute
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Query the group number of the current frame:
 *
 * @code
 *   SmInteger_t GroupNumber;
 *   GroupNumber = (SmInteger_t)TecUtilFrameGetLinking(SV_BETWEENFRAMES,
 *               SV_LINKGROUP);
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON ArbParam_t STDCALL TecUtilLinkingGetValue(const char *Attribute,
                                                      const char *SubAttribute);

/**
 *   Get the dimensions of the Tecplot workspace.
 *
 * @param Width
 *   Width of the workspace (in pixels).
 *
 * @param Height
 *   Height of the workspace (in pixels).
 *
 * @pre Must have one or more pages.
 *
 * @pre <em>Width</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Height</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilWorkAreaGetDimensions(
 *   &           Width,
 *   &           Height)
 *    INTEGER*4       Width
 *    INTEGER*4       Height
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the workspace size in paper coordinates:
 *
 * @code
 *   LgIndex_t width_pixels, height_pixels;
 *   TecUtilWorkAreaGetDimensions(&width_pixels, &height_pixels);
 *   double XPos   = TecUtilConvertXPosition(CoordSys_Screen, CoordSys_Paper, 0.0);
 *   double YPos   = TecUtilConvertYPosition(CoordSys_Screen, CoordSys_Paper, 0.0);
 *   double Width  = TecUtilConvertXDimension(CoordSys_Screen, CoordSys_Paper, width_pixels);
 *   double Height = TecUtilConvertYDimension(CoordSys_Screen, CoordSys_Paper, height_pixels);
 * @endcode
 *
 * @ingroup WorkArea
 *
 */
LINKTOADDON void STDCALL TecUtilWorkAreaGetDimensions(TP_OUT LgIndex_t* Width,
                                                      TP_OUT LgIndex_t* Height);

/**
 *   Get the dimensions of the currently defined paper in the Tecplot workspace.
 *
 * @param Width
 *   Width of the paper (in inches).
 *
 * @param Height
 *   Height of the paper (in inches).
 *
 *
 * @pre <em>Width</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Height</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPaperGetDimensions(
 *   &           Width,
 *   &           Height)
 *    REAL*8          Width
 *    REAL*8          Height
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the current paper's position and size:
 *
 * @code
 *   double width, height;
 *   TecUtilPaperGetDimensions(&width, &height);
 * @endcode
 *
 * @ingroup PageManagement
 *
 */
LINKTOADDON void STDCALL TecUtilPaperGetDimensions(TP_OUT double* Width,
                                                   TP_OUT double* Height);




/**
 * Get the current view magnification.
 *
 * @since
 *   10.0-4-22
 *
 * @param Magnification
 *   Returned magnification.
 *
 * @return
 *   Returns TRUE if a magnification can be retrieved.  Possible
 *   cases where FALSE is returned include XY plots where no mappings are
 *   active or floating point out of range error.
 *
 *
 * @pre <em>Magnification</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewGetMagnification(
 *   &           Magnification)
 *    REAL*8          Magnification
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the current view magnification
 * @code
 *   double Magnification
 *   if (TecUtilViewGetMagnification(&Magnification))
 *     {
 *       ... do something with Magnification.
 *     }
 * @endcode
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewGetMagnification(TP_OUT double* Magnification);


/**
 * Determine if blanking is active.
 *
 * @return
 *   TRUE if blanking is active.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilBlankingIsActive()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *   Boolean_t BlankingIsActive = TecUtilBlankingIsActive();
 * @endcode
 *
 * @ingroup Blanking
 *
 * @since 11.3-17-015
 */
LINKTOADDON Boolean_t STDCALL TecUtilBlankingIsActive(void);

/**
 * Determine if the specified data point in the specified zone is visible or if
 * it is not drawn due to value-blanking or IJK-blanking.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   The number of the zone in which the PointIndex is located.
 *
 * @param PointIndex
 *   The index of the point of interest.
 *
 * @return
 *   TRUE if the data point is visible, FALSE if it is blanked.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilBlankingCheckDataPoint(
 *   &                   Zone,
 *   &                   PointIndex)
 *    INTEGER*4       Zone
 *    INTEGER*4       PointIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Zone 4 is IJ-ordered and has IMax = 12, JMax = 8. Determine if the point
 * (I=3, J=2) of zone 4 is visible:
 *
 * @code
 *   Boolean_t IsVisible = TecUtilBlankingCheckDataPoint(4,(2-1)*8+3);
 * @endcode
 *
 *   Zone 5 is finite-element. Determine if the point (N=17) of zone 5 is visible:
 *
 * @code
 *
 *   Boolean_t IsVisible = TecUtilBlankingCheckDataPoint(5, 17);
 * @endcode
 *
 * @ingroup Blanking
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilBlankingCheckDataPoint(EntIndex_t Zone,
                                                            LgIndex_t  PointIndex);


/**
 * Determine if the specified element in the specified finite-element zone is
 * visible or if it is not drawn due to value-blanking.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   The number of the zone in which the element is located. The zone must be finite-element.
 *
 * @param CellIndex
 *   The index of the element of interest.
 *
 * @return
 *   TRUE if the element is visible, FALSE if it is blanked.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilBlankingCheckFECell(
 *   &                   Zone,
 *   &                   CellIndex)
 *    INTEGER*4       Zone
 *    INTEGER*4       CellIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Zone 5 is finite-element. Determine if element number 13 of zone 5 is visible:
 *
 * @code
 *   Boolean_t IsVisible =
 *    TecUtilBlankingCheckFECell(5, 13);
 * @endcode
 *
 * @ingroup Blanking
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilBlankingCheckFECell(EntIndex_t Zone,
                                                         LgIndex_t  CellIndex);

/**
 *   Determine if the specified cell in the specified IJK-ordered zone is visible or if it is not drawn
 *   due to value-blanking or IJK-blanking.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   The number of the zone in which the cell is located. The zone must be IJK-ordered.
 *
 * @param ZonePlane
 *   The plane in which the cell resides. For I or IJ-ordered data, use
 *   IJKPlanes_K. For IJK-ordered data, this determines what to use to determine
 *   the cell. The possible values are: IJKPlanes_I, IJKPlanes_J, IJKPlanes_K, or
 *   IJKPlanes_Volume.
 *
 * @param CellIndex
 *   The index of the cell of interest.
 *
 * @return
 *   TRUE if the cell is visible, FALSE if it is blanked.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilBlankingCheckIJKCell(
 *   &                   Zone,
 *   &                   ZonePlane,
 *   &                   CellIndex)
 *    INTEGER*4       Zone
 *    INTEGER*4       ZonePlane
 *    INTEGER*4       CellIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Zone 4 is IJ-ordered. Determine if cell 13 of zone 4 is visible:
 *
 * @code
 *    Boolean_t IsVisible =
 *      TecUtilBlankingCheckIJKCell(4, IJKPlanes_K, 13);
 * @endcode
 *
 * @ingroup Blanking
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilBlankingCheckIJKCell(EntIndex_t  Zone,
                                                          IJKPlanes_e ZonePlane,
                                                          LgIndex_t   CellIndex);

/**
 * Determine how many levels of locking are currently active in Tecplot. In
 * other words, return the number of nested calls to TecUtilLockStart() without
 * matching calls to TecUtilLockFinish(). See Chapter 10, "Locking and
 * Unlocking Tecplot," in the ADK User's Manual for more information on locks
 * in Tecplot.
 * This function is \ref threadsafe.
 *
 * @return
 *   The number of times Tecplot has been locked without being unlocked.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLockGetCount()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   If Tecplot is not locked, lock it. Later, get the lock count.
 *
 * @code
 *   if (!TecUtilLockIsOn())
 *     {
 *       int LockCount;
 *       Addon_pa AddonID =
 *       TecUtilAddOnRegister(10,"test","v1.23","author");
 *       TecUtilLockStart(AddonID);
 *       .
 *       .
 *       LockCount = TecUtilLockGetCount();
 *       //  LockCount will be at least 1
 *       .
 *       .
 *       TecUtilLockFinish(AddonID);
 *     }
 * @endcode
 *
 * @ingroup Lock
 *
 */
LINKTOADDON int STDCALL TecUtilLockGetCount(void);
/**
 * Determine if Tecplot is locked. See the Chapter "Locking and Unlocking
 * Tecplot," in the ADK User's Manual for more information on locks in Tecplot.
 * This function is \ref threadsafe.
 *
 * @return
 *   TRUE, if Tecplot is locked. FALSE, if it is not locked.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLockIsOn()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Lock
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilLockIsOn(void);

/**
 * Returns the number of pages managed by Tecplot.
 *
 * @since
 *    11.0-5-013
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPageGetCount()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @return
 *     Number of pages managed by Tecplot.
 *
 * @ingroup PageManagement
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilPageGetCount(void);

/**
 * Get the name of the current page.
 *
 * @param PageName
 *   Receives the name of the current page. You must free the returned string
 *   with TecUtilStringDealloc().
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * @pre Must have one or more pages.
 *
 * @pre <em>PageName</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtili(
 *   &                   PageName,
 *   &                   PageNameLength)
 *    CHARACTER*(*)   PageName
 *    INTEGER*4       PageNameLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PageManagement
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPageGetName(TP_GIVES char** PageName);

/**
 * Gets the unique ID for the current page. A unique ID is an integer value
 * unique to a page during the Tecplot session. Using the unique ID a page
 * can be compared to other pages and manipulated via TecUtil calls that take
 * unique IDs.
 * This function is \ref threadsafe.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPageGetUniqueID()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @return
 *   The unique ID for the current page.
 *
 * @pre Must have one or more pages.
 *
 *
 * @ingroup PageManagement
 *
 */
LINKTOADDON UniqueID_t STDCALL TecUtilPageGetUniqueID(void);

/**
 * Gets the position of the page specified by the unique ID relative to the
 * current page. The page positions are 1 based positions relative to the
 * current page where the current page has a position value of 1, the next
 * page 2, the page after that 3, and so on.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.3.28.867
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPageGetPosByUniqueID(UniqueID)
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @return
 *   The 1-based position of the page specified by the unique ID relative to the
 *   current page. Zero is returned if no page exists for the supplied unique ID.
 *
 * @sa For an example of its use see TecUtilSaveLayoutX().
 *
 * @ingroup PageManagement
 */
LINKTOADDON LgIndex_t STDCALL TecUtilPageGetPosByUniqueID(UniqueID_t UniqueID);

/**
 * @deprecated
 *   Please use TecUtilTecplotGetMajorVersion(), TecUtilTecplotGetMinorVersion(),
 *   TecUtilTecplotGetMajorRevision(), and TecUtilTecplotGetMinorRevision() instead.
 *
 * @ingroup Utilities
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecUtilGetTecplotVersion(void);

/**
 * Returns the next unique ID. The unique ID is generated using the same facility that Tecplot uses
 * for its internal unique IDs.
 * @return
 *     The next unique ID.
 */
LINKTOADDON UniqueID_t STDCALL TecUtilGetNextUniqueID(void);

/**
 * Gets Tecplot's major version number. Tecplot's version number has the form:
 * "MajorVersion.MinorVersion-MajorRevision-MinorRevision". Each part can be
 * obtained separately.
 *
 * @since
 *   10.0-3-129
 *
 * @return
 *   Tecplot's major version number.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecplotGetMajorVersion()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilTecplotGetMinorVersion(), TecUtilTecplotGetMajorRevision(),
 *     TecUtilTecplotGetMinorRevision()
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecplotGetMajorVersion(void);

/**
 * Gets Tecplot's minor version number. Tecplot's version number has the form:
 * "MajorVersion.MinorVersion-MajorRevision-MinorRevision". Each part can be
 * obtained separately.
 *
 * @since
 *   10.0-3-129
 *
 * @return
 *   Tecplot's minor version number.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecplotGetMinorVersion()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilTecplotGetMajorVersion(), TecUtilTecplotGetMajorRevision(),
 *     TecUtilTecplotGetMinorRevision()
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecplotGetMinorVersion(void);

/**
 * Gets Tecplot's major revision number. Tecplot's version number has the form:
 * "MajorVersion.MinorVersion-MajorRevision-MinorRevision". Each part can be
 * obtained separately.
 *
 * @since
 *   10.0-3-129
 *
 * @return
 *   Tecplot's major revision number.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecplotGetMajorRevision()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilTecplotGetMajorVersion(), TecUtilTecplotGetMinorVersion(),
 *     TecUtilTecplotGetMinorRevision()
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecplotGetMajorRevision(void);

/**
 * Gets Tecplot's minor revision number. Tecplot's version number has the form:
 * "MajorVersion.MinorVersion-MajorRevision-MinorRevision". Each part can be
 * obtained separately.
 *
 * @since
 *   10.0-3-129
 *
 * @return
 *   Tecplot's minor revision number.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecplotGetMinorRevision()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilTecplotGetMajorVersion(), TecUtilTecplotGetMinorVersion(),
 *     TecUtilTecplotGetMajorRevision()
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecplotGetMinorRevision(void);

/**
 * Get the Tecplot home directory.
 *
 * @return
 *   The Tecplot home directory. This is specified by the TECxxxHOME
 *   environment variable, or the -h flag used when launching Tecplot from the
 *   command line.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTecplotGetHomeDirectory(
 *   &           Result,
 *   &           ResultLength)
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the Tecplot home directory.
 *
 * @code
 *   char *TecHome = NULL;
 *
 *   TecUtilLockStart(AddOnID);
 *   TecHome = TecUtilTecplotGetHomeDirectory();
 *   .....
 *   TecUtilStringDealloc(&TecHome);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilTecplotGetHomeDirectory(void);

/**
 * Get the full path name of the Tecplot executable.
 *
 * @since
 *     11.0-2-005
 *
 * @return
 *     If successful, an allocated string is returned containing the full
 *     path name to the Tecplot executable, otherwise NULL if unsuccessful.
 *
 * @sa TecUtilGetBasePath, TecUtilAddOnGetPath, TecUtilTecplotGetHomeDirectory
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTecplotGetGetExecPath(
 *   &           Result,
 *   &           ResultLength)
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the full path to the Tecplot executable.
 *
 * @code
 *   TecUtilLockStart(AddOnID);
 *   TecplotExePath = TecUtilTecplotGetExePath();
 *   .....
 *   TecUtilStringDealloc(&TecplotExePath);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilTecplotGetExePath(void);

/**
 * Gets full path name, including the library name, of the specified add-on
 * library. The result can subsequently be passed to TecUtilGetBasePath() to
 * strip the library name from the path name.
 *
 * @since
 *     11.0-2-005
 *
 * @param AddOnID
 *     Add-on identifier of the library of interest.
 *
 * @return
 *     If successful, an allocated string is returned containing the full
 *     path name to the add-on library, otherwise NULL if unsuccessful.
 *
 * @sa TecUtilGetBasePath, TecUtilTecplotGetExePath, TecUtilTecplotGetHomeDirectory
 *
 *
 * @pre <em>AddOnID</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAddOnGetPath(
 *   &           Result,
 *   &           ResultLength)
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the base path of this add-on library.
 * @code
 *   TecUtilLockStart(AddOnID);
 *   char *LibraryFullPath = TecUtilAddOnGetPath(AddOnID);
 *   if (LibraryFullPath)
 *     {
 *       char *LibraryBasePath = TecUtilGetBasePath(LibraryFullPath);
 *       if (LibraryBasePath)
 *         {
 *           .
 *           .
 *           TecUtilStringDealloc(&LibraryBasePath);
 *         }
 *       TecUtilStringDealloc(&LibraryFullPath);
 *     }
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup AddOnManagement
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilAddOnGetPath(AddOn_pa AddOnID);

/**
 *   Get a count of the number of frames currently defined.
 *
 * @return
 *   Number of frames defined.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameGetCount()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON int STDCALL TecUtilFrameGetCount(void);



/**
 * @deprecated
 *   Please use TecUtilFrameGetPlotType() instead.
 *
 * @ingroup FrameManagement
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON FrameMode_e STDCALL TecUtilFrameGetMode(void);


/**
 * Get the plot type of the specified frame.
 * This function is \ref threadsafe.
 *
 * @param FrameID
 *   An ID of the frame for which the query is made.
 *
 * @return
 *   For valid frame contexts, the Plot Type can be one of the following possible values:
 *   \ref PlotType_Cartesian2D, \ref PlotType_Cartesian3D, \ref PlotType_XYLine,
 *   \ref PlotType_Sketch or \ref PlotType_PolarLine. If a valid frame context does not exists
 *   \ref PlotType_Invalid is returned. Note that \ref PlotType_Automatic is part of the enumeration
 *   however it is reserved for special setting purposes and should never be returned by this
 *   TecUtilFrameGetPlotType().
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameGetPlotTypeForFrame(FrameID)
 *    INTEGER*4 FrameID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Do something when the frame mode is XY Line for the frame with ID=1:
 *
 * @code
 *   if ( TecUtilFrameGetPlotTypeForFrame(1) == PlotType_XYLine )
 *     {
 *       // do something
 *     }
 * @endcode
 *
 * @since 14.1
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON PlotType_e STDCALL TecUtilFrameGetPlotTypeForFrame(UniqueID_t FrameID);

/**
 * Get the plot type of the current frame.
 * This function is \ref threadsafe.
 *
 * @return
 *   For valid frame contexts, the Plot Type can be one of the following possible values:
 *   \ref PlotType_Cartesian2D, \ref PlotType_Cartesian3D, \ref PlotType_XYLine,
 *   \ref PlotType_Sketch or \ref PlotType_PolarLine. If a valid frame context does not exists
 *   \ref PlotType_Invalid is returned. Note that \ref PlotType_Automatic is part of the enumeration
 *   however it is reserved for special setting purposes and should never be returned by this
 *   TecUtilFrameGetPlotType().
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameGetPlotType()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Do something when the frame mode is XY Line:
 *
 * @code
 *   if ( TecUtilFrameGetPlotType() == PlotType_XYLine )
 *     {
 *       // do something
 *     }
 * @endcode
 *
 * @sa TecUtilFrameGetPlotTypeForFrame
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON PlotType_e STDCALL TecUtilFrameGetPlotType(void);





/**
 * Get the number of items currently in the pick list. See Section "The
 * Pick List," in the ADK User's Manual for a discussion of pick lists.
 *
 * @return
 *   Returns the number of items currently in the pick list.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetCount()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the number of items in the pick list and loop through the pick list:
 *
 * @code
 *   int PickIndex, NumPickedItems = TecUtilPickListGetCount();
 *   for (PickIndex = 1; PickIndex <= NumPickedItems; PickIndex++)
 *     {
 *        //   Go through each object in the pick list using
 *        //   TecUtilPickListGetType(PickIndex) to determine
 *        //   the type of object
 *     }
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON int STDCALL TecUtilPickListGetCount(void);





/**
 *   Gets the type of object from the pick list at the specified index. See
 *   Section "The Pick List," in the ADK User's Manual for a discussion
 *   of pick lists.
 *
 * @param PickListIndex
 *   Index into the pick list
 *
 * @return
 *   The type of object from the pick list at the specified index.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetType(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Go through the pick list, checking object types:
 * @code
 *   int PickIndex, NumPickedItems = TecUtilPickListGetCount();
 *   for (PickIndex = 1; PickIndex <= NumPickedItems; PickIndex++)
 *     {
 *       PickObjects_e ObjectType = TecUtilPickListGetType(PickIndex);
 *       switch (ObjectType)
 *        {
 *        case PickObjects_Geom :
 *          // Do something with picked geometries
 *          break;
 *        case PickObjects_Text :
 *          // Do something with picked text
 *          break;
 *        case PickObjects_Zone :
 *          // Do something with picked zones
 *          break;
 *        case PickObjects_LineMapping :
 *          // Do something with picked line mappings
 *          break;
 *          .
 *          .
 *        }
 *     }
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON PickObjects_e STDCALL TecUtilPickListGetType(int PickListIndex);

/**
 * Get the contour group number that owns the label item at index PickListIndex.
 *
 * @since
 *   14.2
 *
 * @param PickListIndex
 *   Index into the pick list
 *
 * @return
 *   Contour group number.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetLabelsContourGroup(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Pick
 *
 */
LINKTOADDON int STDCALL TecUtilPickListGetLabelsContourGroup(int PickListIndex);


/**
 * Get the contour group number that owns the legend item at index PickListIndex.
 *
 * @since
 *   14.2
 *
 * @param PickListIndex
 *   Index into the pick list
 *
 * @return
 *   Contour group number.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetLegendContourGroup(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Pick
 *
 */
LINKTOADDON int STDCALL TecUtilPickListGetLegendContourGroup(int PickListIndex);


/**
 *   Get the name of the frame from the pick list at the specified index. The
 *   object in the pick list at the specified index must be of type
 *   PickObjects_Frame. See Section "The Pick List," in the ADK User's
 *   Manual for a discussion of pick lists.
 *
 * @param PickListIndex
 *   Index into the pick list.
 *
 * @return
 *   The name of the frame from the pick list at the specified index. You must
 *   call TecUtilStringDealloc() on the returned string.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPickListGetFrameName(
 *   &           PickListIndex,
 *   &           Result,
 *   &           ResultLength)
 *    INTEGER*4       PickListIndex
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * If the first object in the pick list is a frame, get its name:
 * @code
 *   if (TecUtilPickListGetType(1) == PickObjects_Frame)
 *     {
 *       char *FrameName = TecUtilPickListGetFrameName(1);
 *       .
 *       .
 *       TecUtilStringDealloc(&FrameName);
 *     }
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilPickListGetFrameName(int PickListIndex);





/**
 * Get the unique identifier of the frame from the pick list at the specified
 * index. The object in the pick list at the specified index must be of type
 * PickObjects_Frame. See Section "The Pick List," in the ADK User's
 * Manual for a discussion of pick lists.
 *
 * @param PickListIndex
 *   Index into the pick list
 *
 * @return
 *   The unique identifier of the frame from the pick list at the specified index.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetFrameUniqueID(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   If the first object in the pick list is a frame, get its unique identifier:
 *
 * @code
 *   if (TecUtilPickListGetType(1) == PickObjects_Frame)
 *     {
 *       UniqueID_t UniqueID = TecUtilPickListGetFrameUniqueID(1);
 *       .
 *       .
 *       .
 *     }
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON UniqueID_t STDCALL TecUtilPickListGetFrameUniqueID(int PickListIndex);

/**
 * Get dimension of the bounding box surrounding all frames in @ref CoordSys_Paper units.
 *
 * @param X1 x coordinate of the upper left corner
 * @param Y1 y coordinate of the upper left corner
 * @param X2 x coordinate of the bottom right corner
 * @param Y2 y coordinate of the bottom right corner
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGetBoundingBoxOfAllFrames(
 *   &           X1,
 *   &           Y1,
 *   &           X2,
 *   &           Y2)
 *    INTEGER*4       X1
 *    INTEGER*4       Y1
 *    INTEGER*4       X2
 *    INTEGER*4       Y2
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since 11.2-0-467
 *
 * @ingroup FrameManagement
 */
LINKTOADDON void STDCALL TecUtilGetBoundingBoxOfAllFrames(TP_OUT double* X1,
                                                          TP_OUT double* Y1,
                                                          TP_OUT double* X2,
                                                          TP_OUT double* Y2);


/**
 * Get the current X and Y axis range grid values, this is used for getting
 * the X and Y ranges for the "grid" coordinate system used by text and 
 * geometry annotations.
 * The result depends on the current plot type:
 *    Sketch & 2D: retrieve the axis ranges.
 *    XY line: retrieve the ranges of the lowest active axis.
 *    Polar: transform to the world coordinate system before retrieve it.
 *    3D: retrieve the eye coordinate system grid ranges.
 *
 * Use \ref TecUtilAxisGetRange to get the actual world coordinate ranges
 * on axes.
 *
 * @param AxisGridXMin
 *   Axis X minimum grid value.
 *
 * @param AxisGridYMin
 *   Axis Y minimum grid value.
 *
 * @param AxisGridXMax
 *   Axis X maximum grid value.
 *
 * @param AxisGridYMax
 *   Axis Y maximum grid value.
 *
 * @return
 *   Returns TRUE if the current frame is in a state and has a plot that employs 
 *   axis ranges otherwise FALSE. One scenario where this could return FALSE is if
 *   you have an XY plot but no line mappings defined.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>AxisGridXMin</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>AxisGridYMin</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>AxisGridXMax</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>AxisGridYMax</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAxisGetGridRange(
 *   &           AxisGridXMin,
 *   &           AxisGridYMin,
 *   &           AxisGridXMax,
 *   &           AxisGridYMax)
 *    REAL*8          AxisGridXMin
 *    REAL*8          AxisGridYMin
 *    REAL*8          AxisGridXMax
 *    REAL*8          AxisGridYMax
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *   double Xmin,Ymin,Xmin,YMax;
 *   TecUtilAxisGetGridRange(&Xmin,&Ymin,&Xmin,&YMax);
 * @endcode
 *
 * @since 14.2
 *
 * @ingroup Axis
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAxisGetGridRange(TP_OUT double* AxisGridXMin,
                                                      TP_OUT double* AxisGridYMin,
                                                      TP_OUT double* AxisGridXMax,
                                                      TP_OUT double* AxisGridYMax);


/**
 * Get the current minimum and maximum values for the specified axis.
 *
 * @param Axis
 *   The axis to query. This can be one of 'X', 'Y', 'Z', 'T', or 'R',
 *
 * @param AxisNum
 *   The axis number. For XY-plots this can be any number from one to five. For all other plots this
 *   must be one.
 *
 * @param AxisMin
 *   The current axis minimum value
 *
 * @param AxisMax
 *   The current axis maximum value
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>AxisMin</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>AxisMax</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAxisGetRange(
 *   &           Axis,
 *   &           AxisNum,
 *   &           AxisMin,
 *   &           AxisMax)
 *    CHARACTER*(*)   Axis
 *    INTEGER*4       AxisNum
 *    REAL*8          AxisMin
 *    REAL*8          AxisMax
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the range on Y-Axis number 2 (assume that it was previously determined
 *   that the frame mode is currently XY):
 *
 * @code
 *   double YMin,YMax;
 *   TecUtilAxisGetRange('Y',2,&YMin,&YMax);
 * @endcode
 *
 * @ingroup Axis
 *
 */
LINKTOADDON void STDCALL TecUtilAxisGetRange(char           Axis,
                                             short          AxisNum,
                                             TP_OUT double* AxisMin,
                                             TP_OUT double* AxisMax);

/**
 * Returns the axis variable assignments for 'X' or 'T', 'Y' or 'R', and 'Z'.
 *
 * @param XOrThetaVar
 *     'X' or 'T' axis variable assignment or TECUTILBADVARNUMBER if unassigned.
 * @param YOrRVar
 *     'Y' or 'R' axis variable assignment or TECUTILBADVARNUMBER if unassigned.
 * @param ZVar
 *     'Z' axis variable assignment or TECUTILBADVARNUMBER if unassigned.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>XOrThetaVar</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>YOrRVar</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>ZVar</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAxisGetVarAssignments(
 *   &           XOrThetaVar,
 *   &           YOrRVar,
 *   &           ZVar)
 *    REAL*8 XOrThetaVar
 *    REAL*8 YOrRVar
 *    REAL*8 ZVar
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since 12.2.1.10292
 * @ingroup Axis
 */
LINKTOADDON void STDCALL TecUtilAxisGetVarAssignments(TP_OUT EntIndex_t* XOrThetaVar,
                                                      TP_OUT EntIndex_t* YOrRVar,
                                                      TP_OUT EntIndex_t* ZVar);

/**
 * Gets the specified axis label's number formatting.
 *
 * @param Axis
 *   The axis to query. This can be one of 'X', 'Y', 'Z', 'T', or 'R',
 * @param AxisNum
 *   The axis number. For XY-plots this can be any number from one to five. For
 *   all other plots this must be one.
 * @return
 *   The number formatting currently begin used for the specified axis and axis
 *   number.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAxisLabelGetNumberFormat(
 *   &                   Axis,
 *   &                   AxisNum)
 *    CHARACTER*(*) Axis
 *    INTEGER*4     AxisNum
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilStringFormatTimeDate(), TecUtilStringFormatValue(), TecUtilAxisLabelGetPrecisionFormat()
 * @since 12.2.1.10292
 * @ingroup Axis
 */
LINKTOADDON NumberFormat_e STDCALL TecUtilAxisLabelGetNumberFormat(short Axis,
                                                                   int   AxisNum);
/**
 * Gets the specified axis label's time/date formatting.
 *
 * @param Axis
 *   The axis to query. This can be one of 'X', 'Y', 'Z', 'T', or 'R',
 * @param AxisNum
 *   The axis number. For XY-plots this can be any number from one to five. For
 *   all other plots this must be one.
 * @return
 *     Allocated string containing the time/date formatting that will be used
 *     if the number formatting is \ref NumberFormat_TimeDate. The client is
 *     responsible for deallocating the result when it is no longer needed
 *     using TecUtilStringDealloc().
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAxisLabelGetTimeDateFormat(
 *   &                Axis,
 *   &                AxisNum,
 *   &                Result,
 *   &                ResultLength)
 *    CHARACTER*(*) Axis
 *    INTEGER*4     AxisNum
 *    CHARACTER*(*) Result
 *    INTEGER*4     ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilStringFormatTimeDate(), TecUtilAxisLabelGetNumberFormat()
 * @since 12.2.1.10292
 * @ingroup Axis
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilAxisLabelGetTimeDateFormat(short Axis,
                                                                     int   AxisNum);
/**
 * Gets the specified axis label's formatting precision.
 *
 * @param Axis
 *   The axis to query. This can be one of 'X', 'Y', 'Z', 'T', or 'R',
 * @param AxisNum
 *   The axis number. For XY-plots this can be any number from one to five. For
 *   all other plots this must be one.
 * @return
 *   The number formatting precision that will be used for \ref NumberFormat_FixedFloat,
 *   \ref NumberFormat_SuperScript, and \ref NumberFormat_Exponential formats.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAxisLabelGetPrecisionFormat(
 *   &                   Axis,
 *   &                   AxisNum)
 *    CHARACTER*(*) Axis
 *    INTEGER*4     AxisNum
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilStringFormatValue(), TecUtilAxisLabelGetNumberFormat()
 * @since 12.2.1.10292
 * @ingroup Axis
 */
LINKTOADDON SmInteger_t STDCALL TecUtilAxisLabelGetPrecisionFormat(short Axis,
                                                                   int   AxisNum);

/**
 * @since 14.2
 *
 * Get the next range value from the axis snapped to the grid.
 *
 * @param Axis
 *   The axis to query. This can be one of 'X', 'Y', 'Z', 'T', or 'R',
 *
 * @param AxisNum
 *   The axis number. For XY-plots this can be any number from one to five. For all other plots this
 *   must be one.
 *
 * @param CurrentValue
 *   The current axis value.
 *
 * @param IsIncreasing
 *   TRUE if the next value should be greater than the current value, FALSE otherwise.
 *
 * @param AutoAdjustToNiceValues
 *   TRUE if the resulting value should be adjusted to nice values, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilAxisGetRange(
 *   &           Axis,
 *   &           AxisNum,
 *   &           CurrentValue,
 *   &           IsIncreasing,
 *   &           AutoAdjustToNiceValues)
 *    CHARACTER*(*)   Axis
 *    INTEGER*4       AxisNum
 *    REAL*8          CurrentValue
 *    INTEGER*4       IsIncreasing
 *    INTEGER*4       AutoAdjustToNiceValues
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Axis
 *
 */
LINKTOADDON double STDCALL TecUtilAxisGetNextRangeValue(char      Axis,
                                                        short     AxisNum,
                                                        double    CurrentValue,
                                                        Boolean_t IsIncreasing,
                                                        Boolean_t AutoAdjustToNiceValues);

/**
 * Get the next "nice" value on a range.
 *
 * @param startValue
 *   The current value.
 *
 * @param minValue
 *   The lower limit of the range.
 *
 * @param maxValue
 *   The upper limit of the range, should be bigger than minLimit.
 *
 * @param preferredDivisions
 *   Preferred number of divisions in the range, should be bigger than one.
 *
 * @param isIncreasing
 *   TRUE if the next value should be greater than the current value, FALSE otherwise.
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilGetNextNiceIncDecValue(
 *   &           startValue,
 *   &           minValue,
 *   &           maxValue,
 *   &           preferredDivisions,
 *   &           isIncreasing)
 *    REAL*8          startValue
 *    REAL*8          minValue
 *    REAL*8          maxValue
 *    INTEGER*4       preferredDivisions
 *    INTEGER*4       isIncreasing
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since 14.2
 *
 * @ingroup Utilites
 *
 */
LINKTOADDON double STDCALL TecUtilGetNextNiceIncDecValue(double    startValue,
                                                         double    minValue,
                                                         double    maxValue,
                                                         int       preferredDivisions,
                                                         Boolean_t isIncreasing);

/**
 * Get the picked subobject type of the picked axis object.
 * @param PickListIndex
 *   Index into the pick list. The object in the pick list at the specified index must be of type
 * PickObjects_Axis. See Section "The Pick List," in the ADK User's Manual
 * for a discussion of pick lists.
 * 
 * @return
 *   The kind of the picked axis subobject from the pick list at the specified index.
 *
 * @since
 *   14.2
 *
 * @ingroup Pick
 */
LINKTOADDON AxisSubObject_e STDCALL TecUtilPickListGetAxisSubObject(int PickListIndex);

/**
 * Get the kind of axis (X, Y, or Z) from the pick list at the specified index.
 * The object in the pick list at the specified index must be of type
 * PickObjects_Axis. See Section "The Pick List," in the ADK User's Manual
 * for a discussion of pick lists.
 *
 * @param PickListIndex
 *   Index into the pick list
 *
 * @return
 *   The kind of axis from the pick list at the specified index.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetAxisKind(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   If an axis object is picked, get its type and number:
 *
 * @code
 *   int PickIndex, NumPickedItems = TecUtilPickListGetCount();
 *   for (PickIndex = 1; PickIndex <= NumPickedItems; PickIndex++)
 *     {
 *       if (TecUtilPickListGetType(PickIndex) == PickObjects_Axis)
 *         {
 *           char AxisKind = TecUtilPickListGetAxisKind(PickIndex);
 *           int  AxisNum = TecUtilPickListGetAxisNumber(PickIndex);
 *           .
 *           .
 *         }
 *     }
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON char STDCALL TecUtilPickListGetAxisKind(int PickListIndex);


/**
 * Get the number of the axis from the pick list at the specified index. The
 * object in the pick list at the specified index must be of type
 * PickObjects_Axis. See Section "The Pick List," in the ADK User's Manual
 * for a discussion of pick lists.
 *
 * @param PickListIndex
 *   Index into the pick list
 *
 * @return
 *   For Sketch, 2-D, and 3-D plots, the return value is one. For XY plots, returns the axis number
 *   from the pick list at the specified index.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetAxisNumber(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Pick
 *
 */
LINKTOADDON int STDCALL TecUtilPickListGetAxisNumber(int PickListIndex);


/**
 * Get the number of the zone from the pick list at the specified index. The
 * object in the pick list at the specified index must be of type
 * PickObjects_Zone. See Section "The Pick List," in the ADK User's Manual
 * for a discussion of pick lists.
 *
 * @param PickListIndex
 *   Index into the pick list
 *
 * @return
 *   The number of the zone from the pick list at the specified index.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetZoneNumber(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Pick
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilPickListGetZoneNumber(int PickListIndex);

/**
 * Get the slice group number from the pick list at the specified index. The
 * object in the pick list at the specified index must be of type
 * PickObjects_SliceCOB. See Section "The Pick List," in the ADK User's Manual
 * for a discussion of pick lists.
 *
 * @since
 *   14.2
 *
 * @param PickListIndex
 *   Index into the pick list
 *
 * @return
 *   The slice group associated with the slice that was picked.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetSliceGroup(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Pick
 *
 */
LINKTOADDON SmInteger_t STDCALL TecUtilPickListGetSliceGroup(int PickListIndex);

/**
 * Get the iso-surface group number from the pick list at the specified index. The
 * object in the pick list at the specified index must be of type
 * PickObjects_IsoSurfaceCOB. See Section "The Pick List," in the ADK User's Manual
 * for a discussion of pick lists.
 *
 * @since
 *   14.2
 *
 * @param PickListIndex
 *   Index into the pick list
 *
 * @return
 *   The iso-surface group associated with the iso-surface that was picked.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetIsoSurfaceGroup(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Pick
 *
 */
LINKTOADDON SmInteger_t STDCALL TecUtilPickListGetIsoSurfaceGroup(int PickListIndex);

/**
 * Get the specific point that was selected in the zone from the pick list at
 * the specified index.  The object in the pick list at the specified index
 * must be of type PickObjects_Zone. See Section "The Pick List," in the
 * ADK User's Manual for a discussion of pick lists.
 *
 * @param PickListIndex
 *   Index into the pick list
 *
 * @param IIndex
 *   The I-index value of the point that was selected in the zone
 *
 * @param JIndex
 *   The J-index value of the point that was selected in the zone
 *
 * @param KIndex
 *   The K-index value of the point that was selected in the zone
 *
 *
 * @pre <em>IIndex</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>JIndex</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>KIndex</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPickListGetZoneIndices(
 *   &           PickListIndex,
 *   &           IIndex,
 *   &           JIndex,
 *   &           KIndex)
 *    INTEGER*4       PickListIndex
 *    INTEGER*4       IIndex
 *    INTEGER*4       JIndex
 *    INTEGER*4       KIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   If the first object in the pick list is a zone, get its number and indices:
 *
 * @code
 *   if (TecUtilPickListGetType(1) == PickObjects_Zone)
 *     {
 *      LgIndex_t  IIndex, JIndex, KIndex;
 *      EntIndex_t ZoneNumber = TecUtilPickListGetZoneNumber(1);
 *      TecUtilPickListGetZoneIndices(1, &IIndex, &JIndex, &KIndex);
 *      .
 *      .
 *     }
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON void STDCALL TecUtilPickListGetZoneIndices(int               PickListIndex,
                                                       TP_OUT LgIndex_t* IIndex,
                                                       TP_OUT LgIndex_t* JIndex,
                                                       TP_OUT LgIndex_t* KIndex);





/**
 * @deprecated
 *   Please use TecUtilPickListGetLineMapNumber() instead.
 *
 * @ingroup Pick
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON EntIndex_t STDCALL TecUtilPickListGetXYMapNumber(int PickListIndex);




/**
 * Get the number of the Line-mapping from the pick list at the specified
 * index. The object in the pick list at the specified index must be of type
 * PickObjects_LineMapping. See Section "The Pick List," in the ADK User's
 * Manual for a discussion of pick lists.
 *
 * @param PickListIndex
 *   Index into the pick list.
 *
 * @return
 *   The number of the Line-mapping from the pick list at the specified index.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetLineMapNumber(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Pick
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilPickListGetLineMapNumber(int PickListIndex);



/**
 * @deprecated
 *   Please use TecUtilPickListGetLineMapIndex() instead.
 *
 * @ingroup Pick
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecUtilPickListGetXYMapIndex(int PickListIndex);





/**
 * Get the index value of the specific point that was selected in the
 * Line-mapping from the pick list at the specified index. The object in the
 * pick list at the specified index must be of type PickObjects_LineMapping. In
 * order to get useful information from this function, the mouse mode should be
 * of the type MouseButtonMode_Adjust or MouseButtonMode_AdvancedAdjust. See
 * Section "The Pick List," in the ADK User's Manual for a discussion of pick
 * lists.
 *
 * @param PickListIndex
 *   Index into the pick list.
 *
 * @return
 *   The index value of the specific point that was selected in the
 *   Line-mapping from the pick list at the specified index if the Adjustor
 *   mode is being used. Otherwise, zero.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetLineMapIndex(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   If the first object in the pick list is an Line-mapping, get its number and index:
 *
 * @code
 *   if (TecUtilPickListGetType(1) == PickObjects_LineMapping)
 *     {
 *       EntIndex_t MapNumber = TecUtilPickListGetLineMapNumber(1);
 *       LgIndex_t  MapIndex  = TecUtilPickListGetLineMapIndex(1);
 *
 *       // Do Something here
 *
 *     }
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilPickListGetLineMapIndex(int PickListIndex);









/**
 * Get the text from the pick list at the specified index. The object in the
 * pick list at the specified index must be of type PickObjects_Text. See
 * Section "The Pick List," in the ADK User's Manual for a discussion of
 * pick lists.
 *
 * @param PickListIndex
 *   Index into the pick list
 *
 * @return
 *   The text ID from the pick list at the specified index.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPickListGetText(
 *   &           PickListIndex,
 *   &           ResultPtr)
 *    INTEGER*4      PickListIndex
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   For every text object which is picked, set it to have a filled text box:
 *
 * @code
 *   int PickIndex, NumPickedItems = TecUtilPickListGetCount();
 *   for (PickIndex = 1; PickIndex <= NumPickedItems; PickIndex++)
 *     {
 *       if (TecUtilPickListGetType(PickIndex) == PickObjects_Text)
 *         {
 *           Text_ID TID = TecUtilPickListGetText(PickIndex);
 *           TecUtilTextBoxSetType(TID, TextBox_Filled);
 *         }
 *     }
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Text_ID STDCALL TecUtilPickListGetText(int PickListIndex);



/**
 * Get the geometry from the pick list at the specified index. The object in
 * the pick list at the specified index must be of type PickObjects_Geom. See
 * Section "The Pick List," in the ADK User's Manual for a discussion of
 * pick lists.
 *
 * @param PickListIndex
 *   Index into the pick list
 *
 * @return
 *   The geometry ID from the pick list at the specified index.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPickListGetGeom(
 *   &           PickListIndex,
 *   &           ResultPtr)
 *    INTEGER*4      PickListIndex
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * If a specific point of a polyline geometry object is picked, set the
 * coordinates for that point to be X = 4.5, Y = 3.2 :
 *
 * @code
 *   int PickIndex, NumPickedItems = TecUtilPickListGetCount();
 *   for (PickIndex = 1; PickIndex <= NumPickedItems; PickIndex++)
 *     {
 *       if (TecUtilPickListGetType(PickIndex) == PickObjects_Geom)
 *         {
 *           SmInteger_t PolylineNum;
 *           LgIndex_t   PointIndex;
 *           Geom_ID GID = TecUtilPickListGetGeom(PickIndex);
 *           if (TecUtilGeomGetType(GID) == GeomForm_LineSegs)
 *             TecUtilPickListGetGeomInfo(PickIndex,
 *                                        &PolylineNum,
 *                                        &PointIndex);
 *           if ((PolylineNum > 0) && (PointIndex > 0))
 *             TecUtilGeom2DMPolySetPoint(GID, PolylineNum,
 *                                        PointIndex, 4.5, 3.2);
 *         }
 *     }
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Geom_ID STDCALL TecUtilPickListGetGeom(int PickListIndex);
/**
 * Get the specific point that was selected in the geometry from the pick list
 * at the specified index. The object in the pick list at the specified index
 * must be of type PickObjects_Geom. In order to get useful information from
 * this function, the geometry should be of type GeomForm_LineSegs or
 * GeomForm_LineSegs3D. The mouse mode should be MouseButtonMode_Adjust or
 * MouseButtonMode_AdvancedAdjust. See Section "The Pick List," in the ADK
 * User's Manual for a discussion of pick lists.
 *
 * @param PickListIndex
 *   Index into the pick list.
 *
 * @param PolylineNum
 *   The number of the polyline that was selected if the geometry is a line
 *   segment and the Adjustor mode is being used. Otherwise, zero
 *
 * @param PointIndex
 *   The index of the specific point that was selected in the polyline if the
 *   geometry is a line segment and the Adjustor mode is being used. Otherwise,
 *   zero.
 *
 *
 * @pre <em>PolylineNum</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>PointIndex</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPickListGetGeomInfo(
 *   &           PickListIndex,
 *   &           PolylineNum,
 *   &           PointIndex)
 *    INTEGER*4       PickListIndex
 *    INTEGER*4       PolylineNum
 *    INTEGER*4       PointIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Pick
 *
 */
LINKTOADDON void STDCALL TecUtilPickListGetGeomInfo(int                 PickListIndex,
                                                    TP_OUT SmInteger_t* PolylineNum,
                                                    TP_OUT LgIndex_t*   PointIndex);

/**
 *   Gets the minimum and maximum values of a variable.
 *
 * @param Var
 *   Index of the variable. Must be greater than zero and the variable must be
 *   enabled
 *
 * @param VarMin
 *   Receives the minimum value of the variable. Must not be NULL
 *
 * @param VarMax
 *   Receives the maximum value of the variable. Must not be NULL
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>VarMin</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VarMax</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilVarGetMinMax(
 *   &           Var,
 *   &           VarMin,
 *   &           VarMax)
 *    INTEGER*4       Var
 *    REAL*8          VarMin
 *    REAL*8          VarMax
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the minimum and maximum values of the first variable in a data set:
 *
 * @code
 *   double VarMin,VarMax;
 *   TecUtilVarGetMinMax(1,&VarMin,&VarMax);
 * @endcode
 *
 * @ingroup Variables
 *
 */
LINKTOADDON void STDCALL TecUtilVarGetMinMax(EntIndex_t     Var,
                                             TP_OUT double* VarMin,
                                             TP_OUT double* VarMax);

/**
 * Gets the logically unique nodes, cell size, and cell center position of an
 * entire finite element polytope cell.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.2-0-541
 *
 * @param FaceMap
 *   Face map handle.
 * @param ElemToFaceMap
 *   The element-to-face map handle
 * @param CellIndex
 *   The cell index to query.
 * @param XFieldData
 *   X variable field data handle.
 * @param YFieldData
 *   Y variable field data handle or NULL if not to be used for calculating the
 *   cell size and center.
 * @param ZFieldData
 *   Z variable field data handle or NULL if not to be used for calculating the
 *   cell size and center.
 * @param NumUniqueNodes
 *   Pointer to the resulting number of unique nodes.
 * @param UniqueNodesSize
 *   As input this value is a pointer to the current dimension of the
 *   UniqueNodes array or a pointer to a dimension value of zero if the
 *   UniqueNodes array has not yet been allocated.
 *   @par
 *   As output it is a pointer to the resulting dimension of the UniqueNodes
 *   array which is at least large enough to hold the unique nodes.
 * @param UniqueNodes
 *   As input this value is a pointer to NULL or a pointer to an allocated
 *   array dimensioned as specified by UniqueNodesSize items.
 *   @par
 *   As output it is a pointer to the resulting allocated or reallocated array
 *   containing the unique nodes. The resulting array is only reallocated if
 *   the number of unique nodes exceeds the value pointed to by
 *   UniqueNodesSize.
 *   @par
 *   If you supply a pre-allocated array for UniqueNodes that was not allocated
 *   by a previous call to this function, it MUST be sized large enough to hold
 *   the requested unique nodes. If it were not, Tecplot would attempt to
 *   release the undersized resource causing undefined behavior because
 *   Tecplot's allocator/deallocator is different than that of an add-on.
 *   @par
 *   If UniqueNodes was allocated by a call to this function you must
 *   deallocated it when no longer needed by a call to TecUtilArrayDealloc().
 * @param CellSize
 *   Cell size. The size is area for surface data and volume for volume data.
 * @param CellCenter
 *   Cell center using the nodal average.
 *
 * @return
 *     TRUE if successful, FALSE otherwise.
 *
 *
 * @pre <em>NumUniqueNodes</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>UniqueNodes</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * Get the unique element nodes for an cell 362 and 535 of zone 5 letting
 * Tecplot allocate the array of unique nodes. We assume X, Y, and Z are the
 * first three variables.
 * @code
 *   EntIndex_t       Zone = 5; // ...assume that this is an FE polytope zone
 *   FaceMap_pa       FM   = TecUtilDataFaceMapGetReadableRef(Zone);
 *   ElemToFaceMap_pa EFM  = TecUtilDataElemGetReadableRef(Zone);
 *   FieldData_pa     XFD  = TecUtilDataValueGetReadableNLRef(Zone, 1);
 *   FieldData_pa     YFD  = TecUtilDataValueGetReadableNLRef(Zone, 2);
 *   FieldData_pa     ZFD  = TecUtilDataValueGetReadableNLRef(Zone, 3);
 *   double           CellSize;
 *   XYZ_s            CellCenter;
 *   LgIndex_t        NumUniqueNodes;
 *   LgIndex_t        UniqueNodesSize = 0;    // ...let Tecplot allocate
 *   LgIndex_t       *UniqueNodes     = NULL; // ...let Tecplot allocate
 *
 *   IsOk = ((FM != NULL && EFM != NULL)                 &&
 *           (XFD != NULL && YFD != NULL && ZFD != NULL) &&
 *           TecUtilDataFEPolyGetCellNodesSizeAndCenter(FM, EFM, 362,
 *                                                      XFD, YFD, ZFD,
 *                                                      &NumUniqueNodes,
 *                                                      &UniqueNodesSize,
 *                                                      &UniqueNodes,
 *                                                      &CellSize,
 *                                                      &CellCenter);
 *   if (IsOk)
 *     {
 *       ...do something useful with the unique nodes, size, or center of cell 362
 *     }
 *
 *   // Using the previously allocate array request the unique nodes of cell 535.
 *   // Note that the array may get reallocated if required to hold the unique
 *   // nodes of cell 535.
 *   if (IsOk)
 *     {
 *       IsOk = TecUtilDataFEPolyGetCellNodesSizeAndCenter(FM, EFM, 535,
 *                                                         XFD, YFD, ZFD,
 *                                                         &NumUniqueNodes,
 *                                                         &UniqueNodesSize,
 *                                                         &UniqueNodes,
 *                                                         &CellSize,
 *                                                         &CellCenter);
 *     }
 *
 *   if (IsOk)
 *     {
 *       ...do something useful with the unique nodes, size, or center of cell 535
 *     }
 *
 *   // cleanup
 *   TecUtilArrayDealloc(&((void *)UniqueNodes));
 * @endcode
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilDataFECellGetUniqueNodes()
 * @ingroup DataStructure
 *
 * #internalattributes exclude_fglue, exclude_python, exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataFEPolyGetCellNodesSizeAndCenter(FaceMap_pa                          FaceMap,
                                                                         ElemToFaceMap_pa                    ElemToFaceMap,
                                                                         LgIndex_t                           CellIndex,
                                                                         FieldData_pa                        XFieldData,
                                                                         FieldData_pa                        YFieldData,
                                                                         FieldData_pa                        ZFieldData,
                                                                         TP_OUT LgIndex_t*                   NumUniqueNodes,
                                                                         TP_IN_OUT LgIndex_t*                UniqueNodesSize,
                                                                         TP_ARRAY_RECEIVES_GIVES LgIndex_t** UniqueNodes,
                                                                         TP_OUT double*                      CellSize,
                                                                         TP_OUT XYZ_s*                       CellCenter);
/**
 * Gets the logically unique nodes of an entire finite element cell or, for
 * finite element volume data only, a finite element face.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.2-0-398
 *
 * @param Zone
 *   Zone in which the cell exits. This must be a finite element zone.
 * @param FaceOffset
 *   For the unique nodes of an entire cell this value must be set to zero or,
 *   for finite element volume data only, this value must be set to the desired
 *   face number.
 * @param CellIndex
 *   The cell index to query.
 * @param NumUniqueNodes
 *   Pointer to the resulting number of unique nodes.
 * @param UniqueNodesSize
 *   As input this value is a pointer to the current dimension of the
 *   UniqueNodes array or a pointer to a dimension value of zero if the
 *   UniqueNodes array has not yet been allocated.
 *   @par
 *   As output it is a pointer to the resulting dimension of the UniqueNodes
 *   array which is at least large enough to hold the unique nodes.
 * @param UniqueNodes
 *   As input this value is a pointer to NULL or a pointer to an allocated
 *   array dimensioned as specified by UniqueNodesSize items.
 *   @par
 *   As output it is a pointer to the resulting allocated or reallocated array
 *   containing the unique nodes. The resulting array is only reallocated if
 *   the number of unique nodes exceeds the value pointed to by
 *   UniqueNodesSize.
 *   @par
 *   If you supply a pre-allocated array for UniqueNodes that was not allocated
 *   by a previous call to this function, it MUST be sized large enough to hold
 *   the requested unique nodes. If it were not, Tecplot would attempt to
 *   release the undersized resource causing undefined behavior because
 *   Tecplot's allocator/deallocator is different than that of an add-on.
 *   @par
 *   If UniqueNodes was allocated by a call to this function you must
 *   deallocated it when no longer needed by a call to TecUtilArrayDealloc().
 *
 * @return
 *     TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>NumUniqueNodes</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>UniqueNodes</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * Get the unique element nodes for an cell 362 and 535 of zone 5 letting
 * Tecplot allocate the array of unique nodes.
 * @code
 *   EntIndex_t       Zone       = 5; // ...assume that this is an FE zone
 *   ElemFaceOffset_t FaceOffset = 0; // ...0 because we want the unique nodes for the entire cell
 *   LgIndex_t        NumUniqueNodes;
 *   LgIndex_t        UniqueNodesSize = 0;    // ...let Tecplot allocate
 *   LgIndex_t       *UniqueNodes     = NULL; // ...let Tecplot allocate
 *   IsOk = TecUtilDataFECellGetUniqueNodes(Zone, FaceOffset, 362,
 *                                          &NumUniqueNodes,
 *                                          &UniqueNodesSize,
 *                                          &UniqueNodes);
 *   if (IsOk)
 *     {
 *       ...do something useful with the unique nodes of cell 362
 *     }
 *
 *   // Using the previously allocate array request the unique nodes of cell 535.
 *   // Note that the array may get reallocated if required to hold the unique
 *   // nodes of cell 535.
 *   if (IsOk)
 *     {
 *       IsOk = TecUtilDataFECellGetUniqueNodes(Zone, FaceOffset, 535,
 *                                              &NumUniqueNodes,
 *                                              &UniqueNodesSize,
 *                                              &UniqueNodes);
 *     }
 *
 *   if (IsOk)
 *     {
 *       ...do something useful with the unique nodes of cell 535
 *     }
 *
 *   // cleanup
 *   TecUtilArrayDealloc(&((void *)UniqueNodes));
 * @endcode
 *
 * Get the unique face nodes for a classic FE cell where we supply an automatic
 * variable that is guaranteed to be large enough to hold the unique face
 * nodes.
 * @code
 *   EntIndex_t       Zone       = 3; // ...assume that this is a classic FE brick zone
 *   ElemFaceOffset_t FaceOffset = 2; // ...face 2 of the brick
 *   LgIndex_t        NumUniqueNodes;
 *   LgIndex_t        UniqueNodesSize = 4;    // ...classic FE zones have at most 4 nodes per face
 *   LgIndex_t        ClassicUniqueNodes[4];  // ...classic FE zones have at most 4 nodes per face
 *   IsOk = TecUtilDataFECellGetUniqueNodes(Zone, FaceOffset, 276,
 *                                          &NumUniqueNodes,
 *                                          &UniqueNodesSize,
 *                                          &UniqueNodes);
 *   if (IsOk)
 *     {
 *       ...do something useful with the unique face nodes of cell 276, face 2
 *     }
 *
 *   // using the same array request the unique face nodes of cell 657, face 6
 *   if (IsOk)
 *     {
 *       FaceOffset = 6; // ...face 6 of the brick
 *       IsOk = TecUtilDataFECellGetUniqueNodes(Zone, FaceOffset, 657,
 *                                              &NumUniqueNodes,
 *                                              &UniqueNodesSize,
 *                                              &UniqueNodes);
 *     }
 *
 *   if (IsOk)
 *     {
 *       ...do something useful with the unique face nodes of cell 657, face 6
 *     }
 *
 *   // no need to cleanup because the unique nodes array should never have been resized
 *
 * @endcode
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilDataFEPolyGetCellNodesSizeAndCenter()
 * @ingroup DataStructure
 *
 * #internalattributes exclude_fglue, exclude_python, exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataFECellGetUniqueNodes(EntIndex_t                          Zone,
                                                              ElemFaceOffset_t                    FaceOffset,
                                                              LgIndex_t                           CellIndex,
                                                              TP_OUT LgIndex_t*                   NumUniqueNodes,
                                                              TP_IN_OUT LgIndex_t*                UniqueNodesSize,
                                                              TP_ARRAY_RECEIVES_GIVES LgIndex_t** UniqueNodes);

/**
 * Get the indices for the nodes of a finite-element cell.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   Zone in which the cell exists. This must be a finite-element zone,
 *   but not polyhedral or polygonal.
 *
 * @param Face
 *   Face of the finite-element cell. If the zone uses tetrahedrons this is a
 *   number between one and four. If the zone uses bricks this is a number
 *   between one and six. If the zone uses triangles or quadrilaterals then
 *   this is ignored.
 *
 * @param CellIndex
 *   The cell index (that is, the element number) to query.
 *
 * @param I1
 *   First Node index or zero if the node map could not be loaded.
 *
 * @param I2
 *   Second Node index or zero if the node map could not be loaded.
 *
 * @param I3
 *   Third Node index or zero if the node map could not be loaded. For linear
 *   element type this is a repeat of I1.
 *
 * @param I4
 *   Fourth Node index or zero if the node map could not be loaded. For
 *   triangle or linear element types this is a repeat I1.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>I1</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>I2</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>I3</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>I4</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataFECellGetNodes(
 *   &           Zone,
 *   &           Face,
 *   &           CellIndex,
 *   &           I1,
 *   &           I2,
 *   &           I3,
 *   &           I4)
 *    INTEGER*4       Zone
 *    INTEGER*4       Face
 *    INTEGER*4       CellIndex
 *    INTEGER*4       I1
 *    INTEGER*4       I2
 *    INTEGER*4       I3
 *    INTEGER*4       I4
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the node indices for face number 2 of cell number 23 in zone 3:
 *
 * @code
 *   LgIndex_t I1,I2,I3,I4;
 *
 *   // Add code here to make sure data exists, and the zone
 *   // exists and is the right size and type
 *   TecUtilDataFECellGetNodes(3,
 *                             2,
 *                             23,
 *                             &I1,&I2,&I3,&I4);
 * @endcode
 *
 * @ingroup DataStructure
 *
 */
LINKTOADDON void STDCALL TecUtilDataFECellGetNodes(EntIndex_t        Zone,
                                                   int               Face,
                                                   LgIndex_t         CellIndex,
                                                   TP_OUT LgIndex_t* I1,
                                                   TP_OUT LgIndex_t* I2,
                                                   TP_OUT LgIndex_t* I3,
                                                   TP_OUT LgIndex_t* I4);

/**
 * Get the indices for the nodes a cell in an ordered zone.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   Zone in which the cell exists
 *
 * @param Plane
 *   Plane in which the cell resides. The possible values are: IJKPlanes_I,
 *   IJKPlanes_J, IJKPlanes_K or IJKPlanes_Volume. For I- or IJ-ordered data use
 *   IJKPlanes_K. For IJK-ordered data this determines which of the three faces
 *   (I, J, or K) to use to determine which cell to query.
 *
 * @param CellIndex
 *   The index of the lowest indexed corner of the cell to query
 *
 * @param I1
 *   First node index for the cell. If the zone is IJ-ordered or IJK-ordered,
 *   these indices are calculated from treating the two- or three-dimensional
 *   array as a one-dimensional array.
 *
 * @param I2
 *   Second node index for the cell. If the zone is IJ-ordered or IJK-ordered,
 *   these indices are calculated from treating the two- or three-dimensional
 *   array as a one-dimensional array.
 *
 * @param I3
 *   Third node index for the cell. If the zone is IJ-ordered or IJK-ordered,
 *   these indices are calculated from treating the two- or three-dimensional
 *   array as a one-dimensional array.
 *
 * @param I4
 *   Fourth node index for the cell. If the zone is IJ-ordered or IJK-ordered,
 *   these indices are calculated from treating the two- or three-dimensional
 *   array as a one-dimensional array.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>I1</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>I2</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>I3</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>I4</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataIJKCellGetIndices(
 *   &           Zone,
 *   &           Plane,
 *   &           CellIndex,
 *   &           I1,
 *   &           I2,
 *   &           I3,
 *   &           I4)
 *    INTEGER*4       Zone
 *    INTEGER*4       Plane
 *    INTEGER*4       CellIndex
 *    INTEGER*4       I1
 *    INTEGER*4       I2
 *    INTEGER*4       I3
 *    INTEGER*4       I4
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the node indices for the face that lies in the J-plane of the cell that
 * resides at (1, 1, 1) in zone 2 which is an IJK-ordered data set:
 *
 * @code
 *   LgIndex_t I1,I2,I3,I4;
 *
 *   // Add code here to make sure data exists and the zone
 *   // exists and is the right size and type
 *   TecUtilDataIJKCellGetIndices(2,
 *                                IJKPlanes_J,
 *                                1,
 *                                &I1,&I2,&I3,&I4);
 *
 *   LgIndex_t I1,I2,I3,I4;
 *   LgIndex_t CellIndex;
 *
 *   // Add code here to make sure data exists, and the zone
 *   // exists and is the right size and type
 *
 *   CellIndex = 2+10*((5-1)+20*(13-1));
 *
 *   TecUtilDataIJKCellGetIndices(2,
 *                                IJKPlanes_K,
 *                                CellIndex,
 *                                &I1,&I2,&I3,&I4);
 * @endcode
 *
 * Get the nodel indices for the face that lies in the K-plane of the cell that
 * resides at (2, 5, 13) in zone 2, which is an IJK-ordered data set
 * dimensioned 10 by 20 by 30:
 *
 * @code
 *
 *   LgIndex_t I1,I2,I3,I4;
 *   LgIndex_t CellIndex;
 *
 *   // Add code here to make sure data exists, and the zone
 *   // exists and is the right size and type
 *
 *   CellIndex = 2+10*((5-1)+20*(13-1));
 *
 *   TecUtilDataIJKCellGetIndices(2,
 *                                IJKPlanes_K,
 *                                CellIndex,
 *                                &I1,&I2,&I3,&I4);
 * @endcode
 *
 * @ingroup DataStructure
 *
 */
LINKTOADDON void STDCALL TecUtilDataIJKCellGetIndices(EntIndex_t        Zone,
                                                      IJKPlanes_e       Plane,
                                                      LgIndex_t         CellIndex,
                                                      TP_OUT LgIndex_t* I1,
                                                      TP_OUT LgIndex_t* I2,
                                                      TP_OUT LgIndex_t* I3,
                                                      TP_OUT LgIndex_t* I4);




/**
 * Low level function used to get most page, frame, and general attribute
 * values in Tecplot. The parameters to TecUtilStyleGetLowLevelX() mimic the
 * Macro Frame SetValue Commands described in the Tecplot Reference Manual.
 *
 * @par Note:
 *   This function is to be used with caution.  This function ONLY operates on the
 *   argument list rules for the current version of Tecplot and no attempt is made
 *   for backward compatibility.   Any calls to this function must be
 *   re-inspected when a new version of Tecplot is release to make sure the
 *   arguement list ordering is still valid.
 *
 * @par Note:
 *   The ArgList entries described below define the attributes to get.
 *   Attributes in Tecplot are defined hierarchically. These parameters follow
 *   the same order as you would use when constructing a macro command to set a
 *   value. These parameters are actually strings, but you should use the
 *   supplied SV_constants from the SV.h include file. Using the SV_ constants
 *   will help prevent misspellings and other errors.  At the time of printing,
 *   only the following SV_P1 commands are available: SV_FIELDMAP, SV_LINEMAP,
 *   SV_GLOBALCONTOUR, SV_GLOBALSCATTER, SV_BASICCOLORLEGEND, SV_BLANKING,
 *   SV_GLOBALEDGE, SV_GLOBALRGB, SV_ISOSURFACEATTRIBUTES, SV_STREAMATTRIBUTES,
 *   SV_SLICEATTRIBUTES, SV_GLOBALTWODVECTOR, SV_GLOBALTHREEDVECTOR,
 *   SV_GLOBALLINEPLOT, SV_XYLINEAXIS, SV_POLARAXIS, SV_SKETCHAXIS,
 *   SV_TWODAXIS, SV_THREEDAXIS, and SV_FRAMELAYOUT. Some sub-commands are
 *   not fully supported. Unsupported sub-commands will return
 *   \ref GetValueReturnCode_SyntaxError.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_UNIQUEID
 * Type:
 *   ArbParam_t
 * Arg Function:
 *   TecUtilArgListAppendArbParam()
 * Default:
 *   The current page's or frame's unique ID.
 * Required:
 *   No
 * Notes:
 *   Page and frame style queries are permitted for non-current pages and
 *   frames by supplying the unique ID of the desired page or frame via this
 *   argument. Unique IDs for pages or frames are acquired via calls to
 *   TecUtilPageGetUniqueID() or TecUtilFrameGetUniqueID(). Page style queries
 *   are those whose SV_P1 value is assigned the value SV_PAGE while frame
 *   style queries are those whose SV_P1 value is assigned one of the following
 *   values: SV_PLOTTYPE, SV_FRAMENAME, SV_ACTIVEFIELDMAPS, SV_FIELDMAP,
 *   SV_FIELDLAYERS, SV_ISOSURFACELAYERS, SV_SLICELAYERS, SV_STREAMTRACELAYERS,
 *   SV_GLOBALEDGE, SV_GLOBALRGB, SV_GLOBALCONTOUR, SV_GLOBALTIME,
 *   SV_GLOBALTHREEDVECTOR, SV_GLOBALTWODVECTOR, SV_GLOBALSCATTER,
 *   SV_BASICCOLORLEGEND, SV_BLANKING, SV_STREAMATTRIBUTES,
 *   SV_ISOSURFACEATTRIBUTES, SV_SLICEATTRIBUTES, SV_GLOBALTHREED,
 *   SV_GLOBALPOLAR, SV_GLOBALLINEPLOT, SV_LINEMAP, SV_ACTIVELINEMAPS,
 *   SV_LINEPLOTLAYERS, SV_BASETEXT, SV_BASEGEOM, SV_SKETCHAXIS, SV_XYLINEAXIS,
 *   SV_TWODAXIS, SV_THREEDAXIS, SV_POLARAXIS, SV_FRAMELAYOUT, SV_LINKING,
 *   SV_THREEDVIEW, or SV_POLARVIEW.
 *
 * Name:
 *   SV_P1
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   ---
 * Required:
 *   No
 * Notes:
 *
 * Name:
 *   SV_P2
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   ---
 * Required:
 *   No
 *
 * Name:
 *   SV_P3
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   ---
 * Required:
 *   No
 *
 * Name:
 *   SV_P4
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   ---
 * Required:
 *   No
 *
 * Name:
 *   SV_P5
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   ---
 * Required:
 *   No
 *
 * Name:
 *   SV_OFFSET1
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   ---
 * Required:
 *   No
 * Notes:
 *   Depending on the command the first offset is used to denote the zone, line
 *   map, or contour group of interest.
 *
 * Name:
 *   SV_OFFSET2
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   ---
 * Required:
 *   No
 * Notes:
 *   The second offset is only used if the first is already being used to
 *   identify a zone, linemap or contour group such as color map override
 *   number for a specified contour group
 *
 * Name:
 *   SV_DVALUE
 * Type:
 *   double *
 * Arg Function:
 *   TecUtilArgListAppendDoublePtr()
 * Default:
 *   ---
 * Required:
 *   No
 * Notes:
 *   Address to a variable of type double where the double valued result of the
 *   query can be stored
 *
 * Name:
 *   SV_IVALUE
 * Type:
 *   ArbParam_t *
 * Arg Function:
 *   TecUtilArgListAppendArbParamPtr()
 * Default:
 *   ---
 * Required:
 *   No
 * Notes:
 *   Address to a variable of type ArbParam_t where the ArbParam_t valued
 *   result of the query can be stored. Note that some queries assign values
 *   that were allocated and must be deallocated by the addon. If the resulting
 *   ArbParam_t value is a string it must be deallocated using
 *   TecUtilStringDealloc(). If is is an array it must be deallocated using
 *   TecUtilArrayDealloc().
 * </ArgListTable>
 *
 * @return
 *   The function return value is of type GetValueReturnCode_e with the
 *   following possible values:
 *
 * @verbatim
     GetValueReturnCode_Ok              Value was assigned to either the double
                                        or ArbParam_t value given.

     GetValueReturnCode_ResultTypeError Resulting type mismatch with the supplied
                                        DValue or IValue.

     GetValueReturnCode_SyntaxError     SV_P# parameters did not follow the macro
                                        command syntax. The parameters must mimic the
                                        macro command language and only for the
                                        branches of the language tree that are
                                        available.
   @endverbatim
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStyleGetLowLevelX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Make some miscellaneous queries:
 *
 * @code
 *   ArgList_pa   ArgList;
 *   ArbParam_t   IValue;
 *   double       DValue;
 *   GetValueReturnCode_e GVRC;
 *
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *
 *   // get mesh color for zone 2 ...assuming Tecplot's plot type is Cartesian
 *   TecUtilArgListClear(ArgList);
 *   TecUtilArgListAppendInt(ArgList,         SV_OFFSET1, 2);
 *   TecUtilArgListAppendString(ArgList,      SV_P1,      SV_FIELDMAP);
 *   TecUtilArgListAppendString(ArgList,      SV_P2,      SV_MESH);
 *   TecUtilArgListAppendString(ArgList,      SV_P3,      SV_COLOR);
 *   TecUtilArgListAppendArbParamPtr(ArgList, SV_IVALUE,  (ArbParam_t*)&IValue);
 *   GVRC = TecUtilStyleGetLowLevelX(ArgList);
 *   if (GVRC == GetValueReturnCode_Ok)
 *     {
 *       ColorIndex_t MeshColor = (ColorIndex_t)IValue;
 *       printf("Zone 2's mesh color is %d\n", MeshColor);
 *     }
 *
 *   // get the line thickness of zone 3... same assumptions as above
 *   TecUtilArgListClear(ArgList);
 *   TecUtilArgListAppendInt(ArgList,       SV_OFFSET1, 3);
 *   TecUtilArgListAppendString(ArgList,    SV_P1,      SV_FIELDMAP);
 *   TecUtilArgListAppendString(ArgList,    SV_P2,      SV_MESH);
 *   TecUtilArgListAppendString(ArgList,    SV_P3,      SV_LINETHICKNESS);
 *   TecUtilArgListAppendDoublePtr(ArgList, SV_DVALUE,  &DValue);
 *   GVRC = TecUtilStyleGetLowLevelX(ArgList);
 *   if (GVRC == GetValueReturnCode_Ok)
 *     {
 *       double MeshLineThickness = DValue;
 *       printf("Zone 3's mesh line thickness is %lg\n", MeshLineThickenss);
 *     }
 *
 *   // get the positive prefix number for contour group 1's contour legend
 *   TecUtilArgListClear(ArgList);
 *   TecUtilArgListAppendInt(ArgList,         SV_OFFSET1, 1);
 *   TecUtilArgListAppendString(ArgList,      SV_P1,      SV_GLOBALCONTOUR);
 *   TecUtilArgListAppendString(ArgList,      SV_P2,      SV_LEGEND);
 *   TecUtilArgListAppendString(ArgList,      SV_P3,      SV_NUMFORMAT);
 *   TecUtilArgListAppendString(ArgList,      SV_P3,      SV_POSITIVEPREFIX);
 *   TecUtilArgListAppendArbParamPtr(ArgList, SV_IVALUE,  (ArbParam_t*)&IValue);
 *   if (GVRC == GetValueReturnCode_Ok)
 *     {
 *       char *PositivePrefixStr = (char *)IValue;
 *       if (PositivePrefixStr != NULL)
 *         {
 *           printf("Positive prefix number format for "
 *                  "contour group 1's contour legend is:%s\n",
 *                  PositivePrefixStr);
 *           TecUtilStringDealloc(&PositivePrefixStr);
 *         }
 *       else
 *         printf("Positive prefix number format for "
 *                "contour group 1's contour legend "
 *                "was not specified.\n");
 *     }
 *
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup StyleValue
 *
 */
LINKTOADDON GetValueReturnCode_e STDCALL TecUtilStyleGetLowLevelX(ArgList_pa ArgList);

/**
 * If the last call to TecUtilStyleSetLowLevel or TecUtilStyleSetLowLevelX
 * returns an error, this function may be called to retrieve the
 * error message generated by Tecplot
 *
 * @return
 *     Returns a copy of the error message. This string must be deallocated when you are done using
 *     it. NULL is a valid return value and indicates no error was recorded.
 *
 * @sa TecUtilLastErrorMessage(), TecUtilStyleSetLowLevel, TecUtilStyleSetLowLevelX
 *
 * @ingroup StyleValue
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilStyleGetLastErrorString(void);

/**
 * Returns the last recorded error message by Tecplot. The operation does not modify Tecplot's
 * internal state.
 *
 * @return
 *     Returns a copy of the error message. This string must be deallocated when you are done using
 *     it. NULL is a valid return value and indicates no error was recorded.
 *
 * @since 14.1
 *
 * @sa TecUtilDialogLastMessageBox, TecUtilStyleGetLastErrorString, TecUtilStyleSetLowLevelX
 *
 * @ingroup UserInterface
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilLastErrorMessage(void);

/**
 * Clears the last error message queue such that immediately calling TecUtilLastErrorMessage() will
 * return NULL. If you intend on calling TecUtilLastErrorMessage() after calling another TecUtil
 * function you should call TecUtilLastErrorMessageClear() prior to making the TecUtil call.
 *
 * @since 14.1
 *
 * @code
 *   {
 *       TecUtilLastErrorMessageClear();
 *       Boolean_t isOk = TecUtilDataSetDeleteZone(zoneList);
 *       if (!isOk)
 *       {
 *           char* errorMessage = TecUtilLastErrorMessage();
 *           if (errorMessage != 0)
 *           {
 *               ...
 *           }
 *       }
 *   }
 * @endcode
 *
 * @sa TecUtilLastErrorMessage, TecUtilStyleGetLastErrorString, TecUtilStyleSetLowLevel,
 * TecUtilStyleSetLowLevelX
 *
 * @ingroup UserInterface
 */
LINKTOADDON void STDCALL TecUtilLastErrorMessageClear(void);

/**
 *   Queries a zone attribute. You can use this function to query any plot
 *   attribute that is not a floating point value. To query a floating point
 *   plot attribute, use TecUtilFieldStyleGetDoubleValue().
 *
 * @return
 *   The queried attribute. This must be cast to the appropriate type.
 *
 * @param Zone
 *   Zone number to query
 * @param S1
 *   First parameter that defines the attribute to query. The parameters follow
 *   the same order that you would use when constructing a set value macro
 *   command. If a parameter is not used, then it must be NULL.  If you are not
 *   sure of the possible values for an enumerated type, you can find the
 *   definitions in the Include/GLOBAL.h directory, below the Tecplot home
 *   directory.  These parameters are actually strings, but you can use the
 *   supplied SV_ constants from the SV.h include file. Using the SV_ constants
 *   will help prevent misspellings and other errors.
 * @param S2
 *   Second parameter that defines the attribute to query. See S1.
 * @param S3
 *   Third parameter that defines the attribute to query. See S1.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilFieldStyleGetArbValue(
 *   &           Zone,
 *   &           S1,
 *   &           S2,
 *   &           S3,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    CHARACTER*(*)   S1
 *    CHARACTER*(*)   S2
 *    CHARACTER*(*)   S3
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Query the mesh color for zone 2.
 *
 * @code
 *   {
 *     // Equivalent macro command to set the color is:
 *     // $!FIELD [2] MESH {Color = ...}
 *
 *     ColorIndex_t MeshColor;
 *     ArbParam_t   Result;
 *
 *     TecUtilLockStart(AddOnID);
 *
 *     MeshColor = (ColorIndex_t)
 *     TecUtilFieldStyleGetArbValue(2,SV_MESH,SV_COLOR,NULL);
 *
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON ArbParam_t STDCALL TecUtilFieldStyleGetArbValue(EntIndex_t  Zone,
                                                            const char *S1,
                                                            const char *S2,
                                                            const char *S3);




/**
 *   Queries a zone attribute. You can use this function to query any plot
 *   attribute that is a floating point value. To query a non-floating point
 *   plot attribute, use TecUtilFieldStyleGetArbValue().
 *
 * @param Zone
 *   Zone number to query.
 *
 * @param S1
 *   First parameter used to define the attribute to query. The parameters
 *   follow the same order that you would use when constructing the $!FIELD
 *   macro command.
 *
 * @param S2
 *   Second parameter used to define the attribute to query. See S1.
 *
 * @param S3
 *   Third parameter used to define the attribute to query. See S1.
 *
 * @return
 *   The queried attribute.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilFieldStyleGetDoubleValue(
 *   &                   Zone,
 *   &                   S1,
 *   &                   S2,
 *   &                   S3)
 *    INTEGER*4       Zone
 *    CHARACTER*(*)   S1
 *    CHARACTER*(*)   S2
 *    CHARACTER*(*)   S3
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Query the mesh pattern length for for zone 2.
 *
 * @code
 *   {
 *     // Equivalent macro command to set the color is:
 *     //   $!FIELD [2] MESH {P  ATTERNLENGTH = ... }
 *     double  MeshPatternLength;
 *
 *     TecUtilLockStart(AddOnID);
 *
 *     MeshPatternLength = TecUtilFieldStyleGetDoubleValue(2,SV_MESH,
 *                                                         SV_PATTERNLENGTH,NULL);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON double STDCALL TecUtilFieldStyleGetDoubleValue(EntIndex_t  Zone,
                                                           const char *S1,
                                                           const char *S2,
                                                           const char *S3);



/**
 * @deprecated
 *   Please use TecUtilLineMapStyleGetArbValue() instead.
 *
 * @ingroup LineMap
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON ArbParam_t STDCALL TecUtilXYMapStyleGetArbValue(EntIndex_t  XYMap,
                                                            const char *S1,
                                                            const char *S2,
                                                            const char *S3);


/**
 * Queries an Line-map attribute of the specified frame. You can use this function to query any
 * Line-map attribute that is not a floating point value. To query a floating
 * point attribute, use TecUtilLineMapStyleGetDoubleValue().
 *
 * @param FrameID
 *   Unique ID of a frame for which the query should be made.
 *
 * @param LineMap
 *   Line-map number to query
 *
 * @param S1
 *   First parameter used to define the attribute to query. The parameters
 *   follow the same order that you would use when constructing a set value
 *   macro command. If a parameter is not used, then it must be NULL.These
 *   parameters are actually strings, but you can use the supplied SV_
 *   constants from the SV.h include file. Using the SV_ constants will help
 *   prevent misspellings and other errors.If you are not sure of the possible
 *   values for an enumerate type, you can find the definitions in
 *   Include/GLOBAL.h below the Teclot home directory.
 *
 * @param S2
 *   Second parameter used to define the attribute to query. See S1.
 *
 * @param S3
 *   Third parameter used to define the attribute to query. See S1.This
 *   function will assert if the combination of parameters or the LineMap
 *   number is invalid.
 *
 * @return
 *   The queried attribute. This must be cast to the appropriate type (see the
 *   table below). If the return type is a char *, then you must call
 *   TecUtilStringDealloc() to free the string.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilLineMapStyleGetArbValueForFrame(
 *   &           FrameID,
 *   &           LineMap,
 *   &           S1,
 *   &           S2,
 *   &           S3,
 *   &           ResultPtr)
 *    INTEGER*4       FrameID 
 *    INTEGER*4       LineMap
 *    CHARACTER*(*)   S1
 *    CHARACTER*(*)   S2
 *    CHARACTER*(*)   S3
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Query the name of Line-map number 2 for the frame with ID=1:
 *
 * @code
 *   {
 *     char *LineMapName = NULL;
 *
 *     TecUtilLockStart(AddOnID);
 *
 *     MapName = (const char *) TecUtilLineMapStyleGetArbValueForFrame(1,2,SV_NAME,NULL,NULL);
 *
 *     // Use MapName.
 *
 *     TecUtilStringDealloc(&MapName); // When finished
 *
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 *    Get the error bar variable for line map 7 for the frame with ID=1:
 *
 * @code
 *     EntIndex_t ErrorBarVar;
 *
 *     TecUtilLockStart(AddOnID);
 *     LineThickness = (EntIndex_t)TecUtilLineMapStyleGetArbValueForFrame(1,
 *                                                                        7,
 *                                                                        SV_ERRORBARS,
 *                                                                        SV_VAR,
 *                                                                        NULL);
 *     TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @since 14.1
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON ArbParam_t STDCALL TecUtilLineMapStyleGetArbValueForFrame(UniqueID_t FrameID,
                                                                      EntIndex_t  LineMap,
                                                                      const char *S1,
                                                                      const char *S2,
                                                                      const char *S3);

/**
 * Queries an Line-map attribute. You can use this function to query any
 * Line-map attribute that is not a floating point value. To query a floating
 * point attribute, use TecUtilLineMapStyleGetDoubleValue().
 *
 * @param LineMap
 *   Line-map number to query
 *
 * @param S1
 *   First parameter used to define the attribute to query. The parameters
 *   follow the same order that you would use when constructing a set value
 *   macro command. If a parameter is not used, then it must be NULL.These
 *   parameters are actually strings, but you can use the supplied SV_
 *   constants from the SV.h include file. Using the SV_ constants will help
 *   prevent misspellings and other errors.If you are not sure of the possible
 *   values for an enumerate type, you can find the definitions in
 *   Include/GLOBAL.h below the Teclot home directory.
 *
 * @param S2
 *   Second parameter used to define the attribute to query. See S1.
 *
 * @param S3
 *   Third parameter used to define the attribute to query. See S1.This
 *   function will assert if the combination of parameters or the LineMap
 *   number is invalid.
 *
 * @return
 *   The queried attribute. This must be cast to the appropriate type (see the
 *   table below). If the return type is a char *, then you must call
 *   TecUtilStringDealloc() to free the string.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilLineMapStyleGetArbValue(
 *   &           LineMap,
 *   &           S1,
 *   &           S2,
 *   &           S3,
 *   &           ResultPtr)
 *    INTEGER*4       LineMap
 *    CHARACTER*(*)   S1
 *    CHARACTER*(*)   S2
 *    CHARACTER*(*)   S3
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Query the name of Line-map number 2.
 *
 * @code
 *   {
 *     // Equivalent macro command to set the color is: $!LINEMAP [2] NAME = "..."
 *     char *LineMapName = NULL;
 *
 *     TecUtilLockStart(AddOnID);
 *
 *     MapName = (const char *) TecUtilLineMapStyleGetArbValue(2,SV_NAME,NULL,NULL);
 *
 *     // Use MapName.
 *
 *     TecUtilStringDealloc(&MapName); // When finished
 *
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 *    Get the error bar variable for line map 7:
 *
 * @code
 *     EntIndex_t ErrorBarVar;
 *
 *     TecUtilLockStart(AddOnID);
 *     LineThickness = (EntIndex_t)TecUtilLineMapStyleGetArbValue(7,
 *                                                                SV_ERRORBARS,
 *                                                                SV_VAR,
 *                                                                NULL);
 *     TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @sa TecUtilLineMapStyleGetArbValueForFrame
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON ArbParam_t STDCALL TecUtilLineMapStyleGetArbValue(EntIndex_t  LineMap,
                                                              const char *S1,
                                                              const char *S2,
                                                              const char *S3);


/**
 * @deprecated
 *   Please use TecUtilLineMapStyleGetDoubleValue() instead.
 *
 * @ingroup LineMap
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON double STDCALL TecUtilXYMapStyleGetDoubleValue(EntIndex_t  XYMap,
                                                           const char *S1,
                                                           const char *S2,
                                                           const char *S3);

/**
 * Queries an Line-map attribute. You can use this function to query any
 * attribute that is a floating point value. To query a non-floating point
 * attribute, use TecUtilLineMapStyleGetArbValue().
 *
 * @param LineMap
 *   Line-map number to query
 *
 * @param S1
 *   First parameter used to define the attribute to query. The parameters
 *   follow the same order that you would use when constructing a set value
 *   macro command. If a parameter is not used, then it must be NULL.These
 *   parameters are actually strings, but you can use the supplied SV_
 *   constants from the SV.h include file. Using the SV_ constants will help
 *   prevent misspellings and other errors.If you are not sure of the possible
 *   values for an enumerate type, you can find the definitions in
 *   Include/GLOBAL.h below the Teclot home directory.
 *
 * @param S2
 *   Second parameter used to define the attribute to query. See S1.
 *
 * @param S3
 *   Third parameter used to define the attribute to query. See S1.
 *
 * @return
 *   The queried attribute.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilLineMapStyleGetDoubleValue(
 *   &                   LineMap,
 *   &                   S1,
 *   &                   S2,
 *   &                   S3)
 *    INTEGER*4       LineMap
 *    CHARACTER*(*)   S1
 *    CHARACTER*(*)   S2
 *    CHARACTER*(*)   S3
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Query the pattern length for Line-map number 2.
 *
 * @code
 *   {
 *     // Equivalent macro command to set the pattern
 *     // length is: $!LINEMAP [2] LINES {  PATTERNLENGTH = ... }
 *
 *     double  LinePatternLength;
 *
 *     TecUtilLockStart(AddOnID);
 *
 *     LinePatternLength = TecUtilLineMapStyleGetDoubleValue(2,SV_LINES,
 *                                                           SV_PATTERNLENGTH,
 *                                                           NULL);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON double STDCALL TecUtilLineMapStyleGetDoubleValue(EntIndex_t  LineMap,
                                                             const char *S1,
                                                             const char *S2,
                                                             const char *S3);

/**
 * Determine if Tecplot is currently playing a macro.
 *
 * @since 13.2-0-20865
 *
 * @return
 *   TRUE if Tecplot is playing a macro, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateIsProcessingMacro()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ScriptSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateIsProcessingMacro(void);

/**
 * Determine if Tecplot is currently processing a stylesheet.
 *
 * @since 13.2-0-20865
 *
 * @return
 *   TRUE if Tecplot is processing a stylesheet, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateIsProcessingStylesheet()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ScriptSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateIsProcessingStylesheet(void);

/**
 * Query Tecplot to find out if Tecplot is in the middle of processing the data
 * journal.
 * This function is \ref threadsafe.
 *
 * @return
 *   Returns TRUE if Tecplot is processing the data journal, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateIsProcessingJournal()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup LayoutSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateIsProcessingJournal(void);
/**
 *   Query Tecplot to find out if Tecplot is in the middle of processing a layout.
 *   This function is \ref threadsafe.
 *
 * @return
 *   Returns TRUE if Tecplot is processing a layout, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateIsProcessingLayout()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup LayoutSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateIsProcessingLayout(void);

/**
 * Gets the unique ID for the current frame. A unique ID is an integer value
 * unique to a frame during the Tecplot session. Using the unique ID a frame
 * can be compared to other frames and manipulated via TecUtil calls that take
 * unique IDs.
 * This function is \ref threadsafe.
 *
 * @return
 *   The unique ID for the current frame.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameGetUniqueID()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Push the current frame using its unique ID:
 * @code
 *   {
 *     UniqueID_t ID;
 *     TecUtilLockStart(AddOnID);
 *     ID = TecUtilFrameGetUniqueID();
 *     TecUtilFramePushByUniqueID(ID);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @sa TecUtilFramePopByUniqueID() and TecUtilFramePushByUniqueID().
 * @ingroup FrameManagement
 */
LINKTOADDON UniqueID_t STDCALL TecUtilFrameGetUniqueID(void);

/**
 * Gets the unique ID for the data set in the current frame. A unique ID is an
 * integer value unique to a data set during the Tecplot session. Using the
 * unique ID a data set can be compared to other data sets and manipulated via
 * TecUtil calls that take unique IDs. One such use is to determine if data
 * sets are shared between frames.
 * This function is \ref threadsafe.
 *
 * @return
 *   The unique ID for the data set in the current frame.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetGetUniqueID()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Determine if the datasets of the top 2 frames are the same:
 * @code
 *   {
 *     UniqueID_t ID;
 *     TecUtilLockStart(AddOnID);
 *     ID = TecUtilDataSetGetUniqueID();
 *     TecUtilFramePushTop();
 *     if ( ID == TecUtilDataSetGetUniqueID() )
 *       {
 *         // Datasets are the same for both frames
 *       }
 *     else
 *       {
 *         // Datasets are different
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON UniqueID_t STDCALL TecUtilDataSetGetUniqueID(void);

/**
 * Gets a unique ID for a zone. A unique ID is an integer that uniquely
 * identifies a zone. An addon can use these IDs to internally keep track of a
 * set of zones. TecUtilZoneGetNumByUniqueID() can be used to convert between a
 * unique ID and a zone number.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   Zone number to query.
 *
 * @return
 *   A unique ID for a zone.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetUniqueID(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the UniqueID for zone 1:
 *
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     if ( TecUtilDataSetIsAvailable() && TecUtilZoneIsEnabled(1) )
 *       {
 *         UniqueID_t ID = TecUtilZoneGetUniqueID(1);
 *         ...
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @sa TecUtilZoneGetUniqueIDForFrame
 *
 * @ingroup Zone
 *
 */
LINKTOADDON UniqueID_t STDCALL TecUtilZoneGetUniqueID(EntIndex_t Zone);

/**
 * Gets a unique ID for a zone in the dataset of the specified frame. A unique ID is an integer that
 * uniquely identifies a zone. An addon can use these IDs to internally keep track of a set of zones.
 * TecUtilZoneGetNumByUniqueIDForFrame() can be used to convert between a unique ID and a zone number.
 * This function is \ref threadsafe.
 *
 * @param FrameID
 *   An ID of the frame that is attached to the dataset for which the query is made.
 * @param Zone
 *   Zone number to query.
 *
 * @return
 *   A unique ID for a zone.
 *
 * @pre The frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetUniqueIDForFrame(FrameID, Zone)
 *    INTEGER*4 FrameID
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the UniqueID for zone 1 in frame 1:
 *
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     if ( TecUtilDataSetIsAvailableForFrame(1) && TecUtilZoneIsEnabledForFrame(1, 1) )
 *       {
 *         UniqueID_t ID = TecUtilZoneGetUniqueIDForFrame(1, 1);
 *         ...
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @since 14.1
 *
 * @ingroup Zone
 *
 */
LINKTOADDON UniqueID_t STDCALL TecUtilZoneGetUniqueIDForFrame(UniqueID_t FrameID, 
                                                              EntIndex_t Zone);

/**
 * Gets a unique ID for a variable. A unique ID is an integer that uniquely
 * identifies a variable. An addon can use these IDs to internally keep track
 * of a set of variables.  TecUtilVarGetNumByUniqueID() can be used to convert
 * between a unique ID and a variable number.
 * This function is \ref threadsafe.
 *
 * @param Var
 *   Variable number to query.
 *
 * @return
 *   A unique ID for a variable.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarGetUniqueID(Var)
 *    INTEGER*4 Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the unique ID for variable 1:
 *
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     if ( TecUtilDataSetIsAvailable() && TecUtilVarIsEnabled(1) )
 *       {
 *         UniqueID_t ID = TecUtilVarGetUniqueID(1);
 *         ...
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @sa TecUtilVarGetUniqueIDForFrame
 *
 * @ingroup Variables
 *
 */
LINKTOADDON UniqueID_t STDCALL TecUtilVarGetUniqueID(EntIndex_t Var);

/**
 * Gets a unique ID for a variable inf the dataset of the specified frame. A unique ID is an integer
 * that uniquely identifies a variable. An addon can use these IDs to internally keep track
 * of a set of variables.  TecUtilVarGetNumByUniqueIDForFrame() can be used to convert
 * between a unique ID and a variable number.
 * This function is \ref threadsafe.
 *
 * @param FrameID
 *   An ID of the frame that is attached to the dataset for which the query is made.
 * @param Var
 *   Variable number to query.
 *
 * @return
 *   A unique ID for a variable.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarGetNumByUniqueIDForFrame(FrameID, Var)
 *    INTEGER*4 FrameID
 *    INTEGER*4 Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the unique ID for variable 1 in the frame 1:
 *
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     if ( TecUtilDataSetIsAvailableForFrame(1) && TecUtilVarIsEnabledForFrame(1, 1) )
 *       {
 *         UniqueID_t ID = TecUtilVarGetNumByUniqueIDForFrame(1, 1);
 *         ...
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @since 14.1
 *
 * @ingroup Variables
 *
 */
LINKTOADDON UniqueID_t STDCALL TecUtilVarGetUniqueIDForFrame(UniqueID_t FrameID, 
                                                             EntIndex_t Var);

/**
 * Gets a unique ID for a line map. A unique ID is an integer that uniquely
 * identifies a line map. An addon can use these IDs to internally keep track
 * of a set of line maps. TecUtilLineMapGetNumByUniqueID() can be used to
 * convert between a unique ID and a line map number.
 * This function is \ref threadsafe.
 *
 * @since
 *   10.0-3-129
 *
 * @param LineMap
 *   Line map number to query.
 *
 * @return
 *   A unique ID for a line map.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapGetUniqueID(LineMap)
 *    INTEGER*4 Map
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the unique ID for line map N:
 *
 * @code
 *   ... N is assigned...
 *   {
 *     TecUtilLockStart(AddOnID);
 *     if ( TecUtilDataSetIsAvailable() && TecUtilLineMapGetCount() >= N )
 *       {
 *         UniqueID_t ID = TecUtilLineMapGetUniqueID(N);
 *         ...
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON UniqueID_t STDCALL TecUtilLineMapGetUniqueID(EntIndex_t LineMap);

/**
 * Gets a variable number, given a unique ID.
 * This function is \ref threadsafe.
 *
 * @param UniqueID
 *   Unique ID of the variable
 *
 * @return
 *   The variable number of the variable represented by the unique ID. If there
 *   is no variable number for the given unique ID, the return value is
 *   \ref TECUTILBADID.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarGetNumByUniqueID(UniqueID)
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get a variable number from a unique ID:
 *
 * @code
 *   {
 *     extern UniqueID_t ID; // previously initialized
 *
 *     TecUtilLockStart(AddOnID);
 *     EntIndex_t VarNum = TecUtilVarGetNumByUniqueID(ID);
 *     if (VarNum != TECUTILBADID)
 *       {
 *         ...
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup Variables
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilVarGetNumByUniqueID(UniqueID_t UniqueID);

/**
 * Gets a line map number, given a unique ID.
 * This function is \ref threadsafe.
 *
 * @since
 *   10.0-3-129
 *
 * @param UniqueID
 *   Unique ID of the line map.
 *
 * @return
 *   The line map number of the line map represented by the unique ID. If there
 *   is no line map number for the given unique ID, the return value is
 *   \ref TECUTILBADID.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapGetNumByUniqueID(UniqueID)
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get a line map number from a unique ID:
 *
 * @code
 *   {
 *     extern UniqueID_t ID; // previously initialized
 *
 *     TecUtilLockStart(AddOnID);
 *     if ( TecUtilDataSetIsAvailable() )
 *       {
 *         EntIndex_t MapNum = TecUtilLineMapGetNumByUniqueID(ID);
 *         if (MapNum != TECUTILBADID)
 *           {
 *             ...
 *           }
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilLineMapGetNumByUniqueID(UniqueID_t UniqueID);

/**
 * Gets a zone number, given a unique ID.
 * This function is \ref threadsafe.
 *
 * @param UniqueID
 *   Unique ID of the zone
 *
 * @return
 *   The zone number of the vairable represented by the unique ID. If
 *   there is no zone number for the given unique ID, the return value is
 *   \ref TECUTILBADID.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetNumByUniqueID(UniqueID)
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get a zone number from a unique ID:
 *
 * @code
 *   {
 *     extern UniqueID_t ID; // previously initialized
 *
 *     TecUtilLockStart(AddOnID);
 *     if ( TecUtilDataSetIsAvailable() )
 *       {
 *         EntIndex_t ZoneNum = TecUtilZoneGetNumByUniqueID(ID);
 *         if (ZoneNum != TECUTILBADID)
 *           {
 *             ...
 *           }
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup Zone
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilZoneGetNumByUniqueID(UniqueID_t UniqueID);

/**
 * Gets the number (that is, the index) of a variable based on the variable assignment.
 * This function is \ref threadsafe.
 *
 * @param Var
 *   Variable to get. The frame mode must be 2-D or 3-D.If the frame mode is
 *   2-D, select one of 'X', 'Y', 'U', 'V', 'B', 'C', or 'S'.If the frame mode
 *   is 3-D, select one of 'X','Y','Z','U','V','W', 'B', 'C', or 'S'Table 0-1.
 *   Variable assignment identifiers (Var) and descriptions.
 *
 * @param Var
 *   Description'X' X-axis variable'Y' Y-axis variable'Z' Z-axis variable'U'
 *   U-velocity variable'V' V-velocity variable'W' W-velocity variable'B'
 *   Blanking variable'C' Contouring variable'S' Scatter sizing variable
 *
 * @return
 *   The index (number) of the variable referenced by Var.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set.
 * @pre If current frame's plot type is Sketch, @e Var must be 'B'.
 * @pre If current frame's plot type is XY Line, @e Var must be 'B', 'X', or 'Y'.
 * @pre If current frame's plot type is Polar Line, @e Var must be 'B', 'A', or 'R'.
 * @pre If current frame's plot type is 2D Cartesian, @e Var must be one of 'X', 'Y', 'U', 'V', 'B', 'C', or 'S'.
 * @pre If current frame's plot type is 3D Cartesian, @e Var must be one of 'X', 'Y', 'Z', 'U', 'V', 'W', 'B', 'C', or 'S'.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarGetNumByAssignment(Var)
 *    CHARACTER*(*) Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the index of the 'X' variable:
 *
 * @code
 *   // frame mode must be 2-D or 3-D
 *   EntIndex_t i = TecUtilVarGetNumByAssignment('X');
 * @endcode
 *
 * @ingroup Variables
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilVarGetNumByAssignment(char Var);

/**
 * Gets the number (that is, the index) of a variable based on variable name.
 * This function is \ref threadsafe.
 *
 * @param VarName
 *   Name of the variable. Must not be NULL
 *
 * @return
 *   The index (number) of the variable with name VarName, otherwise
 *   TECUTILSETNOTMEMBER if the variable is not a member of the current frame's
 *   data set.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set.
 *
 * @pre <em>VarName</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarGetNumByName(VarName)
 *    CHARACTER*(*) VarName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the index of the variable Rainfall:
 *
 * @code
 *   EntIndex_t i = TecUtilVarGetNumByName("Rainfall");
 * @endcode
 *
 * @ingroup Variables
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilVarGetNumByName(const char *VarName);

/**
 * Gets the load status of a variable for a zone.
 * This function is \ref threadsafe.
 *
 * @since
 *   12.0-1-3754
 *
 * @param Zone
 *   Number of the zone for which to get the load status.
 *
 * @param Var
 *   Number of the variable for which to get the load status.
 *
 * @return
 *   The load status of the variable. The return value can be one of the
 *   following:
 *   \ref VarStatus_Passive,
 *   \ref VarStatus_Custom,
 *   \ref VarStatus_Map,
 *   \ref VarStatus_Heap,
 *   \ref VarStatus_NotLoaded
 *   \ref VarStatus_Passive indicates that the variable is passive for the zone.
 *   Refer to the <A HREF="../dataformat.pdf" TARGET=_blank>Data Format Guide</A>
 *   for more information on passive variables. \ref VarStatus_Custom indicates
 *   that the variable is loaded on-demand by value. Refer to \ref TecUtilDataValueCustomLOD
 *   for more information on loading variables on-demand by value. \ref VarStatus_Map
 *   indicates that the variable has been loaded and dumped out to a disk cache. \ref VarStatus_Heap
 *   indicates that the variable is currently memory-resident. \ref VarStatus_NotLoaded
 *   indicates that the variable is available for loading but is not yet loaded.
 *
 *
 * @pre <em>VALID_REF(FieldData)</em>
 *   Pointer must be a valid address and non-NULL.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarGetStatus(
 *   &           Zone,
 *   &           Var)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the load status of the third variable of the second zone:
 *
 * @code
 *   EntIndex_t  zone = 2;
 *   EntIndex_t  var  = 3;
 *   VarStatus_e status;
 *   status = TecUtilVarGetStatus(zone, var);
 * @endcode
 *
 * @ingroup Variables
 *
 */
LINKTOADDON VarStatus_e STDCALL TecUtilVarGetStatus(EntIndex_t  Zone,
                                                    EntIndex_t  Var);

/**
 * Gets the load status of a variable for a zone. This function is \ref threadsafe.
 *
 * @since
 *   12.0-1-5558
 *
 * @param FieldData
 *   Handle to the field data. Use TecUtilDataValueGetReadableNativeRef(),
 *   TecUtilDataValueGetReadableDerivedRef(), TecUtilDataValueGetReadableNLRef(),
 *   TecUtilDataValueGetReadableCCRef(), or TecUtilDataValueGetWritableNativeRef() to get
 *   readable or writable handles to the field data.
 *
 * @return
 *   The load status of the variable. The return value can be one of the following:
 *   \ref VarStatus_Passive,
 *   \ref VarStatus_Custom,
 *   \ref VarStatus_Map,
 *   \ref VarStatus_Heap,
 *   \ref VarStatus_NotLoaded
 *   \ref VarStatus_Passive indicates that the variable is passive for the zone.
 *   Refer to the <A HREF="../dataformat.pdf" TARGET=_blank>Data Format Guide</A> for more
 *   information on passive variables. \ref VarStatus_Custom indicates that the variable
 *   is loaded on-demand by value. Refer to \ref TecUtilDataValueCustomLOD for more
 *   information on loading variables on-demand by value. \ref VarStatus_Map indicates
 *   that the variable has been loaded and dumped out to a disk cache. \ref VarStatus_Heap
 *   indicates that the variable is currently memory-resident. \ref VarStatus_NotLoaded
 *   indicates that the variable is available for loading but is not yet loaded.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarGetStatusByRef(
 *   &                   FieldDataPtr)
 *    POINTER         (FieldDataPtr, FieldData)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the load status of the third variable of the second zone:
 *
 * @code
 *   EntIndex_t  zone = 2;
 *   EntIndex_t  var  = 3;
 *   FieldData_pa FieldData = TecUtilDataValueGetReadableNativeRef(zone, var);
 *   VarStatus_s status = TecUtilVarGetStatusByRef(FieldData);
 * @endcode
 *
 * @ingroup Variables
 *
 */
LINKTOADDON VarStatus_e STDCALL TecUtilVarGetStatusByRef(FieldData_pa FieldData);

/**
 * Get a read-only handle to the raw field data in the data set attached to the
 * current frame. If possible this function provides direct access to a Tecplot
 * variable's internal representation. If performance is not a concern consider
 * using TecUtilDataValueGetByRef(). If high performance is essential then use
 * TecUtilDataValueArrayGetByRef() which provide nearly equivalent performance
 * to direct access. Alternatively for high performance consider using the field
 * data's accessor functions by calling TecUtilDataValueRefGetGetFunc(). Note
 * that these high performance functions are a very thin layer over a Tecplot
 * variable's internal representation and unlike the raw field data pointer
 * provided by this function, they are always available.
 * This function is \ref threadsafe.
 *
 * @par Note:
 *     The array is read-only therefore be sure not to change any value. Do not
 *     assume that raw data internal to Tecplot remains in the same location at
 *     all times. Always call this function again after any event where Tecplot
 *     itself may move/alter the raw data.
 *
 * @since
 *     14.1
 *
 * @param FieldData
 *     Valid readable field data handle.
 * @return
 *     The address of the raw field data. NULL may be returned if the type is
 *     too complex. If the type is too complex, you may use
 *     TecUtilDataValueRefGetGetFunc() to get the function that will deal with
 *     the data at a lower level than TecUtilDataValueGetByRef().
 *
 *
 * @pre <em>FieldData</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * @ingroup DataValue
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON const void* STDCALL TecUtilDataValueGetReadableRawPtrByRef(FieldData_pa FieldData);

/**
 * Get a read-only handle to the raw field data in the data set attached to the
 * current frame. If possible this function provides direct access to a Tecplot
 * variable's internal representation. If performance is not a concern consider
 * using TecUtilDataValueGetByRef(). If high performance is essential then use
 * TecUtilDataValueArrayGetByRef() which provide nearly equivalent performance
 * to direct access. Alternatively for high performance consider using the field
 * data's accessor functions by calling TecUtilDataValueRefGetGetFunc(). Note
 * that these high performance functions are a very thin layer over a Tecplot
 * variable's internal representation and unlike the raw field data pointer
 * provided by this function, they are always available.
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   The array is read-only therefore be sure not to change any value. Do not
 *   assume that raw data internal to Tecplot remains in the same location at
 *   all times. Always call this function again after any event where Tecplot
 *   itself may move/alter the raw data.
 *
 * @since
 *   11.0-0-007
 *
 * @param Zone
 *   Number of the zone for which to get the raw field data.
 *
 * @param Var
 *   Number of the variable for which to get the raw field data.
 *
 * @param DataPtr
 *   Receives the address of the raw field data.  May return NULL if
 *   the type is too complex. If the type is too complex, you may use
 *   TecUtilDataValueRefGetGetFunc() and TecUtilDataValueRefGetSetFunc()
 *   to get functions that will deal with the data at a lower level than
 *   TecUtilDataValueGetByRef() and TecUtilDataValueSetByRef().
 *
 * @param FieldDataType
 *   Receives the data type of the raw field data. The following table shows
 *   the possible values for FieldDataType along with the corresponding data
 *   type that DataPtr references:
 *
   @verbatim
     FieldDataType            DataPtr references
     -------------------------------------------
     FieldDataType_Float      float *
     FieldDataType_Double     double *
     FieldDataType_Int32      Int32_t *
     FieldDataType_Int16      Int16_t *
     FieldDataType_Byte       char *
     FieldDataType_Bit        UInt32_t *
     FieldDataType_Invalid    too complex
   @endverbatim
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>DataPtr</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>FieldDataType</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueGetReadableRawPtr(
 *   &           Zone,
 *   &           Var,
 *   &           DataPtr,
 *   &           FieldDataType)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 *    POINTER         (DataPtrPtr, DataPtr)
 *    INTEGER*4       FieldDataType
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the first and second value values of the second variable of zone 5 using
 * a raw data pointer. Note that the first and second values are accessed by
 * zero and one respectively since the raw data is accessed with a zero-base
 * numbering system.
 *
 * @code
 *   EntIndex_t zone = 5;
 *   EntIndex_t var = 2;
 *   void *raw_fd_ptr = NULL;
 *   FieldDataType_e field_data_type;
 *   TecUtilDataValueGetReadableRawPtr(zone, var, &raw_fd_ptr, &field_data_type);
 *   if ( raw_fd_ptr )
 *     {
 *       if ( field_data_type == FieldData_Float )
 *         {
 *           const float *float_ptr = (const float *)raw_fd_ptr;
 *           float v1 = float_ptr[0];
 *           float v2 = float_ptr[1];
 *           ... do something with the data
 *         }
 *     }
 * @endcode
 *
 * @sa TecUtilDataValueGetReadableNativeRef(), TecUtilDataValueGetReadableDerivedRef(),
 *     TecUtilDataValueGetReadableNLRef(), TecUtilDataValueGetReadableCCRef(),
 *     TecUtilDataValueGetWritableNativeRef(), TecUtilDataValueArrayGetByRef(),
 *     TecUtilDataValueArraySetByRef(), TecUtilDataValueRefGetGetFunc(),
 *     and TecUtilDataValueRefGetSetFunc().
 *
 * @ingroup DataValue
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON void STDCALL TecUtilDataValueGetReadableRawPtr(EntIndex_t              Zone, /* <-activex> */
                                                           EntIndex_t              Var,
                                                           TP_ARRAY_OUT void**     DataPtr,
                                                           TP_OUT FieldDataType_e* FieldDataType);

/**
 * Get a read/write handle to the raw field data in the data set attached to
 * the current frame. If possible this function provides direct access to a
 * Tecplot variable's internal representation. If performance is not a concern
 * consider using TecUtilDataValueGetByRef(), TecUtilDataValueSetByRef(). If
 * high performance is essential then use TecUtilDataValueArrayGetByRef(), and
 * TecUtilDataValueArraySetByRef() which provide nearly equivalent preformance
 * to direct access. Alternatively for high performance consider using the
 * field data's accessor functions by calling TecUtilDataValueRefGetGetFunc()
 * and TecUtilDataValueRefGetSetFunc(). Note that these high performance
 * functions are a very thin layer over a Tecplot variable's internal
 * representation and unlike the raw field data pointer provided by this
 * function, they are always available.
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   Do not assume that raw data internal to Tecplot remains in the same
 *   location at all times. Always call this function again after any event
 *   where Tecplot itself may move/alter the raw data. Make sure to call
 *   TecUtilStateChanged() after any field values have changed.
 *
 * @since
 *     14.1
 *
 * @param FieldData
 *     Valid writable field data handle.
 * @return
 *     The address of the raw field data. NULL may be returned if the type is
 *     too complex. If the type is too complex, you may use
 *     TecUtilDataValueRefGetGetFunc() and TecUtilDataValueRefGetSetFunc() to
 *     get functions that will deal with the data at a lower level than
 *     TecUtilDataValueGetByRef() and TecUtilDataValueSetByRef().
 *
 *
 * @pre <em>FieldData</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * @ingroup DataValue
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON void* STDCALL TecUtilDataValueGetWritableRawPtrByRef(FieldData_pa FieldData);

/**
 * Get a read/write handle to the raw field data in the data set attached to
 * the current frame. If possible this function provides direct access to a
 * Tecplot variable's internal representation. If performance is not a concern
 * consider using TecUtilDataValueGetByRef(), TecUtilDataValueSetByRef(). If
 * high performance is essential then use TecUtilDataValueArrayGetByRef(), and
 * TecUtilDataValueArraySetByRef() which provide nearly equivalent preformance
 * to direct access. Alternatively for high performance consider using the
 * field data's accessor functions by calling TecUtilDataValueRefGetGetFunc()
 * and TecUtilDataValueRefGetSetFunc(). Note that these high performance
 * functions are a very thin layer over a Tecplot variable's internal
 * representation and unlike the raw field data pointer provided by this
 * function, they are always available.
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   Do not assume that raw data internal to Tecplot remains in the same
 *   location at all times. Always call this function again after any event
 *   where Tecplot itself may move/alter the raw data. Make sure to call
 *   TecUtilStateChanged() after any field values have changed.
 *
 * @since
 *   11.0-0-007
 *
 * @param Zone
 *   Number of the zone for which to get the raw field data.
 *
 * @param Var
 *   Number of the variable for which to get the raw field data.
 *
 * @param DataPtr
 *   Receives the address of the raw field data.  May return NULL if
 *   the type is too complex. If the type is too complex, you may use
 *   TecUtilDataValueRefGetGetFunc() and TecUtilDataValueRefGetSetFunc()
 *   to get functions that will deal with the data at a lower level than
 *   TecUtilDataValueGetByRef() and TecUtilDataValueSetByRef().
 *
 * @param FieldDataType
 *   Receives the data type of the raw field data. The following table shows
 *   the possible values for FieldDataType along with the corresponding data
 *   type that DataPtr references:
 *
   @verbatim
     FieldDataType            DataPtr references
     -------------------------------------------
     FieldDataType_Float      float *
     FieldDataType_Double     double *
     FieldDataType_Int32      Int32_t *
     FieldDataType_Int16      Int16_t *
     FieldDataType_Byte       char *
     FieldDataType_Bit        UInt32_t *
     FieldDataType_Invalid    too complex
   @endverbatim
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>DataPtr</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>FieldDataType</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueGetWritableRawPtr(
 *   &           Zone,
 *   &           Var,
 *   &           DataPtr,
 *   &           FieldDataType)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 *    POINTER         (DataPtrPtr, DataPtr)
 *    INTEGER*4       FieldDataType
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get and then set the first and second values of the second variable of zone
 * 5 using a raw data pointer. Note that the first and second values are
 * accessed by zero and one respectively since the raw data is accessed with a
 * zero-base numbering system. Be sure to call TecUtilStateChanged() when you
 * change the data in this way:
 *
 * @code
 *   EntIndex_t zone = 5;
 *   EntIndex_t var = 2;
 *   void *raw_fd_ptr = NULL;
 *   FieldDataType_e field_data_type;
 *   TecUtilDataValueGetWritableRawPtr(zone, var, &raw_fd_ptr, &field_data_type);
 *   if ( raw_fd_ptr )
 *     {
 *       if ( field_data_type == FieldData_Float )
 *         {
 *           Set_pa altered_vars = TecUtilSetAlloc(TRUE);
 *           float *float_ptr = (float *)raw_fd_ptr;
 *           float v1 = float_ptr[0];
 *           float v2 = float_ptr[1];
 *           // alter v1 and v2 in some way
 *           v1 = 2.0 * v1;
 *           v2 = 2.0 * v2;
 *           float_ptr[0] = v1;
 *           float_ptr[1] = v2;
 *           // inform Tecplot of var value change
 *           TecUtilSetAddMember(altered_vars, var, TRUE);
 *           TecUtilStateChanged(StateChange_VarsAltered,
 *                               (ArbParam_t)altered_vars);
 *           TecUtilSetDealloc(&altered_vars);
 *         }
 *     }
 * @endcode
 *
 * @sa TecUtilDataValueGetReadableNativeRef(), TecUtilDataValueGetReadableDerivedRef(),
 *     TecUtilDataValueGetReadableNLRef(), TecUtilDataValueGetReadableCCRef(),
 *     TecUtilDataValueGetWritableNativeRef(), TecUtilDataValueArrayGetByRef(),
 *     TecUtilDataValueArraySetByRef(), TecUtilDataValueRefGetGetFunc(),
 *     and TecUtilDataValueRefGetSetFunc().
 *
 * @ingroup DataValue
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON void STDCALL TecUtilDataValueGetWritableRawPtr(EntIndex_t              Zone, /* <-activex> */
                                                           EntIndex_t              Var,
                                                           TP_ARRAY_OUT void**     DataPtr,
                                                           TP_OUT FieldDataType_e* FieldDataType);

/**
 * @deprecated
 *   Please use TecUtilDataValueGetReadableRawPtr() or
 *   TecUtilDataValueGetWritableRawPtr() instead. Calling
 *   TecUtilDataValueGetRawPtr() is equivalent to calling
 *   TecUtilDataValueGetWritableRawPtr() and acquiring the raw data pointer.
 *
 * @ingroup DataServices
 *
 * #internalattributes exclude_python, exclude_sdkdoc, exclude_tcl
 */
LINKTOADDON void STDCALL TecUtilDataValueGetRawPtr(EntIndex_t              Zone, /* <-activex> */
                                                   EntIndex_t              Var,
                                                   TP_ARRAY_OUT void**     DataPtr,
                                                   TP_OUT FieldDataType_e* FieldDataType);

/**
 * @deprecated
 *   Please use TecUtilDataNodeGetReadableRef() or TecUtilDataNodeGetWritableRef()
 *   in conjunction with TecUtilDataNodeArrayGetByRef() or TecUtilDataNodeArraySetByRef()
 *   or else use TecUtilDataNodeGetReadableRawPtr() or TecUtilDataNodeGetWritableRawPtr()
 *   instead. Calling TecUtilDataNodeGetRawPtr() is equivalent to calling TecUtilDataNodeGetWritableRef()
 *   and the acquiring the raw data pointer.
 *
 * @ingroup DataStructure
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON void STDCALL TecUtilDataNodeGetRawPtr(EntIndex_t               Zone,
                                                  TP_ARRAY_OUT NodeMap_t** NodeMapPtr);

/**
 * Get the readable raw pointer from the readable finite-element node map.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *     14.1
 *
 * @par Note:
 *     The array is read-only therefore be sure not to change any value. Do not
 *     assume that raw data internal to Tecplot remains in the same location at
 *     all times. Always call this function again after any event where Tecplot
 *     itself may move/alter the raw data.
 *
 * @return
 *     The address of the raw node map. NULL may be returned if the type is too
 *     complex. If the type is too complex, you may use
 *     TecUtilDataNodeGetReadableRef() in conjunction with
 *     TecUtilDataNodeGetByRef().
 *
 *
 * @pre <em>VALID_REF(NodeMap)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * @ingroup DataServices
 */
LINKTOADDON const NodeMap_t* STDCALL TecUtilDataNodeGetReadableRawPtrByRef(NodeMap_pa NodeMap);

/**
 * Get a readable raw pointer to the finite-element node map of the specified zone
 * in the data set attached to the current frame.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *   12.0-1-5410
 *
 * @par Note:
 *   The array is read-only therefore be sure not to change any value. Do not
 *   assume that raw data internal to Tecplot remains in the same location at
 *   all times. Always call this function again after any event where Tecplot
 *   itself may move/alter the raw data.
 *
 * @param Zone
 *   Number of the zone for which to get the readable raw node map pointer. This
 *   must be a finite-element zone
 *
 * @param NodeMapPtr
 *   Receives the address of the raw node map.  May return NULL if
 *   the type is too complex. If the type is too complex, you may use
 *   TecUtilDataNodeGetReadableRef() in conjunction with TecUtilDataNodeGetByRef().
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>NodeMapPtr</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataNodeGetReadableRawPtr(
 *   &           Zone,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataServices
 */
LINKTOADDON void STDCALL TecUtilDataNodeGetReadableRawPtr(EntIndex_t               Zone,
                                                          TP_ARRAY_OUT NodeMap_t** NodeMapPtr);

/**
 * Get the writable raw pointer from the writable finite-element node map.
 *
 * This function is \ref threadsafe.
 *
 * @par Note:
 *     Do not assume that raw data internal to Tecplot remains in the same
 *     location at all times. Always call this function again after any event
 *     where Tecplot itself may move/alter the raw data. Make sure to call
 *     TecUtilStateChanged() after any values have changed.
 *
 * @since
 *     14.1
 *
 * @return
 *     The address of the raw node map. NULL may be returned if the type is too
 *     complex. If the type is too complex, you may use
 *     TecUtilDataNodeGetWritableRef() in conjunction with
 *     TecUtilDataNodeGetByRef() and TecUtilDataNodeSetByRef().
 *
 *
 * @pre <em>VALID_REF(NodeMap)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * @ingroup DataServices
 */
LINKTOADDON NodeMap_t* STDCALL TecUtilDataNodeGetWritableRawPtrByRef(NodeMap_pa NodeMap);

/**
 * Get a writable raw pointer to the finite-element node map of the specified zone
 * in the data set attached to the current frame.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *   12.0-1-5410
 *
 * @param Zone
 *   Number of the zone for which to get the writable raw node map pointer. This
 *   must be a finite-element zone
 *
 * @param NodeMapPtr
 *   Receives the address of the raw node map.  May return NULL if
 *   the type is too complex. If the type is too complex, you may use
 *   TecUtilDataNodeGetWritableRef() in conjunction with
 *   TecUtilDataNodeGetByRef() or TecUtilDataNodeSetByRef().
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>NodeMapPtr</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataNodeGetWritableRawPtr(
 *   &           Zone,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataServices
 */
LINKTOADDON void STDCALL TecUtilDataNodeGetWritableRawPtr(EntIndex_t               Zone,
                                                          TP_ARRAY_OUT NodeMap_t** NodeMapPtr);


/**
 * @deprecated
 *   There is no replacement for this function. Please do not access face neighbors
 *   using raw pointers, use TecUtilDataFaceNbrGetReadableRef() instead.
 *
 * @ingroup DataServices
 *
 * #internalattributes exclude_python, exclude_sdkdoc, exclude_tcl
 */
LINKTOADDON void STDCALL TecUtilDataFaceNbrGetRawPtr(EntIndex_t               Zone,
                                                     TP_ARRAY_OUT LgIndex_t** FNPtr);




/**
 * Get the name of a specified zone in the data set attached to the supplied
 * frame.
 * This function is \ref threadsafe.
 *
 * @param FrameID
 *   An ID of the frame that is attached to the dataset for which the query is made.
 * @param Zone
 *   Number of the zone for which to get the zone name information
 *
 * @param ZName
 *   Receives the name of the specified zone. You must free the returned string
 *   with TecUtilStringDealloc().
 *
 * @return
 *   TRUE if successful, FALSE if not. FALSE usually indicates an invalid zone
 *   or that the current frame does not have an attached data set.
 *
 *
 * @pre <em>VALID_DATASET(dataSet,true)</em>
 *   Data set must have at least one zone.
 *
 * @pre <em>ZName</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetNameForFrame(
 *   &                   FrameID,
 *   &                   Zone,
 *   &                   ZName,
 *   &                   ZNameLength)
 *    INTEGER*4       FrameID 
 *    INTEGER*4       Zone
 *    CHARACTER*(*)   ZName
 *    INTEGER*4       ZNameLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the name of the first zone for the frame with ID=1:
 *
 * @code
 *   char *name = NULL;
 *   if (TecUtilZoneGetNameForFrame(1,&name)
 *   {
 *     // do something with the name here
 *     TecUtilStringDealloc(&name);
 *   }
 * @endcode
 *
 * @since 14.1
 *
 * @ingroup Zone
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneGetNameForFrame(UniqueID_t      FrameID,
                                                         EntIndex_t      Zone,
                                                         TP_GIVES char** ZName);

/**
 * Get the name of a specified zone in the data set attached to the current
 * frame.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   Number of the zone for which to get the zone name information
 *
 * @param ZName
 *   Receives the name of the specified zone. You must free the returned string
 *   with TecUtilStringDealloc().
 *
 * @return
 *   TRUE if successful, FALSE if not. FALSE usually indicates an invalid zone
 *   or that the current frame does not have an attached data set.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>ZName</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetName(
 *   &                   Zone,
 *   &                   ZName,
 *   &                   ZNameLength)
 *    INTEGER*4       Zone
 *    CHARACTER*(*)   ZName
 *    INTEGER*4       ZNameLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the name of the first zone:
 *
 * @code
 *   char *name = NULL;
 *   if (TecUtilZoneGetName(1,&name)
 *   {
 *     // do something with the name here
 *     TecUtilStringDealloc(&name);
 *   }
 * @endcode
 *
 * @sa TecUtilZoneGetNameForFrame
 *
 * @ingroup Zone
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneGetName(EntIndex_t      Zone,
                                                 TP_GIVES char** ZName);
/**
 * Get the name of a variable in the data set attached to the specified frame.
 * There must be a data set attached to the current frame.
 * This function is \ref threadsafe.
 *
 * @param FrameID
 *   An ID of the frame that is attached to the dataset for which the query is made.
 *
 * @param VarNum
 *   Number of the variable for which to get the variable name information.
 *   Must be greater than zero, and the variable must be enabled
 *
 * @param VName
 *   Receives the name of the specified variable. Must not be NULL. You must
 *   free this string with TecUtilStringDealloc().
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 *
 * @pre <em>VALID_DATASET(dataSet,false)</em>
 *   Must have a valid data set.
 *
 * @pre <em>VarNum</em>
 *   Must specify a valid variable.
 *
 * @pre <em>VName</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarGetNameForFrame(
 *   &                   FrameID,
 *   &                   VarNum,
 *   &                   VName,
 *   &                   VNameLength)
 *    INTEGER*4       FrameID
 *    INTEGER*4       VarNum
 *    CHARACTER*(*)   VName
 *    INTEGER*4       VNameLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the name of the first variable for the frame with ID=1:
 *
 * @code
 *   char buffer[100];
 *   VarName_t Name;
 *   TecUtilVarGetNameForFrame(1, 1,&Name);
 *   sprintf(buffer,"The name of the first variable is %s",Name);
 *   TecUtilStringDealloc(&Name);
 * @endcode
 *
 * @since 14.1
 *
 * @ingroup Variables
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilVarGetNameForFrame(UniqueID_t      FrameID,
                                                        EntIndex_t      VarNum,
                                                        TP_GIVES char** VName);

/**
 * Get the name of a variable in the data set attached to the current frame.
 * There must be a data set attached to the current frame.
 * This function is \ref threadsafe.
 *
 * @param VarNum
 *   Number of the variable for which to get the variable name information.
 *   Must be greater than zero, and the variable must be enabled
 *
 * @param VName
 *   Receives the name of the specified variable. Must not be NULL. You must
 *   free this string with TecUtilStringDealloc().
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set.
 *
 * @pre <em>VarNum</em>
 *   Must specify a valid variable.
 *
 * @pre <em>VName</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarGetName(
 *   &                   VarNum,
 *   &                   VName,
 *   &                   VNameLength)
 *    INTEGER*4       VarNum
 *    CHARACTER*(*)   VName
 *    INTEGER*4       VNameLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the name of the first variable:
 *
 * @code
 *   char buffer[100];
 *   VarName_t Name;
 *   TecUtilVarGetName(1,&Name);
 *   sprintf(buffer,"The name of the first variable is %s",Name);
 *   TecUtilStringDealloc(&Name);
 * @endcode
 *
 * @sa TecUtilVarGetNameForFrame
 *
 * @ingroup Variables
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilVarGetName(EntIndex_t      VarNum,
                                                TP_GIVES char** VName);



/**
 * @deprecated
 *   Please use TecUtilLineMapGetName() instead.
 *
 * @ingroup LineMap
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilXYMapGetName(EntIndex_t      Map,
                                                  TP_GIVES char** Name);
/**
 *   Get the name of an Line-map in the specified frame.
 *
 * @param FrameID
 *   Unique ID of a frame for which the query should be made
 * @param Map
 *   Number of the Line-map
 *
 * @param Name
 *   Allocated string containing the Line-map name
 *
 * @return
 *   Returns TRUE if successful, FALSE otherwise.
 *
 *
 * @pre <em>Name</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapGetNameForFrame(
 *   &                   FrameID,
 *   &                   Map,
 *   &                   Name,
 *   &                   ReturnedNameLength)
 *    INTEGER*4       FrameID
 *    INTEGER*4       Map
 *    CHARACTER*(*)   Name
 *    INTEGER*4       ReturnedNameLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the name of Line-map 3 for the frame with ID=1:
 *
 * @code
 *   Boolean_t IsOk;
 *   char *MapName = NULL;
 *   IsOk = TecUtilLineMapGetNameForFrame(1,3,&MapName);
 * @endcode
 *
 * @since 14.1
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapGetNameForFrame(UniqueID_t FrameID,
                                                            EntIndex_t      Map,
                                                            TP_GIVES char** Name);
/**
 *   Get the name of an Line-map.
 *
 * @param Map
 *   Number of the Line-map
 *
 * @param Name
 *   Allocated string containing the Line-map name
 *
 * @return
 *   Returns TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>Name</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapGetName(
 *   &                   Map,
 *   &                   Name,
 *   &                   ReturnedNameLength)
 *    INTEGER*4       Map
 *    CHARACTER*(*)   Name
 *    INTEGER*4       ReturnedNameLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the name of Line-map 3:
 *
 * @code
 *   Boolean_t IsOk;
 *   char *MapName = NULL;
 *   IsOk = TecUtilLineMapGetName(3,&MapName);
 * @endcode
 *
 * @sa TecUtilLineMapGetNameForFrame
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapGetName(EntIndex_t      Map,
                                                    TP_GIVES char** Name);


/**
 * Gets the number of values associated with the field data reference.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *   11.0-0-353
 *
 * @param FieldData
 *   Handle to the field data. Use TecUtilDataValueGetReadableNativeRef(),
 *   TecUtilDataValueGetReadableDerivedRef(), TecUtilDataValueGetReadableNLRef(),
 *   TecUtilDataValueGetReadableCCRef(), or TecUtilDataValueGetWritableNativeRef()
 *   to get readable or writable handles to the field data.
 *
 * @return
 *   The number of values associated with the field data reference.
 *
 *
 * @pre <em>FieldData</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueGetCountByRef(FieldDataPtr)
 *   &                   FieldDataPtr)
 *    POINTER         (FieldDataPtr, FieldData)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Determine how many values are associated with the second variable of zone 5.
 * @code
 *   FieldData_pa FD = TecUtilDataValueGetReadableNativeRef(5, 2);
 *   LgIndex_t NumValues = TecUtilDataValueGetCountByRef(FD);
 * @endcode
 *
 * @ingroup DataValue
 */
LINKTOADDON LgIndex_t STDCALL TecUtilDataValueGetCountByRef(FieldData_pa FieldData);

/**
 * Convenience function used to obtain information about a specific zone in the dataset attached to
 * the specified frame.  
 * This function is primarily targeted for use with 2D and 3D frame modes. If the
 * frame mode is XY, only the zone dimensions can be queried. To get a field
 * data pointer to axis variables when the frame mode is XY use
 * TecUtilLineMapGetAssignment().
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   This function always returns a writable native field data handle when one
 *   is requested. Getting a writable native field data handle is more
 *   expensive than getting a readable native one therefore if you only intend
 *   to inspect the data you should call TecUtilDataValueGetReadableNativeRef()
 *   instead.
 *
 * @param FrameID
 *   An ID of the frame that is attached to the dataset for which the query is made.
 * @param CurZone
 *   Number of the zone to query.
 *
 * @param IMax
 *   Receives the I-dimension for ordered data. Number of data points for
 *   FE-data. Passing NULL indicates the value is not desired.
 *
 * @param JMax
 *   Receives the J-dimension for ordered data. Number of elements for FE-data.
 *   Passing NULL indicates the value is not desired.
 *
 * @param KMax
 *   Receives the K-dimension for ordered data. Number of nodes per cell for
 *   cell-based FE-data (triangle, brick, tetrahedral, quadtrilateral).  Number of
 *   faces for face-based FE-data (polygons and polyhedrons).  Passing NULL
 *  indicates the value is not desired.
 *
 * @param XVar
 *   Receives the handle to a writeable field data for X. Passing NULL
 *   indicates the value is not desired. If the frame mode is XY this parameter
 *   must be NULL.
 *
 * @param YVar
 *   Receives the handle to a writeable field data for Y. Passing NULL
 *   indicates the value is not desired. If the frame mode is XY this parameter
 *   must be NULL.
 *
 * @param ZVar
 *   Receives the handle to a writeable field data for Z. Passing NULL
 *   indicates the value is not desired. If the frame mode is XY this parameter
 *   must be NULL.
 *
 * @param NMap
 *   Receives the handle for a writeable connectivity list. Passing NULL
 *   indicates the value is not desired. If the frame mode is XY this parameter
 *   must be NULL.
 *
 * @param UVar
 *   Receives the Handle to a writeable field data for U. Passing NULL
 *   indicates the value is not desired. If the frame mode is XY this parameter
 *   must be NULL.
 *
 * @param VVar
 *   Receives the handle to a writable field data for V. Passing NULL indicates
 *   the value is not desired. If the frame mode is XY this parameter must be
 *   NULL.
 *
 * @param WVar
 *   Receives the handle to a writable field data for W. Passing NULL indicates
 *   the value is not desired. If the frame mode is XY this parameter must be
 *   NULL.
 *
 * @param BVar
 *   Receives the handle to a writable field data for the blanking variable.
 *   Passing NULL indicates the value is not desired.  If the frame mode is XY
 *   this parameter must be NULL.
 *
 * @param CVar
 *   Receives the handle to a writable field data for the contouring variable.
 *   Passing NULL indicates the value is not desired. If the frame mode is XY
 *   this parameter must be NULL.
 *
 * @param SVar
 *   Receives the handle to a writable field data for the scatter sizing
 *   variable. Passing NULL indicates the value is not desired. If the frame
 *   mode is XY this parameter must be NULL.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilZoneGetInfoForFrame(
 *   &           FrameID,
 *   &           CurZone,
 *   &           IMax,
 *   &           JMax,
 *   &           KMax,
 *   &           XVarPtr,
 *   &           YVarPtr,
 *   &           ZVarPtr,
 *   &           NMapPtr,
 *   &           UVarPtr,
 *   &           VVarPtr,
 *   &           WVarPtr,
 *   &           BVarPtr,
 *   &           CVarPtr,
 *   &           SVarPtr)
 *    INTEGER*4       FrameID
 *    INTEGER*4       CurZone
 *    INTEGER*4       IMax
 *    INTEGER*4       JMax
 *    INTEGER*4       KMax
 *    POINTER         (XVarPtr, XVar)
 *    POINTER         (YVarPtr, YVar)
 *    POINTER         (ZVarPtr, ZVar)
 *    POINTER         (NMapPtr, NMap)
 *    POINTER         (UVarPtr, UVar)
 *    POINTER         (VVarPtr, VVar)
 *    POINTER         (WVarPtr, WVar)
 *    POINTER         (BVarPtr, BVar)
 *    POINTER         (CVarPtr, CVar)
 *    POINTER         (SVarPtr, SVar)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get IMAX for the first zone for the frame with ID=1:
 *
 * @code
 *   LgIndex_t IMax;
 *   // Use NULL for values we're not interested in
 *   TecUtilZoneGetInfoForFrame(1,1,&IMax,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
 *                      NULL,NULL,NULL,NULL,NULL);
 * @endcode
 *
 * FORTRAN example to get IMAX for the first zone for the frame with ID=1:
 *
 * @code
 *      INTEGER*4 IMax
 *      INTEGER*4 ZoneNum
 *      POINTER   (NullPntr, Null)
 *             .
 *             .
 *             .
 *      NullPntr = 0
 *      FrameID  = 1
 *      ZoneNum  = 1
 *
 *      Call TecUtilZoneGetInfoForFrame(FrameID,
 *     &                        ZoneNum,
 *     &                        IMax,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null)
 * @endcode
 *
 * @ingroup Zone
 *
 * @since 14.1
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON void STDCALL TecUtilZoneGetInfoForFrame(UniqueID_t           FrameID,
                                                    EntIndex_t           CurZone,
                                                    TP_OUT LgIndex_t*    IMax,
                                                    TP_OUT LgIndex_t*    JMax,
                                                    TP_OUT LgIndex_t*    KMax,
                                                    TP_OUT FieldData_pa* XVar,
                                                    TP_OUT FieldData_pa* YVar,
                                                    TP_OUT FieldData_pa* ZVar,
                                                    TP_OUT NodeMap_pa*   NMap,
                                                    TP_OUT FieldData_pa* UVar,
                                                    TP_OUT FieldData_pa* VVar,
                                                    TP_OUT FieldData_pa* WVar,
                                                    TP_OUT FieldData_pa* BVar,
                                                    TP_OUT FieldData_pa* CVar,
                                                    TP_OUT FieldData_pa* SVar);


/**
 * Convenience function used to obtain information about a specific zone.  This
 * function is primarily targeted for use with 2D and 3D frame modes. If the
 * frame mode is XY, only the zone dimensions can be queried. To get a field
 * data pointer to axis variables when the frame mode is XY use
 * TecUtilLineMapGetAssignment().
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   This function always returns a writable native field data handle when one
 *   is requested. Getting a writable native field data handle is more
 *   expensive than getting a readable native one therefore if you only intend
 *   to inspect the data you should call TecUtilDataValueGetReadableNativeRef()
 *   instead.
 *
 * @param CurZone
 *   Number of the zone to query.
 *
 * @param IMax
 *   Receives the I-dimension for ordered data. Number of data points for
 *   FE-data. Passing NULL indicates the value is not desired.
 *
 * @param JMax
 *   Receives the J-dimension for ordered data. Number of elements for FE-data.
 *   Passing NULL indicates the value is not desired.
 *
 * @param KMax
 *   Receives the K-dimension for ordered data. Number of nodes per cell for
 *   cell-based FE-data (triangle, brick, tetrahedral, quadtrilateral).  Number of
 *   faces for face-based FE-data (polygons and polyhedrons).  Passing NULL
 *  indicates the value is not desired.
 *
 * @param XVar
 *   Receives the handle to a writeable field data for X. Passing NULL
 *   indicates the value is not desired. If the frame mode is XY this parameter
 *   must be NULL.
 *
 * @param YVar
 *   Receives the handle to a writeable field data for Y. Passing NULL
 *   indicates the value is not desired. If the frame mode is XY this parameter
 *   must be NULL.
 *
 * @param ZVar
 *   Receives the handle to a writeable field data for Z. Passing NULL
 *   indicates the value is not desired. If the frame mode is XY this parameter
 *   must be NULL.
 *
 * @param NMap
 *   Receives the handle for a writeable connectivity list. Passing NULL
 *   indicates the value is not desired. If the frame mode is XY this parameter
 *   must be NULL.
 *
 * @param UVar
 *   Receives the Handle to a writeable field data for U. Passing NULL
 *   indicates the value is not desired. If the frame mode is XY this parameter
 *   must be NULL.
 *
 * @param VVar
 *   Receives the handle to a writable field data for V. Passing NULL indicates
 *   the value is not desired. If the frame mode is XY this parameter must be
 *   NULL.
 *
 * @param WVar
 *   Receives the handle to a writable field data for W. Passing NULL indicates
 *   the value is not desired. If the frame mode is XY this parameter must be
 *   NULL.
 *
 * @param BVar
 *   Receives the handle to a writable field data for the blanking variable.
 *   Passing NULL indicates the value is not desired.  If the frame mode is XY
 *   this parameter must be NULL.
 *
 * @param CVar
 *   Receives the handle to a writable field data for the contouring variable.
 *   Passing NULL indicates the value is not desired. If the frame mode is XY
 *   this parameter must be NULL.
 *
 * @param SVar
 *   Receives the handle to a writable field data for the scatter sizing
 *   variable. Passing NULL indicates the value is not desired. If the frame
 *   mode is XY this parameter must be NULL.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilZoneGetInfo(
 *   &           CurZone,
 *   &           IMax,
 *   &           JMax,
 *   &           KMax,
 *   &           XVarPtr,
 *   &           YVarPtr,
 *   &           ZVarPtr,
 *   &           NMapPtr,
 *   &           UVarPtr,
 *   &           VVarPtr,
 *   &           WVarPtr,
 *   &           BVarPtr,
 *   &           CVarPtr,
 *   &           SVarPtr)
 *    INTEGER*4       CurZone
 *    INTEGER*4       IMax
 *    INTEGER*4       JMax
 *    INTEGER*4       KMax
 *    POINTER         (XVarPtr, XVar)
 *    POINTER         (YVarPtr, YVar)
 *    POINTER         (ZVarPtr, ZVar)
 *    POINTER         (NMapPtr, NMap)
 *    POINTER         (UVarPtr, UVar)
 *    POINTER         (VVarPtr, VVar)
 *    POINTER         (WVarPtr, WVar)
 *    POINTER         (BVarPtr, BVar)
 *    POINTER         (CVarPtr, CVar)
 *    POINTER         (SVarPtr, SVar)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get IMAX for the first zone:
 *
 * @code
 *   LgIndex_t IMax;
 *   // Use NULL for values we're not interested in
 *   TecUtilZoneGetInfo(1,&IMax,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
 *                      NULL,NULL,NULL,NULL,NULL);
 * @endcode
 *
 * FORTRAN example to get IMAX for the first zone:
 *
 * @code
 *      INTEGER*4 IMax
 *      INTEGER*4 ZoneNum
 *      POINTER   (NullPntr, Null)
 *             .
 *             .
 *             .
 *      NullPntr = 0
 *      ZoneNum  = 1
 *
 *      Call TecUtilZoneGetInfo(ZoneNum,
 *     &                        IMax,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null)
 * @endcode
 *
 * @sa TecUtilZoneGetInfoForFrame
 *
 * @ingroup Zone
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON void STDCALL TecUtilZoneGetInfo(EntIndex_t           CurZone,
                                            TP_OUT LgIndex_t*    IMax,
                                            TP_OUT LgIndex_t*    JMax,
                                            TP_OUT LgIndex_t*    KMax,
                                            TP_OUT FieldData_pa* XVar,
                                            TP_OUT FieldData_pa* YVar,
                                            TP_OUT FieldData_pa* ZVar,
                                            TP_OUT NodeMap_pa*   NMap,
                                            TP_OUT FieldData_pa* UVar,
                                            TP_OUT FieldData_pa* VVar,
                                            TP_OUT FieldData_pa* WVar,
                                            TP_OUT FieldData_pa* BVar,
                                            TP_OUT FieldData_pa* CVar,
                                            TP_OUT FieldData_pa* SVar);


/**
 * Used to obtain the I, J, and K dimensions of a specific zone.
 * This function is \ref threadsafe.
 *
 * @param CurZone
 *   Number of the zone to query.
 *
 * @param IMax
 *   Receives the I-dimension for ordered data. Number of data points for
 *   FE-data. Passing NULL indicates the value is not desired.
 *
 * @param JMax
 *   Receives the J-dimension for ordered data. Number of elements for FE-data.
 *   Passing NULL indicates the value is not desired.
 *
 * @param KMax
 *   Receives the K-dimension for ordered data. Number of nodes per cell for
 *   cell-based FE-data (triangle, brick, tetrahedral, quadtrilateral).  Number of
 *   faces for face-based FE-data (polygons and polyhedrons).  If face generation
 *   is deferred via TecUtilDataFaceMapAssignElemToNodeMap, this value will be 0 
 *   until faces are calculated.  Passing NULL indicates the value is not desired.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>IMax</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>JMax</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>KMax</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilZoneGetIJK(
 *   &           CurZone,
 *   &           IMax,
 *   &           JMax,
 *   &           KMax)
 *    INTEGER*4       CurZone
 *    INTEGER*4       IMax
 *    INTEGER*4       JMax
 *    INTEGER*4       KMax
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get IMAX for the first zone:
 *
 * @code
 *   LgIndex_t IMax;
 *   // Use NULL for values we're not interested in
 *   TecUtilZoneGetIJK(1,&IMax,NULL,NULL);
 * @endcode
 *
 * @sa
 *   TecUtilZoneGetInfo
 *
 * @ingroup Zone
 *
 */
LINKTOADDON void STDCALL TecUtilZoneGetIJK(EntIndex_t        CurZone,
                                           TP_OUT LgIndex_t* IMax,
                                           TP_OUT LgIndex_t* JMax,
                                           TP_OUT LgIndex_t* KMax);


/**
 * Used to obtain the I, J, and K dimensions of a specific zone in the specified data set.
 * This function is \ref threadsafe.
 *
 * @param DatasetID 
 *   A unique ID of a dataset.
 *
 * @param Zone
 *   Number of the zone to query.
 *
 * @param IMax
 *   Receives the I-dimension for ordered data. Number of data points for
 *   FE-data. Passing NULL indicates the value is not desired.
 *
 * @param JMax
 *   Receives the J-dimension for ordered data. Number of elements for FE-data.
 *   Passing NULL indicates the value is not desired.
 *
 * @param KMax
 *   Receives the K-dimension for ordered data. Number of nodes per cell for
 *   cell-based FE-data (triangle, brick, tetrahedral, quadtrilateral).  Number of
 *   faces for face-based FE-data (polygons and polyhedrons).  If face generation
 *   is deferred via TecUtilDataFaceMapAssignElemToNodeMap, this value will be 0 
 *   until faces are calculated.  Passing NULL indicates that the value is not desired.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>IMax</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>JMax</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>KMax</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilZoneGetIJKByUniqueID(
 *   &           DatasetID,
 *   &           Zone,
 *   &           IMax,
 *   &           JMax,
 *   &           KMax)
 *    INTEGER*4       DatasetID 
 *    INTEGER*4       Zone
 *    INTEGER*4       IMax
 *    INTEGER*4       JMax
 *    INTEGER*4       KMax
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get IMAX for the first zone:
 *
 * @code
 *   LgIndex_t IMax;
 *   // Use NULL for values we're not interested in
 *   TecUtilZoneGetIJK(1, 1, &IMax,NULL,NULL);
 * @endcode
 *
 * @sa
 *   TecUtilZoneGetInfo
 *
 * @since 14.1
 *
 * @ingroup Zone
 *
 */
LINKTOADDON void STDCALL TecUtilZoneGetIJKByUniqueID(UniqueID_t        DatasetID,
                                                     EntIndex_t        Zone,
                                                     TP_OUT LgIndex_t* IMax,
                                                     TP_OUT LgIndex_t* JMax,
                                                     TP_OUT LgIndex_t* KMax);
/**
 * Get the title, number of zones, and number of variables of the data set
 * attached to the current frame.
 * This function is \ref threadsafe.
 *
 * @param DataSetTitle
 *   Character string containing the title of the data set attached to the
 *   current frame. If you pass NULL, this will not be assigned. Deallocate the
 *   returned string with TecUtilStringDealloc() when you are done with it.
 *
 * @param NumZones
 *   The number of zones in the data set attached to the current frame. If you
 *   pass NULL, this will not be assigned
 *
 * @param NumVars
 *   The number of variables in the data set attached to the current frame. If
 *   you pass NULL, this will not be assigned.
 *
 * @return
 *   TRUE if successful, FALSE if not.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set.
 *
 * @pre <em>DataSetTitle</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>NumZones</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>NumVars</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetGetInfo(
 *   &                   DataSetTitle,
 *   &                   DataSetTitleLength,
 *   &                   NumZones,
 *   &                   NumVars)
 *    CHARACTER*(*)   DataSetTitle
 *    INTEGER*4       DataSetTitleLength
 *    INTEGER*4       NumZones
 *    INTEGER*4       NumVars
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the data set title and number of zones and variables from the current
 * data set:
 *
 * @code
 *   char *dataset_title = NULL;
 *   EntIndex_t nzones, nvars;
 *
 *   TecUtilDataSetGetInfo(&dataset_title, &nzones, &nvars);
 *   // use dataset_title
 *   TecUtilStringDealloc(&dataset_title);
 * @endcode
 *
 * @ingroup DataServices
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetGetInfo(TP_GIVES char**    DataSetTitle,
                                                    TP_OUT EntIndex_t* NumZones,
                                                    TP_OUT EntIndex_t* NumVars);

/**
 * Get the number of zones in the data set attached to the current frame.
 *
 * This function is \ref threadsafe.
 *
 * @return
 *   The number of zones in the data set attached to the current frame.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetGetNumZones()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa
 *   TecUtilDataSetGetInfo, TecUtilDataSetGetNumVars
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilDataSetGetNumZones(void);

/**
 * Get the number of zones in the data set attached to the specified frame.
 *
 * This function is \ref threadsafe.
 *
 * @param FrameID
 *   An ID of the frame that is attached to the dataset for which the query is made.
 * @return
 *   The number of zones in the data set attached to the current frame.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetGetNumZonesForFrame(FrameID)
 *    INTEGER*4 FrameID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa
 *   TecUtilDataSetGetInfo, TecUtilDataSetGetNumVarsForFrame
 *
 * @since 14.1
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilDataSetGetNumZonesForFrame(UniqueID_t FrameID);

/**
 * Get the number of variables in the data set attached to the current frame.
 *
 * This function is \ref threadsafe.
 *
 * @return
 *   The number of variables in the data set attached to the current frame.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetGetNumVars()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa
 *   TecUtilDataSetGetInfo, TecUtilDataSetGetNumZones
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilDataSetGetNumVars(void);

/**
 * Get the number of variables in the data set attached to the specified frame.
 *
 * This function is \ref threadsafe.
 *
 * @param FrameID
 *   An ID of the frame that is attached to the dataset for which the query is made.
 * @return
 *   The number of variables in the data set attached to the current frame.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetGetNumVarsForFrame(FrameID)
 *    INTEGER*4 FrameID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa
 *   TecUtilDataSetGetInfo, TecUtilDataSetGetNumZonesForFrame
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilDataSetGetNumVarsForFrame(UniqueID_t FrameID);

/**
 * Get the largest Strand number currently in use. Use this value to
 * ensure that your strand assignments are correct when adding zones
 *
 * This function is \ref threadsafe.
 *
 * @return
 *   Returns the largest strand number currently in use.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetGetMaxStrandID()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *   Arglist_pa ArgList;
 *   Strand_t   MaxStrand;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendString(ArgList, SV_NAME, "New Zone");
 *   TecUtilArgListAppendInt(ArgList, SV_ZONETYPE,
 *                           (ArbParam_t)ZoneType_Ordered);
 *   TecUtilArgListAppendInt(ArgList, SV_IMAX, 10);
 *   TecUtilArgListAppendInt(ArgList, SV_JMAX, 20);
 *
 *   MaxStrand = TecUtilDataSetGetMaxStrandID();
 *   TecUtilArgListAppendInt(ArgList, SV_STRANDID, MaxStrand+1);
 *
 *   TecUtilDataSetAddZoneX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @sa TecUtilDataSetAddZoneX
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON Strand_t STDCALL TecUtilDataSetGetMaxStrandID(void);


/**
 * Query Tecplot to see if the journal for the data set attached to the current
 * frame is valid. This is a concern if a layout file is to be generated from
 * an addon. When layouts are generated from an addon the layout must be able
 * to reproduce all data sets via named files and journal entries.  A data set
 * will require saving if any un-journaled data operations are performed on it.
 *
 * This function is \ref threadsafe.
 *
 * @return
 *   Returns TRUE if the data set journal is valid, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetJournalIsValid()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetJournalIsValid(void);



/**
 * @deprecated
 *   Please use TecUtilDataSetJournalIsValid() instead.
 *
 * @ingroup DataServices
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetRequiresSaving(void);

/**
 * Get the position and size of a frame.
 *
 * @param X
 *   Returned X-Coordinate for left hand side of the frame (in inches) relative
 *   to the left hand side of the paper.
 *
 * @param Y
 *   Returned Y-Coordinate for top of the frame (in inches) relative to the top
 *   of the paper.
 *
 * @param Width
 *   Width of the frame (in inches).
 *
 * @param Height
 *   Height of the frame (in inches).
 *
 *
 * @pre <em>X</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Y</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Width</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Height</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilFrameGetPosAndSize(
 *   &           X,
 *   &           Y,
 *   &           Width,
 *   &           Height)
 *    REAL*8          X
 *    REAL*8          Y
 *    REAL*8          Width
 *    REAL*8          Height
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the current frame's position and size:
 *
 * @code
 *   double x, y, width, height;
 *   TecUtilFrameGetPosAndSize(&x, &y, &width, &height);
 * @endcode
 *
 * @ingroup FrameManagement
 *
 */
LINKTOADDON void STDCALL TecUtilFrameGetPosAndSize(TP_OUT double* X,
                                                   TP_OUT double* Y,
                                                   TP_OUT double* Width,
                                                   TP_OUT double* Height);

/**
 * Begin a sequence of calling TecUtilProbeAtPosition() many times.
 * The following rules must be obeyed between TecUtilProbeAtPosSequenceBeginX() and
 * TecUtilProbeAtPosSequenceEnd() calls:
 * @verbatim
     - Do not make any calls that will change the style of the plot.
     - Do not make any calls that will re-order or replace zones in
       the underlying dataset (i.e. push or pop frames, load data, delete
       zones etc.).
     - Once a sequence has been started you MUST end the sequence
       even if there was a failure of any sort in the intervening code.
   @endverbatim
 *
 * @param ArgList
 *   Set of Arglist entries.  THIS PARAMETER IS RESERVED FOR FUTURE USE AND
 *   IS CURRENTLY NOT USED.  YOU MUST SET THIS TO NULL.
 *
 * Assuming a 2-D plot, probe at 100 locations with Y = 1.45 and X
 * varying between 1.0 and 100.0.  Assume that once a "hit" is discovered
 * in a zone that the next hit will be nearby in that same zone.
 * @code
 *   LgIndex_t    ICell, JCell, KCell;
 *   IJKPlanes_e  Plane;
 *   Boolean_t    StartWithLocalCell = FALSE;
 *   EntIndex_t   SourceZone;
 *   EntIndex_t   NumVars;
 *   double      *VValues;
 *   int          IX;
 *
 *   TecUtilDataSetGetInfo((char **)NULL,
 *                         (EntIndex_t *)NULL,
 *                          &NumVars);
 *
 *   VValues = (double *)malloc(NumVars*sizeof(double));
 *
 *   TecUtilProbeAtPosSequenceBeginX((ArgList_pa)NULL);
 *
 *   for (IX = 1; IX <= 100; IX++)
 *     {
 *       if (TecUtilProbeAtPosition((double)IX,
 *                                  1.45,
 *                                  0.0,
 *                                  &ICell,
 *                                  &JCell,
 *                                  &KCell,
 *                                  &Plane,
 *                                  &SourceZone,
 *                                  StartWithLocalCell,
 *                                  VValues,
 *                                  (Set_pa )NULL,
 *                                  FALSE,
 *                                  FALSE,
 *                                  FALSE))
 *
 *         {
 *             // Look at VValues[VarNum-1] to find the
 *             // value for a particular variable.
 *
 *            StartWithLocalCell = TRUE;
 *         }
 *       else
 *         StartWithLocalCell = FALSE;
 *     }
 *
 *    TecUtilProbeAtPosSequenceEnd();
 *
 * @endcode
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilProbeAtPosSequenceBeginX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON void STDCALL TecUtilProbeAtPosSequenceBeginX(ArgList_pa ArgList);







/**
 * End a sequence of calling TecUtilProbeAtPosition() many times.
 * See TecUtilProbeAtPosSequenceEnd() for an details and an example.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilProbeAtPosSequenceEnd()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON void STDCALL TecUtilProbeAtPosSequenceEnd(void);







/**
 * Use Tecplot's probe capability to return field values at a specified X, Y,
 * [Z] location. The current frame must have an attached data set when this
 * function is called and the frame must be in 2D or 3D.
 * This function is \ref threadsafe.
 *
 * @param X
 *   The X-position at which to probe. If the frame plot type is Cartesian 3D and the SearchVolume
 *   argument is FALSE then this coordinate must be in the eye (2D grid) coordinate system and the
 *   returned probe will be located on the first surface encountered.
 *   See TecUtilConvert3DPositionToGrid() for details.
 *
 * @param Y
 *   The Y-position at which to probe. If the frame plot type is Cartesian 3D and the SearchVolume
 *   argument is FALSE then this coordinate must be in the eye (2D grid) coordinate system and
 *   the returned probe will be located on the first surface encountered.
 *   See TecUtilConvert3DPositionToGrid() for details.
 *
 * @param Z
 *   The Z-position at which to probe. This argument is only required if the frame plot type is
 *   Cartesian 3D and the SearchVolume argument is TRUE, otherwise it is unused.
 *
 * @param ICell
 *   For Ordered data this returns the I-Index of the cell in which the data
 *   point was found.  If StartWithLocalCell is TRUE, then this must be
 *   pre-set to the I-index of the cell in which to start looking.
 *   <p>
 *   For Finite-Element data and GetNearestPoint is TRUE this returns the
 *   node number of the probed point. If GetNearestPoint is FALSE this
 *   value should be ignored.
 *
 * @param JCell
 *   For Ordered data this returns the J-Index of the cell in which the data
 *   point was found. If StartWithLocalCell is TRUE, then this must be
 *   pre-set to the J-index of the cell in which to start looking.
 *   <p>
 *   For Finite-Element data this returns the element number of the cell
 *   in which the probe occurred.
 *
 * @param KCell
 *   Returns the K-Index of the cell in which the data point was found. If
 *   StartWithLocalCell is TRUE, then this must be pre-set to the K-index of
 *   the cell in which to start looking.
 *   <p>
 *   For Finite-Element data this value should be ignored.
 *
 * @param Plane
 *   If the current frame is 3D, the plane of the cell in which the data point
 *   was found is returned.  (Ignore if SearchVolume is TRUE.) Plane must be
 *   pre-set if the current frame is 2D and StartWithLocalCell is TRUE and the
 *   zone is not finite element. Plane can be one of: IJKPlanes_I IJKPlanes_J
 *   IJKPlanes_K
 *
 * @param CurZone
 *   Returns the zone of the cell in which the data point was found
 *
 * @param StartWithLocalCell
 *   TRUE if the search should be started in the area of cell
 *   *ICell,*JCell,*KCell. FALSE if the search should not be started in any
 *   particular cell
 *
 * @param VValue_Array
 *   An array of doubles which is the size of the number of variables in the
 *   data set, except the minimum size is 3. The array must be allocated by the calling function. If
 *   TecUtilProbeAtPosition() returns TRUE and GetZoneOnly is FALSE, the array
 *   will be filled with the values of each variable at the probed position
 *
 * @param SourceZones
 *   The set of zones to which to limit the search. Set to NULL to search all
 *   zones
 *
 * @param SearchVolume
 *   Set to TRUE if the XYZ coordinates represent a data point inside of a 3-D
 *   volume zone. Set to FALSE to use the XY coordinates only which will return
 *   a point on the surface of a zone
 *
 * @param GetZoneOnly
 *   Set to TRUE to do the minimal work necessary to only update the CurZone
 *   variable
 *
 * @param GetNearestPoint
 *   TRUE to return values for the nearest grid point, FALSE to return
 *   interpolated values for the exact XYZ coordinates. A value of TRUE will
 *   cause this function to return FALSE if the initial probe does not fall
 *   within a cell
 *
 * @return
 *   TRUE if successful, FALSE otherwise. FALSE usually indicates that the
 *   data point could not be found however it could also mean that the probe
 *   was interrupted by the user or failed to load variables. You can check for
 *   interrupt by calling TecUtilInterruptCheck() or if you are using the
 *   status line family of functions, TecUtilStatusXxxxPercentDone, during the
 *   probe then calling TecUtilStatusCheckPercentDone() will tell you if
 *   Tecplot was interrupted, thereby providing more complete information about
 *   why TecUtilProbeAtPosition() returned FALSE.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>ICell</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>JCell</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>KCell</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Plane</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>CurZone</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(VValue)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>SourceZones</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeAtPosition(
 *   &                   X,
 *   &                   Y,
 *   &                   Z,
 *   &                   ICell,
 *   &                   JCell,
 *   &                   KCell,
 *   &                   Plane,
 *   &                   CurZone,
 *   &                   StartWithLocalCell,
 *   &                   VValue_Array,
 *   &                   SourceZonesPtr,
 *   &                   SearchVolume,
 *   &                   GetZoneOnly,
 *   &                   GetNearestPoint)
 *    REAL*8          X
 *    REAL*8          Y
 *    REAL*8          Z
 *    INTEGER*4       ICell
 *    INTEGER*4       JCell
 *    INTEGER*4       KCell
 *    INTEGER*4       Plane
 *    INTEGER*4       CurZone
 *    INTEGER*4       StartWithLocalCell
 *    REAL*8          VValue_Array
 *    POINTER         (SourceZonesPtr, SourceZones)
 *    INTEGER*4       SearchVolume
 *    INTEGER*4       GetZoneOnly
 *    INTEGER*4       GetNearestPoint
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Assuming a 2-D plot, probe at the location (3.2, 1.45):
 *
 * @code
 *   LgIndex_t    ICell, JCell, KCell;
 *   IJKPlanes_e  Plane;
 *   EntIndex_t   SourceZone;
 *   EntIndex_t   NumVars;
 *   double      *VValues;
 *
 *   TecUtilDataSetGetInfo((char **)NULL,
 *                         (EntIndex_t *)NULL,
 *                          &NumVars);
 *
 *   VValues = (double *)malloc(NumVars*sizeof(double));
 *
 *   if (TecUtilProbeAtPosition(3.2, 1.45, 0.0,
 *                              &ICell,
 *                              &JCell,
 *                              &KCell,
 *                              &Plane,
 *                              &SourceZone,
 *                              FALSE,
 *                              VValues,
 *                              (Set_pa )NULL,
 *                              FALSE,
 *                              FALSE,
 *                              FALSE))
 *      {
 *        // Look at VValues[VarNum-1] to find the
 *        // value for a particular variable.
 *
 *        LgIndex_t PointIndex;
 *        if ( TecUtilZoneIsOrdered(SourceZone) )
 *          {
 *            // Determine the primary point for the cell that was probed
 *            LgIndex_t IMax, JMax, KMax;
 *            TecUtilZoneGetIJK(SourceZone, &IMax, &JMax, &KMax);
 *            PointIndex = 1 + (ICell-1) + (JCell-1)*IMax + (KCell-1)*IMax*JMax;
 *          }
 *        else
 *          {
 *            // Determine the node number for the 1st corner of the probed element
 *            PointIndex = TecUtilDataNodeGetByZone(SourceZone, JCell, 1);
 *            // If we'd done a NearestPoint probe:
 *            // PointIndex = ICell;
 *          }
 *      }
 *    free(VValues);
 * @endcode
 *
 * @ingroup Probe
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilProbeAtPosition(double                 X,
                                                     double                 Y,
                                                     double                 Z,
                                                     TP_IN_OUT LgIndex_t*   ICell,
                                                     TP_IN_OUT LgIndex_t*   JCell,
                                                     TP_IN_OUT LgIndex_t*   KCell,
                                                     TP_IN_OUT IJKPlanes_e* Plane,
                                                     TP_IN_OUT EntIndex_t*  CurZone,
                                                     Boolean_t              StartWithLocalCell,
                                                     TP_ARRAY_OUT double*   VValue_Array,
                                                     Set_pa                 SourceZones,
                                                     Boolean_t              SearchVolume,
                                                     Boolean_t              GetZoneOnly,
                                                     Boolean_t              GetNearestPoint);



/**
 * Allocate a line segment probe result structure to receive
 * results from future calls to TecUtilLineSegProbe().
 * This function is \ref threadsafe.
 *
 * @since
 *   12.0.1.6350
 *
 * @return
 *   An allocated LineSegProbeResult_pa. This must be freed with
 *   TecUtilLineSegProbeResultDealloc()
 *
 * @ingroup Probe
 *
 */
LINKTOADDON TP_GIVES LineSegProbeResult_pa STDCALL TecUtilLineSegProbeResultAlloc(void);


/**
 * Deallocate a line segment probe result structure
 * previously allocated by TecUtilLineSegProbeResultAlloc().
 * This function is \ref threadsafe.
 *
 * @since
 *   12.0.1.6350
 *
 * @param LineSegProbeResult
 *   Pointer to the allocated result. Set to NULL upon return.
 *
 * @ingroup Probe
 *
 */
LINKTOADDON void STDCALL TecUtilLineSegProbeResultDealloc(TP_RECEIVES_GIVES LineSegProbeResult_pa* LineSegProbeResult);


/**
 * Clears a line segment probe result structure
 * of any stored results.
 * This function is \ref threadsafe.
 *
 * @since
 *   12.0.1.6350
 *
 * @param LineSegProbeResult
 *   Pointer to the allocated result. Set to NULL upon return.
 *
 * @ingroup Probe
 *
 */
LINKTOADDON void STDCALL TecUtilLineSegProbeResultClear(TP_RECEIVES_GIVES LineSegProbeResult_pa LineSegProbeResult);



/**
 * Perform "ray shooting" probe along line segments from
 * a starting position to a set of ending positions,
 * stopping if any domain boundaries are encountered,
 * and optionally calling a callback routine each time
 * a ray encounters a cell face.
 * This function is \ref threadsafe.
 *
 * @since
 *   12.0.1.6350
 *
 * @param LineSegProbeResult
 *   Handle to the result. Use TecUtilLineSegProbeResultAlloc()
 *   to allocate the result prior to calling this routine.
 *
 * @param StartingPosition
 *   An array containing the X, Y (and Z for 3D plots) location from which
 *   to begin each probe.
 *
 * @param EndingPositions
 *   An array containing the X, Y (and Z for 3D plots) endpoint for each
 *   probe. The order of its contents is: X1, Y1, Z1, X2, Y2, Z2,...
 *   For 2D plots, omit the Z values: X1, Y1, X2, Y2,...
 *
 * @param NumEndingPositions
 *   The number of X, Y (and Z for 3D) locations contained in EndingPositions.
 *
 * @param ICell
 *   The I-index, if known, of the cell containing StartingPosition. Pass zero
 *   for finite-element zones, or if the location is not known.
 *
 * @param JCell
 *   The J-index, if known, of the cell containing StartingPosition. Pass the
 *   cell number for finite-element zones, or zero if the location is not known.
 *
 * @param KCell
 *   The K-index, if known, of the cell containing StartingPosition. Pass zero
 *   for finite-element zones, or if the location is not known.
 *
 * @param CurZone
 *   The zone number, if known, containing StartingPosition. Pass zero
 *   if the location is not known.
 *
 * @param ZonesToSearch
 *   The set of zones in which to search for StartingPosition and all ending positions.
 *   Pass NULL to search all zones.
 *
 * @param VarsToReturn
 *   The set of variables whose values are to be returned for each probed segment.
 *   Pass NULL to return all variables. Axis variable values are always returned.
 *
 * @param LineSegProbeCallback
 *   This callback is called each time a cell face is about to be passed through
 *   in the course of a probe. It must return FALSE if you want to stop the probe
 *   at the current face. It must return TRUE if you want the probe to progress
 *   through the face toward the specified end point. Pass NULL if you do not
 *   want to check intermediate faces in the probe. Passing NULL has the same
 *   effect as passing a function that always returns TRUE.
 *
 * @param ClientData
 *   Private client data needed by the line segment probe callback to
 *   determine whether to allow the probe to continue. This becomes a
 *   parameter to the callback. The calling routine may store useful
 *   information in memory pointed to by this parameter.
 *
 * @return
 *   A Boolean_t indicating success or failure. If the probe was successful,
 *   results may be retrieved from LineSegProbeResult.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set.
 *
 * @pre <em>VALID_REF(endingPositions)</em>
 *   Pointer to array must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(startingPosition)</em>
 *   Pointer to array must be a valid address and non-NULL.
 *
 * @pre <em>IMPLICATION(zonesToSearch != NULL,VALID_REF(zonesToSearch))</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>IMPLICATION(varsToReturn != NULL,VALID_REF(varsToReturn))</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>IMPLICATION(lineSegProbeCallback != NULL,VALID_REF(lineSegProbeCallback))</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * @ingroup Probe
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineSegProbe (LineSegProbeResult_pa   LineSegProbeResult,
                                                   double *                StartingPosition, /* IN */
                                                   double *                EndingPositions,  /* IN */
                                                   LgIndex_t               NumEndingPositions,
                                                   LgIndex_t               ICell,
                                                   LgIndex_t               JCell,
                                                   LgIndex_t               KCell,
                                                   LgIndex_t               CurZone,
                                                   Set_pa                  ZonesToSearch,
                                                   Set_pa                  VarsToReturn,
                                                   LineSegProbeCallback_pf LineSegProbeCallback,
                                                   ArbParam_t              ClientData);


/**
 * Return the number of segments probed by a "ray shooting" probe.
 * This should equal the number of ending positions passed into the probe.
 * This function is \ref threadsafe.
 *
 * @since
 *   12.0.1.6350
 *
 * @param LineSegProbeResult
 *   Handle to the result. Use TecUtilLineSegProbeResultAlloc()
 *   to allocate the result and TecUtilLineSegProbe() to perform
 *   the probe prior to calling this routine.
 *
 * @return
 *   The number of segments probed.
 *
 * @ingroup Probe
 */
LINKTOADDON int STDCALL TecUtilLineSegProbeResultGetCount(LineSegProbeResult_pa LineSegProbeResult);


/**
 * Return the status of a "ray shooting" probe.
 * This function is \ref threadsafe.
 *
 * @since
 *   12.0.1.6350
 *
 * @param LineSegProbeResult
 *   Handle to the result. Use TecUtilLineSegProbeResultAlloc()
 *   to allocate the result and TecUtilLineSegProbe() to perform
 *   the probe prior to calling this routine.
 *
 * @param WhichSegment
 *   Which segment's result is desired. Pass zero to retrieve
 *   information for the starting position.
 *
 * @return
 *   The status for segment WhichSegment.
 *   It can be one of the following possible values: \ref ProbeStatus_Normal,
 *   \ref ProbeStatus_Terminated, or \ref ProbeStatus_Exited.
 *
 * @ingroup Probe
 */
LINKTOADDON ProbeStatus_e STDCALL TecUtilLineSegProbeGetStatus(LineSegProbeResult_pa LineSegProbeResult,
                                                               int                   WhichSegment);


/**
 * Return a variable value at the terminus of a "ray shooting" probe.
 * This function is \ref threadsafe.
 *
 * @since
 *   12.0.1.6350
 *
 * @param LineSegProbeResult
 *   Handle to the result. Use TecUtilLineSegProbeResultAlloc()
 *   to allocate the result and TecUtilLineSegProbe() to perform
 *   the probe prior to calling this routine.
 *
 * @param WhichSegment
 *   Which segment's result is desired. Pass zero to retrieve
 *   information for the starting position.
 *
 * @param Var
 *   Which variable is desired. This must have been included in the
 *   set of variables passes into TecUtilLineSegProbe().
 *
 * @return
 *   The variable value interpolated at the terminus of segment WhichSegment.
 *
 * @ingroup Probe
 */
LINKTOADDON double STDCALL TecUtilLineSegProbeGetVarValue(LineSegProbeResult_pa LineSegProbeResult,
                                                          int                   WhichSegment,
                                                          EntIndex_t            Var);


/**
 * Return the I index of the cell containing the terminus of a "ray shooting" probe.
 * This function is \ref threadsafe.
 *
 * @since
 *   12.0.1.6350
 *
 * @param LineSegProbeResult
 *   Handle to the result. Use TecUtilLineSegProbeResultAlloc()
 *   to allocate the result and TecUtilLineSegProbe() to perform
 *   the probe prior to calling this routine.
 *
 * @param WhichSegment
 *   Which segment's result is desired. Pass zero to retrieve
 *   information for the starting position.
 *
 * @return
 *   The I index of the cell containing the terminus of segment WhichSegment.
 *
 * @ingroup Probe
 */
LINKTOADDON LgIndex_t STDCALL TecUtilLineSegProbeGetICell(LineSegProbeResult_pa LineSegProbeResult,
                                                          int                   WhichSegment);


/**
 * Return the J index of the cell containing the terminus of a "ray shooting" probe.
 * This function is \ref threadsafe.
 *
 * @since
 *   12.0.1.6350
 *
 * @param LineSegProbeResult
 *   Handle to the result. Use TecUtilLineSegProbeResultAlloc()
 *   to allocate the result and TecUtilLineSegProbe() to perform
 *   the probe prior to calling this routine.
 *
 * @param WhichSegment
 *   Which segment's result is desired. Pass zero to retrieve
 *   information for the starting position.
 *
 * @return
 *   The J index of the cell containing the terminus of segment WhichSegment.
 *
 * @ingroup Probe
 */
LINKTOADDON LgIndex_t STDCALL TecUtilLineSegProbeGetJCell(LineSegProbeResult_pa LineSegProbeResult,
                                                          int                   WhichSegment);


/**
 * Return the K index of the cell containing the terminus of a "ray shooting" probe.
 * This function is \ref threadsafe.
 *
 * @since
 *   12.0.1.6350
 *
 * @param LineSegProbeResult
 *   Handle to the result. Use TecUtilLineSegProbeResultAlloc()
 *   to allocate the result and TecUtilLineSegProbe() to perform
 *   the probe prior to calling this routine.
 *
 * @param WhichSegment
 *   Which segment's result is desired. Pass zero to retrieve
 *   information for the starting position.
 *
 * @return
 *   The K index of the cell containing the terminus of segment WhichSegment.
 *
 * @ingroup Probe
 */
LINKTOADDON LgIndex_t STDCALL TecUtilLineSegProbeGetKCell(LineSegProbeResult_pa LineSegProbeResult,
                                                          int                   WhichSegment);


/**
 * Return the zone containing the terminus of a "ray shooting" probe.
 * This function is \ref threadsafe.
 *
 * @since
 *   12.0.1.6350
 *
 * @param LineSegProbeResult
 *   Handle to the result. Use TecUtilLineSegProbeResultAlloc()
 *   to allocate the result and TecUtilLineSegProbe() to perform
 *   the probe prior to calling this routine.
 *
 * @param WhichSegment
 *   Which segment's result is desired. Pass zero to retrieve
 *   information for the starting position.
 *
 * @return
 *   The zone containing the terminus of segment WhichSegment.
 *
 * @ingroup Probe
 */
LINKTOADDON EntIndex_t STDCALL TecUtilLineSegProbeGetZone(LineSegProbeResult_pa LineSegProbeResult,
                                                          int                   WhichSegment);


/**
 * Return the cell face at which the a "ray shooting" probe terminated, or
 * 0 if the ray reached its destination without terminating.
 * This function is \ref threadsafe.
 *
 * @since
 *   12.0.1.6350
 *
 * @param LineSegProbeResult
 *   Handle to the result. Use TecUtilLineSegProbeResultAlloc()
 *   to allocate the result and TecUtilLineSegProbe() to perform
 *   the probe prior to calling this routine.
 *
 * @param WhichSegment
 *   Which segment's result is desired. Pass zero to retrieve
 *   information for the starting position.
 *
 * @return
 *   The cell face at which segment WhichSegment terminated, or zero if
 *   it did not terminate prior to reaching its destination.
 *
 * @ingroup Probe
 */
LINKTOADDON LgIndex_t STDCALL TecUtilLineSegProbeGetFace(LineSegProbeResult_pa LineSegProbeResult,
                                                         int                   WhichSegment);



/**
 * Get the set of enabled zones. Zones are enabled/disabled when they are read
 * in.
 *
 * @param EnabledZones
 *   Receives the set of enabled zones. You must free this pointer by calling
 *   TecUtilSetDealloc().
 *
 * @return
 *   TRUE if successful, FALSE otherwise
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetEnabled(EnabledZonesPtr)
 *    POINTER (EnabledZonesPtr, EnabledZones)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the set of enabled zones:
 *
 * @code
 *   Set_pa set = NULL;
 *   if (TecUtilZoneGetEnabled(&set))
 *   {
 *    // do something with the set here
 *    TecUtilSetDealloc(&set);
 *   }
 * @endcode
 *
 * @sa TecUtilZoneGetEnabledForFrame
 *
 * @ingroup Zone
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneGetEnabled(TP_GIVES Set_pa* EnabledZones);

/**
 * Get the set of enabled zones for the dataset attached to the specified frame. Zones are 
 * enabled/disabled when they are read in.
 *
 * @param FrameID
 *   An ID of the frame that is attached to the dataset for which the query is made.
 * @param EnabledZones
 *   Receives the set of enabled zones. You must free this pointer by calling
 *   TecUtilSetDealloc().
 *
 * @return
 *   TRUE if successful, FALSE otherwise
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetEnabledForFrame(FrameID, EnabledZonesPtr)
 *    INTEGER*4 FrameID
 *    POINTER (EnabledZonesPtr, EnabledZones)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the set of enabled zones:
 *
 * @code
 *   Set_pa set = NULL;
 *   if (TecUtilZoneGetEnabledForFrame(1, &set))
 *   {
 *    // do something with the set here
 *    TecUtilSetDealloc(&set);
 *   }
 * @endcode
 *
 * @since 14.1
 *
 * @ingroup Zone
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneGetEnabledForFrame(UniqueID_t       FrameID,
                                                            TP_GIVES Set_pa* EnabledZones);

/**
 * @deprecated
 *   Please use TecUtilDataSetGetRelevantZones() instead.
 *
 * @ingroup Zone
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneGetRelevant(double           SolutionTime,
                                                     Boolean_t        IgnoreStaticZones,
                                                     TP_GIVES Set_pa* RelevantZones);

/**
 * Get the set of relevant zones between the supplied maximum and minimum solution time.
 * A transient zone is relevant if its solution time is less than the supplied solution 
 * time and there are no other zones in its strand that have closer solution times.
 * No zones of a strand are relevant if the solution time is outside the range of solution 
 * times for the entire strand. Static zones (non-transient) are always considered relevant. 
 * For more information on transient zones see the Tecplot User's Manual.
 * This function is \ref threadsafe.
 *
 * @since 13.0.0.14385
 *
 * @param IgnoreStaticZones
 *    If set to TRUE the resulting set will not include static zones. If
 *    FALSE, static zones will be included in the result. Static zones are
 *    always "relevant" regardless of the solution time.
 *
 * @param SolutionTimeMin
 *    The min solution time for which to get the relevant zones.
 *
 * @param SolutionTimeMax
 *    The max solution time for which to get the relevant zones.
 *
 * @return
 *    Allocated Set_pa with the resulting set of zones. If something went wrong
 *    RelevantZones will be NULL.
 *
 * <FortranSyntax>
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the set of relevant zones between solution times 0.4324 and 0.5324:
 *
 * @code
 *   Set_pa set = NULL;
 *   set = TecUtilDataSetGetRelevantZones(
 *                       0.4324, //Min
 *                       0.5324, //Max
 *                       IgnoreStaticZones);
 *   if ( set )
 *   {
 *       // do something with the set here
 *       TecUtilSetDealloc(&set);
 *   }
 * @endcode
 *
 * @ingroup Zone
 *
 */
LINKTOADDON TP_GIVES Set_pa STDCALL TecUtilDataSetGetRelevantZones(
                                        double    SolutionTimeMin,
                                        double    SolutionTimeMax,
                                        Boolean_t IgnoreStaticZones);

/**
 * Get the set of relevant zones for the supplied strand Id. A transient zone is
 * relevant if its solution time is less than the supplied solution time and there are
 * no other zones in its strand that have closer solution times. No zones of a strand
 * are relevant if the solution time is outside the range of solution times for the
 * entire strand. Static zones (non-transient) are always considered relevant. For
 * more information on transient zones see the Tecplot User's Manual.
 * This function is \ref threadsafe.
 *
 * @since 13.0.0.14385
 *
 * @param StrandID
 *    The strand ID for which to get the relevant zones.
 *
 * @param SolutionTimeMin
 *    The min solution time for which to get the relevant zones.
 *
 * @param SolutionTimeMax
 *    The max solution time for which to get the relevant zones.
 *
 * @return
 *    Allocated Set_pa with the resulting set of zones. If something went wrong
 *    RelevantZones will be NULL.
 *
 * <FortranSyntax>
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the set of relevant zones for strand ID 5, between solution time 0.5 and 0.8:
 *
 * @code
 *   Set_pa set = NULL;
 *   set = TecUtilDataSetGetStrandRelevantZones(5,0.5,0.8);
 *
 *   if (zones != NULL)
 *   {
 *     // do something with the set here
 *     TecUtilSetDealloc(&set);
 *   }
 * @endcode
 *
 * @ingroup Zone
 *
 */
LINKTOADDON TP_GIVES Set_pa STDCALL TecUtilDataSetGetStrandRelevantZones(
    Strand_t StrandID,
    double   SolutionTimeMin,
    double   SolutionTimeMax);

/**
 * Get the set of enabled variables. Variables are enabled/disabled when they
 * are read in. There must be a data set attached to the current frame.
 *
 * @param EnabledVars
 *   Set of enabled variables. Must not be NULL
 *
 * @return
 *   TRUE if successful, FALSE otherwise
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarGetEnabled(EnabledVarsPtr)
 *    POINTER (EnabledVarsPtr, EnabledVars)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the set of enabled variables. It is assumed that a data set has been
 *   created:
 *
 * @code
 *   Set_pa set = NULL;
 *   TecUtilVarGetEnabled(&set);
 *   // Do something with set
 *   TecUtilSetDealloc(&set);
 * @endcode
 *
 * @sa TecUtilVarGetEnabledForFrame
 *
 * @ingroup Variables
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilVarGetEnabled(TP_GIVES Set_pa* EnabledVars);

/**
 * Get the set of enabled variables for the dataset attached to the specified frame. Variables are 
 * enabled/disabled when they are read in. There must be a data set attached to the current frame.
 *
 * @param FrameID
 *   An ID of the frame that is attached to the dataset for which the query is made.
 * @param EnabledVars
 *   Set of enabled variables. Must not be NULL
 *
 * @return
 *   TRUE if successful, FALSE otherwise
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarGetEnabledForFrame(FrameID, EnabledVarsPtr)
 *    INTEGER*4 FrameID
 *    POINTER (EnabledVarsPtr, EnabledVars)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the set of enabled variables. It is assumed that a data set has been
 *   created:
 *
 * @code
 *   Set_pa set = NULL;
 *   TecUtilVarGetEnabledForFrame(1, &set);
 *   // Do something with set
 *   TecUtilSetDealloc(&set);
 * @endcode
 *
 * @since 14.1
 *
 * @ingroup Variables
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilVarGetEnabledForFrame(UniqueID_t       FrameID,
                                                           TP_GIVES Set_pa* EnabledVars);
/**
 * Obtain the set of active field zones.
 *
 * @param ActiveZones
 *   Receives the set of active field zones. You must call TecUtilSetDealloc()
 *   when you are through using the set.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetActive(ActiveZonesPtr)
 *    POINTER (ActiveZonesPtr, ActiveZones)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Do something if zone 3 is active:
 *
 * @code
 *   Set_pa zone_set = NULL;
 *   TecUtilZoneGetActive(&zone_set);
 *   if ( TecUtilSetIsMember(zone_set, 3) )
 *     {
 *       // do something
 *     }
 *   TecUtilSetDealloc(&zone_set);
 * @endcode
 *
 * @sa TecUtilZoneGetActiveForFrame
 *
 * @ingroup Zone
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneGetActive(TP_GIVES Set_pa* ActiveZones);

/**
 * Obtain the set of active field zones of the dataset attached to the specified frame.
 *
 * @param FrameID
 *   An ID of the frame that is attached to the dataset for which the query is made.
 * @param ActiveZones
 *   Receives the set of active field zones. You must call TecUtilSetDealloc()
 *   when you are through using the set.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetActiveForFrame(
 *    & FrameID
 *    & ActiveZonesPtr)
 *    INTEGER*4 FrameID 
 *    POINTER (ActiveZonesPtr, ActiveZones)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Do something if zone 3 is active:
 *
 * @code
 *   Set_pa zone_set = NULL;
 *   TecUtilZoneGetActiveForFrame(1, &zone_set);
 *   if ( TecUtilSetIsMember(zone_set, 3) )
 *     {
 *       // do something
 *     }
 *   TecUtilSetDealloc(&zone_set);
 * @endcode
 *
 * @since 14.1
 *
 * @ingroup Zone
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneGetActiveForFrame(UniqueID_t FrameID, 
                                                           Set_pa *ActiveZones);

/**
 * @deprecated
 *   Please use TecUtilLineMapGetActive() instead.
 *
 * @ingroup LineMap
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilXYMapGetActive(TP_GIVES Set_pa* ActiveXYMaps);



/**
 * Obtain the set of active Line-maps.
 * This function is \ref threadsafe.
 *
 * @param ActiveLineMaps
 *   Receives the set of active Line-maps. You must call TecUtilSetDealloc()
 *   when you are through using the set. It must not be NULL.
 *
 * @return
 *   TRUE if successful.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>ActiveLineMaps</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapGetActive(ActiveLineMapsPtr)
 *    POINTER (ActiveLineMapsPtr, ActiveLineMaps)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the set of active Line-maps:
 *
 * @code
 *   Set_pa s = NULL;
 *   if (TecUtilLineMapGetActive(&s))
 *   {
 *    // maps are now in s
 *    TecUtilSetDealloc(&s);
 *   }
 * @endcode
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapGetActive(TP_GIVES Set_pa* ActiveLineMaps);




/**
 * @deprecated
 *   Please use TecUtilLineMapGetAssignment() instead.
 *
 * @ingroup LineMap
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilXYMapGetAssignment(EntIndex_t                   XYMap,
                                                   TP_OUT EntIndex_t*           Zone,
                                                   TP_OUT EntIndex_t*           XAxisVar,
                                                   TP_OUT EntIndex_t*           YAxisVar,
                                                   TP_OUT SmInteger_t*          XAxis,
                                                   TP_OUT SmInteger_t*          YAxis,
                                                   TP_OUT FunctionDependency_e* FunctionDependency);
/**
 * Get the assignment information for a given Line-map.
 * This function is \ref threadsafe.
 *
 * @param LineMap
 *   Map number to query
 *
 * @param Zone
 *   Zone number assigned to the Line-map. Set to NULL if you are not
 *   interested in this returned value.
 *
 * @param XOrThetaVar
 *   Receives the X-axis or Theta variable number assigned to the Line-map.
 *   Will only receive a Theta value if plot is in polar mode. Set to NULL if
 *   you are not interested in this returned value
 *
 * @param YOrRVar
 *   Receives the Y-axis or Radian variable number assigned to the Line-map.
 *   Will only receive Radian value if plot is in polar mode. Set to NULL if
 *   you are not interested in this returned value.
 *
 * @param XAxis
 *   Receives the X-axis number assigned to the Line-map. Set to NULL if you
 *   are not interested in this returned value.
 *
 * @param YAxis
 *   Receives the Y-axis number assigned to the Line-map. Set to NULL if you
 *   are not interested in this returned value
 *
 * @param FunctionDependency
 *   Receives the Function dependency assigned to the Line-map. Set to NULL if
 *   you are not interested in this returned value
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>Zone</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>XOrThetaVar</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>YOrRVar</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>XAxis</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>YAxis</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>FunctionDependency</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilLineMapGetAssignment(
 *   &           LineMap,
 *   &           Zone,
 *   &           XOrThetaVar,
 *   &           YOrRVar,
 *   &           XAxis,
 *   &           YAxis,
 *   &           FunctionDependency)
 *    INTEGER*4       LineMap
 *    INTEGER*4       Zone
 *    INTEGER*4       XOrThetaVar
 *    INTEGER*4       YOrRVar
 *    INTEGER*4       XAxis
 *    INTEGER*4       YAxis
 *    INTEGER*4       FunctionDependency
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Obtain the zone number and which variables are used for X and Y for
 *   Line-map number 3:
 *
 * @code
 *     EntIndex_t Zone;
 *     EntIndex_t XVar;
 *     EntIndex_t YVar;
 *
 *     TecUtilLineMapGetAssignment(3,
 *                               &Zone,
 *                               &XVar,
 *                               &YVar,
 *                               (SmInteger_t *)NULL,
 *                               (SmInteger_t *)NULL,
 *                               (FunctionDependency_e *)NULL);
 * @endcode
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON void STDCALL TecUtilLineMapGetAssignment(EntIndex_t                   LineMap,
                                                     TP_OUT EntIndex_t*           Zone,
                                                     TP_OUT EntIndex_t*           XOrThetaVar,
                                                     TP_OUT EntIndex_t*           YOrRVar,
                                                     TP_OUT SmInteger_t*          XAxis,
                                                     TP_OUT SmInteger_t*          YAxis,
                                                     TP_OUT FunctionDependency_e* FunctionDependency);
/**
 * Determine if a zone in the data set attached to the current frame contains
 * finite-element data.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   Number of the zone for which to get the zone type information
 *
 * @return
 *   TRUE if the zone is a finite-element zone, FALSE if it is not.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>Zone</em>
 *   Must specify a valid zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneIsFiniteElement(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Check if the first zone is finite element:
 *
 * @code
 *   if (TecUtilZoneIsFiniteElement(1))
 *   {
 *     // sure is!
 *   }
 * @endcode
 *
 * @ingroup Zone
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneIsFiniteElement(EntIndex_t Zone);


/**
 * Determine if the specified zone in the data set attached to the current
 * frame contains ordered data.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   Number of the zone for which to get the zone type information
 *
 * @return
 *   TRUE if the zone is an I-ordered, IJ-ordered, or IJK-ordered zone; FALSE
 *   if it is not.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneIsOrdered(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Check if the first zone is ordered:
 *
 * @code
 *   if (TecUtilZoneIsOrdered(1))
 *   {
 *     // sure is!
 *   }
 * @endcode
 *
 * @sa TecUtilZoneIsOrderedForFrame
 *
 * @ingroup Zone
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneIsOrdered(EntIndex_t Zone);

/**
 * Determine if the specified zone in the data set attached to the specified
 * frame contains ordered data.
 * This function is \ref threadsafe.
 *
 * @param FrameID
 *   An ID of the frame that is attached to the dataset for which the query is made.
 * @param Zone
 *   Number of the zone for which to get the zone type information
 *
 * @return
 *   TRUE if the zone is an I-ordered, IJ-ordered, or IJK-ordered zone; FALSE
 *   if it is not.
 *
 *
 * @pre <em>VALID_DATASET(dataSet,true)</em>
 *   Data set must have at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneIsOrderedForFrame(FrameID, Zone)
 *    INTEGER*4 FrameID
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Check if the first zone is ordered for the frame with ID=1:
 *
 * @code
 *   if (TecUtilZoneIsOrderedForFrame(1, 1))
 *   {
 *     // sure is!
 *   }
 * @endcode
 *
 * @since 14.1
 *
 * @ingroup Zone
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneIsOrderedForFrame(UniqueID_t FrameID, 
                                                           EntIndex_t Zone);

/**
 * Determine if the specified zone in the data set attached to the current
 * frame is subzone-loadable.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.1
 *
 * @param Zone
 *   Number of the zone for which to get the information
 *
 * @return
 *   TRUE if the zone is subzone-loadable; FALSE if it is not.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneIsSZL(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Check if the first zone is subzone-loadable:
 *
 * @code
 *   if (TecUtilZoneIsSZL(1))
 *   {
 *     // sure is!
 *   }
 * @endcode
 *
 * @ingroup Zone
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneIsSZL(EntIndex_t Zone);



/**
 * Get the type of a specified zone in the data set attached to the current
 * frame.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   Number of the zone for which to get the zone type information
 *
 * @return
 *   The zone type.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>Zone</em>
 *   Must specify a valid zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetType(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the time of the first zone:
 *
 * @code
 *   ZoneType_e type = TecUtilZoneGetType(1);
 * @endcode
 *
 * @ingroup Zone
 *
 */
LINKTOADDON ZoneType_e STDCALL TecUtilZoneGetType(EntIndex_t Zone);



/**
 * Get a field data value. This function does not require you to obtain the
 * handle to the field data as does TecUtilDataValueGetByRef(), however, this
 * function is not very efficient. Use TecUtilDataValueGetByRef() if you are
 * getting multiple values from the same zone.
 *
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   The zone number
 *
 * @param Var
 *   The variable number
 *
 * @param PointIndex
 *   Position in the array of field data values. Position starts at one. If
 *   FieldData came from an IJ- or IJK-ordered zone then the position is
 *   calculated by treating the two- or three-dimensional array as a
 *   one-dimensional array
 *
 * @return
 *   The variable value at a specific point in a zone.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>Zone</em>
 *   Must specify a valid zone.
 *
 * @pre <em>Var</em>
 *   Must specify a valid variable.
 * @pre @e PointIndex must be at least 1 and no more than the number of data values in the field.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilDataValueGetByZoneVar(
 *   &                   Zone,
 *   &                   Var,
 *   &                   PointIndex)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 *    INTEGER*4       PointIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the twenty-first value of the second variable of zone 5:
 *
 * @code
 *   double dd = TecUtilDataValueGetByZoneVar(5, 2, 21);
 *   // Use val.
 * @endcode
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON double STDCALL TecUtilDataValueGetByZoneVar(EntIndex_t Zone,
                                                        EntIndex_t Var,
                                                        LgIndex_t  PointIndex);

/**
 * Get a read-only handle to the native data for the specified zone and
 * variable in the data set attached to the current frame.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.2-0-397
 *
 * @param Zone
 *   Number of the zone for which to get the field data
 *
 * @param Var
 *   Number of the variable for which to get the field data
 *
 * @return
 *   A read-only field data handle to the native data for the specified zone
 *   and variable in the data set attached to the current frame or NULL if
 *   Tecplot was not able to load the data.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueGetReadableNativeRef(
 *   &           Zone,
 *   &           Var,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Function that loads the values of a field variable into
 * a supplied double precision array.  Assume the array is
 * already dimensioned correctly and the dataset, zone, and
 * variable exist.
 * @code
 * static void ReadVals(double     Var[],
 *                      EntIndex_t ZoneNum,
 *                      EntIndex_t VarNum)
 * {
 *   FieldData_pa FieldData = TecUtilDataValueGetReadableNativeRef(ZoneNum, VarNum);
 *   if (FieldData)
 *     {
 *       int i;
 *       LgIndex_t NumValues = TecUtilDataValueGetCountByRef(FieldData);
 *
 *       // Remember that the GetByRef function is 1-based....
 *       for (i = 0; i < NumPointsInZone; i++)
 *         Var[i] = TecUtilDataValueGetByRef(FieldData, i+1);
 *     }
 * }
 * @endcode
 *
 * @sa TecUtilDataValueGetReadableDerivedRef(), TecUtilDataValueGetReadableNLRef(),
 *     TecUtilDataValueGetReadableCCRef(), TecUtilDataValueGetWritableNativeRef()
 *
 * @ingroup DataValue
 */
LINKTOADDON FieldData_pa STDCALL TecUtilDataValueGetReadableNativeRef(EntIndex_t Zone,
                                                                      EntIndex_t Var);

/**
 * Get a read-only handle to the native data for the specified dataset, zone and
 * variable in the specified data set.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.1-0
 *
 * @param DatasetID 
 *   A unique ID of a dataset.
 * 
 * @param Zone
 *   Number of the zone for which to get the field data
 *
 * @param Var
 *   Number of the variable for which to get the field data
 *
 * @return
 *   A read-only field data handle to the native data for the specified zone
 *   and variable in the specified data set or NULL if Tecplot was not able to load the data.
 *
 * Function that loads the values of a field variable into
 * a supplied double precision array.  Assume the array is
 * already dimensioned correctly and the dataset, zone, and
 * variable exist.
 * @code
 * static void ReadVals(double     Var[],
 *                      UniqueID_t DatasetID,
 *                      EntIndex_t ZoneNum,
 *                      EntIndex_t VarNum)
 * {
 *   FieldData_pa FieldData = TecUtilDataValueGetReadableNativeRefByUniqueID(
 *      DatasetID, ZoneNum, VarNum);
 *   if (FieldData)
 *     {
 *       int i;
 *       LgIndex_t NumValues = TecUtilDataValueGetCountByRef(FieldData);
 *
 *       // Remember that the GetByRef function is 1-based....
 *       for (i = 0; i < NumPointsInZone; i++)
 *         Var[i] = TecUtilDataValueGetByRef(FieldData, i+1);
 *     }
 * }
 * @endcode
 *
 * @sa TecUtilDataValueGetReadableDerivedRef(), TecUtilDataValueGetReadableNLRef(),
 *     TecUtilDataValueGetReadableCCRef(), TecUtilDataValueGetWritableNativeRefByUniqueID()
 *
 * @ingroup DataValue
 */
LINKTOADDON FieldData_pa STDCALL TecUtilDataValueGetReadableNativeRefByUniqueID(UniqueID_t DatasetID,
                                                                                EntIndex_t Zone,
                                                                                EntIndex_t Var);

/**
 * Get a read-only handle to the derived data for the specified zone and
 * variable in the data set attached to the current frame.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.2-0-397
 *
 * @param Zone
 *   Number of the zone for which to get the field data
 *
 * @param Var
 *   Number of the variable for which to get the field data
 *
 * @return
 *   A read-only field data handle to the derived data for the specified zone
 *   and variable in the data set attached to the current frame or NULL if
 *   Tecplot was interrupted or was not able to load or derive the data.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueGetReadableDerivedRef(
 *   &           Zone,
 *   &           Var,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Function that loads the values of a field variable into
 * a supplied double precision array.  Assume the array is
 * already dimensioned correctly and the dataset, zone, and
 * variable exist.
 * @code
 * static void ReadVals(double     Var[],
 *                      EntIndex_t ZoneNum,
 *                      EntIndex_t VarNum)
 * {
 *   FieldData_pa FieldData = TecUtilDataValueGetReadableDerivedRef(ZoneNum, VarNum);
 *   if (FieldData)
 *     {
 *       int i;
 *       LgIndex_t NumValues = TecUtilDataValueGetCountByRef(FieldData);
 *
 *       // Remember that the GetByRef function is 1-based....
 *       for (i = 0; i < NumPointsInZone; i++)
 *         Var[i] = TecUtilDataValueGetByRef(FieldData, i+1);
 *     }
 * }
 * @endcode
 *
 * @sa TecUtilDataValueGetReadableNativeRef(), TecUtilDataValueGetReadableNLRef(),
 *     TecUtilDataValueGetReadableCCRef(), TecUtilDataValueGetWritableNativeRef()
 *
 * @ingroup DataValue
 */
LINKTOADDON FieldData_pa STDCALL TecUtilDataValueGetReadableDerivedRef(EntIndex_t Zone,
                                                                       EntIndex_t Var);

/**
 * Get a read-only handle to the node located data for the specified zone and
 * variable in the data set attached to the current frame.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.2-0-397
 *
 * @param Zone
 *   Number of the zone for which to get the field data
 *
 * @param Var
 *   Number of the variable for which to get the field data
 *
 * @return
 *   A read-only field data handle to the node located data for the specified
 *   zone and variable in the data set attached to the current frame or NULL if
 *   Tecplot was interrupted or was not able to load or derive the data.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueGetReadableNLRef(
 *   &           Zone,
 *   &           Var,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Function that loads the values of a field variable into
 * a supplied double precision array.  Assume the array is
 * already dimensioned correctly and the dataset, zone, and
 * variable exist.
 * @code
 * static void ReadVals(double     Var[],
 *                      EntIndex_t ZoneNum,
 *                      EntIndex_t VarNum)
 * {
 *   FieldData_pa FieldData = TecUtilDataValueGetReadableNLRef(ZoneNum, VarNum);
 *   if (FieldData)
 *     {
 *       int i;
 *       LgIndex_t NumValues = TecUtilDataValueGetCountByRef(FieldData);
 *
 *       // Remember that the GetByRef function is 1-based....
 *       for (i = 0; i < NumPointsInZone; i++)
 *         Var[i] = TecUtilDataValueGetByRef(FieldData, i+1);
 *     }
 * }
 * @endcode
 *
 * @sa TecUtilDataValueGetReadableNativeRef(), TecUtilDataValueGetReadableDerivedRef(),
 *     TecUtilDataValueGetReadableCCRef(), TecUtilDataValueGetWritableNativeRef()
 *
 * @ingroup DataValue
 */
LINKTOADDON FieldData_pa STDCALL TecUtilDataValueGetReadableNLRef(EntIndex_t Zone,
                                                                  EntIndex_t Var);

/**
 * Get a read-only handle to the cell centered data for the specified zone and
 * variable in the data set attached to the current frame.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.2-0-397
 *
 * @param Zone
 *   Number of the zone for which to get the field data
 *
 * @param Var
 *   Number of the variable for which to get the field data
 *
 * @return
 *   A read-only field data handle to the cell centered data for the specified
 *   zone and variable in the data set attached to the current frame or NULL if
 *   Tecplot was interrupted or was not able to load or derive the data.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueGetReadableCCRef(
 *   &           Zone,
 *   &           Var,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Function that loads the values of a field variable into
 * a supplied double precision array.  Assume the array is
 * already dimensioned correctly and the dataset, zone, and
 * variable exist.
 * @code
 * static void ReadVals(double     Var[],
 *                      EntIndex_t ZoneNum,
 *                      EntIndex_t VarNum)
 * {
 *   FieldData_pa FieldData = TecUtilDataValueGetReadableCCRef(ZoneNum, VarNum);
 *   if (FieldData)
 *     {
 *       int i;
 *       LgIndex_t NumValues = TecUtilDataValueGetCountByRef(FieldData);
 *
 *       // Remember that the GetByRef function is 1-based....
 *       for (i = 0; i < NumPointsInZone; i++)
 *         Var[i] = TecUtilDataValueGetByRef(FieldData, i+1);
 *     }
 * }
 * @endcode
 *
 * @sa TecUtilDataValueGetReadableNativeRef(), TecUtilDataValueGetReadableDerivedRef(),
 *     TecUtilDataValueGetReadableNLRef(), TecUtilDataValueGetWritableNativeRef()
 *
 * @ingroup DataValue
 */
LINKTOADDON FieldData_pa STDCALL TecUtilDataValueGetReadableCCRef(EntIndex_t Zone,
                                                                  EntIndex_t Var);

/**
 * Get a native read/write handle to the data for the specified dataset, zone and
 * variable. It is important to realize that when altering the values this variable may in fact be 
 * shared.
 * If you want the new values to only apply to the specific zone and variable
 * associated with this dataset then you must first make the DatasetID the current dataset and call
 * TecUtilDataValueBranchShared() prior to calling TecUtilDataValueGetWritableNativeRefByUniqueID().
 *
 * This function is \ref threadsafe.
 *
 * @since
 *   14.1-0
 *
 * @param DatasetID 
 *   A unique ID of a dataset.
 * 
 * @param Zone
 *   Number of the zone for which to get the field data
 *
 * @param Var
 *   Number of the variable for which to get the field data
 *
 * @return
 *   A read/write field data handle to the native data for the specified
 *   zone and variable in the data set associated with the DatasetID or NULL if
 *   Tecplot was interrupted or was not able to load the data.
 *
 * @pre Must have one or more frames.
 *
 *
 * @sa TecUtilDataValueGetReadableNativeRefByUniqueID(), TecUtilDataValueGetReadableDerivedRef(),
 *     TecUtilDataValueGetReadableNLRef(), TecUtilDataValueGetReadableCCRef()
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON FieldData_pa STDCALL TecUtilDataValueGetWritableNativeRefByUniqueID(UniqueID_t DatasetID,
                                                                                EntIndex_t Zone,
                                                                                EntIndex_t Var);
/**
 * Get a native read/write handle to the data for the specified zone and
 * variable in the data set attached to the current frame. It is important to
 * realize that when altering the values this variable may in fact be shared.
 * If you want the new values to only apply to the specific zone and variable
 * associated with this handle then you must first call
 * TecUtilDataValueBranchShared() prior to calling
 * TecUtilDataValueGetWritableNativeRef().
 * This function is \ref threadsafe.
 *
 * @since
 *   11.2-0-397
 *
 * @param Zone
 *   Number of the zone for which to get the field data
 *
 * @param Var
 *   Number of the variable for which to get the field data
 *
 * @return
 *   A read/write field data handle to the native data for the specified
 *   zone and variable in the data set attached to the current frame or NULL if
 *   Tecplot was interrupted or was not able to load the data.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueGetWritableNativeRef(
 *   &           Zone,
 *   &           Var,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Function that multiplies variable VarNum in Zone ZoneNum by 2.
 * @code
 * static void MultiplyV3By2(EntIndex_t ZoneNum,
 *                           EntIndex_t VarNum)
 * {
 *   // Make sure we are only modifying var VarNum in zone ZoneNum.
 *   if (TecUtilDataValueBranchShared(ZoneNum,VarNum))
 *     {
 *       FieldData_pa FieldData = TecUtilDataValueGetWritableNativeRef(ZoneNum, VarNum);
 *       if (FieldData)
 *         {
 *           int i;
 *           LgIndex_t NumValues = TecUtilDataValueGetCountByRef(FieldData);
 *
 *           // Note 1-based
 *           for (i = 1; i <= NumPointsInZone; i++)
 *             {
 *               double OldValue = TecUtilDataValueGetByRef(FieldData, i);
 *               TecUtilDataValueSetByRef(FieldData,i,OldValue*2.0);
 *             }
 *         }
 *     }
 * }
 * @endcode
 *
 * @sa TecUtilDataValueGetReadableNativeRef(), TecUtilDataValueGetReadableDerivedRef(),
 *     TecUtilDataValueGetReadableNLRef(), TecUtilDataValueGetReadableCCRef()
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON FieldData_pa STDCALL TecUtilDataValueGetWritableNativeRef(EntIndex_t Zone,
                                                                      EntIndex_t Var);
/**
 * @deprecated
 *   Please use TecUtilDataValueGetWritableNativeRef() instead. Calling TecUtilDataValueGetWritableRef()
 *   is equivalent to calling TecUtilDataValueGetWritableNativeRef().
 *
 * @ingroup DataServices
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON FieldData_pa STDCALL TecUtilDataValueGetWritableRef(EntIndex_t Zone,
                                                                EntIndex_t Var);
/**
 * @deprecated
 *   Please use TecUtilDataValueGetReadableNativeRef(), TecUtilDataValueGetReadableDerivedRef(),
 *   TecUtilDataValueGetReadableNLRef(), or TecUtilDataValueGetReadableCCRef() instead.
 *   Calling TecUtilDataValueGetReadableRef() is equivalent to calling TecUtilDataValueGetReadableNativeRef().
 *
 * @ingroup DataServices
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON FieldData_pa STDCALL TecUtilDataValueGetReadableRef(EntIndex_t Zone,
                                                                EntIndex_t Var);

/**
 * @deprecated
 *   Please use TecUtilDataValueGetReadableNativeRef(), TecUtilDataValueGetReadableDerivedRef(),
 *   TecUtilDataValueGetReadableNLRef(), TecUtilDataValueGetReadableCCRef(), or TecUtilDataValueGetWritableNativeRef()
 *   instead. Calling TecUtilDataValueGetRef() is equivalent to calling TecUtilDataValueGetWritableNativeRef().
 *
 * @ingroup DataServices
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON FieldData_pa STDCALL TecUtilDataValueGetRef(EntIndex_t Zone,
                                                        EntIndex_t Var);


/**
 * Get the low-level "get value" function associated with a field
 * data handle. In general, using this function is faster than calling
 * TecUtilDataValueGetByRef().
 * This function is \ref threadsafe.
 *
 * @since
 *   10.0-3-128
 *
 * @par Note:
 *   Do not assume that raw data internal to Tecplot remain in the same
 *   location at all times. Always re-fetch the field data reference and
 *   subsequently the "get value" function after any major event as Tecplot may
 *   move/alter the raw data. Make sure to call TecUtilStateChanged() after any
 *   field values have changed.
 *
 * @param FD
 *   A field data reference usually obtained via a call to one of the following
 *   functions: TecUtilDataValueGetReadableNativeRef(),
 *   TecUtilDataValueGetReadableDerivedRef(), TecUtilDataValueGetReadableNLRef(),
 *   TecUtilDataValueGetReadableCCRef(), or TecUtilDataValueGetWritableNativeRef().
 *
 * @return
 *   The low-level "get value" function associated with a field data handle.
 *   This function takes zero-based values and has no assertions associated
 *   with it.
 *
 *
 * @pre <em>FD</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>FD</em>
 *   The specified field data must be readable.
 *
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Efficiently get the first twenty-one values of the second variable of zone 5:
 * @code
 *   LgIndex pt;
 *   FieldData_pa fd = TecUtilDataValueGetReadableNaiveRef(5, 2);
 *   FieldValueGetFunction_pf GetFunction = TecUtilDataValueRefGetGetFunc(fd);
 *   for ( pt = 0; pt < 21; pt++ ) // use =0 and <21 becaused SetFunction is 0-based
 *     {
 *       double val = GetFunction(fd, pt);
 *       // Use val.
 *     }
 * @endcode
 *
 * @ingroup DataValue
 *
 * #internalattributes exclude_fglue, exclude_tcl
 */
LINKTOADDON FieldValueGetFunction_pf STDCALL TecUtilDataValueRefGetGetFunc(FieldData_pa FD);


/**
 * Get the low-level "set value" function associated with a field data handle.
 * In general, using this function is faster than calling
 * TecUtilDataValueSetByRef().
 * This function is \ref threadsafe.
 *
 * @since
 *   10.0-3-128
 *
 * @par Note:
 *   Do not assume that raw data internal to Tecplot remain in the same
 *   location at all times. Always re-fetch the native field data reference and
 *   subsequently the "set value" function associated with that reference after
 *   any major event as Tecplot itself may move/alter the raw data. Make sure
 *   to call TecUtilStateChanged() after any field values have changed.
 *
 * @param FD
 *   A field data handle usually obtained via TecUtilDataValueGetWritableNativeRef().
 *
 * @return
 *   The low-level "set value" function associated with a field data handle.
 *   This function takes zero-based values and has no assertions associated
 *   with it.
 *
 *
 * @pre <em>FD</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *   None.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Efficiently set the twenty-first values of the second variable of zone 5 to 17.6:
 * @code
 *   Set_pa alteredVars = TecUtilSetAlloc(TRUE);
 *   LgIndex pt;
 *   FieldData_pa fd = TecUtilDataValueGetWritableNativeRef(5, 2);
 *   FieldValueSetFunction_pf setFunction = TecUtilDataValueRefSetGetFunc(fd);
 *   for ( pt = 0; pt < 21; pt++ ) // use =0 and <21 becaused setFunction is 0-based
 *     setFunction(fd, pt, 17.6);
 *
 *   // inform Tecplot of var value change
 *   TecUtilSetAddMember(alteredVars, 2, TRUE);
 *   TecUtilStateChanged(StateChange_VarsAltered,
 *                       (ArbParam_t)alteredVars);
 *   TecUtilSetDealloc(&alteredVars);
 * @endcode
 *
 * @ingroup DataValue
 *
 * #internalattributes exclude_fglue, exclude_tcl
 */
LINKTOADDON FieldValueSetFunction_pf STDCALL TecUtilDataValueRefGetSetFunc(FieldData_pa FD);


/**
 * Get a candidate zone and variable associated with the given field data
 * reference. Note that if the variable is shared then more than one zone and
 * variable combination exists, therefore the first zone and variable
 * combination associated with the field data reference is returned. The
 * following search rules are used: Active relevant zones are searched first
 * and then all remaining enabled zones.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.0-1-099
 *
 * @param FD
 *   A field data reference usually obtained via a call to one of the following
 *   functions: TecUtilDataValueGetReadableNativeRef(),
 *   TecUtilDataValueGetReadableDerivedRef(), TecUtilDataValueGetReadableNLRef(),
 *   TecUtilDataValueGetReadableCCRef(), or TecUtilDataValueGetWritableNativeRef().
 *
 * @param Zone
 *   Pointer to the resulting candidate Zone associated with the field data.
 *
 * @param Var
 *   Pointer to the resulting Variable associated with the field data.
 *
 * @return
 *   Returns TRUE if such a zone and variable exists that uses this field data
 *   handle.
 *
 *
 * @pre <em>FD</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Zone</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>Var</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *   None.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get a candidate zone and variable associated with field data FD;
 *
 * @code
 *   EntIndex_t  Zone;
 *   EntIndex_t  Var;
 *   FieldData_pa FD = TecUtilDataValueGetReadableNaiveRef(5, 2);;
 *
 *   if (TecUtilDataValueGetZoneVarByRef(FD,&Zone,&Var))
 *     {
 *       // Do something with Zone and Var.
 *     }
 *
 * @endcode
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueGetZoneVarByRef(FieldData_pa       FD,
                                                              TP_OUT EntIndex_t* Zone,
                                                              TP_OUT EntIndex_t* Var);

/**
 * Get a readable finite-element node map handle to the specified zone in the
 * data set attached to the current frame.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   Number of the zone for which to get the readable node map handle. This
 *   must be a finite-element zone
 *
 * @return
 *   The finite-element node map handle to the specified zone in the data set
 *   attached to the current frame or NULL if it was not able to load it.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataNodeGetReadableRef(
 *   &           Zone,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataServices
 */
LINKTOADDON NodeMap_pa STDCALL TecUtilDataNodeGetReadableRef(EntIndex_t Zone);

/**
 * Get a writable finite-element node map handle to the specified zone in the
 * data set attached to the current frame.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   Number of the zone for which to get the writable node map handle. This
 *   must be a finite-element zone
 *
 * @return
 *   The finite-element node map handle to the specified zone in the data set
 *   attached to the current frame or NULL if it was not able to load it and
 *   create a writable copy.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataNodeGetWritableRef(
 *   &           Zone,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataServices
 */
LINKTOADDON NodeMap_pa STDCALL TecUtilDataNodeGetWritableRef(EntIndex_t Zone);

/**
 * @deprecated
 *   Please use TecUtilDataNodeGetReadableRef() or TecUtilDataNodeGetWritableRef()
 *   instead. Calling TecUtilDataNodeGetRef() is equivalent to calling TecUtilDataNodeGetWritableRef().
 *
 * @ingroup DataStructure
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON NodeMap_pa STDCALL TecUtilDataNodeGetRef(EntIndex_t Zone);

/**
 * @deprecated
 *   Please use TecUtilDataFaceNbrGetReadableRef() instead.
 *
 * @ingroup FaceNeighbors
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON FaceNeighbor_pa STDCALL TecUtilDataFaceNbrGetRef(EntIndex_t Zone);

/**
 * Get the field data type of a field data handle.
 * This function is \ref threadsafe.
 *
 * @param FieldData
 *   The field data reference for which the data type is desired.
 *
 * @return
 *   The field data type of FieldData. The possible values are:
 *   \ref FieldDataType_Float, \ref FieldDataType_Double, \ref FieldDataType_Int32,
 *   \ref FieldDataType_Int16, \ref FieldDataType_Byte, or \ref FieldDataType_Bit.
 *
 *
 * @pre <em>FieldData</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueGetRefType(FieldDataPtr)
 *    POINTER (FieldDataPtr, FieldData)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the type of the data for variable 2 in zone 5 of the data set attached
 * to the current frame and do something special if that type is
 * \ref FieldDataType_Bit
 *
 * @code
 *   FieldData_pa fd = TecUtilDataValueGetReadableNativeRef(5, 2);
 *   FieldDataType_e field_data_type = TecUtilDataValueGetRefType(fd);
 *   if (field_data_type == FieldDataType_Bit)
 *     {
 *       // do something special
 *     }
 * @endcode
 *
 * @sa TecUtilDataValueGetType()
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON FieldDataType_e STDCALL TecUtilDataValueGetRefType(FieldData_pa FieldData);

/**
 * Queries for the data type of the variable.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.0-0-001
 *
 * @param Zone
 *   The zone number.
 * @param Var
 *   The variable number
 *
 * @return
 *   The data type of the variable.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueGetType(
 *   &                   Zone,
 *   &                   Var)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the data type of variable 2 in zone 5:
 *
 * @code
 *   FieldDataType_e ValLoc = TecUtilDataValueGetType(5, 2);
 * @endcode
 *
 * @sa TecUtilDataValueGetRefType()
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON FieldDataType_e STDCALL TecUtilDataValueGetType(EntIndex_t Zone,
                                                            EntIndex_t Var);
/**
 * Queries for the location of the variable.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   The zone number.
 * @param Var
 *   The variable number
 *
 * @return
 *   The value location of the variable.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueGetLocation(
 *   &                   Zone,
 *   &                   Var)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the value location of variable 2 in zone 5:
 *
 * @code
 *   ValueLocation_e ValLoc = TecUtilDataValueGetLocation(5, 2);
 * @endcode
 *
 * @ingroup DataValue
 *
 * @sa TecUtilDataValueGetLocationByRef
 *
 */
LINKTOADDON ValueLocation_e STDCALL TecUtilDataValueGetLocation(EntIndex_t Zone,
                                                                EntIndex_t Var);
/**
 * Queries for the location of the data values associated with the field data reference.
 * This function is \ref threadsafe.
 *
 * @param FieldData
 *   Handle to the field data. Use TecUtilDataValueGetReadableNativeRef(),
 *   TecUtilDataValueGetReadableDerivedRef(), TecUtilDataValueGetReadableNLRef(),
 *   TecUtilDataValueGetReadableCCRef(), or TecUtilDataValueGetWritableNativeRef()
 *   to get readable or writable handles to the field data.
 *
 * @return
 *   The value location of the variable.
 *
 *
 * @pre <em>FieldData</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueGetLocationByRef(FieldDataPtr)
 *   &                   FieldDataPtr)
 *    POINTER         (FieldDataPtr, FieldData)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataValue
 *
 * @sa TecUtilDataValueGetLocation
 */
LINKTOADDON ValueLocation_e STDCALL TecUtilDataValueGetLocationByRef(FieldData_pa FieldData);


/**
 * Get the instructions of the last data loader used to load the data into the
 * data set attached to the current frame. If a foreign data set loader addon
 * was used to load the data, then the instruction string passed to the loader
 * is returned. If the data was loaded by Tecplot, then the DataSetReaderName
 * returned is "TECPLOT" and each file name in the data set is returned in the
 * DataSetLoaderInstructions stringlist parameter. The current frame must have
 * an attached data set when this function is used.
 *
 * @par Note:
 *   This function now has less usefulness in Tecplot than it once did in
 *   previous versions. Tecplot now maintains a data journal which allows for
 *   more complex data load sequences. It is important to note that other data
 *   altering instructions may follow your data loader instructions. This
 *   function does not provide any such information to the add-on.
 *
 * @param DataSetLoaderName
 *   Name of the data set loader. You must use TecUtilStringDealloc() to free
 *   this string when you are done with it.
 *
 * @param DataSetLoaderInstructions
 *   The data set loader instructions. You must use TecUtilStringListDealloc() to
 *   free this string list when you are done with it.
 *
 * @return
 *   Returns TRUE if the data was loaded using a data set loader.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set.
 *
 * @pre <em>IMPLICATION(DataSetReaderName != NULL,VALID_REF(DataSetReaderName))</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>DataSetLoaderInstructions</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImportGetLoaderInstr(
 *   &                   DataSetLoaderName,
 *   &                   DataSetLoaderNameLength,
 *   &                   DataSetLoaderInstructionsPtr)
 *    CHARACTER*(*)   DataSetLoaderName
 *    INTEGER*4       DataSetLoaderNameLength
 *    POINTER         (DataSetLoaderInstructionsPtr, DataSetLoaderInstructions)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the data set loader and instructions used to load the current frame's
 * data set:
 *
 * @code
 *   if (TecUtilDataSetIsAvailable())
 *     {
 *       char* LoaderName = NULL;
 *       StringList_pa LoaderInstructs = NULL;
 *       Boolean_t IsOk = TecUtilImportGetLoaderInstr(&LoaderName,
 *                                                    &LoaderInstructs);
 *       if (IsOk                    &&
 *           LoaderName != NULL      &&
 *           LoaderInstructs != NULL &&
 *           strcmp(LoaderName, "BANANA") == 0)
 *         {
 *           ...
 *         }
 *
 *       if (LoaderName != NULL)
 *         TecUtilStringDealloc(&LoaderName);
 *       if (LoaderInstructs != NULL)
 *         TecUtilStringListDealloc(&LoaderInstructs);
 *     }
 * @endcode
 *
 * @ingroup AddOnLoaders
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilImportGetLoaderInstr(TP_GIVES char**         DataSetLoaderName,
                                                          TP_GIVES StringList_pa* DataSetLoaderInstructions);

/**
 * Display a message to the user and, if desired, prompt for yes or no input.
 *
 * @par Note:
 *   If this function is called when Tecplot is running in batch mode, then it will
 *   return right away and return TRUE;
 *
 * @param Message
 *   Character string to display at the top of the dialog. Must not be NULL
 *
 * @param MessageBoxType
 *   The possible values are: Buttons Included:MessageBoxType_Error :
 *   OKMessageBox_Warning : OKMessageBox_Information : OKMessageBox_Question :
 *   OK, CancelMessageBox_YesNo : Yes, No
 *
 * @return
 *   TRUE if the OK or Yes is clicked, FALSE if Cancel or No is clicked.
 *
 *
 * @pre <em>Message</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogMessageBox(
 *   &                   Message,
 *   &                   MessageBoxType)
 *    CHARACTER*(*)   Message
 *    INTEGER*4       MessageBoxType
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Prompt the user for a response to a question:
 *
 * @code
 *   if (TecUtilDialogMessageBox("Do you like green eggs and ham?",
 *                               MessageBoxType_Question))
 *    {
 *      // do something here...
 *    }
 * @endcode
 *
 * @sa TecUtilMacroIsBatchModeActive()
 *
 * @ingroup UserInterface
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogMessageBox(const char       *Message,
                                                      MessageBoxType_e  MessageBoxType);

/**
 * Display the last recorded message by Tecplot, if any.
 *
 * @return
 *   TRUE if a message exists and was displayed, FALSE if no message existed to display.
 *
 * @since 14.1
 *
 * @sa TecUtilLastErrorMessage()
 *
 * @ingroup UserInterface
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogLastMessageBox(void);


/**
 * Launch a dialog that prompts the user for a color or for a multi-color
 * setting.
 *
 * @since
 *   10.0-3-129
 *
 * @par Note:
 *   This function cannot be called when Tecplot is running in batch mode.
 *
 * @param AllowMultiColor
 *   Set to TRUE if you wish to allow the user to select RGB or multi-color.
 *
 * @param Color
 *   The returned color value. If AllowMultiColor is FALSE then this is in the
 *   range between Black_C and Custom56_C (See GLOBAL.h). If AllowMultiColor
 *   is TRUE then the following constants may also be returned:
 *
 * @verbatim
     MultiColor_C             The user selected the first "MultiColor" option
                              which implies they want the object colored by
                              the contour variable used by contour group 1.

     MultiColor2_C            The user selected the "C2" button
                              which implies they want the object colored by
                              the contour variable used by contour group 2.

     MultiColor3_C            The user selected the "C3" button
                              which implies they want the object colored by
                              the contour variable used by contour group 3.

     MultiColor4_C            The user selected the "C4" button
                              which implies they want the object colored by
                              the contour variable used by contour group 4.

     MultiColor5_C            The user selected the "C5" button
                              which implies they want the object colored by
                              the contour variable used by contour group 5.

     MultiColor6_C            The user selected the "C6" button
                              which implies they want the object colored by
                              the contour variable used by contour group 6.

     MultiColor7_C            The user selected the "C7" button
                              which implies they want the object colored by
                              the contour variable used by contour group 7.

     MultiColor8_C            The user selected the "C8" button
                              which implies they want the object colored by
                              the contour variable used by contour group 8.

     RGBColor_C               The user selected the "RGB" button
                              which implies they want the object colored by
                              RGB.
   @endverbatim
 *
 * @return
 *   TRUE if successful, FALSE if the user pressed the "Cancel" button in the
 *   dialog.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogGetColor(
 *   &                   AllowMultiColor,
 *   &                   Color)
 *    INTEGER*4       AllowMultiColor
 *    INTEGER*4       Color
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Prompt the user for a basic color (no multi-color options):
 *
 * @code
 *   ColorIndex_t Color;
 *   if (TecUtilDialogGetColor(FALSE, // AllowMultiColor
 *                             &Color))
 *     {
 *       // Do something with Color.
 *     }
 * @endcode
 *
 * @sa TecUtilMacroIsBatchModeActive()
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogGetColor(Boolean_t            AllowMultiColor,
                                                    TP_OUT ColorIndex_t* Color);




/**
 * Launch a dialog to prompt the user to input into a simple text field.
 *
 * @par Note:
 *   This function cannot be called when Tecplot is running in batch mode.
 *
 * @param Instructions
 *   String containing the instructions for the user. Tecplot will wrap the
 *   instructions for you.  However, if you include a newline in the string it
 *   will force a new line. Under Windows you are limited to three lines of
 *   text
 *
 * @param DefaultText
 *   Set DefaultText to NULL if you want the default text
 *   to be blank, otherwise pass a default string. If you pass
 *   DefaultText you must deallocate Text no matter what the
 *   return value is.
 *
 * @param Text
 *   The resulting text string is placed here.
 *
 * @return
 *   TRUE if the user enters text and clicks OK.
 *
 *
 * @pre <em>Instructions</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>DefaultText</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>Text</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogGetSimpleText(
 *   &                   Instructions,
 *   &                   DefaultText,
 *   &                   Text,
 *   &                   TextLength)
 *    CHARACTER*(*)   Instructions
 *    CHARACTER*(*)   DefaultText
 *    CHARACTER*(*)   Text
 *    INTEGER*4       TextLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Prompt the user for text:
 *
 * @code
 *   char *Text = NULL;
 *
 *   if (TecUtilDialogGetSimpleText("Enter your name","Fred",&Text))
 *     {
 *       // Do somthing with Text
 *     }
 *   if ( Text )
 *     TecUtilStringDealloc(&Text);
 * @endcode
 *
 * @sa TecUtilMacroIsBatchModeActive()
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogGetSimpleText(const char*     Instructions,
                                                         const char*     DefaultText,
                                                         TP_GIVES char** Text);

/**
 * Query Tecplot to see if a macro function called FunctionName exists.
 *
 * @par Note:
 *   This function requires Tecplot Version 7.5-0-6 or newer.
 *
 * @param FunctionName
 *   Name of the macro function.
 *
 * @return
 *   TRUE if the function exists, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroFunctionExists(FunctionName)
 *    CHARACTER*(*) FunctionName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   If the macro function "abc" exists, then execute it.
 *
 * @code
 *   if (TecUtilMacroFunctionExists("abc"))
 *      {
 *         TecUtilMacroRunFunction("abc",(char *)NULL);
 *      }
 * @endcode
 *
 * @ingroup ScriptSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroFunctionExists(const char *FunctionName);

/**
 *   Determine if Tecplot is currently running in batch mode.
 *   This function is \ref threadsafe.
 *
 * @return
 *   TRUE if Tecplot is in batch mode, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroIsBatchModeActive()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Perform some operations if Tecplot is not running in batch mode:
 *
 * @code
 *   if (!TecUtilMacroIsBatchModeActive())
 *     {
 *       // Perform some operations
 *     }
 * @endcode
 *
 * @ingroup ScriptSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroIsBatchModeActive(void);

/**
 *
 * Get the number of pixels per inch in the vertical and horizontal directions on the screen.
 *
 * @param VDotsPerInch
 *   The number of pixels per inch in the vertical direction
 *
 * @param HDotsPerInch
 *   The number of pixels per inch in the horizontal direction.
 *
 *
 * @pre <em>VDotsPerInch</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>HDotsPerInch</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilInterfaceGetDotsPerInch(
 *   &           VDotsPerInch,
 *   &           HDotsPerInch)
 *    REAL*8          VDotsPerInch
 *    REAL*8          HDotsPerInch
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the pixels per inch values:
 *
 * @code
 *   double VertPixels, HorzPixels;
 *   TecUtilInterfaceGetDotsPerInch(&VertPixels, &HorzPixels);
 * @endcode
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilInterfaceGetDotsPerInch(TP_OUT double* VDotsPerInch,
                                                        TP_OUT double* HDotsPerInch);

/**
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_alldoc
 */
LINKTOADDON int STDCALL TecUtilInterfaceGetBaseFontSize(void);


/**
 * Fetch an Array of values by reference.
 * This function fetches the specified number of values from the source field
 * data starting at the specified source item offset and copies them to the
 * base of the destination value array. The destination value array must be of
 * the same data type as the source field data. In addition, data of type
 * @ref FieldDataType_Bit is currently not supported for array access.
 * This function is \ref threadsafe.
 *
 * @since
 *     10.0-3-12
 *
 * @param SourceFieldData
 *     Field data containing the data to fetch.
 * @param SourceOffset
 *     Member offset in the source field data to begin fetching values.
 * @param SourceCount
 *     Number of values to fetch from the source field data.
 * @param DestValueArray
 *     Pre-allocated array large enough to hold the requested members. The
 *     first member is placed at the base of the array. The native type of the
 *     array must match that of the field data.
 *
 *
 * @pre <em>SourceFieldData</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>SourceFieldData</em>
 *   The specified field data must be readable.
 *
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON void STDCALL TecUtilDataValueArrayGetByRef(FieldData_pa       SourceFieldData,
                                                       LgIndex_t          SourceOffset,
                                                       LgIndex_t          SourceCount,
                                                       TP_ARRAY_OUT void* DestValueArray);
/**
 * Get a field data value. To use this function you must have already obtained
 * a handle to field data.
 * This function is \ref threadsafe.
 *
 * @param FieldData
 *   A field data reference usually obtained via a call to one of the following
 *   functions: TecUtilDataValueGetReadableNativeRef(),
 *   TecUtilDataValueGetReadableDerivedRef(), TecUtilDataValueGetReadableNLRef(),
 *   or TecUtilDataValueGetReadableCCRef().
 *
 * @param PointIndex
 *   Position in the array of field data values. Position starts at one. If
 *   FieldData came from an IJ- or IJK-ordered zone then the position is
 *   calculated by treating the two- or three-dimensional array as a
 *   one-dimensional array.
 *
 * @return
 *   The value at a given position in field data FieldData
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilDataValueGetByRef(
 *   &                   FieldDataPtr,
 *   &                   PointIndex)
 *    POINTER         (FieldDataPtr, FieldData)
 *    INTEGER*4       PointIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the first twenty data values for the second variable in zone 5:
 *
 * @code
 *   FieldData_pa fd = TecUtilDataValueGetReadableNativeRef(5, 2);
 *   LgIndex_t numpts = TecUtilDataValueGetCountByRef(fd);
 *   if ( fd )
 *     {
 *       int ii;
 *       for ( ii = 1; ii <= numpts; ii++ )
 *         {
 *           double val = TecUtilDataValueGetByRef(fd, ii);
 *           // do something with val
 *         }
 *     }
 * @endcode
 *
 * @sa TecUtilDataValueRefGetGetFunc() for obtaining a function as a high
 *     performance alternative.
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON double STDCALL TecUtilDataValueGetByRef(FieldData_pa FieldData,
                                                    LgIndex_t    PointIndex);
/**
 * Indicates if the minimum and maximum values for the Tecplot variable using the zone and variable
 * number are valid. A variable's min/max values can be invalid if they weren't supplied by the
 * loader or if an operation modifies the variable values. Tecplot calculates the min/max values and
 * validates them for a variable when it is needed for plotting or some other operation, otherwise a
 * call to TecUtilDataValueGetMinMaxByRef() or TecUtilDataValueGetMinMaxByZoneVar() can be used to
 * explicitly update the values.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   The zone number of the variable to be examined for min/max values.
 *
 * @param Var
 *   The variable number to be examined for min/max values.
 *
 * @return
 *   TRUE if the variable's minimum/maximum values are valid, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueIsMinMaxValidByZoneVar(
 *   &           Zone,
 *   &           Var)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since
 *   14.0
 *
 * @sa TecUtilDataValueGetMinMaxByRef(),
 *     TecUtilDataValueGetMinMaxByZoneVar()
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueIsMinMaxValidByZoneVar(EntIndex_t Zone,
                                                                     EntIndex_t Var);

/**
 * Get the minimum and maximum values for a Tecplot variable using the zone and variable number. If
 * the min/max values are valid it simply returns them otherwise it makes sure the variable is
 * loaded, then recalculates the values and returns them. A variable's min/max values can be invalid
 * if they weren't supplied by the loader or if an operation modifies the variable values. Tecplot
 * calculates the min/max values and validates them for a variable when it is needed for plotting or
 * some other operation, otherwise a call to TecUtilDataValueGetMinMaxByRef() or
 * TecUtilDataValueGetMinMaxByZoneVar() can be used to explicitly update the values.
 *
 * @since
 *   11.0-0-007
 *
 * @param Zone
 *   The zone number of the variable to be examined for min/max values.
 *
 * @param Var
 *   The variable number to be examined for min/max values.
 *
 * @param Min
 *   Returned minimum value.
 *
 * @param Max
 *   Returned maximum value.
 *
 * @return
 *   TRUE if the min/max value could be retrieved, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>Min</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Max</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueGetMinMaxByZoneVar(
 *   &           Zone,
 *   &           Var,
 *   &           Min,
 *   &           Max)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 *    REAL*8          Min
 *    REAL*8          Max
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the minimum and maximum values for the third variable in zone 2 in the
 * current data set.
 *
 * @code
 *   double Min;
 *   double Max;
 *   TecUtilDataValueGetMinMaxByZoneVar(2,3,&Min,&Max);
 * @endcode
 *
 * @sa TecUtilDataValueSetMinMaxByZoneVar()
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueGetMinMaxByZoneVar(EntIndex_t     Zone,
                                                                 EntIndex_t     Var,
                                                                 TP_OUT double* Min,
                                                                 TP_OUT double* Max);

/**
 * Get the minimum and maximum values for a Tecplot variable using the field data reference. If the
 * min/max values are valid it simply returns them otherwise it recalculates the values and returns
 * them. A variable's min/max values can be invalid if they weren't supplied by the loader or if an
 * operation modifies the variable values. Tecplot calculates the min/max values and validates them
 * for a variable when it is needed for plotting or some other operation, otherwise a call to
 * TecUtilDataValueGetMinMaxByRef() or TecUtilDataValueGetMinMaxByZoneVar() can be used to
 * explicitly update the values.

 * This function is \ref threadsafe.
 *
 * @param FieldData
 *   A readable field data reference usually obtained via a call to one of the following
 *   functions: TecUtilDataValueGetReadableNativeRef(),
 *   TecUtilDataValueGetReadableDerivedRef(), TecUtilDataValueGetReadableNLRef(),
 *   or TecUtilDataValueGetReadableCCRef().
 *
 * @param Min
 *   Returned minimum value.
 *
 * @param Max
 *   Returned maximum value.
 *
 * @return
 *   TRUE if the min/max value could be retrieved, FALSE otherwise.
 *
 *
 * @pre <em>FieldData</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>FieldData</em>
 *   The specified field data must be readable.
 *
 * @pre <em>Min</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Max</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueGetMinMaxByRef(
 *   &           FieldDataPtr,
 *   &           Min,
 *   &           Max)
 *    POINTER         (FieldDataPtr, FieldData)
 *    REAL*8          Min
 *    REAL*8          Max
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the minimum and maximum values for the third variable in zone 2 in the
 * current data set.
 *
 * @code
 *   FieldData_pa FD;
 *   double       Min;
 *   double       Max;
 *
 *   FD = TecUtilDataValueGetReadableNativeRef(2,3);
 *   TecUtilDataValueGetMinMaxByRef(FD,&Min,&Max);
 * @endcode
 *
 * @sa TecUtilDataValueGetYMinMaxByZoneVar(),
 *     TecUtilDataValueSetMinMaxByZoneVar()
 *
 * @ingroup DataValue
 */
LINKTOADDON void STDCALL TecUtilDataValueGetMinMaxByRef(FieldData_pa   FieldData,
                                                        TP_OUT double* Min,
                                                        TP_OUT double* Max);
/**
 * Get the node index for a particular corner of a finite-element. This
 * function does not require you to obtain the handle to the node map as does
 * TecUtilDataNodeGetByRef(), however, this function is not very efficient. Use
 * TecUtilDataNodeGetByRef() if you are getting multiple nodes from the same
 * zone.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   Zone number. This must be a finite-element zone.
 *
 * @param Element
 *   The element number (starts at 1)
 *
 * @param Corner
 *   The element corner (starts at 1)
 *
 * @return
 *   The index of the node or zero if the node map could not be loaded.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataNodeGetByZone(
 *   &                   Zone,
 *   &                   Element,
 *   &                   Corner)
 *    INTEGER*4       Zone
 *    INTEGER*4       Element
 *    INTEGER*4       Corner
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the third node of the 43rd element of zone 5:
 *
 * @code
 *   NodeMap_t n3;
 *   n3 = TecUtilDataNodeGetByZone(5, 43, 3);
 * @endcode
 *
 * @ingroup DataStructure
 *
 */
LINKTOADDON NodeMap_t STDCALL TecUtilDataNodeGetByZone(EntIndex_t Zone,
                                                       LgIndex_t  Element,
                                                       LgIndex_t  Corner);

/**
 * Fetch an array of nodes by reference.
 * This function fetches the specified number of nodes from the source node
 * map starting at the specified source item offset and copies them to the
 * base of the destination node array.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.0-0-019
 *
 * @param SourceNodeMap
 *   Node map containing the data to fetch.
 * @param SourceOffset
 *   Node offset in the source node map to begin fetching nodes.
 * @param SourceCount
 *   Number of nodes to fetch from the source node map.
 * @param DestNodeArray
 *   Pre-allocated array large enough to hold the requested node. The first
 *   node is placed at the base of the array. The node values are one based.
 *
 *
 * @pre <em>SourceNodeMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>DestNodeArray</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataStructure
 *
 */
LINKTOADDON void STDCALL TecUtilDataNodeArrayGetByRef(NodeMap_pa              SourceNodeMap,
                                                      LgIndex_t               SourceOffset,
                                                      LgIndex_t               SourceCount,
                                                      TP_ARRAY_OUT NodeMap_t* DestNodeArray);

/**
 * Get the node index for a particular corner of a finite-element. To use this
 * function you must have already obtained a handle to a node map.
 * This function is \ref threadsafe.
 *
 * @param NodeMapPtr
 *   Handle to the connectivity list (that is, the node map). Use
 *   TecUtilDataNodeGetByRef() or TecUtilZoneGetInfo() to get handles to the
 *   node map
 *
 * @param Element
 *   The element number (starts at 1)
 *
 * @param Corner
 *   The element corner (starts at 1)
 *
 * @return
 *   The index of the node.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataNodeGetByRef(
 *   &                   NodeMapPtrPtr,
 *   &                   Element,
 *   &                   Corner)
 *    POINTER         (NodeMapPtrPtr, NodeMapPtr)
 *    INTEGER*4       Element
 *    INTEGER*4       Corner
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the first two nodes of the 43rd element of zone 5:
 *
 * @code
 *   NodeMap_pa nm;
 *   nm = TecUtilDataNodeGetReadableRef(5);
 *   if ( nm )
 *     {
 *       NodeMap_t n1, n2;
 *       n1 = TecUtilDataNodeGetByRef(nm, 43, 1);
 *       n2 = TecUtilDataNodeGetByRef(nm, 43, 2);
 *       // use n1 and n2
 *     }
 * @endcode
 *
 * @ingroup DataStructure
 *
 */
LINKTOADDON NodeMap_t STDCALL TecUtilDataNodeGetByRef(NodeMap_pa NodeMapPtr,
                                                      LgIndex_t  Element,
                                                      LgIndex_t  Corner);
/**
 * Get the number of nodes per element. To use this
 * function you must have already obtained a handle to a node map.
 * This function is \ref threadsafe.
 *
 * @since
 *   12.1.1.6765
 *
 * @param NodeMapPtr
 *   Handle to the connectivity list (that is, the node map). Use
 *   TecUtilDataNodeGetByRef() or TecUtilZoneGetInfo() to get handles to the
 *   node map
 *
 *
 * @return
 *   The number of nodes per element.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataNodeGetNodesPerElem(
 *   &                   NodeMapPtrPtr)
 *    POINTER         (NodeMapPtrPtr, NodeMapPtr)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *
 * @ingroup DataStructure
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilDataNodeGetNodesPerElem(NodeMap_pa NodeMapPtr);
/**
 * @deprecated
 *   Please use TecUtilDataFaceNbrGetNbrByRef() instead.
 *
 * @ingroup DataServices
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL  TecUtilDataFaceNbrGetByZone(EntIndex_t Zone,
                                                           LgIndex_t  Element,
                                                           LgIndex_t  Face);
/**
 * @deprecated
 *   Please use TecUtilDataFaceNbrGetNbrByRef() instead.
 *
 * @ingroup DataServices
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL  TecUtilDataFaceNbrGetByRef(FaceNeighbor_pa FaceNeighbor,
                                                          LgIndex_t       Element,
                                                          LgIndex_t       Face);

/**
 * Returns the FaceNeigborMode_e value for the referenced zone.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   Zone for the query.
 *
 * @return
 *   Mode of the face neighbor or FaceNeighborMode_Invalid if face neighbors are not applicable
 *   for the zone (such as for linear or Polytope zone types).
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataFaceNbrGetModeByZone(
 *   &                   Zone)
 *    INTEGER*4       Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup FaceNeighbors
 *
 */
LINKTOADDON FaceNeighborMode_e STDCALL TecUtilDataFaceNbrGetModeByZone(EntIndex_t Zone);

/**
 *   Returns the FaceNeigborMode_e value for the referenced face neighbor.
 *   This function is \ref threadsafe.
 *
 * @param FaceNeighbor
 *   The face neighbor handle to the specified zone in the data set attached to the current frame
 *
 * @return
 *   Mode of the face neighbor.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataFaceNbrGetModeByRef(FaceNeighborPtr)
 *    POINTER (FaceNeighborPtr, FaceNeighbor)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup FaceNeighbors
 *
 */
LINKTOADDON FaceNeighborMode_e STDCALL TecUtilDataFaceNbrGetModeByRef(FaceNeighbor_pa FaceNeighbor);

/**
 * Gets the number of face neighbors for the elements's face.
 * This function is \ref threadsafe.
 *
 * @param FaceNeighbor
 *   Handle to the face neighbors. Use TecUtilDataFaceNbrGetReadableRef() to get
 *   handles to the face neighbors.
 *
 * @param Element
 *   The element number (starts at one)
 *
 * @param Face
 *   The face number of the element. Different element types have different
 *   number of faces. Use TecUtilZoneGetType() to get the element type for a
 *   particular zone. Face numbers start at one.ZoneType_FETriangle: Three
 *   faces.ZoneType_FEQuad: Four faces.ZoneType_FETetra: Four
 *   faces.ZoneType_FEBrick: Six faces
 *
 * @return
 *   Number of neighbors for the element's face.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataFaceNbrGetNumNByRef(
 *   &                   FaceNeighborPtr,
 *   &                   Element,
 *   &                   Face)
 *    POINTER         (FaceNeighborPtr, FaceNeighbor)
 *    INTEGER*4       Element
 *    INTEGER*4       Face
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the number of neighbors for face 5 of element 23 of zone 2. It is assumed that zone 2 is of
 *   type ZoneType_FEBrick.
 *
 * @code
 *   LgIndex_t NumNeighbors;
 *   FaceNeighbor_pa FNbr;
 *   FNbr = TecUtilDataFaceNbrGetReadableRef(2);
 *   NumNeighbors = TecUtilDataFaceNbrGetNumNByRef(FNbr, 23, 5);
 * @endcode
 *
 * @ingroup FaceNeighbors
 *
 */
LINKTOADDON LgIndex_t STDCALL  TecUtilDataFaceNbrGetNumNByRef(FaceNeighbor_pa FaceNeighbor,
                                                              LgIndex_t       Element,
                                                              LgIndex_t       Face);
/**
 * Get the cell index of the element the is a neighbor of the specified Element
 * and Face. To use this function you must have already obtained a handle to
 * face neighbors.
 * This function is \ref threadsafe.
 *
 * @param FaceNeighbor
 *   Handle to the face neighbors. Use TecUtilDataFaceNbrGetReadableRef() to get
 *   handles to the face neighbors.
 *
 * @param Element
 *   The element number (starts at one).
 *
 * @param Face
 *   The face number of the element. Different element types have different
 *   number of faces.  Use TecUtilZoneGetType() to get the element type for a
 *   particular zone.  Face numbers start at one.
 *   ZoneType_FETriangle: has three faces.   ZoneType_FEQuad has four faces.
 *   ZoneType_FETetra has four faces.  ZoneType_FEBrick has six faces.
 *   Please look for "Face Neighbors" in the Tecplot User's Manual
 *   for a description of how nodes and faces map to each other.
 *
 * @param NeighborNumber
 *   Specify which neighbor to retrieve. Use TecUtilDataFaceNbrGetNumNByRef()
 *   to get the number of neighbors
 *
 * @param NeighborElem
 *   Pointer that gives the value of the neighboring element number
 *
 * @param NeighborZone
 *   Pointer that gives the value of the neighboring zone number
 *
 *
 * @pre <em>NeighborElem</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>NeighborZone</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataFaceNbrGetNbrByRef(
 *   &           FaceNeighborPtr,
 *   &           Element,
 *   &           Face,
 *   &           NeighborNumber,
 *   &           NeighborElem,
 *   &           NeighborZone)
 *    POINTER         (FaceNeighborPtr, FaceNeighbor)
 *    INTEGER*4       Element
 *    INTEGER*4       Face
 *    INTEGER*4       NeighborNumber
 *    INTEGER*4       NeighborElem
 *    INTEGER*4       NeighborZone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the cell index of the cell next to face 5 of element 23 in zone 2. It is assumed that zone 2
 *   is of type ZoneType_FEBrick.
 *
 * @code
 *   FaceNeighbor_pa FNPtr = TecUtilDataFaceNbrGetReadableRef(2);
 *   if (FNPtr != NULL)
 *     {
 *       LgIndex_t  NeighborElem;
 *       EntIndex_t NeighborZone;
 *       TecUtilDataFaceNbrGetNbrByRef(FNPtr, 23, 5, 1,
 *                                     &NeighborElem,
 *                                     &NeighborZone);
 *
 *       // Do something with NeighborElem and NeighborZone.
 *    }
 * @endcode
 *
 * @ingroup FaceNeighbors
 *
 */
LINKTOADDON void STDCALL TecUtilDataFaceNbrGetNbrByRef(FaceNeighbor_pa    FaceNeighbor,
                                                       LgIndex_t          Element,
                                                       LgIndex_t          Face,
                                                       LgIndex_t          NeighborNumber,
                                                       TP_OUT LgIndex_t*  NeighborElem,
                                                       TP_OUT EntIndex_t* NeighborZone);
/**
 * @deprecated
 *
 * @ingroup FaceNeighbors
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataFaceNbrBeginAssignX(ArgList_pa ArgList);

/**
 * @deprecated
 *
 * @ingroup FaceNeighbors
 *
 */
LINKTOADDON Boolean_t STDCALL  TecUtilDataFaceNbrBeginAssign(EntIndex_t Zone);

/**
 * @deprecated
 *
 *  Use TecUtilDataFaceNbrAssignByRef() instead.
 *
 * @ingroup FaceNeighbors
 *
 */
LINKTOADDON Boolean_t STDCALL  TecUtilDataFaceNbrAssign(LgIndex_t   Element,
                                                        LgIndex_t   Face,
                                                        Boolean_t   NeighborsCompletelyObscure,
                                                        LgIndex_t   NumNeighbors,
                                                        LgIndex_t  *NeighborElems,
                                                        EntIndex_t *NeighborZones);

/**
 * @deprecated
 *
 * Use TecUtilDataFaceNbrAssignArrayByRef() instead.
 *
 * @ingroup FaceNeighbors
 */
LINKTOADDON void STDCALL  TecUtilDataFaceNbrArrayAssign(LgIndex_t  DestOffset,
                                                        LgIndex_t  DestCount,
                                                        LgIndex_t *NeighborElems);

/**
 * @deprecated
 *
 * Use TecUtilDataFaceNbrCustomLOD() instead.
 *
 * @ingroup FaceNeighbors
 *
 */
LINKTOADDON Boolean_t STDCALL  TecUtilDataFaceNbrEndAssign(void);

/**
 * Return the custom load-on-demand client data from a face neighbor handle.
 * The client data should ONLY be retrieved in response to a custom load,
 * unload, or cleanup callback. At no other time is the request valid.
 * This function is \ref threadsafe.
 *
 * @param FaceNeighbor
 *     Custom load-on-demand face neighbor handle.
 *
 * @return
 *     Client data for the custom load-on-demand add-on.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *     Boolean_t STDCALL MyFaceNbrLoader(FaceNeighbor_pa FaceNeighbor)
 *     {
 *       Boolean_t Result;
 *       MyClientData_s *MyClientData = (MyClientData_s *)TecUtilDataFaceNbrGetClientData(FaceNeighbor);
 *
 *       // load the custom face neighbor using client data
 *       .
 *       .
 *       .
 *
 *     return Result;
 *   }
 * @endcode
 *
 * @sa TecUtilDataFaceNbrCustomLOD()
 *
 * @ingroup DataServices
 */
LINKTOADDON ArbParam_t STDCALL TecUtilDataFaceNbrGetClientData(FaceNeighbor_pa FaceNeighbor);

/**
 * Registers with Tecplot the load-on-demand callbacks and client data for the
 * face neighbors for a specific zone. Tecplot will notify the add-on via the
 * callbacks when the face neighbors need to be loaded, unloaded, and cleaned
 * up.
 *
 * All callbacks must be written in a thread-safe manner so that Tecplot can
 * make concurrent requests to load (and/or unload) multiple zone's face
 * neighbors. The easiest way to write thread-safe callbacks is not to use any
 * shared state (i.e. global or static state) in order to perform the requested
 * action but instead to use private client data to maintain all the
 * information needed to perform the requested action.
 *
 * Calls made back to Tecplot in response to a load, unload, or cleanup request
 * should be limited to queries except in the case where data is being loaded
 * into a face neighbor. In addition, no state changes should be broadcast by
 * the callbacks.
 *
 * This function is used in conjunction with deferred variable creation. See
 * the SV_DEFERFACENBRCREATION option for TecUtilDataSetAddZoneX()for details.
 *
 * The method for loading and accessing face neighbor data with custom
 * load-on-demand is similar to custom load-on-demand for field data (see ADK
 * Users Manual for details): The add-on supplied LoadCallback() callback is
 * responsible for loading the entire face neighbor data into the Tecplot
 * prepared face neighbor backing. Tecplot is responsible for allocating and
 * freeing the space for the face neighbor backing. In addition, the add-on
 * must supply the CleanupCallback() callback to receive notification of when
 * the face neighbor is no longer needed. Optionally, the add-on may supply the
 * UnloadCallback() callback to receive notification of when the face neighbor
 * is being unloaded. Most add-ons should supply NULL for the UnloadCallback()
 * callback, instructing Tecplot to assume responsibility for unloading the
 * face neighbor and re-loading it in an efficient form.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.3-0-020
 *
 * @param Zone
 *   Zone for which the face neighbor will now be custom load-on-demand.
 *
 * @param AutoAssignFN
 *   Indicates if Tecplot should auto assign any remaining face neighbors after
 *   the add-on has supplied the boundary connection face neighbors. This is
 *   useful when an add-on only needs to deliver a few specific neighbors. See
 *   TecUtilDataFaceNbrAssignArrayByRef() and TecUtilDataFaceNbrAssignByRef()
 *   for details. Add-ons that wish to supply all the face neighbor connections
 *   should set this value to FALSE.
 *
 * @param LoadCallback
 *   Tecplot calls this callback when the face neighbor is to be loaded. The
 *   LoadCallback() callback may never get called if the face neighbor is not
 *   needed or it may get called immediately if load-on-demand capabilities are
 *   not available. This callback is called asynchronously.
 *
 * @param UnloadCallback
 *   Add-ons can supply NULL for this callback. Supplying NULL instructs Tecplot to handle the
 *   unloading (and subsequent reloading) of the face neighbor without the intervention of the
 *   add-on, however Tecplot will be forced to write the data to its temporary directory when
 *   unloaded thereby incurring additional I/O expense. If the add-on does supply this callback,
 *   Tecplot calls it when the face neighbor is to be unloaded.  This query provides the add-on an
 *   opportunity to allow or deny a face neighbor to be unloaded by returning TRUE or FALSE
 *   respectively. Unless there is a compelling reason, such as very expensive load costs (in which
 *   case NULL should probably be supplied for this callback), the add-on should honor Tecplot's
 *   request to unload the face neighbor (i.e. the UnloadCallback() callback should return TRUE). An
 *   add-on may also cleanup any private resources that are not needed when the face neighbor is
 *   unloaded, however the add-on must still maintain enough information to load the face neighbor
 *   again if requested by Tecplot. The UnloadCallback() callback may never get called if the face
 *   neighbor does not need to be unloaded nor will the UnloadCallback() callback necessarily be
 *   called before the CleanupCallback() callback. This callback is called asynchronously.
 *
 * @param CleanupCallback
 *   Tecplot calls this callback when the face neighbor is to be cleaned up. This
 *   allows the add-on to cleanup any private resources that were used in
 *   conjunction with identifying or loading this face neighbor. After a face
 *   neighbor is cleaned up Tecplot will never again request it to be loaded.
 *   Tecplot may or may not call the UnloadCallback() callback before calling
 *   the CleanupCallback() callback. Additionally, the CleanupCallback() callback
 *   will be called even if the face neighbor was never loaded. This callback
 *   is called asynchronously.
 *
 * @param ClientData
 *   Private client data needed by the custom load-on-demand callbacks to
 *   perform the duties of loading, unloading, and cleaning up the face
 *   neighbor. Tecplot stores the client data in the face neighbor structure
 *   and must be retrieved by the callbacks using TecUtilDataFaceNbrGetClientData().
 *   The client data should ONLY be retrieved in response to a custom load,
 *   unload, or cleanup callback. At no other time is the request valid.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * Following is an example of how to create a face neighbor using the Custom
 * Load on Demand.
 * @code
 *   typedef struct
 *     {
 *       char* DataFileName;
 *       long  SeekOffset;
 *       ... other information needed to load face neighbor data
 *     } MyFaceNbrClientData_s;
 *
 *   Boolean_t STDCALL MyFaceNbrLoader(FaceNeighbor_pa FaceNeighbor)
 *   {
 *     REQUIRE(VALID_REF(FaceNeighbor));
 *
 *     MyFaceNbrClientData_s *MyClientData = (MyFaceNbrClientData_s *)TecUtilDataFaceNbrGetClientData(FaceNeighbor);
 *
 *     // open the data file
 *     FILE *MyDataFile = fopen(MyClientData->DataFileName, "rb");
 *     Boolean_t IsOk = (MyDataFile != NULL);
 *
 *     // seek to the place in the file where the face neighbor data is located
 *     IsOk = IsOk && (fseek(MyDataFile, MyClientData->SeekOffset, SEEK_SET) == 0);
 *     if (IsOk)
 *       {
 *         // load the data into the zone's face neighbor
 *         IsOk = ReadMyFaceNbrDataIntoZone(MyDataFile,
 *                                          MyClientData,
 *                                          FaceNeighbor);
 *       }
 *
 *     // cleanup
 *     if (MyDataFile != NULL)
 *       fclose(MyDataFile);
 *
 *     ENSURE(VALID_BOOLEAN(IsOk));
 *     return IsOk;
 *   }
 *
 *   Boolean_t STDCALL MyFaceNbrUnload(FaceNeighbor_pa FaceNeighbor)
 *   {
 *     REQUIRE(VALID_REF(FaceNeighbor));
 *
 *     // We don't have any private data to cleanup (i.e. in addition to the
 *     // private client data which we don't cleanup here) so all we have to do
 *     // is return TRUE or FALSE letting Tecplot know that it can or can not
 *     // unload the face neighbor.
 *     Boolean_t Result = TRUE; // ...tell Tecplot to go ahead and unload the face neighbor
 *
 *     ENSURE(VALID_BOOLEAN(Result));
 *     return Result;
 *   }
 *
 *   void STDCALL MyFaceNbrCleanup(FaceNeighbor_pa FaceNeighbor)
 *   {
 *     REQUIRE(VALID_REF(FaceNeighbor));
 *
 *     MyFaceNbrClientData_s *MyClientData = (MyFaceNbrClientData_s *)TecUtilDataFaceNbrGetClientData(FaceNeighbor);
 *
 *     // cleanup privately allocated resources
 *     free(MyClientData->DataFileName);
 *     free(MyClientData);
 *   }
 *
 *   .
 *   .
 *   .
 *   MyFaceNbrClientData_s *MyClientData = (MyFaceNbrClientData_s *)malloc(sizeof(MyFaceNbrClientData_s));
 *   const char *MyDataFileName = "MyDataFileName.dat";
 *   MyClientData->MyDataFileName = (char *)malloc(strlen(MyDataFileName)+1);
 *   strcpy(MyClientData->MyDataFileName, MyDataFileName);
 *   MyClientData->SeekOffset = ... determined somewhere else
 *   ...initialize any other client data information needed to load face neighbor data
 *   IsOk = TecUtilDataFaceNbrCustomLOD(3,
 *                                      TRUE,            // AutoAssignFN
 *                                      MyFaceNbrLoader,
 *                                      MyFaceNbrUnload, // most add-ons should pass NULL instead of MyFaceNbrUnload
 *                                      MyFaceNbrCleanup,
 *                                      (ArbParam_t)MyClientData);
 * @endcode
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilDataConnectShare(),
 *     TecUtilDataFaceNbrAssignArrayByRef(),
 *     TecUtilDataFaceNbrAssignByRef()
 *
 * @ingroup DataServices
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataFaceNbrCustomLOD(EntIndex_t                         Zone,
                                                          Boolean_t                          AutoAssignFN,
                                                          LoadOnDemandFaceNeighborLoad_pf    LoadCallback,
                                                          LoadOnDemandFaceNeighborUnload_pf  UnloadCallback,
                                                          LoadOnDemandFaceNeighborCleanup_pf CleanupCallback,
                                                          ArbParam_t                         ClientData);

/**
 * Get the obscuration of the specified Element and Face.
 * To use this function you must have already obtained a handle to
 * face neighbors.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.1
 *
 * @param FaceNeighbor
 *   Handle to the face neighbors. Use TecUtilDataFaceNbrGetReadableRef() to get
 *   handles to the face neighbors.
 *
 * @param Element
 *   The element number (starts at one).
 *
 * @param Face
 *   The face number of the element. Different element types have different
 *   number of faces.  Use TecUtilZoneGetType() to get the element type for a
 *   particular zone.  Face numbers start at one.
 *   ZoneType_FETriangle: has three faces.   ZoneType_FEQuad has four faces.
 *   ZoneType_FETetra has four faces.  ZoneType_FEBrick has six faces.
 *   Please look for "Face Neighbors" in the Tecplot User's Manual
 *   for a description of how nodes and faces map to each other.
 *
 * @param ActiveZones
 *   The set of zones to consider if global face neighbors are present.
 *   If a face is obscured by elements of a zone that are not in this set,
 *   the face will be considered unobscured and this function will return FALSE.
 *
 * @return
 *    TRUE indicates that the face is fully obscured (covered by neighboring elements in
 *    the current zone or by global face neighbors in ActiveZones).
 *    FALSE indicates that the face is at least partially visible or uncovered.
 *
 * <FortranSyntax>
 *    INTEGER*4 TecUtilDataFaceNbrFaceIsObscured(
 *   &           FaceNeighborPtr,
 *   &           Element,
 *   &           Face,
 *   &           ActiveZones)
 *    POINTER         (FaceNeighborPtr, FaceNeighbor)
 *    INTEGER*4       Element
 *    INTEGER*4       Face
 *    POINTER         (ActiveZonePtr, ActiveZones)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the obscuration of face 5 of element 23 in zone 2. It is assumed that zone 2
 *   is of type ZoneType_FEBrick.
 *
 * @code
 *   FaceNeighbor_pa FNPtr = TecUtilDataFaceNbrGetReadableRef(2);
 *   Set_pa ZoneSet = NULL;
 *   if (FNPtr != NULL && TecUtilZoneGetActive(&ZoneSet))
 *     {
 *       Boolean_t isObscured = TecUtilDataFaceNbrFaceIsObscured(FNPtr, 23, 5);
 *
 *       // Do something with isObscured.
 *
         TecUtilSetDealloc(&ZoneSet);
 *     }
 * @endcode
 *
 * @ingroup FaceNeighbors
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataFaceNbrFaceIsObscured(
    FaceNeighbor_pa FaceNeighbor,
    LgIndex_t Element,
    LgIndex_t Face,
    Set_pa ActiveZones);

/**
 * Get a readable face neighbor handle to the specified zone in the data set
 * attached to the current frame.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   Number of the zone for which to get the readable face neighbor handle.
 *
 * @return
 *   The face neighbor handle to the specified zone in the data set attached to
 *   the current frame or NULL if it was not able to load it.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataFaceNbrGetReadableRef(
 *   &           Zone,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataServices
 */
LINKTOADDON FaceNeighbor_pa STDCALL TecUtilDataFaceNbrGetReadableRef(EntIndex_t Zone);

/**
 * Copies the specified number of local one-to-one cell face neighbors from the
 * base of the neighbor element array to the currently open face neighbor
 * assignment sequence starting at the specified destination offset. The face
 * neighbor element array should be organized as follows:
 *      E1F1 E1F2 ... E1Fm E2F1 E2F2 ... EnFm
 * where n is the number of elements and m is the number of faces per element.
 * In the above layout E1F1 is the neighboring element of element 1 face 1.
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   If TecUtilDataFaceNbrAssignArrayByRef() is used in conjunction with
 *   TecUtilDataFaceNbrAssignByRef() then the local one-to-one face neighbors
 *   must be delivered via TecUtilDataFaceNbrAssignArrayByRef() before
 *   delivering discrete boundary connection face neighbors via
 *   TecUtilDataFaceNbrAssignByRef().
 *
 * @since
 *   11.3-0-020
 *
 * @param FaceNeighbor
 *     Face neighbor handle that was passed to a custom face neighbor load
 *     callback.
 * @param DestOffset
 *     Offset in Tecplot's face neighbor array to begin assigning the supplied
 *     neighbor elements.
 * @param NumNeighbors
 *     Number of neighbor elements to assign to Tecplot's face neighbor array.
 * @param NeighborElems
 *     An array containing the one based cell face neighbor elements to copy.
 *     The first element is assumed to be at the base of the array.
 *
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>FaceNeighbor</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataServices
 */
LINKTOADDON void STDCALL TecUtilDataFaceNbrAssignArrayByRef(FaceNeighbor_pa  FaceNeighbor,
                                                            LgIndex_t        DestOffset,
                                                            LgIndex_t        NumNeighbors,
                                                            const LgIndex_t *NeighborElems);

/**
 * Sets the boundary connection face neighbors within an open face neighbor
 * assignment sequence for the specified element and face.
 * This function is \ref threadsafe.
 *
 * @param FaceNeighbor
 *   Face neighbor handle that was passed to a custom face neighbor load
 *   callback.
 * @param Element
 *   The one based element number (starts at one).
 * @param Face
 *   The one based face for which the face neighbor information is desired.
 * @param NbrsCompObscure
 *   Set to TRUE if the supplied neighbors completely obscure the face.
 * @param NumNeighbors
 *   Number of neighbors for this face.
 * @param NeighborElems
 *   Array containing the one based element numbers of the neighbors.
 * @param NeighborZones
 *   Array containing the one based zone numbers of the neighbors for global
 *   neighbors or NULL for local neighbors.
 *
 * @return
 *   TRUE if successful in assigning to the open face neighbor assignment
 *   sequence, FALSE otherwise.
 *
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>FaceNeighbor</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>NeighborElems</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataFaceNbrAssignByRef(
 *   &                   FaceNeighborPtr,
 *   &                   Element,
 *   &                   Face,
 *   &                   NbrsCompObscure,
 *   &                   NumNeighbors,
 *   &                   NeighborElems,
 *   &                   NeighborZones)
 *    POINTER         (FaceNeighborPtr, FaceNeighbor)
 *    INTEGER*4       Element
 *    INTEGER*4       Face
 *    INTEGER*4       NbrsCompObscure
 *    INTEGER*4       NumNeighbors
 *    INTEGER*4       NeighborElems
 *    INTEGER*4       NeighborZones
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataServices
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataFaceNbrAssignByRef(FaceNeighbor_pa   FaceNeighbor,
                                                            LgIndex_t         Element,
                                                            LgIndex_t         Face,
                                                            Boolean_t         NbrsCompObscure,
                                                            LgIndex_t         NumNeighbors,
                                                            const LgIndex_t  *NeighborElems,
                                                            const EntIndex_t *NeighborZones);

/**
 * @deprecated
 *   Please use TecUtilLineMapGetCount() instead.
 *
 * @ingroup LineMap
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON EntIndex_t STDCALL TecUtilXYMapGetCount(void);



/**
 *   Returns the number of Line-maps.
 *
 * @return
 *   The number of Line-maps.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapGetCount()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilLineMapGetCountForFrame
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilLineMapGetCount(void);

/**
 *   Returns the number of Line-maps for a given frame.
 *
 * @param FrameID
 *   Unique ID of a frame for which field map count should be returned.
 * @return
 *   The number of Line-maps.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapGetCountForFrame(FrameID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since 14.1
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilLineMapGetCountForFrame(UniqueID_t FrameID);

/**
 *   Returns the number of Field-maps.
 *
 * @return
 *   The number of Field-maps.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapGetCount()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilFieldMapGetCountForFrame
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilFieldMapGetCount(void);

/**
 *   Returns the number of Field-maps for a given frame.
 *
 * @param FrameID
 *   Unique ID of a frame for which field map count should be returned.
 * @return
 *   The number of Field-maps.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapGetCountForFrame(FrameID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since 14.1
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilFieldMapGetCountForFrame(UniqueID_t FrameID);

/**
 * Obtain the set of active Field-maps.
 *
 * @since
 *   11.2-0-131
 *
 * @param ActiveFieldMaps
 *   Receives the set of active Field-maps. You must call TecUtilSetDealloc()
 *   when you are through using the set. It must not be NULL.
 *
 * @return
 *   TRUE if successful.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>ActiveFieldMaps</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapGetActive(ActiveFieldMapsPtr)
 *    POINTER (ActiveFieldMapsPtr, ActiveFieldMaps)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the set of active Line-maps:
 *
 * @code
 *   Set_pa s = NULL;
 *   if (TecUtiFieldMapGetActive(&s))
 *   {
 *    // maps are now in s
 *    TecUtilSetDealloc(&s);
 *   }
 * @endcode
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFieldMapGetActive(TP_GIVES Set_pa* ActiveFieldMaps);

/**
 * Determine if a fieldmap is active in a frame.
 *
 * @param FrameID
 *   An ID of a frame for which the query is made.
 * @param FieldMap number of the field map.
 *
 * @return
 *   Returns TRUE if the Line-map is active, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapIsActiveForFrame(FrameID, FieldMap)
 *    INTEGER*4 FrameID
 *    INTEGER*4 FieldMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since 14.1
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFieldMapIsActiveForFrame(UniqueID_t FrameID, EntIndex_t FieldMap);

/**
 * Determine if an Field-map is active.
 *
 * @since
 *   11.2-0-131
 *
 * @param FieldMap number of the field map.
 *
 * @return
 *   Returns TRUE if the Line-map is active, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapIsActive(FieldMap)
 *    INTEGER*4 FieldMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilFieldMapIsActiveForFrame
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFieldMapIsActive(EntIndex_t FieldMap);

/**
 *   Returns the mode of a fieldmap in a frame.
 *
 * @param FrameID
 *   An ID of a frame for which the query is made.
 * @param FieldMap
 *   Number of the field map.
 * @return
 *   The mode for a fieldmap.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapGetModeForFrame(FrameID, FieldMap)
 *    INTEGER*4 FrameID
 *    INTEGER*4 FieldMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since 14.1
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON FieldMapMode_e STDCALL TecUtilFieldMapGetModeForFrame(UniqueID_t FrameID, EntIndex_t FieldMap);

/**
 *   Returns the mode of a fieldmap.
 *
 * @param FieldMap
 *   Number of the field map.
 * @return
 *   The mode for a fieldmap.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapGetMode(FieldMap)
 *    INTEGER*4 FieldMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilFieldMapGetModeForFrame
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON FieldMapMode_e STDCALL TecUtilFieldMapGetMode(EntIndex_t FieldMap);

/**
 * Get the set of zones for the fieldmap.
 *
 * @since
 *     12.1.1.8018
 *
 * @param FieldMap
 *   Indicates which field map to query.
 *
 * @param Zones
 *   Receives the set of zones. You must free this pointer by calling
 *   TecUtilSetDealloc().
 *
 * @return
 *   TRUE if successful, FALSE otherwise
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>Zones</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapGetZones(FieldMap, ZonesPtr)
 *    INTEGER*4 FieldMap
 *    POINTER (ZonesPtr, Zones)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the set of zones in field map 2:
 *
 * @code
 *   Set_pa set = NULL;
 *   if (TecUtilFieldMapGetZones(2, &set))
 *   {
 *    // do something with the set here
 *    TecUtilSetDealloc(&set);
 *   }
 * @endcode
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFieldMapGetZones(EntIndex_t       FieldMap,
                                                      TP_GIVES Set_pa* Zones);

/**
 * Returns the number of the candidate zone for the fieldmap in the given frame.
 *
 * @param FrameID
 *   Unique ID of a frame for which field map count should be returned.
 * @param FieldMap
 *   Number of the field map.
 * @return
 *   Zone number.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapGetCandidateZoneForFrame(FrameID, FieldMap)
 *    INTEGER*4 FrameID
 *    INTEGER*4 FieldMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since 14.1
 * @ingroup FieldMap
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilFieldMapGetCandidateZoneForFrame(UniqueID_t FrameID, EntIndex_t FieldMap);

/**
 *   Returns the number of the candidate zone for the fieldmap.
 *
 * @param FieldMap
 *   Number of the field map.
 * @return
 *   Zone number.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapGetCandidateZone(FieldMap)
 *    INTEGER*4 FieldMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilFieldMapGetCandidateZoneForFrame
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilFieldMapGetCandidateZone(EntIndex_t FieldMap);

/**
 * Determines if the fieldmap is relevant for the current time step.
 *
 * @param FieldMap
 *   Number of the field map.
 * @return
 *   TRUE if the field map is relevant. FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapIsRelevant(FieldMap)
 *    INTEGER*4 FieldMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFieldMapIsRelevant(EntIndex_t FieldMap);

/**
 * Determines if the fieldmap contains any ordered zones in the specified frame.
 *
 * 
 * @param FrameID
 *   Unique ID of a frame for which the query should be made.
 * @param FieldMap 
 *   Number of the field map.
 * @return
 *   TRUE if the field map contains a zone that is ordered. FALSE otherwise.
 *
 *
 * @pre <em>VALID_DATASET(frame->DataSet,true)</em>
 *   Data set must have at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapHasOrderedZonesForFrame(FrameID, FieldMap)
 *    INTEGER*4 FrameID
 *    INTEGER*4 FieldMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since 14.1
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFieldMapHasOrderedZonesForFrame(UniqueID_t FrameID, 
                                                                     EntIndex_t FieldMap);


/**
 * Determines if the fieldmap contains any ordered zones.
 *
 * @since
 *   11.2-0-161
 *
 * @return
 *   TRUE if the field map contains a zone that is ordered. FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapHasOrderedZones(FieldMap)
 *    INTEGER*4 FieldMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilFieldMapHasOrderedZonesForFrame
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFieldMapHasOrderedZones(EntIndex_t FieldMap);

/**
 * Determines if the fieldmap contains any IJK ordered zones in the specified frame.
 *
 * @param FrameID
 *   Unique ID of a frame for which the query should be made.
 * @param FieldMap
 *   Field map for which the query should be made.
 * @return
 *   TRUE if the field map contains a zone that is IJK ordered. FALSE otherwise.
 *
 *
 * @pre <em>VALID_DATASET(frame->DataSet,true)</em>
 *   Data set must have at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapHasIJKOrderedZonesForFrame(FrameID, FieldMap)
 *    INTEGER*4 FrameID
 *    INTEGER*4 FieldMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since 14.1
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFieldMapHasIJKOrderedZonesForFrame(UniqueID_t FrameID,
                                                                        EntIndex_t FieldMap);

/**
 * Determines if the fieldmap contains any IJK ordered zones.
 *
 * @since
 *   11.2-0-161
 *
 * @return
 *   TRUE if the field map contains a zone that is IJK ordered. FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapHasIJKOrderedZones(FieldMap)
 *    INTEGER*4 FieldMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilFieldMapHasIJKOrderedZonesForFrame
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFieldMapHasIJKOrderedZones(EntIndex_t FieldMap);

/**
 * Determines if the fieldmap contains any FE zones.
 *
 * @since
 *   11.2-0-161
 *
 * @return
 *   TRUE if the field map contains an FE zone. FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapHasFEZones(FieldMap)
 *    INTEGER*4 FieldMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFieldMapHasFEZones(EntIndex_t FieldMap);

/**
 * Determines if the fieldmap contains any Volume zones.
 *
 * @since
 *   11.2-0-161
 *
 * @return
 *   TRUE if the field map contains a volume zone. FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapHasVolumeZones(FieldMap)
 *    INTEGER*4 FieldMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilFieldMapHasVolumeZonesForFrame
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFieldMapHasVolumeZones(EntIndex_t FieldMap);

/**
 * Determines if the fieldmap contains any volume zones for the dataset attached to the specified frame.
 *
 * @since 14.1 
 *
 * @param FrameID
 *   An ID of the frame that is attached to the dataset for which the query is made.
 * @param FieldMap number of the field map.
 * 
 * @return
 *   TRUE if the field map for the dataset attached to the specified frame contains a volume zone.
 *   FALSE otherwise.
 *
 *
 * @pre <em>VALID_DATASET(frame->DataSet,true)</em>
 *   Data set must have at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapHasVolumeZonesForFrame(FrameID, FieldMap)
 *    INTEGER*4 FrameID
 *    INTEGER*4 FieldMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFieldMapHasVolumeZonesForFrame(UniqueID_t FrameID, 
                                                                    EntIndex_t FieldMap);

/**
 * Determines if the fieldmap contains any Surface zones.
 *
 * @since
 *   11.2-0-161
 *
 * @return
 *   TRUE if the field map contains a surface zone. FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapHasSurfaceZones(FieldMap)
 *    INTEGER*4 FieldMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFieldMapHasSurfaceZones(EntIndex_t FieldMap);

/**
 * Determines if the fieldmap contains any Linear zones.
 *
 * @since
 *   11.2-0-161
 *
 * @return
 *   TRUE if the field map contains a linear zone. FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldMapHasLinearZones(FieldMap)
 *    INTEGER*4 FieldMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup FieldMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilFieldMapHasLinearZones(EntIndex_t FieldMap);

/**
 *   Determine if Tecplot is currently recording a user macro.
 *
 * @return
 *   TRUE if Tecplot is recording user macros, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilUserMacroIsRecordingActive()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   If Tecplot is currently recording a user macro, record the macro command "GO"
 *   for the addon "BANANA":
 *
 * @code
 *   if (TecUtilUserMacroIsRecordingActive())
 *     {
 *       TecUtilMacroRecordExtCommand("BANANA", "GO");
 *     }
 * @endcode
 *
 * @ingroup ScriptSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilUserMacroIsRecordingActive(void);

/**
 *   Determine if Tecplot is currently recording a macro.
 *
 * @return
 *   TRUE if Tecplot is recording macros, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroIsRecordingActive()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   If Tecplot is currently recording a macro, record the macro command "GO"
 *   for the addon "BANANA":
 *
 * @code
 *   if (TecUtilMacroIsRecordingActive())
 *     {
 *       TecUtilMacroRecordExtCommand("BANANA", "GO");
 *     }
 * @endcode
 *
 * @ingroup ScriptSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroIsRecordingActive(void);

/**
 * Convenience function to query Tecplot for a limit value. These are the same
 * integer values you can set using the $!LIMITS command in the Tecplot macro
 * language.
 *
 * @since
 *   7.5-0-6
 *
 * @param LimitString
 *   This must be one of the following: MaxPtsInALine, MaxChrsInTextLabels,
 *   MaxNumContourLevels, IPCTimeoutSeconds, MaxAvailableProcessors,
 *   MaxUsableMemory, PreloadDataTimeThresholdInMS.
 *
 * @return
 *   Returns the limit value as an integer.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLimitGetValue(LimitString)
 *    CHARACTER*(*) LimitString
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the number of points allowed per polyline in Tecplot:
 *
 * @code
 *   LgIndex_t MaxPtsPerLine;
 *   MaxPtsPerLine = TecUtilLimitGetValue("MaxPtsInALine");
 * @endcode
 *
 * @sa TecUtilStyleGetLowLevelX
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilLimitGetValue(const char *LimitString);

/**
 * Determine if any frame in any page has an attached data set with the specified unique ID.
 * This function is \ref threadsafe.
 *
 * @return
 *   TRUE if any frame in any page has an attached data set with the specified unique ID, FALSE if not.
 *
 * @sa TecUtilDataSetIsAvailable
 *
 * @since 13.4.0
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetIsAvailableByUniqueID(UniqueID)
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataSetInfo
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetIsAvailableByUniqueID(UniqueID_t UniqueID);

/**
 * Determine if the specified frame has a data set attached.
 * This function is \ref threadsafe.
 *
 * @param FrameID
 *   Unique ID of a frame for which the query should be made.
 * @return
 *   TRUE if the current frame has an attached data set, FALSE if not.
 *
 * @sa TecUtilDataSetIsAvailableByUniqueID
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetIsAvailableForFrame(FrameID)
 *    INTEGER*4 FrameID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the number of zones for the data set for the frame with ID=1, or use
 *   zero if there is no data set:
 *
 * @code
 *   EntIndex_t nzones = 0;
 *   if ( TecUtilDataSetIsAvailableForFrame(1) )
 *     TecUtilDataSetGetInfoForFrame(1, NULL, &nzones, NULL);
 * @endcode
 *
 * @since 14.1
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetIsAvailableForFrame(UniqueID_t FrameID);

/**
 * Determine if the current frame has a data set attached.
 * This function is \ref threadsafe.
 *
 * @return
 *   TRUE if the current frame has an attached data set, FALSE if not.
 *
 * @sa TecUtilDataSetIsAvailableByUniqueID
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetIsAvailable()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the number of zones for the data set for the current frame, or use
 *   zero if there is no data set:
 *
 * @code
 *   EntIndex_t nzones = 0;
 *   if ( TecUtilDataSetIsAvailable() )
 *     TecUtilDataSetGetInfo(NULL, &nzones, NULL);
 * @endcode
 *
 * @sa TecUtilDataSetIsAvailableForFrame
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetIsAvailable(void);

/**
 *   Determine if a variable is enabled in the dataset attached to the specified frame.
 *   This function is \ref threadsafe.
 *
 * @param FrameID
 *   An ID of the frame that is attached to the dataset for which the query is made.
 * @param Var
 *   Variable whose status is queried.
 *
 * @return
 *   TRUE, if a variable is enabled, otherwise, FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarIsEnabledForFrame(FrameID, Var)
 *    INTEGER*4 FrameID
 *    INTEGER*4 Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Check if the first variable is enabled for the frame with ID=1:
 *
 * @code
 *   if (TecUtilVarIsEnabledForFrame(1, 1))
 *     {
 *       // sure is!
 *     }
 * @endcode
 *
 * @since 14.1
 *
 * @ingroup Variables
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilVarIsEnabledForFrame(UniqueID_t FrameID, EntIndex_t Var);

/**
 *   Determine if a variable is enabled.
 *   This function is \ref threadsafe.
 *
 * @return
 *   TRUE, if a variable is enabled, otherwise, FALSE.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarIsEnabled(Var)
 *    INTEGER*4 Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Check if the first variable is enabled:
 *
 * @code
 *   if (TecUtilVarIsEnabled(1))
 *     {
 *       // sure is!
 *     }
 * @endcode
 *
 * @sa TecUtilVarIsEnabledForFrame
 *
 * @ingroup Variables
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilVarIsEnabled(EntIndex_t Var);

/**
 *   Determine if a zone is enabled.
 *   This function is \ref threadsafe.
 *
 * @param Zone
 *   Number of the zone for which to get the zone type information
 *
 * @return
 *   TRUE, if a zone is enabled, otherwise, FALSE.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneIsEnabled(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Check if the first zone is enabled:
 *
 * @code
 *   if (TecUtilZoneIsEnabled(1))
 *   {
 *     // sure is!
 *   }
 * @endcode
 *
 * @ingroup Zone
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneIsEnabled(EntIndex_t Zone);

/**
 *   Determine if a zone is active.
 *   This function is \ref threadsafe.
 *
 * @return
 *   Returns TRUE if the zone is active, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre The active frame's plot plot type must be 2D or 3D.
 *
 * @pre <em>ZoneIsValid(dataSet, Zone)</em>
 *   Must specify a valid zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneIsActive(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Zone
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneIsActive(EntIndex_t Zone);

/**
 * @deprecated
 *   Please use TecUtilLineMapIsActive() instead.
 *
 * @ingroup LineMap
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilXYMapIsActive(EntIndex_t XYMap);

/**
 *   Determine if an Line-map is active for specified frame.
 *   This function is \ref threadsafe.
 *
 * @param FrameID
 *   Unique ID of a frame which should be queried.
 * @param LineMap
 *   Line map number for which the query is made.
 * @return
 *   Returns TRUE if the Line-map is active, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapIsActiveForFrame(FrameID, LineMap)
 *    INTEGER*4 FrameID
 *    INTEGER*4 LineMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 * 
 * @since 14.1
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapIsActiveForFrame(UniqueID_t FrameID, EntIndex_t LineMap);

/**
 *   Determine if an Line-map is active.
 *   This function is \ref threadsafe.
 *
 * @return
 *   Returns TRUE if the Line-map is active, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapIsActive(LineMap)
 *    INTEGER*4 LineMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilLineMapIsActiveForFrame
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapIsActive(EntIndex_t LineMap);

/**
 * @deprecated
 *   Please use TecUtilFileGetTempName() instead.
 *
 * @ingroup Utilities
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilGetTempFileName(TP_GIVES char** TempFileName);



/**
 * Creates a temporary file name, including a full path. An empty file with the
 * temporary file name is also created.
 * This function is \ref threadsafe.
 *
 * @param TempFileName
 *   Receives the temporary file name, including path. You must later free this
 *   string with TecUtilStringDealloc()
 *
 * @return
 *   TRUE if successful, FALSE otherwise. If the return value is FALSE,
 *   *FileName is set to NULL.
 *
 *
 * @pre <em>TempFileName</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFileGetTempName(
 *   &                   TempFileName,
 *   &                   TempFileNameLength)
 *    CHARACTER*(*)   TempFileName
 *    INTEGER*4       TempFileNameLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a temporary file.
 *
 * @code
 *
 *   {
 *     FILE *TempFile = NULL;
 *     char *TempFileName = NULL;
 *
 *     if ( TecUtilFileGetTempName(&TempFileName) )
 *       {
 *         TempFile = fopen(TempFileName,"w");
 *         if ( TempFile )
 *           {
 *             // Do something with the temp file.
 *             fclose(TempFile);
 *           }
 *
 *         // Be sure to deallocate the string when finished.
 *         TecUtilStringDealloc(&TempFileName);
 *       }
 *   }
 * @endcode
 *
 * @ingroup Utilities
 */
LINKTOADDON Boolean_t STDCALL TecUtilFileGetTempName(TP_GIVES char** TempFileName);

/**
 * Creates a temporary directory name, including a full path. An empty directory with the
 * temporary directory name is also created.
 * This function is \ref threadsafe.
 *
 * @param TempDirName
 *   Receives the temporary directory name, including path. You must later free this
 *   string with TecUtilStringDealloc()
 *
 * @return
 *   TRUE if successful, FALSE otherwise. If the return value is FALSE,
 *   *TempDirName is set to NULL.
 *
 *
 * @pre <em>TempDirName</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFileGetTempDirName(
 *   &                   TempDirName,
 *   &                   TempDirNameLength)
 *    CHARACTER*(*)   TempDirName
 *    INTEGER*4       TempDirNameLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 */
LINKTOADDON Boolean_t STDCALL TecUtilFileGetTempDirName(TP_GIVES char** TempDirName);

/**
 *   Get the RGB components of a basic color.
 *
 * @param BasicColor
 *   Index of basic color to query.   It is best to use the supplied
 *   constants in GLOBAL.h (e.g. Black_C, Red_C, Custom9_C).
 *
 * @param Red
 *   Receives red component of the color. This parameter may be NULL.
 *
 * @param Green
 *   Receives green component of the color. This parameter may be NULL.
 *
 * @param Blue
 *   Receives blue component of the color. This parameter may be NULL
 *
 *
 * @pre <em>Red</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>Green</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>Blue</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilColorMapGetBasicColorRGB(
 *   &           BasicColor,
 *   &           Red,
 *   &           Green,
 *   &           Blue)
 *    INTEGER*4       BasicColor
 *    INTEGER*4       Red
 *    INTEGER*4       Green
 *    INTEGER*4       Blue
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the RGB components of the Custom2_C basic color.
 *
 * @code
 *   {
 *     Byte R,G,B;
 *     TecUtilColorMapGetBasicColorRGB(Custom2_C,&R,&G,&B);
 *   }
 * @endcode
 *
 * @ingroup ColorMap
 *
 */
LINKTOADDON void STDCALL TecUtilColorMapGetBasicColorRGB(ColorIndex_t         BasicColor,
                                                         TP_OUT ColorIndex_t* Red,
                                                         TP_OUT ColorIndex_t* Green,
                                                         TP_OUT ColorIndex_t* Blue);

/**
 *   Get the number of basic colors in Tecplot.
 *
 * @return
 *   The number of basic colors in Tecplot.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilColorMapNumBasicColors()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the number of basic colors in Tecplot.
 *
 * @code
 *   {
 *     LgIndex_t NumBasicColors;
 *
 *     TecUtilLockStart(AddOnID);
 *     NumBasicColors = TecUtilColorMapNumBasicColors();
 *
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup ColorMap
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilColorMapNumBasicColors(void);

/**
 *   Queries the auto redraw state.
 *
 * @return
 *   Returns TRUE if Auto Redraw is active, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAutoRedrawIsActive()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Query the auto redraw state.
 *
 * @code
 *   {
 *   Boolean_t AutoRedrawIsActive;
 *   TecUtilLockStart(AddOnID);
 *   AutoRedrawIsActive = TecUtilAutoRedrawIsActive();
 *   TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup Drawing
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAutoRedrawIsActive(void);


/**
 * Deallocate AuxData. This method is only used in conjunction with TecUtilReadBinaryData. All other
 * AuxData functions return internal references which must NOT be deallocated.
 *
 * @since
 *   13.0-0-13980
 *
 * @param AuxData
 *   The AuxData item to be deallocated.
 *
 *
 * @pre <em>AuxData</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>AuxData</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * @sa TecUtilReadBinaryData
 *
 * @ingroup AuxData
 *
 * #internalattributes exclude_alldoc
 */
LINKTOADDON void STDCALL TecUtilAuxDataDealloc(AuxData_pa* AuxData);


/**
 * Gets a reference to the current data set's auxiliary data.
 *
 * @return
 *   Reference to the current data set's auxiliary data.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAuxDataDataSetGetRef(ResultPtr)
 *    POINTER (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup AuxData
 *
 */
LINKTOADDON AuxData_pa STDCALL TecUtilAuxDataDataSetGetRef(void);

/**
 * Gets a reference to the specified line map's auxiliary data.
 * This function is \ref threadsafe.
 *
 * @since
 *   10.0-3-129
 *
 * @param Map
 *   Line map number for which the auxiliary data is desired.
 *
 * @return
 *   Reference to the specified line map's auxiliary data.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAuxDataLineMapGetRef(
 *   &           Map,
 *   &           ResultPtr)
 *    INTEGER*4       Map
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup AuxData
 *
 */
LINKTOADDON AuxData_pa STDCALL TecUtilAuxDataLineMapGetRef(EntIndex_t Map);

/**
 * Gets a reference to the current page's auxiliary data.
 * This function is \ref threadsafe.
 *
 * @return
 *   Reference to the current page's auxiliary data.
 *
 * @pre Must have one or more pages.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAuxDataPageGetRef(ResultPtr)
 *    POINTER (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup AuxData
 *
 */
LINKTOADDON AuxData_pa STDCALL TecUtilAuxDataPageGetRef(void);

/**
 * Gets a reference to the current frame's auxiliary data.
 * This function is \ref threadsafe.
 *
 * @return
 *   Reference to the current frame's auxiliary data.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAuxDataFrameGetRef(ResultPtr)
 *    POINTER (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup AuxData
 *
 */
LINKTOADDON AuxData_pa STDCALL TecUtilAuxDataFrameGetRef(void);

/**
 * Gets a reference to the specified zone's auxiliary data.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   Zone number for which the auxiliary data is desired.
 *
 * @return
 *   Reference to the specified zone's auxiliary data.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAuxDataZoneGetRef(
 *   &           Zone,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup AuxData
 *
 */
LINKTOADDON AuxData_pa STDCALL TecUtilAuxDataZoneGetRef(EntIndex_t Zone);

/**
 * Gets a reference to the specified variable's auxiliary data.
 * This function is \ref threadsafe.
 *
 * @since
 *   10.0-3-129
 *
 * @param Var
 *   Variable number for which the auxiliary data is desired.
 *
 * @return
 *   Reference to the specified variable's auxiliary data.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAuxDataVarGetRef(
 *   &           Var,
 *   &           ResultPtr)
 *    INTEGER*4       Var
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup AuxData
 *
 */
LINKTOADDON AuxData_pa STDCALL TecUtilAuxDataVarGetRef(EntIndex_t Var);

/**
 * Gets the current number of auxiliary data items maintained by the auxiliary
 * data reference.
 * This function is \ref threadsafe.
 *
 * @param AuxDataRef
 *   Reference to auxiliary data.
 *
 * @return
 *   Number of items maintained by the auxiliary data.
 *
 *
 * @pre <em>AuxDataRef</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAuxDataGetNumItems(AuxDataRefPtr)
 *    POINTER (AuxDataRefPtr, AuxDataRef)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Find the number of auxiliary data items linked to the frame:
 *
 * @code
 *   AuxData_pa AuxDataRef = TecUtilAuxDataFrameGetRef();
 *   if (AuxDataRef != NULL)
 *     {
 *       LgIndex_t NumItems = TecUtilAuxDataGetNumItems(AuxDataRef);
 *       if (NumItems != 0)
 *         {
 *           // ... do something with the 1..NumItems items ...
 *         }
 *     }
 *   else
 *     {
 *       // ... allocation failure ...
 *     }
 * @endcode
 *
 * @ingroup AuxData
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilAuxDataGetNumItems(AuxData_pa AuxDataRef);

/**
 * Gets the index of the named auxiliary data item if found or if not found the
 * index where an auxiliary data item could be inserted in sorted order.
 * This function is \ref threadsafe.
 *
 * @param AuxDataRef
 *   Reference to auxiliary data.
 *
 * @param Name
 *   Name used for search. A valid auxiliary data name character must begin
 *   with a '_' or alpha character and may be followed by one or more '_', '.',
 *   alpha or digit characters.
 *
 * @param ItemIndex
 *   Address to hold the index of the found item or the index where an
 *   auxiliary data item could be inserted.
 *
 * @return
 *   TRUE if the named item was found, FALSE otherwise.
 *
 *
 * @pre <em>AuxDataRef</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>ItemIndex</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAuxDataGetItemIndex(
 *   &                   AuxDataRefPtr,
 *   &                   Name,
 *   &                   ItemIndex)
 *    POINTER         (AuxDataRefPtr, AuxDataRef)
 *    CHARACTER*(*)   Name
 *    INTEGER*4       ItemIndex
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * If it exists, get the item index of the frame's auxiliary data item named
 * "MachNumber":
 *
 * @code
 *   // If it exists, get the item index of the frame's
 *   // auxiliary data item named "MachNumber".
 *   AuxData_pa AuxDataRef = TecUtilAuxDataFrameGetRef();
 *   if (AuxDataRef != NULL)
 *     {
 *       LgIndex_t ItemIndex;
 *       if (TecUtilAuxDataGetItemIndex(AuxDataRef,
 *                                      "MachNumber",
 *                                      &ItemIndex))
 *         {
 *           // ...do something with the item index ...
 *         }
 *       else
 *         {
 *           // ...item not found ...
 *         }
 *     }
 *   else
 *     {
 *       // ...allocation failure ...
 *     }
 * @endcode
 *
 * @ingroup AuxData
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAuxDataGetItemIndex(AuxData_pa        AuxDataRef,
                                                         const char*       Name,
                                                         TP_OUT LgIndex_t* ItemIndex);

/**
 * Gets the auxiliary string data item at the specified index. The resulting
 * name and value are allocated copies and therefore it is the client's
 * responsibility to release them with TecUtilStringDealloc() when no longer
 * needed.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.0-0-007
 *
 * @sa TecUtilStringDealloc()
 *
 * @param AuxDataRef
 *   Reference to the auxiliary data.
 *
 * @param Index
 *   Index of the auxiliary data of interest.
 *
 * @param Name
 *   Address to hold the auxiliary data item name string. It is the client's
 *   responsibility release it with TecUtilStringDealloc() when no longer
 *   needed.
 *
 * @param Value
 *   Address to hold the auxiliary data item value string. It is the client's
 *   responsibility release it with TecUtilStringDealloc() when no longer
 *   needed.
 *
 * @param Retain
 *   Address to hold the auxiliary data item retain flag.
 *
 *
 * @pre <em>AuxDataRef</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Name</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Value</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Retain</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAuxDataGetStrItemByIndex(
 *   &           AuxDataRefPtr,
 *   &           Index,
 *   &           Name,
 *   &           NameLength,
 *   &           ValuePtr,
 *   &           Retain)
 *    POINTER         (AuxDataRefPtr, AuxDataRef)
 *    INTEGER*4       Index
 *    CHARACTER*(*)   Name
 *    INTEGER*4       NameLength
 *    CHARACTER*(*)   Value
 *    INTEGER*4       ValueLength
 *    INTEGER*4       Retain
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Get the frame's fourth auxiliary data item:
 *
 * @code
 *   AuxData_pa AuxDataRef = TecUtilAuxDataFrameGetRef();
 *   if (AuxDataRef != NULL)
 *     {
 *       char          *Name = NULL;
 *       char          *Value = NULL;
 *       Boolean_t      Retain;
 *       TecUtilAuxDataGetStrItemByIndex(AuxDataRef,
 *                                       4,
 *                                       &Name,
 *                                       &Value,
 *                                       &Retain);
 *       if (Value != NULL)
 *         {
 *           // ... do something with the value string ...
 *           // release the allocated string copy
 *            TecUtilStringDealloc(&Name);
 *            TecUtilStringDealloc(&Value);
 *         }
 *       else
 *         {
 *           // ... handle the NULL condition ...
 *         }
 *     }
 *   else
 *     {
 *       // ... allocation failure ...
 *     }
 * @endcode
 *
 * @ingroup AuxData
 *
 */
LINKTOADDON void STDCALL TecUtilAuxDataGetStrItemByIndex(AuxData_pa        AuxDataRef,
                                                         LgIndex_t         Index,
                                                         TP_GIVES char**   Name,
                                                         TP_GIVES char**   Value,
                                                         TP_OUT Boolean_t* Retain);

/**
 * Gets the auxiliary data item at the specified index. The resulting name and
 * value are allocated copies and therefore it is the client's responsibility
 * to release them when no longer needed.
 * This function is \ref threadsafe.
 *
 * @sa TecUtilStringDealloc()
 *
 * @par Note:
 *   This function is only available in C/C++.
 *   For other languages use TecUtilAuxDataGetStrItemByIndex().
 *
 * @param AuxDataRef
 *   Reference to the auxiliary data.
 *
 * @param Index
 *   Index of the auxiliary data of interest.
 *
 * @param Name
 *   Address to hold the auxiliary data item name string. It is the client's
 *   responsibility release it with TecUtilStringDealloc() when no longer
 *   needed.
 *
 * @param Value
 *   Address to hold the auxiliary data item value. If the value is a pointer
 *   to a string, it is the client's responsibility release it with
 *   TecUtilStringDealloc() when no longer needed.
 *
 * @param Type
 *   Address to hold the auxiliary data item type.
 *
 * @param Retain
 *   Address to hold the auxiliary data item retain flag.
 *
 *
 * @pre <em>AuxDataRef</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Name</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Value</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Type</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Retain</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * Get the frame's fourth auxiliary data item:
 *
 * @code
 *   AuxData_pa AuxDataRef = TecUtilAuxDataFrameGetRef();
 *   if (AuxDataRef != NULL)
 *     {
 *       char          *Name;
 *       ArbParam_t     Value;
 *       AuxDataType_e  Type;
 *       Boolean_t      Retain;
 *       TecUtilAuxDataGetItemByIndex(AuxDataRef, 4,
 *                                    &Name,
 *                                    &Value,
 *                                    &Type,
 *                                    &Retain);
 *       if (Type == AuxDataType_String)
 *         // currently the only type supported
 *         {
 *           char *ValueString = (char *)Value;
 *           if (ValueString != NULL)
 *             {
 *              // ... do something with the value string ...
 *              // release the allocated string copy
 *               TecUtilStringDealloc(&Name);
 *               TecUtilStringDealloc(&ValueString);
 *             }
 *           else
 *             {
 *               // ... handle the NULL condition ...
 *             }
 *         }
 *       else
 *         {
 *           // value type not yet supported by this addon
 *         }
 *     }
 *   else
 *     {
 *       // ... allocation failure ...
 *     }
 * @endcode
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup AuxData
 *
 * #internalattributes exclude_fglue
 */
LINKTOADDON void STDCALL TecUtilAuxDataGetItemByIndex(AuxData_pa            AuxDataRef,
                                                      LgIndex_t             Index,
                                                      TP_GIVES char**       Name,
                                                      TP_GIVES ArbParam_t*  Value,
                                                      TP_OUT AuxDataType_e* Type,
                                                      TP_OUT Boolean_t*     Retain);

/**
 * Gets the auxiliary string data item by the specified name if it exists.
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   The resulting value is an allocated copy and therefore it is the caller's
 *   responsibility to release using TecUtilStringDealloc() when no longer
 *   needed.
 *
 * @since
 *   11.0-0-007
 *
 * @param AuxDataRef
 *   Reference to the auxiliary data.
 *
 * @param Name
 *   Auxiliary data name to fetch. A valid auxiliary data name character must
 *   begin with a '_' or alpha character and may be followed by one or more
 *   '_', '.', alpha or digit characters.
 *
 * @param Value
 *   Address to hold the auxiliary data item value string. It is the client's
 *   responsibility release it with TecUtilStringDealloc() when no longer
 *   needed.
 *
 * @param Retain
 *   Address to hold the auxiliary data item retain flag. If Retain is set to
 *   TRUE, then saving the data set will include this item.
 *
 *
 * @pre <em>AuxDataRef</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Value</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Retain</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAuxDataGetStrItemByName(
 *   &                   AuxDataRefPtr,
 *   &                   Name,
 *   &                   ValuePtr,
 *   &                   Retain)
 *    POINTER         (AuxDataRefPtr, AuxDataRef)
 *    CHARACTER*(*)   Name
 *    POINTER         (ValuePtr, Value)
 *    INTEGER*4       Type
 *    INTEGER*4       Retain
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * If it exists, get the frame's auxiliary data item named "MachNumber":
 *
 * @code
 *   // If it exists, get the frame's auxiliary
 *   // data item named "MachNumber".
 *   AuxData_pa AuxDataRef = TecUtilAuxDataFrameGetRef();
 *   if (AuxDataRef != NULL)
 *     {
 *       char     **Value;
 *       Boolean_t     Retain;
 *       if (TecUtilAuxDataGetStrItemByName(AuxDataRef, "MachNumber",
 *                                          &Value, &Retain))
 *         {
 *            if (Value != NULL)
 *              {
 *                // ... do something with the value string ...
 *                double MachNumber;
 *                if (sscanf(Value, "%lf", &MachNumber) == 1)
 *                  {
 *                  }
 *                else
 *                  {
 *                    // ... invalid value ...
 *                  }
 *                  // release the allocated string copy
 *                TecUtilStringDealloc(&Value);
 *              }
 *            else
 *              {
 *                // ... handle the NULL condition ...
 *              }
 *         }
 *       else
 *         {
 *           // ... item not found ...
 *         }
 *     }
 *   else
 *     {
 *       // ... allocation failure ...
 *     }
 * @endcode
 *
 * @ingroup AuxData
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAuxDataGetStrItemByName(AuxData_pa        AuxDataRef,
                                                             const char*       Name,
                                                             TP_GIVES char**   Value,
                                                             TP_OUT Boolean_t* Retain);
/**
 * Gets the auxiliary data item by the specified name if it exists.
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   The resulting value is an allocated copy and therefore it is the caller's
 *   responsibility to release using TecUtilStringDealloc() when no longer
 *   needed. This function is only available in C/C++. For other languages,
 *   use TecUtilAuxDataGetStrItemByName().
 *
 * @param AuxDataRef
 *   Reference to the auxiliary data.
 *
 * @param Name
 *   Auxiliary data name to fetch. A valid auxiliary data name character must
 *   begin with a '_' or alpha character and may be followed by one or more
 *   '_', '.', alpha or digit characters.
 *
 * @param Value
 *   Address to hold the auxiliary data item value. If the value is a pointer
 *   to a string, it is the client's responsibility release it with
 *   TecUtilStringDealloc() when no longer needed.
 *
 * @param Type
 *   Address to hold the auxiliary data item type.
 *
 * @param Retain
 *   Address to hold the auxiliary data item retain flag. If Retain is set to
 *   TRUE, then saving the data set will include this item.
 *
 *
 * @pre <em>AuxDataRef</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Value</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Type</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Retain</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * If it exists, get the frame's auxiliary data item named "MachNumber":
 *
 * @code
 *   // If it exists, get the frame's auxiliary
 *   // data item named "MachNumber".
 *   AuxData_pa AuxDataRef = TecUtilAuxDataFrameGetRef();
 *   if (AuxDataRef != NULL)
 *     {
 *       ArbParam_t    Value;
 *       AuxDataType_e Type;
 *       Boolean_t     Retain;
 *       if (TecUtilAuxDataGetItemByName(AuxDataRef, "MachNumber",
 *                                       &Value, &Type, &Retain))
 *         {
 *           if (Type == AuxDataType_String) // currently the only type supported
 *             {
 *               char *ValueString = (char *)Value;
 *               if (ValueString != NULL)
 *                 {
 *                   // ... do something with the value string ...
 *                   double MachNumber;
 *                   if (sscanf(ValueString, "%lf", &MachNumber) == 1)
 *                     {
 *                     }
 *                   else
 *                     {
 *                       // ... invalid value ...
 *                     }
 *
 *                   // release the allocated string copy
 *                   TecUtilStringDealloc(&ValueString);
 *                 }
 *               else
 *                 {
 *                   // ... handle the NULL condition ...
 *                 }
 *             }
 *           else
 *             {
 *               // ... value type not yet supported by this addon ...
 *             }
 *         }
 *       else
 *         {
 *           // ... item not found ...
 *         }
 *     }
 *   else
 *     {
 *       // ... allocation failure ...
 *     }
 * @endcode
 *
 * @ingroup AuxData
 *
 * #internalattributes exclude_fglue
 */
LINKTOADDON Boolean_t STDCALL TecUtilAuxDataGetItemByName(AuxData_pa            AuxDataRef,
                                                          const char*           Name,
                                                          TP_GIVES ArbParam_t*  Value,
                                                          TP_OUT AuxDataType_e* Type,
                                                          TP_OUT Boolean_t*     Retain);

/**
 * Adds the auxiliary data string item to the auxiliary data or replaces it if one
 * already exists by the same name.
 *
 * @param AuxDataRef
 *   Reference to the auxiliary data.
 *
 * @param Name
 *   Auxiliary data item's name. Be sure to consider a unique naming convention
 *   for your auxiliary data to avoid naming conflicts with auxiliary data
 *   produed by other addonaddons or macros. By convention addons that wish to
 *   share auxiliary data can prepend the name with the "Common" prefix. For
 *   example, the Plot3D loader uses CommonReferenceMachNumber for the
 *   reference Mach number. A valid auxiliary data name character must begin
 *   with a '_' or alpha character and may be followed by one or more '_', '.',
 *   alpha or digit characters.
 *
 * @param Value
 *   Value string pointer of the item to set. A copy of this string is used for
 *   the auxiliary data value.
 *
 * @param Retain
 *   Flag specifying whether or not to retain this item on file export such as
 *   writing out a datafile, etc.
 *
 * @return
 *   TRUE if the item was added to the auxiliay data, FALSE if otherwise.
 *
 *
 * @pre <em>AuxDataRef</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Value</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Retain</em>
 *   Value must be TRUE or FALSE.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAuxDataSetStrItem(
 *   &                   AuxDataRefPtr,
 *   &                   Name,
 *   &                   ValuePtr,
 *   &                   Retain)
 *    POINTER         (AuxDataRefPtr, AuxDataRef)
 *    CHARACTER*(*)   Name
 *    POINTER         (ValuePtr, Value)
 *    INTEGER*4       Retain
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Add an item named "MachNumber" to the frame's auxiliary data.
 *
 * @code
 *   AuxData_pa AuxDataRef = TecUtilAuxDataFrameGetRef();
 *   if (AuxDataRef != NULL)
 *     {
 *       const char *Value = "1.75";
 *       if (TecUtilAuxDataSetStrItem(AuxDataRef,
 *                                 "MachNumber",
 *                                 Value,
 *                                 TRUE)) // Retain
 *         {
 *           // ... item was added ...
 *         }
 *       else
 *         {
 *           // ... item failed to be added ...
 *         }
 *     }
 *   else
 *     {
 *       // ... allocation failure ...
 *     }
 * @endcode
 *
 * @ingroup AuxData
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAuxDataSetStrItem(AuxData_pa  AuxDataRef,
                                                       const char *Name,
                                                       const char *Value,
                                                       Boolean_t   Retain);
/**
 * Adds the auxiliary data item to the auxiliary data or replaces it if one
 * already exists by the same name.
 * @par Note:
 *   This function is available only in C/C++. For other languages,
 *   use TecUtilAuxDataSetStrItem.
 *
 * @param AuxDataRef
 *   Reference to the auxiliary data.
 *
 * @param Name
 *   Auxiliary data item's name. Be sure to consider a unique naming convention
 *   for your auxiliary data to avoid naming conflicts with auxiliary data
 *   produced by other addons or macros. By convention, addons that wish to
 *   share auxiliary data can prepend the name with the "Common" prefix. For
 *   example, the Plot3D loader uses CommonReferenceMachNumber for the
 *   reference Mach number. A valid auxiliary data name character must begin
 *   with a '_' or alpha character and may be followed by one or more '_', '.',
 *   alpha or digit characters.
 *
 * @param Value
 *   Value of the item to set. If the type is a string, a copy of the string
 *   pointed to by Value is used for the auxiliary data value.
 *
 * @param Type
 *   Type of item being set.
 *
 * @param Retain
 *   Flag specifying whether or not to retain this item on file export such as
 *   writing out a datafile, etc.
 *
 * @return
 *   TRUE if the item was added to the auxiliay data, FALSE if otherwise.
 *
 *
 * @pre <em>AuxDataRef</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Add an item named "MachNumber" to the frame's auxiliary data.
 *
 * @code
 *   AuxData_pa AuxDataRef = TecUtilAuxDataFrameGetRef();
 *   if (AuxDataRef != NULL)
 *     {
 *       ArbParam_t Value = (ArbParam_t)"1.75";
 *       if (TecUtilAuxDataSetItem(AuxDataRef,
 *                                 "MachNumber",
 *                                 Value,
 *                                 AuxDataType_String,
 *                                 TRUE)) // Retain
 *         {
 *           // ... item was added ...
 *         }
 *       else
 *         {
 *           // ... item failed to be added ...
 *         }
 *     }
 *   else
 *     {
 *       // ... allocation failure ...
 *     }
 * @endcode
 *
 * @ingroup AuxData
 *
 * #internalattributes exclude_fglue
 */
LINKTOADDON Boolean_t STDCALL TecUtilAuxDataSetItem(AuxData_pa     AuxDataRef,
                                                    const char    *Name,
                                                    ArbParam_t     Value,
                                                    AuxDataType_e  Type,
                                                    Boolean_t      Retain);

/**
 * Deletes the auxiliary data item.
 *
 * @param AuxDataRef
 *   Reference to the auxiliary data.
 *
 * @param Index
 *   Index of the auxiliary data item of interest.
 *
 *
 * @pre <em>AuxDataRef</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAuxDataDeleteItemByIndex(
 *   &           AuxDataRefPtr,
 *   &           Index)
 *    POINTER         (AuxDataRefPtr, AuxDataRef)
 *    INTEGER*4       Index
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Delete the dataset's fourth auxiliary data item:
 *
 * @code
 *   AuxData_pa AuxDataRef = TecUtilAuxDataDataSetGetRef();
 *   if (AuxDataRef != NULL)
 *     {
 *       TecUtilAuxDataDeleteItemByIndex(AuxDataRef, 4);
 *     }
 *   else
 *     {
 *       // ... allocation failure ...
 *     }
 * @endcode
 *
 * @ingroup AuxData
 *
 */
LINKTOADDON void STDCALL TecUtilAuxDataDeleteItemByIndex(AuxData_pa AuxDataRef,
                                                         LgIndex_t  Index);

/**
 * Deletes the auxiliary data item by the specified name if it exists.
 *
 * @param AuxDataRef
 *   Reference to the auxiliary data.
 *
 * @param Name
 *   Name used for the search. A valid auxiliary data name character must begin
 *   with a '_' or alpha character and may be followed by one or more '_', '.',
 *   alpha or digit characters.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 *
 * @pre <em>AuxDataRef</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAuxDataDeleteItemByName(
 *   &                   AuxDataRefPtr,
 *   &                   Name)
 *    POINTER         (AuxDataRefPtr, AuxDataRef)
 *    CHARACTER*(*)   Name
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   If it exists, delete the dataset's auxiliary data item named "MachNumber":
 *
 * @code
 *   AuxData_pa AuxDataRef = TecUtilAuxDataDataSetGetRef();
 *   if (AuxDataRef != NULL)
 *     {
 *       if (TecUtilAuxDataDeleteItemByName(AuxDataRef, "MachNumber"))
 *         {
 *           // ... item found and deleted ...
 *         }
 *       else
 *         {
 *           // ... item not found ...
 *         }
 *     }
 *   else
 *     {
 *       // ... allocation failure ...
 *     }
 * @endcode
 *
 * @ingroup AuxData
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilAuxDataDeleteItemByName(AuxData_pa  AuxDataRef,
                                                             const char *Name);



/**
 * Query to see if variable and connectivity sharing is permitted for this
 * dataset. You must still call TecUtilDataValueIsSharingOk() for variables
 * and TecUtilDataConnectIsSharingOk() for connectivity to determine if a
 * particular variable or if the connectivity may be shared.
 *
 * This function is \ref threadsafe.
 *
 * @return
 *   Returns TRUE if sharing is allowed in Tecplot, FALSE if otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetIsSharingAllowed()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *   Boolean_t IsSharing;
 *   IsSharing = TecUtilDataSetIsSharingAllowed();.
 * @endcode
 *
 * @sa TecUtilDataValueIsSharingOk() and TecUtilDataConnectIsSharingOk().
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetIsSharingAllowed(void);



/**
 * Determine if it is ok to share a variable between zones.  For rules on
 * sharing between zones, see TecUtilDataValueShare().
 *
 * This function is \ref threadsafe.
 *
 * @param SourceZone
 *   The source zone (the zone where the values will be stored).
 * @param DestZone
 *   The destination zone (the zone acquiring the shared values).
 * @param Var
 *   The variable to be shared.
 *
 * @return
 *   Returns TRUE if the specified variable sharing is allowed, FALSE if
 *   otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueIsSharingOk(
 *   &                   SourceZone,
 *   &                   DestZone,
 *   &                   Var)
 *    INTEGER*4       SourceZone
 *    INTEGER*4       DestZone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * If sharing is ok for variable 7 between zones 3 and 5 then share the variable.
 * @code
 *   if (TecUtilDataValueIsSharingOk(3,5,7))
 *     TecUtilDataValueShare(3,5,7);
 * @endcode
 *
 * @sa TecUtilDataSetIsSharingAllowed() and TecUtilDataConnectIsSharingOk().
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueIsSharingOk(EntIndex_t SourceZone,
                                                          EntIndex_t DestZone,
                                                          EntIndex_t Var);
/**
 * Determine if it is ok to share the connectivity between zones.
 *
 * @param SourceZone
 *   The source zone (the zone where the values will be stored).
 * @param DestZone
 *   The destination zone (the zone acquiring the shared values).
 *
 * @return
 *   Returns TRUE if the specified connectivity sharing is allowed, FALSE if
 *   otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataConnectIsSharingOk(
 *   &                   SourceZone,
 *   &                   DestZone)
 *    INTEGER*4       SourceZone
 *    INTEGER*4       DestZone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * If sharing is ok for connectivity between zones 3 and 5 then share the
 * connectivity.
 * @code
 *   if (TecUtilDataConnectIsSharingOk(3,5))
 *     TecUtilDataConnectShare(3,5);
 * @endcode
 *
 * @sa TecUtilDataSetIsSharingAllowed() and TecUtilDataValueIsSharingOk().
 *
 * @ingroup DataSharing
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataConnectIsSharingOk(EntIndex_t SourceZone,
                                                            EntIndex_t DestZone);

/**
 * Indicates if the connectivity has subzone load-on-demand data.
 *
 * @param Zone
 *   The zone to examine.
 *
 * @return
 *   Returns TRUE if the zone's connectivity has subzone load-on-demand data.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataConnectIsSZLData(
 *   &                   Zone)
 *    INTEGER*4       Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilVarIsSZLData()
 *
 * @ingroup Zone
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataConnectIsSZLData(EntIndex_t Zone);

/**
 * Indicates if the variable's data range is estimated.
 *
 * @param Zone
 *   Zone to examine.
 * @param Var
 *   Variable for which the information is desired.
 *
 * @return
 *   Returns TRUE if the variable's data range is estimated, otherwise FALSE.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarRangeIsEstimated(
 *   &                   Zone,
 *   &                   Var)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataValue
 */
LINKTOADDON Boolean_t STDCALL TecUtilVarRangeIsEstimated(EntIndex_t Zone,
                                                         EntIndex_t Var);
/**
 * Indicates if the variable's data is managed using subzone load-on-demand.
 *
 * @param Zone
 *   Zone to examine.
 * @param Var
 *   Variable for which subzone load-on-demand information is desired.
 *
 * @return
 *   Returns TRUE if the variable's data is managed using subzone load-on-demand.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarIsSZLData(
 *   &                   Zone,
 *   &                   Var)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilDataConnectIsSZLData()
 *
 * @ingroup DataValue
 */
LINKTOADDON Boolean_t STDCALL TecUtilVarIsSZLData(EntIndex_t Zone,
                                                  EntIndex_t Var);

/**
 * Gets the previous zone in the set of zones to consider that is sharing the variable with the
 * specified zone. If the zone isn't sharing with a lower numbered zone in the set of zones to
 * consider, TECUTILBADZONENUMBER is returned.
 *
 * @param ZonesToConsider
 *     Set of zones to consider when searching for a lower numbered zone. It must be a subset of the
 *     enabled zones in the dataset or NULL meaning all enabled zones should be considered.
 * @param Zone
 *     Zone for which the question is being asked.
 * @param Var
 *     Variable for which the question is being asked.
 * @return
 *     Lower numbered zone in the set of zones to consider that is sharing variable with this
 *     zone or TECUTILBADZONENUMBER if none of the zones to consider below this zone are sharing
 *     with it.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueGetPrevSharedZone(
 *   &                   ZonesToConsider,
 *   &                   Zone,
 *   &                   Var)
 *    POINTER         (ZonesToConsiderPtr, ZonesToConsider)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataValue
 */
LINKTOADDON EntIndex_t STDCALL TecUtilDataValueGetPrevSharedZone(Set_pa     ZonesToConsider,
                                                                 EntIndex_t Zone,
                                                                 EntIndex_t Var);
/**
 * Gets the set of zones that share the variable with the specified zone. If
 * the specified zone's variable is shared then it is also a member of the
 * resulting set otherwise an empty set is returned.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   Zone for which sharing information is desired.
 * @param Var
 *   Variable for which sharing information is desired.
 *
 * @return
 *   Allocated set of zones that share the variable with the specified zone.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueGetShareZoneSet(
 *   &           Zone,
 *   &           Var,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON TP_GIVES Set_pa STDCALL TecUtilDataValueGetShareZoneSet(EntIndex_t Zone,
                                                                    EntIndex_t Var);

/**
 * Gets the previous zone in the set of zones to consider that is sharing connectivity with the
 * specified zone. If the zone isn't sharing with a lower numbered zone in the set of zones to
 * consider, TECUTILBADZONENUMBER is returned.
 *
 * @param ZonesToConsider
 *     Set of zones to consider when searching for a lower numbered zone. It must be a subset of the
 *     enabled zones in the dataset or NULL meaning all enabled zones should be considered.
 * @param Zone
 *     Zone for which the question is being asked.
 * @return
 *     Lower numbered zone in the set of zones to consider that is sharing connectivity with this
 *     zone or TECUTILBADZONENUMBER if none of the zones to consider below this zone are sharing
 *     with it.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilConnectGetPrevSharedZone(
 *   &                   ZonesToConsider,
 *   &                   Zone)
 *    POINTER         (ZonesToConsiderPtr, ZonesToConsider)
 *    INTEGER*4       Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataSharing
 */
LINKTOADDON EntIndex_t STDCALL TecUtilConnectGetPrevSharedZone(Set_pa     ZonesToConsider,
                                                               EntIndex_t Zone);

/**
 * Gets the set of zones that share the connectivity with the specified zone.
 * If the specified zone's connectivity is shared then it is also a member of
 * the resulting set otherwise an empty set is returned.
 *
 * @param Zone
 *   Zone for which sharing information is desired.
 *
 * @return
 *   Allocated set of zones that share the connectivity with the specified
 *   zone.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilConnectGetShareZoneSet(
 *   &           Zone,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataSharing
 *
 */
LINKTOADDON TP_GIVES Set_pa STDCALL TecUtilConnectGetShareZoneSet(EntIndex_t Zone);

/**
 * Get a count for the number of times a particular variable is shared.
 * This function is \ref threadsafe.
 *
 * @param Zone
 *   Zone in which the shared variable is located.
 *
 * @param Var
 *   Variable that is shared.
 *
 * @return
 *   Returns share count for the given variable within the given zone. This is
 *   the number of times the data handle is shared. 1 means not shared (shared
 *   once), 2 means two zones share it, etc.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueGetShareCount(
 *   &                   Zone,
 *   &                   Var)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get share count of variable 2 in zone 1.
 *
 * @code
 *   EntIndex_t ShareCount = TecUtilDataValueGetShareCount(1, 2);
 * @endcode
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilDataValueGetShareCount(EntIndex_t Zone,
                                                             EntIndex_t Var);
/**
 * Returns the share count for connectivity for the given zone. This is the
 * number of times the connectivity is shared. 1 means not shared (shared
 * once), 2 means two zones share it, etc.
 *
 * @param Zone
 *   Zone number where connectivity is to be branched.
 *
 * @return
 *   Number of zones sharing connectivity.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataConnectGetShareCount(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataSharing
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilDataConnectGetShareCount(EntIndex_t Zone);

/**
 *
 * @ingroup Utilities
 *
 * #internalattributes exclude_alldoc
 */
LINKTOADDON AppMode_e       STDCALL TecUtilTecplotGetAppMode(void);
/**
 *
 * @ingroup Utilities
 *
 * #internalattributes exclude_alldoc
 */
LINKTOADDON ProductFlavor_e STDCALL TecUtilTecplotGetProductFlavor(void);


/**
 * Gets the number of contour levels and contour level values currently
 * defined for the specified contour group. The LevelValues array must be
 * deallocated by the addon using TecUtilArrayDealloc().
 *
 * @param ContourGroup
 *   The contour group of interest and must be an integer between 1 and 8.
 *
 * @param NumLevels
 *   Pointer to an LgIndex_t variable that will receive the current number of
 *   levels for the specified contour group.
 *
 * @param LevelValues
 *   Pointer to a double pointer variable that will receive the allocated
 *   double array of level values or NULL if there are none
 *
 * @return
 *   FALSE if an allocation error occured otherwise TRUE.
 *
 *
 * @pre <em>NumLevels</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>LevelValues</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilContourGetLevels(
 *   &                   ContourGroup,
 *   &                   NumLevels,
 *   &                   LevelValues)
 *    INTEGER*4       ContourGroup
 *    INTEGER*4       NumLevels
 *    REAL*8(*)       LevelValues
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Fetch the number of contour levels for contour group 2:
 *
 * @code
 *   Boolean_t IsOk;
 *   LgIndex_t NumLevels;
 *   double    *LevelValues;
 *
 *   TecUtilLockStart(AddOnID);
 *   IsOk = TecUtilContourGetLevels(2, &NumLevels, &LevelValues);
 *   if (IsOk)
 *     {
 *       if (NumLevels != 0)
 *         {
 *           LgIndex_t LIndex;
 *           printf("There are %d levels for contour group #2:\n",
 *                  NumLevels);
 *           for (LIndex = 0; LIndex < NumLevels; LIndex++)
 *             printf("  %lg\n", LevelValues[LIndex]);
 *           TecUtilArrayDealloc((void **)&LevelValues);
 *         }
 *       else
 *         printf("No levels are specified for contour group #2\n");
 *     }
 *
     TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup Contour
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourGetLevels(SmInteger_t             ContourGroup,
                                                      TP_OUT LgIndex_t*       NumLevels,
                                                      TP_ARRAY_GIVES double** LevelValues);


/**
 * Query to see if a color band is in use in the a contour group.
 * This function only applies to the current frame.
 *
 * @param ContourGroup
 *   The contour group of interest and must be an integer between 1 and 8.
 *
 * @return
 *   TRUE if a color band is in use in the current frame for a contour group.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryColorBandsInUseForContourGroup(
 *   &                   ContourGroup)
 *    INTEGER*4       ContourGroup
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Verify if a color band is in use for the contour group 3:
 *
 * @code
 *   Boolean_t IsOk;
 *   LgIndex_t NumLevels;
 *   double    *LevelValues;
 *
 *   TecUtilLockStart(AddOnID);
 *   IsOk = ;
 *   if (TecUtilQueryColorBandsInUseForContourGroup(3))
 *     printf("There is a color bands in use for contour group #3\n");
 *   else
 *     printf("There are no color bands in use for contour group #3\n");
 * @endcode
 *
 * @ingroup Contour
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryColorBandsInUseForContourGroup(SmInteger_t ContourGroup);


/**
 * Query to see if Load on Demand is permitted for this
 * dataset.
 *
 * This function is \ref threadsafe.
 *
 * @return
 *   Returns TRUE if Load On Demand is allowed in Tecplot, FALSE if otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetIsLODAllowed()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *   Boolean_t IsLODAllowed;
 *   IsLODAllowed = TecUtilDataSetIsLODAllowed();.
 * @endcode
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetIsLODAllowed(void);


/**
 * Returns the Solution Time associated with the specified zone.
 * This function is \ref threadsafe.
 *
 * @return
 *   Solution time for the zone.
 *
 * @param Zone
 *   A zone number for a currently enabled zone.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilZoneGetSolutionTime(
 *   &                Zone)
 *    INTEGER*4    Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilSolutionTimeSetCurrent() and TecUtilSolutionTimeGetCurrent()
 *
 * @ingroup Zone
 *
 */
LINKTOADDON double STDCALL TecUtilZoneGetSolutionTime(EntIndex_t Zone);

/**
 * Returns the StrandID associated with the specified zone.
 *
 * @return
 *   StrandID for the zone. 0 indicates that the zone is not
 *   part of a strand. Transient zones will have a StrandID of
 *   1 or greater.
 *
 * @param Zone
 *   A zone number for a currently enabled zone.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetStrandID(
 *   &                   Zone)
 *    INTEGER*4       Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilZoneGetSolutionTime()
 *
 * @ingroup Zone
 *
 */
LINKTOADDON Strand_t STDCALL TecUtilZoneGetStrandID(EntIndex_t Zone);

/**
 * Returns the number of the Parent Zone associated with the specified zone.
 *
 * @return
 *   Parent Zone for the zone. 0 indicates that the zone has no
 *   parent zone.
 *
 * @param Zone
 *   A zone number for a currently enabled zone.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetParentZone(
 *   &                   Zone)
 *    INTEGER*4       Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Zone
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilZoneGetParentZone(EntIndex_t Zone);


/**
 * Gets the position of the zone in the Zone Style dialog. This function
 * may only be called when the Plot Type is 2D or 3D.
 *
 * @pre Must have one or more frames.
 *
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Zone
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilZoneGetFieldMap(EntIndex_t Zone);

/**
 * Returns the current solution time for the current frame.
 * This function is \ref threadsafe.
 *
 * @since
 *     12.1.1.8018
 *
 * @return
 *   Current solution time for the current frame.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilSolutionTimeGetCurrent();
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilSolutionTimeSetCurrent(), TecUtilSolutionTimeGetMin(),
 *     TecUtilSolutionTimeGetMax(), and TecUtilZoneGetSolutionTime().
 *
 * @ingroup Time
 *
 */
LINKTOADDON double STDCALL TecUtilSolutionTimeGetCurrent(void);

/**
 * Returns the minimum solution time for the current frame.
 * This function is \ref threadsafe.
 *
 * @since
 *     12.1.1.8018
 *
 * @return
 *   Minimum solution time for the current frame.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilSolutionTimeGetMin();
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilSolutionTimeSetCurrent(), TecUtilSolutionTimeGetCurrent(),
 *     TecUtilSolutionTimeGetMax(), and TecUtilZoneGetSolutionTime().
 *
 * @ingroup Time
 *
 */
LINKTOADDON double STDCALL TecUtilSolutionTimeGetMin(void);

/**
 * Returns the maximum solution time for the current frame.
 * This function is \ref threadsafe.
 *
 * @return
 *   Maximum solution time for the current frame.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilSolutionTimeGetMax();
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilSolutionTimeSetCurrent(), TecUtilSolutionTimeGetCurrent(),
 *     TecUtilSolutionTimeGetMin(), and TecUtilZoneGetSolutionTime().
 *
 * @ingroup Time
 *
 */
LINKTOADDON double STDCALL TecUtilSolutionTimeGetMax(void);

/**
 * Indicates how many faces comprise the specified face mapping.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *     11.2-1-8349
 *
 * @param FaceMap
 *     Face mapping.
 *
 * @return
 *     Number of faces.
 *
 *
 * @pre <em>FaceMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataFaceMapGetNFaces (
 *   &          FaceMapPtr)
 *    POINTER   (FaceMapPtr, FaceMap)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilDataFaceMapGetNFaces(FaceMap_pa FaceMap);

/**
 * Indicates how many nodes comprise the specified face.
 * Always returns 2 for polygonal zones.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *     11.2-1-0
 *
 * @param FaceMap
 *     Face mapping.
 * @param Face
 *     The face for which the number of nodes is desired.
 *
 * @return
 *     Number of face nodes.
 *
 *
 * @pre <em>FaceMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataFaceMapGetNFaceNodes (
 *   &          FaceMapPtr,
 *   &          Face)
 *    POINTER   (FaceMapPtr, FaceMap)
 *    INTEGER*4 Face
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilDataFaceMapGetNFaceNodes(FaceMap_pa FaceMap,
                                                              LgIndex_t  Face);


/**
 * Fetch a node from the face.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *     11.2-1-0
 *
 * @param FaceMap
 *     Face mapping.
 * @param Face
 *     The face for which the node is desired.
 * @param NodeOffset
 *     The node offset within the face.
 *
 * @return
 *     The desired node.
 *
 *
 * @pre <em>FaceMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataFaceMapGetFaceNode (
 *   &          FaceMapPtr,
 *   &          Face,
 *   &          NodeOffset)
 *    POINTER   (FaceMapPtr, FaceMap)
 *    INTEGER*4 Face
 *    INTEGER*4 NodeOffset
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilDataFaceMapGetFaceNode(FaceMap_pa FaceMap,
                                                            LgIndex_t  Face,
                                                            LgIndex_t  NodeOffset);


/**
 * The left element associated with the specified face.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *     11.2-1-0
 *
 * @param FaceMap
 *     Face mapping.
 * @param Face
 *     The face for which the left element is desired.
 *
 * @return
 *     Left element number, TECUTIL_NO_NEIGHBORING_ELEM if there is
 *     no neighbor, or TECUTIL_BOUNDARY_FACE indicating that this
 *     is a boundary face. Further information about a boundary
 *     face may be gathered. See TecUtilDataFaceMapGetNBndryConns
 *     for more details.
 *
 *
 * @pre <em>FaceMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataFaceMapGetLeftElem (
 *   &          FaceMapPtr,
 *   &          Face)
 *    POINTER   (FaceMapPtr, FaceMap)
 *    INTEGER*4 Face
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilDataFaceMapGetLeftElem(FaceMap_pa FaceMap,
                                                            LgIndex_t  Face);


/**
 * The right element associated with the specified face.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *     11.2-1-0
 *
 * @param FaceMap
 *     Face mapping.
 * @param Face
 *     The face for which the right element is desired.
 *
 * @return
 *     Right element number, TECUTIL_NO_NEIGHBORING_ELEM if there is
 *     no neighbor, or TECUTIL_BOUNDARY_FACE indicating that this
 *     is a boundary face. Further information about a boundary
 *     face may be gathered. See TecUtilDataFaceMapGetNBndryConns
 *     for more details.
 *
 *
 * @pre <em>FaceMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataFaceMapGetRightElem (
 *   &          FaceMapPtr,
 *   &          Face)
 *    POINTER   (FaceMapPtr, FaceMap)
 *    INTEGER*4 Face
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilDataFaceMapGetRightElem(FaceMap_pa FaceMap,
                                                             LgIndex_t  Face);


/**
 * Retrieves the number of boundary connections for the specified face.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *     11.2-1-0
 *
 * @param FaceMap
 *     Face mapping.
 * @param Face
 *     The face for which the number of connections is desired.
 *
 * @return
 *     The number of boundary connections for the face.
 *
 *
 * @pre <em>FaceMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataFaceMapGetNBndryConns (
 *   &          FaceMapPtr,
 *   &          Face)
 *    POINTER   (FaceMapPtr, FaceMap)
 *    INTEGER*4 Face
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilDataFaceMapGetNBndryConns(FaceMap_pa FaceMap,
                                                               LgIndex_t  Face);


/**
 * Fetch a connected element and its zone for a boundary face.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *     11.2-1-0
 *
 * @param FaceMap
 *     Face mapping.
 * @param Face
 *     The boundary face for which the connected element is desired.
 * @param BndryConnOffset
 *     The boundary connection offset for the element. This must be between
 *     1 and the number of boundary elements returned by
 *     TecUtilDataFaceMapGetNBndryConns.
 * @param BndryElem
 *     The desired connected element. A returned element number of
 *     TECUTIL_NO_NEIGHBORING_ELEM indicates no neighboring element. If
 *     this is the only boundary connection for the face,
 *     then the boundary face has no neighboring elements. If there are
 *     more boundary connections for this boundary face, then the
 *     boundary face is partially obscured, and the remaining
 *     boundary connections for the face will list the elements and
 *     zones that partially obscure the face.
 * @param BndryElemZone
 *     The desired connected zone. A returned zone number of
 *     TECUTIL_NO_NEIGHBORING_ZONE indicates a self reference (in
 *     other words, the zone associated with the face mapping).
 *
 *
 * @pre <em>FaceMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>BndryElem</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>BndryElemZone</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataFaceMapGetBndryConn (
 *   &           FaceMapPtr,
 *   &           Face,
 *   &           BndryConnOffset,
 *   &           BndryElem,
 *   &           BndryElemZone)
 *    POINTER   (FaceMapPtr, FaceMap)
 *    INTEGER*4 Face
 *    INTEGER*4 BndryConnOffset
 *    INTEGER*4 BndryElem
 *    INTEGER*4 BndryElemZone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON void STDCALL TecUtilDataFaceMapGetBndryConn(FaceMap_pa         FaceMap,
                                                        LgIndex_t          Face,
                                                        LgIndex_t          BndryConnOffset,
                                                        TP_OUT LgIndex_t*  BndryElem,
                                                        TP_OUT EntIndex_t* BndryElemZone);


/**
 * Gets a readable face mapping for the specified polyhedral or
 * polygonal zone of the current frame.
 * This function is \ref threadsafe.
 *
 * @since
 *     11.2-1-0
 *
 * @param Zone
 *     Polyhedral or polygonal zone number.
 *
 * @return
 *     A readable face mapping for the specified polyhedral or
 *     polygonal zone or NULL if unsuccessful.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataFaceMapGetReadableRef (
 *   &           Zone,
 *   &           FaceMapPtr)
 *    INTEGER*4 Zone
 *    POINTER   (FaceMapPtr, FaceMap)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON FaceMap_pa STDCALL TecUtilDataFaceMapGetReadableRef(EntIndex_t Zone);


/**
 * Gets a writable face mapping for the specified polyhedral or
 * polygonal zone of the current frame. If the current face mapping
 * for the zone is not writable, the current face mapping will be
 * replaced with a writable copy. If the original face mapping was
 * shared with other zones, the writable copy will be shared
 * with the same zones.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *     11.2-1-0
 *
 * @param Zone
 *     Polyhedral or polygonal zone number.
 *
 * @return
 *     A writable face mapping for the specified polyhedral or
 *     polygonal zone or NULL if unsuccessful.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataFaceMapGetWritableRef (
 *   &           Zone,
 *   &           FaceMapPtr)
 *    INTEGER*4 Zone
 *    POINTER   (FaceMapPtr, FaceMap)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON FaceMap_pa STDCALL TecUtilDataFaceMapGetWritableRef(EntIndex_t Zone);


/**
 * Gets a readable node-to-element mapping for the specified finite element
 * zone of the current frame.
 * This function is \ref threadsafe.
 *
 * @since
 *     12.0.1.5894
 *
 * @param Zone
 *     Finite element zone number.
 *
 * @return
 *     A readable node-to-element mapping for the specified finite element zone
 *     or NULL if unsuccessful.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataNodeToElemMapGetReadableRef (
 *   &           Zone,
 *   &           NodeToElemMapPtr)
 *    INTEGER*4 Zone
 *    POINTER   (NodeToElemMapPtr, NodeToElemMap)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataServices
 */
LINKTOADDON NodeToElemMap_pa STDCALL TecUtilDataNodeToElemMapGetReadableRef(EntIndex_t Zone);

/**
 * Gets the number of elements that are connected to the specified node.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *     12.0.1.5894
 *
 * @param NodeToElemMap
 *   The node-to-element map handle.
 *
 * @param Node
 *   The node for which the connected elements are desired.
 *
 * @return
 *     The number of elements that are connected to the specified node.
 *
 *
 * @pre <em>NodeToElemMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataNodeToElemMapGetNumElems (
 *   &           NodeToElemMapPtr,
 *   &           Node)
 *    POINTER   (NodeToElemMapPtr, NodeToElemMap)
 *    INTEGER*4 Node
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataServices
 */
LINKTOADDON LgIndex_t STDCALL TecUtilDataNodeToElemMapGetNumElems(NodeToElemMap_pa NodeToElemMap,
                                                                  LgIndex_t        Node);
/**
 * Gets an element that is connected to the specified node.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *     12.0.1.5894
 *
 * @param NodeToElemMap
 *   The node-to-element map handle.
 *
 * @param Node
 *   The node for which the connected element is desired.
 *
 * @param ElemOffset
 *   The element offset of the desired node element.
 *   Must be in the range [1, n], where n is the number of
 *   elements returned by TecUtilDataNodeToElemMapGetNumElems() for the
 *   specified node.
 *
 * @return
 *     The element connected to the specified node.
 *
 *
 * @pre <em>NodeToElemMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataNodeToElemMapGetElem (
 *   &           NodeToElemMapPtr,
 *   &           Node,
 *   &           ElemOffset)
 *    POINTER   (NodeToElemMapPtr, NodeToElemMap)
 *    INTEGER*4 Node
 *    INTEGER*4 ElemOffset
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataServices
 */
LINKTOADDON LgIndex_t STDCALL TecUtilDataNodeToElemMapGetElem(NodeToElemMap_pa NodeToElemMap,
                                                              LgIndex_t        Node,
                                                              LgIndex_t        ElemOffset);
/**
 * Gets a readable element-to-face mapping for the specified
 * polyhedral or polygonal zone of the current frame.
 * This function is \ref threadsafe.
 *
 * @since
 *     11.2-1-0
 *
 * @param Zone
 *     Polyhedral or polygonal zone number.
 *
 * @return
 *     A readable element-to-face mapping for the specified
 *     polyhedral or polygonal zone or NULL if unsuccessful.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataElemGetReadableRef (
 *   &           Zone,
 *   &           ElemToFaceMapPtr)
 *    INTEGER*4 Zone
 *    POINTER   (ElemToFaceMapPtr, ElemToFaceMap)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON ElemToFaceMap_pa STDCALL TecUtilDataElemGetReadableRef(EntIndex_t Zone);


/**
 * Gets the number of faces that comprise the specified
 * polyhedral or polygonal element.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *     11.2-1-0
 *
 * @param ElemToFaceMap
 *   The element-to-face map handle
 *
 * @param Elem
 *   The element for which to return the number of faces.
 *
 * @return
 *     The number of faces that comprise the element.
 *
 *
 * @pre <em>ElemToFaceMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataElemGetNumFaces (
 *   &           ElemToFaceMapPtr,
 *   &           Elem)
 *    POINTER   (ElemToFaceMapPtr, ElemToFaceMap)
 *    INTEGER*4 Elem
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilDataElemGetNumFaces(ElemToFaceMap_pa ElemToFaceMap,
                                                         LgIndex_t        Elem);


/**
 * Gets the specified face number for the specified
 * polyhedral or polygonal element.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *     11.2-1-0
 *
 * @param ElemToFaceMap
 *   The element-to-face map handle
 *
 * @param Elem
 *   The element for which to return the number of faces.
 *
 * @param FaceOffset
 *   The face offset for which to return the face number.
 *   Must be in the range [1, n], where n is the number of
 *   faces returned by TecUtilDataElemGetNumFaces() for the
 *   specified element.
 *
 * @return
 *     The face number for the element.
 *
 *
 * @pre <em>ElemToFaceMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataElemGetFace (
 *   &           ElemToFaceMapPtr,
 *   &           Elem,
 *   &           FaceOffset)
 *    POINTER   (ElemToFaceMapPtr, ElemToFaceMap)
 *    INTEGER*4 Elem
 *    INTEGER*4 FaceOffset
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilDataElemGetFace(ElemToFaceMap_pa ElemToFaceMap,
                                                     LgIndex_t        Elem,
                                                     ElemFaceOffset_t FaceOffset);


/**
 * Determine if a variable is passive for a particular zone.
 * Refer to the <A HREF="../dataformat.pdf" TARGET=_blank>Data Format Guide</A>
 * for more information on passive variables.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.2-0-380
 *
 * @param Zone
 *   The number of the zone to which the variable belongs.
 *
 * @param Var
 *   The number of the variable in question.
 *
 * @return
 *   Returns TRUE if the specified variable is passive, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueIsPassive(
 *   &                   Zone,
 *   &                   Var)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueIsPassive(EntIndex_t Zone,
                                                        EntIndex_t Var);

/**
 * Calculates the width of an image for exporting for the given export format and plot region.
 *
 * @since
 *   11.2-0-439
 *
 * @param ExportFormat
 *   The format in which an image will be exported.
 *
 * @param ExportRegion
 *   The region of the plot that will be exported.
 *
 * @return
 *   Returns default width of the image for exporting.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGetDefaultExportImageWidth(
 *   &                   ExportFormat,
 *   &                   ExportRegion)
 *    INTEGER*4       ExportFormat
 *    INTEGER*4       ExportRegion
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Export
 *
 */
LINKTOADDON ScreenDim_t STDCALL TecUtilGetDefaultExportImageWidth(ExportFormat_e ExportFormat,
                                                                  ExportRegion_e ExportRegion);

/**
 * Determine if the vector variables have been assigned to valid variable number.
 *
 * @since
 *   12.0.1.5625
 *
 * @return
 *   Returns true if all vector variable components have been assigned to a valid variable
 *   number, false otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVectorCheckVariableAssignments()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Vector
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilVectorCheckVariableAssignments(void);

/**
 * Returns a list of zones associated to the specifiesd strand Id.
 *
 * @param strandID
 *     Strand Id to get the related zones.
 *
 * @return
 *     Allocated set containing the zones related to the specified strand Id.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa
 * @since 13.0.0.14385
 * @ingroup Utilities
 */
LINKTOADDON TP_GIVES Set_pa STDCALL TecUtilDataSetGetZonesForStrandID(Strand_t strandID);

/**
 * Returns a list of all the existing strand Ids.
 *
 * @return
 *     Allocated set containing the strand Ids.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa
 * @since 13.0.0.14385
 * @ingroup Utilities
 */
LINKTOADDON TP_GIVES Set_pa STDCALL TecUtilDataSetGetStrandIDs(void);

/**
 * Returns a list of all the active existing strand Ids.
 *
 * @return
 *     Allocated set containing the active strand Ids.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa
 * @since 13.0.0.14385
 * @ingroup Utilities
 */
LINKTOADDON TP_GIVES Set_pa STDCALL TecUtilDataSetGetActiveStrandIDs(void);

/**
 * Extracts time values from a consistent tokenized location within the list of file names.
 * Each file name is broken into its tokenized parts separated by alpha characters and numbers.
 * A dash ('-') or a plus sign ('+') is considered part of a number if it precedes a number
 * and is not preceded by an alpha character or a digit character, otherwise it is
 * considered part of the alpha tokens. If the tokenized file names have multiple numeric
 * fields the one that provides the unique set of times is chosen.
 * Example:
 * The following list of file names:
 * @verbatim
     mach1.1_alpha0.75_+1.0e-1+1530.q
     mach1.1_alpha0.80_+1.1e-1+1530.q
     mach1.1_alpha0.85_+1.2e-1+1530.q
     mach1.1_alpha0.90_+1.3e-1+1530.q
     mach1.2_alpha0.75_+1.4e-1+1530.q
     mach1.2_alpha0.80_+1.5e-1+1530.q
     mach1.2_alpha0.85_+1.6e-1+1530.q
     mach1.2_alpha0.90_+1.7e-1+1530.q
   @endverbatim
 * will be splitted into the following tokens:
 * @verbatim
     "mach", "1.1", "_alpha", "0.75_", "+1.0e-1", "+", "1530", ".q"
     "mach", "1.1", "_alpha", "0.80_", "+1.1e-1", "+", "1530", ".q"
     "mach", "1.1", "_alpha", "0.85_", "+1.2e-1", "+", "1530", ".q"
     "mach", "1.1", "_alpha", "0.90_", "+1.3e-1", "+", "1530", ".q"
     "mach", "1.2", "_alpha", "0.75_", "+1.4e-1", "+", "1530", ".q"
     "mach", "1.2", "_alpha", "0.80_", "+1.5e-1", "+", "1530", ".q"
     "mach", "1.2", "_alpha", "0.85_", "+1.6e-1", "+", "1530", ".q"
     "mach", "1.2", "_alpha", "0.90_", "+1.7e-1", "+", "1530", ".q"
   @endverbatim
 * and will return the following time steps
 * @verbatim
     0.1
     0.11
     0.12
     0.13
     0.14
     0.15
     0.16
     0.17
   @endverbatim
 * since the second to last numerical column (the fourth to last considering all columns)
 * is the one that contains unique values.
 *
 * @param fileNames
 *     String list of file names containing an embedded time value.
 * @param times
 *     A pre-sized array to receive the resulting time values. If the file names contain
 *     multiple numeric token fields, the token field that contains a unique set of time
 *     values is used.
 * @param requireAlphaTokenMatching
 *     A boolean flag that turns on or off the behavior to force the alpha character tokens
 *     to match, case sensitive. TRUE enables this behavior, FALSE disables it.
 * @return
 *     TRUE if the time values could be parsed, FALSE otherwise.
 *
 *
 * @pre <em>times</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>requireAlphaTokenMatching</em>
 *   Value must be TRUE or FALSE.
 *
 *
 * @since 13.2.0.16744
 * @sa
 * @ingroup Utilities
 */
LINKTOADDON Boolean_t STDCALL TecUtilExtractTimesFromFileNames(StringList_pa        fileNames,
                                                               TP_ARRAY_OUT double* times,
                                                               Boolean_t            requireAlphaTokenMatching);


/**
 * Determine if a variable is loaded for a particular zone.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @param Zone
 *   The number of the zone to which the variable belongs.
 *
 * @param Var
 *   The number of the variable in question.
 *
 * @return
 *   Returns TRUE if the specified variable is loaded, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueIsLoaded(
 *   &                   Zone,
 *   &                   Var)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueIsLoaded(EntIndex_t Zone,
                                                       EntIndex_t Var);

/**
 * Determine if streamtraces can be ploted. 
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if streamtraces can be plotted, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryCanPlotStreamtraces()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryCanPlotStreamtraces(void);

/**
 * Determine if volume streamtraces can be ploted. 
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if volume streamtraces can be plotted, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryCanPlotVolumeStreamtraces()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryCanPlotVolumeStreamtraces(void);

/**
 * Determine if slices can be ploted.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if slices be plotted, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryCanPlotSlices()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryCanPlotSlices(void);

/**
 * Determine if the plot contains contour lines.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if plot contains contour lines, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryPlotContainsContourLines()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryPlotContainsContourLines(void);

/**
 * Determine if contour level modification are allowed.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if contour level modification are allowed, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryContourLevelModificationsAllowed()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryContourLevelModificationsAllowed(void);

/**
 * Determine if points can be extracted.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if points can be extracted, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryOkToExtractPoints()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryOkToExtractPoints(void);

/**
 * Determine if contour lines can be extracted.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if contour lines can be extracted, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryOkToExtractContourLines()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryOkToExtractContourLines(void);

/**
 * Determine if isosurfaces can be extracted.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if isosurfaces can be extracted, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryOkToExtractIsoSurfaces()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryOkToExtractIsoSurfaces(void);

/**
 * Determine if current slices can be extracted.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if current slices can be extracted, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryOkToExtractSlices()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryOkToExtractSlices(void);

/**
 * Determine if stream traces can be extracted.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if stream traces can be extracted, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryOkToExtractStream()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryOkToExtractStream(void);

/**
 * Determine if there are active streamtraces.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if there are active streamtraces, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryStreamtracesAreActive()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryStreamtracesAreActive(void);

/**
 * Determine if there are picked objects to copy
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if there are picked objects to copy, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryOkToCopyPickedObjects()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryOkToCopyPickedObjects(void);

/**
 * Determine if there are picked objects to paste
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if there are picked objects to paste, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryOkToPastePickedObjects()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryOkToPastePickedObjects(void);

/**
 * Determine if there are picked objects to clear
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if there are picked objects to clear, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryOkToClearPickedObjects()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryOkToClearPickedObjects(void);

/**
 * Determine if there are picked objects to send to the back or bring to the front
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if there are picked objects to send to the back or bring to the
 *   front, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryOkToPushPopPickedObjects()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryOkToPushPopPickedObjects(void);

/**
 * Determine if the dirty layout flag was activated
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if the dirty flag was activated, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryIsLayoutDirty()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryIsLayoutDirty(void);

/**
 * Determine if the current layout has any style, checking if there are pages
 * or data ser defined, as well as a base text or base geom on a current frame
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if the current layout has style, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryLayoutHasStyle()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryLayoutHasStyle(void);

/**
 *   Queries the state of the probe callback
 *
 * @return
 *   Returns TRUE if a probe callback is installed, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeIsCallbackInstalled()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Query the state of the probe callback
 *
 * @code
 *   {
 *   Boolean_t ProbeCallbackIsInstalled;
 *   TecUtilLockStart(AddOnID);
 *   ProbeCallbackIsInstalled = TecUtilProbeIsCallbackInstalled();
 *   TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @since 14.2
 *
 * @sa TecUtilProbeInstallCallback TecUtilProbeInstallCallbackX
 *
 * @ingroup Probe
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilProbeIsCallbackInstalled(void);

/**
 * Determine if the plot contains surfaces in the current frame.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if plot contains surfaces, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryPlotContainsSurfaceZones()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryPlotContainsSurfaceZones(void);

/**
 * Determine if the plot contains points in the current frame.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if plot contains points, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryPlotContainsPoints()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryPlotContainsPoints(void);

/**
 * Determine if the plot contains volume zones in the current frame.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if plot contains volume zones, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryPlotContainsVolumeZones()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryPlotContainsVolumeZones(void);


/**
 * Determine the rank of a zone. 
 * 1 = I-Ordered, J-Ordered, K-Ordered or FE-Line segment
 * 2 = IJ-Ordered, JK-Ordered, IK-Ordered, FE-Triangle, FE-Quad, or FE-Polygon
 * 3 = IJK-Ordered, FE-Tetrahedral, FE-Brick, or FE-Polyhedral
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns the rank
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryGetZoneRank()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON SmInteger_t STDCALL TecUtilQueryGetZoneRank(EntIndex_t Zone);


/**
 * Determine if iso surfaces can be ploted in the current frame.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.2
 *
 * @return
 *   Returns TRUE if iso surfaces be plotted, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQueryCanPlotIsoSurfaces()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilQueryCanPlotIsoSurfaces(void);


/**
 * Get the number of macro functions which should be displayed in the quick macro panel.
 *
 * @return
 *  The number of macro functions.
 *
 *   Examine all macro functions:
 *
 * @code
 *  LgIndex_t count = TecUtilMacroFunctionGetCount();
 *  for (LgIndex_t index = 1; index <= count; ++index)
 *  {
 *      char *macroFunctionName = NULL;
 *      if (TecUtilMacroFunctionGetName(index, &macroFunctionName))
 *      {
 *          printf("The macro function name is: %s\n", macroFunctionName);
 *          TecUtilStringDealloc(&macroFunctionName);
 *      }
 *      char accelerator = TecUtilMacroFunctionGetAcceleratorKey(index);
 *  }
 * @endcode
 *
 * @sa TecUtilMacroFunctionGetName, TecUtilMacroFunctionGetAcceleratorKey
 */
LINKTOADDON LgIndex_t STDCALL TecUtilMacroFunctionGetCount(void);


/**
 * Get the name of the macro function, given a 1-based index.
 *
 * @param Index
 *      1-based index of the macro function. 
 *
 * @param MacroFunctionName
 *      Receives the macro function name. Result must be deallocated with TecUtilStringDealloc() when no longer needed.
 *
 *
 * @pre <em>MacroFunctionName</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 *   Examine all macro functions:
 *
 * @code
 *  LgIndex_t count = TecUtilMacroFunctionGetCount();
 *  for (LgIndex_t index = 1; index <= count; ++index)
 *  {
 *      char *macroFunctionName = NULL;
 *      if (TecUtilMacroFunctionGetName(index, &macroFunctionName))
 *      {
 *          printf("The macro function name is: %s\n", macroFunctionName);
 *          TecUtilStringDealloc(&macroFunctionName);
 *      }
 *      char accelerator = TecUtilMacroFunctionGetAcceleratorKey(index);
 *  }
 * @endcode
 *
 * @ingroup MacroFunction
 *
 * @sa TecUtilMacroFunctionGetCount, TecUtilMacroFunctionGetAcceleratorKey
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroFunctionGetName(LgIndex_t       Index, 
                                                          TP_GIVES char** MacroFunctionName);

/**
 * Gets the accelerator key for the macro, given a 1-based index.
 *
 * @return
 *  The accelerator key for the macro.
 *
 * @param Index
 *      1-based index of the macro function
 *
 *   Examine all macro functions:
 *
 * @code
 *  for (MacroFunction_ID macroFunctionID = TecUtilMacroFunctionGetBase();
 *                        macroFunctionID != TECUTILBADID;
 *                        macroFunctionID = TecUtilMacroFunctionGetNext(macroFunctionID))
 *  {
 *      char *macroFunctionName = NULL;
 *      if (TecUtilMacroFunctionGetName(macroFunctionId, &macroFunctionName))
 *      {
 *          printf("The macro function name is: %s\n", macroFunctionName);
 *          TecUtilStringDealloc(&macroFunctionName);
 *      }
 *      char accelerator = TecUtilMacroFunctionGetAcceleratorKey(macroFunctionID);
 *  }
 * @endcode
 *
 * @ingroup MacroFunction
 *
 * @sa TecUtilMacroFunctionGetCount, TecUtilMacroFunctionGetName
 */
LINKTOADDON char STDCALL TecUtilMacroFunctionGetAcceleratorKey(LgIndex_t Index);

/**
 * Gets the total number of colormaps.
 * These include both standard and custom color maps.
 *
 * @since
 *   14.1
 *
 * @return
 *   Total number of colormaps.
 *
 * @sa TecUtilColorMapGetName, TecUtilColorMapGetColorMapNumber
 */
LINKTOADDON SmInteger_t STDCALL TecUtilColorMapGetCount(void);

/**
 * Given a 1-based colormap number, queries the name of the colormap.
 *
 * @since
 *   14.1
 *
 * @param ColorMapNumber
 *  1-based ColorMapNumber to query.
 *  This number must be between 1 and the number of color maps.
 *
 * @param ColorMapName
 *  Receives the name of the color map.
 *  This string must be released by the caller using TecUtilStringDealloc.
 *
 * @sa TecUtilColorMapGetCount, TecUtilColorMapGetColorMapNumber
 */
LINKTOADDON void  STDCALL TecUtilColorMapGetName(SmInteger_t ColorMapNumber, char **ColorMapName);

/**
 * Given a colormap name, queries the 1-based color map number for that name.
 *
 * @since
 *   14.1
 *
 * @param ColorMapName
 *  Name of the colormap to query. Must be a non-zero length string.
 *
 * @return
 *  Color map number for the given name.
 *
 * @sa TecUtilColorMapGetCount, TecUtilColorMapGetName
 */
LINKTOADDON SmInteger_t TecUtilColorMapGetColorMapNumber(const char *ColorMapName);

/**
 * Returns in a string the coefficients for the equations used to draw the curve 
 * for a selected line-mapping.
 *
 * @since
 *   14.1
 *
 * @param LineMap
 *   The number of a line-mapping
 *
 * @param DisplayInfo
 *   Variable were the information will be written
 *
 * @return
 *   TRUE if the curve information was retrieved, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCurveGetDisplayInfo(
 *   &                   LineMap,
 *   &                   DisplayInfo
 *   &                   DisplayInfoLength)
 *    INTEGER*4       LineMap
 *    CHARACTER*(*)   DisplayInfo
 *    INTEGER*4       DisplayInfoLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Receive the coefficients information of the line-mapping number 3 in the
 * variable displayInfo:
 *
 * @code
 *   Boolean_t IsOk;
 *   char* DisplayInfo = NULL;
 *   IsOk = TecUtilCurveGetDisplayInfo(3, &DisplayInfo);
 *   // Use DisplayInfo
 *   TecUtilStringDealloc(&DisplayInfo);
 * @endcode
 *
 * @ingroup LineMap
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilCurveGetDisplayInfo(EntIndex_t      LineMap,
                                                         TP_GIVES char** DisplayInfo);

 /**
  * Check if a zone is a valid Fourier Transform zone.
  *
  * @since
  *   14.2
  *
  * @param zoneNum
  *   1-based zone number to check
  *
  * @return
  *   TRUE if the zone is a valid Fourier Transform zone, FALSE otherwise.
  *   
  * <FortranSyntax>
  *    INTEGER*4 FUNCTION TecUtilIsValidFourierTransformZone(
  *   &                   zoneNumber)
  *    INTEGER*4 zoneNumber
  * </FortranSyntax>
  *
  * <PythonSyntax>
  * </PythonSyntax>
  */
LINKTOADDON Boolean_t STDCALL TecUtilFourierTransformIsValidZone(EntIndex_t zoneNum);

#endif /* _TECUTIL_Q_H_ */
