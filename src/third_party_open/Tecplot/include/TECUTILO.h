#ifndef _TECUTILO_H
#define _TECUTILO_H
/*
******************************************************************
******************************************************************
*******                                                   ********
******             (C) 1988-2013 Tecplot, Inc.             *******
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
#if defined TECUTILOMODULE
#define EXTERN
#else
#define EXTERN extern
#endif

/* CORE SOURCE CODE REMOVED */

#if !defined SV_PROBEATFIELDPOSITIONRESULT
#define SV_PROBEATFIELDPOSITIONRESULT "PROBEATFIELDPOSITIONRESULT"
#endif

/*{{<windows_only> 
   TecUtilInterfaceWinAddPreMsgFn             PreTranslateMessage_pf 
   TecEngWinPrintImageRegisterCallback        WinPrintImageCallback_pf
   TecEngWinPrinterGetContextRegisterCallback WinPrinterGetContextCallback_pf
</windows_only> }}*/

#if defined MSWIN  || defined DOCUMENT_BUT_DONT_COMPILE
/* Add new Windows-only functions here */
/**
 * Add a PreTranslateMessage function to Tecplot's message loop so that
 * modeless dialogs messages can be dispatched correctly. See the ADK User's
 * Manual for information on using TecUtilInterfaceWinAddPreMsgFn(). This
 * function is only available under Windows.
 *
 * @param PreTranslateMessageProc
 *   Function to call to pretranslate messages. The PreTranslateMessage
 *   function you provide will have the following syntax: Boolean_t
 *   PreTranslateMessage(MSG *pMsg)Where pMsg is the message to preprocess
 *
 * @return
 *   TRUE if successfully installed, FALSE otherwise.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Install a PreTranslateMessage function for modeless dialogs for an add-on
 *   written in MFC:
 *
 * @code
 *   Boolean_t PreTranslateMessage(MSG *pMsg)
 *   {
 *     AFX_MANAGE_STATE(AfxGetStaticModuleState());
 *     return AfxGetApp()->PreTranslateMessage(pMsg);
 *   }
 *   .
 *   .
 *   .
 *
 *     // In InitTecAddon Function
 *     TecUtilInterfaceWinAddPreMsgFn(PreTranslateMessage);
 * @endcode
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc, exclude_tcl, windows_only
 */
LINKTOADDON Boolean_t STDCALL TecUtilInterfaceWinAddPreMsgFn(PreTranslateMessage_pf PreTranslateMessageProc);
#endif

/* The following functions should be excluded from all derived libraries if gencode is run with -E */

/*{{<exclude_engine>
    TecUtilMenuAddOption
    TecUtilMenuAddSeparator
    TecUtilMenuSetSensitivity
    TecUtilDialogGetFolderName
    TecUtilDialogGetFolderNameX
    TecUtilDialogGetFileName
    TecUtilDialogGetFileNames
    TecUtilDialogGetFileNamesX
</exclude_engine> }}*/

/* The engine functions are excluded from all derived libraries (libtec and fglue) */

/*{{<exclude_all>
    TecGUIMFCSidebarRegister
    TecEngPageCreateNew
    TecEngPageCreateRegisterCallback
    TecEngPageDestroyRegisterCallback
    TecEngPageNewCurrentRegisterCallback
    TecEngPolarCacheGetInnerGridRadiusAndDelta
    TecAppPolarCacheGetInnerGridRadiusAndDelta
    TecEngInit
    TecAppGetAddonHelpAbout
    TecAppUpdateDefaultGeom
    TecEngGetHelpAbout
    TecAppGetHelpAbout
    TecEngRenderDestRegisterCallback RenderDestCallback_pf
    TecEngRenderQueryRegisterCallback RenderQueryCallback_pf
    TecEngRenderDestSizeRegisterCallback RenderDestSizeCallback_pf
    TecEngSwapBuffersRegisterCallback SwapBuffersCallback_pf
    TecEngKeyStateRegisterCallback KeyStateCallback_pf
    TecEngMouseButtonStateRegisterCallback MouseButtonStateCallback_pf
    TecEngWaitCursorStateRegisterCallback WaitCursorStateCallback_pf
    TecEngCursorStyleRegisterCallback BaseCursorStyleCallback_pf
    TecEngProcessBusyEventsRegisterCallback ProcessBusyEventsCallback_pf
    TecEngEngineNotBusyRegisterCallback EngineNotBusyCallback_pf
    TecEngDotPitchRegisterCallback DotPitchCallback_pf    
    TecEngScreenSizeRegisterCallback ScreenSizeCallback_pf
    TecEngDialogMessageBoxRegisterCallback DialogMessageBoxCallback_pf
    TecEngStartup
    TecEngCleanupOnAbnormalShutdown
    TecEngInitiateShutdown
    TecEngFinalizeShutdown
    TecEngProcessTasksOnIdle
    TecEngPageDamaged
    TecEngSetLicenseIsValid
    TecUtilParentLockStart
    TecUtilParentLockFinish
    TecEngOffscreenImageCreateRegisterCallback TecEngOffscreenImageCreate_pf
    TecEngOffscreenImageDestroyRegisterCallback TecEngOffscreenImageDestroy_pf
    TecEngRenderOffscreenImage
    TecEngOffscreenImageGetRGBRowRegisterCallback TecEngOffscreenImageGetRGBRow_pf
    TecEngWinPrintImageRegisterCallback WinPrintImageCallback_pf
    TecEngWinPrinterGetContextRegisterCallback WinPrinterGetContextCallback_pf
    TecEngStatusLineRegisterCallback TecEngStatusLineCallback_pf
    TecEngPercentDoneRegisterCallback TecEngPercentDoneCallback_pf
    TecEngMRULayoutFilenameRegisterCallback TecEngMRULayoutFilenameCallback_pf
    TecEngOpenGLVersionStringRegisterCallback TecEngOpenGLVersionStringCallback_pf


    TecGUICreateDialogBar
    TecGUIGetTPMainWnd

    TecAppImportGetNumRegisteredForeignLoaders
    TecAppImportGetForeignLoaderName
    TecAppImportGetForeignLoaderGUILaunchCallback DynamicMenuCallback_pf
    TecAppPickHandlesDisableDrawing
    TecAppAnyActiveSliceIsClipPlane
    TecAppLoadEquationFile
    TecAppWriteEquationFile

    TecAppDiagnosticMacroRecordStart
    TecAppDiagnosticMacroRecordEnd
    TecAppDiagnosticMacroRecordIsEnabled
 </exclude_all> }}*/

/*{{<exclude_python>
                    TecUtilAddOnRegisterInfo
                    TecUtilDataSetAddPostConvInstr
                    TecUtilDataSetDefVarLoadFinish
                    TecUtilDataValueCanMemMapData
                    TecUtilDataValueMemMapData
                    TecUtilDispatchWorkAreaEvent
                    TecUtilGeomGetXYZAnchorPos
                    TecUtilGeomSetXYZAnchorPos
                    TecUtilProbeXYGetDepValue
                    TecUtilProbeXYGetIndValue
                    TecUtilProbeXYGetSourceMap
                    TecUtilSidebarAutoSensitivity
                    TecUtilSidebarSetSensitivity
                    TecUtiltDataSetDefVarLoadFinish
                    TecUtilTextGetXYPos
                    TecUtilTextSetXYPos
                    TecUtilThreadCreateDetached ThreadFunction_pf
                    TecUtilThreadMutexAlloc
                    TecUtilThreadRecursiveMutexAlloc
                    TecUtilThreadMutexDealloc
                    TecUtilThreadMutexLock
                    TecUtilThreadMutexUnlock
                    TecUtilThreadConditionAlloc
                    TecUtilThreadConditionDealloc
                    TecUtilThreadSignalCondition
                    TecUtilThreadBroadcastCondition
                    TecUtilThreadWaitForCondition
                    TecUtilThreadTimedWaitForCondition
                    TecUtilThreadPoolJobControlAlloc
                    TecUtilThreadPoolJobControlDealloc
                    TecUtilThreadPoolAddJob ThreadPoolJob_pf
                    TecUtilThreadPoolPoolSize
                    TecUtilThreadPoolJobThreadOffset
                    TecUtilThreadPoolWait
                    TecUtilThreadPoolGetNumConcurrentJobs
                    TecUtilDataSetGetSZLRegistration
</exclude_python>}}*/

/*{{<exclude_fglue>
                    TecUtilDataFEPolyGetCellNodesSizeAndCenter
                    TecUtilTimerAddCallback AddOnTimerCallback_pf
                    TecUtilDataSetAddVar
                    TecUtilStringAlloc
                    TecUtilStringDealloc
                    TecUtilStringListGetRawStringPtr
                    TecUtilDataSetAddZone
                    TecUtilStateChangeAddCallback
                    TecUtilStateChangeSetMode
                    StateChangeAddOnCallback_pf
                    StateChangeAddOnCallbackV2_pf
                    TecUtilCheckActiveAllocs
                    TecUtilStateChangeGetName
                    TecUtilThreadCreateDetached ThreadFunction_pf
                    TecUtilThreadMutexAlloc
                    TecUtilThreadRecursiveMutexAlloc
                    TecUtilThreadMutexDealloc
                    TecUtilThreadMutexLock
                    TecUtilThreadMutexUnlock
                    TecUtilThreadConditionAlloc
                    TecUtilThreadConditionDealloc
                    TecUtilThreadSignalCondition
                    TecUtilThreadBroadcastCondition
                    TecUtilThreadWaitForCondition
                    TecUtilThreadTimedWaitForCondition
                    TecUtilThreadPoolJobControlAlloc
                    TecUtilThreadPoolJobControlDealloc
                    TecUtilThreadPoolAddJob ThreadPoolJob_pf
                    TecUtilThreadPoolPoolSize
                    TecUtilThreadPoolJobThreadOffset
                    TecUtilThreadPoolWait
                    TecUtilThreadPoolGetNumConcurrentJobs
                    TecUtilDataSetGetSZLRegistration
  </exclude_fglue> }}*/

/*{{<exclude_tcl>
                    TecUtilStringAlloc
                    TecUtilStringDealloc
                    TecUtilInterfaceWinAddPreMsgFn
                    TecUtilTimerAddCallback
                    TecUtilOnIdleQueueAddCallback
                    TecUtilOnIdleQueueRemoveCallback
                    TecUtilProbeInstallCallback
                    TecUtilExtractInstallCallback
                    TecUtilQuitAddQueryCallback
                    TecUtilEventAddPreDrawCallback
                    TecUtilEventAddPostDrawCallback
                    TecUtilStateChangeSetMode
                    TecUtilStateChangeAddCallback
                    TecUtilMacroAddCommandCallback
                    TecUtilConvAddPostReadCallback
                    TecUtilMenuAddOption
                    TecUtilMenuInsertOption
                    TecUtilMenuInsertToggle
                    TecUtilMenuRegisterSensitivityCallback
                    TecUtilMenuAddToolTip
                    TecUtilDialogGetFolderName
                    TecUtilDialogGetFolderNameX
                    TecUtilDialogGetFileName
                    TecUtilDialogGetFileNames
                    TecUtilDialogGetFileNamesX
                    TecUtilImportAddConverter
                    TecUtilImportAddLoader
                    TecUtilAddOnRegForeignLibLoader
                    TecUtilDynamicLabelRegisterCallback
                    TecUtilCurveRegisterExtCrvFit
                    TecUtilDataValueCustomLOD
                    TecUtilStringListSort
                    TecUtilReadBinaryData
                    TecUtilGeomImageGetImage
                    TecUtilThreadCreateDetached ThreadFunction_pf
                    TecUtilThreadMutexAlloc
                    TecUtilThreadRecursiveMutexAlloc
                    TecUtilThreadMutexDealloc
                    TecUtilThreadMutexLock
                    TecUtilThreadMutexUnlock
                    TecUtilThreadConditionAlloc
                    TecUtilThreadConditionDealloc
                    TecUtilThreadSignalCondition
                    TecUtilThreadBroadcastCondition
                    TecUtilThreadWaitForCondition
                    TecUtilThreadTimedWaitForCondition
                    TecUtilThreadPoolJobControlAlloc
                    TecUtilThreadPoolJobControlDealloc
                    TecUtilThreadPoolAddJob ThreadPoolJob_pf
                    TecUtilThreadPoolPoolSize
                    TecUtilThreadPoolJobThreadOffset
                    TecUtilThreadPoolWait
                    TecUtilThreadPoolGetNumConcurrentJobs
                    TecUtilDataSetGetSZLRegistration
  </exclude_tcl> }}*/

/**
 * Adds a timer callback, a function which is called by Tecplot at a set
 * interval of time.
 *
 * @par Note:
 *   In your timer callback you must check to see if Tecplot is locked before
 *   calling any TecUtil functions, as illustrated in the source code example.
 *
 * @param Interval
 *   Timeout interval in milliseconds.
 *
 * @param ClientData
 *   This can be any 32-bit value and will be passed to the timer callback.
 *   Typically this is a pointer to a structure
 *
 * @param TimerCallback
 *   Function to be called at each timeout interval. This function should
 *   return TRUE to continue the timer, or FALSE to stop it.
 *
 * @return
 *   TRUE if the callback was successfully added, FALSE otherwise. This
 *   function returns FALSE only if Tecplot is unable to obtain a timer from
 *   the operating system.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTimerAddCallback(
 *   &                   Interval,
 *   &                   ClientDataPtr,
 *   &                   TimerCallback)
 *    INTEGER*4          Interval
 *    POINTER            (ClientDataPtr, ClientData)
 *    EXTERNAL           TimerCallback
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set up a timer with an interval of one second.
 *
 * @code
 *   static Boolean_t STDCALL MyTimerCallback(ArbParam_t MyData)
 *   {
 *     if (!TecUtilLockIsOn())
 *       {
 *         // Tecplot isn't currently locked, so it's safe to call TecUtil functions
 *         TecUtilLockStart(AddOnID); // Lock Tecplot for ourselves
 *
 *         // ... Do processing with TecUtil functions
 *
 *         // Release Tecplot for other addons
 *         TecUtilLockFinish(AddOnID);
 *       }
 *     else
 *       {
 *         // Another add-on has locked Tecplot. It is NOT safe to
 *         // call any TecUtil functions, so do nothing.
 *       }
 *
 *     // Return TRUE to continue the timer, return FALSE to stop the timer.
 *     return TRUE;
 *   }
 *   // Make the above function called every second.
 *   TecUtilTimerAddCallback(1000,NULL,MyTimerCallback);
 * @endcode
 *
 * @par Note:
 *   For a complete example of an add-on which uses timers, see the timetest
 *   sample add-on.
 *
 * @sa TecUtilOnIdleQueueAddCallback()
 *
 * @ingroup Utilities
 *
 * #internalattributes exclude_fglue, exclude_sdkdoc, exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilTimerAddCallback(UInt32_t               Interval,
                                                      ArbParam_t             ClientData,
                                                      AddOnTimerCallback_pf  TimerCallback);

/**
 * Adds the function to a list of functions that Tecplot calls only one time
 * when Tecplot is in an idle state. This is particularly important for any
 * add-on that needs to perform potentially disruptive Tecplot operations (such
 * as modifying data) in response to a particular state change.  FORTRAN addons
 * can only register one on-idle callback at a time.   All other addons can
 * register multiple on-idle callbacks.
 *
 * @param Callback
 *   Function to be called only one time when Tecplot becomes idle (and is not
 *   locked). After the call, the function is removed from the queue. No attempt
 *   is made to ensure that only one instance of Callback is registered for a given
 *   task. It is up to the add-on to keep track of registration (see the use of the
 *   PendingMyUpdateFuncOnIdle variable in the example illustrated below).
 *
 * @param ClientData
 *   This can be any value and will be passed to the on-idle callback
 *   when the on idle callback is invoked. Typically this is a pointer to a
 *   structure needed by the callback function.
 *
 * @return
 *   TRUE if the callback was successfully added, FALSE otherwise.
 *
 * <FortranSyntax>
 * C
 * C  Note: Only one on-idle callback function can active for each FORTRAN add-on.
 * C  Registering another on-idle callback function overwrites the first.
 * C  You may use ClientData to differentiate between different on-idle situations.
 * C
 *    INTEGER*4 FUNCTION TecUtilOnIdleQueueAddCallback(
 *   &                   Callback,
 *   &                   ClientDataPtr)
 *    EXTERNAL           Callback
 *    POINTER            (ClientDataPtr, ClientData)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *
 *   // Make sure only one on-idle function is registered at a time.
 *   static Boolean_t PendingMyUpdateFuncOnIdle = FALSE;
 *
 *   static void STDCALL MyUpdateFuncOnIdle(ArbParam_t ClientData)
 *   {
 *     // ...do some potentially disruptive work in Tecplot
 *
 *     // clear the flag so we can register another update if needed
 *     PendingMyUpdateFuncOnIdle = FALSE;
 *   }
 *
 *   // A state change monitor usually used to update dialogs and
 *   // other add-on state when Tecplot (or another add-on) changes
 *   // the Tecplot's current state
 *
 *   void STDCALL MyStateChangeMonitor(StateChange_e StateChange,
 *                                     ArbParam_t    CallData)
 *   {
 *     if (StateChange == StateChange_VarsAltered)
 *       {
 *         // Addon needs to do something that is disruptive so we
 *         // need to register an on-idle callback to do the work
 *         // when Tecplot is idle.
 *
 *         // if one is not already registered ...
 *         if (!PendingMyUpdateFuncOnIdle)
 *           {
 *             // keep track of our registration
 *             PendingMyUpdateFuncOnIdle = TRUE;
 *             // Queue the callback
 *             TecUtilOnIdleQueueAddCallback(MyUpdateFuncOnIdle,
 *                                           (ArbParam_t)MyData);
 *           }
 *       }
 *   }
 * @endcode
 *
 * @ingroup Utilities
 *
 * #internalattributes exclude_sdkdoc, exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilOnIdleQueueAddCallback(OnIdleCallback_pf Callback,
                                                            ArbParam_t        ClientData);

/**
 * Remove the oldest registered callback/client data pair from the on-idle queue if it exists.
 *
 * @param Callback
 *   Registered callback function.
 *
 * @param ClientData
 *   Client data registered with the function.
 *
 * @return
 *   TRUE if the callback/client data pair was found and removed, FALSE if it wasn't found.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 * #internalattributes exclude_sdkdoc, exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilOnIdleQueueRemoveCallback(OnIdleCallback_pf Callback,
                                                               ArbParam_t        ClientData);


/**
 * Download a file given a valid URL.
 *
 * @param SourceURL
 *   A string representing the URL for the file to download. Must be valid URL syntax
 *
 * @param LocalDestinationFile
 *   A string representing a local filename where the data is to be stored
 *
 * @param IsAscii
 *   Set to TRUE if the file is to be treated as an ASCII file during the
 *   download. Set to FALSE if it is to be treated as a Binary file during the
 *   download
 *
 * @param ConfirmOverwrite
 *   Set to TRUE if you want the user to be prompted with a message and options
 *   if LocalDestinationFile exists. The user can then choose to cancel the
 *   operation. If set to FALSE and LocalDestinationFile exists it will be
 *   overwritten with no warning
 *
 *
 * @pre <em>SourceURL</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>LocalDestinationFile</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFileDownloadURL(
 *   &                   SourceURL,
 *   &                   LocalDestinationFile,
 *   &                   IsAscii,
 *   &                   ConfirmOverwrite)
 *    CHARACTER*(*)   SourceURL
 *    CHARACTER*(*)   LocalDestinationFile
 *    INTEGER*4       IsAscii
 *    INTEGER*4       ConfirmOverwrite
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilFileDownloadURL(const char  *SourceURL,
                                                     const char  *LocalDestinationFile,
                                                     Boolean_t    IsAscii,
                                                     Boolean_t    ConfirmOverwrite);
/**
 *   Upload a file given a valid URL.
 *
 * @param LocalSourceFile
 *   A string representing the local file to upload
 *
 * @param DestinationURL
 *   A string representing the remote filename where the data is to be stored. Must be valid URL
 *   syntax,
 *
 * @param IsAscii
 *   Set to TRUE if the file is to be treated as an ASCII file during the upload. Set to FALSE if it is
 *   to be treated as a Binary file during the upload
 *
 * @param ConfirmOverwrite
 *   Set to TRUE if you want the user to be prompted with a message and options if exists. The user can
 *   then choose to cancel the operation. If set to FALSE and DestinationURL exists it will be
 *   overwritten with no warning
 *
 *
 * @pre <em>LocalSourceFile</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>DestinationURL</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFileUploadURL(
 *   &                   LocalSourceFile,
 *   &                   DestinationURL,
 *   &                   IsAscii,
 *   &                   ConfirmOverwrite)
 *    CHARACTER*(*)   LocalSourceFile
 *    CHARACTER*(*)   DestinationURL
 *    INTEGER*4       IsAscii
 *    INTEGER*4       ConfirmOverwrite
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilFileUploadURL(const char *LocalSourceFile,
                                                   const char *DestinationURL,
                                                   Boolean_t   IsAscii,
                                                   Boolean_t   ConfirmOverwrite);
/**
 * Convenience function that will determine if a supplied string uses valid URL
 * syntax.
 *
 * @param *URLFName
 *   A string containing the URL to test
 *
 * @return
 *   Returns TRUE if URL is valid, FALSE otherwise
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFileIsURL(URLFName)
 *    CHARACTER*(*) URLFName
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 */
LINKTOADDON Boolean_t STDCALL TecUtilFileIsURL(const char *URLFName);

/**
 * Deallocates an array that was allocated and given to an add-on by Tecplot and
 * assigns the pointer to NULL. This function may not be used for deallocating
 * memory allocated by any other facility (such as the add-on itself).
 *
 * @param Array
 *   Pointer to the Array to be deallocated.
 *
 *
 * @pre <em>Array</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Array</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilArrayDealloc(Array)
 *    POINTER (ArrayPtr, Array)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa See TecGUIListGetSelectedItems() for a complete example.
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON void STDCALL TecUtilArrayDealloc(TP_RECEIVES_GIVES void** Array);

/**
 * Get Tecplot's current mouse mode.
 *
 * @return
 *   Returns the current mode.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMouseGetCurrentMode()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Mouse
 *
 */
LINKTOADDON MouseButtonMode_e STDCALL TecUtilMouseGetCurrentMode(void);

/**
 * This function will tell you if the specified mouse mode is valid for the
 * current state of Tecplot.
 *
 * @param MouseMode
 *   Mouse mode to check.
 *
 * @return
 *   Returns TRUE if the specified mode is appropriate, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMouseIsValidMode(MouseMode)
 *    INTEGER*4 MouseMode
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Mouse
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilMouseIsValidMode(MouseButtonMode_e MouseMode);

/**
 * Sets Tecplot's mouse mode to the one specified. This service request behaves
 * exactly as if the user had selected the mouse mode via Tecplot's user
 * interface.
 *
 * @param MouseMode
 *   Desired mouse mode.
 *
 * @return
 *    TRUE if the requested mouse mode was set, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMouseSetMode(MouseMode)
 *    INTEGER*4 MouseMode
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Mouse
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilMouseSetMode(MouseButtonMode_e MouseMode);




/**
 * @deprecated
 *   exclude_python;exclude_sdkdoc
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilSidebarAutoSensitivity(Boolean_t DoAuto);



/**
 * @deprecated
 *   Please use TecUtilMouseSetMode() and TecUtilMouseIsValidMode() instead.
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilSidebarSetSensitivity(MouseButtonMode_e MouseMode,
                                                      Boolean_t         IsSensitive);

/**
 * Repeats the last probe event using the probe coordinates from that event.
 * This is useful if the contents of the dataset has changed since the last
 * probe requiring a new probe event.
 *
 * @return
 *     TRUE if the probe succeeded, FALSE otherwise
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * @sa
 *   TecUtilProbeInstallCallback, TecUtilProbeInstallCallbackX
 *
 * @since 12.2-1-10240
 * @ingroup Probe
 */
LINKTOADDON Boolean_t STDCALL TecUtilProbeRepeatLastEvent(void);

/**
 * Use this function in concert with TecUtilProbeInstallCallback and
 * TecUtilProbeInstallCallbackX.  Calling this function will call the
 * most recently installed probe callback.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid or NULL.
 *
 *
 * <ArgListTable>
 *
 * Name:
 *   SV_X
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   X-position at which to probe.
 *
 * Name:
 *   SV_Y
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   Y-position at which to probe.
 *
 * Name:
 *   SV_Z
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   No
 * Default:
 *   0.0
 * Notes:
 *   The Z-position at which to probe. Only required if plot type is 3D.
 *
 * Name:
 *   SV_ZONESET
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Required:
 *   No
 * Default:
 *   NULL
 * Notes:
 *   The set of zones to which to limit the search. If not supplied
 *   all zones will be searched.
 *
 * Name:
 *   SV_SEARCHVOLUME
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE if the XYZ coordinates represent a data point inside of a 3-D
 *   volume zone. Set to FALSE to use the XY coordinates only which will return
 *   a point on the surface of a zone.  Ignored when plot type is 2D Cartesian
 *
 * Name:
 *  SV_PROBEATFIELDPOSITIONRESULT
 * Type:
 *   ArbParam_t
 * Arg Function:
 *   TecUtilArgListAppendArbParam()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   Pass the address of a Boolean_t variable which will receive the
 *   result of the 3D probe. The result will be TRUE if
 *   probe succeeded, FALSE otherwise.
 *
 * </ArgListTable>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *   // Install the probe callback
 *   ArgList_pa argList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendFunction(argList, SV_CALLBACKFUNCTION, MyProbeCallback);
 *   TecUtilArgListAppendString(argList, SV_STATUSLINETEXT, "My probe information text");
 *   TecUtilArgListAppendArbParam(argList, SV_CLIENTDATA, 0);
 *   TecUtilProbeInstallCallbackX(argList);
 *
 *   // Clear the argument list to ready it for TecUtilProbeAtFieldPositionX
 *   TecUtilArgListClear(argList);
 *
 *   // Do the probe
 *   TecUtilArgListAppendDouble(argList, SV_X, 0.5);
 *   TecUtilArgListAppendDouble(argList, SV_Y, 0.2);
 *   TecUtilProbeAtFieldPositionX(argList);
 *   TecUtilArgListDealloc(&argList);
 *
 *   // This function is called by TecUtilProbeAtFieldPositionX
 *   void STDCALL MyProbeCallback(Boolean_t  wasSuccessful,
 *                                Boolean_t  isNearestPoint,
 *                                ArbParam_t clientData)
 *   {
 *     TecUtilLockStart(AddOnID);
 *     if ( wasSuccessful )
 *       {
 *         ...
 *         Call TecUtilProbeXXXGetXXX functions to extract probe information
 *         ...
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @sa
 *   TecUtilProbeInstallCallback, TecUtilProbeInstallCallbackX
 *
 * @ingroup Probe
 */
LINKTOADDON void STDCALL TecUtilProbeAtFieldPositionX(ArgList_pa ArgList);


/**
 * Use this function in concert with TecUtilProbeInstallCallback and
 * TecUtilProbeInstallCallbackX.  Calling this function will call the
 * most recently installed probe callback.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * <ArgListTable>
 *
 * Name:
 *   SV_I
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   I-Index at which to probe.
 *
 * Name:
 *   SV_J
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   J-Index at which to probe.
 *
 * Name:
 *   SV_K
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   K-Index at which to probe.
 *
 * Name:
 *   SV_ZONE
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The zone for this to perform the probe
 *
 * </ArgListTable>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa
 *   TecUtilProbeInstallCallback, TecUtilProbeInstallCallbackX, TecUtilProbeAtFieldPositionX
 *
 * @ingroup Probe
 */
LINKTOADDON void STDCALL TecUtilProbeAtFieldIndexX(ArgList_pa ArgList);

/**
 * Use this function in concert with TecUtilProbeInstallCallback and
 * TecUtilProbeInstallCallbackX.  Calling this function will call the
 * most recently installed probe callback.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * <ArgListTable>
 *
 * Name:
 *   SV_POSITION
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   Position along the line at which to probe
 *
 * Name:
 *   SV_AXIS
 * Type:
 *   char
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   'X' or 'Y' for XY-Line plots
 *   'T' or 'R' for Polar-Line plots
 *
 * Name:
 *   SV_AXISNUM
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   If the plot type is PlotType_XYLine there are up to 5 X- and Y- axes.
 *   This parameter specifies the axis index on which to probe.
 *
 * </ArgListTable>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa
 *   TecUtilProbeInstallCallback, TecUtilProbeInstallCallbackX, TecUtilProbeAtFieldPositionX
 *
 * @ingroup Probe
 */
LINKTOADDON void STDCALL TecUtilProbeAtLinePositionX(ArgList_pa ArgList);

/**
 * Use this function in concert with TecUtilProbeInstallCallback and
 * TecUtilProbeInstallCallbackX.  Calling this function will call the
 * most recently installed probe callback.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * <ArgListTable>
 *
 * Name:
 *   SV_I
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   I-Index at which to probe.
 *
 * Name:
 *   SV_J
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   J-Index at which to probe.
 *
 * Name:
 *   SV_K
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   K-Index at which to probe.
 *
 * Name:
 *   SV_LINEMAP
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The line-map on which to probe
 *
 * </ArgListTable>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa
 *   TecUtilProbeInstallCallback, TecUtilProbeInstallCallbackX, TecUtilProbeAtFieldIndexX
 *
 * @ingroup Probe
 */
LINKTOADDON void STDCALL TecUtilProbeAtLineIndexX(ArgList_pa ArgList);


/**
 * Change the mouse mode to be the probe tool, and instruct Tecplot to call a different
 * function when the user completes a probe operation in the work area. This function
 * callback will remain in effect until the mouse mode is changed. In order to call this
 * function, the current plot type must not be Sketch.
 *
 * Note that by default, probes on COBs (iso-surfaces, slices, streamtraces, and
 * so forth) will not trigger a callback. To obtain this behavior, call
 * TecUtilProbeAllowCOBs() after installing the callback.
 *
 * @param ProbeDestination
 *   Function to call when the probe event takes place.
 *
 * @param InformationLineText
 *   Text to write on the status line when the override is in effect.
 *
 * @return
 *   TRUE if successfully installed.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>InformationLineText</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeInstallCallback(
 *   &                   ProbeDestination,
 *   &                   InformationLineText)
 *    EXTERNAL        ProbeDestination
 *    CHARACTER*(*)   InformationLineText
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Override the behavior of the probe tool in Tecplot. When a probe occurs,
 * just print out the results of the probe to simulate some of the capabilities
 * of the probe dialogs in Tecplot.  The task is to provide a mechanism whereby
 * your own probe callback overrides the default behavior when a probe event
 * occurs in Tecplot. One way to do this is to add an option to the Tools menu
 * in Tecplot. The callback function assigned to the menu option would then
 * turn around and install the probe callback with the following code: .
 *
 * @code
 *   .
 *   TecUtilProbeInstallCallback(MyProbeCallback,
 *                       "Click to print out my probe information");
 *   .
 *   TecUtilProbeAllowCOBs()
 *   .
 *
 *   The function MyProbeCallback is then defined as:
 *   void STDCALL MyProbeCallback(Boolean_t IsNearestPoint)
 *   {
 *     FrameMode_e FrameMode;
 *     TecUtilLockStart(AddOnID);
 *     FrameMode = TecUtilFrameGetMode();
 *
 *     if (FrameMode == Frame_XY)
 *       {
 *         printf("XY Probe, Independent value is: %G\n",
 *                TecUtilProbeLinePlotGetIndValue());
 *         if (IsNearestPoint)
 *           {
 *             double DepValue;
 *             TecUtilProbeXYGetDepValue(1,&DepValue);
 *             printf("Nearest Point:  Dependent Value = %G\n",
 *                    DepValue);
 *             printf("                Source LineMap    = %d\n",
 *                    TecUtilProbeLinePlotGetMap());
 *             printf("                Point Index     = %d\n",
 *                    TecUtilProbeGetPointIndex());
 *           }
 *         else
 *           {
 *             EntIndex_t M;
 *             for (M = 1; M <= TecUtilLineMapGetCount(); M++)
 *               {
 *                 double DepValue;
 *                 printf("LineMap = %02d, Dependent Value = ",M);
 *                 if (TecUtilProbeLinePlotGetDepValue(M,&DepValue))
 *                   printf("%G\n",DepValue);
 *                 else
 *                   printf("Unknown\n");
 *               }
 *           }
 *       }
 *     else
 *       {
 *         EntIndex_t  SourceZone = TecUtilProbeFieldGetZone();
 *         ZoneType_e  ZoneType   = TecUtilZoneGetType(SourceZone);
 *         IJKPlanes_e Plane      = TecUtilProbeFieldGetPlane();
 *         EntIndex_t  V;
 *         EntIndex_t  NumVars;
 *
 *         TecUtilDataSetGetInfo((char **)NULL,
 *                               (EntIndex_t *)NULL,
 *                               &NumVars);
 *
 *         if (IsNearestPoint)
 *           printf("Nearest point probe:\n");
 *         else
 *          {
 *            LgIndex_t ProbeCell = TecUtilProbeFieldGetCell();
 *            printf("Interpolated  probe:(Cell = %d)\n",ProbeCell);
 *          }
 *
 *         for (V = 1; V <= NumVars; V++)
 *           {
 *             char *VName;
 *             if (TecUtilVarGetName(V,&VName))
 *               {
 *                 printf("%20s : ",VName);
 *                 TecUtilStringDealloc(&VName);
 *               }
 *             else
 *               printf("<Unknown>            : ");
 *             printf("%G\n",TecUtilProbeFieldGetValue(V));
 *           }
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 *   A complete example can be found in the probtest sample add-on included in the distribution.
 * @endcode
 *
 * @ingroup Probe
 *
 * @sa
 *   TecUtilProbeInstallCallbackX, TecUtilProbeAllowCOBs
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilProbeInstallCallback(ProbeDestination_pf ProbeDestination,
                                                          const char         *InformationLineText);

/**
 * Change the mouse mode to be the probe tool, and instruct Tecplot to call a different
 * function when the user completes a probe operation in the work area. This function
 * callback will remain in effect until the mouse mode is changed. In order to call this
 * function, the current plot type must not be Sketch.
 *
 * Note that, unlike TecUtilProbeInstallCallback, installing a callback via this
 * function will always allows probes on COBs (iso-surfaces, slices, streamtraces, and
 * so forth). So TecUtilProbeAllowCOBs need not be called afterwards.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 *
 * <ArgListTable>
 *
 * Name:
 *   SV_CALLBACKFUNCTION
 * Type:
 *   ProbeDestinationX_pf
 * Arg Function:
 *   TecUtilArgListAppendFunction()
 * Required:
 *   Yes
 * Notes:
 *   Function to call when the probe event takes place.
 *
 * Name:
 *   SV_STATUSLINETEXT
 * Type:
 *   char*
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   No
 * Default:
 *   NULL
 * Notes:
 *   Text to write on the status line when the override is in effect.
 *
 * Name:
 *   SV_CLIENTDATA
 * Type:
 *   ArbParam_t
 * Arg Function:
 *   TecUtilArgListAppendArbParam()
 * Required:
 *   No
 * Default:
 *   0
 * Notes:
 *   Client data to be sent to the callback as a parameter.
 *
 * </ArgListTable>
 *
 * @return
 *   TRUE if successfully installed.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeInstallCallback(
 *   &                   ProbeDestination,
 *   &                   InformationLineText)
 *    EXTERNAL        ProbeDestination
 *    CHARACTER*(*)   InformationLineText
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 * @sa
 *   TecUtilProbeInstallCallback
 */
LINKTOADDON Boolean_t STDCALL TecUtilProbeInstallCallbackX(ArgList_pa ArgList);

/**
 * Unregister an installed probe callback. For addons this will revert 360 back to
 * using the default probe callback. Parent applications may also want to use this 
 * to put the engine back into a known state where no probe callback is registered.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilUninstallProbeCallback()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 * @since 14.1
 *
 * @sa
 *   TecUtilProbeInstallCallbackX
 */
LINKTOADDON void STDCALL TecUtilUninstallProbeCallback(void);

/**
 * Instructs Tecplot to include COBs (iso-surfaces, slices, streamtraces, and
 * so forth) along with zones during a probe when an add-on has a callback
 * registered with TecUtilProbeInstallCallback() and if the user is pressing
 * the Alt key. By default the Alt key is ignored for an add-on's installed
 * probe callback. This function should be called after TecUtilProbeInstallCallback().
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilProbeAllowCOBs()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON void STDCALL TecUtilProbeAllowCOBs(void);



/**
 * @deprecated
 *   Please use TecUtilProbeLinePlotGetIndValue() instead.
 *
 * @ingroup Pick
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON double STDCALL TecUtilProbeXYGetIndValue(void);



/**
 * Call this function from a probe destination callback to get the independent
 * value from the previous probe event in an Line-plot.
 *
 * @return
 *   The independent value from the previous probe event.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilProbeLinePlotGetIndValue()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON double STDCALL TecUtilProbeLinePlotGetIndValue(void);


/**
 * @deprecated
 *   Please use TecUtilProbeLinePlotGetDepValue() instead.
 *
 * @ingroup Pick
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilProbeXYGetDepValue(EntIndex_t     MapNum,
                                                        TP_OUT double* DepValue);
/**
 * Call this function from a probe destination callback to get a dependent
 * value from the previous probe event in an Line-plot.
 *
 * @param MapNum
 *   Number of the Line-map to query for its dependent value at the previous
 *   probe.
 *
 * @param DepValue
 *   Dependent value resulting from previous probe. This parameter is ignored
 *   if the previous probe was a nearest-point probe.
 *
 * @return
 *   TRUE if the dependent value for the selected Line-map number is valid,
 *   otherwise FALSE. A dependent value can be invalid if a probe occurs
 *   outside the range of the independent variable for a given Line-map.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>DepValue</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeLinePlotGetDepValue(
 *   &                   MapNum,
 *   &                   DepValue)
 *    INTEGER*4       MapNum
 *    REAL*8          DepValue
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilProbeLinePlotGetDepValue(EntIndex_t     MapNum,
                                                              TP_OUT double* DepValue);



/**
 * @deprecated
 *   Please use TecUtilProbeLinePlotGetSourceMap() instead.
 *
 * @ingroup Pick
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON EntIndex_t STDCALL TecUtilProbeXYGetSourceMap(void);



/**
 * Call this function from a probe destination callback to get the Line-map
 * whose point was selected in the previous nearest-point probe. In order to
 * use this function, the value passed to the probe destination callback must
 * have been TRUE, indicating a nearest-point probe.
 *
 * @return
 *   The number of the Line-map.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeLinePlotGetSourceMap()
 * </FortranSyntax>
 *
 * @par Note:
 *   IMPORTANT!  Note that the FORTRAN Name for this function is truncated
 *   to 31 characters!
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilProbeLinePlotGetSourceMap(void);

/**
 * Call this function from a probe destination callback to get the indepedent
 * axis kind associated with the line probe. Together with TecUtilProbeLinePlotGetIndAxisNumber,
 * this allows one to determine the axis associated with the value returned from
 * TecUtilProbeLinePlotGetDepValue.
 *
 * @return
 *   The axis kind, either 'X', 'Y', 'T' (theta), or 'R' (radial).
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeLinePlotGetIndAxisK()
 * </FortranSyntax>
 *
 * @par Note:
 *   IMPORTANT!  Note that the FORTRAN Name for this function is truncated
 *   to 31 characters!
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON char STDCALL TecUtilProbeLinePlotGetIndAxisKind(void);

/**
 * Call this function from a probe destination callback to get the indepedent
 * axis number associated with the line probe. Together with TecUtilProbeLinePlotGetIndAxisKind,
 * this allows one to determine the axis associated with the value returned from
 * TecUtilProbeLinePlotGetDepValue.
 *
 * @return
 *   The axis number. For Polar Line plots, this will always be one. For XY Line plots, this will
 *   be a number between MaxXAxes or MaxYAxes.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeLinePlotGetIndAxisN()
 * </FortranSyntax>
 *
 * @par Note:
 *   IMPORTANT!  Note that the FORTRAN Name for this function is truncated
 *   to 31 characters!
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON int STDCALL TecUtilProbeLinePlotGetIndAxisNumber(void);

/**
 * Call this function from a probe destination callback to get the point index
 * from the previous nearest-point probe event in a field plot or an XY-plot.
 * In order to use this function, the value passed to the probe destination
 * callback must have been TRUE, indicating a nearest-point probe.
 *
 * @return
 *   The index of the data point selected in the previous nearest point probe.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeGetPointIndex()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilProbeGetPointIndex(void);
/**
 * Returns the plot type of the last probe event.
 *
 * @since
 *   14.2
 *
 * @return
 *   The Plot Type can be one of the following possible values:
 *   \ref PlotType_Cartesian2D, \ref PlotType_Cartesian3D, \ref PlotType_XYLine
 *   or \ref PlotType_PolarLine.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeGetPlotType()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON PlotType_e STDCALL TecUtilProbeGetPlotType(void);
/**
 * Indicates type type of COB or zone that was selected during the probe event.
 *
 * @return
 *   Type of COB selected by the probe. This can be one of seven values:
 *   CZType_FieldDataZone CZType_FEBoundaryCOB CZType_IsoSurfaceCOB
 *   CZType_SliceCOB CZType_StreamtraceCOB CZType_StreamtraceMarkerCOB
 *   CZType_StreamtraceArrowheadCOB.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeFieldGetCZType()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON CZType_e STDCALL TecUtilProbeFieldGetCZType(void);
/**
 * Call this function from a probe destination callback to get a field variable
 * value from the previous probe event in a field plot.
 *
 * @param VarNum
 *   The variable number for which to get the field value.
 *
 * @return
 *   The value for variable VarNum at the previous probe data point.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilProbeFieldGetValue(VarNum)
 *    INTEGER*4 VarNum
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON double STDCALL TecUtilProbeFieldGetValue(EntIndex_t VarNum);
/**
 * Call this function from a probe destination callback to get the zone number
 * from the previous probe event in a field plot.
 *
 * @return
 *   The number of the zone which was probed.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeFieldGetZone()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON EntIndex_t STDCALL TecUtilProbeFieldGetZone(void);
/**
 * Call this function from a probe destination callback to get the I-, J-, or
 * K-plane from the previous probe event in a field plot.
 *
 * @return
 *   The plane which was probed. This can be one of IJKPlanes_I, IJKPlanes_J, or
 *   IJKPlanes_K.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeFieldGetPlane()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON IJKPlanes_e STDCALL TecUtilProbeFieldGetPlane(void);

/**
 * Call this function from a probe destination callback to get the face number
 * from the previous probe event in a field plot.
 *
 * @return
 *   The face number of the cell that was probed. For ordered volume and FE
 *   brick data the value is between 1 and 6. For ordered surface, FE
 *   tetra, and FE quandrangle data the value is between 1 and 4. For FE
 *   triangular data the value is between 1 and 3. For ordered or FE linear
 *   data the value is 0.
 *
 *   A return value of zero indicates that no face was probed. This may
 *   occur on nearest point probes.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeFieldGetFaceNumber()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON SmInteger_t STDCALL TecUtilProbeFieldGetFaceNumber(void);

/**
 * Call this function from a probe destination callback to get the cell from
 * the previous probe event in a field plot.
 *
 * @par Note:
 *   For all zone types other than ordered volume data this function returns
 *   the same information as TecUtilProbeFieldGetCell(). For ordered volume
 *   data the distinction is that this function returns the face cell whereas
 *   TecUtilProbeFieldGetCell() returns the volume cell.
 *
 * @return
 *   The index of the cell which was probed. For ordered data, this is
 *   equivalent to the index value of the node in the lowest indexed corner of
 *   the face cell. For finite-element data, this is the element number.
 *
 *   A return value of zero indicates that no face was probed. This may
 *   occur on nearest point probes.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeFieldGetFaceCell()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON LgIndex_t  STDCALL TecUtilProbeFieldGetFaceCell(void);

/**
 * Call this function from a probe destination callback to get the cell from
 * the previous probe event in a field plot.
 *
 * @par Note:
 *   For all zone types other than ordered volume data this function returns
 *   the same information as TecUtilProbeFieldGetFaceCell(). For ordered volume
 *   data the distinction is that this function returns the volume cell whereas
 *   TecUtilProbeFieldGetFaceCell() returns the face cell.
 *
 * @return
 *   The index of the cell which was probed. For ordered data, this is
 *   equivalent to the index value of the node in the lowest indexed corner of
 *   the cell. For finite-element data, this is the element number.
 *
 *   A return value of zero indicates that no cell was probed. This may
 *   occur on nearest point probes.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeFieldGetCell()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON LgIndex_t  STDCALL TecUtilProbeFieldGetCell(void);

/**
 * Call this function from a probe destination callback to get the name of the item
 * that was probed. This may return a zone name if a zone was probed, or a COB name
 * if TecUtilProbeAllowCOBs was called when the probe destination callback was installed.
 *
 * @param Name
 *   Receives the name of the last-probed item. You must free the returned string
 *   with TecUtilStringDealloc().
 *
 * @return
 *   TRUE if successful, FALSE if not. FALSE usually indicates the Name string
 *   was unable to be allocated.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>Name</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeFieldGetName(
 *   &                   Name,
 *   &                   NameLength)
 *    CHARACTER*(*)   ZName
 *    INTEGER*4       ZNameLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilProbeFieldGetName(TP_GIVES char** Name);

/**
 * Call this function from a probe destination callback to get a handle to the native data
 * from the probed item for the specified variable. Note that no attempt is made to make
 * the data readable; if data access is needed, use TecUtilProbeFieldGetReadableNativeRef
 * instead. This method can be used to query for information about the variable without
 * causing the data to be loaded.
 * 
 * This function is \ref threadsafe.
 *
 * @param Var
 *   Number of the variable for which to get the field data
 *
 * @return
 *   A field data handle to the native data for the probed item and specified variable.
 *
 * @since
 *   12.0-1-5558
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilProbeFieldGetNativeRef(
 *   &           Var,
 *   &           ResultPtr)
 *    INTEGER*4       Var
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON FieldData_pa STDCALL TecUtilProbeFieldGetNativeRef(EntIndex_t Var);

/**
 * Call this function from a probe destination callback to get a read-only handle to
 * the native data from the probed item for the specified variable.
 *
 * This function is \ref threadsafe.
 *
 * @param Var
 *   Number of the variable for which to get the field data
 *
 * @return
 *   A read-only field data handle to the native data for the probed item
 *   and specified variable, or NULL if Tecplot was not able to load the data.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilProbeFieldGetReadableNat(
 *   &           Var,
 *   &           ResultPtr)
 *    INTEGER*4       Var
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON FieldData_pa STDCALL TecUtilProbeFieldGetReadableNativeRef(EntIndex_t Var);

/**
 * Call this function from a probe destination callback to get a read-only handle to
 * the derived data from the probed item for the specified variable.
 *
 * This function is \ref threadsafe.
 *
 * @param Var
 *   Number of the variable for which to get the field data
 *
 * @return
 *   A read-only field data handle to the derived data for the probed item
 *   and specified variable, or NULL if Tecplot was not able to load the data.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilProbeFieldGetReadableDer(
 *   &           Var,
 *   &           ResultPtr)
 *    INTEGER*4       Var
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON FieldData_pa STDCALL TecUtilProbeFieldGetReadableDerivedRef(EntIndex_t Var);

/**
 * Call this function from a probe destination callback to get a read-only handle to
 * the node-located data from the probed item for the specified variable.
 *
 * This function is \ref threadsafe.
 *
 * @param Var
 *   Number of the variable for which to get the field data
 *
 * @return
 *   A read-only field data handle to the node-located data for the probed item
 *   and specified variable, or NULL if Tecplot was not able to load the data.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilProbeFieldGetReadableNLR(
 *   &           Var,
 *   &           ResultPtr)
 *    INTEGER*4       Var
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON FieldData_pa STDCALL TecUtilProbeFieldGetReadableNLRef(EntIndex_t Var);

/**
 * Call this function from a probe destination callback to get a read-only handle to
 * the cell-centered data from the probed item for the specified variable.
 *
 * This function is \ref threadsafe.
 *
 * @param Var
 *   Number of the variable for which to get the field data
 *
 * @return
 *   A read-only field data handle to the cell-centered data for the probed item
 *   and specified variable, or NULL if Tecplot was not able to load the data.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilProbeFieldGetReadableCCR(
 *   &           Var,
 *   &           ResultPtr)
 *    INTEGER*4       Var
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Probe
 *
 */
LINKTOADDON FieldData_pa STDCALL TecUtilProbeFieldGetReadableCCRef(EntIndex_t Var);

/**
 * If the current frame is 2D or 3D, change the mouse mode to be the extract
 * discrete points tool and instruct Tecplot to call a different function when
 * the user completes an extract-like operation in the work area. This function
 * callback will remain in effect until the user changes mouse modes in the
 * Tecplot interface.
 *
 * @param ExtractDestination
 *   Function to call when the extract event has been completed
 *
 * @param InformationLineText
 *   Text to write on the information line when the override is in effect
 *
 * @return
 *   TRUE if successfully installed.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>InformationLineText</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilExtractInstallCallback(
 *   &                   ExtractDestination,
 *   &                   InformationLineText)
 *    EXTERNAL        ExtractDestination
 *    CHARACTER*(*)   InformationLineText
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Change the mouse mode to be the Extract Discrete Points tool and install an extract callback
 *   function.
 *
 * @code
 *   void STDCALL MyExtractFunction(LgIndex_t NumPts,
 *                                  double   *XValues,
 *                                  double   *YValues)
 *   {
 *     // do something
 *   }
 *   .
 *   .
 *   .
 *     // elsewhere in the add-on
 *     TecUtilExtractInstallCallback(MyExtractFunction,
 *                             "Status line info for my function");
 * @endcode
 *
 * @ingroup DataCreate
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilExtractInstallCallback(ExtractDestination_pf ExtractDestination,
                                                            const char           *InformationLineText);




/**
 * Create a new data set and attach it to the current frame. This only
 * allocates space for a data set specification. You must immediately begin to
 * add zones to the data set by calling TecUtilDataSetAddZone() after creating
 * a data set.
 *
 * @param DataSetTitle
 *   Title for the current frame's data set. The name will be trimmed of any
 *   leading or trailing whitespace and truncated to be no more than 128
 *   characters in length.
 *
 * @param VarNames
 *   String list of variable names. See TecUtilStringListXXX functions for
 *   string list details
 *
 * @param ResetStyle
 *   Clears out all style information for the current frame before creating the
 *   data set. It is highly recommended that you always pass TRUE for this
 *   parameter.
 *
 * @return
 *   Returns TRUE if a data set could be allocated and attached.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>DataSetTitle</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetCreate(
 *   &                   DataSetTitle,
 *   &                   VarNamesPtr,
 *   &                   ResetStyle)
 *    CHARACTER*(*)   DataSetTitle
 *    POINTER         (VarNamesPtr, VarNames)
 *    INTEGER*4       ResetStyle
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a data set with two variables:
 *
 * @code
 *   StringList_pa sl = TecUtilStringListAlloc();
 *   TecUtilStringListAppendString(sl,"V1");  // first variable
 *   TecUtilStringListAppendString(sl,"V2");  // second variable
 *
 *   if ( TecUtilDataSetCreate("My Data Set",sl, TRUE));
 *      {
 *
 *      // Immediately call TecUtilDataSetAddZone() here
 *      }
 *   TecUtilStringListDealloc(&sl);
 * @endcode
 *
 * @ingroup DataCreate
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetCreate(const char    *DataSetTitle,
                                                   StringList_pa VarNames,
                                                   Boolean_t     ResetStyle);

/**
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>VALID_NON_ZERO_LEN_STR(LoaderName)</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>VALID_REF(DataSetTitle)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * @ingroup DataCreate
 *
 * #internalattributes exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilOEMDataSetCreate(const char    *LoaderName,
                                                      const char    *DataSetTitle,
                                                      StringList_pa VarNames,
                                                      Boolean_t     ResetStyle);


/**
 * Get the variable load mode for the current data set.
 *
 * This function is \ref threadsafe.
 *
 * @return
 *   The variable load mode. Possible values are VarLoadMode_ByName and VarLoadMode_ByPosition.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetGetVarLoadMode()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the variable load mode for the current data set:
 *
 * @code
 *   VarLoadMode_e VarLoadMode;
 *   if ( TecUtilDataSetIsAvailable() )
 *     {
 *        VarLoadMode = TecUtilDataSetGetVarLoadMode();
 *       ...
 *     }
 * @endcode
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON VarLoadMode_e STDCALL TecUtilDataSetGetVarLoadMode(void);

/**
 * Instruct Tecplot to either build or forgo building zone optimization
 * information. Zone optimization information enhances interactive performance
 * but has an upfront performance cost. This function can be called any time
 * after the zone has been created.
 *
 * @param Zone
 *   Zone for which the decision to build zone optimization information
 *   needs changing.
 *
 * @param BuildZoneOptInfo
 *   Indicates if Tecplot should build zone optimization if needed.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilZoneSetBuildZoneOptInfo(
 *   &           Zone,
 *   &           BuildZoneOptInfo)
 *    INTEGER*4       Zone
 *    INTEGER*4       BuildZoneOptInfo
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilDataSetAddZoneX() allows you to create a zone with the
 *     appropriate setting.
 *
 * @ingroup Zone
 *
 */
LINKTOADDON void STDCALL TecUtilZoneSetBuildZoneOptInfo(EntIndex_t Zone,
                                                        Boolean_t  BuildZoneOptInfo);

/**
 * @deprecated
 *   Tecplot no longer requires that this function be called and it does nothing.
 *
 * @ingroup DataServices
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetDefVarLoadFinish(Boolean_t IsDataSetOk);

/**
 * @deprecated
 *   This function was originally misspelled. Please use TecUtilDataSetDefVarLoadFinish()
 *   instead.
 *
 * @ingroup DataServices
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtiltDataSetDefVarLoadFinish(Boolean_t IsDataSetOk);


/**
 * Add a zone to the data set attached to the current frame. This function call
 * does not load any data into the zone. In the case of finite-element zones,
 * this function also does not assign values to the connectivity list. This
 * function only allocates space for the zone. Call TecUtilStateChanged() after
 * adding a zone.
 *
 * @param Name
 *   Name of the zone. The name will be trimmed of any leading or trailing
 *   whitespace and truncated to be no more than 128 characters in length.
 *
 * @param IMax
 *   I-Dimension of the zone if ordered. If the zone is finite-element then
 *   IMax is the number of data points.
 *
 * @param JMax
 *   J-Dimension of the zone if ordered. If the zone is finite-element then
 *   JMax is the total number of element.
 *
 * @param KMax
 *   K-Dimension of the zone. If the zone is a finite-element polyhedral or polygon,
 *   KMax is the number of faces.  KMax is not used for other finite-element zone types.
 *   If you do not know the number of faces for a finite-element zone, you may specify
 *   0 for this parameter and later call TecUtilDataFaceMapAssignElemToNodeMap() inside
 *   a TecUtilDataFaceMapCustomLOD() callback to set this value and generate the zone's
 *   face map.
 *
 * @param ZoneType
 *   The possible values are: \ref ZoneType_Ordered, \ref ZoneType_FETriangle,
 *   \ref ZoneType_FEQuad, \ref ZoneType_FETetra, \ref ZoneType_FEBrick,
 *   \ref ZoneType_FELineSeg, \ref ZoneType_FEPolygon or \ref ZoneType_FEPolyhedron
 *   For polytope zones you must subsequently give the face mapping a data
 *   backing by calling TecUtilDataFaceMapAlloc() or TecUtilDataConnectShare()
 *   or give it deferred load-on-demand status by calling
 *   TecUtilDataFaceMapCustomLOD().
 *
 * @param VarDataType_Array
 *   This is an array of the data types to use for each variable. If you set
 *   this to NULL then the data types of the variables in zone 1 of the
 *   existing data set are used or FieldDataType_Float if this is the first
 *   zone. The possible values are: \ref FieldDataType_Float, \ref FieldDataType_Double,
 *   \ref FieldDataType_Int32, \ref FieldDataType_Int16, \ref FieldDataType_Byte, or
 *   \ref FieldDataType_Bit.
 *
 * @return
 *   Returns TRUE if the zone was successfully added, otherwise FALSE.
 *
 *   Add a 10 by 10 by 10 zone to the current data set:
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>Name</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VarDataType_Array</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetAddZone(
 *   &                   ZoneName,
 *   &                   IMax,
 *   &                   JMax,
 *   &                   KMax,
 *   &                   ZoneType,
 *   &                   VarFieldDataType)
 *    CHARACTER*(*)   AddOnIDString
 *    INTEGER*4       IMax
 *    INTEGER*4       JMax
 *    INTEGER*4       KMax
 *    INTEGER*4       ZoneType
 *    INTEGER*4       VarFieldDataType(1)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @par Note:
 *   VarFieldDataType is an INTEGER*4 array dimensioned by the number of
 *   variables.
 *
 * @code
 *   if ( TecUtilDataSetAddZone("New Ordered Zone", 10, 10, 1,
 *                              ZoneType_Ordered, NULL) )
 *     {
 *       Set_pa zones_added = TecUtilSetAlloc(TRUE);
 *       // new zone is always last zone
 *       EntIndex_t newzone;
 *       TecUtilDataSetGetInfo(NULL, &newzone, NULL);
 *       // fill new zone with values for all variables
 *       .
 *       .
 *       .
 *       // inform Tecplot of new zone
 *       TecUtilSetAddMember(zones_added, newzone, TRUE);
 *       TecUtilStateChanged(StateChange_ZonesAdded,
 *                           (ArbParam_t)zones_added);
 *       TecUtilSetDealloc(&zones_added);
 *     }
 * @endcode
 *
 * @ingroup DataServices
 *
 * #internalattributes exclude_fglue
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetAddZone(const char      *Name,
                                                    LgIndex_t        IMax,
                                                    LgIndex_t        JMax,
                                                    LgIndex_t        KMax,
                                                    ZoneType_e       ZoneType,
                                                    FieldDataType_e *VarDataType_Array);




/**
 * Add a zone to the current data set. This function was extended from
 * TecUtilDataSetAddZone() to allow addition of zones in locations where zombie
 * zones currently exist in a data set. For a simpler, less flexible interface
 * use TecUtilDataSetAddZone() instead. Be sure to call TecUtilStateChanged()
 * after adding zones.
 * Note that all settings for variable properties apply ONLY to those specific variables
 * in this newly created zone and do not apply to any other variables in other zones
 * created in the future.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_NAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   The name of the zone to create. The name will be trimmed of any leading or
 *   trailing whitespace and truncated to be no more than 128 characters in
 *   length.
 *
 * Name:
 *   SV_PARENTZONE
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   A value of zero indicates that this zone is not associated with a parent
 *   zone otherwise a value greater than zero is considered this zone's parent.
 *   A zone may not specify itself as its own parent. With a parent zone
 *   association, Tecplot can generate surface-restricted streamtraces.
 *
 * Name:
 *   SV_STRANDID
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   If 0 no strand ID is assigned to the zone otherwise values greater than
 *   zero are used to assoicated zones with a particular strand.
 *
 * Name:
 *   SV_SOLUTIONTIME
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 * Notes:
 *   The solution time for the zone.
 *
 * Name:
 *   SV_ZONETYPE
 * Type:
 *   ZoneType_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   ZoneType_Ordered
 * Required:
 *   No
 * Notes:
 *   The possible values are: \ref ZoneType_Ordered, \ref ZoneType_FETriangle,
 *   \ref ZoneType_FEQuad, \ref ZoneType_FETetra, \ref ZoneType_FEBrick,
 *   \ref ZoneType_FELineSeg, \ref ZoneType_FEPolyhedron, or \ref ZoneType_FEPolygon.
 *   For polytope zones you must subsequently give the face mapping a data
 *   backing by calling TecUtilDataFaceMapAlloc() or TecUtilDataConnectShare()
 *   or give it deferred load-on-demand status by calling
 *   TecUtilDataFaceMapCustomLOD().
 *
 * Name:
 *   SV_ZONE
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   NumZones+1
 * Required:
 *   No
 * Notes:
 *   Number of the zone to add or replace. If omitted, the new zone number will
 *   be assigned a zone number, NumZones+1, where NumZones is the previous
 *   number of zones. This value can be set to the number of a zone that
 *   already exists thereby replacing the existing zone.
 *
 * Name:
 *   SV_BUILDZONEOPTINFO
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to FALSE to instruct Tecplot to forgo building zone optimization
 *   information. Zone optimization information enhances interactive
 *   performance but has an upfront performance cost. This value can be enabled
 *   or disabled at a later time by calling TecUtilZoneSetBuildZoneOptInfo().
 *
 * Name:
 *   SV_IMAX
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   For an ordered zone SV_IMAX is the number of data points in the I
 *   dimension and for a finite-element zone it is the number of data points.
 *
 * Name:
 *   SV_JMAX
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   For an ordered zone SV_JMAX is the number of data points in the J
 *   dimension and for a finite-element zone it is the number of elements.
 *
 * Name:
 *   SV_KMAX
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   For an ordered zone, SV_KMAX is the number of data points in the K
 *   dimension, and for a polytope zones it is the total number of faces.
 *   If the number of polytope faces is unknown, do not supply this 
 *   argument - the number of faces will be determined later by calling
 *   TecUtilDataFaceMapAssignElemToNodeMap() inside a TecUtilDataFaceMapCustomLOD()
 *   callback.  For a classic FE zone, if the value is supplied, it is ignored.
 *
 * Name:
 *   SV_VARDATATYPE
 * Type:
 *   FieldDataType_e *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   An array of FieldDataType_e dimensioned by the number of variables
 *   currently defined in the dataset where each member specifies the type of
 *   data. If you set this to NULL then the data types of the variables in zone
 *   1 of the existing data set are used or FieldDataType_Float if this is the
 *   first zone.   These settings ONLY apply to the data types of the variables
 *   in this newly created zone.  The possible values are: \ref FieldDataType_Float,
 *   \ref FieldDataType_Double, \ref FieldDataType_Int32, \ref FieldDataType_Int16,
 *   \ref FieldDataType_Byte, or \ref FieldDataType_Bit.
 *
 * Name:
 *   SV_VALUELOCATION
 * Type:
 *   ValueLocation_e *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   An array of ValueLocation_e values (sized by the number of variables).   This
 *   will assign the value locations for all variables in the zone.  If NULL, all
 *   variables will use the locations from zone 1 of the existing data set or
 *   ValueLocation_Nodal if this is the first zone.
 *
 * Name:
 *   SV_VARSHAREZONELIST
 * Type:
 *   EntIndex_t *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   An array of zone numbers dimensioned by the number of variables currently
 *   defined in the dataset where each member specifies the zone used for
 *   sharing each variable. Set the zone number to 0 to specify no sharing for
 *   a specific variable. This option is mutually exclusive with
 *   SV_DEFERVARCREATION. To use variable sharing with SV_DEFERVARCREATION you
 *   must use TecUtilDataValueShare().
 *
 * Name:
 *   SV_DEFERVARCREATION
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Indicates if the creation of the variable should be deferred until the
 *   variable data is ready to be populated or shared. A data loader should
 *   allocate, share, auto or custom load the variable with TecUtilDataValueAlloc(),
 *   TecUtilDataValueShare(), TecUtilDataValueAutoLOD(), or TecUtilDataValueCustomLOD()
 *   before returning control to Tecplot. Any variables that have not been
 *   allocated, shared, auto or custom loaded will become passive variables.
 *   Passive variables always return zero for every point but do not
 *   participate in Tecplot's min/max calculations such as when Tecplot is
 *   choosing good contour levels based on the available data. This option is
 *   mutually exclusive with SV_VARSHAREZONELIST.
 *
 * Name:
 *   SV_DEFERNODEMAPCREATION
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Indicates if the creation of the node map should be deferred until the
 *   node map data is ready to be populated or the connectivity shared. A data
 *   loader must allocate, share, auto or custom load the node map with
 *   TecUtilDataNodeAlloc(), TecUtilDataConnectShare(), TecUtilDataNodeAutoLOD(),
 *   or TecUtilDataNodeCustomLOD() before returning control to Tecplot.
 *   This option is mutually exclusive with SV_CONNECTSHAREZONE.
 *
 * Name:
 *   SV_DEFERFACENBRCREATION
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Indicates if the creation of the face neighbor should be deferred until the
 *   face neighbor data is ready to be populated or the connectivity shared. A data
 *   loader must allocate, share, or custom load the face neighbor data with
 *   TecUtilDataConnectShare(), or TecUtilDataFaceNbrCustomLOD() before
 *   returning control to Tecplot.
 *   This option is mutually exclusive with SV_CONNECTSHAREZONE.
 *
 * Name:
 *   SV_CONNECTSHAREZONE
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TECUTILBADZONENUMBER
 * Required:
 *   No
 * Notes:
 *   Number of the zone to use for sharing of connectivity information. If not
 *   supplied the connectivity will not be shared. This option is mutually
 *   exclusive with SV_DEFERNODEMAPCREATION and SV_DEFERFACENBRCREATION.
 *   To use node map sharing with SV_DEFERNODEMAPCREATION and/or
 *   SV_DEFERFACENBRCREATION you must use TecUtilDataConnectShare().
 *
 * Name:
 *   SV_AUTOASSIGNFN
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if Tecplot should auto assign any remaining
 *   face neighbors after the add-on has supplied the faces. This is useful
 *   when an add-on only needs to deliver a few specific neighbors. Add-ons
 *   that wish to supply all the face neighbor connections should set this
 *   value to FALSE.
 *
 * Name:
 *   SV_FACENEIGHBORMODE
 * Type:
 *   FaceNeighborMode_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FaceNeighborMode_LocalOneToOne
 * Required:
 *   No
 * Notes:
 *   Specifies the face-neighbor mode.  Options are:
 *   FaceNeighborMode_LocalOneToOne, FaceNeighborMode_LocalOneToMany,
 *   FaceNeighborMode_GlobalOneToOne, FaceNeighborMode_GlobalOneToMany.  The
 *   default is FaceNeighborMode_LocalOneToOne
 * </ArgListTable>
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetAddZoneX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Add a 10 by 20 ordered zone as zone number 3.
 *
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendString(ArgList, SV_NAME, "New Zone");
 *   TecUtilArgListAppendInt(ArgList, SV_ZONE, 3);
 *   TecUtilArgListAppendInt(ArgList, SV_ZONETYPE,
 *                           (ArbParam_t)ZoneType_Ordered);
 *   TecUtilArgListAppendInt(ArgList, SV_IMAX, 10);
 *   TecUtilArgListAppendInt(ArgList, SV_JMAX, 20);
 *   TecUtilDataSetAddZoneX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * Add a volume zone and two associated child patch zones. The child zones are
 * loosely associated with the parent volume zone. With this association,
 * Tecplot can generate surface-restricted streamtraces.
 *
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *
 *   TecUtilArgListAppendString(ArgList, SV_NAME, "Parent Volume Zone");
 *   TecUtilArgListAppendInt(ArgList, SV_ZONE, 1);
 *   TecUtilArgListAppendInt(ArgList, SV_ZONETYPE, (ArbParam_t)ZoneType_Ordered);
 *   TecUtilArgListAppendInt(ArgList, SV_IMAX, 10);
 *   TecUtilArgListAppendInt(ArgList, SV_JMAX, 20);
 *   TecUtilArgListAppendInt(ArgList, SV_KMAX, 30);
 *   TecUtilDataSetAddZoneX(ArgList);
 *
 *   TecUtilArgListAppendString(ArgList, SV_NAME, "Child Patch Zone 1");
 *   TecUtilArgListAppendInt(ArgList, SV_ZONE, 2);
 *   TecUtilArgListAppendInt(ArgList, SV_PARENTZONE, 1); // absolution reference to the parent zone
 *   TecUtilArgListAppendInt(ArgList, SV_ZONETYPE, (ArbParam_t)ZoneType_Ordered);
 *   TecUtilArgListAppendInt(ArgList, SV_IMAX, 10);
 *   TecUtilArgListAppendInt(ArgList, SV_JMAX, 20);
 *   TecUtilDataSetAddZoneX(ArgList);
 *
 *   TecUtilArgListAppendString(ArgList, SV_NAME, "Child Patch Zone 2");
 *   TecUtilArgListAppendInt(ArgList, SV_ZONE, 3);
 *   TecUtilArgListAppendInt(ArgList, SV_PARENTZONE, 1); // absolution reference to the parent zone
 *   TecUtilArgListAppendInt(ArgList, SV_ZONETYPE, (ArbParam_t)ZoneType_Ordered);
 *   TecUtilArgListAppendInt(ArgList, SV_IMAX, 20);
 *   TecUtilArgListAppendInt(ArgList, SV_JMAX, 30);
 *   TecUtilDataSetAddZoneX(ArgList);
 *
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @sa TecUtilDataSetAddVarX()
 *
 * @ingroup DataServices
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetAddZoneX(ArgList_pa ArgList);







/**
 * Add a variable to the current data set. Make sure and call
 * TecUtilStateChanged() after adding variables.  Note that all settings for
 * variable properties apply ONLY to those specific variables created in the
 * existing set of zones.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_NAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   Name of newly created variable. The name will be trimmed of any leading
 *   or trailing whitespace and truncated to be no more than 128 characters
 *   in length.
 *
 * Name:
 *   SV_VARDATATYPE
 * Type:
 *   FieldDataType_e *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Array of FieldDataType_e dimensioned by the number of zones currently
 *   defined in the dataset where each member specifies type of data.
 *   If you set this to NULL then FieldData_Float is used if this is the
 *   first variable in the dataset otherwise the type from the first variable
 *   for each zone is used.
 *
 * Name:
 *   SV_VALUELOCATION
 * Type:
 *   ValueLocation_e *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Array of ValueLocation_e dimensioned by the number of zones currently
 *   defined in the dataset where each member specifies the data value
 *   location.  If NULL then ValueLocation_Nodal is used.
 *
 * Name:
 *   SV_TRYSHAREVARWITHALLZONES
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   A boolean property that instructs Tecplot to share the variable with all
 *   applicable zones in the dataset. This option is mutually exclusive with
 *   SV_DEFERVARCREATION. To use variable sharing with SV_DEFERVARCREATION you
 *   must use TecUtilDataValueShare().
 *
 * Name:
 *   SV_DEFERVARCREATION
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Indicates if the creation of the variable should be deferred until the
 *   variable data is ready to be populated or shared. A data loader should
 *   allocate, share, auto or custom load the variable with TecUtilDataValueAlloc(),
 *   TecUtilDataValueShare(), TecUtilDataValueAutoLOD(), or TecUtilDataValueCustomLOD()
 *   before returning control to Tecplot. Any variables that have not been
 *   allocated, shared, auto or custom loaded will become passive variables.
 *   Passive variables always return zero for every point but do not
 *   participate in Tecplot's min/max calculations such as when Tecplot is
 *   choosing good contour levels based on the available data. This option is
 *   mutually exclusive with SV_VARSHAREZONELIST.
 * </ArgListTable>
 *
 * @return
 *   TRUE if the variable was added, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetAddVarX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Add a variable called "newvar" to the dataset.
 *
 * @code
 *   ArgList_pa ArgList;
 *   FieldDataType_e *VarDataType;   // dimension by zone
 *   ValueLocation_e *ValueLocation; // dimension by zone
 *
 *   TecUtilLockStart(AddOnID);
 *
 *   ...
 *
 *   //
 *   // Allocate and populate VarDataType and ValueLocation
 *   // with the appropriate value for each zone.
 *
 *   ...
 *
 *   // collect the function arguments
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendString(ArgList,
 *                              SV_NAME,
 *                              "newvar");
 *   TecUtilArgListAppendArray(ArgList,
 *                             SV_VARDATATYPE,
 *                             (void *)VarDataType);
 *   TecUtilArgListAppendArray(ArgList,
 *                             SV_VALUELOCATION,
 *                             (void *)ValueLocation);
 *   TecUtilArgListAppendInt(ArgList,
 *                           SV_TRYSHAREVARWITHALLZONES,
 *                           FALSE);
 *
 *   // add the variable for all zones in the dataset
 *   TecUtilDataSetAddVarX(ArgList);
 *
 *   // cleanup
 *   TecUtilArgListDealloc(&ArgList);
 *
 *   // Inform Tecplot that a variable was added
 *
 *   VarsAdded = TecUtilSetAlloc(FALSE);
 *   if (VarsAdded)
 *     {
 *       EntIndex_t NumVars;
 *       TecUtilDataSetGetInfo((char **)NULL,
 *                             (EntIndex_t *)NULL,
 *                             &NumVars);
 *       TecUtilSetAddMember(VarsAdded,NumVars,FALSE);
 *       TecUtilStateChanged(StateChange_VarsAdded,
 *                           (ArbParam_t)VarsAdded);
 *       TecUtilSetDealloc(&VarsAdded);
 *     }
 *
 *   ...
 *
 *   // cleanup VarDataType and ValueLocation allocations
 *
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @sa TecUtilDataSetAddZoneX()
 *
 * @ingroup DataServices
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetAddVarX(ArgList_pa ArgList);


/**
 * Reallocate the classic FE or ordered zone in the data set attached to the
 * current frame. This in effect re-dimensions the raw data referenced by the
 * zone.
 *
 * Data in the reallocated zone is preserved as much as possible. If the zone
 * is reduced in size all field data should be preserved where like I,J,K
 * subscripted locations in the old zone (using the old dimensions to calculate
 * the offset) are copied to the same I,J,K subscripted locations in the new
 * zone (using the new dimensions).  If the dimensions of the zone are
 * increased then the field data at subscripts beyond the original dimensions
 * are initialized to zero.
 *
 * If the zone is classic finite element and an element contains a point that
 * is no longer available (because the zone was reduced in size) it is reset to
 * the first point in the dataset. If the connectivity list is expanded then
 * all nodes in the newly created elements will reference the first point in
 * the dataset.
 *
 * @par Note:
 *   The preferred method of resizing a zone is to call
 *   TecUtilDataSetAddZoneX() using the zone number of
 *   the target zone to resize.
 *
 * @param Zone
 *   One-based index of the zone to reallocate
 *
 * @param NewIMaxOrNumDataPoints
 *   New IMax or number of data points
 *
 * @param NewJMaxOrNumElements
 *   New JMax or number of elements
 *
 * @param NewKMax
 *   New KMax
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneRealloc(
 *   &                   Zone,
 *   &                   NewIMaxOrNumDataPoints,
 *   &                   NewJMaxOrNumElements,
 *   &                   NewKMax)
 *    INTEGER*4       Zone
 *    INTEGER*4       NewIMaxOrNumDataPoints
 *    INTEGER*4       NewJMaxOrNumElements
 *    INTEGER*4       NewKMax
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Reallocate the first zone:
 * @code
 *   TecUtilZoneRealloc(1,15,4,1);
 * @endcode
 *
 * @ingroup Zone
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneRealloc(EntIndex_t Zone,
                                                 LgIndex_t  NewIMaxOrNumDataPoints,
                                                 LgIndex_t  NewJMaxOrNumElements,
                                                 LgIndex_t  NewKMax);


/**
 * Add a variable to the current data set. Call TecUtilStateChanged() after
 * adding a variable. This function is superceded by TecUtilDataSetAddVarX().
 *
 * @param VarName
 *   Name of the variable being added. The name will be trimmed of any leading
 *   or trailing whitespace and truncated to be no more than 128 characters
 *   in length.
 *
 * @param FieldDataType_Array
 *   This is an array of the data types to use for each zone (i.e., it must
 *   be dimensioned by the number of zones). If you pass NULL,
 *   the data types of the variables in variable 1 of the existing data set are
 *   used. The possible choices are: FieldDataType_Float, FieldDataType_Double,
 *   FieldDataType_Int32, FieldDataType_Int16, FieldDataType_Byte, or
 *   FieldDataType_Bit.
 *
 * @return
 *   Returns TRUE if the variable was added successfully, otherwise FALSE.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>VarName</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>FieldDataType_Array</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetAddVar(
 *   &                   VarName,
 *   &                   VarFieldDataType)
 *    CHARACTER*(*)   AddOnIDString
 *    INTEGER*4       VarFieldDataType(1)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @par Note:
 *   VarFieldDataType is an INTEGER*4 array dimensioned by the number of
 *   zones.
 *
 * Add a variable to the current data set:
 *
 * @code
 *   if ( TecUtilDataSetAddVar("New Variable", NULL) )
 *     {
 *       // New variable is always last variable.
 *       EntIndex_t newvar;
 *       TecUtilDataSetGetInfo(NULL, NULL, &newvar);
 *       // Fill new var with values for all zones.
 *       .
 *       .
 *       .
 *       // Inform Tecplot a variable has been added.
 *       TecUtilStateChanged(StateChange_VarsAdded,
 *                           (ArbParam_t)NULL);
 *     }
 * @endcode
 *
 * @ingroup DataServices
 *
 * #internalattributes exclude_fglue
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetAddVar(const char      *VarName,
                                                   FieldDataType_e *FieldDataType_Array);


/**
 * Include a function in the list of functions to call when Tecplot is
 * considering shutting down. If you are building an add-on that could
 * potentially be in a state where it is not convenient to shut down, then use
 * this function. If the add-on does not wish for Tecplot to quit then the
 * function referenced in this call can issue an error message to the user as
 * to why this is the case and then return FALSE. A FALSE return value back to
 * Tecplot will abort the shutdown sequence in Tecplot. TRUE return values by
 * all MopupQuery functions will allow the shut down sequence to occur.  The
 * quit query callback is not to be used to do mopup operations such as closing
 * of files and freeing of memory. Instead use TecUtilStateChangeAddCallback()
 * to register a function that will listen for the message
 * \ref StateChange_QuitTecplot which is sent when Tecplot has begun its
 * shutdown sequence.
 *
 * @return
 *   TRUE if the callback has been added. FALSE only if there is not enough
 *   room to allocate space for an additional callback (highly unlikely).
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQuitAddQueryCallback(QuitQueryCallback)
 *    EXTERNAL QuitQueryCallback
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 * #internalattributes exclude_sdkdoc, exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilQuitAddQueryCallback(MopupQueryAddOnCallback_pf QuitQueryCallback);

/**
 * Adds the callback and its client data to the list of pre-draw event
 * callbacks. Tecplot calls all pre-draw event callbacks with their associated
 * client data immediately before performing a redraw.
 *
 * @since
 *   11.0-0-363
 *
 * @param DrawEventCallback
 *   Callback to call immediately before a Tecplot performs a redraw.
 * @param ClientData
 *   Client data that Tecplot passes along to the callback as a parameter.
 *
 * @return
 *   TRUE if the callback and its client data were successfully added, FALSE
 *   otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilEventAddPreDrawCallback(
 *   &                   DrawEventCallback,
 *   &                   ClientDataPtr)
 *    EXTERNAL           DrawEventCallback
 *    POINTER            (ClientDataPtr, ClientData)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilEventAddPostDrawCallback()
 *
 * @ingroup Drawing
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilEventAddPreDrawCallback(DrawEventCallback_pf DrawEventCallback,
                                                             ArbParam_t           ClientData);

/**
 * Adds the callback and its client data to the list of post-draw event
 * callbacks. Tecplot calls all post-draw event callbacks with their associated
 * client data immediately after performing a redraw.
 *
 * @since
 *   11.0-0-363
 *
 * @param DrawEventCallback
 *   Callback to call immediately after a Tecplot performs a redraw.
 * @param ClientData
 *   Client data that Tecplot passes along to the callback as a parameter.
 *
 * @return
 *   TRUE if the callback and its client data were successfully added, FALSE
 *   otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilEventAddPostDrawCallback(
 *   &                   DrawEventCallback,
 *   &                   ClientDataPtr)
 *    EXTERNAL           DrawEventCallback
 *    POINTER            (ClientDataPtr, ClientData)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilEventAddPreDrawCallback()
 *
 * @ingroup Drawing
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilEventAddPostDrawCallback(DrawEventCallback_pf DrawEventCallback,
                                                              ArbParam_t           ClientData);

/**
 * Set the mode in which state changes are propagated to the specified state
 * change callback.
 *
 * @param Callback
 *   Function already registered to receive state change callbacks
 *
 * @param Mode
 *   Mode you want state changes propagated to your state change callback
 *   function. Choose either @ref StateChangeMode_v80, @ref StateChangeMode_v100,
 *   or @ref StateChangeMode_v113. See the section "Sending State Changes" in
 *   the "ADK User's Manual" for a complete description of the differences
 *   between these options.
 *
 * @return
 *   Returns TRUE successful (that is, the callback function was registered), FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeSetMode(
 *   &                   StateChangeCallback,
 *   &                   Mode)
 *    EXTERNAL   StateChangeCallback
 *    INTEGER*4  Mode
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Set the mode state change callbacks to the function BananaCallbackFunction
 * to use the v113 mode.
 *
 * @code
 *     TecUtilStateChangeSetMode(BananaCallbackFunction,
 *                               StateChangeMode_v113);
 * @endcode
 *
 * @ingroup StateChange
 *
 * #internalattributes exclude_fglue, exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeSetMode(StateChangeAddOnCallback_pf Callback,
                                                        StateChangeMode_e           Mode);

/**
 * Include a function in the list of functions to call when a state change
 * occurs in Tecplot. For more detailed discussion, either see
 * TecUtilOnIdleQueueAddCallback() or the ADK User's Manual. If you want to
 * take advantage of newer capabilities with regard to state changes then use
 * TecUtilStateChangeAddCallbackX() instead.
 *
 * @param StateChangeCallback
 *   This is the name of the callback function you provide. This function will
 *   be called by Tecplot each time a state change occurs. See
 *
 * @return
 *   Returns TRUE if callback has been added. Returns FALSE only if there is
 *   not enough room to allocate space for an additional callback (highly
 *   unlikely).
 *
 * The following example will set up a state change callback. This callback
 * will do the following:
 *
 *   -# When the user picks an object, check and see if it is a zone. If it is, then change its color to red.
 *   -# If anything has happened to the fourth variable, then show an error message.
 *   -# If Tecplot is quitting, then close some files that are open.
 *
 * First, in the Tecplot initialization code add the callback:
 * @code
 *   IsOk=TecUtilStateChangeAddCallback(MyStateChangeCallback);
 * @endcode
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeAddCallback(StateChangeCallback)
 *    EXTERNAL StateChangeCallback
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * And add the state change callback:
 * @code
 *   void STDCALL MyStateChangeCallback(StateChange_e StateChange,
 *                                      ArbParam_t    CallData)
 *   {
 *     .
 *     .
 *     .
 *     //
 *     // Item 1.  Check for the case when the user picks a zone.
 *     //
 *     if (StateChange == StateChange_PickListSingleSelect)
 *       {
 *         int NumPickedObjects;
 *         NumPickedObjects = TecUtilPickListGetCount();
 *
 *          //
 *          // Check to see if the last object picked is a zone.
 *          //
 *         if (TecUtilPickListGetType(NumPickedObjects)==
 *             PickObjects_Zone)
 *           {
 *             EntIndex_t ZonePicked;
 *             Set_pa     ZoneSet;
 *
 *             // Get the number of the zone picked
 *             ZonePicked = TecUtilPickListGetZoneNumber(NumPickedObjects);
 *
 *             //
 *             // Build the zone set to pass to TecUtilZoneSetMesh().
 *             // In this case there is only one zone
 *             //
 *             ZoneSet = TecUtilSetAlloc(FALSE);
 *             if (ZoneSet)
 *               {
 *                 TecUtilSetAddMember(ZoneSet,ZonePicked,TRUE);
 *
 *                 //
 *                 // Change the mesh color attribute.
 *                 //
 *                 TecUtilZoneSetMesh(SV_COLOR, ZoneSet, 0.0,
 *                                    (ArbParam_t)Red_C);
 *                 TecUtilSetDealloc(&ZoneSet);
 *               }
 *           }
 *       }
 *       //
 *       // Item 2. Check for a change in the 4th variable.
 *       //
 *       else if (StateChange == StateChange_VarsAltered)
 *         {
 *           if (TecUtilSetIsMember((Set_pa)CallData, (SetIndex_t)4))
 *             TecUtilDialogErrMsg("Var number 4 was altered");
 *         }
 *
 *       //
 *       // Item 3.  Close an open file if Tecplot is
 *       // shutting down.
 *       //
 *       else if (StateChange == StateChange_QuitTecplot)
 *         {
 *           fclose(SomeOpenFile);
 *         }
 *       .
 *       .
 *       .
 *     }
 * @endcode
 *
 * @ingroup StateChange
 *
 * #internalattributes exclude_fglue, exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeAddCallback(StateChangeAddOnCallback_pf StateChangeCallback);






/**
 * Inform tecplot of a state change.  Currently this must be called in the following
 * situations:
 *
 *       - Launch and dismiss of modal dialogs (Windows only).
 *       - After a variable has been added and subsequently modified.
 *       - After a variable has been modified.
 *       - After TecUtilDataSetAddZone() has been called and the field data has
 *         been modified (Use \ref StateChange_ZonesAdded).
 *       - After the node map has been altered.
 *
 * The CallData parameter is required for the following state changes:
 *
 * @verbatim
                                  CallData
     StateChange                  Type      Represents
     --------------------------------------------------------------------------
     StateChange_VarsAltered      Set_pa    set of variables altered
     StateChange_VarsAdded        Set_pa    set of variables added
     StateChange_ZonesAdded       Set_pa    set of zones added
     StateChange_NodeMapsAltered  Set_pa    set of zones where nodemaps were altered
   @endverbatim
 *
 * @param StateChange
 *   Specifies the state change of which to inform Tecplot. addons are only
 *   allowed to send specific state change messages. See the ADK User's Manual
 *
 * @param CallData
 *   Extra information for the state change.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStateChanged(
 *   &           StateChange,
 *   &           CallDataPtr)
 *    INTEGER*4       StateChange
 *    POINTER         (CallDataPtr, CallData)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a zone, modify its variable values, and inform Tecplot of the state
 *   change:
 *
 * @code
 *   if (TecUtilDataSetAddZone("Banana", 10, 10, 1,
 *                             ZoneType_Ordered, NULL))
 *     {
 *       EntIndex_t Zone;
 *       Set_pa     ZSet = TecUtilSetAlloc(TRUE);
 *       TecUtilDataSetGetInfo(NULL, &Zone, NULL);
 *
 *       //  Set up the variable values for the new zone
 *
 *       TecUtilSetAddMember(ZSet, Zone,TRUE);
 *       TecUtilStateChanged(StateChange_ZonesAdded,(ArbParam_t)ZSet);
 *       TecUtilSetDealloc(&ZSet);
 *     }
 * @endcode
 *
 * @ingroup StateChange
 *
 */
LINKTOADDON void STDCALL TecUtilStateChanged(StateChange_e StateChange,
                                             ArbParam_t    CallData);

/**
 * Remove a previously added callback from Tecplot's list of add-on state
 * change callbacks.
 *
 * @par Note:
 *     An add-on may not remove its callback in response to a state change.
 *
 * @since
 *     11.2-0-113
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 *
 * <ArgListTable>
 * Name:
 *   SV_CALLBACKFUNCTION
 * Type:
 *   void *
 * Arg Function:
 *   TecUtilArgListAppendFunction()
 * Required:
 *   Yes
 * Notes:
 *   Previously registered add-on state change callback to remove from Tecplot's
 *   list of state change callbacks.
 *
 * Name:
 *   SV_CLIENTDATA
 * Type:
 *   ArbParam_t
 * Arg Function:
 *   TecUtilArgListAppendArbParam()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   Client data that was previously registered with the callback. If supplied,
 *   Tecplot will only remove the callback that matches both the supplied
 *   callback function and client data. This is useful if the same callback was
 *   previously registered with different client data. If not supplied Tecplot
 *   will remove the first callback that matches the supplied function
 *   regardless of the client data registered with the callback.
 * </ArgListTable>
 *
 * @return
 *     TRUE if the callback was found in the list of add-on state change
 *     callbacks, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeRemoveCBX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup StateChange
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeRemoveCBX(ArgList_pa ArgList);

/**
 * Remove a previously added callback from Tecplot's list of add-on state
 * change callbacks.
 *
 * @par Note:
 *     An add-on may not remove its callback in response to a state change.
 *
 * @since
 *     11.0-5-041
 *
 * @param AddOnStateChangeCallback
 *     Previously registered add-on callback to remove from Tecplot's list of
 *     state change callbacks.
 *
 * @return
 *     TRUE if the callback was found in the list of add-on state change
 *     callbacks, FALSE otherwise.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup StateChange
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeRemoveCallback(void *AddOnStateChangeCallback);

/**
 * Add a callback to Tecplot's list of add-on's to receive state changes
 * notifications.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_STATECHANGEMODE
 * Type:
 *   StateChangeMode_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   StateChangeMode_v100
 * Required:
 *   No
 * Notes:
 *   Assigns the state change mode.  Possible values are @ref StateChangeMode_v75,
 *   @ref StateChangeMode_v80, @ref StateChangeMode_v100, and @ref StateChangeMode_v113.
 *   See the section "Sending State Changes" in the "ADK User's Manual" for a
 *   complete description of the differences between these options.
 *
 * Name:
 *   SV_CALLBACKFUNCTION
 * Type:
 *   void *
 * Arg Function:
 *   TecUtilArgListAppendFunction()
 * Required:
 *   Yes
 * Notes:
 *   Callback Function for state changes.   See discussion of StateChangeCallbackAPI below.
 *
 * Name:
 *   SV_STATECHANGECALLBACKAPI
 * Type:
 *   StateChangeCallbackAPI_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   StateChangeCallbackAPI_ChangeOnly
 * Required:
 *   No
 * Notes:
 *   Determines the API for the statechange callback function.
 *   Use StateChangeCallbackAPI_Classic if your callback uses StateChangeAddOnCallback_pf.
 *   Use StateChangeCallbackAPI_ChangeOnly if your callback uses StateChangeAddOnCallbackV2_pf
 *   Use StateChangeCallbackAPI_ChangePlusClient if your callback uses
 *   StateChangeAddOnCallbackWithClient_pf.
 *
 * Name:
 *   SV_CLIENTDATA
 * Type:
 *   ArbParam_t
 * Arg Function:
 *   TecUtilArgListAppendArbParam()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   Client data sent to callback if StateChangeCallbackAPI is set to
 *   StateChangeCallbackAPI_ChangePlusClient.
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
 *    INTEGER*4 FUNCTION TecUtilStateChangeAddCallbackX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Register the function MyStateChangeCallback with Tecplot using the "ChangeOnly" API:
 *
 * @code
 *   static void STDCALL MyStateChangeCallback(StateChange_e StateChange)
 *   {
 *     TecUtilLockStart(AddOnID);
 *     switch (StateChange)
 *       {
 *         case StateChange_VarsAltered,
 *           {
 *             Set_pa VarsAlteredRef = NULL;
 *             if (TecUtilStateChangeGetVarSet(&VarsAlteredRef))
 *               {
 *                 Set_pa ZonesAlteredRef = NULL;
 *                 if (TecUtilStateChangeGetZoneSet(&ZonesAlteredRef))
 *                   {
 *                     ...take action knowing which vars in which
 *                     ...were altered.
 *                   }
 *                 else
 *                   {
 *                     ...assume all zones were affected.  Take action
 *                     ...knowing which vars were altered.
 *                   }
 *               }
 *           } break;
 *         ...and so on...
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 *   .
 *   .
 *   .
 *   void InitTecAddOn(void)
 *   {
 *     ArgList_pa ArgList;
 *     .
 *     .
 *     .
 *     ArgList = TecUtilArgListAlloc();
 *     TecUtilArgListAppendFunction(ArgList,
 *                                  SV_CALLBACKFUNCTION,
 *                                  MyStateChangeCallback);
 *     TecUtilStateChangeAddCallbackX(ArgList);
 *     TecUtilArgListDealloc(&ArgList);
 *     .
 *     .
 *     .
 *   }
 * @endcode
 *
 * Register the function MyStateChageCallback with Tecplot using the "ChangePlusClient" API:
 *
 * @code
 *   static void MyStateChangeCallback(StateChange_e StateChange,
 *                                     ArbParam_t    ClientData)
 *   {
 *     // Do something with ClientData....
 *     TecUtilLockStart(AddOnID);
 *     switch (StateChange)
 *       {
 *         case StateChange_VarsAltered,
 *           {
 *             Set_pa VarsAlteredRef = NULL;
 *             if (TecUtilStateChangeGetVarSet(&VarsAlteredRef))
 *               {
 *                 Set_pa ZonesAlteredRef = NULL;
 *                 if (TecUtilStateChangeGetZoneSet(&ZonesAlteredRef))
 *                   {
 *                     ...take action knowing which vars in which
 *                     ...were altered.
 *                   }
 *                 else
 *                   {
 *                     ...assume all zones were affected.  Take action
 *                     ...knowing which vars were altered.
 *                   }
 *               }
 *           } break;
 *         ... and so on...
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 *   .
 *   .
 *   .
 *   void InitTecAddOn(void)
 *   {
 *     ArgList_pa ArgList;
 *     ArbParam_t MyClientData;
 *     .
 *     . ...Assign something to MyClientData...
 *     .
 *     ArgList = TecUtilArgListAlloc();
 *     TecUtilArgListAppendFunction(ArgList,
 *                                  SV_CALLBACKFUNCTION,
 *                                  (const void *)MyStateChangeCallback);
 *
 *     TecUtilArgListAppendInt(     ArgList,
 *                                  SV_STATECHANGECALLBACKAPI,
 *                                  (LgIndex_t)StateChangeCallbackAPI_ChangePlusClient);
 *
 *     TecUtilArgListAppendArbParam(ArgList,
 *                                  SV_CLIENTDATA,
 *                                  MyClientData);
 *
 *     TecUtilStateChangeAddCallbackX(ArgList);
 *     TecUtilArgListDealloc(&ArgList);
 *     .
 *     .
 *     .
 *   }
 * @endcode
 *
 * @ingroup StateChange
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeAddCallbackX(ArgList_pa ArgList);


/**
 * Retrieve Index supplemental information from the previous state change.
 *
 * @since
 *   11.0-5-016
 *
 * @param Index
 *   Retrieved Index value from the previous state change
 *
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *
 *
 * @pre <em>Index</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetIndex(Index)
 *    INTEGER*4 Index
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Your state change callback was just called with \ref StateChange_VarsAltered.
 *   Take action if you can retrieve which data point index was altered.
 *
 * @code
 *     LgIndex_t IndexValue;
 *     if (TecUtilStateChangeGetIndex(&IndexValue))
 *       {
 *           ....take some action using IndexValue.
 *       }
 * @endcode
 *
 * @ingroup StateChange
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetIndex(TP_OUT LgIndex_t* Index);



/**
 * Retrieve the page UniqueID supplemental information from the previous state
 * change.
 *
 * @since
 *   11.3-7-003
 *
 * @param UniqueID
 *   Retrieved page UniqueID value from the previous state change.
 *
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *
 *
 * @pre <em>UniqueID</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetPageUniqueID(UniqueID)
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *   UniqueID_t UniqueID;
 *   if (TecUtilStateChangeGetPageUniqueID(&UniqueID))
 *     {
 *       ....take some action using UniqueID.
 *     }
 * @endcode
 *
 * @ingroup StateChange
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetPageUniqueID(TP_OUT UniqueID_t* UniqueID);

/**
 * Retrieve the frame UniqueID supplemental information from the previous state
 * change.
 *
 * @since
 *   11.3-7-003
 *
 * @param UniqueID
 *   Retrieved frame UniqueID value from the previous state change.
 *
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *
 *
 * @pre <em>UniqueID</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetFrameUniqueID(UniqueID)
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *   UniqueID_t UniqueID;
 *   if (TecUtilStateChangeGetFrameUniqueID(&UniqueID))
 *     {
 *       ....take some action using UniqueID.
 *     }
 * @endcode
 *
 * @ingroup StateChange
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetFrameUniqueID(TP_OUT UniqueID_t* UniqueID);

/**
 * Retrieve the dataset UniqueID supplemental information from the previous state
 * change.
 *
 * @since
 *   11.3-7-003
 *
 * @param UniqueID
 *   Retrieved dataset UniqueID value from the previous state change.
 *
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *
 *
 * @pre <em>UniqueID</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetDataSetUniqueID(UniqueID)
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *   UniqueID_t UniqueID;
 *   if (TecUtilStateChangeGetDataSetUniqueID(&UniqueID))
 *     {
 *       ....take some action using UniqueID.
 *     }
 * @endcode
 *
 * @ingroup StateChange
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetDataSetUniqueID(TP_OUT UniqueID_t* UniqueID);

/**
 * Retrieve UniqueID supplemental information from the previous state change.
 *
 * @since
 *   11.0-5-016
 *
 * @param UniqueID
 *   Retrieved UniqueID value from the previous state change.
 *
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *
 *
 * @pre <em>UniqueID</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetUniqueID(UniqueID)
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *   UniqueID_t UniqueID;
 *   if (TecUtilStateChangeGetUniqueID(&UniqueID))
 *     {
 *       ....take some action using UniqueID.
 *     }
 * @endcode
 *
 * @ingroup StateChange
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetUniqueID(TP_OUT UniqueID_t* UniqueID);



/**
 * Retrieve enumerated supplemental information from the previous state change.
 *
 * @param ArbEnum
 *   Retrieved enumerated value from the previous state change. Type cast this to the appropriate enum
 *   to get the value
 *
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *
 *
 * @pre <em>ArbEnum</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetArbEnum(ArbEnum)
 *    INTEGER*4 ArbEnum
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Your state change callback was just called with \ref StateChange_View.
 *   Take action if the view type was \ref View_Zoom
 *
 * @code
 *     LgIndex_t ArbEnumValue;
 *     TecUtilStateChangeGetArbEnum(&ArbEnumValue);
 *     if ((View_e)ArbEnumValue == View_Zoom)
 *        {
 *           ....take some action.
 *        }
 * @endcode
 *
 * @ingroup StateChange
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetArbEnum(TP_OUT LgIndex_t* ArbEnum);

/**
 * Retrieve a reference to the set of zones associated with the previous state
 * change.
 *
 * @par Note:
 *   Unlike many other properties retrieved from Tecplot this set is not an
 *   allocated copy and should not be deallocated.
 *
 * @param ZoneSetRef
 *   Read-only reference to a retreived set of zones associated with the
 *   previous state change.
 *
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *
 *
 * @pre <em>ZoneSetRef</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetZoneSet(ZoneSetRefPtr)
 *    POINTER (ZoneSetRefPtr, ZoneSetRef)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Your state change callback was just called with \ref StateChange_VarsAltered.
 * Take action using the retrieved set of variables that were altered. In
 * addition, if possible, make use of the set of zones that were altered if you
 * can retrieve that information.
 *
 * @code
 *   Set_pa VarsAlteredRef;
 *   if (TecUtilStateChangeGetVarSet(&VarsAlteredRef))
 *     {
 *        Set_pa ZonesAlteredRef;
 *        if (TecUtilStateChangeGetZoneSet(&ZonesAlteredRef))
 *           {
 *              ... take action knowing both what zones and
 *              ... what vars were altered.
 *           }
 *         else
 *           {
 *              ....take some action using only VarsAltered.
 *              .... assume all zones were altered.
 *           }
 *     }
 * @endcode
 *
 * @ingroup StateChange
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetZoneSet(TP_OUT Set_pa* ZoneSetRef);

/**
 * Retrieve a reference to the set of variables associated with the previous
 * state change.
 *
 * @par Note:
 *   Unlike many other properties retrieved from Tecplot this set is not an
 *   allocated copy and should not be deallocated.
 *
 * @param VarSetRef
 *   Read-only reference to a retrieved set of variables associated with the
 *   previous state change.
 *
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *
 *
 * @pre <em>VarSetRef</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetVarSet(VarSetRefPtr)
 *    POINTER (VarSetRefPtr, VarSetRef)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Your state change callback was just called with \ref StateChange_VarsAltered.
 * Take action using the retrieved set of variables that were altered.
 *
 * @code
 *   Set_pa VarsAlteredRef;
 *   if (TecUtilStateChangeGetVarSet(&VarsAlteredRef))
 *     {
 *         ....take some action using VarsAltered.
 *         .... Do not dealloc VarsAltered when finished.
 *     }
 * @endcode
 *
 * @ingroup StateChange
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetVarSet(TP_OUT Set_pa* VarSetRef);

/**
 * Retrieve a reference to the name associated with the previous state change.
 *
 * @since
 *   10.0-3-129
 *
 * @par Note:
 *   Unlike many other properties retrieved from Tecplot this name string is
 *   not an allocated copy and should not be deallocated.
 *
 * @param NameRef
 *   Read-only reference to a retrieved name associated with the previous state
 *   change.
 *
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *
 *
 * @pre <em>NameRef</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetName(
 *   &                   Name,
 *   &                   NameLength)
 *    CHARACTER*(*)   Name
 *    INTEGER*4       NameLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Your state change callback was just called with \ref StateChange_AuxDataAdded.
 * Take action if it was "COMMON.TIME" auxiliary data associated with zone 2.
 *
 * @code
 *     LgIndex_t   ArbEnumValue;
 *     EntIndex_t  Zone;
 *     char       *Name;
 *     if ((TecUtilStateChangeGetArbEnum(&ArbEnumValue) &&
 *          ((AuxDataLocation_e)ArbEnumValue == AuxDataLocation_Zone)) &&
 *         (TecUtilStateChangeGetZone(&Zone) &&
 *          Zone == 2)                                                 &&
 *         (TecUtilStateChangeGetName(&Name) &&
 *          ustrcmp(Name, AuxData_Common_Time) == 0))
 *        {
 *           ....take some knowing aux data "COMMON.TIME" was just added
 *           .... to zone Zone.
 *        }
 * @endcode
 *
 * @ingroup StateChange
 *
 * #internalattributes exclude_fglue
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetName(TP_OUT char** NameRef);

/**
 * Retrieve a reference to the map associated with the previous state change.
 *
 * @since
 *   11.0-0-320
 *
 * @param Map
 *   Reference to the map associated with the previous state change.
 *
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *
 *
 * @pre <em>Map</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetMap(Map)
 *    INTEGER*4 Map
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup StateChange
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetMap(TP_OUT EntIndex_t* Map);

/**
 * Retrieve the number of the zone associated with the previous state change.
 *
 * @param Zone
 *   Retreived zone number associated with previous state change
 *
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *
 *
 * @pre <em>Zone</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetZone(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Your state change callback was just called with \ref StateChange_AuxDataAdded.
 * Take action if it was auxiliary data associated with zone 2.
 *
 * @code
 *     LgIndex_t  ArbEnumValue;
 *     EntIndex_t Zone;
 *     if ((TecUtilStateChangeGetArbEnum(&ArbEnumValue) &&
 *          ((AuxDataLocation_e)ArbEnumValue == AuxDataLocation_Zone)) &&
 *         (TecUtilStateChangeGetZone(&Zone) &&
 *          Zone == 2))
 *        {
 *           ....take some knowing aux data was just added
 *           .... to zone Zone.
 *        }
 * @endcode
 *
 * @ingroup StateChange
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetZone(TP_OUT EntIndex_t* Zone);

/**
 * Retrieve a reference to the variable associated with the previous state change.
 *
 * @since
 *   11.0-0-320
 *
 * @param Var
 *   Reference to the variable associated with the previous state change.
 *
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *
 *
 * @pre <em>Var</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetVar(Var)
 *    INTEGER*4 Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup StateChange
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetVar(TP_OUT EntIndex_t* Var);

/**
 * Retrieve one of the P1,P2,P3,P4,P5,P6, or P7 style parameters from the
 * previous style state change.
 *
 * @param Param
 *   The parameter number to retrieve. Must be a number between 1 and 7
 *
 * @param StyleParam
 *   Style parameter retrieved.  DO NOT FREE THIS STRING.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *
 *
 * @pre <em>StyleParam</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 *   Your state change callback was just called with \ref StateChange_Style. Take
 *   action if the first two style parameters are SV_INTERFACE and
 *   SV_USEAPPROXIMATEPLOTS.
 *
 * @code
 *     char *P1;
 *     char *P2;
 *     if (TecUtilStateChangeGetStyleParam(1,&P1) &&
 *         TecUtilStateChangeGetStyleParam(2,&P2) &&
 *         (strcmp(P1,SV_INTERFACE) == 0)             &&
 *         (strcmp(P2,SV_USEAPPROXIMATEPLOTS) == 0))
 *       {
 *           ....take some action.
 *       }
 * @endcode
 *
 * @ingroup StateChange
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetStyleParam(int                 Param,
                                                              TP_OUT const char** StyleParam);


/**
 * Retrieve the P1,P2,P3,P4,P5 style parameters from the
 * previous style state change. As well as the Offset1, Offset2, and
 * ObjectSet parameters.
 *
 * This function is useful since the form is more consistent with the
 * form required for calling TecUtilStyleSetLowLevelX and
 * TecUtilStyleGetLowLevelX
 *
 * @param ArgList
 *   An argument list containing the appropriate entries described below.
 *   Arguments are added to the list via calls to TecUtilArgListAppendXXXX
 *   functions.
 *
 * <ArgListTable>
 * Name:
 *   SV_P1
 * Type:
 *   char **
 * Arg Function:
 *   TecUtilArgListAppendArbParamPtr()
 * Required:
 *   No
 * Default:
 *   NULL
 * Notes:
 *   DO NOT FREE THIS STRING. NULL will be returned if
 *   the previous state change does not have a P1 parameter
 *
 * Name:
 *   SV_P2
 * Type:
 *   char **
 * Arg Function:
 *   TecUtilArgListAppendArbParamPtr()
 * Required:
 *   No
 * Default:
 *   NULL
 * Notes:
 *   DO NOT FREE THIS STRING. NULL will be returned if
 *   the previous state change does not have a P2 parameter
 *
 * Name:
 *   SV_P3
 * Type:
 *   char **
 * Arg Function:
 *   TecUtilArgListAppendArbParamPtr()
 * Required:
 *   No
 * Default:
 *   NULL
 * Notes:
 *   DO NOT FREE THIS STRING. NULL will be returned if
 *   the previous state change does not have a P3 parameter
 *
 * Name:
 *   SV_P4
 * Type:
 *   char **
 * Arg Function:
 *   TecUtilArgListAppendArbParamPtr()
 * Required:
 *   No
 * Default:
 *   NULL
 * Notes:
 *   DO NOT FREE THIS STRING. NULL will be returned if
 *   the previous state change does not have a P4 parameter
 *
 * Name:
 *   SV_P5
 * Type:
 *   char **
 * Arg Function:
 *   TecUtilArgListAppendArbParamPtr()
 * Required:
 *   No
 * Default:
 *   NULL
 * Notes:
 *   DO NOT FREE THIS STRING. NULL will be returned if
 *   the previous state change does not have a P5 parameter
 *
 * Name:
 *   SV_OFFSET1
 * Type:
 *   ArbParam_t *
 * Arg Function:
 *   TecUtilArgListAppendArbParamPtr()
 * Required:
 *   No
 * Default:
 *   NULL
 * Notes:
 *   IMPORTANT:  You must supply a pointer to an ArbParam_t value for this parameter.
 *   You can later reassign the value to a LgIndex_t type if needed (most likely the case).
 *   An offset of -1 will be returned if Offset1 does not apply to
 *   the previous state change
 *
 * Name:
 *   SV_OFFSET2
 * Type:
 *   ArbParam_t *
 * Arg Function:
 *   TecUtilArgListAppendArbParamPtr()
 * Required:
 *   No
 * Default:
 *   NULL
 * Notes:
 *   IMPORTANT:  You must supply a pointer to an ArbParam_t value for this parameter.
 *   You can later reassign the value to a LgIndex_t type if needed (most likely the case).
 *   An offset of -1 will be returned if Offset1 does not apply to
 *   the previous state change
 *
 * Name:
 *   SV_OBJECTSET
 * Type:
 *   Set_pa *
 * Arg Function:
 *   TecUtilArgListAppendArbParamPtr()
 * Required:
 *   No
 * Default:
 *   NULL
 * Notes:
 *   DO NOT FREE THIS SET. NULL will be returned if ObjectSet
 *   does not apply to the previous state change.
 * </ArgListTable>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * @code
 *    const char* P1;
 *    const char* P2;
 *    const char* P3;
 *    const char* P4;
 *    const char* P5;
 *    Set_pa objectSet = NULL;
 *    Arbparam_t offset1;
 *    Arbparam_t offset2;
 *    LgIndex_t  finalOffset1;
 *    LgIndex_t  finalOffset2;
 *    ArgList_pa argList = TecUtilArgListAlloc();
 *    TecUtilArgListAppendArbParamPtr(argList, SV_P1, (ArbParam_t*)&P1);
 *    TecUtilArgListAppendArbParamPtr(argList, SV_P2, (ArbParam_t*)&P2);
 *    TecUtilArgListAppendArbParamPtr(argList, SV_P3, (ArbParam_t*)&P3);
 *    TecUtilArgListAppendArbParamPtr(argList, SV_P4, (ArbParam_t*)&P4);
 *    TecUtilArgListAppendArbParamPtr(argList, SV_P5, (ArbParam_t*)&P5);
 *    TecUtilArgListAppendArbParamPtr(argList, SV_OBJECTSET, (ArbParam_t*)&objectSet);
 *    TecUtilArgListAppendArbParamPtr(argList, SV_OFFSET1, &offset1);
 *    TecUtilArgListAppendArbParamPtr(argList, SV_OFFSET2, &offset2);
 *    TecUtilStateChangeGetInfoX(argList);
 *    finalOffset1 = offset1;
 *    finalOffset2 = offset2;
 *    ... do something with P1, P2, P3, etc ...
 * @endcode
 *
 * @ingroup StateChange
 *
 */
LINKTOADDON void STDCALL TecUtilStateChangeGetInfoX(ArgList_pa ArgList);


/**
 * Inform Tecplot of a state change.
 *
 * @param ArgList
 *   An argument list containing the appropriate entries described below.
 *   Arguments are added to the list via calls to TecUtilArgListAppendXXXX
 *   functions.
 *
 * <ArgListTable>
 * Name:
 *   SV_STATECHANGE
 * Type:
 *   StateChange_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The state change to send to Tecplot. See sub-section "Sending state
 *   changes" in the ADK User's Manual for a list of possible state changes.
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
 *   No, (see below)
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
 *   No, (see below)
 *
 * Name:
 *   SV_INDEX
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Default:
 *   NULL
 * Required:
 *   No, (see below)
 * </ArgListTable>
 *
 * Name:
 *   SV_DATASETUNIQUEID
 * Type:
 *   ArbParam_t
 * Arg Function:
 *   TecUtilArgListAppendArbParam()
 * Default:
 *   The dataset of the current frame.
 * Required:
 *   No
 * Notes:
 *   A unique ID of the dataset whose state is changed.
 *
 * Name:
 *   SV_UNIQUEID
 * Type:
 *   ArbParam_t
 * Arg Function:
 *   TecUtilArgListAppendArbParam()
 * Default:
 *    INVALID_UNIQUE_ID
 * Required:
 *   No
 * Notes:
 *   A unique ID of an object whose state is changed.
 *
 * @par Note:
 *   The SV_VARLIST, SV_ZONELIST and SV_INDEX arguments are required or
 *   optional based on the value of the state change argument. For a
 *   description of when you can and/or must supply these arguments see
 *   sub-section "Sending state changes" in the ADK User's Manual.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStateChangedX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Inform tecplot that variables have been altered.  Tell
 * Tecplot that variable 2 in zones 3 and 4 has been altered.
 * @code
 * Set_pa     ZoneList = NULL;
 * Set_pa     VarList  = NULL;
 * ArgList_pa ArgList  = NULL;
 *
 * ArgList  = TecUtilArgListAlloc();
 * ZoneList = TecUtilSetAlloc(FALSE);
 * VarList  = TecUtilSetAlloc(FALSE);
 *
 * TecUtilSetAddMember(VarList,2,FALSE);
 *
 * TecUtilSetAddMember(ZoneList,3,FALSE);
 * TecUtilSetAddMember(ZoneList,4,FALSE);
 *
 * TecUtilArgListAppendInt(ArgList,    SV_STATECHANGE,      StateChange_VarsAltered);
 * TecUtilArgListAppendSet(ArgList,    SV_ZONELIST,         ZoneList);
 * TecUtilArgListAppendSet(ArgList,    SV_VARLIST,          VarList);
 * TecUtilStateChangedX(ArgList);
 *
 * TecUtilArgListDealloc(&ArgList);
 * TecUtilSetDealloc(&ZoneList);
 * TecUtilSetDealloc(&VarList);
 * @endcode
 *
 * Same example as above but for FORTRAN.
 * @code
 *       INTEGER*4 DummyArgList
 *       pointer (ArgListPtr,DummyArgList)
 *       pointer (ZoneListPtr,DummyZoneList)
 *       INTEGER*4 DummyZoneList
 *       pointer (VarListPtr,DummyVarList)
 *       INTEGER*4 DummyVarList
 *       INTEGER*4 IShowErr,IErr
 *
 *       IShowErr = 0
 *
 *       Call TecUtilArgListAlloc(ArgListPtr)
 *       Call TecUtilSetAlloc(IShowErr,ZoneListPtr)
 *       Call TecUtilSetAlloc(IShowErr,VarListPtr)
 *
 *       IErr = TecUtilSetAddMember(VarListPtr,2,IShowErr)
 *
 *       IErr = TecUtilSetAddMember(ZoneListPtr,3,IShowErr)
 *       IErr = TecUtilSetAddMember(ZoneListPtr,4,IShowErr)
 *
 *       IErr = TecUtilArgListAppendInt(ArgListPtr,
 *      &                               'STATECHANGE'//char(0),
 *      &                               StateChange_VarsAltered)
 *       IErr = TecUtilArgListAppendSet(ArgListPtr,
 *      &                               'ZONELIST'//char(0),
 *      &                               ZoneListPtr)
 *       IErr = TecUtilArgListAppendSet(ArgListPtr,
 *      &                               'VARLIST'//char(0),
 *      &                               VarListPtr)
 *
 *       Call TecUtilStateChangedX(ArgListPtr)
 *
 *       Call TecUtilArgListDealloc(ArgListPtr)
 *       Call TecUtilSetDealloc(ZoneListPtr)
 *       Call TecUtilSetDealloc(VarListPtr)
 * @endcode
 *
 * @ingroup StateChange
 *
 */
LINKTOADDON void STDCALL TecUtilStateChangedX(ArgList_pa ArgList);

/**
 * Include a function in the list of functions to call when the $!EXTENDEDCOMMAND
 * macro command is processed. This allows you to extend Tecplot's macro
 * language, so that commands needed to perform operations in your application
 * or add-on can be included in a Tecplot macro.
 *
 * @param CommandProcessorIDString
 *   A unique string used to determine the function to call when an
 *   $!EXTENDEDCOMMAND macro command is processed. Each application or add-on
 *   should have its own unique ID string. For example, if a file converter
 *   add-on responsible for converting DXF files for Tecplot defines an ID
 *   string of "DXFCONVERTTOOL-1.2" then this same ID string must be used in
 *   the calls to TecUtilMacroRecordExtCommand() and
 *   TecUtilMacroAddCommandCallback().
 *
 * @param MacroCommandCallback
 *   Name of the function to call when the $!EXTENDEDCOMMAND macro command is
 *   processed. The callback function you provide will have the following
 *   syntax:.
 *
 * @return
 *   Returns TRUE if callback has been added. Returns FALSE if the CommandProcessorIDString
 *   is invalid or if there is not enough room to allocate space for an additional
 *   callback (highly unlikely).
 *
 *
 * @pre <em>CommandProcessorIDString</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroAddCommandCallback(
 *   &                   CommandProcessorIDString,
 *   &                   MacroCommandCallback)
 *    CHARACTER*(*)   AddOnIDString
 *    EXTERNAL        MacroCommandCallback
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * The following example shows how an add-on can augment the Tecplot macro
 * language with the commands "GO" and "STOP." First create a function that
 * will be called by Tecplot when a "GO" or "STOP" command is encountered in
 * a macro file.
 *
 * @code
 *   Boolean_t STDCALL ProcessBananaCommands(char *Command,
 *                                           char **ErrMsg)
 *   {
 *     Boolean_t IsOk = TRUE;
 *     if (strcmp(Command,"GO") == 0)
 *       {
 *         // code here to execute a GO command
 *       }
 *     else if (strcmp(Command, "STOP") == 0)
 *       {
 *         // code here to execute a STOP command
 *       }
 *    else
 *       {
 *         *ErrMsg = TecUtilStringAlloc(80, "Error message string");
 *         sprintf(*ErrMsg, "Unknown BANANA command");
 *         IsOk = FALSE;
 *       }
 *     return IsOk;
 *   }
 * @endcode
 *
 * In the initialization code for your add-on, register the function with:
 * @code
 *     .
 *     .
 *     TecUtilMacroAddCommandCallback("BANANA",
 *                                    ProcessBananaCommands);
 *     .
 *     .
 * @endcode
 *
 * @ingroup ScriptSupport
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroAddCommandCallback(const char                *CommandProcessorIDString,
                                                             MacroCommandExtCallback_pf MacroCommandCallback);


/**
 * Instruct Tecplot to record a macro command to the macro file
 * which is currently being recorded.
 *
 * @param CommandProcessorIDString
 *   Unique string to identify command as belonging to a particular command
 *   processor. Use TecUtilMacroAddCommandCallback() to install a callback
 *   function.
 *
 * @param Command
 *   Character string containing the command. This command must be one that
 *   your command processor understands since it will be passed to the
 *   function you register with TecUtilMacroAddCommandCallback().
 *
 * @return
 *   TRUE if successful. FALSE if an I/O error occurs while writing the
 *   command to a file.
 *
 *
 * @pre <em>CommandProcessorIDString</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>Command</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroRecordExtCommand(
 *   &                   CommandProcessorIDString,
 *   &                   Command)
 *    CHARACTER*(*)   AddOnIDString
 *    CHARACTER*(*)   Command
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ScriptSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroRecordExtCommand(const char *CommandProcessorIDString,
                                                           const char *Command);

/**
 * @deprecated
 *   Please use TecUtilMacroRecordExtCommand() instead.
 *
 * @ingroup ScriptSupport
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroRecordAddOnCommand(const char *AddOnIDString,
                                                             const char *Command);


/**
 * Instruct Tecplot to record a macro command, that includes raw data,
 * to the macro file.
 *
 * @param CommandProcessorIDString
 *   Unique string to identify command as belonging to a particular command
 *   processor. Use TecUtilMacroAddCommandCallback() to install a callback
 *   function.
 *
 * @param Command
 *   Character string containing the command. This command must be one that
 *   your command processor understands since it will be passed to the
 *   function you register with TecUtilMacroAddCommandCallback().
 *
 * @param RawData
 *   Character string containing the raw data. This text will follow a RAWDATA
 *   marker in the macro file. Use of newlines to organize the raw data in a
 *   readable fashion is encouraged. The RawData section cannot contain the $!
 *   since the $! marks the start of a Tecplot macro command. The # may be used
 *   in the raw data, however all text following the # up to the following
 *   newline will be discarded when the macro is processed. When the
 *   $!EXTENDEDCOMMAND is later processed by Tecplot the text in the RAWDATA
 *   section will be concatenated to the command string (including a newline
 *   (\\n) to separate the command from the raw data.
 *
 * @return
 *   TRUE if successful; FALSE if an I/O error occurs while writing the command to a file.
 *
 *
 * @pre <em>CommandProcessorIDString</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>Command</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>RawData</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroRecordExtComRaw(
 *   &                   CommandProcessorIDString,
 *   &                   Command,
 *   &                   RawData)
 *    CHARACTER*(*)   CommandProcessorIDString
 *    CHARACTER*(*)   Command
 *    CHARACTER*(*)   RawData
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Record an add-on command that has the text "3.7 9.4" in the RAWDATA section:
 *
 * @code
 *   if (TecUtilMacroIsRecordingActive())
 *     {
 *       TecUtilMacroRecordExtComRaw("MyMacroProcessor",
 *                                   "LOADXY",
 *                                   "3.7 9.4");
 *     }
 * @endcode
 *
 * @ingroup ScriptSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroRecordExtComRaw(const char *CommandProcessorIDString,
                                                          const char *Command,
                                                          const char *RawData);

/**
 * @deprecated
 *   Please use TecUtilMacroRecordExtComRaw() instead.
 *
 * @ingroup ScriptSupport
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroRecordAddOnComRaw(const char *AddOnIDString,
                                                            const char *Command,
                                                            const char *RawData);

/**
 *   Send anything you want to the Tecplot record file.
 *
 * @param Command
 *   Character string to write to the record file. You can send anything you want
 *
 * @return
 *   TRUE if successful; FALSE if an I/O error occurs while writing the command to a file.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroRecordRawCommand(Command)
 *    CHARACTER*(*) Command
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Record commands that will cause Tecplot to loop to animate 3 zones when the macro is played back:
 *
 * @code
 *   if (TecUtilMacroIsRecordingActive())
 *     {
 *       TecUtilMacroRecordRawCommand("$!Loop 3\n"
 *                                    "$!ActiveFieldZones[|Loop|]\n"
 *                                    "$!Redraw\n"
 *                                    "$!EndLoop\n");
 * @endcode
 *
 * @ingroup ScriptSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroRecordRawCommand(const char *Command);


/**
 * Adds a command to the data journal.
 *
 * @par Note:
 *   Using this function will render your layout files incompatibile with
 *   Tecplot 11.2-0-382 or earlier.
 *
 * @param CommandProcessorIDString
 *   The ID string of the command processor. You must have registered a macro
 *   command callback with Tecplot to use this functionality. See
 *   TecUtilMacroAddCommandCallback for more information on registering a
 *   macro command callback
 *
 * @param Instructions
 *   Command Instructions
 *
 * @param RawData
 *   Raw Data. Please see TecUtilMacroRecordExtComRaw() for a description of
 *   the raw data format.
 *
 * @return
 *   Returns TRUE if successful, FALSE otherwise.
 *
 *
 * @pre <em>CommandProcessorIDString</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>Instructions</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>RawData</em>
 *   Pointer must be a valid address or NULL.
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetAddJournalCommand(
 *   &                   AddOnIDString,
 *   &                   Instructions,
 *   &                   RawData)
 *    CHARACTER*(*)   AddOnIDString
 *    CHARACTER*(*)   Instructions
 *    CHARACTER*(*)   RawData
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa
 *   TecUtilMacroAddCommandCallback, TecUtilMacroRecordExtComRaw
 *
 * @ingroup LayoutSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetAddJournalCommand(const char *CommandProcessorIDString,
                                                              const char *Instructions,
                                                              const char *RawData);

/**
 *   Adds a raw macro command to the data journal.
 *
 * @param Command
 *   The raw macro command to add to the journal.
 *
 * @return
 *   Returns TRUE if successful (i.e., the command is valid and could be attached to the journal),
 *   FALSE otherwise.
 *
 *
 * @pre <em>Command</em>
 *   String must have a valid address and non-zero length.
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetAddRawJournalCom(Command)
 *    CHARACTER*(*) Command
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup LayoutSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetAddRawJournalCom(const char *Command);


/**
 * @deprecated
 *   Please use TecUtilDataSetAddJournalCommand() instead.
 *
 * @ingroup DataServices
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetAddPostConvInstr(const char *AddOnIDString,
                                                             const char *Instructions,
                                                             const char *RawData);



/**
 *
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * @ingroup AddOnManagement
 *
 * #internalattributes exclude_alldoc
 */
LINKTOADDON void STDCALL TecUtilDataSetPostReadFinished(Boolean_t IsReadOk);




/**
 *
 *
 * @pre <em>VALID_NON_ZERO_LEN_STR(ConverterIDString)</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * @ingroup AddOnManagement
 *
 * #internalattributes exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilConvAddPostReadCallback(const char                   *ConverterIDString,
                                                             ConverterPostReadCallback_pf  ConverterPostReadCallback);



/**
 *   Stops Tecplot for altering or marking the loaded dataset.
 *
 * @param DoSuspend
 *   A TRUE value to suspend the marking of the dataset and a FALSE value to allow marking.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataSetSuspendMarking(DoSuspend)
 *    INTEGER*4 DoSuspend
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the Marking property to be TRUE:
 *
 * @code
 *   TecUtilDataSetSuspendMarking(TRUE);
 * @endcode
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON void STDCALL TecUtilDataSetSuspendMarking(Boolean_t DoSuspend);


/**
 * Dispatches a Tecplot work area event to the Tecplot engine. The event is
 * queued to be processed at time deemed appropriate by Tecplot.
 *
 * @since
 *   11.0-5-036
 *
 * @param I
 *   I-Location in the work area where the event occurs (in screen coordinates) with 0 being the left
 *   edge of the work area.
 *
 * @param J
 *   J-Location in the work area where the event occurs (in screen coordinates) with 0 being the top
 *   edge of the work area.
 *
 * @param ButtonOrKey
 *   The ButtonOrKey parameter assumes the following: If the event is a button
 *   press, then: ButtonOrKey = 1 ....... Mouse button 1. ButtonOrKey = 2
 *   ....... Mouse button 2. If the event is a key press then: ButtonOrKey = 32
 *   to 127 for ASCII characters on the keyboard.
 *
 * @param Event
 *   Event type. The possible values are: \ref Event_ButtonPress, \ref Event_ButtonRelease,
 *   \ref Event_ButtonDoublePress, \ref Event_Motion, \ref Event_Drag or \ref Event_KeyPress
 *
 * @param IsShifted
 *   Set to TRUE if Shift key is held down.
 *
 * @param IsAlted
 *   Set to TRUE if Alt key is held down.
 *
 * @param IsControlled
 *   Set to TRUE if Ctrl key is held down.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilDispatchWorkAreaEvent(int       I,
                                                      int       J,
                                                      int       ButtonOrKey,
                                                      Event_e   Event,
                                                      Boolean_t IsShifted,
                                                      Boolean_t IsAlted,
                                                      Boolean_t IsControlled);

/**
 * Specifies whether to show the Tecplot toolbar or not.
 *
 * @param Activate
 *   Set to TRUE to show the toolbar, FALSE to hide it
 *
 * <FortranSyntax>
 *   SUBROUTINE TecUtilToolbarActivate(
 *  &           Activate)
 *   INTEGER*4      Activate
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilToolbarActivate(Boolean_t Activate);

/**
 * Specifies whether to show the Tecplot menu or not.
 *
 * @param Activate
 *   Set to TRUE to show the menu, FALSE to hide it
 *
 * <FortranSyntax>
 *   SUBROUTINE TecUtilMenuActivate(
 *  &           Activate)
 *   INTEGER*4      Activate
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since
 *    12.2-1-9808
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilMenuActivate(Boolean_t Activate);


/**
 * Registers a character string to be displayed on the status line for a menu item.
 * If called again on the same menu item, the new string replaces the old one.
 *
 * @since
 *   11.2-0-391
 *
 * @param MenuItem
 *   Specify the menu item for which to register status line help.
 *   This item can be an option, toggle, or submenu, but it cannot be a separator.
 *
 * @param StatusLineHelp
 *   Specify the text shown on the status line for a menu item.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilMenuAddStatusLineHelp(
 *   &           MenuItemPtr,
 *   &           StatusLineHelp)
 *    POINTER         (MenuItemPtr, MenuItem)
 *    CHARACTER*(*)   StatusLineHelp
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the status line help for an existing menu item.
 *
 * @code
 *   extern Menu_pa DoSomething1Option; // created elsewhere
 *   TecUtilMenuAddStatusLineHelp(DoSomething1Option,
 *                                "Do Something New");
 * @endcode
 *
 * @ingroup UserInterface
 *
 */
LINKTOADDON void STDCALL TecUtilMenuAddStatusLineHelp(Menu_pa     MenuItem,
                                                      const char *StatusLineHelp);


/**
 *   Gets the menu handle for Tecplot's main menu. This handle may then be used
 *   with other functions such as TecUtilMenuInsertSubMenu().
 *
 * @return
 *   Returns menu handle for Tecplot's main menu, or NULL if there is no main menu (as
 *   happens when Tecplot is running in batch mode).
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilMenuGetMain(ResultPtr)
 *    POINTER (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilMenuInsertSubMenu() for an example of TecUtilMenuGetMain().
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Menu_pa STDCALL TecUtilMenuGetMain(void);

/**
 * Gets the menu handle for a top-level standard Tecplot menu. This handle may
 * then be used with other functions such as TecUtilMenuInsertOption().
 *
 * @param StandardMenu
 *   Specify the top-level menu for which to get the handle.
 *
 * @return
 *   Returns menu handle for the specified top-level menu, or NULL if there is
 *   no such menu (as happens when Tecplot is running in batch mode or when the
 *   menu has been deleted). Also will return NULL if Tecplot is run with the
 *   -nomainmenu command line options.
 *
 *   IMPORTANT: You MUST check the return value of this function for NULL, and ensure that you do
 *   not call any TecUtilMenu functions with a NULL Menu_pa parameter. Calling a TecUtilMenu
 *   function with a NULL Menu_pa will cause a runtime error.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilMenuGetStandard(
 *   &           StandardMenu,
 *   &           ResultPtr)
 *    INTEGER*4       StandardMenu
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilMenuInsertOption() for an example of TecUtilMenuGetStandard().
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Menu_pa STDCALL TecUtilMenuGetStandard(StandardMenu_e StandardMenu);

/**
 *   Inserts a submenu into a menu. A submenu displays other menu items when it
 *   is selected in the menu system.
 *
 * @param ParentMenu
 *   Specify the menu in which to insert the new submenu.
 *
 * @param InsertPos
 *   Specify the position in the parent menu for the new submenu.
 *   MENU_POSITION_FIRST will put the new submenu first in the parent menu.
 *   MENU_POSITION_LAST will put the new submenu last in the parent menu.
 *
 * @param SubMenuLabel
 *   Specify the text for the new submenu. You may specify a mnemonic for the submenu
 *   option by putting a '&' in front of the desired letter. If you want to specify a
 *   '&' in the submenu itself, use "&&". (A mnemonic is a keyboard short-cut
 *   to access the menu.)
 *
 * @return
 *   Returns menu handle for the new submenu, or NULL if creating the submenu failed (as
 *   happens when Tecplot is running in batch mode). Also will return NULL if Tecplot
 *   is run with the -nomainmenu command line options.
 *
 *   IMPORTANT: You MUST check the return value of this function for NULL, and ensure that you do
 *   not call any TecUtilMenu functions with a NULL Menu_pa parameter. Calling a TecUtilMenu
 *   function with a NULL Menu_pa will cause a runtime error.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilMenuInsertSubMenu(
 *   &           ParentMenuPtr,
 *   &           InsertPos,
 *   &           SubMenuLabel,
 *   &           ResultPtr)
 *    POINTER         (ParentMenuPtr, ParentMenu)
 *    INTEGER*4       InsertPos
 *    CHARACTER*(*)   SubMenuLabel
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Insert a submenu in the main menu called "Extras".
 *   This submenu will appear first in the main menu.
 *   This submenu has a keyboard short-cut of "X".  That is, the user can activate
 *   this submenu by simply pressing the letter "X" while in the main menu.  Since
 *   the option is in the main menu, "Alt+X" will work on some systems as well.
 *
 * @code
 *    Menu_pa MainMenu = TecUtilMenuGetMain();
 *    IsOk = TecUtilMenuInsertSubMenu(MainMenu,
 *                                    MENU_POSITION_FIRST,
 *                                    "E&xtras");
 * @endcode
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Menu_pa STDCALL TecUtilMenuInsertSubMenu(Menu_pa     ParentMenu,
                                                     int         InsertPos,
                                                     const char *SubMenuLabel);

/**
 * Inserts a menu option into a menu. A menu option performs some action when
 * selected in the menu system.
 *
 * @param ParentMenu
 *   Specify the menu in which to insert the new option.
 *
 * @param InsertPos
 *   Specify the position in the parent menu for the new option.
 *   MENU_POSITION_FIRST will put the new option first in the parent menu.
 *   MENU_POSITION_LAST will put the new option last in the parent menu.
 *
 * @param OptionLabel
 *   Specify the text for the new option. You may specify a mnemonic for the
 *   option by putting a '&' in front of the desired letter. If you want to specify a
 *   '&' in the menu option itself, use "&&". (A mnemonic is a keyboard short-cut
 *   to access the menu.) You may specify an accelerator key at the end of the label
 *   by separating it from the rest with a tab ('\\t'). An accelerator key is a key that can
 *   be used from within Tecplot to activate the optionwithout going through
 *   the menu system. Accelerators should be of the form "Ctrl+L" or "Shift+L"
 *   or "Alt+L" or a combination such as "Ctrl+Shift+L" where "L" can be any letter
 *   or number.
 *
 * @param ActivateCallback
 *    Specify callback function to call when new menu option is activated.
 *
 * @param ActivateClientData
 *    Specify data to send to ActivateCallback when called for this new menu option.
 *
 * @return
 *   Returns menu handle for the new menu option, or NULL if creating the menu option failed (as
 *   happens when Tecplot is running in batch mode).
 *
 * <FortranSyntax>
 * C
 * C  Note: Only one menu option callback function can active for each FORTRAN add-on.
 * C  Registering another menu option callback function overwrites the first.
 * C  You may use ClientData to differentiate between different menu options.
 * C
 *    SUBROUTINE TecUtilMenuInsertOption(
 *   &           ParentMenuPtr,
 *   &           InsertPos,
 *   &           OptionLabel,
 *   &           ActivateCallback,
 *   &           ActivateClientDataPtr,
 *   &           ResultPtr)
 *    POINTER         (ParentMenuPtr, ParentMenu)
 *    INTEGER*4       InsertPos
 *    CHARACTER*(*)   OptionLabel
 *    EXTERNAL        ActivateCallback
 *    POINTER         (ActivateClientDataPtr, ActivateClientData)
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Insert into the Tools menu two new options called "Do Something 1" and "Do Something 2".
 * These menu options will appear as the first two options in the Tools menu.
 *
 * First create a DoSomethingCallback function:
 *
 * @code
 *   void STDCALL DoSomethingCallback(ArbParam_t ClientData)
 *   {
 *      // code executed when one of the "Do Something" options is selected in the
 *      // "Tools" menu.  ClientData will be 1 for the "Do Something 1" menu option,
 *      // and ClientData will be 2 for the "Do Something 2" menu option.
 *   }
 * @endcode
 *
 * Elsewhere (probably in the initialization of the add-on) add the menu items to
 * the "Tools" menu.
 *
 * @code
 *   Menu_pa ToolsMenu, DoSomething1Option, DoSomething2Option;
 *   ToolsMenu = TecUtilMenuGetStandard(StandardMenu_Tools);
 *   DoSomething1Option = TecUtilMenuInsertOption(ToolsMenu,
 *                                                MENU_POSITION_FIRST,
 *                                                "Do Something 1",
 *                                                DoSomethingCallback,
 *                                                1);
 *   DoSomething2Option = TecUtilMenuInsertOption(ToolsMenu,
 *                                                2, // second item in menu
 *                                                "Do Something 2",
 *                                                DoSomethingCallback,
 *                                                2);
 * @endcode
 *
 *   To assign keyboard short-cuts to the menu options, use the following code instead.
 *   "Do Something 1" has a keyboard short-cut of '1'.  That is, it
 *   can be accessed quickly while in the "Tools" menu by simply pressing one.  Likewise
 *   "Do Something 2" has a keyboard short-cut of '2'.
 *
 * @code
 *   DoSomething1Option = TecUtilMenuInsertOption(ToolsMenu,
 *                                                MENU_POSITION_FIRST,
 *                                                "Do Something &1",
 *                                                DoSomethingCallback,
 *                                                1);
 *   DoSomething2Option = TecUtilMenuInsertOption(ToolsMenu,
 *                                                2, // second item in menu
 *                                                "Do Something &2",
 *                                                DoSomethingCallback,
 *                                                2);
 * @endcode
 *   To assign keyboard accelerators to the menu option, use the following code instead.
 *   "Do Something 1" still has the keyboard short-cut, but it also has an accelerator
 *   key of "Alt+1".  An accelerator key allows the user to activate the menu option without
 *   accessing the menu.  Pressing "Alt+1" while in the Tecplot main menu will activate
 *   the "Do Something 1" menu option.  Likewise, "Alt+2" activates the "Do Something 2"
 *   menu option.
 *
 * @code
 *   DoSomething1Option = TecUtilMenuInsertOption(ToolsMenu,
 *                                                MENU_POSITION_FIRST,
 *                                                "Do Something &1\tAlt+1",
 *                                                DoSomethingCallback,
 *                                                1);
 *   DoSomething2Option = TecUtilMenuInsertOption(ToolsMenu,
 *                                                2, // second item in menu
 *                                                "Do Something &2\tAlt+2",
 *                                                DoSomethingCallback,
 *                                                2);
 * @endcode
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc, exclude_tcl
  */
LINKTOADDON Menu_pa STDCALL TecUtilMenuInsertOption(Menu_pa                 ParentMenu,
                                                    int                     InsertPos,
                                                    const char             *OptionLabel,
                                                    MenuActivateCallback_pf ActivateCallback,
                                                    ArbParam_t              ActivateClientData);

/**
 *   Inserts a menu toggle into a menu. Like a menu option, a menu toggle performs
 *   some action when selected in the menu system, but a menu toggle also displays
 *   an on/off state.
 *
 * @param ParentMenu
 *   Specify the menu in which to insert the new toggle.
 *
 * @param InsertPos
 *   Specify the position in the parent menu for the new toggle.
 *   MENU_POSITION_FIRST will put the new toggle first in the parent menu.
 *   MENU_POSITION_LAST will put the new toggle last in the parent menu.
 *
 * @param ToggleLabel
 *   Specify the text for the new toggle. You may specify a mnemonic for the
 *   toggle by putting a '&' in front of the desired letter. If you want to specify a
 *   '&' in the menu toggle itself, use "&&". (A mnemonic is a keyboard short-cut
 *   to access the menu.) You may specify an accelerator key at the end of the label
 *   by separating it from the rest with a tab ('\\t'). An accelerator key is a key that can
 *   be used from within Tecplot to activate the menu toggle without going through
 *   the menu system. Accelerators should be of the form "Ctrl+L" or "Shift+L"
 *   or "Alt+L" or a combination such as "Ctrl+Shift+L" where "L" can be any letter
 *   or number.
 *
 * @param ActivateCallback
 *    Specify callback function to call when new menu toggle is activated (either to
 *    turn it on or to turn it off).
 *
 * @param ActivateClientData
 *    Specify data to send to ActivateCallback when called for this new menu toggle.
 *
 * @param GetToggleStateCallback
 *    Specify callback function that will return the current state of the new menu toggle.
 *    This callback is called whenever Tecplot needs to know the checked/unchecked stated
 *    of the toggle. If the callback returns TRUE, the toggle is checked. If the callback
 *    returns FALSE, the toggle is unchecked.
 *
 * @param GetToggleStateClientData
 *    Specify data to send to GetToggleStateCallback when called for this new menu toggle.
 *
 * @return
 *   Returns menu handle for the new menu toggle, or NULL if creating the menu toggle failed (as
 *   happens when Tecplot is running in batch mode).
 *
 * <FortranSyntax>
 * C
 * C  Note: Only one menu toggle activate callback function can active for each FORTRAN add-on.
 * C  Registering another menu toggle activate callback function overwrites the first.
 * C  You may use ClientData to differentiate between different menu toggles.
 * C  Likewise, only one menu toggle get-state callback can be active for each FORTRAN add-on.
 * C
 *    SUBROUTINE TecUtilMenuInsertToggle(
 *   &           ParentMenuPtr,
 *   &           InsertPos,
 *   &           ToggleLabel,
 *   &           ActivateCallback,
 *   &           ActivateClientDataPtr,
 *   &           GetToggleStateCallback,
 *   &           GetToggleStateClientDataPtr,
 *   &           ResultPtr)
 *    POINTER         (ParentMenuPtr, ParentMenu)
 *    INTEGER*4       InsertPos
 *    CHARACTER*(*)   ToggleLabel
 *    EXTERNAL        ActivateCallback
 *    POINTER         (ActivateClientDataPtr, ActivateClientData)
 *    EXTERNAL        GetToggleStateCallback
 *    POINTER         (GetToggleStateClientDataPtr, GetToggleStateClientData)
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Insert into the Tools menu a toggle menu item called "Simple Toggle".
 *   This toggle will appear last in the Tools menu.
 *
 *   First create a SimpleToggleActivate callback function.  This is called
 *   whenever the toggle is selected.
 *
 * @code
 *   // global variable to track the state of the toggle
 *   static Boolean_t SimpleToggleState = TRUE; // checked by default
 *
 *   void STDCALL SimpleToggleActivate(ArbParam_t ClientData)
 *   {
 *      // code executed when the "Simple Toggle" is selected in the
 *      // "Tools" menu.  ClientData will be 0 because that is what was
 *      // past in the call to TeCUtilMenuInsertToggle() below.
 *      //
 *      // For this simple case, we just invert a global variable.
 *      //
 *      SimpleToggleState = !SimpleToggleState;
 *   }
 * @endcode
 *
 *   Next create a SimpleToggleActivate callback function.  This is called
 *   whenever the state of the toggle is needed.
 *
 * @code
 *   Boolean_t STDCALL GetSimpleToggleState(ArbParam_t ClientData)
 *   {
 *      // code executed whenever the state of the "Simple Toggle" is needed.
 *      // ClientData will be 1 because that is what was past in the call
 *      // to TeCUtilMenuInsertToggle() below.
 *      //
 *      // For this simple case, we just look at the value of a global variable.
 *      //
 *      return SimpleToggleState;
 *   }
 * @endcode
 *
 * Elsewhere (probably in the initialization of the add-on) add the toggle to
 * the "Tools" menu.
 *
 * @code
 *   Menu_pa ToolsMenu, SimpleToggle;
 *   ToolsMenu = TecUtilMenuGetStandard(StandardMenu_Tools);
 *   SimpleToggle = TecUtilMenuInsertToggle(ToolsMenu,
 *                                          MENU_POSITION_FIRST,
 *                                          "Simple Toggle",
 *                                          SimpleToggleActivate,
 *                                          0, //ActivateClientData
 *                                          GetSimpleToggleState,
 *                                          1); //GetToggleStateClientData
 * @endcode
 *
 * @par Note:
 *   Menu toggles can have keyboard short-cuts and accelerators just like menu
 *   options.
 *
 * @sa TecUtilMenuInsertOption() for examples.
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc, exclude_tcl
 */
LINKTOADDON Menu_pa STDCALL TecUtilMenuInsertToggle(Menu_pa                       ParentMenu,
                                                    int                           InsertPos,
                                                    const char                   *ToggleLabel,
                                                    MenuActivateCallback_pf       ActivateCallback,
                                                    ArbParam_t                    ActivateClientData,
                                                    MenuGetToggleStateCallback_pf GetToggleStateCallback,
                                                    ArbParam_t                    GetToggleStateClientData);

/**
 * Inserts a separator into a menu. A separator is a line displayed between menu items, it cannot
 * be selected from the menu system.
 *
 * @param ParentMenu
 *   Specify the menu in which to insert the new separator.
 *
 * @param InsertPos
 *   Specify the position in the parent menu for the new separator.
 *   MENU_POSITION_FIRST will put the new separator first in the parent menu.
 *   MENU_POSITION_LAST will put the new separator last in the parent menu.
 *
 * @return
 *   Returns menu handle for the new separator, or NULL if creating the separator failed (as
 *   happens when Tecplot is running in batch mode).
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilMenuInsertSeparator(
 *   &           ParentMenuPtr,
 *   &           InsertPos,
 *   &           ResultPtr)
 *    POINTER         (ParentMenuPtr, ParentMenu)
 *    INTEGER*4       InsertPos
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * First create a DoSomethingCallback function:
 *
 * @code
 *   void DoSomethingCallback(ArbParam_t ClientData)
 *   {
 *      // code executed when one of the "Do Something" options is selected in the
 *      // "Tools" menu.  ClientData will be 1 for the "Do Something 1" menu option,
 *      // and ClientData will be 2 for the "Do Something 2" menu option.
 *   }
 * @endcode
 *
 * Add some options to the top of the "Tools" menu and then separate them
 * from the rest of that menu by including a separator.
 *
 * @code
 *   Menu_pa ToolsMenu, DoSomething1Option, DoSomething2Option, Separator;
 *   ToolsMenu = TecUtilMenuGetStandard(StandardMenu_Tools);
 *   DoSomething1Option = TecUtilMenuInsertOption(ToolsMenu,
 *                                                MENU_POSITION_FIRST,
 *                                                "Do Something 1",
 *                                                DoSomethingCallback,
 *                                                1);
 *   DoSomething2Option = TecUtilMenuInsertOption(ToolsMenu,
 *                                                2, // second item in menu
 *                                                "Do Something 2",
 *                                                DoSomethingCallback,
 *                                                2);
 *   Separator = TecUtilMenuItemInsertSeparator(ToolsMenu,
 *                                              3); // third item in the menu
 * @endcode
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Menu_pa STDCALL TecUtilMenuInsertSeparator(Menu_pa ParentMenu,
                                                       int     InsertPos);

/**
 *   Inserts a standard Tecplot menu into another menu as a submenu.
 *
 * @param ParentMenu
 *   Specify the menu in which to insert the standard menu.
 *
 * @param InsertPos
 *   Specify the position in the parent menu for the standard menu.
 *   MENU_POSITION_FIRST will put the standard menu first in the parent menu.
 *   MENU_POSITION_LAST will put the standard menu last in the parent menu.
 *
 * @param StandardMenu
 *   Specify the top-level menu for which to get the handle.
 *   This menu cannot exist already in Tecplot.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilMenuInsertStandard(
 *   &           ParentMenuPtr,
 *   &           InsertPos,
 *   &           ResultPtr)
 *    POINTER         (ParentMenuPtr, ParentMenu)
 *    INTEGER*4       InsertPos
 *    INTEGER*4       StandardMenu
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Move the Edit menu to be the first menu in the system.  First delete the
 *   Edit menu if it exists, then readd it to the menu system as the first
 *   item in the main menu.
 *
 * @code
 *   Menu_pa MainMenu, EditMenu;
 *   MainMenu = TecUtilMenuGetMain();
 *   EditMenu = TecUtilMenuGetStandard(StandardMenu_Edit);
 *   if ( EditMenu != NULL )
 *     TecUtilMenuDelete(&EditMenu);
 *   TecUtilMenuInsertStandard(MainMenu,
 *                             MENU_POSITION_FIRST,
 *                             StandardMenu_Edit);
 * @endcode
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilMenuInsertStandard(Menu_pa        ParentMenu,
                                                   int            InsertPos,
                                                   StandardMenu_e StandardMenu);

/**
 *   Registers a function that will determine the sensitivity of a menu item (i.e.,
 *   whether the item is enabled for user input or grayed out).  If this function is
 *   not used for a particular menu item, the item will always be available.  If called
 *   again on the same menu item, the new function replaces the old one.
 *
 * @param MenuItem
 *   Specify the menu item for which to register a sensitivity
 *   function. This item can be a option, toggle, or submenu, but it cannot be a separator.
 *
 * @param GetSensitivityCallback
 *    Specify callback function to call to determine the sensitivity of the menu item.  This
 *    function returns TRUE if the menu is sensitive, and FALSE if the menu is insensive (gray).
 *
 * @param GetSensitivityClientData
 *    Specify data to send to GetSensitivityCallback when called for the specified menu item.
 *
 * <FortranSyntax>
 * C
 * C  Note: Only one menu sensitivity callback function can active for each FORTRAN add-on.
 * C  Registering another menu sensitivity callback callback function overwrites the first.
 * C  You may use ClientData to differentiate between different menu items.
 * C
 *    SUBROUTINE TecUtilMenuRegisterSensitivityCallback(
 *   &           MenuItemPtr,
 *   &           InsertPos,
 *   &           ToggleLabel,
 *   &           ActivateCallback,
 *   &           ActivateClientDataPtr,
 *   &           GetToggleStateCallback,
 *   &           GetToggleStateClientDataPtr,
 *   &           ResultPtr)
 *    POINTER         (MenuItemPtr, MenuItem)
 *    EXTERNAL        GetSensitivityCallback
 *    POINTER         (GetSensitivityClientDataPtr, GetSensitivityClientData)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the sensitivities of two menu items.  The first will be available only when
 *   the frame is 3D.  The second will be available only when the frame is a polar
 *   line plot.
 *
 *   First create a sensitivity callback function.  This one uses
 *   the ClientData parameter to check different plot types.
 *
 * @code
 *   Boolean_t STDCALL CheckIfCurrentFrameIsOfType(ArbParam_t ClientData)
 *   {
 *     PlotType_e PlotType = (PlotType_e)ClientData;
 *     return TecUtilFrameGetPlotType()==PlotType;
 *   }
 * @endcode
 *
 *   Then add the sensitivity callback to the menu item.  Usually this is
 *   done when the item is created.
 *
 * @code
 *   extern Menu_pa DoSomething1Option, DoSomething2Option; // created elsewhere
 *   TecUtilMenuRegisterSensitivityCallback(DoSomething1Option,
 *                                          CheckIfCurrentFrameIsOfType,
 *                                          (ArbParam_t)PlotType_Cartesian3D);
 *   TecUtilMenuRegisterSensitivityCallback(DoSomething2Option,
 *                                          CheckIfCurrentFrameIsOfType,
 *                                          (ArbParam_t)PlotType_PolarLine);
 * @endcode
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc, exclude_tcl
 */
LINKTOADDON void STDCALL TecUtilMenuRegisterSensitivityCallback(Menu_pa                       MenuItem,
                                                                MenuGetSensitivityCallback_pf GetSensitivityCallback,
                                                                ArbParam_t                    GetSensitivityClientData);

/**
 * Deletes a menu item. If that menu item is a submenu, all of its sub menu
 * items are deleted as well. Deleting submenus can delete the menu items of
 * other add-ons and may make those add-ons inaccessible.
 *
 * @par Note:
 *   This function should not be called as a direct response to a menu
 *   selection event as it will delete the menu system out from under it and
 *   produce unpredictable results. To handle this case register an on-idle
 *   callback to clear the menu system. See TecUtilOnIdleQueueAddCallback for
 *   registering an on-idle callback.
 *
 * @param MenuItemPtr
 *   Specify a pointer to the menu item to delete. This item can be an option,
 *   toggle, submenu, or separator.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilMenuDelete(MenuItemPtr)
 *    POINTER (MenuItemPtr, MenuItem)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * To delete the File menu.
 *
 * @code
 *   Menu_pa FileMenu = TecUtilMenuGetStandard(StandardMenu_File);
 *   TecUtilMenuDelete(&FileMenu);
 * @endcode
 *
 * This code does the same thing using as TecUtilMenuClearAll(). The main menu
 * always exists, so TecUtilMenuDelete() on the main menu only deletes the
 * submenus of the main menu.
 *
 * @code
 *   Menu_pa MainMenu = TecUtilMenuGetMain();
 *   TecUtilMenuDelete(&MainMenu);
 *   // main menu is still around, so add an item to it
 *   TecUtilMenuInsertOption(MainMenu,
 *                           MENU_POSITION_FIRST,
 *                           "Do Something &1\tAlt+1",
 *                           DoSomethingCallback,
 *                           1);
 * @endcode
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilMenuDelete(TP_RECEIVES_GIVES Menu_pa* MenuItemPtr);

/**
 * Remove all menus, submenus, and menu items from the Tecplot menu bar. This
 * will clear menu items added by other add-ons, making those add-ons
 * inaccessible. The add-on containing this call should either be the only
 * add-on loaded into Tecplot, or it should be loaded into Tecplot first and it
 * should clear the menus during initialization before the other add-ons are
 * loaded. For more control over which menu items are deleted, use
 * TecUtilMenuDelete() instead.
 *
 * @par Note:
 *   This function should not be called as a direct response to a menu
 *   selection event as it will delete the menu system out from under it and
 *   produce unpredictable results. To handle this case register an on-idle
 *   callback to clear the menu system. See TecUtilOnIdleQueueAddCallback for
 *   registering an on-idle callback.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilMenuClearAll()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * To wipe out Tecplot's standard menu and replace it with a single option
 * called "Banana" which is located under a menu called "Go":
 *
 * @code
 *   TecUtilMenuClearAll():
 *   IsOk = TecUtilMenuAddOption("&Go", "Banana", 'B', BananaCallBack);
 * @endcode
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilMenuClearAll(void);



/****************************************************************************
 *          DataSet Converters and Loaders.                                 *
 *                                                                          *
 * A Converter is an add-on that connects to tecplot in a way such that      *
 * tecplot uses its own file/io dialogs to read the non-Tecplot data into   *
 * tecplot and the converter is a simple function that only knows how       *
 * to convert from its own data format into tecplot's binary format.        *
 *                                                                          *
 * Converters are registered with tecplot by calling:                       *
 *                                                                          *
 *              TecUtilImportAddConverter()                                 *
 *                                                                          *
 * An loader works like this:                                               *
 *     - TecUtilMenuAddOption() is called to register a callback            *
 *       (most often used to bring up a dialog) to the loader add-on        *
 *                                                                          *
 *     - The loader registers itself with tecplot by calling                *
 *       TecUtilImportAddLoader()                                           *
 *                                                                          *
 *     When a request is made to use the loader function:                   *
 *                                                                          *
 *        - The loader creates a new dataset and loads the data into        *
 *          tecplot by hand.                                                *
 *                                                                          *
 *        - The loader then calls TecUtilImportSetLoaderInstr()             *
 *          to assign the specific instructions needed to load the data     *
 *          into the current dataset. These instructions will be included   *
 *          in any layout files written out.                                *
 ****************************************************************************/
/**
 * Register a data set converter with Tecplot. This will add an option to the
 * list of data imports accessed via the File/Import menu option. See Section
 * 9.2, "Data Set Loaders," in the ADK User's Manual for a discussion of data
 * set loaders.
 *
 * @param ConverterCallback
 *   Name of the function to call to convert data to the Tecplot binary format.
 *
 * @param ConverterName
 *   Unique name given to the data set converter. This name is used in the list
 *   of importers in the dialog launched by choosing File/Import. If a layout
 *   file is created the $READDATASET macro command will also use this name to
 *   identify the converter to use.
 *
 * @param FNameExtension
 *   This is the file name extension used by files converted with this data set
 *   converter
 *
 * @return
 *   Returns TRUE if the data set converter is added.
 *
 *
 * @pre <em>ConverterName</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImportAddConverter(
 *   &                   ConverterCallback,
 *   &                   ConverterName,
 *   &                   FNameExtension)
 *    POINTER         (ConverterCallbackPtr, ConverterCallback)
 *    CHARACTER*(*)   ConverterName
 *    CHARACTER*(*)   FNameExtension
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   An add-on is created that has the following data set converter function:
 *
 * @code
 *   Boolean_t STDCALL ConvertBananaData(char  *DataFName,
 *                                       char  *TempBinFName,
 *                                       char **MessageString);
 *   {
 *      Boolean_t IsOk = TRUE;
 *      //
 *      // Code here to open DataFName,
 *      // read in the data and write out a binary
 *      // Tecplot datafile to TempBinFName using
 *      // calls to TecIO TECXxx functions. If there is
 *      // a problem, call TecUtilStringAlloc() on
 *      // MessageString, supply a message describing the
 *      // issue, and set ISOk to FALSE.
 *      //
 *      return (IsOk);
 *   }
 *   The call to register the data set converter with Tecplot is then accomplished using the following:
 *   .
 *   .
 *       IsOk = TecUtilImportAddConverter(ConvertBananaData,
 *                                        "BANANA",
 *                                        "*.ban");
 * @endcode
 *
 * @ingroup AddOnLoaders
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilImportAddConverter(DataSetConverter_pf  ConverterCallback,
                                                        const char          *ConverterName,
                                                        const char          *FNameExtension);


/**
 *   Register a data set loader with Tecplot. This will add an option to the list of data imports
 *   accessed via the File/Import menu option. Data set loaders are more complex than data set
 *   converters, but provide you with greater flexibility in terms of the graphical user interface and
 *   how the data can be retrieved. See Section 9.2, "Data Set Loaders," in the ADK User's Manual for
 *   a discussion of data set loaders.
 *
 * @param LoaderCallback
 *   Function to call to load non-Tecplot format data into Tecplot. The data set loader
 *   itself calls this function when a request is made to load non-Tecplot format data in via the user
 *   interface. Tecplot also calls this function when processing a $!READDATASET macro command that
 *   identifies this loader.
 *
 * @param DataSetLoaderName
 *   Unique name given to the DataSet Loader. This name is used in the list of importers in the dialog
 *   launched by choosing File/Import. If a layout file is created, the $READDATASET macro command
 *   will also use this name to identify the loader to use.
 *
 * @param LoaderSelectedCallback
 *   Function that is called when the user selects this data set loader from the list of
 *   importers in the File/Import dialog. This function typically will launch a custom dialog to
 *   prompt the user to identify the data to be loaded. If NULL Tecplot will not register
 *   a callback or put an entry in the File/Import dialog.
 *
 * @param InstructionOverrideCallback
 *   Name of the function to call when the user chooses to override the data source for a given data
 *   set when a layout file is being read in. If set to NULL then Tecplot will issue an error message
 *   stating that this operation is not available. If provided, this function typically will launch a
 *   dialog that shows the user what the current settings are to load the data and provide a means by
 *   which the user can alter these instructions. The Instructions stringlist is updated according to
 *   changes made by the user and the new information is then used to load the data.
 *
 * @return
 *   Returns TRUE if the data set loader is added.
 *
 *
 * @pre <em>DataSetLoaderName</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImportAddLoader(
 *   &                   LoaderCallback,
 *   &                   DataSetLoaderName,
 *   &                   LoaderSelectedCallback,
 *   &                   InstructionOverrideCallback)
 *    POINTER         (LoaderCallbackPtr, LoaderCallback)
 *    CHARACTER*(*)   DataSetLoaderName
 *    POINTER         (LoaderSelectedCallbackPtr, LoaderSelectedCallback)
 *    POINTER         (InstructionOverrideCallbackPtr, InstructionOverrideCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * An add-on is created that has the following data set loader function:
 * @code
 *   Boolean_t STDCALL LoadBananaData(StringList_pa Instructions)
 *   {
 *     Boolean_t IsOk = TRUE;
 *     //
 *     // Add code to scan through instructions
 *     // and load the data.  When done, inform
 *     // Tecplot about the instructions used to
 *     // load the data.
 *     //
 *      if (IsOk)
 *        TecUtilImportSetLoaderInstr("BANANA", Instructions);
 *      return (IsOk);
 *   }
 * @endcode
 *
 * A function is also created to handle user requests to use the loader from the File/Import dialog:
 * @code
 *   void STDCALL BananaLoaderDialog(void)
 *     {
 *      //
 *      // Launch a custom dialog to prompt the
 *      // user to identify the data to be loaded.
 *      //
 *     }
 * @endcode
 *
 * A function is also created to handle user requests to modify the
 * instructions. This function is optional.
 * @code
 *   Boolean_t STDCALL OverrideBananaInstructions(StringList_pa Instructions)
 *     {
 *        Boolean_t IsOk = TRUE;
 *        //
 *        // Code here to view the current instructions and present
 *        // an interface to the user to change them.
 *        //
 *         return (IsOk);
 *     }
 * @endcode
 *
 *   The call to register the data set loader with Tecplot is then accomplished using the following:
 * @code
 *      .
 *      .
 *      IsOk = TecUtilImportAddLoader(LoadBananaData,
 *                                    "BANANA",
 *                                    BananaLoaderDialog,
 *                                    OverrideBananaInstructions);
 * @endcode
 *
 * @ingroup AddOnLoaders
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilImportAddLoader(DataSetLoader_pf                    LoaderCallback,
                                                     const char                         *DataSetLoaderName,
                                                     DynamicMenuCallback_pf              LoaderSelectedCallback,
                                                     DataSetLoaderInstructionOverride_pf InstructionOverrideCallback);


/**
 * Register a data set loader with Tecplot. This will add an option to the list of data imports
 * accessed via the File/Import menu option. Data set loaders are more complex than data set
 * converters, but provide you with greater flexibility in terms of the graphical user interface and
 * how the data can be retrieved. See the ADK User's Manual for a discussion of data set loaders.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 *
 * <ArgListTable>
 *
 * Name:
 *   SV_FILENAMEEXTENSIONS
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   "*"
 * Required:
 *   No
 * Notes:
 *    A spaced separated string of file extensions used by the loaders. For example "*.dat *.plt".
 *
 * Name:
 *   SV_ALLOWMULTIFILESELECTION
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Indicates if the loader handles multiple files at once for its loader instructions.
 *
 * Name:
 *   SV_ALLOWADVANCEDOPTIONS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Indicates if the loader provides an its own dialog to handle advanced loader options.
 *
 * Name:
 *   SV_ALLOWAPPENDING
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()()
 * Default:
 *   FALSE for V1 and V2 loaders. Ignored for V3 loaders, where it will always be set to TRUE.
 * Required:
 *   No
 * Notes:
 *   Indicates if the loader allows appending of data.
 *   This option is only valid for V1 and V2 loaders.
 *   This will always be set to TRUE if a V3 loader is being registered.
 *
 * Name:
 *   SV_LOADERCALLBACKVERSION
 * Type:
 *   LoaderCallbackVersion_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   LoaderCallbackVersion_V1
 * Required:
 *   No
 * Notes:
 *   API version of the family of loader callbacks registered with this function. Valid values are
 *   LoaderCallbackVersion_V1 for compatibility with DataSetLoader_pf, DynamicMenuCallback_pf,
 *   DataSetLoaderInstructionOverride_pf callbacks and LoaderCallbackVersion_V2 for compatibility
 *   with DataLoader_pf, DataLoaderSelected_pf, DataLoaderInstructionOverride_pf. See SV_CLIENTDATA
 *   for registering client data with the V2 family of callbacks.
 *
 * Name:
 *   SV_LOADERSELECTEDCALLBACK
 * Type:
 *   DynamicMenuCallback_pf for V1 and DataLoaderSelected_pf for V2
 * Arg Function:
 *   TecUtilArgListAppendFunction() 
 * Required:
 *   Yes
 * Notes:
 *   Function that is called when the user selects this data set loader from the list of
 *   importers in the File/Import dialog. Functions of type DynamicMenuCallback_pf
 *   typically will launch a custom dialog to prompt the user to identify the data to be loaded. 
 *   Functions of type DataLoaderSelected_pf it will require a list of files to be opened
 *   and a flag to launch a custom dialog.
 *   If NULL Tecplot will not register a callback or put an entry in the File/Import dialog.
 *
 * Name:
 *   SV_LOADERNAME
 * Type:
 *   const char*
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   Unique name given to the DataSet Loader. If a layout file is created, the $READDATASET macro command
 *   will also use this name to identify the loader to use.
 *
 * Name:
 *   SV_LOADERCALLBACK
 * Type:
 *   DataSetLoader_pf for V1 and DataLoader_pf for V2
 * Arg Function:
 *   TecUtilArgListAppendFunction() 
 * Required:
 *   Yes
 * Notes:
 *   Function to call to load non-Tecplot format data into Tecplot. Tecplot also calls this function 
 *   when processing a $!READDATASET macro command that identifies this loader.
 *
 * Name:
 *   SV_LOADERINSTRUCTIONOVERRIDECALLBACK
 * Type:
 *   DataSetLoaderInstructionOverride_pf for V1 and DataLoaderInstructionOverride_pf for V2
 * Arg Function:
 *   TecUtilArgListAppendFunction() 
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Name of the function to call when the user chooses to override the data source for a given data
 *   set when a layout file is being read in. If set to NULL then Tecplot will issue an error message
 *   stating that this operation is not available. If provided, this function typically will launch a
 *   dialog that shows the user what the current settings are to load the data and provide a means by
 *   which the user can alter these instructions. The Instructions string list is updated according
 *   to changes made by the user and the new information is then used to load the data.
 *
 * Name:
 *   SV_CLIENTDATA
 * Type:
 *   ArbParam_t
 * Arg Function:
 *   TecUtilArgListAppendArbParam()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   Client data sent to the V2 family of registered callbacks. See note for
 *   SV_LOADERCALLBACKVERSION.
 *
 * </ArgListTable>
 *
 * @return
 *   Returns TRUE if the data set loader is added.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * @ingroup AddOnLoaders
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilImportAddLoaderX(ArgList_pa ArgList);

/**
 * Inform Tecplot about the instructions used to load the current data set. It
 * is assumed that the current data set was loaded via a data set loader. The
 * current frame must have an attached data set when this function is used.
 *
 * IMPORTANT: Version 3 data loader add-ons must call this function if and only if the data was successfully loaded. 
 * If the data was not successfully loaded, do not call this function. 
 * If the data was successfully loaded, calling this function is required.
 *
 * @param DataSetLoaderName
 *   Unique loader name. This same name must be used in
 *   TecUtilImportAddLoader().
 *
 * @param Instructions
 *   Instructions used to load the current data set
 *
 * @return
 *   Returns TRUE if the instructions were successfully loaded.
 *
 *
 * @pre <em>DataSetLoaderName</em>
 *   String must have a valid address and non-zero length.
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImportSetLoaderInstr(
 *   &                   DataSetLoaderName,
 *   &                   InstructionsPtr)
 *    CHARACTER*(*)   DataSetLoaderName
 *    POINTER         (InstructionsPtr, Instructions)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup AddOnLoaders
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilImportSetLoaderInstr(const char    *DataSetLoaderName,
                                                          StringList_pa  Instructions);

/**
 * Instruct Tecplot to reset the journal with the specified loader instructions and thereby
 * associate the dataset with the layout. This function is typically called by an exporter after
 * successfully writing a dataset and before registering its write instructions to the journal.
 * The current frame must have an attached data set when this function is used.
 *
 * @param DataSetLoaderName
 *   Unique loader name. This same name must be used in TecUtilImportAddLoader().
 *
 * @param Instructions
 *   Instructions used to load the data into the current data set.
 *
 * @return
 *   Returns TRUE if the instructions were successfully loaded.
 *
 *
 * @pre <em>DataSetLoaderName</em>
 *   String must have a valid address and non-zero length.
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImportSetLoaderInstr(
 *   &                   DataSetLoaderName,
 *   &                   InstructionsPtr)
 *    CHARACTER*(*)   DataSetLoaderName
 *    POINTER         (InstructionsPtr, Instructions)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup AddOnLoaders
 */
LINKTOADDON Boolean_t STDCALL TecUtilImportResetLoaderInstr(const char    *DataSetLoaderName,
                                                            StringList_pa Instructions);

/**
 * Writes a $!READDATASET macro command to the macro file if macro
 * recording is on.
 *
 * @par Note:
 *   Since TecUtilImportSetLoaderInstr() will automatically call this function,
 *   you normally do not need to call this function.  If you are writing a
 *   loader that does not use an instruction string, that is it does not call
 *   TecUtilImportSetLoaderInstr(), then you should call this function before
 *   displaying your data.
 *
 * @param DataSetLoaderName
 *   Unique loader name. This same name must be used when calling
 *   TecUtilImportAddLoader().
 *
 * @param Instructions
 *   Instructions used to load the current data set. If you are not calling
 *   TecUtilImportSetLoaderInstr(), then typically this would be the filename
 *   which was used to load your data.
 *
 * @return
 *   Returns TRUE if the instructions were written, FALSE otherwise
 *
 *
 * @pre <em>VALID_REF(DataSetReader)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImportWriteLoaderInstr(
 *   &                   DataSetLoaderName,
 *   &                   InstructionsPtr)
 *    CHARACTER*(*)   DataSetLoaderName
 *    POINTER         (InstructionsPtr, Instructions)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set up an instruction containing a filename and write a $!READDATASET macro command to the current
 *   macro file:
 *
 * @code
 *   StringList_pa Instructs = TecUtilStringListAlloc();
 *   TecUtilStringListAppendString(Instructs, "myfile.dat");
 *   TecUtilImportWriteLoaderInstr("BANANA", Instructs);
 *   TecUtilStringListDealloc(&Instructs);
 * @endcode
 *
 * @ingroup AddOnLoaders
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilImportWriteLoaderInstr(const char   *DataSetLoaderName,
                                                            StringList_pa Instructions);



/**
 * Register a data set writer with Tecplot. This will add an option to the list of data writers
 * accessed via the File/Write Dataset menu option. 
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 *
 * <ArgListTable>
 *
 * Name:
 *   SV_FILENAMEEXTENSIONS
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   "*"
 * Required:
 *   No
 * Notes:
 *    A spaced separated string of file extensions used by the writer.  Almost always is a single extension (e.g. "*.plt")
 *
 * Name:
 *   SV_DATASETWRITERSELECTEDCALLBACK
 * Type:
 *   DataWriterSelectedCallback_pf
 * Arg Function:
 *   TecUtilArgListAppendFunction() 
 * Required:
 *   Yes
 * Notes:
 *   Function that is called when the user selects this data set writer from the list of
 *   writers in the File/Write Datset dialog. 
 *
 * Name:
 *   SV_DATASETWRITERNAME
 * Type:
 *   const char*
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   Unique name given to the DataSet writer. 
 *
 * Name:
 *   SV_CLIENTDATA
 * Type:
 *   ArbParam_t
 * Arg Function:
 *   TecUtilArgListAppendArbParam()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   Client data sent to the DataWriterSelectedCallback
 *
 * </ArgListTable>
 *
 * @return
 *   Returns TRUE if the data set writer is added.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * @since 14.1
 *
 * @ingroup AddOnLoaders
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetAddWriterX(ArgList_pa ArgList);

/**
 * Get the number of registered foreign writers (internal
 * tecplot writer is not included in the count).
 *
 * @return
 *     Returns the number of registered foreign writers.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON int STDCALL TecAppDataSetWriterGetNumRegisteredForeignWriters(void);

/**
 * Get the name of a registered foreign writer.
 *
 * @param ForeignWriter
 *     The number of the registered foreign writer (1-based).
 *
 * @param Name
 *     A pointer to a character pointer with the name of the writer.  You must
 *     free this string with TecUtilStringDealloc().
 *
 * @return
 *     TRUE if the name was retrieved correctly, FALSE otherwise.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppDataSetWriterGetForeignWriterName(int             ForeignWriter,
                                                                      TP_GIVES char** Name);

/**
 * Get the filter (extensions) registered for a foreign writer.
 *
 * @param ForeignWriter
 *     The number of the registered foreign writer (1-based).
 *
 * @param Extensions
 *     A pointer to a character pointer with the extensions for the writer.  You must
 *     free this string with TecUtilStringDealloc().
 *
 * @return
 *     TRUE if the extensions were retrieved correctly, FALSE otherwise.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppDataSetWriterGetForeignWriterExtensions(int             ForeignWriter,
                                                                            TP_GIVES char** Extensions);

/**
 * Get the registered writer selected callback.
 *
 * @param ForeignWriter
 *     The number of the registered foreign writer (1-based).
 *
 * @return callback
 *     The function to call when the writer is selected.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void* STDCALL TecAppDataSetWriterGetForeignWriterSelectedCallback(int ForeignWriter);

/**
 * Returns client data associated with a foreign writer.
 *
 * @param ForeignWriter
 *     The number of the registered foreign writers (1-based).
 *
 * @param ClientData
 *     A pointer to an ArbParam_t variable to receive the client data.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON ArbParam_t* STDCALL TecAppDataSetWriterGetForeignWriterClientData(int ForeignWriter);

/**
 * @deprecated
 *   Please use TecUtilAddOnRegister() instead.
 *
 * @ingroup AddOnManagement
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilAddOnRegisterInfo(const char *OfficialName,
                                                  const char *Version,
                                                  const char *Author);


/**
 * Register information about your add-on with Tecplot. This information will
 * mainly be used in an add-on information dialog accessible via the Help menu
 * option.
 *
 * @par Note:
 *   This function must be called from within your add-on initialization
 *   function, and cannot be called from anywhere else.
 *
 * @param TecplotBaseVersionNumber
 *   Usually this is a hard coded integer value reflecting the version number
 *   of Tecplot at the time the add-on was written. By providing this number
 *   Tecplot can make assumptions about what capabilities the add-on writer was
 *   aware of at the time of writing. For example, by supplying a value of 100
 *   Tecplot assumes that the add-on knows how to deal with shared variables
 *   since they were first introduced in base version 10. For Tecplot version
 *   11 (aka Tecplot 360) use a value of 110.
 *
 * @param OfficialName
 *   The official name of your add-on.
 *
 * @param Version
 *   A string indicating the version of your add-on.
 *
 * @param Author
 *   A string indicating the author of the add-on (usually the company name).
 *
 *
 * @pre <em>TecplotBaseVersionNumber >= 100</em>
 *   Tecplot version number must be at least 100.
 *
 * @pre <em>OfficialName</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Version</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Author</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>AddOnBeingInitialized</em>
 *   This function may only be called during add-on initialization.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAddOnRegister(
 *   &           TecplotBaseVersionNumber,
 *   &           OfficialName,
 *   &           Version,
 *   &           Author,
 *   &           ResultPtr)
 *    INTEGER*4       TecplotBaseVersionNumber
 *    CHARACTER*(*)   OfficialName
 *    CHARACTER*(*)   Version
 *    CHARACTER*(*)   Author
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * To register an add-on called Circular Stream from Tecplot, Inc.:
 * @code
 *   AddOn_pa AddOnID;
 *   void InitTecAddOn(void)
 *   {
 *     TecUtilLockOn();
 *     AddOnID=TecUtilAddOnRegister(100,"Circular Stream",
 *                                  "1.0 - 05/01/1998",
 *                                  "Tecplot, Inc.");
 *     // other initialization
 *     TecUtilLockOff();
 *   }
 * @endcode
 *
 * @ingroup AddOnManagement
 *
 */
LINKTOADDON AddOn_pa STDCALL TecUtilAddOnRegister(int         TecplotBaseVersionNumber,
                                                  const char *OfficialName,
                                                  const char *Version,
                                                  const char *Author);
/**
 * Query Tecplot's list of loaded addons for the specified add-on's version and
 * author strings.
 *
 * @param OfficialName
 *   Official name of the add-on. This is the same unique name with which an
 *   add-on was registered.
 *
 * @param Version
 *   A pointer to a character pointer. If the add-on is loaded, *Version is
 *   assigned a copy of the add-on's version string, otherwise a value of NULL
 *   is assigned. You must free this string with TecUtilStringDealloc().
 *
 * @param Author
 *   A pointer to a character pointer. If the add-on is loaded, *Author is
 *   assigned a copy of the add-on's version string, otherwise a value of NULL
 *   is assigned. You must free this string with TecUtilStringDealloc().
 *
 * @return
 *   Returns TRUE if the add-on is loaded, FALSE if not.
 *
 *
 * @pre <em>OfficialName</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>Version</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>Author</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAddOnGetRegisteredInfo(
 *   &                   OfficialName,
 *   &                   Version,
 *   &                   VersionLength,
 *   &                   Author,
 *   &                   AuthorLength)
 *    CHARACTER*(*)   OfficialName
 *    CHARACTER*(*)   Version
 *    INTEGER*4       VersionLength
 *    CHARACTER*(*)   Author
 *    INTEGER*4       AuthorLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   To find out if the add-on Circular Stream is loaded into Tecplot:
 *
 * @code
 *   char *version = NULL;
 *   char *author = NULL;
 *   if (TecUtilAddOnGetRegisteredInfo("Circular Stream", &version, &author))
 *   {
 *      // Circular Stream loaded, do something with it
 *      TecUtilStringDealloc(&version);
 *      TecUtilStringDealloc(&author);
 *   }
 * @endcode
 *
 * @ingroup AddOnManagement
 */
LINKTOADDON Boolean_t STDCALL TecUtilAddOnGetRegisteredInfo(const char*     OfficialName,
                                                            TP_GIVES char** Version,
                                                            TP_GIVES char** Author);


/**
 *
 *
 * @pre <em>VALID_REF(ForeignLibID)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(AddOnList)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>AddOnBeingInitialized</em>
 *   This function may only be called during add-on initialization.
 *
 *
 * @ingroup AddOnManagement
 *
 * #internalattributes exclude_alldoc
 */
LINKTOADDON void STDCALL TecUtilAddOnRegForeignLibLoader(const char          *ForeignLibID,
                                                         ForeignLibLoader_pf  ForeignLibLoader,
                                                         ArbParam_t           ClientData);


/**
 * Registers a dynamic axis label provider callback.
 * @since
 *   10.0-6-015
 *
 * @param DynamicLabelName
 *   Unique name given to the Callback.
 *   This name is used in the list of available Callback in SpecifyNumberFormat dialog.
 *
 *  @param DynamicLabelCallback
 *    A callback that will be called for the label string.
 *
 * @param ClientData
 *   This can be any 32-bit value and will be passed to the timer callback.
 *   Typically this is a pointer to a structure
 *
 *
 * @pre <em>DynamicLabelName</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDynamicLabelRegisterCallback(
 *   &           DynamicLabelName,
 *   &           DynamicLabelCallback,
 *   &           ClientData)
 *   CHARACTER*(*)   DynamicLabelName
 *   POINTER         (DynamicLabelCallback_pf, DynamicLabelCallback)
 *   POINTER         (ClientDataPtr, ClientData)
 *  </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Axis
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON void STDCALL TecUtilDynamicLabelRegisterCallback(const char              *DynamicLabelName,
                                                             DynamicLabelCallback_pf  DynamicLabelCallback,
                                                             ArbParam_t               ClientData);


/**
 * Registers an extended curve fit add-on. This will add an option to the single
 * selection list launched by the Curve Type/Extended option on the Curves page
 * of the Plot Attributes dialog.
 *
 * @param CurveFitName
 *   Unique name given to the extended curve fit. This name is used in the list
 *   of extended curve fits in the Extended Curve Fits dialog, launched from
 *   Curve Type/Extended in the Plot Attributes dialog.
 *
 * @param GetLinePlotDataPointsCallback
 *   The name of the function that will calculate the curve fit. This is the
 *   only function that needs to be defined to create an extended curve fit
 *   add-on.
 *
 * @param GetProbeValueCallback
 *   The name of the function that will return the dependent value when the
 *   extended curve fit is probed at a given independent value. If this
 *   function is set to NULL, Tecplot will perform a linear interpolation based
 *   on the values returned by the GetLinePlotDataPoints function.
 *
 * @param GetCurveInfoStringCallback
 *   The name of the function that will create a string to be presented in the
 *   Data/LinePlot Curve Info dialog. This callback may be set to NULL if you
 *   do not wish to present a string to the LinePlot Curve Info dialog.
 *
 * @param GetCurveSettingsCallback
 *   The name of the function that is called when the Curve Settings button on
 *   the Curves page of the Plot Attributes dialog is pressed while the
 *   extended curve fit is set as the Curve Type. This function may be set to
 *   NULL if there are not configurable settings for the extended curve fit. If
 *   settings are changed, it is the responsibility of the add-on writer to
 *   inform Tecplot of the change by calling the function
 *   TecUtilCurveSetExtendedSettings(). This function is usually called when OK
 *   is clicked on the add-on dialog.
 *
 * @param GetAbbreviatedSettingsStringCallback
 *   See \ref GetAbbreviatedSettingsStringCallback_pf.
 *
 * @return
 *   Returns TRUE if the extended curve fit was added.
 *
 *
 * @pre <em>CurveFitName</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCurveRegisterExtCrvFit(
 *   &                   CurveFitName,
 *   &                   GetLinePlotDataPointsCallback,
 *   &                   GetProbeValueCallback,
 *   &                   GetCurveInfoStringCallback,
 *   &                   GetCurveSettingsCallback,
 *   &                   GetAbbreviatedSettingsStringCallback)
 *    CHARACTER*(*)   CurveFitName
 *    POINTER         (GetLinePlotDataPointsCallbackPtr, GetLinePlotDataPointsCallback)
 *    POINTER         (GetProbeValueCallbackPtr, GetProbeValueCallback)
 *    POINTER         (GetCurveInfoStringCallbackPtr, GetCurveInfoStringCallback)
 *    POINTER         (GetCurveSettingsCallbackPtr, GetCurveSettingsCallback)
 *    POINTER         (GetAbbreviatedSettingsStringCallbackPtr, GetAbbreviatedSettingsStringCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup LineMap
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilCurveRegisterExtCrvFit(const char                             *CurveFitName,
                                                            GetLinePlotDataPointsCallback_pf        GetLinePlotDataPointsCallback,
                                                            GetProbeValueCallback_pf                GetProbeValueCallback,
                                                            GetCurveInfoStringCallback_pf           GetCurveInfoStringCallback,
                                                            GetCurveSettingsCallback_pf             GetCurveSettingsCallback,
                                                            GetAbbreviatedSettingsStringCallback_pf GetAbbreviatedSettingsStringCallback);


/**
 * Launch a dialog with an error message.
 * This function is \ref threadsafe.
 *
 * @param Message
 *   String containing the error message.
 *
 *
 * @pre <em>Message</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDialogErrMsg(Message)
 *    CHARACTER*(*) Message
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Display an error message:
 *
 * @code
 *   TecUtilDialogErrMsg("File not found.");
 * @endcode
 *
 * @ingroup UserInterface
 */
LINKTOADDON void STDCALL TecUtilDialogErrMsg(const char *Message);

/**
 * Notifies Tecplot that a major data load operation is about to begin. Please
 * see TecUtilDataLoadEnd() for more details.
 *
 * TecUtilDataLoadBegin() and TecUtilDataLoadEnd() are used together with
 * add-ons that perform a moving window of calculations through data. This is
 * particularly important for add-ons processing transient data.
 *
 * @since
 *   11.0-0-430
 *
 * @sa TecUtilDataLoadEnd, TecUtilDataValueAutoLOD, TecUtilDataValueCustomLOD
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataLoad
 *
 */
LINKTOADDON void STDCALL TecUtilDataLoadBegin(void);

/**
 * Notifies Tecplot that a major data load operation has completed. At the
 * beginning or end of each major data load operation Tecplot examines its
 * current memory use and decides if it needs to unload any data.
 *
 * After this call all data references (i.e. field data, node maps, face
 * neighbors, etc.) previously acquired by the add-on are invalid and should be
 * re-acquired before using them again.
 *
 * TecUtilDataLoadBegin() and TecUtilDataLoadEnd() are used together with
 * add-ons that perform a moving window of calculations through data. This is
 * particularly important for add-ons processing transient data.
 *
 * @since
 *   11.0-0-430
 *
 * @sa TecUtilDataLoadBegin, TecUtilDataValueAutoLOD, TecUtilDataValueCustomLOD
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataLoad
 *
 */
LINKTOADDON void STDCALL TecUtilDataLoadEnd(void);

#if defined __cplusplus
namespace tecplot { namespace szl {
    class DataSetRegistrationInterface;
}}
LINKTOADDON tecplot::szl::DataSetRegistrationInterface* STDCALL TecUtilDataSetGetSZLRegistration(void);
#endif

/**
 * Sets the properties of the variable so that it is shared between source and destination zones
 * (using the source for values). Both zones must have the same structure and value location: Ordered
 * zones must have the same number of points if nodal but cell centered zones must specifically have
 * the same I,J, and K values.  Finite-element zones must have the same number of points if nodal or the
 * same number of elements if cell centered.  Sharing data between ordered and finite-element zones is
 * only allowed if the values are nodal and the zones have the same number of points.  Cell centered
 * sharing is not allowed because ordered zones have ghost cells and finite-element zones do not.
 *
 * @param SourceZone
 *   The zone number where the data values are based.
 *
 * @param DestZone
 *   The zone number where the data values will be shared from the source zone.
 *
 * @param Var
 *   The variable to be shared.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueShare(
 *   &           SourceZone,
 *   &           DestZone,
 *   &           Var)
 *    INTEGER*4       SourceZone
 *    INTEGER*4       DestZone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the first variable of zone 3 to be shared with zone 2:
 *
 * @code
 *   TecUtilDataValueShare(2, 3, 1);
 * @endcode
 *
 * @sa TecUtilDataValueAlloc() and TecUtilDataValueIsSharingOk()
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON void STDCALL TecUtilDataValueShare(EntIndex_t SourceZone,
                                               EntIndex_t DestZone,
                                               EntIndex_t Var);
/**
 * Copies the data from the source zone's variable to the destination zone. The
 * destination zone's variable must already be allocated or memory mapped and
 * it may not be shared. Both zones must have the same structure (both Ordered
 * with the same I,J, and K values; or both are finite-elements with the same
 * element type and same number of nodes.
 *
 * @param SourceZone
 *   The zone number where the data values are based.
 *
 * @param DestZone
 *   The zone number where the data values will be copied from the source zone.
 *
 * @param Var
 *   The variable to be copied.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueCopy(
 *   &           SourceZone,
 *   &           DestZone,
 *   &           Var)
 *    INTEGER*4       SourceZone
 *    INTEGER*4       DestZone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Copy the values from variable 1 of zone 2 to zone 3.
 *
 * @code
 *   TecUtilDataValueCopy(2, 3, 1);
 * @endcode
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON void STDCALL TecUtilDataValueCopy(EntIndex_t SourceZone,
                                              EntIndex_t DestZone,
                                              EntIndex_t Var);

/**
 * Allocates the space needed for the variable. This function is used in
 * conjunction with deferred variable creation. See the SV_DEFERVARCREATION
 * option for TecUtilDataSetAddZoneX() and TecUtilDataSetAddVarX() for details.
 *
 * @since
 *   10.0-3-129
 *
 * @param Zone
 *   The zone needing the variable allocated.
 *
 * @param Var
 *   The variable to be allocated.
 *
 * @return
 *   TRUE if the variable was sucessfully allocated, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueAlloc(
 *   &                   Zone,
 *   &                   Var)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Allocate the first variable of zone 3. Note that this example is only valid
 * if the zone was added with the deferred variable creation option set to
 * true.
 *
 * @code
 *   IsOk = TecUtilDataValueAlloc(3, 1);
 * @endcode
 *
 * @sa TecUtilDataValueShare()
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueAlloc(EntIndex_t Zone,
                                                    EntIndex_t Var);

/**
 * @deprecated
 *   Please use TecUtilDataValueAutoLOD() instead.
 *
 * @ingroup DataServices
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueCanMemMapData(EntIndex_t     Zone,
                                                            EntIndex_t     Var,
                                                            MemMapOffset_t Offset,
                                                            Boolean_t      IsDataNativeByteOrder);
/**
 * @deprecated
 *   Please use TecUtilDataValueAutoLOD() instead.
 *
 * @ingroup DataServices
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueMemMapData(EntIndex_t     Zone,
                                                         EntIndex_t     Var,
                                                         int            FileDescriptor,
                                                         MemMapOffset_t Offset,
                                                         Boolean_t      IsDataNativeByteOrder);

/**
 * Specifies where in the data file the data for the specified variable
 * resides. The variable must be laid out in memory using the structure
 * specified. Using this information Tecplot will automatically load and unload
 * the variable when Tecplot deems it necessary. It is the responsibility of
 * the add-on to ensure that the file remains present and unaltered while
 * Tecplot maintains a reference to this file as Tecplot may unload and
 * subsequently reload the variable at unspecified times.
 *
 * This function is used in conjunction with deferred variable creation. See
 * the SV_DEFERVARCREATION option for TecUtilDataSetAddZoneX() and
 * TecUtilDataSetAddVarX() for details.
 *
 * @since
 *   11.0-0-001
 *
 * @param Zone
 *   The zone of the variable to have Tecplot automatically load on demand.
 * @param Var
 *   Variable to have Tecplot automatically load on demand.
 * @param DataValueStructure
 *   Specifies the structure type to which the data in the file conforms.
 * @param FileName
 *   Data file name containing the variable data laid out using Tecplot's
 *   binary block data format.
 * @param Offset
 *   Absolute offset to the start of the variable data in the file.
 * @param Stride
 *   The stride tells Tecplot how to skip through the file for nodal data. Only
 *   nodal data can specify a stride greater than one. A stride of one offers
 *   much higher load/unload performance than strides greater than one.
 * @param IsDataNativeByteOrder
 *   Indicates if the byte ordering of the data in the file matches the
 *   machine's native byte ordering.
 *
 * @return
 *   TRUE if the variable is setup for auto loading, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>FileName</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * @sa TecUtilDataValueCustomLOD()
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueAutoLOD(EntIndex_t            Zone,
                                                      EntIndex_t            Var,
                                                      DataValueStructure_e  DataValueStructure,
                                                      const char           *FileName,
                                                      FileOffset_t          Offset,
                                                      Int64_t               Stride,
                                                      Boolean_t             IsDataNativeByteOrder);

/**
 * Return the custom load-on-demand client data from a field data handle. The
 * client data should ONLY be retrieved in response to a custom load, unload,
 * cleanup, get-value, or set-value callback. At no other time is the request
 * valid.
 * This function is \ref threadsafe.
 *
 * @param FieldData
 *   Custom load-on-demand field data handle.
 *
 * @return
 *   Client data for the custom load-on-demand add-on.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *   double STDCALL MyGetValueFunction(const FieldData_pa FieldData,
 *                                     LgIndex_t          PointIndex)
 *   {
 *     double Result;
 *     MyClientData_s *MyClientData = (MyClientData_s *)TecUtilDataValueGetClientData(FieldData);
 *
 *     // calculate or extract the requested value from the client data
 *       .
 *       .
 *       .
 *
 *     return Result;
 *   }
 * @endcode
 *
 * @sa TecUtilDataValueCustomLOD()
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON ArbParam_t STDCALL TecUtilDataValueGetClientData(FieldData_pa FieldData);

/**
 * Registers with Tecplot the load-on-demand callbacks and client data for a
 * specific variable. Tecplot will notify the add-on via the callbacks when the
 * variable needs to be loaded, unloaded, and cleaned up or when a value needs
 * to be fetched or submitted.
 *
 * All callbacks must be written in a thread-safe manner so that Tecplot can
 * make concurrent requests to load (and/or unload) multiple variables. The
 * easiest way to write thread-safe callbacks is to not use any shared state
 * (i.e. global or static state) in order to perform the requested action but
 * instead to use private client data to maintain all the information needed to
 * perform the requested action.
 *
 * It is important that GetValue and SetValue callbacks do NOT lock/unlock
 * Tecplot or call TecUtil functions that require Tecplot to be locked as this
 * will incur a performance penalty. GetValue and SetValue functions should be
 * lightweight and fast.
 *
 * Calls made back to Tecplot in response to a load, unload, or cleanup request
 * should be limited to queries except in the case where data is being loaded
 * into a variable. In addition, no state changes should be broadcast by the
 * callbacks.
 *
 * This function is used in conjunction with deferred variable creation. See
 * the SV_DEFERVARCREATION option for TecUtilDataSetAddZoneX() and
 * TecUtilDataSetAddVarX() for details.
 *
 * Two methods for loading and accessing data are available for custom
 * load-on-demand (see ADK Users Manual for details):
 * - Custom Load Variable on Demand:\n
 *   The add-on supplied VariableLoad() callback is responsible for loading the
 *   entire variable data into the Tecplot prepared field data backing. Tecplot
 *   is responsible for allocating and freeing the space for the field data
 *   backing. In addition the add-on must supply the VariableCleanup() callback
 *   to receive notification when the variable source is no longer needed.
 *   Optionally the add-on may supply the VariableUnload() callback to receive
 *   notification of the variable being unloaded. Most add-ons should supply
 *   NULL for the VariableUnload() callback instructing Tecplot to assume
 *   responsibility for unloading the variable and re-loading it in an
 *   efficient form. The "Custom Load Variable on Demand" method is signified
 *   by passing NULL for the GetValueFunction() and SetValueFunction() callback
 *   parameters.
 * - Custom Load Value on Demand:\n
 *   The add-on supplied GetValueFunction() and an optional SetValueFunction()
 *   are used by Tecplot to fetch and submit variable data. If the
 *   SetValueFunction() is not provided Tecplot will create its own copy of
 *   the variable when an attempt is made to modify the variable values (such
 *   as via a Tecplot Data Alter operation). The VariableLoad(), VariableUnload(),
 *   and VariableCleanup() callbacks are optional. The add-on may supply these
 *   callback to receive notification of Tecplot's intent or NULL if the add-on
 *   is not interested in the notifications.
 *
 * @since
 *   11.0-0-001
 *
 * @param Zone
 *   Zone containing the variable that will now be custom load-on-demand.
 *
 * @param Var
 *   Variable that will now be custom load-on-demand.
 *
 * @param VariableLoad
 *   Tecplot calls this callback when the variable is to be loaded. The
 *   VariableLoad() callback may never get called if the variable is not needed
 *   or it may get called immediately if load-on-demand capabilities are not
 *   available. Depending on the load-on-demand method the callback has
 *   different responsibilities:
 *   - If the variable is using the "Custom Load Variable on Demand" method
 *     (signified by passing NULL for the GetValueFunction() and
 *     SetValueFunction() callback parameters) the callback is responsible for
 *     loading the entire variable data into the Tecplot prepared field data
 *     backing using information from its private client to locate or generate
 *     the data.
 *   - If the variable is using the "Custom Load Value on Demand" method
 *     (signified by passing a non-NULL GetValueFunction() callback parameter)
 *     the callback is simply a notification that Tecplot is getting ready to
 *     fetch data via the GetValueFunction() or, if supplied, submit data via
 *     the SetValueFunction().
 *   This callback is called asynchronously.
 *
 * @param VariableUnload
 *   Add-ons can supply NULL for this callback. Supplying NULL instructs Tecplot to handled the
 *   unloading (and subsequent reloading) of the variable without the intervention of the add-on,
 *   however Tecplot will be forced to write the data to its temporary directory when unloaded thereby
 *   incurring additional I/O expense. If the add-on does supply this callback, Tecplot calls it
 *   when the variable is to be unloaded. This query provides the add-on an opportunity to allow or
 *   deny a variable to be unloaded by returning TRUE or FALSE respectively. Unless there is a
 *   compelling reason, such as very expensive load costs (in which case NULL should probably be
 *   supplied for this callback), the add-on should honor Tecplot's request to unload the variable
 *   (i.e. the VariableUnload() callback should return TRUE). An add-on may also cleanup any private
 *   resources that are not needed when the variable is unloaded, however the add-on must still
 *   maintain enough information to load the variable again if requested by Tecplot. The
 *   VariableUnload() callback may never get called if the variable does not need to be unloaded nor
 *   will the VariableUnload() callback necessarily be called before the VariableCleanup() callback.
 *   This callback is called asynchronously.
 *
 * @param VariableCleanup
 *   Tecplot calls this callback when the variable is to be cleaned up. This
 *   allows the add-on to cleanup any private resources that were used in
 *   conjunction with identifying or loading this variable. After a variable is
 *   cleaned up Tecplot will never again request it to be loaded. Tecplot may
 *   or may not call the VariableUnload() callback before calling the
 *   VariableCleanup() callback. Additionally, the VariableCleanup() callback
 *   will be called even if the variable was never loaded.
 *   This callback is called asynchronously.
 *
 * @param GetValueFunction
 *   Tecplot calls this callback to fetch the value from the field data at the
 *   specified index. This callback is only registered by add-ons that are
 *   using the "Custom Load Value on Demand" method, otherwise pass NULL.
 *   This callback is called asynchronously.
 *
 * @param SetValueFunction
 *   Tecplot calls this callback to submit a value to the field data at the
 *   specified index. This callback is only registered by add-ons that are
 *   using the "Custom Load Value on Demand" method, otherwise pass NULL. By
 *   providing a SetValueFunction() callback the add-on is signifying it will
 *   store any changes to the field data until the variable is cleaned up. Most
 *   "Custom Load Value on Demand" add-ons should pass NULL for this callback
 *   and allow Tecplot to store variable modifications.
 *   This callback is called asynchronously.
 *
 * @param ClientData
 *   Private client data needed by the custom load-on-demand callbacks to
 *   perform the duties of loading, unloading, and cleaning up the variable or
 *   to perform the get or set requests. Tecplot stores the client data in the
 *   field data structure and must be retrieved by the callbacks using
 *   TecUtilDataValueGetClientData(). The client data should ONLY be retrieved
 *   in response to a custom load, unload, cleanup, get-value, or set-value
 *   callback. At no other time is the request valid.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * Following is an example of how to create a variable using the "Custom Load
 * Value on Demand" method by registering a very simple GetValueFunction() that
 * always returns zero without the overhead of allocating any data.
 * @code
 *   double STDCALL *ZeroVariableGetValue(const FieldData_pa FieldData,
 *                                        LgIndex_t          PointIndex)
 *   {
 *       return 0.0;
 *   }
 *
 *   .
 *   .
 *   .
 *   IsOk = TecUtilDataValueCustomLOD(3, 4, NULL, NULL, NULL,
 *                                    ZeroVariableGetValue, NULL, 0);
 * @endcode
 *
 * Following is an example of how to create a variable using the "Custom Load
 * Variable on Demand" method by registering some simple load/unload/cleanup
 * callbacks.
 * @code
 *   typedef struct
 *     {
 *         char      *DataFileName;
 *         long       SeekOffset;
 *         LgIndex_t  NumValues;
 *         ...other information needed to load variable data
 *     } MyVariableClientData_s;
 *
 *   Boolean_t STDCALL MyVariableLoader(FieldData_pa FieldData)
 *   {
 *       REQUIRE(VALID_REF(FieldData));
 *
 *       MyVariableClientData_s *MyClientData = (MyVariableClientData_s *)TecUtilDataValueGetClientData(FieldData);
 *
 *       // open the data file
 *       FILE *MyDataFile = fopen(MyClientData->DataFileName, "rb");
 *       Boolean_t IsOk = (MyDataFile != NULL);
 *
 *       // seek to the place in the file where the variable data is located
 *       IsOk = IsOk && (fseek(MyDataFile, MyClientData->SeekOffset, SEEK_SET) == 0);
 *       if (IsOk)
 *       {
 *           // load the data into the variable's field data
 *           IsOk = ReadMyDataInfoVariable(MyDataFile, MyClientData, FieldData);
 *       }
 *
 *       // cleanup
 *       if (MyDataFile != NULL)
 *           fclose(MyDataFile);
 *
 *       ENSURE(VALID_BOOLEAN(IsOk));
 *       return IsOk;
 *   }
 *
 *   Boolean_t STDCALL MyVariableUnload(FieldData_pa FieldData)
 *   {
 *       REQUIRE(VALID_REF(FieldData));
 *
 *       // We don't have any private data to cleanup (i.e in addition to the
 *       // private client data which we don't cleanup here) so all we have to do
 *       // is return TRUE or FALSE letting Tecplot know that it can or can not
 *       // unload the variable.
 *       Boolean_t Result = TRUE; // ...tell Tecplot to go ahead and unload the variable
 *
 *       ENSURE(VALID_BOOLEAN(Result));
 *       return Result;
 *   }
 *
 *   void STDCALL MyVariableCleanup(FieldData_pa FieldData)
 *   {
 *       REQUIRE(VALID_REF(FieldData));
 *
 *       MyVariableClientData_s *MyClientData = (MyVariableClientData_s *)TecUtilDataValueGetClientData(FieldData);
 *
 *       // cleanup privately allocated resources
 *       free(MyClientData->DataFileName);
 *       free(MyClientData);
 *   }
 *
 *   .
 *   .
 *   .
 *   MyVariableClientData_s *MyClientData = (MyVariableClientData_s *)malloc(sizeof(MyVariableClientData_s));
 *   const char *MyDataFileName = "MyDataFileName.dat";
 *   MyClientData->MyDataFileName = (char *)malloc(strlen(MyDataFileName)+1);
 *   strcpy(MyClientData->MyDataFileName, MyDataFileName);
 *   MyClientData->SeekOffset = ... determined somewhere else
 *   MyClientData->NumValues = ... determined somewhere else
 *   ...initialize any other client data information needed to load variable data
 *   IsOk = TecUtilDataValueCustomLOD(3, 4,
 *                                    MyVariableLoader,
 *                                    MyVariableUnload, // most add-ons should pass NULL instead of MyVariableUnload
 *                                    MyVariableCleanup,
 *                                    NULL, // passing NULL for GetValue function signifies load-variable-on-demand
 *                                    NULL,
 *                                    (ArbParam_t)MyClientData);
 * @endcode
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilDataValueAutoLOD(), TecUtilDataValueSetMinMaxByZoneVar()
 *
 * @ingroup DataValue
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueCustomLOD(EntIndex_t                Zone,
                                                        EntIndex_t                Var,
                                                        LoadOnDemandVarLoad_pf    VariableLoad,
                                                        LoadOnDemandVarUnload_pf  VariableUnload,
                                                        LoadOnDemandVarCleanup_pf VariableCleanup,
                                                        FieldValueGetFunction_pf  GetValueFunction,
                                                        FieldValueSetFunction_pf  SetValueFunction,
                                                        ArbParam_t                ClientData);

/**
 * Instructs Tecplot to unload the variable. All field data references to the
 * unloaded variable are invalid. If after unloading the variable you wish to
 * inspect or modify the field data you must re-obtain a readable or writable
 * field data reference or raw pointer.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *   11.0-0-244
 *
 * @param Zone
 *   Zone containing the variable to unload.
 *
 * @param Var
 *   Variable to unload.
 *
 * @return
 *   TRUE if the variable was unloaded, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * @sa TecUtilDataValueGetReadableNativeRef(), TecUtilDataValueGetReadableDerivedRef(),
 *     TecUtilDataValueGetReadableNLRef(), TecUtilDataValueGetReadableCCRef(),
 *     TecUtilDataValueGetWritableNativeRef(), TecUtilDataValueGetReadableRawPtr(),
 *     TecUtilDataValueGetWritableRawPtr()
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueUnload(EntIndex_t Zone,
                                                     EntIndex_t Var);

/**
 * Branch off a shared variable. The specified variable of the specified zone
 * is branched so it is no longer shared with anything.
 *
 * @param Zone
 *   Zone in which the shared variable is located.
 *
 * @param Var
 *   Variable that will be branched
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueBranchShared(
 *   &                   Zone,
 *   &                   Var)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Branch variable 2 in zone 1.
 *
 * @code
 *   TecUtilDataValueBranchShared(1, 2);
 * @endcode
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueBranchShared(EntIndex_t Zone,
                                                           EntIndex_t Var);




/**
 * Sets the properties of the connectivity so that it is shared between source and destination zones
 * (using the source for values). Both zones must have the same structure (both Ordered with the
 * same I,J, and K values; or both are finite-elements with the same element type and same number of
 * nodes).  Both zones must also have the same local face neighbor mode.
 *
 * @param SourceZone
 *   The zone number where the connectivity is based.
 *
 * @param DestZone
 *   The zone number where the connectivity will be shared from the source zone.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataConnectShare(
 *   &           SourceZone,
 *   &           DestZone)
 *    INTEGER*4       SourceZone
 *    INTEGER*4       DestZone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the connectivity in zone 3 to be shared with zone 2:
 *
 * @code
 *   TecUtilDataConnectShare(2, 3);
 * @endcode
 *
 * @ingroup DataSharing
 *
 */
LINKTOADDON void STDCALL TecUtilDataConnectShare(EntIndex_t SourceZone,
                                                 EntIndex_t DestZone);






/**
 * Branch the connectivity information. Returns False if out of memory.
 *
 * @param Zone
 *   Zone number where connectivity is to be branched.
 *
 * @return
 *   TRUE if connectivity is branched, FALSE if out of memory.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataConnectBranchShared(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataSharing
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataConnectBranchShared(EntIndex_t Zone);




/**
 * Copies the specified number of values from the base of the source value
 * array to the destination field data starting at the specified offset. The
 * source value array must be of the same data type as the destination field
 * data. In addition, data of type @ref FieldDataType_Bit is currently not
 * supported for array access.
 * This function is \ref threadsafe.
 *
 * @since
 *     10.0-3-12
 *
 * @param DestFieldData
 *     Field data to receive the source values.
 * @param DestOffset
 *     Member offset in the destination field data to begin assigning values.
 *     DestOffset >= 1 and DestOffset <= DestCount.
 * @param DestCount
 *     Number of values to assign to the destination field data. This value
 *     must not exceed the number of items supplied by SourceValueArray.
 * @param SourceValueArray
 *     An array containing the members to copy. The first member is assumed to
 *     be at the base of the array.
 *
 *
 * @pre <em>DestFieldData</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON void STDCALL TecUtilDataValueArraySetByRef(FieldData_pa  DestFieldData,
                                                       LgIndex_t     DestOffset,
                                                       LgIndex_t     DestCount,
                                                       const void   *SourceValueArray);
/**
 * Assign a value to a field variable at a specific position. If the zone
 * referenced is IJ- or IJK-ordered, the position is calculated by treating the
 * two- or three-dimensional array as a one-dimensional array. Be sure to call
 * TecUtilStateChanged() after changing field data in this way.
 * This function is \ref threadsafe.
 *
 * @param FD
 *   A field data reference usually obtained via a call to one of the following
 *   functions: TecUtilDataValueGetReadableNativeRef(),
 *   TecUtilDataValueGetReadableDerivedRef(), TecUtilDataValueGetReadableNLRef(),
 *   TecUtilDataValueGetReadableCCRef(), or TecUtilDataValueGetWritableRef().
 *
 * @param PointIndex
 *   Position in the array of field data values. Position starts at one. For
 *   cell centered variables in ordered zones, the array includes values for
 *   IMax, JMax and KMax, even though these values are not used. You must
 *   account for these "ghost" cells in calculating the PointIndex. The formula
 *   for PointIndex in terms of I,J, and K is the same for both Nodal and cell
 *   centered variables. PointIndex = I + (J-1)*IMax + (K-1)*IMax*JMax;
 *
 * @param Value
 *   New value for the position in the field data.
 *
 *
 * @pre <em>FD</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueSetByRef(
 *   &           FDPtr,
 *   &           PointIndex,
 *   &           Value)
 *    POINTER         (FDPtr, FD)
 *    INTEGER*4       PointIndex
 *    REAL*8          Value
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Set the first two values of the second variable of zone 5 to be 1.25 and
 * 1.35 respectively:
 *
 * @code
 *   Set_pa altered_vars;
 *   FieldData_pa fd = TecUtilDataValueGetWritableNativeRef(5, 2);
 *   if ( fd )
 *     {
 *       TecUtilDataValueSetByRef(fd, 1, 1.25);
 *       TecUtilDataValueSetByRef(fd, 2, 1.35);
 *
 *       // inform Tecplot of var value change
 *
 *       altered_vars = TecUtilSetAlloc(TRUE);
 *
 *       TecUtilSetAddMember(altered_vars, var, TRUE);
 *       TecUtilStateChanged(StateChange_VarsAltered,
 *                           (ArbParam_t)altered_vars);
 *       TecUtilSetDealloc(&altered_vars);
 *     }
 * @endcode
 *
 * @sa TecUtilDataValueRefGetSetFunc() for obtaining a function as a high
 *     performance alternative.
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON void STDCALL TecUtilDataValueSetByRef(FieldData_pa FD,
                                                  LgIndex_t    PointIndex,
                                                  double       Value);

/**
 * Set the minimum and maximum values for a tecplot variable using a field data
 * reference. Although Tecplot will calculate the min/max value for a variable
 * if one is not supplied there are cases where it is valuable to supply these
 * values. If a loader add-on knows the min/max values it may save a
 * significant amount of time to supply them. Additionally, if an add-on is
 * loading variables using Tecplot's load-on-demand facility supplying the
 * min/max value for a variable prevents Tecplot from having to load the
 * variable in some situations, saving time and memory. It is important that
 * the supplied values accurately represent the min/max values of the data and
 * not a subset of the data.
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   If supplied, the min/max values MUST be assigned after a vars altered
 *   state change as the state change call will invalidate the min/max values.
 *
 * @since
 *   11.0-0-314
 *
 * @param FieldData
 *   The field data handle of the variable to receive the min/max value
 *   assignments.
 * @param MinValue
 *   Minimum variable value.
 * @param MaxValue
 *   Maximum variable value.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>FieldData</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueSetMinMaxByRef(
 *   &           FieldDataPtr,
 *   &           MinValue,
 *   &           MaxValue)
 *    POINTER         (FieldDataPtr, FieldData)
 *    REAL*8          MinValue
 *    REAL*8          MaxValue
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Set the minimum and maximum values for a newly loaded variable.
 * @code
 *
 *   ... after defining the data to load, issue state change ...
 *
 *   // supply the min/max value of the variable reference
 *   TecUtilDataValueSetMinMaxByRef(FieldData, MinValue, MaxValue);
 * @endcode
 *
 * Multiply the value of the 3rd variable of the 2nd zone and then tell Tecplot
 * the new min/max value so it doesn't have to recalculate it.
 * @code
 *   double     MinValue;
 *   double     MaxValue;
 *   double     Factor = 3.0;
 *   EntIndex_t Zone   = 2;
 *   EntIneex_t Var    = 3;
 *
 *   FieldData_pa FieldData = TecUtilDataValueReadableGetRef(Zone, Var);
 *   TecUtilDataValueGetMinMaxByRef(FieldData, &MinValue, &MaxValue);
 *
 *   // mutiply variable by the factor and issue a vars altered state change
 *   MyFuncToMultiplyVarByFactor(Zone, Var, Factor); //...not shown
 *   MyFuncToBroadcastStateChange(Zone, Var); //...not shown
 *
 *   // assign the min/max values after a vars altered state change
 *   // as the state change call will invalidate the min/max values
 *   TecUtilDataValueSetMinMaxByRef(FieldData, MinValue*Factor, MaxValue*Factor);
 * @endcode
 *
 * @sa TecUtilDataValueGetMinMaxByRef(), TecUtilDataValueGetMinMaxByZoneVar(),
 *     TecUtilDataValueSetMinMaxByZoneVar(), TecUtilDataValueCustomLOD()
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON void STDCALL TecUtilDataValueSetMinMaxByRef(FieldData_pa FieldData,
                                                        double       MinValue,
                                                        double       MaxValue);
/**
 * Set the minimum and maximum values for a tecplot variable using a zone and
 * variable number. Although Tecplot will calculate the min/max value for a
 * variable if one is not supplied there are cases where it is valuable to
 * supply these values. If a loader add-on knows the min/max values it may save
 * a significant amount of time to supply them. Additionally, if an add-on is
 * loading variables using Tecplot's load-on-demand facility supplying the
 * min/max value for a variable prevents Tecplot from having to load the
 * variable in some situations, saving time and memory. It is important that
 * the supplied values accurately represent the min/max values of the data and
 * not a subset of the data.
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   If supplied, the min/max values must be assigned after a vars altered
 *   state change as the state change call will invalidate the min/max values.
 *
 * @since
 *   11.0-0-007
 *
 * @param Zone
 *   The zone number of the variable to receive the min/max value assignments.
 * @param Var
 *   The variable number to receive the min/max value assignments.
 * @param MinValue
 *   Minimum variable value.
 * @param MaxValue
 *   Maximum variable value.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueSetMinMaxByZoneVar(
 *   &           Zone,
 *   &           Var,
 *   &           MinValue,
 *   &           MaxValue)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 *    REAL*8          MinValue
 *    REAL*8          MaxValue
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Set the minimum and maximum values for a newly loaded variable.
 * @code
 *
 *   ... after defining the data to load, issue state change ...
 *
 *   // supply the min/max value for the variable 2 of zone 5
 *   TecUtilDataValueSetMinMaxByZoneVar(5, 2, MinValue, MaxValue);
 * @endcode
 *
 * Multiply the value of the 3rd variable of the 2nd zone and then tell Tecplot
 * the new min/max value so it doesn't have to recalculate it.
 * @code
 *   double     MinValue;
 *   double     MaxValue;
 *   double     Factor = 3.0;
 *   EntIndex_t Zone   = 2;
 *   EntIneex_t Var    = 3;
 *
 *   TecUtilDataValueGetMinMaxByZoneVar(Zone Var, &MinValue, &MaxValue);
 *
 *   // mutiply variable by the factor and issue a vars altered state change
 *   MyFuncToMultiplyVarByFactor(Zone, Var, Factor); //...not shown
 *   MyFuncToBroadcastStateChange(Zone, Var); //...not shown
 *
 *   TecUtilDataValueSetMinMaxByZoneVar(Zone, Var, MinValue*Factor, MaxValue*Factor);
 * @endcode
 *
 * @sa TecUtilDataValueGetMinMaxByZoneVar(), TecUtilDataValueCustomLOD()
 *
 * @ingroup DataValue
 *
 */
LINKTOADDON void STDCALL TecUtilDataValueSetMinMaxByZoneVar(EntIndex_t   Zone,
                                                            EntIndex_t   Var,
                                                            double       MinValue,
                                                            double       MaxValue);

/**
 * Set the node index for a particular corner of a finite-element. This
 * function does not require you to obtain the handle to the node map as does
 * TecUtilDataNodeArraySetByRef() or TecUtilDataNodeSetByRef(), however, this
 * function is not very efficient. Use TecUtilDataNodeArraySetByRef() or
 * TecUtilDataNodeSetByRef() if you are setting multiple nodes for the same
 * zone. You do not need to call TecUtilStateChanged() after calling this
 * function as Tecplot does that for you.
 *
 * @param Zone
 *   Zone number.
 *
 * @param Element
 *   The element number (starts at 1).
 *
 * @param Corner
 *   The element corner (starts at 1).
 *
 * @param Node
 *   The new node index for that element at that corner.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataNodeSetByZone(
 *   &           Zone,
 *   &           Element,
 *   &           Corner,
 *   &           Node)
 *    INTEGER*4       Zone
 *    INTEGER*4       Element
 *    INTEGER*4       Corner
 *    INTEGER*4       Node
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the third node of the 43rd element of zone 5 to be 129:
 *
 * @code
 *   TecUtilDataNodeSetByZone(5, 43, 3, 129);
 * @endcode
 *
 * @ingroup DataStructure
 *
 */
LINKTOADDON void STDCALL TecUtilDataNodeSetByZone(EntIndex_t Zone,
                                                  LgIndex_t  Element,
                                                  LgIndex_t  Corner,
                                                  NodeMap_t  Node);

/**
 * Copies the specified number of nodes from the base of the source node array
 * to the destination node map starting at the specified offset.
 *
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   Unless this call is in response to a custom load-on-demand callback be
 *   sure to issue a state change StateChange_NodeMapsAltered before returning
 *   control to Tecplot.
 *
 * @since
 *   11.0-0-019
 *
 * @param DestNodeMap
 *   Node map to receive the source nodes.
 * @param DestOffset
 *   Node offset in the destination node map to begin assigning nodes.
 * @param DestCount
 *   Number of nodes to assign to the destination node map. This value must not
 *   exceed the number of items supplied by SourceValueArray.
 * @param SourceNodeArray
 *   An array containing the one based nodes to copy. The first node is assumed
 *   to be at the base of the array.
 *
 *
 * @pre <em>DestNodeMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>SourceNodeArray</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataStructure
 *
 */
LINKTOADDON void STDCALL TecUtilDataNodeArraySetByRef(NodeMap_pa       DestNodeMap,
                                                      LgIndex_t        DestOffset,
                                                      LgIndex_t        DestCount,
                                                      const NodeMap_t *SourceNodeArray);

/**
 * Set the node index for a particular corner of a finite-element. To use this
 * function you must have already obtained the handle to the node map.
 *
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   Unless this call is in response to a custom load-on-demand callback be
 *   sure to issue a state change StateChange_NodeMapsAltered before returning
 *   control to Tecplot.
 *
 * @param NM
 *   Handle to the connectivity list (that is, the node map). Use
 *   TecUtilDataNodeGetReadableRef() or TecUtilDataNodeGetWritableRef()
 *   to get a handle to the node map.
 *
 * @param Element
 *   The element number (starts at 1)
 *
 * @param Corner
 *   The element corner (starts at 1).
 *
 * @param Node
 *   The new node index for that element at that corner
 *
 *
 * @pre <em>NM</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataNodeSetByRef(
 *   &           NMPtr,
 *   &           Element,
 *   &           Corner,
 *   &           Node)
 *    POINTER         (NMPtr, NM)
 *    INTEGER*4       Element
 *    INTEGER*4       Corner
 *    INTEGER*4       Node
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Set the first two nodes of the 43rd element of zone 5 to be 127 and 128
 * respectively:
 *
 * @code
 *   Set_pa altered_zones;
 *   NodeMap_pa nm;
 *   nm = TecUtilDataNodeGetWritableRef(5);
 *   if ( nm )
 *     {
 *       TecUtilDataNodeSetByRef(nm, 43, 1, 127);
 *       TecUtilDataNodeSetByRef(nm, 43, 2, 128);
 *       // inform Tecplot of node map change
 *       altered_zones = TecUtilSetAlloc(TRUE);
 *       TecUtilSetAddMember(altered_zones, 5, TRUE);
 *       TecUtilStateChanged(StateChange_NodeMapsAltered,
 *                           (ArbParam_t)altered_zones);
 *       TecUtilSetDealloc(&altered_zones);
 *   }
 * @endcode
 *
 * @ingroup DataStructure
 *
 */
LINKTOADDON void STDCALL TecUtilDataNodeSetByRef(NodeMap_pa NM,
                                                 LgIndex_t  Element,
                                                 LgIndex_t  Corner,
                                                 NodeMap_t  Node);

/**
 * Return the custom load-on-demand client data from a node map handle. The
 * client data should ONLY be retrieved in response to a custom load, unload,
 * or cleanup callback. At no other time is the request valid.
 * This function is \ref threadsafe.
 *
 * @param NodeMap
 *     Custom load-on-demand node map handle.
 *
 * @return
 *     Client data for the custom load-on-demand add-on.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *     Boolean_t STDCALL MyNodeMapLoader(NodeMap_pa NodeMap)
 *     {
 *       Boolean_t Result;
 *       MyClientData_s *MyClientData = (MyClientData_s *)TecUtilDataNodeGetClientData(NodeMap);
 *
 *       // load the custom node map using client data
 *       .
 *       .
 *       .
 *
 *     return Result;
 *   }
 * @endcode
 *
 * @sa TecUtilDataNodeCustomLOD()
 *
 * @ingroup DataServices
 *
 */
LINKTOADDON ArbParam_t STDCALL TecUtilDataNodeGetClientData(NodeMap_pa NodeMap);

/**
 * Specifies where in the data file the data for the specified node map
 * resides. The node map must be laid out in memory exactly as expected by
 * Tecplot where zero based node numbers for each element are specified
 * consecutively. Using this information Tecplot will automatically load and
 * unload the node map when Tecplot deems it necessary. It is the
 * responsibility of the add-on to ensure that the file remains present and
 * unaltered while Tecplot maintains a reference to this file as Tecplot may
 * unload and subsequently reload the node map at unspecified times.
 *
 * This function is used in conjunction with deferred variable creation. See
 * the SV_DEFERNODEMAPCREATION option for TecUtilDataSetAddZoneX() for details.
 *
 * @since
 *   11.3-0-010
 *
 * @param Zone
 *   The zone number of the node map to have Tecplot automatically load on
 *   demand.
 * @param FileName
 *   Data file name containing the node map data laid out using Tecplot's
 *   binary data format.
 * @param Offset
 *   Absolute offset to the start of the node map data in the file.
 * @param IsDataNativeByteOrder
 *   Indicates if the byte ordering of the data in the file matches the
 *   machine's native byte ordering.
 *
 * @return
 *   TRUE if the node map is setup for auto loading, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>FileName</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * @sa TecUtilDataNodeCustomLOD()
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataServices
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataNodeAutoLOD(EntIndex_t   Zone,
                                                     const char*  FileName,
                                                     FileOffset_t Offset,
                                                     Boolean_t    IsDataNativeByteOrder);
/**
 * Registers with Tecplot the load-on-demand callbacks and client data for a
 * node mapping for a specific zone. Tecplot will notify the add-on via the
 * callbacks when the node mapping needs to be loaded, unloaded, and cleaned
 * up.
 *
 * All callbacks must be written in a thread-safe manner so that Tecplot can
 * make concurrent requests to load (and/or unload) multiple node mappings. The
 * easiest way to write thread-safe callbacks is not to use any shared state
 * (i.e. global or static state) in order to perform the requested action but
 * instead to use private client data to maintain all the information needed to
 * perform the requested action.
 *
 * Calls made back to Tecplot in response to a load, unload, or cleanup request
 * should be limited to queries except in the case where data is being loaded
 * into a node mapping. In addition, no state changes should be broadcast by
 * the callbacks.
 *
 * This function is used in conjunction with deferred variable creation. See
 * the SV_DEFERNODEMAPCREATION option for TecUtilDataSetAddZoneX()for details.
 *
 * The method for loading and accessing node map data with custom
 * load-on-demand is similar to custom load-on-demand for field data (see ADK
 * Users Manual for details): The add-on supplied LoadCallback() callback is
 * responsible for loading the entire node map data into the Tecplot prepared
 * node map backing. Tecplot is responsible for allocating and freeing the
 * space for the node map backing. In addition, the add-on must supply the
 * CleanupCallback() callback to receive notification of when the node map is
 * no longer needed. Optionally, the add-on may supply the UnloadCallback()
 * callback to receive notification of when the node map is being unloaded.
 * Most add-ons should supply NULL for the UnloadCallback() callback,
 * instructing Tecplot to assume responsibility for unloading the node map and
 * re-loading it in an efficient form.
 *
 * @since
 *   11.3-0-010
 *
 * @param Zone
 *   Zone for which the node map will now be custom load-on-demand.
 *
 * @param LoadCallback
 *   Tecplot calls this callback when the node map is to be loaded. The
 *   LoadCallback() callback may never get called if the node map is not needed
 *   or it may get called immediately if load-on-demand capabilities are not
 *   available.  This callback is called asynchronously.
 *
 * @param UnloadCallback
 *   Add-ons can supply NULL for this callback. Supplying NULL instructs Tecplot to handle the
 *   unloading (and subsequent reloading) of the node map without the intervention of the add-on,
 *   however Tecplot will be forced to write the data to its temporary directory when unloaded
 *   thereby incurring additional I/O expense. If the add-on does supply this callback, Tecplot
 *   calls it when the node map is to be unloaded. This query provides the add-on an opportunity to
 *   allow or deny a node map to be unloaded by returning TRUE or FALSE respectively. Unless there
 *   is a compelling reason, such as very expensive load costs (in which case NULL should probably
 *   be supplied for this callback), the add-on should honor Tecplot's request to unload the node
 *   map (i.e. the UnloadCallback() callback should return TRUE). An add-on may also cleanup any
 *   private resources that are not needed when the node map is unloaded, however the add-on must
 *   still maintain enough information to load the node map again if requested by Tecplot. The
 *   UnloadCallback() callback may never get called if the node map does not need to be unloaded nor
 *   will the UnloadCallback() callback necessarily be called before the CleanupCallback() callback.
 *   This callback is called asynchronously.
 *
 * @param CleanupCallback
 *   Tecplot calls this callback when the node map is to be cleaned up. This
 *   allows the add-on to cleanup any private resources that were used in
 *   conjunction with identifying or loading this node map. After a node map is
 *   cleaned up Tecplot will never again request it to be loaded. Tecplot may
 *   or may not call the UnloadCallback() callback before calling the
 *   CleanupCallback() callback. Additionally, the CleanupCallback() callback
 *   will be called even if the node map was never loaded. This callback is called
 *   asynchronously.
 *
 * @param ClientData
 *   Private client data needed by the custom load-on-demand callbacks to
 *   perform the duties of loading, unloading, and cleaning up the node map.
 *   Tecplot stores the client data in the node map structure and must be
 *   retrieved by the callbacks using TecUtilDataNodeGetClientData(). The
 *   client data should ONLY be retrieved in response to a custom load,
 *   unload, or cleanup callback. At no other time is the request valid.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * Following is an example of how to create a node map using the Custom Load
 * on Demand.
 * @code
 *   typedef struct
 *     {
 *       char* DataFileName;
 *       long  SeekOffset;
 *       ... other information needed to load node map data
 *     } MyNodeMapClientData_s;
 *
 *   Boolean_t STDCALL MyNodeMapLoader(NodeMap_pa NodeMap)
 *   {
 *     REQUIRE(VALID_REF(NodeMap));
 *
 *     MyNodeMapClientData_s *MyClientData = (MyNodeMapClientData_s *)TecUtilDataNodeGetClientData(NodeMap);
 *
 *     // open the data file
 *     FILE *MyDataFile = fopen(MyClientData->DataFileName, "rb");
 *     Boolean_t IsOk = (MyDataFile != NULL);
 *
 *     // seek to the place in the file where the node map data is located
 *     IsOk = IsOk && (fseek(MyDataFile, MyClientData->SeekOffset, SEEK_SET) == 0);
 *     if (IsOk)
 *       {
 *         // load the data into the zone's node map
 *         IsOk = ReadMyNodeMapDataIntoZone(MyDataFile, MyClientData, NodeMap);
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
 *   Boolean_t STDCALL MyNodeMapUnload(NodeMap_pa NodeMap)
 *   {
 *     REQUIRE(VALID_REF(NodeMap));
 *
 *     // We don't have any private data to cleanup (i.e. in addition to the
 *     // private client data which we don't cleanup here) so all we have to do
 *     // is return TRUE or FALSE letting Tecplot know that it can or can not
 *     // unload the node map.
 *     Boolean_t Result = TRUE; // ...tell Tecplot to go ahead and unload the node map
 *
 *     ENSURE(VALID_BOOLEAN(Result));
 *     return Result;
 *   }
 *
 *   void STDCALL MyNodeMapCleanup(NodeMap_pa NodeMap)
 *   {
 *     REQUIRE(VALID_REF(NodeMap));
 *
 *     MyNodeMapClientData_s *MyClientData = (MyNodeMapClientData_s *)TecUtilDataNodeGetClientData(NodeMap);
 *
 *     // cleanup privately allocated resources
 *     free(MyClientData->DataFileName);
 *     free(MyClientData);
 *   }
 *
 *   .
 *   .
 *   .
 *   MyNodeMapClientData_s *MyClientData = (MyNodeMapClientData_s *)malloc(sizeof(MyNodeMapClientData_s));
 *   const char *MyDataFileName = "MyDataFileName.dat";
 *   MyClientData->MyDataFileName = (char *)malloc(strlen(MyDataFileName)+1);
 *   strcpy(MyClientData->MyDataFileName, MyDataFileName);
 *   MyClientData->SeekOffset = ... determined somewhere else
 *   ...initialize any other client data information needed to load node map data
 *   IsOk = TecUtilDataNodeCustomLOD(3,
 *                                   MyNodeMapLoader,
 *                                   MyNodeMapUnload, // most add-ons should pass NULL instead of MyNodeMapUnload
 *                                   MyNodeMapCleanup,
 *                                   (ArbParam_t)MyClientData);
 * @endcode
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilDataNodeAlloc(), TecUtilDataConnectShare(),
 *     TecUtilDataNodeArraySetByRef()
 *
 * @ingroup DataServices
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataNodeCustomLOD(EntIndex_t                    Zone,
                                                       LoadOnDemandNodeMapLoad_pf    LoadCallback,
                                                       LoadOnDemandNodeMapUnload_pf  UnloadCallback,
                                                       LoadOnDemandNodeMapCleanup_pf CleanupCallback,
                                                       ArbParam_t                    ClientData);
/**
 * Allocates the space needed for the node mapping. This function is used in
 * conjunction with deferred node map creation. See the SV_DEFERNODEMAPCREATION
 * option for TecUtilDataSetAddZoneX() for details.
 *
 * @since
 *   11.3-0-010
 *
 * @param Zone
 *   The zone needing the node mapping allocated.
 *
 * @return
 *   TRUE if the node mapping was successfully allocated, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataNodeAlloc(
 *   &                   Zone)
 *    INTEGER*4       Zone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Allocate the node map of zone 3.
 *
 * @code
 *   IsOk = TecUtilDataNodeAlloc(3);
 * @endcode
 *
 * @sa TecUtilDataConnectShare(), TecUtilDataNodeCustomLOD()
 *
 * @ingroup DataServices
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataNodeAlloc(EntIndex_t Zone);

/**
 * Lock Tecplot. For every call to TecUtilLockOn(), you must have a matching
 * call to TecUtilLockOff(). However, both these functions have been replaced
 * by TecUtilLockStart and TecUtilLockFinish(). The only time these functions should be used in
 * lieu of TecUtilLockStart() and TecUtilLock Finish() is in the InitTecAddon
 * function when an AddonID has not yet been created. See "Locking and
 * Unlocking Tecplot," in the ADK User's Manual for more information on locks
 * in Tecplot.
 *
 * @sa TecUtilLockStart()
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilLockOn()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Lock
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilLockOn(void);

/**
 * Unlock Tecplot. For every call to TecUtilLockOff(), you must have a matching
 * call to TecUtilLockOn().  However, both these functions have been replaced
 * by TecUtilLockStart() and TecUtilLockFinish(). The only time these functions
 * should be used in lieu of TecUtilLockStart() and TecUtilLockFinish() is in the
 * InitTecAddon function when an AddonID has not yet been created. See "Locking
 * and Unlocking Tecplot," in the ADK User's Manual for more information on
 * locks in Tecplot.
 *
 * @sa TecUtilLockFinish()
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilLockOff()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Lock
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilLockOff(void);

/**
 * Lock Tecplot. For every call to TecUtilLockStart(), you must have a matching
 * call to TecUtilLockFinish(). See "Locking and Unlocking Tecplot," in the ADK
 * User's Manual for more information on locks in Tecplot.
 * This function is \ref threadsafe.
 *
 * @param AddOn
 *   The add-on id from which the function is called
 *
 * @pre Add-on ID must be a valid lock handle.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilLockStart(AddOnPtr)
 *    POINTER (AddOnPtr, AddOn)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Lock
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void   STDCALL TecUtilLockStart(AddOn_pa AddOn);

/**
 * Unlock Tecplot. Call only after you have first called TecUtilLockStart().
 * See "Locking and Unlocking Tecplot," in the ADK User's Manual for more
 * information on locks in Tecplot.
 * This function is \ref threadsafe.
 *
 * @param AddOn
 *   The add-on id from which the function is called
 *
 * @pre Add-on ID must be a valid lock handle.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilLockFinish(AddOnPtr)
 *    POINTER (AddOnPtr, AddOn)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Lock
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void   STDCALL TecUtilLockFinish(AddOn_pa AddOn);
/**
 * Queries for and returns the name of the object currently locking Tecplot.
 *
 * @return
 *   Character string containing the name of the lock owner. You must free this
 *   string using TecUtilStringDealloc() when finished.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilLockGetCurrentOwnerName(
 *   &           Result,
 *   &           ResultLength)
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   char *Name = NULL;
 *
 * @code
 *   name = TecUtilLockGetCurrentOwnerName(void);
 *   if (Name)
 *     {
 *      TecUtilStringDealloc(&Name);
 *     }
 * @endcode
 *
 * @ingroup Lock
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilLockGetCurrentOwnerName(void);

/**
 * Called when a drag operation on a slice start.
 *
 * @since
 *   14.2
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilSliceStartDragging()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilSliceSolidPlaneSetPosition, TecUtilSliceFinishDragging,
 *     TecUtilQuerySlicesDrawAsSolidPlane, TecUtilSetSlicesDrawAsSolidPlane
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON void STDCALL TecUtilSliceStartDragging(void);

/**
 * Called to show a solid plane instead of the full slice in a
 * drag operation.
 * \ref TecUtilSliceStartDragging should be called before to call 
 * this function and \ref TecUtilSetSlicesDrawAsSolidPlane should
 * be set to TRUE.
 * \ref TecUtilSliceFinishDragging should be called after to
 * call this function.
 *
 * @param slicePosition
 *   Slice position for the slider control. Set in terms of the fractional 
 *   distance between 0-1 where 0 will set the slider control to it's min 
 *   setting and 1 to its max setting.
 *
 * @return
 *   The accepted value where the slice was moved.
 *
 * @since
 *   14.2
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilSliceSolidPlaneSetPosition(
 *   &                   slicePosition)
 *    REAL*8          slicePosition
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilSliceStartDragging, TecUtilSliceFinishDragging,
 *     TecUtilQuerySlicesDrawAsSolidPlane, TecUtilSetSlicesDrawAsSolidPlane
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON double STDCALL TecUtilSliceSolidPlaneSetPosition(double slicePosition);

/**
 * Called when a slice drag operation is finish.
 *
 * @since
 *   14.2
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilSliceFinishDragging()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilSliceStartDragging, TecUtilSliceSolidPlaneSetPosition, 
 *     TecUtilQuerySlicesDrawAsSolidPlane, TecUtilSetSlicesDrawAsSolidPlane
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON void STDCALL TecUtilSliceFinishDragging(void);

/**
 * Called when a long operation that needs to present percent done information
 * on the status line begins.
 *
 * @param PercentDoneText
 *   Text string to initally display. This can be changed as progress continues
 *   by calling TecUtilStatusSetPercentDoneText().
 *
 * @param ShowStopButton
 *   TRUE to show button, FALSE otherwise.
 *
 * @param ShowProgressBar
 *   TRUE to show progress bar, FALSE otherwise.
 *
 *
 * @pre <em>PercentDoneText</em>
 *   Pointer must be a valid address and non-NULL.
 * @pre TecUtilStatusStartPercentDone should not be called while in the middle of another operation.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStatusStartPercentDone(
 *   &           PercentDoneText,
 *   &           ShowStopButton,
 *   &           ShowProgressBar)
 *    CHARACTER*(*)   PercentDoneText
 *    INTEGER*4       ShowStopButton
 *    INTEGER*4       ShowProgressBar
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilStatusSetPercentDoneText, TecUtilStatusCheckPercentDone,
 *     TecUtilStatusFinishPercentDone
 *
 * @ingroup StatusBar
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilStatusStartPercentDone(const char *PercentDoneText,
                                                       Boolean_t   ShowStopButton,
                                                       Boolean_t   ShowProgressBar);

/**
 * Sets the string to displayed on the percent done status line.
 *
 * @param PercentDoneText
 *   String to displayed.
 *
 *
 * @pre <em>PercentDoneText</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStatusSetPercentDoneText(PercentDoneText)
 *    CHARACTER*(*) PercentDoneText
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilStatusStartPercentDone, TecUtilStatusCheckPercentDone,
 *     TecUtilStatusFinishPercentDone
 *
 * @ingroup StatusBar
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilStatusSetPercentDoneText(const char *PercentDoneText);

/**
 * Tells the status line what percentage of the task is complete.
 *
 * @par Note:
 *   If you are simply wanting to check the interrupted state of Tecplot then
 *   call \ref TecUtilInterruptCheck instead. Note that the sense of the return
 *   value between these two functions is reversed.
 *
 * @param PercentDone
 *   Integer value between 0 and 100 indicating the progress of the operation.
 *
 * @return
 *   TRUE if the task was NOT interrupted or FALSE if the user interrupted the
 *   task.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStatusCheckPercentDone(PercentDone)
 *    INTEGER*4 PercentDone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilInterruptCheck, TecUtilStatusStartPercentDone,
 *     TecUtilStatusSetPercentDoneText, TecUtilStatusFinishPercentDone
 *
 * @ingroup StatusBar
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilStatusCheckPercentDone(int PercentDone);

/**
 * Called when a long operation that needs to present percent done information
 * on the status line completes. A call to TecUtilStatusStartPercentDone() must
 * have preceded this call.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStatusFinishPercentDone()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilStatusStartPercentDone, TecUtilStatusSetPercentDoneText,
 *     TecUtilStatusCheckPercentDone
 *
 * @ingroup StatusBar
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilStatusFinishPercentDone(void);

/**
 * Launch the Percent Done dialog.
 *
 * @param Label
 *   Text to describe the action about to be performed.
 *
 * @param ShowTheScale
 *   Set to TRUE if you want the scale to be used, otherwise set to FALSE
 *
 *
 * @pre <em>Label</em>
 *   String must have a valid address and non-zero length.
 * @pre TecUtilDialogLaunchPercentDone should not be called while in the middle of another operation.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDialogLaunchPercentDone(
 *   &           Label,
 *   &           ShowTheScale)
 *    CHARACTER*(*)   Label
 *    INTEGER*4       ShowTheScale
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilDialogLaunchPercentDone(const char *Label,
                                                        Boolean_t   ShowTheScale);
/**
 * Update the text in the Percent Done dialog.
 *
 * @param Text
 *   Text to display in the percent done dialog.
 *
 *
 * @pre <em>Text</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDialogSetPercentDoneText(Text)
 *    CHARACTER*(*) Text
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Update the text in the Percent Done dialog to say "Phase II."
 *
 * @code
 *   TecUtilDialogSetPercentDoneText("Phase II");
 * @endcode
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilDialogSetPercentDoneText(const char *Text);

/**
 * Set the current value of the Percent Done dialog and check to see if the
 * user has clicked Cancel.
 *
 * @par Note:
 *   This function cannot be called when Tecplot is running in batch mode.
 *
 * @param PercentDone
 *   Value to which Percent Done dialog is to be set. If the
 *   TecUtilDialogLaunchPercentDone() call had ShowTheScale set to FALSE, then
 *   this parameter is ignored.
 *
 * @return
 *   Returns TRUE if the user has not clicked Cancel (that is, it is OK to
 *   continue processing).  Returns FALSE if Cancel has been clicked.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogCheckPercentDone(PercentDone)
 *    INTEGER*4 PercentDone
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Launch, check, and destroy Percent Done dialog.
 *
 * @code
 *   TecUtilDialogLaunchPercentDone("Calculate",TRUE);
 *   // do some processing
 *   if (!TecUtilDialogCheckPercentDone(35))
 *     {
 *       // user pressed cancel button
 *     }
 *   else
 *     {
 *       // do some more processing
 *     }
 *   // finished processing
 *   TecUtilDialogDropPercentDone();
 * @endcode
 *
 * @sa TecUtilMacroIsBatchModeActive()
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogCheckPercentDone(int PercentDone);

/**
 *   Drop the Percent Done dialog.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDialogDropPercentDone()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL      TecUtilDialogDropPercentDone(void);

/**
 * Instruct Tecplot to execute a single macro command. The macro command is supplied as a string.
 * Currently this command is restricted as follows:
 *     - Only commands that do not require raw data are accepted.
 *     - Command must be all on one line-no newlines.
 *
 * See the Tecplot Reference Manual for details about Tecplot's macro language.
 *
 * @param Command
 *   Macro command. This must not be NULL
 *
 * @return
 *   TRUE if Command executed successfully, otherwise FALSE.
 *
 *
 * @pre <em>Command</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroExecuteCommand(Command)
 *    CHARACTER*(*) Command
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Execute a macro command to animate the I-planes:
 *
 * @code
 *   TecUtilMacroExecuteCommand("$!ANIMATEIJKPLANES PLANES = I");
 * @endcode
 *
 * @ingroup ScriptSupport
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroExecuteCommand(const char *Command);

/**
 *   Set the value for a macro variable. Any macro executed after this call may then reference the
 *   value using |macrovar|.
 *
 * @param MacroVar
 *   Name of the macro variable you want to assign a value.
 *
 * @param ValueString
 *   Value to assign to MacroVar. Must be a valid string of length greater than zero
 *
 * @return
 *   TRUE if the MacroVar is a valid variable name and memory could be allocated to store ValueString,
 *   FALSE otherwise.
 *
 *
 * @pre <em>MacroVar</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>ValueString</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroSetMacroVar(
 *   &                   MacroVar,
 *   &                   ValueString)
 *    CHARACTER*(*)   MacroVar
 *    CHARACTER*(*)   ValueString
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Assign a file name to the macro variable FNAME, then use it in a Tecplot macro:
 *
 * @code
 *   IsOk = TecUtilMacroSetMacroVar("FName","/home/george/test.dat");
 *   ....
 *   In a later macro you can reference |FName|:
 *   $!ReadDataSet "|FName|"
 * @endcode
 *
 * @ingroup ScriptSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroSetMacroVar(const char *MacroVar,
                                                      const char *ValueString);

/**
 * Checks if Tecplot is in an interrupted state. Unlike TecUtilInterruptCheck() which, if called
 * from the user interface thread, will examine the event loop for interrupts and as a consequence
 * process any exposure events, TecUtilInterruptIsSet() simply examines Tecplot's interrupted
 * flag. Therefore it is guaranteed to have the lowest overhead for checking if Tecplot is in an
 * interrupted state from multiple threads. TecUtilInterruptCheck() and TecUtilInterruptIsSet()
 * are nearly equivalent in performance if a query is made from a non user interface thread.
 * This function is \ref threadsafe.
 *
 * @since
 *   14.1
 *
 * @return
 *   TRUE if Tecplot is in an interrupted state, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilInterruptIsSet()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilInterrupt, TecUtilStatusCheckPercentDone, TecUtilInterruptCheck
 *
 * @ingroup Utilities
 */
LINKTOADDON Boolean_t STDCALL TecUtilInterruptIsSet(void);

/**
 * Checks if Tecplot is in an interrupted state. This function should be called
 * during long tasks that are performed within the add-on where the user might
 * want to interrupt the process by pressing a key.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.0-1-075
 *
 * @return
 *   TRUE if Tecplot is in an interrupted state, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilInterruptCheck()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilInterrupt, TecUtilStatusCheckPercentDone, TecUtilInterruptIsSet
 *
 * @ingroup Utilities
 */
LINKTOADDON Boolean_t STDCALL TecUtilInterruptCheck(void);

/**
 * Interrupt Tecplot execution. This is mainly for use with addons which use
 * timers.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilInterrupt()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   The function NewDataIsReady is assumed to be called when a timer goes off and some new data is
 *   ready to be read into Tecplot. OldDataIsNotDoneDrawingYet is a boolean which is TRUE if the
 *   previous data is still being drawn in Tecplot's workspace. Interrupt Tecplot if the new data is
 *   ready before the old data is finished redrawing.
 *
 * @code
 *   extern Boolean_t OldDataIsNotDoneDrawingYet;
 *   void NewDataIsReady (void)
 *   {
 *     if (OldDataIsNotDoneDrawingYet)
 *       TecUtilInterrupt();
 *     //  Load the new data into Tecplot and redraw
 *   }
 * @endcode
 *
 * @ingroup Utilities
 */
LINKTOADDON void STDCALL TecUtilInterrupt(void);

/**
 *   Deletes the specified geometry object.
 *
 * @param GID
 *   Handle to a geometry object
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomDelete(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Delete the first geometry object from the list of geometry objects maintained by the current
 *   frame.
 *
 * @code
 *   Geom_ID Geom;
 *   Geom = TecUtilGeomGetBase();
 *   if (Geom != TECUTILBADID)
 *     {
 *       TecUtilGeomDelete(Geom);
 *     }
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeomDelete(Geom_ID GID);

/**
 *   Deletes the specified text object.
 *
 * @param TID
 *   Handle to a text object
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextDelete(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Delete the first text object from the list of text objects maintained by the current frame.
 *
 * @code
 *   Text_ID Text;
 *
 *   Text = TecUtilTextGetBase();
 *   if (Text != TECUTILBADID)
 *     {
 *       TecUtilTextDelete(Text);
 *     }
 * @endcode
 *
 * @ingroup Text
 *
 */
LINKTOADDON void STDCALL TecUtilTextDelete(Text_ID TID);

/**
 *   Add the specified geometry to the pick list. See Section 17.4, "The Pick List," in the ADK User's
 *   Manual for a discussion of pick lists.
 *
 * @param GID
 *   Geometry ID to pick
 *
 * @return
 *   TRUE if successful. A return value of FALSE usually indicates that Tecplot's limit on the number
 *   of picked objects has been exceeded.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickGeom(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Pick the first geometry in the current frame:
 *
 * @code
 *   Geom_ID gid;
 *   gid = TecUtilGeomGetBase();
 *   if (gid ! = NULL)
 *      TecUtilPickGeom(gid);
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickGeom(Geom_ID GID);

/**
 *   Add the specified text to the pick list. See the ADK User's Manual for a discussion of pick lists.
 *
 * @param TID
 *   Text ID to pick
 *
 * @return
 *   TRUE if successful. A return value of FALSE usually indicates that Tecplot's limit on the number
 *   of picked objects has been exceeded.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickText(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Pick the first text in the current frame.
 *
 * @code
 *   Text_ID tid;
 *   tid = TecUtilTextGetBase();
 *   if (tid ! = NULL)
 *      TecUtilPickText(tid);
 * @endcode
 *
 * @ingroup Pick
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickText(Text_ID TID);

/**
 *   Validate a geometry ID.
 *
 * @param GID
 *   Geometry ID.
 *
 * @return
 *   TRUE if GID is a valid geometry ID. FALSE if not.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomIsValid(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Determine if a geometry ID is still valid, and if it is valid, change its color to red:
 *
 * @code
 *   extern Geom_ID g; // created elsewhere
 *   if ( TecUtilGeomIsValid(g) )
 *      TecUtilGeomSetColor(g, Red_C);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilGeomIsValid(Geom_ID GID);

/**
 *   Determine if the text object is valid in the current frame context.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @return
 *   TRUE if TID is a valid text object, otherwise FALSE.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextIsValid(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilTextIsValid(Text_ID TID);

/**
 *   Convert a text string using the old formatting syntax into the new formatting syntax.
 *
 * @param OldString
 *   Character string containing old formatting syntax.
 *
 * @param BaseFont
 *   Assumed base font used by the old string.
 *
 * @return
 *   Returns the converted character string. You must free this string when you are finished with it.
 *
 *
 * @pre <em>OldString</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringConvOldFormatting(
 *   &           OldString,
 *   &           BaseFont,
 *   &           Result,
 *   &           ResultLength)
 *    CHARACTER*(*)   OldString
 *    INTEGER*4       BaseFont
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilStringConvOldFormatting(const char *OldString,
                                                                  Font_e      BaseFont);
/**
 *   Allocate a character string. Use TecUtilStringDealloc() to deallocate
 *   strings allocated using TecUtilStringAlloc().
 *   This function is \ref threadsafe.
 *
 * @param MaxLength
 *   The usable length of the string. The size must be greater than or equal to
 *   zero
 *
 * @param DebugInfo
 *   Character string identifying the reason why the string is being allocated.
 *   This parameter is not yet enabled, but you still must supply a string
 *
 * @return
 *   Returns the address of the string or NULL if the memory cannot be allocated.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   The following example will allocate a string for displaying an error
 *   message and then deallocate it.
 *
 * @code
 *   char *S;
 *   S = TecUtilStringAlloc(80,"error message string");
 *   sprintf(S, "The error occurred on step %d", IStep);
 *   TecUtilDialogErrMsg(S);
 *   TecUtilStringDealloc(&S);
 * @endcode
 *
 * @ingroup Utilities
 *
 * #internalattributes exclude_fglue, exclude_tcl
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilStringAlloc(int         MaxLength, /* <-activex> */
                                                      const char* DebugInfo);
/**
 * Free a string previously allocated with TecUtilStringAlloc(), or one that
 * was allocated and returned as the result of calling any other TecUtilXxx
 * function.
 * This function is \ref threadsafe.
 *
 * @param S
 *   Reference to a valid string handle. Use TecUtilStringAlloc() to create a string
 *
 *
 * @pre <em>S</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>S</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringDealloc(
 *   &           S,
 *   &           SLength)
 *    CHARACTER*(*)   S
 *    INTEGER*4       SLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 * #internalattributes exclude_fglue, exclude_tcl
 */
LINKTOADDON void STDCALL TecUtilStringDealloc(TP_RECEIVES_GIVES char** S); /* <-activex> */


/* STRING LIST FUNCTIONS */
/**
 * Remove all members of the string list. See the Chapter "Using String Lists,"
 * in the ADK User's Manual for a discussion of string lists.
 * This function is \ref threadsafe.
 *
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a
 *   string list
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListClear(StringListPtr)
 *    POINTER (StringListPtr, StringList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Clear a string list so that it no longer maintains any strings items.
 *
 * @code
 *   Boolean_t     ClearNames = FALSE;
 *   StringList_pa Names = NULL;
 *
 *   // do some processing to get names
 *     .
 *     .
 *     .
 *
 *   if (ClearNames)
 *     {
 *       TecUtilStringListClear(Names);
 *       TecUtilDialogMessageBox("All names cleared.",
 *                               MessageBoxType_Information);
 *     }
 * @endcode
 *
 * @ingroup StringList
 *
 */
LINKTOADDON void STDCALL TecUtilStringListClear(StringList_pa StringList);

/**
 * Remove the specified number of strings beginning at the nth string. The
 * members following the items removed are shifted to fill the vacated space.
 * See the Chapter "Using String Lists," in the ADK User's Manual for a
 * discussion of string lists.
 * This function is \ref threadsafe.
 *
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a
 *   string list
 *
 * @param StringNumber
 *   Start position in the string list. Value must be greater than or equal to
 *   one, and less than or equal to the number of items maintained by the
 *   string list. Use TecUtilStringListGetCount() to get the number of strings in
 *   the string list
 *
 * @param Count
 *   Number of items to remove from the string list. Value must be greater than
 *   or equal to one, and less than or equal to the number of items remaining,
 *   including the string at the start position.  Use TecUtilStringListGetCount()
 *   to get the number of strings in the string list
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListRemoveStrings(
 *   &           StringListPtr,
 *   &           StringNumber,
 *   &           Count)
 *    POINTER         (StringListPtr, StringList)
 *    INTEGER*4       StringNumber
 *    INTEGER*4       Count
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Remove all but the first and last item from a name list.
 *
 * @code
 *   LgIndex_t     Count = 0;
 *   StringList_pa Names = NULL;
 *
 *   // do some processing to get names
 *     .
 *     .
 *     .
 *
 *   Count = TecUtilStringListGetCount(Names);
 *   TecUtilStringListRemoveStrings(Names, 2, Count-2);
 * @endcode
 *
 * @ingroup StringList
 *
 */
LINKTOADDON void STDCALL TecUtilStringListRemoveStrings(StringList_pa StringList,
                                                        LgIndex_t     StringNumber,
                                                        LgIndex_t     Count);
/**
 * Remove the nth string from the string list. The members following the
 * removed item are shifted to fill the vacated space. See the Chapter "Using
 * String Lists," in the ADK User's Manual for a discussion of string lists.
 * This function is \ref threadsafe.
 *
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a
 *   string list.
 *
 * @param StringNumber
 *   Number of the string to remove. Must be greater than or equal to one, and
 *   less than or equal to the number of items maintained by the string list.
 *   Use TecUtilStringListGetCount() to get the number of items
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListRemoveString(
 *   &           StringListPtr,
 *   &           StringNumber)
 *    POINTER         (StringListPtr, StringList)
 *    INTEGER*4       StringNumber
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Remove the first name from a name list.
 *
 * @code
 *   StringList_pa Names = NULL;
 *
 *   // do some processing to get names
 *     .
 *     .
 *     .
 *
 *   TecUtilStringListRemoveString(Names, 1);
 * @endcode
 *
 * @ingroup StringList
 *
 */
LINKTOADDON void STDCALL TecUtilStringListRemoveString(StringList_pa StringList,
                                                       LgIndex_t     StringNumber);
/**
 * Deallocate the string list members and handle, and set the handle to NULL.
 * See the Chapter "Using String Lists," in the ADK User's Manual for a
 * discussion of string lists.
 * This function is \ref threadsafe.
 *
 * @param StringList
 *   Reference to a valid string list handle. Use TecUtilStringListAlloc() to create a string list
 *
 *
 * @pre <em>StringList</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>StringList</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListDealloc(StringListPtr)
 *    POINTER (StringListPtr, StringList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create and then deallocate a string list:
 *
 * @code
 *   StringList_pa MyStrList = TecUtilStringListAlloc();
 *   .
 *   .
 *   .
 *   TecUtilStringListDealloc(&MyStrList);
 * @endcode
 *
 * @ingroup StringList
 *
 */
LINKTOADDON void STDCALL TecUtilStringListDealloc(TP_RECEIVES_GIVES StringList_pa* StringList);

/**
 * Create an empty string list. See the Chapter "Using String Lists," in the
 * ADK User's Manual for a discussion of string lists. Use
 * TecUtilStringListDealloc() to deallocate the string list when it is no longer
 * needed.
 * This function is \ref threadsafe.
 *
 * @return
 *   Handle to an empty string list. A handle of NULL is returned if sufficient memory is not
 *   available.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListAlloc(ResultPtr)
 *    POINTER (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Allocate and deallocate a string list.
 *
 * @code
 *   StringList_pa Names = NULL;
 *
 *   Names = TecUtilStringListAlloc();
 *   if (Names != NULL)
 *     {
 *       // do something with the name list, append, clear, etc
 *         .
 *         .
 *         .
 *
 *       // get rid of the name list
 *       TecUtilStringListDealloc(&Names);
 *     }
 * @endcode
 *
 * @ingroup StringList
 *
 */
LINKTOADDON TP_GIVES StringList_pa STDCALL TecUtilStringListAlloc(void);

/**
 * Append a copy of the string to the string list. The string list expands to
 * accommodate the additional item. See the Chapter "Using String Lists," in
 * the ADK User's Manual for a discussion of string lists.
 * This function is \ref threadsafe.
 *
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a string list.
 *
 * @param String
 *   A copy of String is appended to the string list. String may be NULL
 *
 * @return
 *   A return value of TRUE indicates the operation was successful. A return
 *   value of FALSE indicates that sufficient memory was not available for the
 *   additional item.
 *
 *
 * @pre <em>String</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStringListAppendString(
 *   &                   StringListPtr,
 *   &                   String)
 *    POINTER         (StringListPtr, StringList)
 *    CHARACTER*(*)   String
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Append two variable names to a string list
 *
 * @code
 *   Boolean_t     IsOk = FALSE;
 *   StringList_pa Names = NULL;
 *
 *   Names = TecUtilStringListAlloc();
 *   if (Names != NULL)
 *     {
 *       IsOk = TecUtilStringListAppendString(Names, "X");
 *       IsOk = TecUtilStringListAppendString(Names, "Y");
 *       if (IsOk)
 *         {
 *           // do some processing with the name list
 *             .
 *             .
 *             .
 *         }
 *
 *       // get rid of the name list
 *       TecUtilStringListDealloc(&Names);
 *     }
 * @endcode
 *
 * @ingroup StringList
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStringListAppendString(StringList_pa StringList,
                                                            const char    *String);
/**
 * Count the number of strings currently maintained by the string list. See the
 * Chapter "Using String Lists," in the ADK User's Manual for a discussion of
 * string lists.
 * This function is \ref threadsafe.
 *
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to create a string list.
 *
 * @return
 *   The number of strings maintained by the string list.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStringListGetCount(StringListPtr)
 *    POINTER (StringListPtr, StringList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get each instruction used to load the current frame's data set:
 *
 * @code
 *   StringList_pa LoaderInstructs;
 *   char *LoaderName = NULL;
 *   if (TecUtilImportGetLoaderInstr(&LoaderName,&LoaderInstructs))
 *     {
 *      LgIndex_t ii, Count =
 *        TecUtilStringListGetCount(LoaderInstructs);
 *      for (ii = 1; ii <= Count; ii++)
 *       {
 *        char *Instruct =
 *          TecUtilStringListGetString(LoaderInstructs, ii);
 *        // Do some processing
 *        .
 *        .
 *        .
 *        TecUtilStringDealloc(&Instruct);
 *       }
 *     }
 * @endcode
 *
 * @ingroup StringList
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilStringListGetCount(StringList_pa StringList);

/**
 * Return a reference to the nth string in a string list. See the Chapter
 * "Using String Lists", in the ADK User's Manual for a discussion of string
 * lists.
 * This function is \ref threadsafe.
 *
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a string list
 *
 * @param StringNumber
 *   Position of string to be copied into the string list. StringNumber must be greater than or equal
 *   to one, and less than or equal to the number of items maintained by the string list. Use
 *   TecUtilStringListGetCount() to get the number of items.
 *
 * @return
 *   Returns a REFERENCE to the string. DO OT DEALLOCATE THIS REFERENCE.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Operate on the set of files retrieved using TecUtilDialogGetFileNames().
 * @code
 *   StringList_pa FileNames = NULL;
 *
 *   if (TecUtilDialogGetFileNames(SelectFileOption_ReadMultiFile,
 *                                 &FileNames,
 *                                 "any file",
 *                                 (StringList_pa)NULL,
 *                                 "*"))
 *     {
 *       LgIndex_t N,NumFiles;
 *
 *       NumFiles = TecUtilStringListGetCount(FileNames);
 *       for (N = 1; N < Numfiles; N++)
 *         {
 *           const char *RawFNamePtr = TecUtilStringListGetRawStringPtr(FileNames,N);
 *
 *           //
 *           // Do something with RawFNamePtr.  DO NOT DEALLOCATE RawFNamePtr.
 *           //
 *         }
 *
 *       //
 *       // We do however dealloc the stringlist itself.
 *       //
 *
 *       TecUtilStringListDealloc(&FileNames);
 *     }
 * @endcode
 *
 * @ingroup StringList
 *
 */
LINKTOADDON const char * STDCALL TecUtilStringListGetRawStringPtr(StringList_pa StringList,
                                                                  LgIndex_t     StringNumber);
/**
 * Return a copy of the nth string from a string list. See the Chapter "Using
 * String Lists," in the ADK User's Manual for a discussion of string lists.
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   The caller is responsible for de-allocating the copy of the string when it
 *   is no longer needed.
 *
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a string list
 *
 * @param StringNumber
 *   Position of string to be copied into the string list. StringNumber must be
 *   greater than or equal to one, and less than or equal to the number of
 *   items maintained by the string list. Use TecUtilStringListGetCount() to get
 *   the number of items
 *
 * @return
 *   Copy of the nth string.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListGetString(
 *   &           StringListPtr,
 *   &           StringNumber,
 *   &           Result,
 *   &           ResultLength)
 *    POINTER         (StringListPtr, StringList)
 *    INTEGER*4       StringNumber
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup StringList
 *
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilStringListGetString(StringList_pa StringList,
                                                              LgIndex_t     StringNumber);
/**
 * Place a copy of the specified string at the nth position in the string list.
 * If the position is beyond the end of the string list, the string list is
 * resized, so that the string references between the last item of the string
 * list in its original state and the last item of the string list in its new
 * state are assigned NULL. If the position is within the boundaries of the
 * original string list, the string at the specified position is replaced by
 * the new value. See the Chapter "Using String Lists," in the ADK User's
 * Manual for a discussion of string lists.
 * This function is \ref threadsafe.
 *
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a
 *   string list
 *
 * @param StringNumber
 *   Item position in the string list. Value must be greater than or equal to one
 *
 * @param String
 *   A copy of String is appended to the string list. String may be NULL
 *
 * @return
 *   A return value of TRUE indicates the operation was successful. FALSE
 *   indicates that sufficient memory was not available for the additional item
 *   at the specified position.
 *
 *
 * @pre <em>String</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStringListSetString(
 *   &                   StringListPtr,
 *   &                   StringNumber,
 *   &                   String)
 *    POINTER         (StringListPtr, StringList)
 *    INTEGER*4       StringNumber
 *    CHARACTER*(*)   String
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Replace the first item of a name list with a new value and put a new item ten positions past the
 *   current last item.
 *
 * @code
 *   LgIndex_t     Count = 0;
 *   StringList_pa Names = NULL;
 *
 *   // do some processing to get names
 *     .
 *     .
 *     .
 *
 *   IsOk = TecUtilStringListSetString(Names, 1,
 *                                     "New First Name");
 *   Count = TecUtilStringListGetCount(Names);
 *   IsOk = TecUtilStringListSetString(Names, Count+10,
 *                                     "New Last Name");
 * @endcode
 *
 * @ingroup StringList
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStringListSetString(StringList_pa  StringList,
                                                         LgIndex_t      StringNumber,
                                                         const char    *String);
/**
 * Insert a copy of the string into the nth position of the string list. The
 * string list expands and the items are shifted to accommodate the additional
 * item. See the Chapter "Using String Lists," in the ADK User's Manual for a
 * discussion of string lists.
 * This function is \ref threadsafe.
 *
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a string list
 *
 * @param StringNumber
 *   Position where string is inserted in the string list. This value must be
 *   greater than or equal to one, and less than or equal to the number of
 *   items maintained by the string list. Use TecUtilStringListGetCount() to get
 *   the number of items
 *
 * @param String
 *   A copy of String is inserted into the string list. String may be NULL
 *
 * @return
 *   A return value of TRUE indicates the operation was successful. FALSE indicates that the memory
 *   available was not sufficient for the additional item.
 *
 *
 * @pre <em>String</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStringListInsertString(
 *   &                   StringListPtr,
 *   &                   StringNumber,
 *   &                   String)
 *    POINTER         (StringListPtr, StringList)
 *    INTEGER*4       StringNumber
 *    CHARACTER*(*)   String
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Insert a string at the beginning and end of an existing list.
 *
 * @code
 *   Boolean_t     IsOk = FALSE;
 *   StringList_pa Names = NULL;
 *   LgIndex_t     Count = 0;
 *
 *   // do some processing to get names
 *     .
 *     .
 *     .
 *
 *   // insert a name at the beginning and end of the list
 *   IsOk = TecUtilStringListInsertString(Names, 1,
 *                                        "Very First Name");
 *   Count = TecUtilStringListGetCount(Names);
 *   IsOk = TecUtilStringListInsertString(Names, Count+1,
 *                                        "Very Last Name");
 * @endcode
 *
 * @ingroup StringList
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStringListInsertString(StringList_pa  StringList,
                                                            LgIndex_t      StringNumber,
                                                            const char    *String);
/**
 * Return a handle to a duplicate of the specified string list and its
 * contents. See the Chapter "Using String Lists," in the ADK User's Manual for
 * a discussion of string lists.
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   The caller is responsible for deallocating the string list when it is no
 *   longer needed.
 *
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to create a string list.
 *
 * @return
 *   A handle to a duplicate string list is returned if the operation was
 *   successful. A handle of NULL is returned if sufficient memory is not
 *   available.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListCopy(
 *   &           StringListPtr,
 *   &           ResultPtr)
 *    POINTER         (StringListPtr, StringList)
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Make a copy of a string list.
 *
 * @code
 *   StringList_pa Names = NULL;
 *   StringList_pa CopyOfNames = NULL;
 *
 *   // do some processing to get names
 *     .
 *     .
 *     .
 *
 *   CopyOfNames = TecUtilStringListCopy(Names);
 *   if (CopyOfNames != NULL)
 *     {
 *       // do some processing on the name list copy
 *         .
 *         .
 *         .
 *
 *       // get rid of the name list copy
 *       TecUtilStringListDealloc(&CopyOfNames);
 *     }
 * @endcode
 *
 * @ingroup StringList
 *
 */
LINKTOADDON TP_GIVES StringList_pa STDCALL TecUtilStringListCopy(StringList_pa StringList);

/**
 * Append a copy of the contents of the source string list to the target string
 * list. See the Chapter "Using String Lists," in the ADK User's Manual for a
 * discussion of string lists.
 * This function is \ref threadsafe.
 *
 * @param Target
 *   String list to which the Source string list is appended. Use
 *   TecUtilStringListAlloc() to allocate a string list
 *
 * @param Source
 *   String list to append to the Target. Use TecUtilStringListAlloc() to
 *   allocate a string list.
 *
 * @return
 *   A return value of TRUE indicates the operation was successful. A return
 *   value of FALSE indicates that sufficient memory was not available for the
 *   request.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStringListAppend(
 *   &                   TargetPtr,
 *   &                   SourcePtr)
 *    POINTER         (TargetPtr, Target)
 *    POINTER         (SourcePtr, Source)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Append one string list to another.
 *
 * @code
 *   Boolean_t     IsOk = FALSE;
 *   StringList_pa Names = NULL;
 *   StringList_pa DefaultNames = NULL;
 *
 *   // call some function to get a names list from the user
 *   Names = MyFuncForGettingNamesFromUser();
 *
 *   // call some function to get some default name list
 *   DefaultNames = MyFuncForGettingDefaultNames();
 *
 *   // combine the two name lists into one
 *   if (Names != NULL && DefaultNames != NULL)
 *     {
 *       IsOk = TecUtilStringListAppend(Names, DefaultNames);
 *       if (IsOk)
 *         {
 *           // do more processing
 *             .
 *             .
 *             .
 *
 *           // get rid of the name lists
 *           TecUtilStringListDealloc(&Names);
 *           TecUtilStringListDealloc(&DefaultNames);
 *         }
 *     }
 * @endcode
 *
 * @ingroup StringList
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilStringListAppend(StringList_pa Target,
                                                      StringList_pa Source);
/**
 * Return a newline delimited string representation of the string list. A
 * newline delimited string is a character string with newlines (\\n) used to
 * separate one substring from the next. See the chapter on "Using String
 * Lists" in the ADK User's Manual for a discussion of string lists.
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   The caller is responsible for de-allocating the copy of the newline
 *   delimited string when it is no longer needed.
 *
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a string list.
 *
 * @return
 *   A newline, (\\n), delimited string representation of the string list.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListToNLString(
 *   &           StringListPtr,
 *   &           Result,
 *   &           ResultLength)
 *    POINTER         (StringListPtr, StringList)
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Given a string list containing 3 members: "Hello", "", and "World", the function will return the
 *   following string: "Hello\\n\\nWorld".
 *
 * @code
 *   StringList_pa List = NULL;
 *
 *   List = TecUtilStringListAlloc();
 *   if (List != NULL)
 *     {
 *       // add items to the string list
 *       TecUtilStringListAppendString(List, "Hello");
 *       TecUtilStringListAppendString(List, "");
 *       TecUtilStringListAppendString(List, "World");
 *
 *       //print the newline separated string representation
 *       String = TecUtilStringListToNLString(List);
 *       if (String != NULL)
 *         {
 *           printf("%s\n", String);
 *           TecUtilStringDealloc(&String);
 *         }
 *
 *       // get rid of the list
 *       TecUtilStringListDealloc(&List);
 *     }
 * @endcode
 *
 * @ingroup StringList
 *
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilStringListToNLString(StringList_pa StringList);

/**
 * Create a string list from a newline delimited string. A newline delimited
 * string is a character string with newlines (\\n) used to separate one
 * substring from the next. See the Chapter "Using String Lists," in the ADK
 * User's Manual for a discussion of string lists.
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   The caller is responsible for deallocating the string list when it is no
 *   longer needed.
 *
 * @param String
 *   The newline delimited string
 *
 * @return
 *   Handle to the created string list. A handle of NULL is returned if
 *   sufficient memory is not available.
 *
 *
 * @pre <em>String</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListFromNLString(
 *   &           String,
 *   &           ResultPtr)
 *    CHARACTER*(*)   String
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Given the string, Hello\\n\\nWorld, the function will return a string list containing 3 members:
 *   "Hello, "" (that is, an empty string), and "World."
 *
 * @code
 *   StringList_pa List = NULL;
 *
 *   List = TecUtilStringListFromNLString("Hello\\n\\nWorld");
 *   if (List != NULL)
 *     {
 *       LgIndex_t I = 0;
 *       LgIndex_t Count = 0;
 *
 *       // print each element of the string list
 *       for (I = 0, Count = TecUtilStringListGetCount(List);
 *             I < Count;
 *             I++)
 *         {
 *           String = TecUtilStringListGetString(List, I+1);
 *           if (String != NULL)
 *             {
 *               printf("Item #%d: %s\n", I+1, String);
 *               TecUtilStringDealloc(&String);
 *             }
 *         }
 *
 *       // get rid of the list
 *       TecUtilStringListDealloc(&List);
 *     }
 * @endcode
 *
 * @ingroup StringList
 *
 */
LINKTOADDON TP_GIVES StringList_pa STDCALL TecUtilStringListFromNLString(const char *String);

/**
 * Sorts the string list by repeatedly calling the 'Comparator' function until
 * the list is in order.
 *
 * @since
 *   10.0-3-129
 *
 * @param StringList
 *     String list to sort.
 * @param Comparator
 *     Function called to compare two string list strings or NULL for the
 *     default sort. The default sorting handles NULL elements and uses the
 *     system's strcmp utility for comparing valid strings elements.
 * @param ClientData
 *     Contextual information that is passed along to the comparator function.
 *     Client data isn't used by the default comparator and can be passed any
 *     value. For specialized comparator functions the client data is used to
 *     hold contextual information so that global variable do not have to be
 *     used.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListSort(
 *   &           StringListPtr,
 *   &           ComparatorPtr,
 *   &           ClientDataPtr)
 *    POINTER         (StringListPtr, StringList)
 *    POINTER         (ComparatorPtr, Comparator)
 *    POINTER         (ClientDataPtr, ClientData)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Sort the variable string list using Tecplot's default comparator:
 * @code
 *   TecUtilStringListSort(MyVarList, NULL, 0);
 * @endcode
 *
 * Sort the variable string list using own own comparator function. We pass
 * some client data to our own comparator function simply to show how to use
 * it. In this case all the client data is used for is to keep track of the
 * number of times our comparator function was called... not very useful.
 * @code
 *   static int STDCALL MyStrComparator(const char *String1,
 *                                      const char *String2,
 *                                      ArbParam_t  ClientData)
 *   {
 *     int Result = 0;
 *     LgIndex_t *NumTimesCalled;
 *
 *     REQUIRE(VALID_REF(String1) || String1 == NULL);
 *     REQUIRE(VALID_REF(String2) || String2 == NULL);
 *
 *     NumTimesCalled = (LgIndex_t *)ClientData;
 *     (*NumTimesCalled) += 1;
 *
 *     if (String1 != NULL && String2 != NULL)
 *       Result = strcmp(String1, String2);
 *     else if (String1 == NULL && String2 == NULL)
 *       Result = 0;
 *     else if (String1 == NULL)
 *       Result = -1;
 *     else if (String2 == NULL)
 *       Result = 1;
 *     else
 *       CHECK(FALSE);
 *
 *     return Result;
 *   }
 *
 * ...
 *
 *   // After calling TecUtilStringListSort NumTimesCalled will contain the
 *   // number of times that our comparator was called.
 *   LgIndex_t NumTimesCalled = 0;
 *   TecUtilStringListSort(MyVarList, MyStrComparator, &NumTimesCalled);
 * @endcode
 *
 * @ingroup StringList
 *
 * #internalattributes exclude_tcl
 */
LINKTOADDON void STDCALL TecUtilStringListSort(StringList_pa                  StringList,
                                               StringListStringComparator_pf  Comparator,
                                               ArbParam_t                     ClientData);

/* * SET FUNCTIONS * */

/**
 * Allocate a new empty set. See the chapter "Using Sets" in the ADK User's
 * Manual for a discussion of sets.
 * This function is \ref threadsafe.
 *
 * @param ShowErr
 *   TRUE to display an error message if the function's return value is FALSE;
 *   FALSE to display no error message
 *
 * @return
 *   The new set if successful, NULL if not. An unsuccessful return value
 *   indicates that there was not enough memory to create a new set.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilSetAlloc(
 *   &           ShowErr,
 *   &           ResultPtr)
 *    INTEGER*4       ShowErr
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create two sets, A and B:
 *
 * @code
 *   Set_pa A, B;
 *   A = TecUtilSetAlloc(TRUE);
 *   B = TecUtilSetAlloc(TRUE);
 * @endcode
 *
 * @ingroup Set
 *
 */
LINKTOADDON TP_GIVES Set_pa STDCALL TecUtilSetAlloc(Boolean_t ShowErr);
/**
 * Free all memory associated with the specified set and assign the set to be NULL. See the
 * chapter "Using Sets" in the ADK User's Manual for a discussion of sets.
 * This function is \ref threadsafe.
 *
 * @param Set
 *   The set to deallocate.
 *
 *
 * @pre <em>Set</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Set</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilSetDealloc(SetPtr)
 *    POINTER (SetPtr, Set)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Set
 *
 */
LINKTOADDON void STDCALL TecUtilSetDealloc(TP_RECEIVES_GIVES Set_pa* Set);
/**
 * Copy one set to another. See the chapter "Using Sets" in the ADK User's
 * Manual for a discussion of sets.
 * This function is \ref threadsafe.
 *
 * @param DstSet
 *   The destination set, which must already be allocated with
 *   TecUtilSetAlloc().
 *
 * @param SrcSet
 *   The source set
 *
 * @param ShowErr
 *   Set to TRUE to display an error message if an error occurs during the call
 *
 * @return
 *   TRUE if successful, FALSE if not. FALSE indicates that SrcSet contains elements that cannot be
 *   added to DstSet.
 *
 *
 * @pre <em>DstSet</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>SrcSet</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetCopy(
 *   &                   DstSetPtr,
 *   &                   SrcSetPtr,
 *   &                   ShowErr)
 *    POINTER         (DstSetPtr, DstSet)
 *    POINTER         (SrcSetPtr, SrcSet)
 *    INTEGER*4       ShowErr
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Make a copy of the set of active Line-maps:
 *
 * @code
 *   Set_pa MySet, LineMaps = NULL;
 *   MySet = TecUtilSetAlloc(TRUE);
 *   TecUtilLineMapGetActive(&LineMaps);
 *   TecUtilSetCopy(MySet, LineMaps, TRUE);
 *   .
 *   .
 *   .
 *   TecUtilSetDealloc(&MySet);
 *   TecUtilSetDealloc(&LineMaps);
 * @endcode
 *
 * @ingroup Set
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilSetCopy(Set_pa    DstSet,
                                              Set_pa    SrcSet,
                                              Boolean_t ShowErr);
/**
 *   Empties the specified set. See the chapter "Using Sets" in the ADK User's Manual for a discussion
 *   of sets.
 * This function is \ref threadsafe.
 *
 * @param Set
 *   The set to empty
 *
 *
 * @pre <em>Set</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilSetClear(SetPtr)
 *    POINTER (SetPtr, Set)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the set of active zones, then clear the set so it can be used again:
 *
 * @code
 *   Set_pa Zones = NULL;
 *   TecUtilZoneGetActive(&Zones);
 *   .
 *   . // Use the set of active zones
 *   .
 *   TecUtilSetClear(Zones);
 *   .
 *   . // Use the set for something else
 * @endcode
 *
 * @ingroup Set
 *
 */
LINKTOADDON void       STDCALL TecUtilSetClear(Set_pa Set);
/**
 *   Add the specified member to the specified set. See the chapter "Using Sets" in the ADK User's
 *   Manual for a discussion of sets.
 * This function is \ref threadsafe.
 *
 * @param Set
 *   The set to which to add the specified member.
 *
 * @param Member
 *   The item to add to the specified set. Members start at one.
 *
 * @param ShowErr
 *   TRUE to display an error message if the function's return value is FALSE; FALSE to display no
 *   error message
 *
 * @return
 *   TRUE if successful, FALSE if not. A FALSE value is highly unlikely and only occurs if the set
 *   cannot be expanded to accomodate the new member.
 *
 *
 * @pre <em>Set</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetAddMember(
 *   &                   SetPtr,
 *   &                   Member,
 *   &                   ShowErr)
 *    POINTER         (SetPtr, Set)
 *    INTEGER*4       Member
 *    INTEGER*4       ShowErr
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a set called ZonesToDelete and add zones 2 and 4 to it:
 *
 * @code
 *   Set_pa ZonesToDelete = TecUtilSetAlloc(TRUE);
 *   TecUtilSetAddMember(ZonesToDelete, 2, TRUE);
 *   TecUtilSetAddMember(ZonesToDelete, 4, TRUE);
 * @endcode
 *
 * @ingroup Set
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilSetAddMember(Set_pa     Set,
                                                   SetIndex_t Member,
                                                   Boolean_t  ShowErr);
/**
 *   Remove a member from a set. See the chapter "Using Sets" in the ADK User's Manual for a
 *   discussion of sets.
 * This function is \ref threadsafe.
 *
 * @param Set
 *   The set from which to remove the specified member.
 *
 * @param Member
 *   The member to remove from the specified set. Members start at one
 *
 *
 * @pre <em>Set</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilSetRemoveMember(
 *   &           SetPtr,
 *   &           Member)
 *    POINTER         (SetPtr, Set)
 *    INTEGER*4       Member
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Set
 *
 */
LINKTOADDON void       STDCALL TecUtilSetRemoveMember(Set_pa     Set,
                                                      SetIndex_t Member);
/**
 *   Determine if the specified member is in the specified set. See the chapter "Using Sets" in the
 *   ADK User's Manual for a discussion of sets.
 * This function is \ref threadsafe.
 *
 * @param Set
 *   The set to check for the specified member.
 *
 * @param Member
 *   The item for which to check the specified set. Members start at one
 *
 * @return
 *   TRUE if Member is a member of Set, FALSE if not.
 *
 *
 * @pre <em>Set</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetIsMember(
 *   &                   SetPtr,
 *   &                   Member)
 *    POINTER         (SetPtr, Set)
 *    INTEGER*4       Member
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Determine if the set MySet contains the member MyMember, and if so, remove MyMember from MySet:
 *
 * @code
 *   Set_pa MySet;
 *   SetIndex_t MyMember;
 *   .
 *   .
 *   if (TecUtilSetIsMember(MySet, MyMember))
 *     TecUtilSetRemoveMember(MySet, MyMember);
 * @endcode
 *
 * @ingroup Set
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilSetIsMember(Set_pa     Set,
                                                  SetIndex_t Member);
/**
 *   Determine if the specified set is NULL or contains no members. See the chapter "Using Sets" in
 *   the ADK User's Manual for a discussion of sets.
 * This function is \ref threadsafe.
 *
 * @param Set
 *   The set to check for members
 *
 * @return
 *   TRUE if Set is NULL or contains no members, FALSE if not.
 *
 *
 * @pre <em>Set</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetIsEmpty(SetPtr)
 *    POINTER (SetPtr, Set)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Determine if the set MySet is empty or contains no members:
 *
 * @code
 *   Set_pa MySet;
 *
 *   if (TecUtilSetIsEmpty(MySet))
 *     {
 *       .... take action based on the set being empty.
 *     }
 * @endcode
 *
 * @ingroup Set
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilSetIsEmpty(Set_pa Set);



/**
 *   Get the count of the number of members in a set. See the chapter "Using Sets" in the ADK User's
 *   Manual for a discussion of sets.
 * This function is \ref threadsafe.
 *
 * @param Set
 *   The set for which to get the count
 *
 * @return
 *   The count of the number of members in the set Set.
 *
 *
 * @pre <em>Set</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetGetMemberCount(SetPtr)
 *    POINTER (SetPtr, Set)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Set
 *
 */
LINKTOADDON SetIndex_t STDCALL TecUtilSetGetMemberCount(Set_pa Set);



/**
 *   Determines if the specified sets are equal (have the same members). See the chapter "Using Sets"
 *   in the ADK User's Manual for a discussion of sets.
 * This function is \ref threadsafe.
 *
 * @param Set1
 *   The set to compare with Set2.
 *
 * @param Set2
 *   The set to compare with Set1
 *
 * @return
 *   TRUE if the specified sets are equal, FALSE if they are not.
 *
 *
 * @pre <em>Set1</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Set2</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetIsEqual(
 *   &                   Set1Ptr,
 *   &                   Set2Ptr)
 *    POINTER         (Set1Ptr, Set1)
 *    POINTER         (Set2Ptr, Set2)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Determine if all enabled zones are active:
 *
 * @code
 *   Boolean_t AllEnabledZonesAreActive;
 *   Set_pa ActiveZones  = NULL;
 *   Set_pa EnabledZones = NULL;
 *
 *   TecUtilZoneGetActive(&ActiveZones);
 *   TecUtilZoneGetEnabled(&EnabledZones);
 *   AllEnabledZonesAreActive = TecUtilSetIsEqual(ActiveZones, EnabledZones);
 * @endcode
 *
 * @ingroup Set
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilSetIsEqual(Set_pa Set1,
                                                 Set_pa Set2);
/**
 *
 *   Get the member of the specified set at the specified position. See the chapter "Using Sets" in
 *   the ADK User's Manual for a discussion of sets.
 * This function is \ref threadsafe.
 *
 * @param Set
 *   The set from which to get the member.
 *
 * @param Position
 *   The position in the set.
 *
 * @return
 *   The member of the specified set at the specified position. Members start at one. If the set does
 *   not contain a member at the specified position, the return value is TECUTILSETNOTMEMBER.
 *
 *
 * @pre <em>Set</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetGetMember(
 *   &                   SetPtr,
 *   &                   Position)
 *    POINTER         (SetPtr, Set)
 *    INTEGER*4       Position
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get each member from the set MySet:
 *
 * @code
 *   Set_pa MySet;
 *   .
 *   .
 *   SetIndex_t Member;
 *   SetIndex_t Count;
 *   SetIndex_t Position;
 *
 *   Count = TecUtilSetGetMemberCount(MySet);
 *   for (Position = 1; Position <= Count; Position++)
 *     {
 *       Member = TecUtilSetGetMember(MySet, Position);
 *       .
 *       .
 *     }
 * @endcode
 *
 * @ingroup Set
 *
 */
LINKTOADDON SetIndex_t STDCALL TecUtilSetGetMember(Set_pa     Set,
                                                   SetIndex_t Position);
/**
 *
 * Get the position in the specified set at which the specified member is located.
 * See the chapter "Using Sets" in the ADK User's Manual for a discussion of sets.
 * This function is \ref threadsafe.
 *
 * @param Set
 *   The set from which to get the member
 *
 * @param Member
 *   The member after which to get the position. Members start at one.
 *
 * @return
 *   The position in the specified set at which the specified member is located. If the specified
 *   member is not found, the return value is TECUTILSETNOTMEMBER.
 *
 *
 * @pre <em>Set</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetGetPosition(
 *   &                   SetPtr,
 *   &                   Member)
 *    POINTER         (SetPtr, Set)
 *    INTEGER*4       Member
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the position of the member MyMember of the set MySet:
 *
 * @code
 *   Set_pa MySet;
 *   SetIndex_t Member;
 *   .
 *   .
 *   SetIndex_t Position =
 *     TecUtilSetGetPosition(MySet, MyMember);
 * @endcode
 *
 * @ingroup Set
 *
 */
LINKTOADDON SetIndex_t STDCALL TecUtilSetGetPosition(Set_pa     Set,
                                                     SetIndex_t Member);
/**
 *   Get the next member in the specified set which is located after the specified member. See Chapter
 *   20, "Using Sets," in the ADK User's Manual for a discussion of sets.
 * This function is \ref threadsafe.
 *
 * @param Set
 *   The set from which to get the member
 *
 * @param Member
 *   The member after which to return the next member. Members start at one. Use TECUTILSETNOTMEMBER to
 *   get the first member of the set
 *
 * @return
 *   The next member of the specified set after the specified member. Members start at one. If the
 *   specified member is not found or if it is the last member in the set, the return value is
 *   TECUTILSETNOTMEMBER.
 *
 *
 * @pre <em>Set</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetGetNextMember(
 *   &                   SetPtr,
 *   &                   Member)
 *    POINTER         (SetPtr, Set)
 *    INTEGER*4       Member
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Loop through all members of the set MySet:
 *
 * @code
 *   Set_pa MySet;
 *   .
 *   .
 *   SetIndex_t Member = TecUtilSetGetNextMember(MySet,
 *                                               TECUTILSETNOTMEMBER);
 *
 *   while (Member != TECUTILSETNOTMEMBER)
 *      {
 *        .
 *        .
 *        Member = TecUtilSetGetNextMember(MySet, Member);
 *      }
 * @endcode
 *
 * @ingroup Set
 *
 */
LINKTOADDON SetIndex_t STDCALL TecUtilSetGetNextMember(Set_pa     Set,
                                                       SetIndex_t Member);


/**
 * Get the previous member in the specified set which is located before the specified member. See Chapter
 * 20, "Using Sets," in the ADK User's Manual for a discussion of sets.
 * This function is \ref threadsafe.
 *
 * @since
 *     12.1.1.8018
 *
 * @param Set
 *   The set from which to get the member
 *
 * @param Member
 *   The member before which to return the previous member. Members start at one. Use TECUTILSETNOTMEMBER to
 *   get the last member of the set
 *
 * @return
 *   The previous member of the specified set before the specified member. Members start at one. If the
 *   specified member is not found or if it is the first member in the set, the return value is
 *   TECUTILSETNOTMEMBER.
 *
 *
 * @pre <em>Set</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetGetPrevMember(
 *   &                   SetPtr,
 *   &                   Member)
 *    POINTER         (SetPtr, Set)
 *    INTEGER*4       Member
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Set
 *
 */
LINKTOADDON SetIndex_t STDCALL TecUtilSetGetPrevMember(Set_pa     Set,
                                                       SetIndex_t Member);


#if defined DOCUMENT_BUT_DONT_COMPILE
/*
 *  Entry provided here is dummied up so doxygen picks it up.   This
 *  code should not be compiled.
 */

/**
 * Convenience macro for looping through all members in a set.
 *
 * @par Note:
 *   This is a macro and not a function.
 *
 * @param Member
 *   Set member placeholder. This parameter does not have to be initialized before
 *   use.  Since TecUtilSetForEachMember is a macro you do not have to pass the
 *   address of Member.
 *
 * @param Set
 *   Any valid Tecplot set.
 *
 * Perform some processing on all active zones:
 * @code
 *   Set_pa ActiveZones;
 *   if ( TecUtilDataSetIsAvailable() &&
 *        TecUtilZoneGetActive(&ActiveZones))
 *     {
 *       TecUtilSetForEachMember(Zone, ActiveZones)
 *         {
 *           // do something with the active Zone
 *           ...
 *         }
 *       TecUtilSetDealloc(&ActiveZones);
 *     }
 * @endcode
 */
void TecUtilSetForEachMember(SetIndex_t Member,
                             Set_pa     Set);
#endif


#define TecUtilSetForEachMember(Member, Set) \
            for (Member = TecUtilSetGetNextMember(Set, TECUTILSETNOTMEMBER); \
                 Member != TECUTILSETNOTMEMBER; \
                 Member = TecUtilSetGetNextMember(Set, Member))

/**
 * Convert the specified X-coordinate value from one coordinate system to
 * another.
 *
 * @param OldCoordSys
 *   The coordinate system in which OldX is currently defined.
 *   The possible values are: \ref CoordSys_Grid, \ref CoordSys_Frame,
 *   \ref CoordSys_Paper, or \ref CoordSys_Screen.
 *
 * @param NewCoordSys
 *   The coordinate system into which to transform OldX.
 *   The possible values are: \ref CoordSys_Grid, \ref CoordSys_Frame,
 *   \ref CoordSys_Paper, or \ref CoordSys_Screen.
 *
 * @param OldX
 *   The value to convert from one coordinate system to another
 *
 * @return
 *   The value OldX converted into the new coordinate system.
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilConvertXPosition(
 *   &                   OldCoordSys,
 *   &                   NewCoordSys,
 *   &                   OldX)
 *    INTEGER*4       OldCoordSys
 *    INTEGER*4       NewCoordSys
 *    REAL*8          OldX
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Determine the position on the paper of a text label. Assume the Text_ID has
 * already been obtained (See TecUtilPickListXxx functions or TecUtilTextXxx
 * functions for examples on how to obtain a Text_ID).
 *
 * @code
 *   Text_ID TID;
 *   double  XPos,YPos;
 *   double PaperXPos,PaperYPos;
 *
 *   ... Text_ID obtained....
 *
 *   TecUtilTextGetXYPos(TID,&XPos,&YPos);
 *
 *   PaperXPos = TecUtilConvertXPosition(TecUtilTextGetPositionCoordSys(TID),
 *                                       CoordSys_Paper,
 *                                       XPos);
 *   PaperYPos = TecUtilConvertYPosition(TecUtilTextGetPositionCoordSys(TID),
 *                                       CoordSys_Paper,
 *                                       YPos);
 * @endcode
 *
 * @ingroup Axis
 *
 */
LINKTOADDON double STDCALL TecUtilConvertXPosition(CoordSys_e  OldCoordSys,
                                                   CoordSys_e  NewCoordSys,
                                                   double      OldX);
/**
 * Convert the specified horizontal dimension from one coordinate system to another.
 *
 * @param OldCoordSys
 *   Coordinate system in which OldDimension is measured.
 *   The possible values are: \ref CoordSys_Grid, \ref CoordSys_Frame,
 *   \ref CoordSys_Paper, or \ref CoordSys_Screen
 *
 * @param NewCoordSys
 *   Coordinate system in which the return value is measured.
 *   The possible values are: \ref CoordSys_Grid, \ref CoordSys_Frame,
 *   \ref CoordSys_Paper, or \ref CoordSys_Screen
 *
 * @param OldDimension
 *   Dimension to convert
 *
 * @return
 *   Converted dimension in the new coordinate system.
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilConvertXDimension(
 *   &                   OldCoordSys,
 *   &                   NewCoordSys,
 *   &                   OldDimension)
 *    INTEGER*4       OldCoordSys
 *    INTEGER*4       NewCoordSys
 *    REAL*8          OldDimension
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Find the size of the current frame in screen pixels.
 *
 * @code
 *   double X, Y, Width, Height;
 *   // get frame width and height in inches
 *   TecUtilFrameGetPosAndSize(&X,&Y, &Width, &Height);
 *   // convert width and height to screen coordinates (pixels)
 *   Width = TecUtilConvertXDimension(CoordSys_Paper,
 *                                    CoordSys_Screen, Width);
 *   Height = TecUtilConvertYDimension(CoordSys_Paper,
 *                                     CoordSys_Screen, Height);
 * @endcode
 *
 * @ingroup Axis
 *
 */
LINKTOADDON double STDCALL TecUtilConvertXDimension(CoordSys_e  OldCoordSys,
                                                    CoordSys_e  NewCoordSys,
                                                    double      OldDimension);
/**
 * Convert the specified Y-coordinate value from one coordinate system to
 * another.
 *
 * @param OldCoordSys
 *   The coordinate system in which OldY is currently defined.
 *   The possible values are: \ref CoordSys_Grid, \ref CoordSys_Frame,
 *   \ref CoordSys_Paper, or \ref CoordSys_Screen
 *
 * @param NewCoordSys
 *   The coordinate system into which to transform OldY.
 *   The possible values are: \ref CoordSys_Grid, \ref CoordSys_Frame,
 *   \ref CoordSys_Paper, or \ref CoordSys_Screen
 *
 * @param OldY
 *   The value to convert from one coordinate system to another
 *
 * @return
 *   The value OldY converted into the new coordinate system.
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilConvertYPosition(
 *   &                   OldCoordSys,
 *   &                   NewCoordSys,
 *   &                   OldY)
 *    INTEGER*4       OldCoordSys
 *    INTEGER*4       NewCoordSys
 *    REAL*8          OldY
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Determine the position on the paper of a text label. Assume the Text_ID has
 * already been obtained (See TecUtilPickListXxx functions or TecUtilTextXxx
 * functions for examples on how to obtain a Text_ID).
 *
 * @code
 *   Text_ID TID;
 *   double  XPos,YPos;
 *   double PaperXPos,PaperYPos;
 *
 *   ... Text_ID obtained....
 *
 *   TecUtilTextGetXYPos(TID,&XPos,&YPos);
 *
 *   PaperXPos = TecUtilConvertXPosition(TecUtilTextGetPositionCoordSys(TID),
 *                                       CoordSys_Paper,
 *                                       XPos);
 *   PaperYPos = TecUtilConvertYPosition(TecUtilTextGetPositionCoordSys(TID),
 *                                       CoordSys_Paper,
 *                                       YPos);
 * @endcode
 *
 * @ingroup Axis
 *
 */
LINKTOADDON double STDCALL TecUtilConvertYPosition(CoordSys_e  OldCoordSys,
                                                   CoordSys_e  NewCoordSys,
                                                   double      OldY);
/**
 * Convert the specified vertical dimension from one coordinate system to another.
 *
 * @param OldCoordSys
 *   Coordinate system in which OldDimension is measured.
 *   The possible values are: \ref CoordSys_Grid, \ref CoordSys_Frame,
 *   \ref CoordSys_Paper, or \ref CoordSys_Screen
 *
 * @param NewCoordSys
 *   Coordinate system in which OldDimension is measured.
 *   The possible values are: \ref CoordSys_Grid, \ref CoordSys_Frame,
 *   \ref CoordSys_Paper, or \ref CoordSys_Screen
 *
 * @param OldDimension
 *   Dimension to convert
 *
 * @return
 *   Converted dimension in the new coordinate system.
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilConvertYDimension(
 *   &                   OldCoordSys,
 *   &                   NewCoordSys,
 *   &                   OldDimension)
 *    INTEGER*4       OldCoordSys
 *    INTEGER*4       NewCoordSys
 *    REAL*8          OldDimension
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Find the size of the current frame in screen pixels.
 *
 * @code
 *   double X, Y, Width, Height;
 *   // get frame width and height in inches
 *   TecUtilFrameGetPosAndSize(&X,&Y, &Width, &Height);
 *   // convert width and height to screen coordinates (pixels)
 *   Width = TecUtilConvertXDimension(CoordSys_Paper,
 *                                    CoordSys_Screen, Width);
 *   Height = TecUtilConvertYDimension(CoordSys_Paper,
 *                                     CoordSys_Screen, Height);
 * @endcode
 *
 * @ingroup Axis
 *
 */
LINKTOADDON double STDCALL TecUtilConvertYDimension(CoordSys_e  OldCoordSys,
                                                    CoordSys_e  NewCoordSys,
                                                    double      OldDimension);
/**
 * Convert from one measurement system to another.
 *
 * @param OldUnits
 *   Units in which OldSize is measured.
 *   The possible values are: \ref Units_Grid, \ref Units_Frame,
 *   \ref Units_Point, \ref Units_Screen or \ref Units_AxisPercentage.
 *
 * @param NewUnits
 *   Unit space in which the return value is measured.
 *   The possible values are: \ref Units_Grid, \ref Units_Frame,
 *   \ref Units_Point, \ref Units_Screen or \ref Units_AxisPercentage.
 *
 * @param OldSize
 *   Size in the old measurement system.
 *
 * @return
 *   Converted size in the new measurement system.
 *
 * @par Note:
 *   Units_AxisPercentage are only pertinent to Polar and 3D plot types.
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilConvertUnits(
 *   &                   OldUnits,
 *   &                   NewUnits,
 *   &                   OldSize)
 *    INTEGER*4       OldUnits
 *    INTEGER*4       NewUnits
 *    REAL*8          OldSize
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * To create a line with a line thickness of three points:
 *
 * @code
 *   Geom_ID g;
 *   double frame_units;
 *   frame_units = TecUtilConvertUnits(Units_Point, Units_Frame, 3.);
 *   g = TecUtilGeom2DLineSegmentCreate(CoordSys_Frame, 5., 5., 95., 95.);
 *   TecUtilGeomSetLineThickness(g, frame_units)
 * @endcode
 *
 * @ingroup Axis
 *
 */
LINKTOADDON double STDCALL TecUtilConvertUnits(Units_e OldUnits,
                                               Units_e NewUnits,
                                               double  OldSize);


/**
 *
 * @ingroup DataLoad
 *
 * @param DatasetAuxData
 *   DatasetAuxData must be deallocated using TecUtilAuxDataDealloc.  TecUtilAuxDataDealloc,
 *   must not be used on any AuxData_pa's returns from other TecUtilAuxData functions.
 *
 *
 * @pre <em>VALID_NON_ZERO_LEN_STR(FName)</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>VALID_REF(IVersion)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(DataSetTitle)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(NumZones)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(NumVars)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>IMPLICATION(VarNames != NULL,VALID_REF(VarNames))</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>IMPLICATION(ZoneNames != NULL,VALID_REF(ZoneNames))</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>IMPLICATION(NumPtsI != NULL,VALID_REF(NumPtsI))</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>IMPLICATION(NumPtsJ != NULL,VALID_REF(NumPtsJ))</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>IMPLICATION(NumPtsK != NULL,VALID_REF(NumPtsK))</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>IMPLICATION(ZoneType != NULL,VALID_REF(ZoneType))</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>IMPLICATION(UserRec != NULL,VALID_REF(UserRec))</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>DatasetAuxData</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * #internalattributes exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilReadBinaryData(Boolean_t      GetHeaderInfoOnly, /* <-activex> */
                                                    const char*    FName,
                                                    short*         IVersion,
                                                    char**         DataSetTitle,
                                                    EntIndex_t*    NumZones,
                                                    EntIndex_t*    NumVars,
                                                    StringList_pa* VarNames,
                                                    StringList_pa* ZoneNames,
                                                    LgIndex_t**    NumPtsI,
                                                    LgIndex_t**    NumPtsJ,
                                                    LgIndex_t**    NumPtsK,
                                                    ZoneType_e**   ZoneType,
                                                    StringList_pa* UserRec,
                                                    AuxData_pa*    DatasetAuxData,
                                                    Boolean_t      RawDataspaceAllocated,
                                                    NodeMap_t***   NodeMap,
                                                    double***      VDataBase);

/**
 *   Get the clipping properties of a text object.
 *
 * @param TID
 *   ID of the text object
 *
 * @return
 *   Returns the clipping type. Can be one of Clipping_ClipToViewport or Clipping_ClipToFrame.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetClipping(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON Clipping_e    STDCALL TecUtilTextGetClipping(Text_ID TID);

/**
 * Get the anchor coordinate position of the text object in the current
 * coordinate system.
 *
 * @param TID
 *   Handle to a text object
 *
 * @param XOrThetaPos
 *   Pointer to the text object's X or Theta anchor position.
 *
 * @param YOrRPos
 *   Pointer to the text object's Y or R (radius) anchor position
 *
 * @param ZPos
 *   Pointer to the text object's anchor Z position.
 *
 *
 * @pre <em>XOrThetaPos</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>YOrRPos</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>ZPos</em>
 *   Pointer must be a valid address or NULL.
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextGetAnchorPos(
 *   &           TIDPtr,
 *   &           XOrThetaPos,
 *   &           YOrRPos,
 *   &           ZPos)
 *    POINTER         (TIDPtr, TID)
 *    REAL*8          XOrThetaPos
 *    REAL*8          YOrRPos
 *    REAL*8          ZPos
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Text_ID Text;
 *
 * @code
 *   double  XOrThetaPos;
 *   double  YOrRPos;
 *   double  ZPos;
 *
 *   // use TecUtilTextDelete() when 'Text' is no longer needed
 *   Text = TecUtilTextCreate(CoordSys_Frame, 50.0, 50.0,
 *                            Units_Point,30.0, "Hello");
 *   if (Text != TECUTILBADID)
 *     {
 *       // do all sorts of things
 *         .
 *         .
 *         .
 *
 *       // get the current anchor position of the text
 *       TecUtilTextGetAnchorPos(Text, &XOrThetaPos, &YOrRPos,
 *    &ZPos);
 *     }
 * @endcode
 *
 * @sa TecUtilTextSetCoordSysAndUnits()
 *
 * @ingroup Text
 *
 */
LINKTOADDON void STDCALL TecUtilTextGetAnchorPos(Text_ID        TID,
                                                 TP_OUT double* XOrThetaPos,
                                                 TP_OUT double* YOrRPos,
                                                 TP_OUT double* ZPos);

/**
 * @deprecated
 *   Please use TecUtilTextGetAnchorPos() instead.
 *
 * @ingroup Text
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilTextGetXYPos(Text_ID        TID,
                                             TP_OUT double* XPos,
                                             TP_OUT double* YPos);
/**
 *   Get the coordinate system to which the text is associated.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @return
 *   Text object's coordinate system. The possible values are: \ref CoordSys_Grid, \ref CoordSys_Frame.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetPositionCoordSys(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON CoordSys_e    STDCALL TecUtilTextGetPositionCoordSys(Text_ID TID);
/**
 * Get the zone or map with which the text object is associated (if it is
 * attached).
 *
 * @param TID
 *   Handle to a text object.
 *
 * @return
 *   Zone or map.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetZoneOrMap(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilTextIsAttached() and TecUtilTextSetAttached().
 *
 * @ingroup Text
 *
 */
LINKTOADDON EntIndex_t    STDCALL TecUtilTextGetZoneOrMap(Text_ID TID);
/**
 * Determine if the text object is attached to a zone or map.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @return
 *   TRUE if attached, otherwise FALSE.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextIsAttached(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilTextGetZoneOrMap() and TecUtilGeomSetZoneOrMap().
 *
 * @ingroup Text
 *
 */
LINKTOADDON Boolean_t     STDCALL TecUtilTextIsAttached(Text_ID TID);
/**
 *   Get the color of the text object.
 *
 * @param TID
 *   Handle to a text object
 *
 * @return
 *   Text color. The possible values are: \ref Black_C, \ref Blue_C,
 *   \ref Red_C, \ref Green_C, \ref Cyan_C, \ref Purple_C, \ref Yellow_C,
 *   \ref White_C, CustomXX_C where XX is in the range 1-64.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetColor(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON ColorIndex_t  STDCALL TecUtilTextGetColor(Text_ID TID);
/**
 *   Get the font used for the text object.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @return
 *   Text font. The possible values are: Font_Helvetica, Font_HelveticaBold, Font_Greek, Font_Math,
 *   Font_UserDefined, Font_Times, Font_TimesItalic, Font_TimesItalicBold, Font_TimesBold,
 *   Font_Courier, Font_CourierBold.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetFont(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilTextSetTypeface(), TecUtilTextGetTypefaceFamily(),
 *     TecUtilTextGetTypefaceIsBold(), TecUtilTextGetTypefaceIsItalic()
 *
 * @ingroup Text
 *
 */
LINKTOADDON Font_e        STDCALL TecUtilTextGetFont(Text_ID TID);

/**
 * Gets the typeface font family used for the text object.
 *
 * @par Note:
 *     The caller is responsible for de-allocating the typeface font family with
 *     TecUtilStringDealloc() when no longer needed.
 *
 * @param TID
 *     Handle to a text object.
 *
 * @return
 *     An allocated string containing the typeface font family used for the text object.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextGetTypefaceFamily(
 *   &           TIDPtr,
 *   &           Result,
 *   &           ResultLength)
 *    POINTER(TIDPtr, TID)
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilTextSetTypeface(), TecUtilTextGetTypefaceIsBold(),
 *     TecUtilTextGetTypefaceIsItalic()
 *
 * @ingroup Text
 *
 * @since 12.2.1.96181
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilTextGetTypefaceFamily(Text_ID TID);

/**
 * Indicates if the text object's typeface has bold font style.
 *
 * @param TID
 *     Handle to a text object.
 *
 * @return
 *     TRUE if the text object's typeface has bold font style, FALSE otherwise.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetTypefaceIsBold(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilTextSetTypeface(), TecUtilTextGetTypefaceFamily(),
 *     TecUtilTextGetTypefaceIsItalic()
 *
 * @ingroup Text
 *
 * @since 12.2.1.96181
 */
LINKTOADDON Boolean_t STDCALL TecUtilTextGetTypefaceIsBold(Text_ID TID);

/**
 * Indicates if the text object's typeface has italic font style.
 *
 * @param TID
 *     Handle to a text object.
 *
 * @return
 *     TRUE if the text object's typeface has italic font style, FALSE otherwise.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetTypefaceIsItalic(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilTextSetTypeface(), TecUtilTextGetTypefaceFamily(),
 *     TecUtilTextGetTypefaceIsBold()
 *
 * @ingroup Text
 *
 * @since 12.2.1.96181
 */
LINKTOADDON Boolean_t STDCALL TecUtilTextGetTypefaceIsItalic(Text_ID TID);

/**
 *   Get the text height in the currently defined text size units.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @return
 *   Text height measured in the currently defined text size units.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilTextGetHeight(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON double        STDCALL TecUtilTextGetHeight(Text_ID TID);
/**
 * Get the size units for the text object.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @return
 *   Text size units. The possible values are: Units_Grid, Units_Frame or Units_Point.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetSizeUnits(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilTextGetHeight()
 *
 * @ingroup Text
 *
 */
LINKTOADDON Units_e       STDCALL TecUtilTextGetSizeUnits(Text_ID TID);
/**
 * Get the type of the box surrounding the text object.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @return
 *   The possible values are: TextBox_None, TextBox_Filled, TextBox_Hollow.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextBoxGetType(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON TextBox_e     STDCALL TecUtilTextBoxGetType(Text_ID TID);
/**
 *   Get the margin between the text and the box surrounding the text object.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @return
 *   The text box margin in frame units.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilTextBoxGetMargin(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON double        STDCALL TecUtilTextBoxGetMargin(Text_ID TID);
/**
 *   Get the line thickness of the text box border.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @return
 *   The text box line thickness in frame units.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilTextBoxGetLineThickness(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON double        STDCALL TecUtilTextBoxGetLineThickness(Text_ID TID);
/**
 *   Get the line color of the box surrounding the text object.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @return
 *   The text box color.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextBoxGetColor(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON ColorIndex_t  STDCALL TecUtilTextBoxGetColor(Text_ID TID);
/**
 *   Get the fill color of the box surrounding the text object.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @return
 *   The text box fill color.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextBoxGetFillColor(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON ColorIndex_t  STDCALL TecUtilTextBoxGetFillColor(Text_ID TID);
/**
 *   Get the text angle.
 *
 * @param TID
 *   Handle to a text object
 *
 * @return
 *   The text angle in degrees.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilTextGetAngle(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON double        STDCALL TecUtilTextGetAngle(Text_ID TID);
/**
 *   Get the text anchor style.
 *
 * @param TID
 *   Handle to a text object
 *
 * @return
 *   The text anchor style. The possible values are: TextAnchor_Left, TextAnchor_Center,
 *   TextAnchor_Right, TextAnchor_MidLeft, TextAnchor_MidCenter, TextAnchor_MidRight,
 *   TextAnchor_HeadLeft, TextAnchor_HeadCenter, TextAnchor_HeadRight, TextAnchor_OnSide.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetAnchor(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON TextAnchor_e  STDCALL TecUtilTextGetAnchor(Text_ID TID);
/**
 * Get the spacing between lines of text.
 *
 * @par Note:
 *   A newline (\\n) character within a string causes the line to wrap at that
 *   point.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @return
 *   Vertical line spacing between multiple lines of a ext object.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilTextGetLineSpacing(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON double        STDCALL TecUtilTextGetLineSpacing(Text_ID TID);
/**
 *   Get the scope of the text object. Text with local scope is displayed only in the frame in which it
 *   is created. If the text is defined as having global scope it will appear in all "like"
 *   frames, that is, those frames using the same data set as the one in which the text was created.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @return
 *   Text scope. The possible values are: Scope_Local or Scope_Global.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetScope(TIDPtr)
 *    POINTER(TIDPtr, TID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON Scope_e       STDCALL TecUtilTextGetScope(Text_ID TID);



/**
 * Get the macro function command string associated with the text object.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @param MacroFunctionCommand
 *   Handle to a macro function command string. The result must be deallocated
 *   with TecUtilStringDealloc() when it is no longer needed
 *
 * @return
 *   TRUE if sufficient memory is available for the string, otherwise FALSE.
 *
 *
 * @pre <em>MacroFunctionCommand</em>
 *   Pointer must be a valid address and non-NULL.
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetMacroFunctionCmd(
 *   &                   TIDPtr,
 *   &                   MacroFunctionCommand,
 *   &                   MacroFunctionCommandLength)
 *    POINTER         (TIDPtr, TID)
 *    CHARACTER*(*)   MacroFunctionCommand
 *    INTEGER*4       MacroFunctionCommandLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Boolean_t IsOk;
 *
 * @code
 *   Text_ID   Text;
 *   char      *MacroCommand = NULL;
 *     .
 *     .
 *     .
 *
 *   // get the text's associated macro command
 *   IsOk = TecUtilTextGetMacroFunctionCmd(Text, &MacroCommand);
 *   if (IsOk)
 *     {
 *       // do something with command
 *         .
 *         .
 *         .
 *
 *       // cleanup; macro command no longer needed
 *       TecUtilStringDealloc(&MacroCommand);
 *     }
 * @endcode
 *
 * @ingroup Text
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilTextGetMacroFunctionCmd(Text_ID         TID,
                                                             TP_GIVES char** MacroFunctionCommand);
/**
 * Get the string associated with the text object.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @param TextString
 *   String of the text object. Result must be deallocated with
 *   TecUtilStringDealloc() when no longer needed.
 *
 * @return
 *   TRUE if sufficient memory is avialable for the string, otherwise FALSE.
 *
 *
 * @pre <em>TextString</em>
 *   Pointer must be a valid address and non-NULL.
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetString(
 *   &                   TIDPtr,
 *   &                   TextString,
 *   &                   TextStringLength)
 *    POINTER         (TIDPtr, TID)
 *    CHARACTER*(*)   TextString
 *    INTEGER*4       TextStringLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Text_ID Text;
 *
 * @code
 *   Boolean_t IsOk = FALSE;
 *   char      *TextString = NULL;
 *
 *   Text = TecUtilTextCreate(CoordSys_Frame,50.0, 50.0, Units_Point,
 *                            30.0, "Hello");
 *   if (Text != TECUTILBADID)
 *     {
 *       // do some things with the text
 *         .
 *         .
 *         .
 *
 *       // change the string of the text object
 *       TecUtilTextSetString(Text, "Hello World");
 *
 *       // do some more things
 *         .
 *         .
 *         .
 *
 *       // print the contents of the text
 *       // string to standard output
 *       IsOk = TecUtilTextGetString(Text, &TextString);
 *       if (IsOk)
 *         {
 *           printf("%s\n", TextString);
 *           TecUtilStringDealloc(&TextString);
 *         }
 *     }
 * @endcode
 *
 * @ingroup Text
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilTextGetString(Text_ID         TID,
                                                   TP_GIVES char** TextString);

/**
 * Get the first text object from the list of text objects maintained by the
 * current frame.
 *
 * @sa TecUtilTextGetNext() and TecUtilTextGetPrev()
 *
 * @return
 *   First text object of the list maintained by the current frame. If no text
 *   objects are maintained by the current frame then \ref TECUTILBADID is
 *   returned.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextGetBase(ResultPtr)
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Examine all text labels in the current frame and see if any of them are
 *   red.
 *
 * @code
 *   TextID Text;
 *   for (Text = TecUtilTextGetBase();
 *        Text != TECUTILBADID;
 *        Text = TecUtilTextGetNext(Text))
 *     {
 *       if (TecUtilTextGetColor(Text) == Red_C)
 *         {
 *           // do something with the red text
 *         }
 *     }
 * @endcode
 *
 * @ingroup Text
 *
 */
LINKTOADDON Text_ID STDCALL TecUtilTextGetBase(void);

/**
 * Get the next text object, relative to the specified text object, from the
 * list of text objects maintained by the current frame.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @return
 *   Text object following the specified text object. If the specified text
 *   object is the last in the list then \ref TECUTILBADID is returned.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *   SUBROUTINE TecUtilTextGetNext(
 *  &           TIDPtr,
 *  &           ResultPtr)
 *   POINTER        (TIDPtr, TID)
 *   POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON Text_ID       STDCALL TecUtilTextGetNext(Text_ID TID);

/**
 * Get the previous text object, relative to the specified text object, from the list of text objects
 * maintained by the current frame.
 *
 * @param TID
 *   Handle to a text object defined in the current frame.
 *
 * @return
 *   Text object preceeding the specified text object. If the specified text object is the first in the
 *   list then the last text object in the list is returned.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *   SUBROUTINE TecUtilTextGetPrev(
 *  &           TIDPtr,
 *  &           ResultPtr)
 *   POINTER        (TIDPtr, TID)
 *   POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilTextGetBase() and TecUtilTextGetNext().
 *
 * @ingroup Text
 *
 */
LINKTOADDON Text_ID       STDCALL TecUtilTextGetPrev(Text_ID TID);

/**
 */
/**
 *   Function will get the clipping properties of a geometry.
 *
 * @param GID
 *   ID of a geometry.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetClipping(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a red circle and set the clipping to "ClipToFrame":
 *
 * @code
 *   Geom_ID g;
 *   Clipping_e clip;
 *   g = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   clip = TecUtilGeomGetClipping(g);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Clipping_e    STDCALL TecUtilGeomGetClipping(Geom_ID GID);
/**
 */
/**
 *   Gets the anchor postion of the specified geometry.
 *
 * @param GID
 *   Geometry ID. The must be an ellipse geometry
 *
 * @param XOrThetaPos
 *   The X or Theta axis position of the geometry anchor
 *
 * @param YOrRPos
 *   The Y or Radian axis position of the geometry anchor
 *
 * @param ZPos
 *   The Z axis position of the geometry anchor
 *
 *
 * @pre <em>XOrThetaPos</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>YOrRPos</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>ZPos</em>
 *   Pointer must be a valid address and non-NULL.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomGetAnchorPos(
 *   &           GIDPtr,
 *   &           XOrThetaPos,
 *   &           YOrRPos,
 *   &           ZPos)
 *    POINTER         (GIDPtr, GID)
 *    REAL*8          XOrThetaPos
 *    REAL*8          YOrRPos
 *    REAL*8          ZPos
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the achor position of a newly created circle:
 *
 * @code
 *
 *   double XPos, YPos, ZPos;
 *
 *   Geom_ID Geom;
 *   Geom = TecUtilGeomCircleCreate(CoordSys_Grid,
 *     4.0, 3.0, 5.0);
 *
 *   TecUtilGeomGetAnchorPos(Geom, &XPos, &YPos, &ZPos);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeomGetAnchorPos(Geom_ID        GID,
                                                 TP_OUT double* XOrThetaPos,
                                                 TP_OUT double* YOrRPos,
                                                 TP_OUT double* ZPos);

/**
 *   Sets the state of the "preserve aspect ratio" toggle for an image geometry.
 *
 * @param GID
 *   Geometry ID. Must be an image geometry
 *
 * @param MaintainAspectRatio
 *   TRUE to preserve the aspect ratio when drawing an image geometry
 *
 * @return
 *   TRUE if the "preserve aspect ratio" toggle is set, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomImageSetUseRatio(
 *   &           GIDPtr,
 *   &           MaintainAspectRatio)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       MaintainAspectRatio
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the state of the preserve aspect ration toggle in an image to TRUE:
 *
 * @code
 *   TecUtilGeomImageSetUseRatio(GID, TRUE);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeomImageSetUseRatio(Geom_ID   GID,
                                                     Boolean_t MaintainAspectRatio);


/**
 * @deprecated
 *   Please use TecUtilGeomGetAnchorPos() instead.
 *
 * @ingroup Geom
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilGeomGetXYZAnchorPos(Geom_ID        GID,
                                                    TP_OUT double* XPos,
                                                    TP_OUT double* YPos,
                                                    TP_OUT double* ZPos);
/**
 * Get the zone or Line-mapping to which the geometry is attached. Use
 * TecUtilGeomIsAttached() to determine whether or not the geometry is attached
 * at all.
 *
 * @param GID
 *   ID of a geometry.
 *
 * @return
 *   The zone number or the Line-mapping number.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetZoneOrMap(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * See TecUtilGeomIsAttached() for an example of using
 * TecUtilGeomGetZoneOrMap().
 *
 * @ingroup Geom
 *
 */
LINKTOADDON EntIndex_t    STDCALL TecUtilGeomGetZoneOrMap(Geom_ID GID);




/**
 * Determine whether or not a geometry is attached to a zone or Line-mapping.
 * Use TecUtilGeomGetZoneOrMap() to get the zone or Line-mapping number to
 * which the geometry is attached.
 *
 * @param GID
 *   Geometry ID. Must be an image geometry
 *
 * @return
 *   TRUE if the geometry is attached to a zone or Line-mapping, otherwise FALSE.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomIsAttached(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Determine the zone or Line-mapping that a geometry is attached to:
 *
 * @code
 *   extern Geom_ID g; // created elsewhere
 *   EntIndex_t zone = 0;
 *   EntIndex_t LineMap = 0;
 *   if ( TecUtilFrameGetMode() == Frame_XY )
 *      LineMap = TecUtilGeomGetZoneOrMap(g);
 *   else if ( TecUtilFrameGetMode() != Frame_Sketch )
 *      zone = TecUtilGeomGetZoneOrMap(g);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Boolean_t     STDCALL TecUtilGeomIsAttached(Geom_ID GID);
/**
 *   Get the geometry line color.
 *
 * @param GID
 *   ID of a geometry.
 *
 * @return
 *   The line color of the geometry. The possible values are: Black_C, Blue_C, Red_C, Green_C, Cyan_C,
 *   Purple_C, Yellow_C, White_C, or CustomXX_C where XX ranges from 1 to 64.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetColor(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON ColorIndex_t  STDCALL TecUtilGeomGetColor(Geom_ID GID);
/**
 * Get the geometry fill color. Use TecUtilGeomGetIsFilled() to determine
 * whether or not the geometry is filled with a color.
 *
 * @param GID
 *   ID of a geometry.
 *
 * @return
 *   The geometry fill color. The possible values are: Black_C, Blue_C, Red_C,
 *   Green_C, Cyan_C, Purple_C, Yellow_C, White_C, or CustomXX_C where XX
 *   ranges from 1 to 64.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetFillColor(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON ColorIndex_t  STDCALL TecUtilGeomGetFillColor(Geom_ID GID);
/**
 *   Determine if a geometry if filled.
 *
 * @param GID
 *   ID of a geometry.
 *
 * @return
 *   TRUE if the geometry is filled, otherwise FALSE.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetIsFilled(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Boolean_t     STDCALL TecUtilGeomGetIsFilled(Geom_ID GID);
/**
 *   Get the geometry type.
 *
 * @param GID
 *   ID of a geometry.
 *
 * @return
 *   The geometry type. This can be one of: GeomType_LineSegs (includes 2-D and 3-D line, polyline and
 *   multi-polyline geometries), GeomType_Rectangle, GeomType_Square, GeomType_Circle, GeomType_Ellipse.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetType(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON GeomForm_e    STDCALL TecUtilGeomGetType(Geom_ID GID);
/**
 *   Get the line pattern of a geometry.
 *
 * @param GID
 *   ID of a geometry. This must be a circle or ellipse geometry
 *
 * @return
 *   The geometry line pattern. The possible values are: LinePattern_Solid,
 *   LinePattern_Dashed, LinePattern_DashDot, LinePattern_Dotted,
 *   LinePattern_LongDash, LinePattern_DashDotDot.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetLinePattern(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON LinePattern_e STDCALL TecUtilGeomGetLinePattern(Geom_ID GID);
/**
 *   Get the geometry line pattern length.
 *
 * @param GID
 *   ID of a geometry.
 *
 * @return
 *   The line pattern length in frame units.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilGeomGetPatternLength(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON double        STDCALL TecUtilGeomGetPatternLength(Geom_ID GID);
/**
 *   Get the geometry line thickness.
 *
 * @param GID
 *   ID of a geometry.
 *
 * @return
 *   The geometry line thickness in frame units.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilGeomGetLineThickness(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON double        STDCALL TecUtilGeomGetLineThickness(Geom_ID GID);
/**
 *   Get the number of points used to draw a circle or ellipse geometry.
 *
 * @param GID
 *   ID of a geometry. This must be a circle or ellipse geometry
 *
 * @return
 *   The number of points used to draw the circle or geometry.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomEllipseGetNumPoints(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON SmInteger_t   STDCALL TecUtilGeomEllipseGetNumPoints(Geom_ID GID);
/**
 *   Get the geometry arrowhead style.
 *
 * @param GID
 *   ID of the geometry. This must be a polyline or a multi-polyline geometry
 *
 * @return
 *   The arrowhead style. The possible values are: ArrowheadStyle_Plain, ArrowheadStyle_Filled or
 *   ArrowheadStyle_Hollow.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomArrowheadGetStyle(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON ArrowheadStyle_e        STDCALL TecUtilGeomArrowheadGetStyle(Geom_ID GID);
/**
 *   Get the geometry arrowhead attachment.
 *
 * @param GID
 *   ID of the geometry. This must be a polyline or a multi-polyline geometry
 *
 * @return
 *   The arrowhead attachment. The possible values are: ArrowheadAttachment_None,
 *   ArrowheadAttachment_AtBeginning, ArrowheadAttachment_AtEnd,
 *   ArrowheadAttachment_AtBothEnds.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomArrowheadGetAttach(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON ArrowheadAttachment_e   STDCALL TecUtilGeomArrowheadGetAttach(Geom_ID GID);
/**
 *   Get the geometry arrowhead size.
 *
 * @param GID
 *   ID of the geometry. This must be a polyline or a multi-polyline geometry
 *
 * @return
 *   The arrowhead size in frame units.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilGeomArrowheadGetSize(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON double        STDCALL TecUtilGeomArrowheadGetSize(Geom_ID GID);
/**
 *   Get the geometry arrowhead angle.
 *
 * @param GID
 *   ID of the geometry. This must be a polyline or a multi-polyline geometry
 *
 * @return
 *   The arrowhead angle in degrees.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilGeomArrowheadGetAngle(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON double        STDCALL TecUtilGeomArrowheadGetAngle(Geom_ID GID);
/**
 *   Get the geometry scope.
 *
 * @param GID
 *   ID of a geometry.
 *
 * @return
 *   The geometry scope. The possible values are: Scope_Local (show in current
 *   frame only), Scope_Global (show in all frames with the same data set as
 *   the current frame).
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetScope(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Scope_e       STDCALL TecUtilGeomGetScope(Geom_ID GID);
/**
 *   Get the geometry position coordinate system.
 *
 * @param GID
 *   ID of a geometry.
 *
 * @return
 *   The coordinate system. The possible values are: CoordSys_Grid3D or CoordSys_Frame.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetPositionCoordSys(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON CoordSys_e    STDCALL TecUtilGeomGetPositionCoordSys(Geom_ID GID);
/**
 *   Get the resize filter of an image geometry. The resize filter determines
 *   the algorithm used when an image is resized.
 *
 * @param GID
 *   Geometry ID. Must be an image geometry
 *
 * @return
 *   The resize filter of the indicated geometry. See ImageResizeFilter_e.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomImageGetResizeFilter(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the resize filter of a geometry.
 *
 * @code
 *   ImageResizeFilter_e Filter =
 *                       TecUtilGeomImageSetResizeFilter(G);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON ImageResizeFilter_e STDCALL TecUtilGeomImageGetResizeFilter(Geom_ID GID);
/**
 *   Sets the resize filter of an image geometry. The resize filter determines
 *   the algorithm used when an image is resized.
 *
 * @param GID
 *   Geometry ID. Must be an image geometry
 *
 * @param ResizeFilter
 *   Resize filter. See ImageResizeFilter_e.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomImageSetResizeFilter(
 *   &           GIDPtr,
 *   &           ResizeFilter)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       ResizeFilter
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the resize filter for an image geometry to Box:
 *
 * @code
 *   TecUtilGeomImageSetResizeFilter(G, ImageResizeFilter_Box);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeomImageSetResizeFilter(Geom_ID             GID,
                                                         ImageResizeFilter_e ResizeFilter);
/**
 * Get the name of the file associated with an image geometry.
 *
 * @param GID
 *   Geometry ID. Must be an image geometry
 *
 * @param FileName
 *   Receives the file name associated with this geometry. This is always an
 *   absolute path. You must call TecUtilStringDealloc() to free this string
 *   when you are done with it
 *
 *
 * @pre <em>FileName</em>
 *   Pointer must be a valid address and non-NULL.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomImageGetFileName(
 *   &           GIDPtr,
 *   &           FileName,
 *   &           FileNameLength)
 *    POINTER         (GIDPtr, GID)
 *    CHARACTER*(*)   FileName
 *    INTEGER*4       FileNameLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the name of the file associated with a geometry.
 *
 * @code
 *   char *GeomFileName = NULL;
 *   GTecUtilGeomImageGetFileName(G, &GeomFileName);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeomImageGetFileName(Geom_ID         GID,
                                                     TP_GIVES char** FileName);

/**
 *   Sets the width of an image geometry. Note that the size of a geometry when it is displayed in a
 *   frame is not necessarily the size of the image in the file. The image will be resized to fit the
 *   dimensions specified when calling this function.
 *
 * @param GID
 *   Geometry ID. Must be an image geometry
 *
 * @param Width
 *   New width of the geometry. Must be greater than 0.0
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomImageSetWidth(
 *   &           GIDPtr,
 *   &           Width)
 *    POINTER         (GIDPtr, GID)
 *    REAL*8          Width
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the width of an image geometry to 5.0:
 *
 * @code
 *   TecUtilGeomImageSetWidth(GID, 5.0);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeomImageSetWidth(Geom_ID GID,
                                                  double  Width);

/**
 *   Sets the Height of an image geometry. Note that the size of a geometry when it is displayed in a
 *   frame is not necessarily the size of the image in the file. The image will be resized to fit the
 *   dimension specified when calling this function.
 *
 * @param GID
 *   Geometry ID. Must be an image geometryGeometry ID. Must be an image geometry
 *
 * @param Height
 *   New height of the image, must be greater than 0.0
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomImageSetHeight(
 *   &           GIDPtr,
 *   &           Height)
 *    POINTER         (GIDPtr, GID)
 *    REAL*8          Height
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the height of an image geometry to 5.0:
 *
 * @code
 *   TecUtilGeomImageSetHeight(GID, 5.0);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeomImageSetHeight(Geom_ID GID,
                                                   double  Height);

/**
 *   Get the width and height of an image geometry.
 *
 * @param GID
 *   Geometry ID. Must be an image geometry
 *
 * @param Width
 *   Receives the width of the specified image geometry.
 *
 * @param Height
 *   Receives the height of the specified image geometry.
 *
 *
 * @pre <em>Width</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>Height</em>
 *   Pointer must be a valid address or NULL.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomImageGetSize(
 *   &           GIDPtr,
 *   &           Width,
 *   &           Height)
 *    POINTER         (GIDPtr, GID)
 *    REAL*8          Width
 *    REAL*8          Height
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the width and height of an image in an image geometry.
 *
 * @code
 *   double Width;
 *   double Height;
 *   TecUtilGeomImageGetFileName(GID, &Width, &Height);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeomImageGetSize(Geom_ID        GID,
                                                 TP_OUT double* Width,
                                                 TP_OUT double* Height);

/**
 *   Resets the aspect ratio after any changes have been made in the position
 *   of an image geometry.
 *
 * @param GID
 *   Geometry ID. Must be an image geometry
 *
 * @pre Must have one or more frames.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomImageResetAspectRati(GIDPtr)
 *    POINTER(GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @par Note:
 *   IMPORTANT!  Note that the FORTRAN Name for this function is truncated
 *   to 31 characters!
 *
 *   Reset the aspect ratio of an image geometry:
 *
 * @code
 *
 *   TecUtilGeomImageResetAspectRatio(GID);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeomImageResetAspectRatio(Geom_ID GID);



/**
 *
 *
 * @pre <em>IMPLICATION(Width != NULL,VALID_REF(Width))</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>IMPLICATION(Height != NULL,VALID_REF(Height))</em>
 *   Pointer must be a valid address or NULL.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * @ingroup Geom
 *
 * #internalattributes exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilGeomImageGetImage(Geom_ID                 GID,
                                                       TP_OUT ScreenDim_t*     Width,
                                                       TP_OUT ScreenDim_t*     Height,
                                                       TP_ARRAY_GIVES Byte_t** RGBData);


/**
 * Get the geometry macro function command.
 *
 * @param GID
 *   ID of a geometry.
 *
 * @param MacroFunctionCmd
 *   Character string containing the macro command. You must free this string
 *   using TecUtilStringDealloc() when you are done with it
 *
 * @return
 *   Returns TRUE if space can be allocated for the command string, otherwise
 *   FALSE.
 *
 *
 * @pre <em>MacroFunctionCmd</em>
 *   Pointer must be a valid address and non-NULL.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetMacroFunctionCmd(
 *   &                   GIDPtr,
 *   &                   MacroFunctionCmd,
 *   &                   MacroFunctionCmdLength)
 *    POINTER         (GIDPtr, GID)
 *    CHARACTER*(*)   MacroFunctionCmd
 *    INTEGER*4       MacroFunctionCmdLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilGeomGetMacroFunctionCmd(Geom_ID         GID,
                                                             TP_GIVES char** MacroFunctionCmd);

/**
 * Create an image geometry. Use the ID obtained from this function to set
 * geometry attributes such as position and coordinates system.
 *
 * @param FName
 *   Image file to attach. The format of this file must be Microsoft Windows
 *   Bitmap (*.bmp), JPEG (*.jpg or *.jpeg) or Portable Network Graphics
 *   (*.png)
 *
 * @param CornerX
 *   X coordinate for the location to initially place the image (frame
 *   coordinates).
 *
 * @param CornerY
 *   Y coordinate for the location to initially place the image (frame
 *   coordinates).
 *
 * @param Size
 *   The default size of the image. You may change the size later using
 *   TecUtilImageSetWidth() and TecUtilImageSetHeight().
 *
 * @return
 *   If successfully created, then the return is a valid ID that you may use to
 *   further set attributes for this geometry. Otherwise, \ref TECUTILBADID is
 *   returned. If the return value is \ref TECUTILBADID, then the most likely
 *   cause is the file does not exist.
 *
 *
 * @pre <em>FName</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomImageCreate(
 *   &           FName,
 *   &           CornerX,
 *   &           CornerY,
 *   &           Size,
 *   &           ResultPtr)
 *    CHARACTER*(*)  FName
 *    REAL*8         CornerX
 *    REAL*8         CornerY
 *    REAL*8         Size
 *    POINTER        (ResultPtr, Result)
 *
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create an image geometry anchored at (0.1,0.1) with a size of 0.5, using the file "myimage.png":
 *
 * @code
 *   Geom_ID G;
 *   G = TecUtilGeomImageCreate("c:\\myimage.png",0.1,0.1,0.5);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeomImageCreate(const char *FName,
                                                   double      CornerX,
                                                   double      CornerY,
                                                   double      Size);

/**
 * Get the base geometry attached to the current frame.
 *
 * @return
 *   Returns \ref TECUTILBADID if the current frame does not have any attached
 *   geometries, otherwise, a valid geometry ID. Use TecUtilGeomGetNext() to
 *   get successive geometries.
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomGetBase(ResultPtr)
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeomGetBase(void);

/**
 * Get the next geometry in the list of geometries attached to the current
 * frame.
 *
 * @param GID
 *   ID of a geometry.
 *
 * @return
 *   Returns the ID of the next geometry or \ref TECUTILBADID if there are no
 *   more geometries.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomGetNext(
 *   &           GIDPtr,
 *   &           ResultPtr)
 *    POINTER        (GIDPtr, GID)
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Change all geometries in the current frame to be red:
 *
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeomGetBase();
 *   while ( g != TECUTILBADID )
 *     {
 *       TecUtilGeomSetColor(g, Red_C);
 *       g = TecUtilGeomGetNext(g);
 *     }
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Geom_ID       STDCALL TecUtilGeomGetNext(Geom_ID GID);

/**
 * Get the previous geometry in the list of geometries attached to the current
 * frame.
 *
 * @param GID
 *   ID of a geometry.
 *
 * @return
 *   Returns the ID of the previous geometry or \ref TECUTILBADID if GID was
 *   the base geometry.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomGetPrev(
 *   &           GIDPtr,
 *   &           ResultPtr)
 *    POINTER        (GIDPtr, GID)
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a circle with color of the previous circle:
 *
 * @code
 *   Geom_ID new_geom, prev_geom;
 *   new_geom = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   prev_geom = TecUtilGeomGetPrev(new_geom);
 *   while ( prev_geom != TECUTILBADID &&
 *           TecUtilGeomGetType(prev_geom) != Geom_Circle )
 *     prev_geom = TecUtilGeomGetPrev(prev_geom);
 *   if ( prev_geom != TECUTILBADID )
 *     TecUtilGeomSetColor(new_geom,TecUtilGeomGetColor(prev_geom));
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Geom_ID       STDCALL TecUtilGeomGetPrev(Geom_ID GID);

/**
 */
/**
 *   Set the clipping properties of a text object.
 *
 * @param TID
 *   ID of the text object
 *
 * @param Clipping
 *   New clipping property for the text object. The possible values are: Clipping_ClipToViewport and
 *   Clipping_ClipToFrame.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetClipping(
 *   &           TIDPtr,
 *   &           Clipping)
 *    POINTER        (TIDPtr, TID)
 *    INTEGER*4       Clipping
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a texts string "Test Text Object" and set the clipping to "ClipToFrame":
 *
 * @code
 *   Text_ID TID;
 *   TID = TecUtilTextCreate(CoordSys_Frame,20,30,
 *                           Units_Point,
 *                           15,"Test Text Object");
 *   TecUtilTextSetClipping(t, Clipping_ClipToFrame);
 * @endcode
 *
 * @ingroup Text
 *
 */
LINKTOADDON void       STDCALL TecUtilTextSetClipping(Text_ID    TID,
                                                      Clipping_e Clipping);
/**
 */
/**
 * Set the XY-position for the text object. The text is always anchored in the
 * coordinate system specified by TecUtilTextSetCoordSysAndUnits() each time
 * the text object is drawn.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @param XOrThetaPos
 *   The text object's X or Theta anchor position.
 *
 * @param YOrRPos
 *   The text object's Y or R (radius) anchor position.
 *
 * @param ZPos
 *   The text object's Z position.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetAnchorPos(
 *   &           TIDPtr,
 *   &           XOrThetaPos,
 *   &           YOrRPos,
 *   &           ZPos)
 *    POINTER         (TIDPtr, TID)
 *    REAL*8          XOrThetaPos
 *    REAL*8          YOrRPos
 *    REAL*8          ZPos
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON void       STDCALL TecUtilTextSetAnchorPos(Text_ID TID,
                                                       double  XOrThetaPos,
                                                       double  YOrRPos,
                                                       double  ZPos);
/**
 * @deprecated
 *   Please use TecUtilTextSetAnchorPos() instead.
 *
 * @ingroup Text
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON void       STDCALL TecUtilTextSetXYPos(Text_ID TID,
                                                   double XPos,
                                                   double YPos);
/**
 * Set the coordinate system for the position and the units for the character height of a text
 * object. The text object's position and text height are adjusted so that it remains identical to
 * its visual appearance in the original coordinate and unit system.
 *
 * @param TID
 *   Handle to the text object.
 *
 * @param PositionCoordSys
 *   Coordinate system in which the text is positioned. The possible values
 *   are: \ref CoordSys_Frame or \ref CoordSys_Grid.
 *
 * @param HeightUnits
 *   Units for the character height of the text. If CoordSys is
 *   \ref CoordSys_Frame, units must be \ref Units_Frame or \ref Units_Point.
 *   If CoordSys is \ref CoordSys_Grid, units must be \ref Units_Frame or
 *   \ref Units_Grid.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetCoordSysAndUnits(
 *   &           TIDPtr,
 *   &           PositionCoordSys,
 *   &           HeightUnits)
 *    POINTER         (TIDPtr, TID)
 *    INTEGER*4       PositionCoordSys
 *    INTEGER*4       HeightUnits
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a text which is positioned and sized in the grid coordinate system that reads "Hello."
 *   Then, change the text to be positioned and sized in the frame coordinate system :
 *
 * @code
 *   Text_ID Text;
 *   Text = TecUtilTextCreate(CoordSys_Grid, 0.25, 0.25,
 *                            Units_Grid, 0.25, "Hello.");
 *   if (Text != TECUTILBADID)
 *     {
 *        .
 *        .
 *      // Change the text position and size to
 *      // be in the frame coordinate system and
 *      // set the text position to be the center
 *      // of the frame coordinate system
 *       TecUtilTextSetCoordSysAndUnits(Text,CoordSys_Frame,
 *                                      Units_Frame);
 *       TecUtilTextSetXYPos(Text, 50.0, 50.0);
 *         .
 *         .
 *       TecUtilTextDelete(Text);
 *     }
 * @endcode
 *
 * @ingroup Text
 *
 */
LINKTOADDON void       STDCALL TecUtilTextSetCoordSysAndUnits(Text_ID    TID,
                                                              CoordSys_e PositionCoordSys,
                                                              Units_e    HeightUnits);
/**
 * Set the zone or map to which the text object is associated (if it is attached).
 *
 * @param TID
 *   Handle to the text object.
 *
 * @param ZoneOrMap
 *   Zone or Map.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetZoneOrMap(
 *   &           TIDPtr,
 *   &           ZoneOrMap)
 *    POINTER         (TIDPtr, TID)
 *    INTEGER*4       ZoneOrMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilTextIsAttached() and TecUtilTextSetAttached().
 *
 * @ingroup Text
 *
 */
LINKTOADDON void       STDCALL TecUtilTextSetZoneOrMap(Text_ID    TID,
                                                       EntIndex_t ZoneOrMap);
/**
 * Indicate if the text object should be attached to a zone or map.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @param Attached
 *   Set to TRUE to attach, FALSE otherwise.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetAttached(
 *   &           TIDPtr,
 *   &           Attached)
 *    POINTER         (TIDPtr, TID)
 *    INTEGER*4       Attached
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilTextGetZoneOrMap() and TecUtilTextSetZoneOrMap().
 *
 * @ingroup Text
 *
 */
LINKTOADDON void       STDCALL TecUtilTextSetAttached(Text_ID    TID,
                                                      Boolean_t Attached);
/**
 *   Set the color of a text object.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @param Color
 *   Text color. The possible values are: Black_C, Blue_C, Red_C, Green_C, Cyan_C, Purple_C, Yellow_C,
 *   White_C, Custom1_C, Custom2_C, Custom3_C, Custom4_C, Custom5_C, Custom6_C, Custom7_C, Custom8_C.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetColor(
 *   &           TIDPtr,
 *   &           Color)
 *    POINTER         (TIDPtr, TID)
 *    INTEGER*4       Color
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON void       STDCALL TecUtilTextSetColor(Text_ID      TID,
                                                   ColorIndex_t Color);
/**
 *   Set the font for a text object.
 *
 * @param TID
 *   Handle to the text object.
 *
 * @param Font
 *   Text font. The possible values are: Font_Helvetica, Font_HelveticaBold, Font_Greek, Font_Math,
 *   Font_UserDefined, Font_Times, Font_TimesItalic, Font_TimesItalicBold, Font_TimesBold,
 *   Font_Courier, Font_CourierBold
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetFont(
 *   &           TIDPtr,
 *   &           Font)
 *    POINTER         (TIDPtr, TID)
 *    INTEGER*4       Font
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilTextSetTypeface(), TecUtilTextGetTypefaceFamily(),
 *     TecUtilTextGetTypefaceIsBold(), TecUtilTextGetTypefaceIsItalic()
 *
 * @ingroup Text
 *
 */
LINKTOADDON void       STDCALL TecUtilTextSetFont(Text_ID TID,
                                                  Font_e  Font);

/**
 * Sets the requested typeface for a text object.
 *
 * For consistency across various platforms, Tecplot guarantees that the
 * following standard typeface family names are available: "Helvetica",
 * "Times", "Courier", "Greek", "Math", and "User Defined". Other typeface
 * family names may or may not be available depending on the TrueType fonts
 * available. If the typeface family name or style is not available, a suitable
 * replacement will be selected.
 *
 * @param TID
 *   Handle to the text object.
 * @param FontFamily
 *     Requested typeface family name.
 * @param IsBold
 *     Indicates if a bold typeface is requested.
 * @param IsItalic
 *     Indicates if a italic typeface is requested.
 *
 *
 * @pre <em>FontFamily</em>
 *   String must have a valid address and non-zero length.
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetTypeface(
 *   &           TIDPtr,
 *   &           FontFamily,
 *   &           IsBold,
 *   &           IsItalic)
 *    POINTER         (TIDPtr, TID)
 *    CHARACTER*(*)   FontFamily
 *    INTEGER*4       IsBold
 *    INTEGER*4       IsItalic
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilTextGetTypefaceFamily(), TecUtilTextGetTypefaceIsBold(),
 *     TecUtilTextGetTypefaceIsItalic()
 *
 * @ingroup Text
 *
 * @since 12.2.1.96181
 */
LINKTOADDON void STDCALL TecUtilTextSetTypeface(Text_ID     TID,
                                                const char* FontFamily,
                                                Boolean_t   IsBold,
                                                Boolean_t   IsItalic);

/**
 * Set the character height for a text object.
 *
 * @param TID
 *   Handle to the text object.
 *
 * @param Height
 *   Character height in the current text size units.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetHeight(
 *   &           TIDPtr,
 *   &           Height)
 *    POINTER         (TIDPtr, TID)
 *    REAL*8          Height
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilTextSetCoordSysAndUnits()
 *
 * @ingroup Text
 *
 */
LINKTOADDON void       STDCALL TecUtilTextSetHeight(Text_ID TID,
                                                    double  Height);
/**
 *   Set the type of the box surrounding the text object.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @param TextBoxType
 *   Text box type. The possible values are: TextBox_None, TextBox_Filled, TextBox_Hollow
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextBoxSetType(
 *   &           TIDPtr,
 *   &           TextBoxType)
 *    POINTER         (TIDPtr, TID)
 *    INTEGER*4       TextBoxType
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a hollow boxed text label.
 *
 * @code
 *   Text_ID Text;
 *   Text = TecUtilTextCreate(CoordSys_Frame,50.0,50.0,Units_Points,
 *                              30.0,"Hi Mom");
 *   TecUtilTextBoxSetType(Text, TextBox_Hollow);
 * @endcode
 *
 * @ingroup Text
 *
 */
LINKTOADDON void       STDCALL TecUtilTextBoxSetType(Text_ID   TID,
                                                     TextBox_e TextBoxType);
/**
 *   Set the margin between the text and the box surrounding the text object.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @param Margin
 *   Margin between the text and the box in percentage of the text height
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextBoxSetMargin(
 *   &           TIDPtr,
 *   &           Margin)
 *    POINTER         (TIDPtr, TID)
 *    REAL*8          Margin
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a boxed text label with a box margin of 60 percent of the height of the text:
 *
 * @code
 *   Text_ID Text;
 *   Text = TecUtilTextCreate(CoordSys_Frame,50.0,50.0,Units_Points,30.0,
 *                            "Hi Mom");
 *   TecUtilTextBoxSetType(Text, TextBox_Hollow);
 *   TecUtilTextBoxSetMargin(Text, 60.0);
 * @endcode
 *
 * @ingroup Text
 *
 */
LINKTOADDON void       STDCALL TecUtilTextBoxSetMargin(Text_ID TID,
                                                       double  Margin);
/**
 *   Set the line thickness of the box surrounding the text object.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @param LineThickness
 *   Line thickness of the box, in frame units
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextBoxSetLineThickness(
 *   &           TIDPtr,
 *   &           LineThickness)
 *    POINTER         (TIDPtr, TID)
 *    REAL*8          LineThickness
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON void       STDCALL TecUtilTextBoxSetLineThickness(Text_ID TID,
                                                              double  LineThickness);
/**
 *   Set the line color for the box surrounding a text object.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @param BoxColor
 *   Line color of the box. The possible values are: Black_C, Blue_C, Red_C, Green_C, Cyan_C, Purple_C,
 *   Yellow_C, White_C, CustomXX_C where XX is in the range 1-64.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextBoxSetColor(
 *   &           TIDPtr,
 *   &           BoxColor)
 *    POINTER         (TIDPtr, TID)
 *    INTEGER*4       BoxColor
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a boxed text label with the box color set to red.
 *
 * @code
 *   Text_ID Text;
 *   Text = TecUtilTextCreate(CoordSys_Frame,50.0,50.0,Units_Point,
 *                            30.0,"Hi Mom");
 *   TecUtilTextBoxSetType(Text, TextBox_Hollow);
 *   TecUtilTextBoxSetColor(Text, Red_C);
 * @endcode
 *
 * @ingroup Text
 *
 */
LINKTOADDON void       STDCALL TecUtilTextBoxSetColor(Text_ID      TID,
                                                      ColorIndex_t BoxColor);
/**
 *   Set the fill color of the box surrounding a text object.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @param BoxFillColor
 *   Fill color of the box. The possible values are: Black_C, Blue_C, Red_C, Green_C, Cyan_C, Purple_C,
 *   Yellow_C, White_C, CustomXX_C where XX is in the range 1-64.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextBoxSetFillColor(
 *   &           TIDPtr,
 *   &           BoxFillColor)
 *    POINTER         (TIDPtr, TID)
 *    INTEGER*4       BoxFillColor
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a filled boxed text label with a fill color of blue.
 *
 * @code
 *   Text_ID Text;
 *   Text = TecUtilTextCreate(CoordSys_Frame,50.0,50.0,Units_Point,30.0,
 *                            "Hi Mom");
 *   TecUtilTextBoxSetType(Text, TextBox_Filled);
 *   TecUtilTextBoxSetColor(Text, Blue_C);
 * @endcode
 *
 * @ingroup Text
 *
 */
LINKTOADDON void       STDCALL TecUtilTextBoxSetFillColor(Text_ID      TID,
                                                          ColorIndex_t BoxFillColor);
/**
 *   Set the angle in degrees for a text object.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @param Angle
 *   Text angle in degrees that must be between the inclusive angles of -360 and 360.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetAngle(
 *   &           TIDPtr,
 *   &           Angle)
 *    POINTER         (TIDPtr, TID)
 *    REAL*8          Angle
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON void       STDCALL TecUtilTextSetAngle(Text_ID TID,
                                                   double  Angle);
/**
 *   Set the anchor style for a text object.
 *
 * @param TID
 *   Handle to a text object.
 *
 * @param Anchor
 *   Anchor style. The possible values are: TextAnchor_Left, TextAnchor_Center, TextAnchor_Right,
 *   TextAnchor_MidLeft, TextAnchor_MidCenter, TextAnchor_MidRight, TextAnchor_HeadLeft,
 *   TextAnchor_HeadCenter, TextAnchor_HeadRight, TextAnchor_OnSide
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetAnchor(
 *   &           TIDPtr,
 *   &           Anchor)
 *    POINTER         (TIDPtr, TID)
 *    INTEGER*4       Anchor
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON void       STDCALL TecUtilTextSetAnchor(Text_ID      TID,
                                                    TextAnchor_e Anchor);
/**
 *   Set the line spacing for a text object. Line spacing is dependent on the height of the text and
 *   the size unit system in which it is drawn.
 *
 * @param TID
 *   Handle to the text object.
 *
 * @param LineSpacing
 *   Vertical spacing between multiple lines of a text object. Multiple lines are achieved by inserting
 *   newline (\\n) characters within the string of the text object
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetLineSpacing(
 *   &           TIDPtr,
 *   &           LineSpacing)
 *    POINTER         (TIDPtr, TID)
 *    REAL*8          LineSpacing
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON void       STDCALL TecUtilTextSetLineSpacing(Text_ID TID,
                                                         double  LineSpacing);
/**
 *   Set the scope of the text object. Text with local scope is displayed only in the frame in which it
 *   is created. If the text is defined as having global scope it will appear in all "like"
 *   frames, that is, those frames using the same data set as the one in which the text was created.
 *
 * @param TID
 *   Handle to the text object.
 *
 * @param Scope
 *   Text scope. The possible values are Scope_Local or Scope_Global.
 *
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetScope(
 *   &           TIDPtr,
 *   &           Scope)
 *    POINTER         (TIDPtr, TID)
 *    INTEGER*4       Scope
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON void       STDCALL TecUtilTextSetScope(Text_ID TID,
                                                   Scope_e Scope);
/**
 *   Set the macro function command associated with a text object.
 *
 * @param TID
 *   Handle to the text object.
 *
 * @param Command
 *   Macro function command string.
 *
 * @return
 *   TRUE if sufficient memory was available to make a copy of Command, otherwise FALSE.
 *
 *
 * @pre <em>Command</em>
 *   Pointer must be a valid address or NULL.
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextSetMacroFunctionCmd(
 *   &                   TIDPtr,
 *   &                   Command)
 *    POINTER         (TIDPtr, TID)
 *    CHARACTER*(*)   Command
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilTextSetMacroFunctionCmd(Text_ID     TID,
                                                              const char *Command);
/**
 *   Set the text string for a text object.
 *
 * @param TID
 *   Handle to the text object.
 *
 * @param TextString
 *   String copied into the text object.
 *
 * @return
 *   TRUE if sufficient memory exists for the string copy, otherwise FALSE.
 *
 *
 * @pre <em>TextString</em>
 *   Pointer must be a valid address or NULL.
 * @pre @e TID must be a valid text ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextSetString(
 *   &                   TIDPtr,
 *   &                   TextString)
 *    POINTER         (TIDPtr, TID)
 *    CHARACTER*(*)   TextString
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilTextSetString(Text_ID     TID,
                                                    const char *TextString);

/**
 */
/**
 *   Set the clipping properties of a geometry.
 *
 * @param GID
 *   ID of the geometry
 *
 * @param Clipping
 *   New clipping property for the geometry. The possible values are: Clipping_ClipToViewport and
 *   Clipping_ClipToFrame
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetClipping(
 *   &           GIDPtr,
 *   &           Clipping)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       Clipping
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a red circle and set the clipping to "ClipToFrame":
 *
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   TecUtilGeomSetClipping(g, Clipping_ClipToFrame);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomSetClipping(Geom_ID    GID,
                                                      Clipping_e Clipping);
/**
 */
/**
 *   Set the anchor position for a geometry. For circles and ellipses, this is the center. For squares
 *   and rectangles, this is the base corner. For lines, this is the offset added to all points of the
 *   geometry.
 *
 * @param GID
 *   ID of a geometry
 *
 * @param XPos
 *   X-anchor position of geometry
 *
 * @param YPos
 *   Y-anchor position of geometry
 *
 * @param ZPos
 *   Z-anchor position of geometry (3-D geometries only)
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetAnchorPos(
 *   &           GIDPtr,
 *   &           XPos,
 *   &           YPos,
 *   &           ZPos)
 *    POINTER         (GIDPtr, GID)
 *    REAL*8          XPos
 *    REAL*8          YPos
 *    REAL*8          ZPos
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a circle and then move it:
 *
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   TecUtilRedraw();
 *   TecUtilGeomSetAnchorPos(45., 45., 0.);
 *   TecUtilRedraw();
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomSetAnchorPos(Geom_ID GID,
                                                       double  XPos,
                                                       double  YPos,
                                                       double  ZPos);
/**
 * @deprecated
 *   Please use TecUtilGeomGetAnchorPos() instead.
 *
 * @ingroup Geom
 *
 * #internalattributes exclude_python, exclude_sdkdoc
 */
LINKTOADDON void       STDCALL TecUtilGeomSetXYZAnchorPos(Geom_ID GID,
                                                          double  XPos,
                                                          double  YPos,
                                                          double  ZPos);
/**
 * Set the zone or Line-mapping attachment for a geometry. Use
 * TecUtilGeomSetAttached() to specify whether or not the geometry is attached
 * to a zone or Line-mapping.
 *
 * @param GID
 *   ID of a geometry
 *
 * @param ZoneOrMap
 *   Zone number or mapping number to which the geometry should be attached
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetZoneOrMap(
 *   &           GIDPtr,
 *   &           ZoneOrMap)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       ZoneOrMap
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomSetZoneOrMap(Geom_ID    GID,
                                                       EntIndex_t ZoneOrMap);
/**
 * Set whether or not a geometry is attached to a zone or Line-mapping. Use
 * TecUtilGeom() to set which zone or Line-mapping the geometry is attached to.
 *
 * @param GID
 *   Id of the geometry.
 *
 * @param Attached
 *   TRUE to attach the geometry to a zone or an Line-mapping.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetAttached(
 *   &           GIDPtr,
 *   &           Attached)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       Attached
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Attach a geometry to zone or mapping 5:
 *
 * @code
 *   extern Geom_ID g; // created elsewhere
 *   TecUtilGeomSetAttached(g, TRUE);
 *   TecUtilGeomSetZoneOrMap(g, 5);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomSetAttached(Geom_ID   GID,
                                                      Boolean_t Attached);
/**
 *   Set the line color of a geometry.
 *
 * @param GID
 *   ID of the geometry
 *
 * @param Color
 *   New line color for the geometry. The possible values are: Black_C, Blue_C, Red_C, Green_C, Cyan_C,
 *   Purple_C, Yellow_C, White_C, or CustomXX_C where XX ranges from 1 to 64.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetColor(
 *   &           GIDPtr,
 *   &           Color)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       Color
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a red circle:
 *
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   TecUtilGeomSetColor(g, Red_C);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomSetColor(Geom_ID      GID,
                                                   ColorIndex_t Color);
/**
 * Set the fill color of a geometry. Use TecUtilGeomSetIsFilled() to specify
 * whether or not a geometry is filled with color.
 *
 * @param GID
 *   ID of the geometry
 *
 * @param FillColor
 *   New fill color for the geometry. The possible values are: Black_C, Blue_C, Red_C, Green_C, Cyan_C,
 *   Purple_C, Yellow_C, White_C, or CustomXX_C where XX ranges from 1 to 64
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetFillColor(
 *   &           GIDPtr,
 *   &           FillColor)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       FillColor
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a red circle filled with yellow:
 *
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   TecUtilGeomSetColor(g, Red_C);
 *   TecUtilGeomSetFillColor(g, Yellow_C);
 *   TecUtilGeomSetIsFilled(g, TRUE);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomSetFillColor(Geom_ID      GID,
                                                       ColorIndex_t FillColor);
/**
 *   Set whether or not a geometry is filled with a color. Use TecUtilGeomSetFillColor() to specify the
 *   actual color to fill the geometry with.
 *
 * @param GID
 *   ID of a geometry
 *
 * @param IsFilled
 *   TRUE to fill the geometry, FALSE to not fill.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetIsFilled(
 *   &           GIDPtr,
 *   &           IsFilled)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       IsFilled
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomSetIsFilled(Geom_ID   GID,
                                                      Boolean_t IsFilled);
/**
 *   Set the line pattern for a geometry.
 *
 * @param GID
 *   ID of a geometry
 *
 * @param LinePattern
 *   Line pattern for the geometry. The possible values are LinePattern_Solid, LinePattern_Dashed,
 *   LinePattern_DashDot, LinePattern_Dotted, LinePattern_LongDash or LinePattern_DashDotDot
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetLinePattern(
 *   &           GIDPtr,
 *   &           LinePattern)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       LinePattern
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a dotted circle:
 *
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   TecUtilGeomSetLinePattern(g, LinePattern_Dotted);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomSetLinePattern(Geom_ID       GID,
                                                         LinePattern_e LinePattern);
/**
 *   Set the line pattern length for a geometry.
 *
 * @param GID
 *   ID of a geometry
 *
 * @param PatternLength
 *   Length of the line pattern in frame units.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetPatternLength(
 *   &           GIDPtr,
 *   &           PatternLength)
 *    POINTER         (GIDPtr, GID)
 *    REAL*8          PatternLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create two concentric dashed circles of different line pattern lengths (two and ten percent):
 *
 * @code
 *   Geom_ID g1, g2;
 *   g1 = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 20.);
 *   TecUtilGeomSetLinePattern(g1, Pattern_Dashed);
 *   TecUtilGeomSetPatternLength(g1, 2.);
 *   g2 = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 30.);
 *   TecUtilGeomSetLinePattern(g2, Pattern_Dashed);
 *   TecUtilGeomSetPatternLength(g2, 10.);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomSetPatternLength(Geom_ID GID,
                                                           double  PatternLength);
/**
 *   Set the line thickness for a geometry.
 *
 * @param GID
 *   ID of a geometry
 *
 * @param LineThickness
 *   Thickness of the lines in frame units
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetLineThickness(
 *   &           GIDPtr,
 *   &           LineThickness)
 *    POINTER         (GIDPtr, GID)
 *    REAL*8          LineThickness
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a circle with five percent thick lines:
 *
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   TecUtilGeomSetLineThickness(g, 5.);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomSetLineThickness(Geom_ID GID,
                                                           double  LineThickness);
/**
 *   Set the number of points used to draw a circle or an ellipse geometry.
 *
 * @param GID
 *   GID of a geometry. This must be a circle or ellipse geometry
 *
 * @param NumEllipsePts
 *   The number of points use to draw the circle or ellipse. This must be at least three points
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomEllipseSetNumPoints(
 *   &           GIDPtr,
 *   &           NumEllipsePts)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       NumEllipsePts
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a circle approximated by only five points. (This will look like a pentagon.)
 *
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   TecUtilGeomEllipseSetNumPoints(g, 5);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomEllipseSetNumPoints(Geom_ID     GID,
                                                              SmInteger_t NumEllipsePts);
/**
 *   Set the arrowhead style for a geometry.
 *
 * @param GID
 *   ID of the geometry. This must be a polyline or a multi-polyline geometry
 *
 * @param ArrowheadStyle
 *   The arrowhead style. The possible values are: ArrowheadStyle_Plain, ArrowheadStyle_Filled or
 *   ArrowheadStyle_Hollow.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomArrowheadSetStyle(
 *   &           GIDPtr,
 *   &           ArrowheadStyle)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       ArrowheadStyle
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a line with a filled arrowhead at the end:
 *
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeom2DLineSegmentCreate(CoordSys_Frame, 5., 5., 95., 95.);
 *   TecUtilGeomArrowheadSetAttach(g, ArrowheadAttachment_AtEnd);
 *   TecUtilGeomArrowheadSetStyle(g, ArrowheadStyle_Filled);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomArrowheadSetStyle(Geom_ID          GID,
                                                            ArrowheadStyle_e ArrowheadStyle);
/**
 *   Set the arrowhead attachment for a geometry.
 *
 * @param GID
 *   ID of the geometry. This must be a polyline or a multi-polyline geometry
 *
 * @param ArrowheadAttachment
 *   The arrowhead attachment style. The possible values are: ArrowheadAttachment_None
 *   ArrowheadAttachment_AtBeginning ArrowheadAttachment_AtEnd ArrowheadAttachment_AtBothEnds
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomArrowheadSetAttach(
 *   &           GIDPtr,
 *   &           ArrowheadAttachment)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       ArrowheadAttachment
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a line with arrowheads at both ends:
 *
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeom2DLineSegmentCreate(CoordSys_Frame, 5., 5., 95., 95.);
 *   TecUtilGeomArrowheadSetAttach(g, ArrowheadAttachment_AtBothEnds);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomArrowheadSetAttach(Geom_ID               GID,
                                                             ArrowheadAttachment_e ArrowheadAttachment);
/**
 *   Set the arrowhead size for a geometry.
 *
 * @param GID
 *   ID of the geometry. This must be a polyline or a multi-polyline geometry
 *
 * @param ArrowheadSize
 *   The arrowhead size in frame units
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomArrowheadSetSize(
 *   &           GIDPtr,
 *   &           ArrowheadSize)
 *    POINTER         (GIDPtr, GID)
 *    REAL*8          ArrowheadSize
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a line with a ten percent (frame units) arrowhead at the end:
 *
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeom2DLineSegmentCreate(CoordSys_Frame, 5., 5., 95., 95.);
 *   TecUtilGeomArrowheadSetAttach(g, ArrowheadAttachment_AtEnd);
 *   TecUtilGeomArrowheadSetSize(g, 10.);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomArrowheadSetSize(Geom_ID  GID,
                                                           double   ArrowheadSize);
/**
 *   Set the arrowhead angle for a geometry.
 *
 * @param GID
 *   ID of the geometry. This must be a polyline or a multi-polyline geometry
 *
 * @param ArrowheadAngle
 *   Angle for the arrowhead measured in degrees
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomArrowheadSetAngle(
 *   &           GIDPtr,
 *   &           ArrowheadAngle)
 *    POINTER         (GIDPtr, GID)
 *    REAL*8          ArrowheadAngle
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a line with a 15 degree arrowhead at the end:
 *
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeom2DLineSegmentCreate(CoordSys_Frame, 5., 5., 95., 95.);
 *   TecUtilGeomArrowheadSetAttach(g, ArrowheadAttachment_AtEnd);
 *   TecUtilGeomArrowheadSetAngle(g, 15.);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomArrowheadSetAngle(Geom_ID  GID,
                                                            double   ArrowheadAngle);

/**
 *   Sets the draw order of a geometry.
 *
 * @param GID
 *   ID of the geometry
 *
 * @param DrawOrder
 *   Must be DrawOrder_BeforeData or DrawOrder_AfterData.
 *
 * @pre Must have one or more frames.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetDrawOrder(
 *   &           GIDPtr,
 *   &           DrawOrder)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       DrawOrder
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomSetDrawOrder(Geom_ID     GID,
                                                       DrawOrder_e DrawOrder);

/**
 *   Queries the state of the "preserve aspect ratio" toggle for an image geometry.
 *
 * @param GID
 *   Geometry ID. Must be an image geometry
 *
 * @return
 *   Returns TRUE if the "preserve aspect ratio" toggle for an image geometry, FALSE otherwise.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomImageGetUseRatio(GIDPtr)
 *    POINTER (GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the state of the preserve aspect ration toggle in an image geometry:
 *
 * @code
 *   Boolean_t UsePreserveAspect = TecUtilGeomImageSetUseRatio(GID);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Boolean_t   STDCALL TecUtilGeomImageGetUseRatio(Geom_ID GID);

/**
 *   Gets the draw order of a geometry.
 *
 * @param GID
 *   ID of a geometry.
 *
 * @return
 *   The draw order of the geometry. Returns either DrawOrder_BeforeData or DrawOrder_AfterData.
 *
 * @pre Must have one or more frames.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetDrawOrder(GIDPtr)
 *    POINTER (GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the draw order of a geometry:
 *
 * @code
 *   DrawOrder_e DrawOrder = TecUtilGeomGetDrawOrder(GID);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON DrawOrder_e STDCALL TecUtilGeomGetDrawOrder(Geom_ID GID);

/**
 *   Set the scope for a geometry.
 *
 * @param GID
 *   ID of a geometry
 *
 * @param Scope
 *   Scope of geometry. The possible values are:Scope_Local (Show in the current frame
 *   only).Scope_Global (Show in all frames with the same data set as the current frame).
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetScope(
 *   &           GIDPtr,
 *   &           Scope)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       Scope
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomSetScope(Geom_ID  GID,
                                                   Scope_e  Scope);
/**
 *   Set the position coordinate system for a geometry. This will convert all values in the geometry as
 *   well as the anchor position such that the geometry remains in the same position on the screen.
 *
 * @param GID
 *   ID of a geometry. This must not be a 3-D polyline.
 *
 * @param CoordSys
 *   Coordinate system for the position of the geometry. The possible values are: CoordSys_Frame or
 *   CoordSys_Grid
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetPositionCoordSys(
 *   &           GIDPtr,
 *   &           CoordSys)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       CoordSys
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a 2-D line segment in frame coordinates and then convert those coordinates to grid
 *   coordinates. The geometry will be in the same location on the screen as its initial frame
 *   coordinates would indicate until the next time the view for that frame is changed.
 *
 * @code
 *   Geom_ID  g;
 *   g = TecUtilGeom2DLineSegmentCreate(CoordSys_Frame, 5, 5, 95, 95);
 *   TecUtilGeomSetPositionCoordSys(CoordSys_Grid);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void       STDCALL TecUtilGeomSetPositionCoordSys(Geom_ID    GID,
                                                              CoordSys_e CoordSys);
/**
 *   Set the macro function command for a geometry.
 *
 * @param GID
 *   ID of a geometry
 *
 * @param Command
 *   Macro function (and parameters) to be executed when the user holds down Ctrl and clicks the right
 *   mouse button on the geometry
 *
 *
 * @pre <em>Command</em>
 *   Pointer must be a valid address or NULL.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomSetMacroFunctionCmd(
 *   &                   GIDPtr,
 *   &                   Command)
 *    POINTER         (GIDPtr, GID)
 *    CHARACTER*(*)   Command
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set a geometry so that macro function "PlotData" is called whenever the user holds down Ctrl and
 *   clicks the right mouse button on the geometry.
 *
 * @code
 *   extern Geom_ID g; // created elsewhere
 *   TecUtilGeomSetMacroFunctionCmd(g, "PlotData");
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Boolean_t  STDCALL TecUtilGeomSetMacroFunctionCmd(Geom_ID     GID,
                                                              const char *Command);
/**
 *   Forces drop of opening banner. If this function is not called, the opening banner will stay up
 *   until all addons are loaded.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDropOpeningBanner()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilDropOpeningBanner(void);


/**
 * Pop Tecplot to the top of the Z-Order. This is Windows
 * only and may not be called when Tecplot is running in
 * batch mode.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPopMainProcessWindow()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilMacroIsBatchModeActive
 *
 * @ingroup Utilities
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecUtilPopMainProcessWindow(void);

/* Geometry/text convenience functions */

/**
 * Creates a text object. Use the handle obtained from this function to set
 * text attributes using the TecUtilTextSetXxx functions. Units are in frame
 * coordinates by default.
 *
 * @param PositionCoordSys
 *   Coordinate system used to position the text object. The possible values
 *   are: \ref CoordSys_Grid or \ref CoordSys_Frame
 *
 * @param PosX
 *   X-Coordinate for anchor position of the text in the specified
 *   PositionCoordSys coordinate system.
 *
 * @param PosY
 *   Y-Coordinate for anchor position of the text in the specified
 *   PositionCoordSys coordinate system.
 *
 * @param HeightUnits
 *   Units for the character height of the text. If PositionCoordSys is
 *   \ref CoordSys_Frame, units must be \ref Units_Frame or \ref Units_Point.
 *   If PositionCoordSys is \ref CoordSys_Grid, units must be \ref Units_Frame
 *   or \ref Units_Grid.
 *
 * @param Height
 *   Character height of the text in the specified HeightUnits units
 *
 * @param Text
 *   String to use to create the text object. Cannot be NULL
 *
 * @return
 *   If successfully created then the return value is a valid ID that you
 *   may use to further set attributes for this text object. Otherwise,
 *   \ref TECUTILBADID is returned.
 *
 *
 * @pre <em>Text</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextCreate(
 *   &           PositionCoordSys,
 *   &           PosX,
 *   &           PosY,
 *   &           HeightUnits,
 *   &           Height,
 *   &           Text,
 *   &           ResultPtr)
 *    INTEGER*4      PositionCoordSys
 *    REAL*8         PosX
 *    REAL*8         PosY
 *    INTEGER*4      HeightUnits
 *    REAL*8         Height
 *    CHARACTER*(*)  Text
 *    POINTER        (ResultPtr, Result)
 *
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a simple text label:
 *
 * @code
 *   Text_ID Text;
 *   Text = TecUtilTextCreate(CoordSys_Frame, 50.0, 50.0,
 *                            Units_Point, 30.0, "Hello");
 * @endcode
 *
 * @ingroup Text
 *
 */
LINKTOADDON Text_ID STDCALL TecUtilTextCreate(CoordSys_e  PositionCoordSys,
                                              double      PosX,
                                              double      PosY,
                                              Units_e     HeightUnits,
                                              double      Height,
                                              const char *Text);

/**
 * Create a text object and apply the attributes supplied in the arglist.
 * Use the handle obtained from this function to change any text attributes
 * using the TecUtilTextSetXxx functions. Units are in frame coordinates by
 * default.
 *
 * @since
 *   12.1.1.7313
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_TEXT
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   String for the text object.  Cannot be NULL.
 *
 * Name:
 *   SV_XPOS
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   No
 * Default:
 *   0.0
 * Notes:
 *   X-coordinate for anchor position specified by SV_POSITIONCOORDSYS
 *
 * Name:
 *   SV_YPOS
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 * Notes:
 *   Y-coordinate for anchor position specified by SV_POSITIONCOORDSYS
 *
 * Name:
 *   SV_POSITIONCOORDSYS
 * Type:
 *   CoordSys_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   CoordSys_Frame 
 * Required:
 *   No
 * Notes:
 *   Coordinate system used to position the text object. The possible values
 *   are: \ref CoordSys_Grid or \ref CoordSys_Frame
 *
 * Name:
 *   SV_FONT
 * Type:
 *   Font_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   Font_HelveticaBold
 * Required:
 *   No
 * Notes:
 *   This option is mutually exclusive with SV_FONTFAMILY, SV_ISBOLD, and
 *   SV_ISITALIC.
 *   Font used to render the text may have any of the following values:
 *   Font_Helvetica, Font_HelveticaBold Font_Greek, Font_Math,
 *   Font_UserDefined, Font_Times, Font_TimesItalic, Font_TimesBold,
 *   Font_TimesItalicBold, Font_Courier, Font_CourierBold
 *
 * Name:
 *   SV_FONTFAMILY
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   "Helvetica"
 * Required:
 *   No
 * Notes:
 *   This option is mutually exclusive with SV_FONT.
 *   Sets the requested typeface family name.
 *   For consistency across various platforms, Tecplot guarantees that the
 *   following standard typeface family names are available: "Helvetica",
 *   "Times", "Courier", "Greek", "Math", and "User Defined". Other typeface
 *   family names may or may not be available depending on the TrueType fonts
 *   available. If the typeface family name or style is not available, a
 *   suitable replacement will be selected.
 *
 * Name:
 *   SV_ISBOLD
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   This option is mutually exclusive with SV_FONT.
 *   Indicates if a bold typeface is requested.
 *
 * Name:
 *   SV_ITALIC
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   This option is mutually exclusive with SV_FONT.
 *   Indicates if an italic typeface is requested.
 *
 * Name:
 *   SV_SIZEUNITS
 * Type:
 *   Units_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   Units_Point
 * Required:
 *   No
 * Notes:
 *   Text sizing units may have any of the following values: Units_Grid,
 *   Units_Frame or Units_Point
 *
 * Name:
 *   SV_HEIGHT
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   14.0
 * Required:
 *   No
 * Notes:
 *   Text height in the specified units
 *
 * Name:
 *   SV_TEXTCOLOR
 * Type:
 *   ColorIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   Black_C
 * Required:
 *   No
 * Notes:
 *   Text color may be a value greater than or equal to zero
 *
 * Name:
 *   SV_ANGLE
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 * Notes:
 *   Text angle may have a value greater than or equal to -360 and less than or
 *   equal to 360
 *
 * Name:
 *   SV_LINESPACING
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   1.0
 * Required:
 *   No
 * Notes:
 *   Line spacing may have a value greater that or equal to zero and less than
 *   or equal to 50
 *
 * Name:
 *   SV_ANCHOR
 * Type:
 *   TextAnchor_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TextAnchor_Left
 * Required:
 *   No
 * Notes:
 *   Text anchor may have any of the following values: TextAnchor_Left,
 *   TextAnchor_Center, TextAnchor_Right, TextAnchor_MidLeft,
 *   TextAnchor_MidCenter, TextAnchor_MidRight, TextAnchor_HeadLeft,
 *   TextAnchor_HeadCenter, TextAnchor_HeadRight, TextAnchor_OnSide
 *
 * Name:
 *   SV_BOXTYPE
 * Type:
 *   TextBox_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TextBox_None
 * Required:
 *   No
 * Notes:
 *   Text box type may have any of the following values:  TextBox_None,
 *   TextBox_Filled, or TextBox_Hollow
 *
 * Name:
 *   SV_LINETHICKNESS
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.1
 * Required:
 *   No
 * Notes:
 *   Text box line thickness may be a value greater than or equal to 0.0001 or
 *   less than or equal to 100
 *
 * Name:
 *   SV_COLOR
 * Type:
 *   ColorIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   Black_C
 * Required:
 *   No
 * Notes:
 *   Text box line color may be a value greater or equal to zero.
 *
 * Name:
 *   SV_FILLCOLOR
 * Type:
 *   ColorIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   White_C
 * Required:
 *   No
 * Notes:
 *   Text box fill color may be a value greater or equal to zero
 *
 * Name:
 *   SV_MARGIN
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   20.0
 * Required:
 *   No
 * Notes:
 *   Margin between the text and the text box may be a value greater than or
 *   equal to zero and less than or equal to 2000
 *
 * Name:
 *   SV_ATTACHTOZONE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Flag that specifies the new text object will be attached to a zone or
 *   line map specified by the SV_ZONE parameter.
 *
 * Name:
 *   SV_ZONE
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Only if SV_ATTACHTOZONE is set to TRUE
 * Notes:
 *   Zone or line map to which the text will be attached, provided the
 *   SV_ATTACHTOZONE parameter is set to TRUE.
 * </ArgListTable>
 *   
 * @return
 *   If successfully created then the return value is a valid ID that you
 *   may use to further set attributes for this text object. Otherwise,
 *   \ref TECUTILBADID is returned.
 *
 *
 * @pre <em>ArgList</em>
 *   Argument list must be valid.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextCreateX(ArgListPtr)
 *    POINTER        (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a simple text label:
 *
 * @code
 *   Text_ID TextID;
 *   ArgList_pa ArgList;
 *   ArgList = TecUtilArgListAlloc();
 *   if (ArgList != NULL)
 *   {
 *       if (TecUtilArgListAppendString(ArgList, SV_TEXT, "Hello"))
 *           TextID = TecUtilTextCreateX(ArgList);
 *       TecUtilArgListDealloc(&ArgList);
 *   }
 * @endcode
 *
 * @ingroup Text
 *
 */
LINKTOADDON Text_ID STDCALL TecUtilTextCreateX(ArgList_pa ArgList);

/**
 *   Create a 3D text label in Tecplot.
 *
 * @param PosX
 *   The X-position of the text label.
 *
 * @param PosY
 *   The Y-position of the text label.
 *
 * @param PosZ
 *   The Z-position of the text label.
 *
 * @param HeightUnits
 *   The units to use for the text height.
 *
 * @param Height
 *   The height of the text.
 *
 * @param Text
 *   The text character string.
 *
 * @return
 *   Returns the ID for the created text.
 *
 *
 * @pre <em>Text</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilText3DCreate(
 *   &           PosX,
 *   &           PosY,
 *   &           PosZ,
 *   &           HeightUnits,
 *   &           Height,
 *   &           Text,
 *   &           ResultPtr)
 *    REAL*8         PosX
 *    REAL*8         PosY
 *    REAL*8         PosZ
 *    INTEGER*4      HeightUnits
 *    REAL*8         Height
 *    CHARACTER*(*)  Text
 *    POINTER        (ResultPtr, Result)
 *
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Text
 *
 */
LINKTOADDON Text_ID STDCALL TecUtilText3DCreate(double      PosX,
                                                double      PosY,
                                                double      PosZ,
                                                Units_e     HeightUnits,
                                                double      Height,
                                                const char *Text);
/**
 *   Get the position of the four corners of the box surrounding the text object.
 *
 * @param T
 *   Handle to a text object.
 *
 * @param X1
 *   Returned X-Coordinate for bottom left corner of the text box.
 *
 * @param Y1
 *   Returned Y-Coordinate for bottom left corner of the text box.
 *
 * @param X2
 *   Returned X-Coordinate for bottom right corner of the text box.
 *
 * @param Y2
 *   Returned Y-Coordinate for bottom right corner of the text box.
 *
 * @param X3
 *   Returned X-Coordinate for upper right corner of the text box.
 *
 * @param Y3
 *   Returned Y-Coordinate for upper right corner of the text box.
 *
 * @param X4
 *   Returned X-Coordinate for upper left corner of the text box.
 *
 * @param Y4
 *   Returned Y-Coordinate for upper left corner of the text box.
 *
 *
 * @pre <em>X1</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>X2</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>X3</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>X4</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>Y1</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>Y2</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>Y3</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>Y4</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextBoxGetPosition(
 *   &           T,
 *   &           X1,
 *   &           Y1,
 *   &           X2,
 *   &           Y2,
 *   &           X3,
 *   &           Y3,
 *   &           X4,
 *   &           Y4)
 *    INTEGER*4       T
 *    REAL*8          X1
 *    REAL*8          Y1
 *    REAL*8          X2
 *    REAL*8          Y2
 *    REAL*8          X3
 *    REAL*8          Y3
 *    REAL*8          X4
 *    REAL*8          Y4
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Obtain the four corners of the text referenced by T.
 *
 * @code
 *   // variable T of type Text_ID already obtained at this point.
 *   double X1,Y1,X2,Y2,X3,Y3,X4,Y4;
 *   TecUtilTextBoxGetPosition(T,&X1,&Y1,&X2,&Y2,&X3,&Y3,&X4,&Y4);
 * @endcode
 *
 * @ingroup Text
 *
 */
LINKTOADDON void STDCALL TecUtilTextBoxGetPosition(Text_ID        T,
                                                   TP_OUT double* X1,
                                                   TP_OUT double* Y1,
                                                   TP_OUT double* X2,
                                                   TP_OUT double* Y2,
                                                   TP_OUT double* X3,
                                                   TP_OUT double* Y3,
                                                   TP_OUT double* X4,
                                                   TP_OUT double* Y4);

/**
 * Create a square geometry. Use the ID obtained from this function to set
 * geometry attributes such as line style and color using the TecUtilGeomSetXxx
 * functions.
 *
 * @param PositionCoordSys
 *   Coordinate system used to position the geometry. The possible values are:
 *   \ref CoordSys_Grid or \ref CoordSys_Frame
 *
 * @param CornerX
 *   X-Coordinate for Lower left corner of the square
 *
 * @param CornerY
 *   Y-Coordinate for Lower left corner of the square
 *
 * @param Size
 *   Width/height of the square. Must be non-zero
 *
 * @return
 *   If successfully created then the return value is a valid ID that
 *   you may use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSquareCreate(
 *   &           PositionCoordSys,
 *   &           CornerX,
 *   &           CornerY,
 *   &           Size,
 *   &           ResultPtr)
 *    INTEGER*4      PositionCoordSys
 *    REAL*8         CornerX
 *    REAL*8         CornerY
 *    REAL*8         Size
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a square of width 0.5 and anchored at (0.1, 0.1):
 *
 * @code
 *   Geom_ID G;
 *   G = TecUtilGeomSquareCreate(CoordSys_Grid,0.1,0.1,0.5);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeomSquareCreate(CoordSys_e PositionCoordSys,
                                                    double     CornerX,
                                                    double     CornerY,
                                                    double     Size);

/**
 * Create a circle geometry. Use the ID obtained from this function to set
 * geometry attributes such as line style and color using the TecUtilGeomSetXxx
 * functions. To set the number of points used to draw the circle use
 * TecUtilGeomEllipseSetNumPoints().
 *
 * @param PositionCoordSys
 *   Coordinate system used to position the geometry. The possible values are:
 *   \ref CoordSys_Grid or \ref CoordSys_Frame
 *
 * @param CenterX
 *   X-Coordinate for the center of the circle.
 *
 * @param CenterY
 *   Y-Coordinate for the center of the circle.
 *
 * @param Radius
 *   Radius of the circle. Must be non-zero
 *
 * @return
 *   If successfully created then the return value is a valid ID that
 *   you may use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomCircleCreate(
 *   &           PositionCoordSys,
 *   &           CenterX,
 *   &           CenterY,
 *   &           Radius,
 *   &           ResultPtr)
 *    INTEGER*4      PositionCoordSys
 *    REAL*8         CenterX
 *    REAL*8         CenterY
 *    REAL*8         Radius
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a circle at 0.5, 0.5, with a radius of 0.2:
 *
 * @code
 *   Geom_ID G;
 *   G = TecUtilGeomCircleCreate(CoordSys_Grid,0.5,0.5,0.2);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeomCircleCreate(CoordSys_e PositionCoordSys,
                                                    double     CenterX,
                                                    double     CenterY,
                                                    double     Radius);

/**
 * Create a rectangle geometry. Use the ID obtained from this function to set
 * geometry attributes such as line style and color using the TecUtilGeomSetXxx
 * functions.
 *
 * @param PositionCoordSys
 *   Coordinate system used to position the geometry. The possible values are:
 *   \ref CoordSys_Grid or \ref CoordSys_Frame
 *
 * @param CornerX
 *   X-Coordinate for rectangle anchor position (left side of the rectangle).
 *
 * @param CornerY
 *   Y-Coordinate for rectangle anchor position (top of the rectangle).
 *
 * @param Width
 *   Width the rectangle. Must be non-zero.
 *
 * @param Height
 *   Height of the rectangle. Must be non-zero.
 *
 * @return
 *   If successfully created then the return value is a valid ID that
 *   you may use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomRectangleCreate(
 *   &           PositionCoordSys,
 *   &           CornerX,
 *   &           CornerY,
 *   &           Width,
 *   &           Height,
 *   &           ResultPtr)
 *    INTEGER*4      PositionCoordSys
 *    REAL*8         CornerX
 *    REAL*8         CornerY
 *    REAL*8         Width
 *    REAL*8         Height
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a rectangle anchored at (0.1, 0.1), with a width of 0.2 and a height of 0.3:
 *
 * @code
 *   Geom_ID G;
 *   G = TecUtilGeomRectangleCreate(.1,.1,0.2,0.3);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeomRectangleCreate(CoordSys_e PositionCoordSys,
                                                       double     CornerX,
                                                       double     CornerY,
                                                       double     Width,
                                                       double     Height);

/**
 * Create an ellipse geometry. Use the ID obtained from this function to set
 * geometry attributes such as line style and color using the TecUtilGeomSetXxx
 * functions.
 *
 * @param PositionCoordSys
 *   Coordinate system used to position the geometry. Valid values are
 *   \ref CoordSys_Grid or \ref CoordSys_Frame
 *
 * @param CenterX
 *   X-Coordinate for the center of the ellipse
 *
 * @param CenterY
 *   Y-Coordinate for the center of the ellipse
 *
 * @param HAxis
 *   Length of the horizontal axis. Must be non-zero
 *
 * @param VAxis
 *   Length of the vertical Axis. Must be non-zero
 *
 * @return
 *   If successfully created then the return value is a valid ID that
 *   you may use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomEllipseCreate(
 *   &           PositionCoordSys,
 *   &           CenterX,
 *   &           CenterY,
 *   &           HAxis,
 *   &           VAxis,
 *   &           ResultPtr)
 *    INTEGER*4      PositionCoordSys
 *    REAL*8         CenterX
 *    REAL*8         CenterY
 *    REAL*8         HAxis
 *    REAL*8         VAxis
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create an ellipse centered at 0.5, 0.5, with a horizontal axis of length 0.2 and a vertical axis
 *   of length 0.3:
 *
 * @code
 *   Geom_ID G;
 *   G = TecUtilGeomEllipseCreate(CoordSys_Grid, .5,.5,.2,.3);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeomEllipseCreate(CoordSys_e PositionCoordSys,
                                                     double     CenterX,
                                                     double     CenterY,
                                                     double     HAxis,
                                                     double     VAxis);

/**
 * Create a 2-D polyline geometry. Use the ID obtained from this function to
 * set geometry attributes such as line style and color using the
 * TecUtilGeomSetXxx functions. By default, the anchor position is set to
 * (0,0). All points are drawn relative to the anchor position. The anchor
 * position can be changed using TecUtilGeomSetXYZAnchorPos().
 *
 * @param PositionCoordSys
 *   Coordinate system used to position the geometry. The possible values are:
 *   \ref CoordSys_Grid or \ref CoordSys_Frame.
 *
 * @param PtsX_Array
 *   Array of X-coordinates of the polyline
 *
 * @param PtsY_Array
 *   Array of Y-coordinates of the polyline
 *
 * @param NumPts
 *   Number of points in the array (that is, number of points in the polyline).
 *   Must be at least two points.
 *
 * @return
 *   If successfully created, then the return value is a valid ID that
 *   you may use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *
 *
 * @pre <em>VALID_REF(PtsX)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(PtsY)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom2DPolylineCreate(
 *   &           PositionCoordSys,
 *   &           PtsX_Array,
 *   &           PtsY_Array,
 *   &           NumPts,
 *   &           ResultPtr)
 *    INTEGER*4      PositionCoordSys
 *    REAL*8         PtsX_Array
 *    REAL*8         PtsY_Array
 *    INTEGER*4      NumPts
 *    POINTER        (ResultPtr, Result)
 *
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Create a 2-D polyline with four points:
 * @code
 *   double X[4] = {.2,.5,.6,.4}; //x coords of the polyline
 *   double Y[4] = {.2,.5,.1,.7}; //y coords of the polyline
 *   Geom_ID G;
 *   G = TecUtilGeom2DPolylineCreate(CoordSys_Grid,X,Y,4);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeom2DPolylineCreate(CoordSys_e PositionCoordSys,
                                                        double    *PtsX_Array,
                                                        double    *PtsY_Array,
                                                        LgIndex_t  NumPts);

/**
 * Create a 3-D polyline geometry. Use the ID obtained from this function to
 * set geometry attributes such as line style and color using the
 * TecUtilGeomSetXxx function. By default, the anchor position is set to (0,0).
 * All points are drawn relative to the anchor position. The anchor position
 * can be changed using TecUtilGeomSetXYZAnchorPos(). All units are in grid
 * coordinates.
 *
 * @param PtsX_Array
 *   Array of X-coordinates of the polyline.
 *
 * @param PtsY_Array
 *   Array of Y-coordinates of the polyline.
 *
 * @param PtsZ_Array
 *   Array of Z-coordinates of the polyline.
 *
 * @param NumPts
 *   Number of points in the array (that is, number of points in the polyline).
 *   Must be at least two points
 *
 * @return
 *   If successfully created then the return value is a valid ID that
 *   you may use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *
 *
 * @pre <em>VALID_REF(PtsX)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(PtsY)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(PtsZ)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom3DPolylineCreate(
 *   &           PtsX_Array,
 *   &           PtsY_Array,
 *   &           PtsZ_Array,
 *   &           NumPts,
 *   &           ResultPtr)
 *    REAL*8         PtsX_Array
 *    REAL*8         PtsY_Array
 *    REAL*8         PtsZ_Array
 *    INTEGER*4      NumPts
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a 3-D polyline with four points:
 *
 * @code
 *   double X[4] = {.2,.5,.6,.4}; //x coords of the polyline
 *   double Y[4] = {.2,.5,.1,.7}; //y coords of the polyline
 *   double Z[4] = {.1,.2,.3,.4}; //z coords of the polyline
 *   Geom_ID G;
 *   G = TecUtilGeom3DPolylineCreate(X,Y,Z,4)
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeom3DPolylineCreate(double   *PtsX_Array,
                                                        double   *PtsY_Array,
                                                        double   *PtsZ_Array,
                                                        LgIndex_t NumPts);
/**
 *   Create a 2-D multi-polyline geometry. After creating the 2-D
 *   multi-polyline geometry, you must assign values to the points in it with
 *   TecUtilGeom2DPolylineSetPoint() or TecUtilGeom2DMPolySetPolyline(). Use
 *   the ID obtained from this function to set geometry attributes such as line
 *   style and color using the TecUtilGeomSetXxx functions.
 *
 * @param PositionCoordSys
 *   Coordinate System. The possible values are: CoordSys_Grid or
 *   CoordSys_Frame.
 *
 * @param NumPolys
 *   Number of polylines in the multi-polyline. Must be greater than zero
 *
 * @param NumPointsInPolylines_Array
 *   Array of points in each polyline. Each polyline must have at least two
 *   points
 *
 * @return
 *   The geometry ID of the 2-D multi polyline.
 *
 *
 * @pre <em>VALID_REF(NumPointsInPolylines)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE  TecUtilGeom2DMPolyCreate(
 *   &            PositionCoordSys,
 *   &            NumPolys,
 *   &            NumPointsInPolylines_Array,
 *   &            ResultPtr)
 *    INTEGER*4       PositionCoordSys
 *    INTEGER*4       NumPolys
 *    INTEGER*4       NumPointsInPolylines_Array
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a 2-D multi-polyline with two polylines. The first polyline has three points, the second
 *   has two:
 *
 * @code
 *   LgIndex_t pts_per_line[2] = { 3, Z }; // two polylines
 *   double x_polyline_1 = { 0.0, 1.0, 2.0 }; // three points
 *   double y_polyline_1 = { 0.0, 1.0, 0.0 };
 *   double x_polyline_2 = { 1.0, 2.0 }; // two points
 *   double y_polyline_2 = { 1.0, 0.0 };
 *   Geom_ID g;
 *
 *   g = TecUtilGeom2DMPolyCreate(CoordSys_Grid, Z, pts_per_line);
 *   TecUtilGeom2DMPolySetPolyline(g, 1, x_polyline_1, y_polyline_1);
 *   TecUtilGeom2DMPolySetPolyline(g, 2, x_polyline_2, y_polyline_2);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeom2DMPolyCreate(CoordSys_e PositionCoordSys,
                                                     LgIndex_t  NumPolys,
                                                     LgIndex_t  *NumPointsInPolylines_Array);

/**
 * Create a 3-D multi-polyline geometry. After creating the 3-D multi-polyline,
 * you must assign values to the points in it with either
 * TecUtilGeom3DMPolySetPoint() or TecUtilGeom3DMPolySetPolyline(). Use the ID
 * obtained from this function to set geometry attributes such as line style
 * and color using the TecUtilGeomSetXxx functions.
 *
 * @param NumPolys
 *   Number of polylines in the 3-D multi-polyline. Must be greater than zero.
 *
 * @param NumPointsInPolylines_Array
 *   Array of points in each polyline. Each polyline must have at least two
 *   points.
 *
 * @return
 *   The geometry ID of the 3-D multi-polyline.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom3DMPolyCreate(
 *   &           NumPolys,
 *   &           NumPointsInPolylines_Array,
 *   &           ResultPtr)
 *    INTEGER*4      NumPolys
 *    INTEGER*4      NumPointsInPolylines_Array
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Create a 3-D multi-polyline with 2 polylines. The first polyline has three
 * points, the second has two:
 *
 * @code
 *   LgIndex_t pts_per_line[2] = { 3, 2 }; // two polylines
 *   double x_polyline_1[] = { 0.0, 1.0, 2.0 }; // three points
 *   double y_polyline_1[] = { 0.0, 1.0, 0.0 };
 *   double z_polyline_1[] = { 0.0, 0.5, 0.0 };
 *   double x_polyline_2[] = { 1.0, 2.0 }; // two points
 *   double y_polyline_2[] = { 1.0, 0.0 };
 *   double z_polyline_2[] = { 0.5, 0.5 };
 *   Geom_ID g;
 *
 *   g = TecUtilGeom3DMPolyCreate(2, pts_per_line);
 *   TecUtilGeom3DMPolySetPolyline(g, 1, x_polyline_1,
 *                                 y_polyline_1, z_polyline_1);
 *   TecUtilGeom3DMPolySetPolyline(g, 2, x_polyline_2,
 *                                 y_polyline_2, z_polyline_2);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeom3DMPolyCreate(LgIndex_t NumPolys,
                                                     LgIndex_t *NumPointsInPolylines_Array);
/**
 * Create a 2-D arc. The arc is currently implemented as a 2-D polyline
 * geometry, thus, the type of object returned is a 2-D polyline geometry
 * object. Use the ID obtained from this function to set geometry attributes
 * such as line style and color using the TecUtilGeomSetXxx functions.
 *
 * @param PositionCoordSys
 *   Coordinate system used to position the geometry. Valid values are
 *   \ref CoordSys_Grid or \ref CoordSys_Frame.
 *
 * @param CenterX
 *   X-Coordinate for the Center of the arc.
 *
 * @param CenterY
 *   Y-Coordinate for the Center of the arc.
 *
 * @param Radius
 *   Radius of the arc. Must be greater than zero
 *
 * @param StartAngle
 *   Starting angle of the arc in degrees. Must be between zero and 360. (The
 *   2-D polyline that is created has on segment per degree of arc.)
 *
 * @param EndAngle
 *   Ending angle of the arc in degrees.
 *
 * @return
 *   If successfully created then the return value is a valid ID that
 *   you may use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomArcCreate(
 *   &           PositionCoordSys,
 *   &           CenterX,
 *   &           CenterY,
 *   &           Radius,
 *   &           StartAngle,
 *   &           EndAngle,
 *   &           ResultPtr)
 *    INTEGER*4      PositionCoordSys
 *    REAL*8         CenterX
 *    REAL*8         CenterY
 *    REAL*8         Radius
 *    REAL*8         StartAngle
 *    REAL*8         EndAngle
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create an arc of a circle of radius 0.5 centered at (0, 0) with an arc angle from 35 to 90 degrees
 *   (a 2-D polyline with 56 points, one point at each degree between 35 and 90):
 *
 * @code
 *   Geom_ID G;
 *   G = TecUtilGeomArcCreate(CoordSys_Grid, .3,.3,.5,35,90);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeomArcCreate(CoordSys_e PositionCoordSys,
                                                 double     CenterX,
                                                 double     CenterY,
                                                 double     Radius,
                                                 double     StartAngle,
                                                 double     EndAngle);

/**
 * Create a 2-D line geometry. Use the ID obtained from this function to set
 * geometry attributes such as line style and color using the TecUtilGeomSetXxx
 * functions.
 *
 * @param PositionCoordSys
 *   Coordinate system used to position the geometry. The possible values are:
 *   \ref CoordSys_Grid or \ref CoordSys_Frame.
 *
 * @param X1
 *   X-Coordinate for the starting position of the line.
 *
 * @param Y1
 *   Y-Coordinate for the starting position of the line.
 *
 * @param X2
 *   X-Coordinate for the ending position of the line.
 *
 * @param Y2
 *   Y-Coordinate for the ending position of the line.
 *
 * @return
 *   If successfully created then the return value is a valid ID that you may
 *   use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom2DLineSegmentCreate(
 *   &           PositionCoordSys,
 *   &           X1,
 *   &           Y1,
 *   &           X2,
 *   &           Y2,
 *   &           ResultPtr)
 *    INTEGER*4      PositionCoordSys
 *    REAL*8         X1
 *    REAL*8         Y1
 *    REAL*8         X2
 *    REAL*8         Y2
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a 2-D line geometry from (0.1, 0.2) to (0.5, 0.6):
 *
 * @code
 *   Geom_ID G;
 *   G = TecUtilGeom2DLineSegmentCreate(CoordSys_Grid,
 *                                      .1,.2,.5,.6);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeom2DLineSegmentCreate(CoordSys_e PositionCoordSys,
                                                           double     X1,
                                                           double     Y1,
                                                           double     X2,
                                                           double     Y2);

/**
 * Create a 3-D line. Use the ID obtained from this function to set geometry
 * attributes such as line style and color using the TecUtilGeomSetXxx
 * functions. All coordinates are in grid coordinates.
 *
 * @param X1
 *   X-Coordinate for Starting position of the line.
 *
 * @param Y1
 *   Y-Coordinate for Starting position of the line.
 *
 * @param Z1
 *   Z-Coordinate for Starting position of the line.
 *
 * @param X2
 *   X-Coordinate for ending position of the line.
 *
 * @param Y2
 *   Y-Coordinate for ending position of the line.
 *
 * @param Z2
 *   Z-Coordinate for ending position of the line.
 *
 * @return
 *   If successfully created then the return value is a valid ID that
 *   you may use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom3DLineSegmentCreate(
 *   &           X1,
 *   &           Y1,
 *   &           Z1,
 *   &           X2,
 *   &           Y2,
 *   &           Z2,
 *   &           ResultPtr)
 *    REAL*8         X1
 *    REAL*8         Y1
 *    REAL*8         Z1
 *    REAL*8         X2
 *    REAL*8         Y2
 *    REAL*8         Z2
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Create a 3-D line geometry from (0.1, 0.2, 0.2) to (0.5, 0.6, 0.1):
 *
 * @code
 *   Geom_ID G;
 *   G = TecUtilGeom3DLineSegmentCreate(.1,.2,.2,.5,.6,.1);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeom3DLineSegmentCreate(double X1,
                                                           double Y1,
                                                           double Z1,
                                                           double X2,
                                                           double Y2,
                                                           double Z2);
/**
 *   Get the number of polylines in a multi-polyline geometry.
 *
 * @param GID
 *   Geometry ID. Must be a multi-polyline geometry
 *
 * @return
 *   The number of polylines in a multi-polyline geometry.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomMPolyGetPolylineCnt(GIDPtr)
 *    POINTER (GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   To determine the number of polylines in a multi-polyline geometry:
 *
 * @code
 *   extern Geom_ID g; //created elsewhere, must be a multi-polyline geometry
 *   LgIndex_t npolylines = TecUtilGeomMPolyGetPolylineCnt(g);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilGeomMPolyGetPolylineCnt(Geom_ID GID);
/**
 *   Get the number of points in a polyline geometry.
 *
 * @param GID
 *   Geometry ID. Must be a multi-polyline geometry
 *
 * @return
 *   The number of points in a polyline geometry.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomPolyGetPointCount(GIDPtr)
 *    POINTER (GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilGeomPolyGetPointCount(Geom_ID GID);
/**
 *   Get information about the number of points in a polyline of a multi-polyline geometry.
 *
 * @param GID
 *   Geometry ID. Must be a multi-polyline geometry
 *
 * @param PolyNum
 *   Polyline of the multi-polyline geometry (starts at one)
 *
 * @return
 *   The number of points in the polyline.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomMPolyGetPointCount(
 *   &                   GIDPtr,
 *   &                   PolyNum)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       PolyNum
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   To determine the number of points in the second polyline of a multi-polyline geometry:
 *
 * @code
 *   extern Geom_ID g; //created elsewhere, must be a multi-polyline geometry
 *   LgIndex_t npts_2nd_polyline = TecUtilGeomMPolyGetPointCount(g, 2);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilGeomMPolyGetPointCount(Geom_ID   GID,
                                                            LgIndex_t PolyNum);
/**
 *   Gets the 2-D (X,Y) value of point in a 2-D multi-polyline geometry.
 *
 * @param GID
 *   Geometry ID. This must be a 2-D multi-polyline geometry
 *
 * @param PolyNum
 *   Polyline number. Must be greater than or equal to one, and less than or equal to the number of
 *   polylines in the geometry
 *
 * @param PointIndex
 *   Index of the point in the polyline. Must be greater than or equal to one, and less than or equal
 *   to the number of points in the polyline
 *
 * @param X
 *   Receives the X-value of the point. Must not be NULL
 *
 * @param Y
 *   Receives the Y-value of the point. Must not be NULL
 *
 *
 * @pre <em>VALID_REF(XOrTheta)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(YOrR)</em>
 *   Pointer must be a valid address and non-NULL.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom2DMPolyGetPoint(
 *   &           GIDPtr,
 *   &           PolyNum,
 *   &           PointIndex,
 *   &           X,
 *   &           Y)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       PolyNum
 *    INTEGER*4       PointIndex
 *    REAL*8          X
 *    REAL*8          Y
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the value of the tenth point in the second polyline of a 2-D multi-polyline geometry:
 *
 * @code
 *   double X,Y;
 *   extern Geom_ID g; // assume this was allocated somewhere
 *   TecUtilGeom2DMPolyGetPoint(g,2,10,&X,&Y);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeom2DMPolyGetPoint(Geom_ID        GID,
                                                    LgIndex_t      PolyNum,
                                                    LgIndex_t      PointIndex,
                                                    TP_OUT double* X,
                                                    TP_OUT double* Y);
/**
 *   Get a point (X,Y) of a 2-D polyline.
 *
 * @param GID
 *   Geometry ID. This must be a 2-D multi-polyline geometry
 *
 * @param PointIndex
 *   Index of the point in the polyline. Must be greater than or equal to one, and less than or equal
 *   to the number of points in the polyline
 *
 * @param X
 *   Receives the X-value of the point. Must not be NULL
 *
 * @param Y
 *   Receives the Y-value of the point. Must not be NULL
 *
 *
 * @pre <em>VALID_REF(XOrTheta)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(YOrR)</em>
 *   Pointer must be a valid address and non-NULL.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom2DPolylineGetPoint(
 *   &           GIDPtr,
 *   &           PointIndex,
 *   &           X,
 *   &           Y)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       PointIndex
 *    REAL*8          X
 *    REAL*8          Y
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the second point of a 2-D polyline geometry:
 *
 * @code
 *   double X,Y;
 *   extern Geom_ID g; // allocated somewhere else
 *   TecUtilGeom2DPolylineGetPoint(g,2,&X,&Y);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeom2DPolylineGetPoint(Geom_ID        GID,
                                                       LgIndex_t      PointIndex,
                                                       TP_OUT double* X,
                                                       TP_OUT double* Y);
/**
 *   Set the 2-D (X,Y) value of point in a 2-D multi-polyline geometry.
 *
 * @param GID
 *   Geometry ID. This must be a 2-D multi-polyline geometry
 *
 * @param PolyNum
 *   Polyline number. Must be greater than or equal to one, and less than or equal to the number of
 *   polylines in the geometry
 *
 * @param PointIndex
 *   Index of the point in the polyline. Must be greater than or equal to one, and less than or equal
 *   to the number of points in the polyline
 *
 * @param X
 *   New X-value of the point
 *
 * @param Y
 *   New Y-value of the point
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom2DMPolySetPoint(
 *   &           GIDPtr,
 *   &           PolyNum,
 *   &           PointIndex,
 *   &           X,
 *   &           Y)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       PolyNum
 *    INTEGER*4       PointIndex
 *    REAL*8          X
 *    REAL*8          Y
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the value of the tenth point in the second polyline of a 2-D multi-polyline geometry:
 *
 * @code
 *   extern Geom_ID g; // assume this was allocated somewhere
 *   TecUtilGeom2DMPolySetPoint(g,2,10,1.5,2.2);//set to (1.5,2.2)
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeom2DMPolySetPoint(Geom_ID   GID,
                                                    LgIndex_t PolyNum,
                                                    LgIndex_t PointIndex,
                                                    double    X,
                                                    double    Y);
/**
 *   Set a point (X,Y) of a 2-D polyline.
 *
 * @param GID
 *   Geometry ID. This must be a 2-D multi-polyline geometry
 *
 * @param PointIndex
 *   Index of the point to set. Must be greater than or equal to one, and less than or equal to the
 *   number of points in the polyline
 *
 * @param X
 *   The new X-value of the point
 *
 * @param Y
 *   The new Y-value of the point
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom2DPolylineSetPoint(
 *   &           GIDPtr,
 *   &           PointIndex,
 *   &           X,
 *   &           Y)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       PointIndex
 *    REAL*8          X
 *    REAL*8          Y
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the second point of a 2-D polyline geometry:
 *
 * @code
 *   extern Geom_ID g; // allocated somewhere else
 *
 *   // set to (1.1,2.5)
 *   TecUtilGeom2DPolylineSetPoint(g,2,1.1,2.5);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeom2DPolylineSetPoint(Geom_ID   GID,
                                                       LgIndex_t PointIndex,
                                                       double    X,
                                                       double    Y);
/**
 *   Set the points for a polyline in a 2-D multi-polyline geometry.
 *
 * @param GID
 *   Geometry ID. This must be a 2-D multi-polyline geometry
 *
 * @param PolyNum
 *   Polyline number. Must be greater than or equal to one, and less than or equal to the number of
 *   polylines in the geometry
 *
 * @param X_Array
 *   Array of X-values. The number of X-values must be equal to the number of points in the polyline
 *   specified by PolyNum. Must not be NULL
 *
 * @param Y_Array
 *   Array of Y-values. The number of Y-values must be equal to the number of points in the polyline
 *   specified by PolyNum. Must not be NULL
 *
 *
 * @pre <em>VALID_REF(XOrThetaArray)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(YOrRArray)</em>
 *   Pointer must be a valid address and non-NULL.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom2DMPolySetPolyline(
 *   &           GIDPtr,
 *   &           PolyNum,
 *   &           X_Array,
 *   &           Y_Array)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       PolyNum
 *    REAL*8          X_Array
 *    REAL*8          Y_Array
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeom2DMPolySetPolyline(Geom_ID   GID,
                                                       LgIndex_t PolyNum,
                                                       double    *X_Array,
                                                       double    *Y_Array);
/**
 *   Get the 3-D (X, Y, Z) value of point in a 3-D multi-polyline geometry.
 *
 * @param GID
 *   Geometry ID. This must be a 3-D multi-polyline geometry
 *
 * @param PolyNum
 *   Polyline number. Must be greater than or equal to one and less than or equal to the number of
 *   polylines in the geometry
 *
 * @param PointIndex
 *   Index of the point in the polyline. Must be greater than or equal to one and less than or equal to
 *   the number of points in the polyline
 *
 * @param X
 *   Receives the X-value of the point. Must not be NULL.
 *
 * @param Y
 *   Receives the Y-value of the point. Must not be NULL
 *
 * @param Z
 *   Receives the Z-value of the point. Must not be NULL
 *
 *
 * @pre <em>X</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Y</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Z</em>
 *   Pointer must be a valid address and non-NULL.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom3DMPolyGetPoint(
 *   &           GIDPtr,
 *   &           PolyNum,
 *   &           PointIndex,
 *   &           X,
 *   &           Y,
 *   &           Z)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       PolyNum
 *    INTEGER*4       PointIndex
 *    REAL*8          X
 *    REAL*8          Y
 *    REAL*8          Z
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the value of the tenth point in the second 3-D polyline of a multi-polyline geometry:
 *
 * @code
 *   double X,Y,Z;
 *   extern Geom_ID g; // assume this was allocated somewhere
 *   TecUtilGeom3DMPolyGetPoint(g,2,10,&X,&Y,&Z);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeom3DMPolyGetPoint(Geom_ID        GID,
                                                    LgIndex_t      PolyNum,
                                                    LgIndex_t      PointIndex,
                                                    TP_OUT double* X,
                                                    TP_OUT double* Y,
                                                    TP_OUT double* Z);
/**
 *   Get a point (X, Y, Z) of a 3-D polyline
 *
 * @param GID
 *   Geometry ID. This must be a 3-D multi-polyline geometry
 *
 * @param PointIndex
 *   Index of the point to get. Must be greater than or equal to one and less than or equal to the
 *   number of points in the polyline.
 *
 * @param X
 *   Receives the X-value of the point. Must not be NULL
 *
 * @param Y
 *   Receives the Y-value of the point. Must not be NULL
 *
 * @param Z
 *   Receives the Z-value of the point. Must not be NULL
 *
 *
 * @pre <em>X</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Y</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Z</em>
 *   Pointer must be a valid address and non-NULL.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom3DPolylineGetPoint(
 *   &           GIDPtr,
 *   &           PointIndex,
 *   &           X,
 *   &           Y,
 *   &           Z)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       PointIndex
 *    REAL*8          X
 *    REAL*8          Y
 *    REAL*8          Z
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the second point of a 3-D polyline geometry:
 *
 * @code
 *   double X,Y,Z;
 *   extern Geom_ID g; // allocated somewhere else
 *   TecUtilGeom3DPolylineGetPoint(g,2,&X,&Y,&Z);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeom3DPolylineGetPoint(Geom_ID        GID,
                                                       LgIndex_t      PointIndex,
                                                       TP_OUT double* X,
                                                       TP_OUT double* Y,
                                                       TP_OUT double* Z);
/**
 *   Set the 3-D (X, Y, Z) value of point in a 3-D multi-polyline geometry.
 *
 * @param GID
 *   Geometry ID. This must be a 3-D multi-polyline geometry
 *
 * @param PolyNum
 *   Polyline number. Must be greater than or equal to one and less than or equal to the number of
 *   polylines in the geometry
 *
 * @param PointIndex
 *   Index of the point in the polyline. Must be greater than or equal to one and less than or equal to
 *   the number of points in the polyline
 *
 * @param X
 *   New X-value of the point
 *
 * @param Y
 *   New Y-value of the point
 *
 * @param Z
 *   New Z-value of the point
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom3DMPolySetPoint(
 *   &           GIDPtr,
 *   &           PolyNum,
 *   &           PointIndex,
 *   &           X,
 *   &           Y,
 *   &           Z)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       PolyNum
 *    INTEGER*4       PointIndex
 *    REAL*8          X
 *    REAL*8          Y
 *    REAL*8          Z
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the value of the tenth point in the second polyline of a 3-D multi-polyline geometry:
 *
 * @code
 *   extern Geom_ID g; // assume this was allocated somewhere
 *   // set to (2.3,5.4,1.1)
 *   TecUtilGeom3DMPolySetPoint(g,2,10,2.3,5.4,1.1);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeom3DMPolySetPoint(Geom_ID   GID,
                                                    LgIndex_t PolyNum,
                                                    LgIndex_t PointIndex,
                                                    double    X,
                                                    double    Y,
                                                    double    Z);
/**
 *   Set a point (X, Y, Z) of a 3-D polyline.
 *
 * @param GID
 *   Geometry ID. This must be a 3-D multi-polyline geometry
 *
 * @param PointIndex
 *   Index of the point to set. Must be greater than or equal to one and less than or equal to the
 *   number of points in the polyline.
 *
 * @param X
 *   The new X-value of the point
 *
 * @param Y
 *   The new Y-value of the point
 *
 * @param Z
 *   The new Z-value of the point
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom3DPolylineSetPoint(
 *   &           GIDPtr,
 *   &           PointIndex,
 *   &           X,
 *   &           Y,
 *   &           Z)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       PointIndex
 *    REAL*8          X
 *    REAL*8          Y
 *    REAL*8          Z
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the second point of a 3-D polyline geometry:
 *
 * @code
 *   extern Geom_ID g; // allocated somewhere else
 *   TecUtilGeom3DPolylineSetPoint(g,2,1.1,2.5,1.0);
 *   // set to (1.1,2.5,1.0)
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeom3DPolylineSetPoint(Geom_ID   GID,
                                                       LgIndex_t PointIndex,
                                                       double    X,
                                                       double    Y,
                                                       double    Z);
/**
 *   Set the points for a polyline in a 3-D multi-polyline geometry.
 *
 * @param GID
 *   Geometry ID. This must be a 3-D multi-polyline geometry
 *
 * @param PolyNum
 *   Polyline number. Must be greater than or equal to one and less than or equal to the number of
 *   polylines in the geometry
 *
 * @param X_Array
 *   Array of X-values. The number of X-values must be equal to the number of points in the polyline
 *   specified by PolyNum. Must not be NULL
 *
 * @param Y_Array
 *   Array of Y-values. The number of Y-values must be equal to the number of points in the polyline
 *   specified by PolyNum. Must not be NULL
 *
 * @param Z_Array
 *   Array of Z-values. The number of Z-values must be equal to the number of points in the polyline
 *   specified by PolyNum. Must not be NULL
 *
 *
 * @pre <em>VALID_REF(X)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(Y)</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VALID_REF(Z)</em>
 *   Pointer must be a valid address and non-NULL.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom3DMPolySetPolyline(
 *   &           GIDPtr,
 *   &           PolyNum,
 *   &           X_Array,
 *   &           Y_Array,
 *   &           Z_Array)
 *    POINTER         (GIDPtr, GID)
 *    INTEGER*4       PolyNum
 *    REAL*8          X_Array
 *    REAL*8          Y_Array
 *    REAL*8          Z_Array
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeom3DMPolySetPolyline(Geom_ID   GID,
                                                       LgIndex_t PolyNum,
                                                       double    *X_Array,
                                                       double    *Y_Array,
                                                       double    *Z_Array);
/**
 *   Return the radius of a circle geometry.
 *
 * @param GID
 *   Geometry ID. This must be a circle geometry
 *
 * @return
 *   The radius of the circle.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilGeomCircleGetRadius(GIDPtr)
 *    POINTER         (GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Geom
 *
 */
LINKTOADDON double STDCALL TecUtilGeomCircleGetRadius(Geom_ID GID);
/**
 *   Set the radius of a circle geometry.
 *
 * @param GID
 *   Geometry ID. This must be a circle geometry
 *
 * @param Radius
 *   New radius of the circle. This must be non-zero
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomCircleSetRadius(
 *   &           GIDPtr,
 *   &           Radius)
 *    POINTER         (GIDPtr, GID)
 *    REAL*8          Radius
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the radius of a circle to one:
 *
 * @code
 *   extern Geom_id g; // must be a circle
 *   TecUtilGeomCircleSetRadius(g,1.0);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeomCircleSetRadius(Geom_ID GID,
                                                    double  Radius);
/**
 *   Get the size of a square geometry.
 *
 * @param GID
 *   Geometry ID. Must be a square geometry.
 *
 * @return
 *   The size of the square.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilGeomSquareGetSize(GIDPtr)
 *    POINTER (GIDPtr, GID)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the size of a square geometry:
 *
 * @code
 *   extern Geom_ID g; // must be a square
 *   double size = TecUtilGeomSquareGetSize(g);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON double STDCALL TecUtilGeomSquareGetSize(Geom_ID GID);
/**
 *   Set the size of a square geometry.
 *
 * @param GID
 *   Geometry ID. Must be a square geometry.
 *
 * @param Size
 *   New size of the square. Must be non-zero.
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSquareSetSize(
 *   &           GIDPtr,
 *   &           Size)
 *    POINTER         (GIDPtr, GID)
 *    REAL*8          Size
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the size of a square to one:
 *
 * @code
 *   extern Geom_id g; // must be a square
 *   TecUtilGeomSquareSetSize(g,1.0);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeomSquareSetSize(Geom_ID GID,
                                                  double  Size);
/**
 *   Get the width and height of a rectangle geometry.
 *
 * @param GID
 *   Geometry ID. Must be a rectangle geometry
 *
 * @param Width
 *   Receives the width of the rectangle. Must not be NULL
 *
 * @param Height
 *   Receives the height of the rectangle. Must not be NULL
 *
 *
 * @pre <em>Width</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>Height</em>
 *   Pointer must be a valid address or NULL.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomRectangleGetSize(
 *   &           GIDPtr,
 *   &           Width,
 *   &           Height)
 *    POINTER         (GIDPtr, GID)
 *    REAL*8          Width
 *    REAL*8          Height
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the width and height of a rectangle:
 *
 * @code
 *   double W,H;
 *   extern Geom_ID g; // must be a rectangle
 *   TecUtilGeomRectangleGetSize(g,&W,&H);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeomRectangleGetSize(Geom_ID        GID,
                                                     TP_OUT double* Width,
                                                     TP_OUT double* Height);
/**
 *   Set the width and height of a rectangle geometry.
 *
 * @param GID
 *   Geometry ID. Must be a rectangle geometry
 *
 * @param Width
 *   New width of the rectangle. Must be non-zero
 *
 * @param Height
 *   New height of the rectangle. Must be non-zero
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomRectangleSetSize(
 *   &           GIDPtr,
 *   &           Width,
 *   &           Height)
 *    POINTER         (GIDPtr, GID)
 *    REAL*8          Width
 *    REAL*8          Height
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the width and height of a rectangle:
 *
 * @code
 *   extern Geom_ID g; // must be a rectangle
 *   TecUtilGeomRectangleSetSize(g,4,1);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeomRectangleSetSize(Geom_ID GID,
                                                     double  Width,
                                                     double  Height);
/**
 *   Get length of the axes of an ellipse.
 *
 * @param GID
 *   ID of a geometry. This must be an ellipse geometry
 *
 * @param HAxis
 *   Receives the length of the horizontal axis. Must not be NULL
 *
 * @param VAxis
 *   Receives the length of the vertical axis. Must not be NULL
 *
 *
 * @pre <em>HAxis</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>VAxis</em>
 *   Pointer must be a valid address and non-NULL.
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomEllipseGetSize(
 *   &           GIDPtr,
 *   &           HAxis,
 *   &           VAxis)
 *    POINTER         (GIDPtr, GID)
 *    REAL*8          HAxis
 *    REAL*8          VAxis
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Get the length of the axes of an ellipse:
 *
 * @code
 *   extern Geom_ID g; // must be an ellipse
 *   double A,B;
 *   TecUtilGeomEllipseGetSize(g,&A,&B);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeomEllipseGetSize(Geom_ID        GID,
                                                   TP_OUT double* HAxis,
                                                   TP_OUT double* VAxis);
/**
 *   Set the length of the axes of an ellipse.
 *
 * @param GID
 *   Geometry ID. The must be an ellipse geometry
 *
 * @param HAxis
 *   The length for the horizontal axis. This must be non-zero
 *
 * @param VAxis
 *   The length for the vertical axis. This must be non-zero
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomEllipseSetSize(
 *   &           GIDPtr,
 *   &           HAxis,
 *   &           VAxis)
 *    POINTER         (GIDPtr, GID)
 *    REAL*8          HAxis
 *    REAL*8          VAxis
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Set the major and minor axes of an ellipse:
 *
 * @code
 *   extern Geom_ID g; // must be an ellipse
 *   TecUtilGeomEllipseGetSize(g,2.0,1.0);
 * @endcode
 *
 * @ingroup Geom
 *
 */
LINKTOADDON void STDCALL TecUtilGeomEllipseSetSize(Geom_ID GID,
                                                   double  HAxis,
                                                   double  VAxis);

/**
 * Get the number of custom label sets.
 *
 * @since
 *   14.1
 *
 * @return
 *   The number of custom label sets in the dataset attached to the current frame.
 *
 * @sa TecUtilCustomLabelsGet, TecUtilCustomLabelsAppend
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCustomLabelsGetNumSets()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Determine if the current frame's dataset has any custom labels.
 *
 * @code
 *   if (TecUtilCustomLabelsGetNumSets() > 0)
 *     {
 *       // there are custom labels, do something
 *     }
 * @endcode
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON LgIndex_t STDCALL TecUtilCustomLabelsGetNumSets(void);

/**
 * Get the custom label set.
 *
 * @since
 *   14.1
 *
 * @param LabelList
 *   Receives the list of custom labels. You must free this pointer by calling
 *   TecUtilStringListDealloc().
 *
 * @param WhichSet
 *   Specifies which set of labels to return. Must be between 1 and the number of
 *   available sets, inclusive.
 *
 * @return
 *   The TRUE if successful, FALSE otherwise.
 *
 * @sa TecUtilCustomLabelsGetNumSets, TecUtilCustomLabelsAppend
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set.
 *
 * @pre <em>LabelList</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCustomLabelsGet(LabelList, WhichSet)
 *    POINTER(LabelListPtr, LabelList)
 *    INTEGER*4 WhichSet
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Retrieve the first set of custom labels:
 *
 * @code
 *   StringList_pa LabelList = NULL;
 *   if (TecUtilCustomLabelsGet(&LabelList, 1))
 *     {
 *       // Do something with the labels
 *       TecUtilStringListDealloc(&LabelList);
 *     }
 * @endcode
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilCustomLabelsGet(StringList_pa * LabelList, LgIndex_t WhichSet);

/**
 * Append a custom label set to data set.
 *
 * @since
 *   14.1
 *
 * @param LabelList
 *   Contains the list of custom labels to be appended.
 *
 * @return
 *   The TRUE if successful, FALSE otherwise.
 *
 * @sa TecUtilCustomLabelsGetNumSets, TecUtilCustomLabelsGet
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set.
 *
 * @pre <em>LabelList</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCustomLabelsAppend(LabelList)
 *    POINTER(LabelListPtr, LabelList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Append a set of custom labels:
 *
 * @code
 *   StringList_pa LabelList = TecUtilStringListAlloc();
 *   // Add labels to the string list
 *   if (TecUtilCustomLabelsAppend(LabelList))
 *     {
 *       // The new set is the last one
 *       LgIndex_t NewCustomLabelsNum = TecUtilCustomLabelsGetNumSets();
 *       ...
 *     }
 *   TecUtilStringListDealloc(&LabelList);
 * @endcode
 *
 * @ingroup DataServices
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilCustomLabelsAppend(StringList_pa LabelList);

/**
 * Get the current layout file name.
 *
 * @return
 *   The current layout file name including the path. You must call
 *   TecUtilStringDealloc() on the returned string.
 *
 *
 * @pre <em>IMPLICATION(LayoutFName != NULL,VALID_REF(LayoutFName))</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGetCurLayoutFName(
 *   &           Result,
 *   &           ResultLength)
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup LayoutSupport
 *
 */
LINKTOADDON TP_GIVES char* STDCALL TecUtilGetCurLayoutFName(void);

/* CORE SOURCE CODE REMOVED */


/**
 * Indicates if the variable is locked and optionally the mode in which it was
 * locked and the current lock owner.
 *
 * @since
 *   11.0-0-321
 *
 * @param Var
 *   Offset of the variable in question.
 * @param VarLockMode
 *   Reference to a variable locking mode enumeration. If NULL the argument is
 *   ignored otherwise the current locking mode is placed into the location
 *   pointed to by the reference.
 * @param LockOwner
 *   Reference to a lock owner string pointer. If NULL the argument is ignored
 *   otherwise the a copy of the lock owner string is placed into the location
 *   pointed to by the reference. It is the client's responsibility to free the
 *   resulting lock owner string with TecUtilStringDealloc() when finished with
 *   it.
 *
 * @return
 *   TRUE if the variable is locked, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>VarLockMode</em>
 *   Pointer must be a valid address or NULL.
 *
 * @pre <em>LockOwner</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * @sa TecUtilVariableLockOn(), TecUtilVariableLockOff().
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Variables
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilVariableIsLocked(EntIndex_t            Var,
                                                      TP_OUT VarLockMode_e* VarLockMode,
                                                      TP_GIVES char**       LockOwner);
/**
 * Lock the variable from being altered according to the specified locking
 * mode. The variable can only be locked if it isn't already locked.
 *
 * @since
 *   11.0-0-321
 *
 * @param Var
 *   Current offset of the variable to lock. The offset may change over the
 *   course of a Tepclot session as other variables are deleted or inserted
 *   into the dataset. For help tracking variables see TecUtilVarGetUniqueID()
 *   and TecUtilVarGetNumByUniqueID().
 * @param VarLockMode
 *   Variable locking mode. The possible values are:
 *   \ref VarLockMode_ValueChange (prevents modification of values in a
 *   variable but permits deletion), and \ref VarLockMode_Delete (prevents
 *   deletion of a varaible but permits modification).
 * @param LockOwner
 *   A unique non-zero length string identifying the lock owner. Add-ons can
 *   use the ADDON_NAME define for this value.
 *
 * @return
 *   TRUE if the variable could be locked, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>LockOwner</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * @sa TecUtilVariableLockOff(), TecUtilVariableIsLocked().
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Variables
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilVariableLockOn(EntIndex_t     Var,
                                                    VarLockMode_e  VarLockMode,
                                                    const char    *LockOwner);

/**
 * Unlock the variable. The variable must have been previously locked by the
 * same lock owner.
 *
 * @since
 *   11.0-0-321
 *
 * @param Var
 *   Offset of the variable to unlock. The offset used to lock the variable may
 *   not be the same as variables could be deleted or inserted into the For
 *   help tracking variables see TecUtilVarGetUniqueID() and
 *   TecUtilVarGetNumByUniqueID().
 * @param LockOwner
 *   A unique non-zero length string identifying the lock owner. Add-ons can
 *   use the ADDON_NAME define for this value. This string must match that of
 *   the variable locker.
 *
 * @return
 *   TRUE if the variable could be unlocked, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>LockOwner</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * @sa TecUtilVariableLockOn(), TecUtilVariableIsLocked().
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Variables
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilVariableLockOff(EntIndex_t  Var,
                                                     const char *LockOwner);

/**
 * Lock the data set attached to the current frame.
 *
 * @param LockString
 *   Unique string identifier originally used to lock the data set.
 *
 * @return
 *   Returns TRUE if the data set can be locked, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>LockString</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetLockOn(LockString)
 *    CHARACTER*(*) LockString
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Lock the data set using the identifier "banana."
 *
 * @code
 *   Boolean_t IsBananaLocked = FALSE;
 *   if (!TecUtilLockIsOn((char **)NULL))
 *     {
 *       IsLocked = TecUtilDataSetLockOn("banana");
 *     }
 *   ...
 *   if (IsBananaLocked)
 *     TecUtilDataSetLockOff("banana");
 * @endcode
 *
 * @ingroup DataServices
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetLockOn(const char *LockString);
/**
 *   Unlock the data set attached to the current frame.
 *
 * @param LockString
 *   Unique string identifier originally used to lock the data set.
 *
 * @return
 *   Returns TRUE if the data set can be unlocked, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>LockString</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetLockOff(LockString)
 *    CHARACTER*(*) LockString
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataServices
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetLockOff(const char *LockString);
/**
 * Query to see of the data set attached to the current frame is locked.
 *
 * This function is \ref threadsafe.
 *
 * @param LockString
 *   Allocated return string telling you the identifier originally used to lock the data set. You must
 *   deallocate this string when you are through with it. You can pass NULL for this parameter if you
 *   do not need to know who locked the data set
 *
 * @return
 *   Returns TRUE if the data set is locked, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 *
 * @pre <em>LockString</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetIsLocked(
 *   &                   LockString,
 *   &                   LockStringLength)
 *    CHARACTER*(*)   LockString
 *    INTEGER*4       LockStringLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetIsLocked(TP_GIVES char** LockString);

/**
 * Shows or hides the wait cursor and optionally displays a wait message.
 * This function can be used to display an hourglass cursor and optional
 * message during non-cancellable procedures. (If you need a cancel button
 * and/or progress indicator, then you must use
 * TecUtilDialogLaunchPercentDone() and TecUtilDialogDropPercentDone() rather
 * than this function.)
 *
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   Although this function can be called by multiple threads it is a no-op on all threads with the
 *   exception of the main user interface thread.
 *
 * @param WaitMessage
 *   Status message to be shown. The previous status message will be restored
 *   when the status is popped. If WaitMessage is NULL then "Working..." is
 *   placed on the status line
 *
 * @param DoWait
 *   Use TRUE at the start of a wait, FALSE at the end (calls must balance).
 *
 *
 * @pre <em>WaitMessage</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPleaseWait(
 *   &           WaitMessage,
 *   &           DoWait)
 *    CHARACTER*(*)   WaitMessage
 *    INTEGER*4       DoWait
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Show a wait message for a lengthy operation:
 *
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     TecUtilPleaseWait("Computing the first 1,000,000 digits of PI",TRUE);
 *     ComputePI(1000000); // This may take awhile on a slow machine
 *     TecUtilPleaseWait(NULL,FALSE); // Pop the wait cursor and
 *          // restore it to its previous state
 *     TecUtilDialogMessageBox("Finished computing PI",
 *                              MessageBoxType_Information);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 * @ingroup Utilities
 */
LINKTOADDON void STDCALL TecUtilPleaseWait(const char *WaitMessage,
                                           Boolean_t   DoWait);

/**
 *
 * @ingroup Undo
 *
 * #internalattributes exclude_alldoc
 */
LINKTOADDON void STDCALL TecUtilUndoStateBegin(UndoStateCategory_e Category);
/**
 *
 * @ingroup Undo
 *
 * #internalattributes exclude_alldoc
 */
LINKTOADDON void STDCALL TecUtilUndoStateEnd(Boolean_t DoInvalidate,
                                             Boolean_t DoCommit);

/**
 * Determine if you can undo the last operation.
 *
 * @return
 *   Returns TRUE if the last operation is undoable otherwise FALSE.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Undo
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilUndoCanUndo(void);
/**
 * Undo the last operation.
 *
 * @return
 *   Returns TRUE if the undo is successful otherwise FALSE.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Undo
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilUndoDoUndo(void);

/**
 * Get the current Undo category text string
 *
 * @param CategoryText
 *     The Undo category text.
 *
 * @return
 *      TRUE if the category text is returned, FALSE otherwise.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Undo
 *
 * @since 14.2
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilUndoGetCategoryText(TP_GIVES char** CategoryText);

/**
 * Create a new thread.   The thread will execute the specified function with the specified
 * client data.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.2-0-374
 *
 * @param ThreadFunction
 *   Function called with the new thread.
 * @param ThreadData
 *   Client data for the newly created thread.
 *
 * @return
 *   Returns TRUE if the thread was successfully created otherwise FALSE.
 *
 *
 * @pre <em>ThreadFunction</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Create a new thread that executes a function called SpawnedFunction.
 *
 * @code
 *   if (!TecUtilThreadCreateDetached(SpawnedFunction,(ArbParam_t)0))
 *     {
 *        //
 *        // Do something to handle thread failure.  System likely is
 *        // in serious trouble.
 *        //
 *     }
 * @endcode
 *
 * @ingroup Threading
 */
LINKTOADDON Boolean_t STDCALL TecUtilThreadCreateDetached(ThreadFunction_pf ThreadFunction,
                                                          ArbParam_t        ThreadData);

/**
 * Allocates and initializes a new thread mutex for use with the mutex
 * locking and unlocking functions.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.0-0-089
 *
 * @return
 *   Returns the newly allocated Mutex.
 *
 * @sa TecUtilThreadRecursiveMutexAlloc(), TecUtilThreadMutexDealloc(), TecUtilThreadMutexLock(),
 *     TecUtilThreadMutexUnlock().
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Allocate and use a mutex to make a section of code thread-safe. In reality,
 *   the mutex should be allocated in a thread-safe portion of the code, such as
 *   the add-on's InitTecAddOn routine, and freed only when there is no chance
 *   it will be locked again:
 *
 * @code
 *   extern Mutex_pa Mutex;
 *
 *   if (!Mutex)
 *       Mutex = TecUtilThreadMutexAlloc();
 *
 *   TecUtilThreadMutexLock(Mutex);
 *
 *   // do some operation involving global data
 *             .
 *             .
 *             .
 *   TecUtilThreadMutexUnlock(Mutex);
 *
 *   // get rid of the mutex
 *   TecUtilThreadMutexDealloc(&Mutex);
 * @endcode
 *
 * @ingroup Threading
 */
LINKTOADDON TP_GIVES Mutex_pa STDCALL TecUtilThreadMutexAlloc(void);

/**
 * Allocates and initializes a new thread mutex for use with the mutex locking and unlocking
 * functions. Unlike the non-recursive mutex, the same thread may attempt to re-lock the mutex
 * without causing a deadlock.
 * This function is \ref threadsafe.
 * @since
 *   13.4.0.23334
 *
 * @return
 *   Returns the newly allocated Mutex.
 *
 * @sa TecUtilThreadMutexAlloc(), TecUtilThreadMutexDealloc(), TecUtilThreadMutexLock(),
 *     TecUtilThreadMutexUnlock()
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Allocate and use a mutex to make a section of code thread-safe. In reality,
 *   the mutex should be allocated in a thread-safe portion of the code, such as
 *   the add-on's InitTecAddOn routine, and freed only when there is no chance
 *   it will be locked again:
 *
 * @code
 *   extern Mutex_pa Mutex;
 *
 *   if (!Mutex)
 *       Mutex = TecUtilThreadRecursiveMutexAlloc();
 *
 *   TecUtilThreadMutexLock(Mutex);
 *
 *   // do some operation involving global data
 *             .
 *             .
 *             .
 *   TecUtilThreadMutexUnlock(Mutex);
 *
 *   // get rid of the mutex
 *   TecUtilThreadMutexDealloc(&Mutex);
 * @endcode
 *
 * @ingroup Threading
 */
LINKTOADDON TP_GIVES Mutex_pa STDCALL TecUtilThreadRecursiveMutexAlloc(void);

/**
 * Deallocates thread mutex previously allocated with TecUtilThreadMutexAlloc().
 * The mutex must not be locked when this routine is called.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.0-0-089
 *
 * @param Mutex
 *   Pointer to the mutex to be deallocated.
 *
 * @sa TecUtilThreadMutexAlloc(), TecUtilThreadRecursiveMutexAlloc(),
 *     TecUtilThreadMutexLock(), TecUtilThreadMutexUnlock()
 *
 *
 * @pre <em>Mutex</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Mutex</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Threading
 */
LINKTOADDON void STDCALL TecUtilThreadMutexDealloc(TP_RECEIVES_GIVES Mutex_pa* Mutex);

/**
 * Lock on a thread mutex previously allocated with TecUtilThreadMutexAlloc().
 * Returns only when the lock is available.
 * This function is \ref threadsafe.
 *
 * @since
 *   11.0-0-089
 *
 * @param Mutex
 *   The mutex to be locked.
 *
 * @sa TecUtilThreadMutexAlloc(), TecUtilThreadRecursiveMutexAlloc(), TecUtilThreadMutexDealloc(),
 *     TecUtilThreadMutexUnlock()
 *
 *
 * @pre <em>Mutex</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Threading
 */
LINKTOADDON void STDCALL TecUtilThreadMutexLock(Mutex_pa Mutex);

/**
 * Unlock on a thread mutex previously locked with TecUtilThreadMutexLock().
 * This function is \ref threadsafe.
 *
 * @since
 *   11.0-0-089
 *
 * @param Mutex
 *   The mutex to be unlocked.
 *
 * @sa TecUtilThreadMutexAlloc(), TecUtilThreadRecursiveMutexAlloc(), TecUtilThreadMutexDealloc(),
 *     TecUtilThreadMutexLock()
 *
 *
 * @pre <em>Mutex</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Threading
 */
LINKTOADDON void STDCALL TecUtilThreadMutexUnlock(Mutex_pa Mutex);

/**
 * Allocates a condition variable.
 * This function is \ref threadsafe.
 *
 * @since 11.2-1-015
 *
 * @return
 *   Allocated condition variable or NULL if insufficient resources.
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Threading
 */
LINKTOADDON TP_GIVES Condition_pa STDCALL TecUtilThreadConditionAlloc(void);

/**
 * Deallocates a previously allocated condition variable.
 * This function is \ref threadsafe.
 *
 * @since 11.2-1-015
 *
 * @param Condition
 *   Address of a previously allocated condition variable or address to a NULL
 *   pointer.
 *
 *
 * @pre <em>Condition</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Condition</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Threading
 */
LINKTOADDON void STDCALL TecUtilThreadConditionDealloc(TP_RECEIVES_GIVES Condition_pa* Condition);

/**
 * Signals at least one thread, if any, waiting on the condition variable. The
 * signaling thread does not have to hold the mutex associated with the
 * conditional variable to send the signal however if predictable scheduling is
 * required it is recommended that the mutex be acquired first.
 * This function is \ref threadsafe.
 *
 * @since 11.2-1-015
 *
 * @param Condition
 *   Condition variable potentially being waited upon by other threads.
 *
 * Add a message to the message queue and signal a single waiting consumer
 * thread.
 * @code
 *   MyMessage SomeMessage;
 *   ... prepare message
 *   // add the message to the queue and signal a single waiting consumer thread
 *   TecUtilThreadMutexLock(MyMutex);
 *   QueueAddMessage(MyMessageQueue,SomeMessage);
 *   TecUtilThreadSignalCondition(MyCondition);
 *   TecUtilThreadMutexUnlock(MyMutex);
 *   ...
 * @endcode
 *
 *
 * @pre <em>Condition</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Threading
 */
LINKTOADDON void STDCALL TecUtilThreadSignalCondition(Condition_pa Condition);

/**
 * Signals all threads, if any, waiting on the condition variable. The
 * signaling thread does not have to hold the mutex associated with the
 * conditional variable to broadcast the signal, however if predictable
 * scheduling is required, it is recommended that the mutex be acquired first.
 * This function is \ref threadsafe.
 *
 * @since 11.2-1-015
 *
 * @param Condition
 *   Condition variable potentially being waited upon by other threads.
 *
 * Add a message to the message queue and signal all waiting waiting consumer
 * threads.
 * @code
 *   MyMessage SomeMessage;
 *   ... prepare message
 *   // add the message to the queue and signal all waiting consumer threads
 *   TecUtilThreadMutexLock(MyMutex);
 *   QueueAddMessage(MyMessageQueue,SomeMessage);
 *   TecUtilThreadBroadcastCondition(MyCondition);
 *   TecUtilThreadMutexUnlock(MyMutex);
 *   ...
 * @endcode
 *
 *
 * @pre <em>Condition</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Threading
 */
LINKTOADDON void STDCALL TecUtilThreadBroadcastCondition(Condition_pa Condition);

/**
 * Wait on the specified condition variable until signaled. The associated
 * mutex must be locked before issuing this call. After entering a wait state
 * the thread unlocks the mutex and waits for a signal. Upon successful return
 * the mutex is locked and owned by the calling thread.
 * This function is \ref threadsafe.
 *
 * @since 11.2-1-015
 *
 * @param Condition
 *   Condition variable used to wait upon.
 * @param Mutex
 *   A locked mutex associated with the condition variable.
 *
 * Wait for message queue to be given a message and consume it.
 * @code
 *   TecUtilThreadMutexLock(MyMutex);
 *
 *   // loop to make sure another thread didn't sneak in and take the message
 *   while (QueueIsEmpty(MyMessageQueue))
 *     TecUtilThreadWaitForCondition(MyCondition,MyMutex);
 *
 *   MyMessage SomeMessage = QueuePopMessage(MyMessageQueue);
 *
 *   TecUtilThreadMutexUnlock(MyMutex);
 *   ...do something with retrieved message
 * @endcode
 *
 *
 * @pre <em>Condition</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Mutex</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Threading
 */
LINKTOADDON void STDCALL TecUtilThreadWaitForCondition(Condition_pa Condition,
                                                       Mutex_pa     Mutex);

/**
 * Wait on the specified condition variable until signaled or until timed out.
 * The associated mutex must be locked before issuing this call. After entering
 * a wait state the thread unlocks the mutex and waits for a signal or the time
 * to expire. Upon successful return the mutex is locked and owned by the
 * calling thread.
 * This function is \ref threadsafe.
 *
 * @since 11.2-1-015
 *
 * @param Condition
 *   Condition variable used to wait upon.
 * @param Mutex
 *   A locked mutex associated with the condition variable.
 * @param WaitPeriodInMS
 *   Maximum time to wait before returning if not first signaled.
 *
 * @return
 *   @ref ConditionAwakeReason_Signaled is returned if the condition variable
 *   was signaled by another thread prior to the time expiring or
 *   @ref ConditionAwakeReason_TimedOut is returned if the time expired prior
 *   prior to being signaled by another thread.
 *
 * Wait for message queue to be given a message and consume it.
 * @code
 *   TecUtilThreadMutexLock(MyMutex);
 *
 *   Boolean_t TimedOut = FALSE;
 *   const Int32_t OneMinute = 60*1000L;
 *
 *   // loop to make sure another thread didn't sneak in and take the message
 *   while (QueueIsEmpty(MyMessageQueue) && !TimedOut)
 *     {
 *       ConditionAwakeReason_e AwakeReason =
 *         TecUtilThreadTimedWaitForCondition(MyCondition,MyMutex,OneMinute);
 *       TimedOut = (AwakeReason == ConditionAwakeReason_TimedOut);
 *     }
 *
 *   MyMessage SomeMessage;
 *   if (!TimedOut)
 *     SomeMessage = QueuePopMessage(MyMessageQueue);
 *
 *   TecUtilThreadMutexUnlock(MyMutex);
 *   ...do something with retrieved message if the wait didn't time out
 * @endcode
 *
 *
 * @pre <em>Condition</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>Mutex</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Threading
 */
LINKTOADDON ConditionAwakeReason_e STDCALL TecUtilThreadTimedWaitForCondition(Condition_pa Condition,
                                                                              Mutex_pa     Mutex,
                                                                              Int32_t      WaitPeriodInMS);

/**
 * Allocates a job control variable for associating jobs submitted to Tecplot's
 * standard thread pool with one another other. Primarily this value is used to
 * perform a join via a call to TecUtilThreadPoolWait().
 * This function is \ref threadsafe.
 *
 * @since 11.2-1-015
 *
 * @return
 *   Allocated job control variable or NULL if insufficient resources.
 *
 * @sa TecUtilThreadPoolAddJob(), TecUtilThreadPoolJobControlDealloc(),
 *     TecUtilThreadPoolPoolSize(), TecUtilThreadPoolJobThreadOffset()
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Threading
 */
LINKTOADDON TP_GIVES JobControl_pa STDCALL TecUtilThreadPoolJobControlAlloc(void);

/**
 * Deallocates a previously allocated job control variable that is no longer
 * needed. A job control variable is no longer needed when there are no more
 * jobs associated with it executing or waiting to be executed in the thread
 * pool.
 * This function is \ref threadsafe.
 *
 * @since 11.2-1-015
 *
 * @param JobControl
 *   Address of a previously allocated job control variable or address to a
 *   NULL pointer.
 *
 * @sa TecUtilThreadPoolAddJob(), TecUtilThreadPoolJobControlAlloc(),
 *     TecUtilThreadPoolPoolSize(), TecUtilThreadPoolJobThreadOffset()
 *
 *
 * @pre <em>JobControl</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>JobControl</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Threading
 */
LINKTOADDON void STDCALL TecUtilThreadPoolJobControlDealloc(TP_RECEIVES_GIVES JobControl_pa* JobControl);

/**
 * Submits a job be executed on the mainline. The current thread will wait until the job executes
 * on the mainline. This can be useful for calling routines that are required to be called on the
 * mainline, or to guarentee single-threaded execution.
 * This function is \ref threadsafe.
 *
 * @param Job
 *   Callback function executed by mainline.
 * @param JobData
 *   Job specific client data needed by the callback. It is passed to the
 *   callback when executed by the mainline.
 *
 * @code
 *   typedef struct
 *   {
 *     char* SomeData;
 *     int   SomeOtherData;
 *   } MyJobInfo_s;
 *
 *   ...
 *   static void STDCALL MyMainlineJob(ArbParam_t JobData)
 *   {
 *     MyJobInfo_s* MyJobInfo = (MyJobInfo_s*)JobData;
 *     ...do mainline task
 *   }
 *
 *  ...
 *  // submit job for mainline
 *  MyJobInfo_s MyJobInfo = {"Hello Mainline", 0};
 *  TecUtilMainlineInvoke(MyMainlineJob,
 *                        MyJobInfo);
 * @endcode
 *
 * @sa TecUtilThreadPoolAddJob()
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Threading
 */
LINKTOADDON void STDCALL TecUtilMainlineInvoke(ThreadPoolJob_pf Job,
                                               ArbParam_t       JobData);

/**
 * Submits a job to the thread pool using the specified job control. The
 * job may execute immediately or sometime in the future.
 * This function is \ref threadsafe.
 *
 * @since 11.2-1-015
 *
 * @par Note:
 *   All jobs submitted to the thread pool must be finished before Tecplot
 *   exits. To ensure that submitted jobs have finished use
 *   TecUtilThreadPoolWait() in conjunction with the associated
 *   JobControl to block the current thread until all jobs have finished.
 *   The last possible time to complete unfinished jobs is when you receive a
 *   StateChange_QuitTecplot in your registered state change callback.
 *
 * @param Job
 *   Callback function executed by a thread in the pool.
 * @param JobData
 *   Job specific client data needed by the callback. It is passed to the
 *   callback when executed by the thread.
 * @param JobControl
 *   Used to track jobs submitted to the thread pool. Most add-ons will submit
 *   all jobs to the pool using the same job control. Add-ons utilize this
 *   value by issuing calls to TecUtilThreadPoolWait() with the job control.
 *
 * @code
 *   typedef struct
 *   {
 *     int  Count;
 *     int* SomeData;
 *     ...perhaps some other data for the concurrent task
 *   } MyJobInfo_s;
 *
 *   ...
 *   static void STDCALL MyConcurrentJob(ArbParam_t JobData)
 *   {
 *     MyJobInfo_s* MyJobInfo = (MyJobInfo_s*)JobData;
 *     ...do concurrent task
 *   }
 *
 *  ...
 *  MyJobInfo_s* MyJobInfoList; // ...allocated and initialized elsewhere
 *  JobControl_pa MyJobControl = TecUtilThreadPoolJobControlAlloc();
 *  ...
 *  // submit jobs and wait until all threads complete
 *  for (int J = 0; J < NumJobs; J++)
 *    TecUtilThreadPoolAddJob(MyConcurrentJob,
 *                            &MyJobInfoList[J],
 *                            MyJobControl);
 *  TecUtilThreadPoolWait(MyJobControl);
 *
 *  // all jobs have finished, summarize results
 *  MyResults_s Results = SummarizeMyResults(MyJobInfoList);
 * @endcode
 *
 * @sa TecUtilThreadPoolWait(), TecUtilThreadPoolJobControlAlloc()
 *
 *
 * @pre <em>JobControl</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Threading
 */
LINKTOADDON void STDCALL TecUtilThreadPoolAddJob(ThreadPoolJob_pf Job,
                                                 ArbParam_t       JobData,
                                                 JobControl_pa    JobControl);

/**
 * Returns the number of worker threads assigned to this thread pool.
 * @return
 *     Number of worker threads assigned to this pool.
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since 13.2.0.19661
 * @ingroup Utilities
 */
LINKTOADDON int STDCALL TecUtilThreadPoolPoolSize(void);

/**
 * Returns this worker thread's unique offset. Worker threads in this thread's pool are sequentially
 * assigned unique, one based numbers between 0 and the value of TecUtilThreadPoolPoolSize().
 * This thread offset can be used as an indirection into some random access storage dimensioned by
 * the thread's thread pool value of TecUtilThreadPoolPoolSize().
 *
 * @par IMPORTANT NOTE:
 *     Only threads that have been created by Tecplot's thread pool may call this function.
 *
 * @return
 *     Zero based offset representing the unique thread offset.
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since 13.2.0.19661
 * @ingroup Utilities
 */
LINKTOADDON int STDCALL TecUtilThreadPoolJobThreadOffset(void);

/**
 * Blocks the calling thread until all jobs associated with the job control are
 * finished. If all jobs associated with the job control are finished this
 * function returns immediately.
 * This function is \ref threadsafe.
 *
 * @since 11.2-1-015
 *
 * @par Note:
 *   All jobs submitted to the thread pool must be finish before Tecplot
 *   exists. The last possible time to complete unfinished jobs is when you
 *   receive a StateChange_QuitTecplot in your registered state change
 *   callback.
 *
 * @param JobControl
 *   Identifies which jobs to wait on for completion.
 *
 * @sa TecUtilThreadPoolAddJob(), TecUtilThreadPoolJobControlAlloc(),
 *     TecUtilThreadPoolPoolSize(), TecUtilThreadPoolJobThreadOffset()
 *
 *
 * @pre <em>JobControl</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Threading
 */
LINKTOADDON void STDCALL TecUtilThreadPoolWait(JobControl_pa JobControl);

/**
 * Returns the number of jobs that can concurrently run within the thread pool.
 * This function is \ref threadsafe.
 *
 * @deprecated
 *   Please use TecUtilThreadPoolPoolSize
 *
 * @since 11.3-0-018
 *
 * @sa TecUtilThreadPoolAddJob(), TecUtilThreadPoolJobControlAlloc(),
 *     TecUtilThreadPoolPoolSize(), TecUtilThreadPoolJobThreadOffset()
 *
 * <FortranSyntax>
 *   This function is not available for FORTRAN.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Utilities
 */
LINKTOADDON int STDCALL TecUtilThreadPoolGetNumConcurrentJobs(void);

/**
 * Creates a new page.
 *
 * @param RenderHandle is a page's  client data. It will be used to render the page.
 *
 * @param PageConstructionHints user defined construction hints for a page that will be passed to
 * the pageCreateCallback when the page would need to be restored.
 *
 * @return TRUE if a page is created successfully.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @since
 *   11.2-0-447
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecEngPageCreateNew(ArbParam_t    RenderHandle,
                                                  StringList_pa PageConstructionHints);
/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback called whenever a page is created.
 *
 * @since
 *   11.0-5-17
 *
 * @param PageCreateCallback
 *   Page create callback function.
 * @param RegrationClientData
 *   Any client data that is needed by the page create callback function
 *   (other than the page client data)
 *
 * @sa TecEngPageDestroyRegisterCallback
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngPageCreateRegisterCallback(PageCreateCallback_pf PageCreateCallback,
                                                          ArbParam_t            RegistrationClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback called whenever a page is destroyed.
 *
 * @since
 *   11.0-5-17
 *
 * @param PageDestroyCallback
 *   Page create callback function.
 * @param RegrationClientData
 *   Any client data that is needed by the page destroy callback function
 *   (other than the page client data)
 *
 * @sa TecEngPageCreateRegisterCallback
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngPageDestroyRegisterCallback(PageDestroyCallback_pf PageDestroyCallback,
                                                           ArbParam_t             RegistrationClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback called whenever a different page is made the "current" page.
 *
 * @since
 *   11.0-5-17
 *
 * @param PageNewCurrentCallback
 *   New current page callback function.
 * @param RegrationClientData
 *   Any client data that is needed by the NewCurrentPage callback function
 *   (other than the page client data)
 *
 * @sa TecEngPageCreateRegisterCallback
 * @sa TecEngPageDestroyRegisterCallback
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngPageNewCurrentRegisterCallback(PageNewCurrentCallback_pf PageNewCurrentCallback,
                                                              ArbParam_t                RegistrationClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Set the print preview state of the current page.
 *
 * @since
 *   11.2-0-420
 *
 * @param IsActive
 *   Set to TRUE to enter print preview for the current page. Set to FALSE
 *   to exit print preview for the current page. The current page must not
 *   be in print preview mode to turn on print preview. Similarly the current
 *   page must be in print preview mode to deactivate print preview.
 *
 * @par Note:
 *   Do not change any Page settings while in print preview. This includes but is
 *   not limited to: UseSoftwareRendering and PaperAttributes.
 *
 * @sa TecEngPagePrintPreviewIsActive
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngPagePrintPreviewSetActive(Boolean_t IsActive);

/* - NO DOXYGEN COMMENT GENERATION -
 * Queries the print preview state of the current page.
 *
 * @since
 *   11.2-0-420
 *
 * @return
 *   TRUE if the current page is in print preview mode, FALSE otherwise.
 *
 * @sa TecEngPagePrintPreviewSetActive
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecEngPagePrintPreviewIsActive(void);

/* - NO DOXYGEN COMMENT GENERATION -
 * Register a callback called whenever an offscreen image needs to be constructed.
 *
 * @since
 *    11.2-0-054
 *
 * @param OffscreenImageCreateCallback
 *   callback function responsible for creation of a new offscreenimage.
 * @param RegrationClientData
 *   Any client data that is needed by the NewCurrentPage callback function
 *   (other than the page client data)
 *
 * @sa TecEngOffscreenImageDestroyRegisterCallback
 * @sa TecEngOffscreenImageGetRGBRowRegisterCallback
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngOffscreenImageCreateRegisterCallback(OffscreenImageCreateCallback_pf OffscreenImageCreateCallback,
                                                                    ArbParam_t                      RegistrationClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Register a callback called whenever an offscreen image needs to be destroyed.
 *
 * @since
 *    11.2-0-054
 *
 * @param OffscreenImageDestroyCallback
 *   callback function responsible for creation of a new offscreenimage.
 * @param RegrationClientData
 *   Any client data that is needed by the NewCurrentPage callback function
 *   (other than the page client data)
 *
 * @sa TecEngOffscreenImageCreateRegisterCallback
 * @sa TecEngOffscreenImageGetRGBRowRegisterCallback
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngOffscreenImageDestroyRegisterCallback(OffscreenImageDestroyCallback_pf OffscreenImageDestroyCallback,
                                                                     ArbParam_t                       RegistrationClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Executes offscreen rendering.
 *
 * @since
 *    14.1
 *
 * @param ExportRegion
 *   The region to be exported. This argument should be passed to TecEngRenderOffscreenImage.
 *
 * @param ImageHandle
 *  ImageHandle that will be requested to set the current OpenGL context. This handle would, 
 *  normally, be created by the function that was registered via TecEngOffscreenImageCreateRegisterCallback.
 *
 * @sa TecEngOffscreenImageCreateRegisterCallback
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecEngRenderOffscreenImage(
    ExportRegion_e ExportRegion,   /* IN */
    ArbParam_t     ImageHandle);   /* IN */

/* - NO DOXYGEN COMMENT GENERATION -
 * Register a callback called whenever RGB values need to be retrieved from an offscreen image.
 *
 * @since
 *    11.2-0-054
 *
 * @param OffscreenImageGetRGBRowCallback
 *   callback function responsible for creation of a new offscreenimage.
 * @param RegrationClientData
 *   Any client data that is needed by the NewCurrentPage callback function
 *   (other than the page client data)
 *
 * @sa TecEngOffscreenImageCreateRegisterCallback
 * @sa TecEngOffscreenImageDestroyRegisterCallback
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngOffscreenImageGetRGBRowRegisterCallback(OffscreenImageGetRGBRowCallback_pf OffscreenImageGetRGBRowCallback,
                                                                       ArbParam_t                         RegistrationClientData);

#if defined MSWIN 
/* - NO DOXYGEN COMMENT GENERATION -
 * Register a callback that will be called for printing an image.
 *
 * @since
 *    11.2-0-463
 *
 * @param WinPrintImageCallback
 *   callback function responsible for printing an image.
 *
 * @param RegrationClientData
 *   Any client data that is needed by the NewCurrentPage callback function
 *   (other than the page client data)
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngWinPrintImageRegisterCallback(WinPrintImageCallback_pf WinPrintImageCallback,
                                                             ArbParam_t               RegistrationClientData);
#endif /* MSWIN */

#if defined MSWIN
/* - NO DOXYGEN COMMENT GENERATION -
 * Register a callback that will be called during the printing.
 *
 * @since
 *    11.2-0-468
 *
 * @param WinGetPrinterContext
 *   callback function responsible for providing a printer context.
 *
 * @param RegistrationClientData
 *   Any client data that is needed by the NewCurrentPage callback function
 *   (other than the page client data)
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngWinPrinterGetContextRegisterCallback(WinPrinterGetContextCallback_pf WinGetPrinterContext,
                                                                    ArbParam_t                      RegistrationClientData);
#endif /* MSWIN */

/* - NO DOXYGEN COMMENT GENERATION -
 * Implements limited TU_ASSERT for Tecplot engine
 *
 * @since 11.3-5-010
 *
 * @param type
 *   Assertion Type
 *
 * @param expression
 *   Text representation of the assertion
 *
 * @param explanation
 *   Text description of the assertion
 *
 * @param utility
 *   Name of the utility containing the assertion
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngDoTUAssert(const char *type,
                                          const char *expression,
                                          const char *explanation,
                                          const char *utility);

/* - NO DOXYGEN COMMENT GENERATION -
 * Initializes the non-graphical part of the Tecplot engine. This function must
 * be called before TecEngStartup().
 *
 * @since
 *   11.0-0-397
 *
 * @param argc
 *   Number of command line arguments for Tecplot engine.
 * @param argv
 *   Array of command line argument strings for Tecplot engine.
 * @param oemAuthorizationCode
 *   Authorization code provided by Tecplot, Inc. to the application
 *   developer. The code is in the form of:
 *       "oemid tecplotver flags hashvalue"
 * @param modernizationLevel
 *   Level of modernization to use when initializing the engine.
 *
 * @return
 *   TRUE if initialization was successful, FALSE otherwise. FALSE generally
 *   indicates that an invalid oemAuthorizationCode was passed.
 *
 * @sa TecEngStartup
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON TecEngInitReturnCode_e STDCALL TecEngInit(int                       argc,
                                                      char                    **argv,
                                                      const char               *oemAuthorizationCode,
                                                      StateModernizationLevel_e modernizationLevel);

/**
 * Update a contour group. This function is only needed in UI code where immediate 
 * update of contour coloring is important.
 *
 * @since
 *   14.2
 *
 * @param ContourGroup
 *   The contour group that will be updated.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecAppUpdateGlobalContour(
 *   &                  ContourGroup)
 *    INTEGER*4 ContourGroup
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Contour
 *
 */
LINKTOADDON Boolean_t STDCALL TecAppUpdateGlobalContour(SmInteger_t ContourGroup);

/**
 * Initializes contour style if not already initialized.
 *
 * @param specificContourGroup
 *      If this parameter is -1, all contour groups are initialized,
 *      otherwise initializes just the specified group.
 *
 * @return
 *  TRUE if successful, FALSE otherwise
 *
 * @since
 *    14.2
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecAppContourAutoAssignStyle(
 *   &                   specificContourGroup)
 *    INTEGER*4          specificContourGroup
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecAppContourAutoAssignStyle(SmInteger_t specificContourGroup);


/**
 * Initializes RGB variables if not already initialized.
 *
 * @return
 *  TRUE if successful, FALSE otherwise
 *
 * @since
 *    14.2
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecAppRGBChannelAutoAssign()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecAppRGBChannelAutoAssign(void);

/**
 * Install a color map group. This function is only needed in UI code where immediate 
 * update of color map group is important.
 *
 * @since
 *   14.2
 *
 * @param ColorMapGroup
 *   The color map group that will be installed.
 *
 * <FortranSyntax>
 *    SUBROUTINE FUNCTION TecAppColorMapInstallGroup(
 *   &                  ColorMapGroup)
 *    INTEGER*4 ColorMapGroup
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Contour
 *
 */
LINKTOADDON void STDCALL TecAppColorMapInstallGroup(SmInteger_t ColorMapGroup);

/**
 *
 * Returns if the current layout filename is valid.
 *
 * @since
 *   14.2
 *
 * <FortranSyntax>
 *    SUBROUTINE FUNCTION TecAppLayoutFilenameIsValid()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppLayoutFilenameIsValid(void);

/**
 *
 * Activate the drawing of an axis at the center of rotation.
 *
 * @since
 *   14.2
 *
 * <FortranSyntax>
 *    SUBROUTINE FUNCTION TecAppUpdateCenterOfRotation()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Axis
 *
 */
LINKTOADDON void STDCALL TecAppUpdateCenterOfRotation(void);

/**
 *
 * Deactivate the drawing of an axis at the center of rotation.
 *
 * @since
 *   14.2
 *
 * <FortranSyntax>
 *    SUBROUTINE FUNCTION TecAppDisableCenterOfRotation()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Axis
 *
 */
LINKTOADDON void STDCALL TecAppDisableCenterOfRotation(void);

/**
 * This function is intended to be used only be used by SDK parent applications.
 * Retrieves the Addon Help About Tecplot SDK... information for
 * display in the parent application's Help dialog.
 *
 * @since
 *   14.1-0-24645
 *
 * @param AddonHelpAboutInfo
 *   Address to which an allocated copy the addon help about information is assigned.
 *
 * @return
 *   TRUE if the string was allocated, FALSE otherwise.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppGetAddonHelpAbout(TP_GIVES char** AddonHelpAboutInfo);

/* - NO DOXYGEN COMMENT GENERATION -
 * Retrieve information about the runtime environment.
 
 * @since
 * 14.2
 *
 * @param info
 *   Address to which an allocated copy of the information is assigned.
 *
 * @return
 *   TRUE if the string was allocated, FALSE otherwise.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppGetRuntimeEnvironment(TP_GIVES char** info);

/* - NO DOXYGEN COMMENT GENERATION -
 * Retrieve information about the platform.
 
 * @since
 * 14.2
 *
 * @param info
 *   Address to which an allocated copy of the information is assigned.
 *
 * @return
 *   TRUE if the string was allocated, FALSE otherwise.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppGetPlatform(TP_GIVES char** info);

/* - NO DOXYGEN COMMENT GENERATION -
* Retrieve the system temp folder for application data.

* @since
* 14.2
*
* @param info
*   Address to which an allocated copy of the information is assigned.
*
* @return
*   TRUE if the string was allocated, FALSE otherwise.
*
* <PythonSyntax>
* </PythonSyntax>
*
* @ingroup TecApp
*
* #internalattributes exclude_all, exclude_alldoc
*/

LINKTOADDON Boolean_t STDCALL TecAppGetTempDir(char** info);

/* - NO DOXYGEN COMMENT GENERATION -
 * Retrieves the Help About Tecplot SDK... information for
 * display in the parent application's Help dialog.
 *
 * @deprecated
 *   Please use TecAppGetHelpAbout
 *
 * @since
 *   11.2-0-495
 *
 * @param HelpAboutInfo
 *   Address to which an allocated copy the help about information is assigned.
 *
 * @return
 *   TRUE if the string was allocated, FALSE otherwise.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecEngGetHelpAbout(TP_GIVES char** HelpAboutInfo);

/**
 * This function is intended to be used only be used by SDK parent applications.
 * Retrieves the Help About Tecplot SDK... information for
 * display in the parent application's Help dialog.
 *
 * @since
 *   14.1-0-24645
 *
 * @param HelpAboutInfo
 *   Address to which an allocated copy the help about information is assigned.
 *
 * @return
 *   TRUE if the string was allocated, FALSE otherwise.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppGetHelpAbout(TP_GIVES char** HelpAboutInfo);

/**
 * Returns the state of OkToAutoSaveLayout engine variable
 * @since
 *  14.2   
 *
 * @return
 *   TRUE if Layout was already saved and can be auto saved.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppQueryOkToAutoSaveLayout(void);

/**
 * Determine if there is a data set attached to any page.
 *
 * @return
 *   TRUE if a data set exist, FALSE if not.
 *
 * @sa TecUtilDataSetIsAvailable
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecAppQueryIsAnyDataSetAvailable()
 * </FortranSyntax>
 *
 * @ingroup DataSetInfo
 *
 */
LINKTOADDON Boolean_t STDCALL TecAppQueryIsAnyDataSetAvailable(void);

/**
 * Skips the execution of the default button action in the next event.
 *
 * @since
 *  14.2   
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecAppSkipDefaultButtonActionOnNextEvent(void);

/**
 * This function saves the current style of the geometry so it is use the
 * next a geometry is created.
 *
 * @since
 *   14.2
 *
 * @param Geom_ID
 *   ptr to geometry details currently used
 *
 * @return
 *   void
 *
 * @pre @e GID must be a valid geometry ID.
 *
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecAppUpdateDefaultGeom(Geom_ID GID);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to be notified whenever the Tecplot engine needs to
 * switch the render destination such as to the workarea or offscreen bitmap.
 *
 * @since
 *   11.0-0-397
 *
 * @param RenderDestCallback
 *   Render desitination callback function.
 * @param ClientData
 *   Any client data that is needed by the registered render destination
 *   function.
 *
 * @sa TecEngInit
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngRenderDestRegisterCallback(RenderDestCallback_pf RenderDestCallback,
                                                          ArbParam_t            ClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to be notified whenever the Tecplot engine needs to
 * know if it should be rendering into a page.
 *
 * @since
 *   11.0-5-018
 *
 * @param RenderDestCallback
 *   Render desitination callback function.
 * @param ClientData
 *   Any client data that is needed by the registered render destination
 *   function.
 *
 * @sa TecEngInit
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngRenderQueryRegisterCallback(RenderQueryCallback_pf RenderQueryCallback,
                                                           ArbParam_t             ClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to be notified whenever the Tecplot engine needs to
 * know the size of the current render destination.
 *
 * @since
 *   11.0-0-397
 *
 * @param RenderDestSizeCallback
 *   Render desitination size callback function.
 * @param ClientData
 *   Any client data that is needed by the registered render destination
 *   size function.
 *
 * @sa TecEngInit
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngRenderDestSizeRegisterCallback(RenderDestSizeCallback_pf RenderDestSizeCallback,
                                                              ArbParam_t                ClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to be notified whenever the Tecplot engine needs to
 * swap the front and back buffers of the current OpenGL drawing state's render
 * destination.
 *
 * @since
 *   11.0-0-397
 *
 * @param SwapBuffersCallback
 *   Swap buffers callback function.
 * @param ClientData
 *   Any client data that is needed by the registered swap buffers function.
 *
 * @sa TecEngInit
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngSwapBuffersRegisterCallback(SwapBuffersCallback_pf SwapBuffersCallback,
                                                           ArbParam_t             ClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to be notified whenever the Tecplot engine needs to
 * know the state of selected keys.   Monitoring the event stream just doesn't
 * cut it.
 *
 * @since
 *   11.0-0-399
 *
 * @param KeyStateCallback
 *   Key State callback function.
 * @param ClientData
 *   Any client data that is needed by the registered key state function.
 *
 * @sa TecEngInit
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngKeyStateRegisterCallback(KeyStateCallback_pf KeyStateCallback,
                                                        ArbParam_t          ClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to be notified whenever the Tecplot engine needs to
 * know the state of the selected mouse button.  Monitoring the event stream
 * just doesn't cut it.
 *
 * @since
 *   11.0-0-424
 *
 * @param MouseButtonStateCallback
 *   Mouse button state callback function.
 * @param ClientData
 *   Any client data that is needed by the registered key state function.
 *
 * @sa TecEngInit
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngMouseButtonStateRegisterCallback(MouseButtonStateCallback_pf MouseButtonStateCallback,
                                                                ArbParam_t                  ClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to be notified whenever the Tecplot engine needs to
 * change the mouse cursor to a wait cursor. It is the responsibility of the
 * parent application to install the wait cursor when the WaitCursorStateCallback
 * is called.
 *
 * @since
 *   11.2-0-302
 *
 * @param WaitCursorStateCallback
 *   Wait cursor state callback function.
 * @param ClientData
 *   Any client data that is needed by the registered function.
 *
 * @sa TecEngInit
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngWaitCursorStateRegisterCallback(WaitCursorStateCallback_pf WaitCursorStateCallback,
                                                               ArbParam_t                 ClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to be notified whenever the Tecplot engine needs to
 * change the style of the mouse cursor. It is the responsibility of the
 * parent application to change the mouse cursor shape.
 *
 * @since
 *   11.2-0-302
 *
 * @param BaseCursorStyleCallback
 *   Mouse button state callback function.
 * @param ClientData
 *   Any client data that is needed by the registered key state function.
 *
 * @sa TecEngInit
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngBaseCursorStyleRegisterCallback(BaseCursorStyleCallback_pf BaseCursorStyleCallback,
                                                               ArbParam_t                 ClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to determine an action to take when selecting regions
 *
 * @since
 *   14.3
 *
 * @param SelectRegionCallback
 *   Select Region callback function.
 *
 * @param ClientData
 *   Any client data that is needed by the registered function.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngSelectRegionRegisterCallback(SelectRegionCallback_pf    SelectRegionCallback,
                                                            ArbParam_t                 ClientData);


/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to be notified whenever the Tecplot engine needs events
 * to be processed while it is busy doing a requested operation.
 *
 * @since
 *   11.0-0-415
 *
 * @param ProcessBusyEventsCallback
 *   Process busy events callback function. This function will be called at
 *   regular intervals to repair the interface and if required check for
 *   interrupts when the Tecplot engine is busy performing a long task. Very
 *   little work should be done by this function.
 * @param ClientData
 *   Any client data that is needed by the registered process busy events
 *   function.
 *
 * @sa TecEngInit, TecUtilInterrupt
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngProcessBusyEventsRegisterCallback(ProcessBusyEventsCallback_pf ProcessBusyEventsCallback,
                                                                 ArbParam_t                   ClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers callback function that will be called every time the engine returns from the last call
 * to a TecUtil/TecEng/TecApp. Tecplot SDK Integration Manager will react to this callback by 
 * scheduling a zero-timeout timer for the detection of the idle state.
 *
 * @since
 *   14.1
 *
 * @param EngineNotBusyCallback
 *   The callback function that will be called when engine returns from the last call to a 
 *   TecUtil/TecEng/TecApp. Use NULL to unregister a previously registered callback function.
 * @param ClientData
 *   Client data that was registered with the callback.
 */
LINKTOADDON void STDCALL TecEngEngineNotBusyRegisterCallback(EngineNotBusyCallback_pf EngineNotBusyCallback,
                                                             ArbParam_t               ClientData);
/**
 * Registers a launch and drop callback for the specified dialog.
 *
 * @since
 *   11.3-14-003
 *
 * @deprecated
 *
 * @param Dialog
 *   Dialog associated with the callbacks and client data.
 * @param DialogLaunchCallback
 *   Callback responsible for launching the dialog.
 * @param DialogDropCallback
 *   Callback responsible for dropping the dialog.
 * @param ClientData
 *   Any client data that is needed by the registered launch and/or drop dialog
 *   function.
 *
 * @sa TecUtilDialogLaunch, TecUtilDialogDrop,
 * TecUtilDialogAllowDoubleClickLaunch
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecUtilDialogLaunchDropRegisterCallbacks(Dialog_e                Dialog,
                                                                  DialogLaunchCallback_pf DialogLaunchCallback,
                                                                  DialogDropCallback_pf   DialogDropCallback,
                                                                  ArbParam_t              ClientData);




/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a launch and drop callback for the specified dialog.
 *
 * @since
 *   11.0-0-407 (exposed to the ADK since 11.0-4-056)
 *
 * @param Dialog
 *   Dialog associated with the callbacks and client data.
 * @param DialogLaunchCallback
 *   Callback responsible for launching the dialog.
 * @param DialogDropCallback
 *   Callback responsible for dropping the dialog.
 * @param ClientData
 *   Any client data that is needed by the registered launch and/or drop dialog
 *   function.
 *
 * @sa TecUtilDialogLaunch, TecUtilDialogDrop,
 * TecUtilDialogAllowDoubleClickLaunch
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngDialogLaunchDropRegisterCallbacks(Dialog_e                Dialog,
                                                                 DialogLaunchCallback_pf DialogLaunchCallback,
                                                                 DialogDropCallback_pf   DialogDropCallback,
                                                                 ArbParam_t              ClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to be notified whenever the Tecplot engine needs to
 * know the physical display's dot pitch.
 *
 * @since
 *   11.0-0-407
 *
 * @param DotPitchCallback
 *   Dot pitch callback function.
 * @param ClientData
 *   Any client data that is needed by the registered dot pitch function.
 *
 * @sa TecEngInit
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngDotPitchRegisterCallback(DotPitchCallback_pf DotPitchCallback,
                                                        ArbParam_t          ClientData);
                                                                                                             

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to be notified whenever the Tecplot engine needs to
 * know the physical display's screen size.
 *
 * @since
 *   11.2-0-471
 *
 * @param ScreenSizeCallback_pf
 *   Screen size callback function.
 * @param ClientData
 *   Any client data that is needed by the registered dot pitch function.
 *
 * @sa TecEngInit
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngScreenSizeRegisterCallback(ScreenSizeCallback_pf ScreenSizeCallback,
                                                          ArbParam_t            ClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to be notified whenever the Tecplot engine needs to
 * know present a modal message dialog.
 *
 * @since
 *   11.0-0-415
 *
 * @param DialogMessageBoxCallback
 *   Dialog message box callback function. The dialog message box callback
 *   should not return until the dialog is dismissed.
 * @param ClientData
 *   Any client data that is needed by the registered dialog message box
 *   function.
 *
 * @sa TecEngInit
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngDialogMessageBoxRegisterCallback(DialogMessageBoxCallback_pf DialogMessageBoxCallback,
                                                                ArbParam_t                  ClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to be notified whenever the Tecplot engine needs to
 * prompt the user to input into a simple text field.
 *
 * @since
 *   14.2
 *
 * @param DialogGetSimpleText
 *   Dialog callback function. The dialog callback should not return until 
 *   the dialog is dismissed.
 * @param ClientData
 *   Any client data that is needed by the registered dialog message box
 *   function.
 *
 * @sa TecEngInit
 * @sa TecUtilDialogGetSimpleText
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngDialogGetSimpleTextRegisterCallback(DialogGetSimpleTextCallback_pf DialogGetSimpleText,
                                                                   ArbParam_t                     ClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to be notified whenever the Tecplot engine needs to
 * present a status line text.
 *
 * @since
 *   11.2-0-085
 *
 * @param StatusLineCallback
 *   Status line callback function.
 * @param ClientData
 *   Any client data that is needed by the registered dialog message box
 *   function.
 *
 * @sa TecEngPercentDoneRegisterCallback
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngStatusLineRegisterCallback(StatusLineCallback_pf StatusLineCallback,
                                                          ArbParam_t            ClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to be notified whenever the Tecplot engine have
 * new coordinates to display.
 *
 * @since
 *   14.2
 *
 * @param ShowRunningCoordinatesCallback
 *   Show running coordinates callback function.
 * @param ClientData
 *   Any client data that is needed by the registered function.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngShowRunningCoordinatesRegisterCallback(ShowRunningCoordinatesCallback_pf ShowRunningCoordinatesCallback,
                                                                      ArbParam_t                        ClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to be notified about the progress of an operation.
 *
 * @since
 *   11.2-0-098
 *
 * @param ProgressMonitorCallback
 *   Callback function that will be called with the an updated percentage of a progress.
 *
 * @param ProgressMonitorStartCallback
 *   Callback function that will be called when Tecplot Engine is about to perform an operation.
 *
 * @param ProgressMonitorFinishCallback
 *   Callback function that will be called when Tecplot Engine had finished performing an operation.
 *
 * @param ClientData
 *   Any client data that is needed by the registered dialog message box
 *   function.
 *
 * @sa TecEngStatusLineRegisterCallback
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngProgressMonitorRegisterCallback(ProgressMonitorCallback_pf       ProgressMonitorCallback,
                                                               ProgressMonitorStartCallback_pf  ProgressMonitorStartCallback,
                                                               ProgressMonitorFinishCallback_pf ProgressMonitorFinishCallback,
                                                               ArbParam_t                       RegistrationClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to handle requests for timers.
 *
 * @since
 *   12.0.1.5642
 *
 * @param TimerCallback
 *   Callback that will be called when Tecplot Engine has has requested an event timer to be created.
 *
 * @param ClientData
 *   Any client data that is needed by the registered timer function.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngTimerRegisterCallback(TimerCallback_pf TimerCallback,
                                                     ArbParam_t       RegistrationClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Starts a previously initialized Tecplot engine so that it is ready to use.
 *
 * @since
 *   11.0-0-397
 *
 * @return
 *   TRUE if startup was successful, FALSE otherwise.
 *
 * @sa TecEngInit
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecEngStartup(void);

/* - NO DOXYGEN COMMENT GENERATION -
 * Cleans up a previously initialized Tecplot engine on an abnormal shutdown.
 * The use of this function is mutually exclusive with TecEngInitiateShutdown()
 * and TecEngFinalizeShutdown() and should only be called for abnormal
 * shutdowns such as a crash or assertion failure or other abnormal exit
 * signal.
 *
 * The primary purpose of this function is to remove large temporary files
 * created by the Tecplot engine.
 *
 * @since
 *   11.0-0-421
 *
 * @return
 *   TRUE if Tecplot was able to cleanup the Tecplot engine's temporary
 *   resources.
 *
 * @sa TecEngInit, TecEngInitiateShutdown, TecEngFinalizeShutdown
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecEngCleanupOnAbnormalShutdown(void);

/* - NO DOXYGEN COMMENT GENERATION -
 * Notifies a previously initialize Tecplot engine that a shutdown is desired.
 * This function does not actually perform the shutdown but notifies the engine
 * and other state change listeners, such as add-ons, of the intent to
 * shutdown. Upon receipt of this shutdown request the Tecplot engine will
 * broadcast a StateChange_QuitTecplot message, a state change for which the
 * parent application must be listening.
 *
 * Note that add-ons and macros can independently initiate shutdown of the
 * Tecplot engine therefore the parent application is responsible for listing
 * to the StateChange_QuitTecplot message to perform the shutdown finalization
 * regardless if the parent application initiated the shutdown sequence or not.
 * See TecEngFinalizeShutdown() for details regarding shutdown finalization.
 *
 * @since
 *   11.0-0-424
 *
 * @param IsOk
 *    Indicates if the shutdown occurred because of an error condition, such
 *    as an error processing a macro file in batch mode, or for normal reasons
 *    from the parent application.
 *
 * @sa
 *   TecEngInit, TecUtilStateChangeAddCallback, TecUtilStateChangeAddCallbackX,
 *   TecEngFinalizeShutdown, TecEngCleanupOnAbnormalShutdown
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngInitiateShutdown(Boolean_t IsOk);

/* - NO DOXYGEN COMMENT GENERATION -
 * Finializes the shutting down of the Tecplot engine. The shutdown
 * finalization should only occur after the parent application or the Tecplot
 * engine has initiated a shutdown via TecEngInitiateShutdown(). The parent
 * application is notified of this initiation via its registered state change
 * listener. Upon receipt of a StateChange_QuitTecplot message the parent
 * application should post an event or signal itself in some way that it needs
 * to call TecEngFinalizeShutdown() after returning from the state changed
 * callback. Note that the parent application must not call
 * TecEngFinalizeShutdown() within the state change listener.
 *
 * @since
 *   11.0-0-424
 *
 * @sa
 *   TecEngInit, TecUtilStateChangeAddCallback, TecUtilStateChangeAddCallbackX,
 *   TecEngInitiateShutdown, TecEngCleanupOnAbnormalShutdown
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngFinalizeShutdown(void);

/* - NO DOXYGEN COMMENT GENERATION -
 * Processes Tecplot engine tasks and onidle callback tasks on idle. This
 * function should be called whenever the parent application's event queue
 * becomes empty.
 *
 * @since
 *   11.0-0-405
 *
 * @sa TecEngInit
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngProcessTasksOnIdle(void);

/* - NO DOXYGEN COMMENT GENERATION -
 * Notifies the Tecplot engine that the work area of the page has been damaged
 * due to an exposure of a previously occluded portion of the page work area or
 * due to a resize of the page work area.
 *
 * @since
 *   11.0-0-409
 *
 * @param DoFullReset
 *   Indicates if the page work area needs to be fully reset. Passing TRUE
 *   notifies the Tecplot engine that an event occurred requiring a full reset
 *   of the page work area. This is usually due to resize event or a desire to
 *   rebuild the graphics cache. Passing FALSE will simply replay the graphics
 *   cache if it exists and is valid, otherwise the page will be fully redrawn.
 * @param UniqueID
 *   Unique ID of the page that has been damaged.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngPageDamaged(Boolean_t  DoFullReset,
                                           UniqueID_t UniqueID);

/**
 * Lock the Tecplot engine. For every call to TecUtilParentLockStart(), you must have
 * a matching call to TecUtilParentLockFinish(). A parent application using the
 * Tecplot engine must call TecUtilParentLockStart() and TecUtilParentLockFinish() instead of
 * the TecUtilLockOn(), and TecUtilLockOff(), or TecUtilLockStart() and
 * TecUtilLockFinish() which are reserved for add-ons.
 *
 * @param ShutdownImplicitRecording
 *     Indicates if implicit recording should be shutdown when the Tecplot Engine is performing
 *     recording.
 *
 * @sa TecUtilParentLockFinish()
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Lock
 *
 * #internalattributes exclude_all, exclude_tecplotdoc
 */
LINKTOADDON void STDCALL TecUtilParentLockStart(Boolean_t ShutdownImplicitRecording);

/**
 * Unlock the Tecplot engine. For every call to TecUtilParentLockStart(), you must
 * have a matching call to TecUtilParentLockFinish(). A parent application using the
 * Tecplot engine must call TecUtilParentLockStart() and TecUtilParentLockFinish() instead of
 * the TecUtilLockOn(), and TecUtilLockOff(), or TecUtilLockStart() and
 * TecUtilLockFinish() which are reserved for add-ons.
 *
 * @sa TecUtilParentLockStart()
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup Lock
 *
 * #internalattributes exclude_all, exclude_tecplotdoc
 */
LINKTOADDON void STDCALL TecUtilParentLockFinish(void);

#if defined DOCUMENT_BUT_DONT_COMPILE
/******************************************************************
 *                                                                *
 *                  FORTRAN CONVENIENCE FUNCTIONS                 *
 *                                                                *
 *            Code provided here is ONLY for purposes of          *
 *            Adding entries to the on-line reference.            *
 *                                                                *
 *            DO NOT USE THE "LINKTOADDON" QUALIFIER!             *
 *                                                                *
 ******************************************************************/


/**
 * Check to see if a file exists.
 *
 * This is mainly provided as a convenience for FORTRAN development.  This
 * function requires linking against the fglue library.
 *
 * @param FName
 *   String containing the filename.
 *
 * @return
 *   Returns TRUE if the file exists.  FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION FExtFileExists(
 *   &           FName)
 *    CHARACTER*(*)   FName
 * </FortranSyntax>
 *
 * FORTRAN Example:
 *
 * @code
 *       if (FExtFileExists('MyFile').Eq.TRUE) Then
 *C      .... do something with 'MyFile'
 *       Endif
 * @endcode
 *
 */
LgIndex_t STDCALL FExtFileExists(const char *FName);







/**
 * Get the version of the fglue library.
 *
 * This is mainly provided as a convenience for FORTRAN development.  This
 * function requires linking against the fglue library.
 *
 * @return
 *   Returns the version.  Current version is 2000.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION FExtGetFGlueVersion()
 * </FortranSyntax>
 *
 * FORTRAN Example:
 *
 * @code
 *       if (FExtGetFGlueVersion().Lt.2000) Then
 *C      ....take some action.
 *       Endif
 * @endcode
 */
LgIndex_t STDCALL FExtGetFGlueVersion(void);


/**
 * Get an integer from a C-String.  This is most often called from
 * callback functions generated by the tecplot gui builder where a C-string
 * is the parameter.
 *
 * This is mainly provided as a convenience for FORTRAN development.  This
 * function requires linking against the fglue library.
 *
 * @param S
 *    C-String to be parsed.
 *
 * @param IRetValue
 *    Returned Integer value produced by parsing S.
 *
 * @return
 *   Returns 0 if there was no error otherwise returns 1.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION FExtGetIntFromCString(
 *   &          S,
 *   &          IRetValue)
 *    INTEGER*1 S(1)
 *    INTEGER*4 IRetValue
 * </FortranSyntax>
 *
 * FORTRAN Example:
 *
 * @code
 *       if (FExtGetIntFromCString(S,IRetValue).Eq.0) Then
 *         write(*,*) 'S contained the integer value ',IRetValue
 *       Endif
 * @endcode
 */
LgIndex_t STDCALL FExtGetIntFromCString(const char *S,
                                        LgIndex_t  *IRetValue);








/**
 * Get a real*8 value from a C-String.  This is most often called from
 * callback functions generated by the tecplot gui builder where a C-string
 * is the parameter.
 *
 * This is mainly provided as a convenience for FORTRAN development.  This
 * function requires linking against the fglue library.
 *
 * @param S
 *    C-String to be parsed.
 *
 * @param RetValue
 *    Returned double precision value produced by parsing S.
 *
 * @return
 *   Returns 0 if there was no error otherwise returns 1.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION FExtGetReal8FromCString(
 *   &          S,
 *   &          RetValue)
 *    INTEGER*1 S(1)
 *    REAL*8    RetValue
 * </FortranSyntax>
 *
 * FORTRAN Example:
 *
 * @code
 *       REAL*8 RetValue
 *
 *       if (FExtGetReal8FromCString(S,RetValue).Eq.0) Then
 *         write(*,*) 'S contained the real value ',RetValue
 *       Endif
 * @endcode
 */
LgIndex_t STDCALL FExtGetReal8FromCString(const char *S,
                                          double     *RetValue);




/**
 * Given a pointer to a C-String, return a FORTRAN string.
 *
 * This is mainly provided as a convenience for FORTRAN development.  This
 * function requires linking against the fglue library.
 *
 * @param CStringPtr
 *   Pointer to a C-String
 *
 * @param FString
 *   Returned Fortran string.
 *
 * @param CStringLen
 *   Returned length of the original C-String.
 *
 * <FortranSyntax>
 *    SUBROUTINE FUNCTION FExtGetCharFromCStringPtr(
 *   &           CStringPtr,
 *   &           FString,
 *   &           CStringLen)
 *    POINTER       (CStringPtr,CString)
 *    INTEGER*1     CString
 *    CHARACTER*(*) FString
 *    INTEGER*4     CStringLen
 * </FortranSyntax>
 *
 * FORTRAN Example:
 * The following example shows a sub-routine that was registered as the state change
 * callback function. This example shows how to take the incoming state change
 * calldata parameter and extract a FORTRAN string (only allowed for style state
 * changes).
 *
 * @code
 *      SUBROUTINE MyCallback(StateChange, CallDataPtr)
 *      INCLUDE FGLUE.INC
 *      INTEGER*4 StateChange
 *      POINTER(CallDataPtr, CallData)
 *      CHARACTER*256 StyleString
 *      INTEGER*4 StyleStringLength
 *      if (StateChange .eq. STATECHANGE_STYLE) then
 *        if (CallDataPtr .ne. NULLPTR) then
 *          call FExtGetCharFromCStringPtr(CallDataPtr,
 *     &                                   StyleString,
 *     &                                   StyleStringLength)
 *          write (*,*) StyleString(1:StyleStringLength)
 *        endif
 *      endif
 *      end
 * @endcode
 */
void STDCALL FExtGetCharFromCStringPtr(const char      **CStringPtr,
                                       char             *FString,
                                       LgIndex_t        *CStringLen);

/**
 * Given a C-String, return a FORTRAN string.
 *
 * This is mainly provided as a convenience for FORTRAN development.  This
 * function requires linking against the fglue library.
 *
 * @param CString
 *   C-String to be converted.
 *
 * @param FString
 *   Returned Fortran string.
 *
 * @param CStringLen
 *   Returned length of the original C-String.
 *
 * <FortranSyntax>
 *    SUBROUTINE FUNCTION FExtGetCharFromCString(
 *   &           CString,
 *   &           FString,
 *   &           CStringLen)
 *    INTEGER*1     CString
 *    CHARACTER*(*) FString
 *    INTEGER*4     CStringLen
 * </FortranSyntax>
 *
 * FORTRAN Example:
 *
 * The following example shows the callback generated by the Tecplot GUI
 * Builder for a text field.
 *
 * @code
 *       INTEGER FUNCTION TF1_TF_D1_CB(S)
 * C
 * C...Treat the incoming "C-String" as an array of bytes..
 * C
 *       INTEGER*1 S(1)
 * C
 * C...S2 will hold the converted C String.
 * C
 *       CHARACTER*80 S2
 *       INTEGER*4 ILen
 *       INCLUDE FGLUE.INC
 *       INCLUDE ADDGLBL.INC
 *       INCLUDE GUIDEFS.INC
 *       INCLUDE GUI.INC.
 *       Call TecUtilLockStart(AddonID)
 * C
 * C... Convert the incoming "C-String" to a Fortran String
 * C
 *       call FExtGetCharFromCString(S,S2,ILen)
 *       write (*,*) 'CS = ',S2(1:ILen)
 *       TF1_TF_D1_CB = 1
 *       Call TecUtilLockFinish()
 *       RETURN
 * @endcode
 */
void STDCALL FExtGetCharFromCString(const char        *CString,
                                    char              *FString,
                                    LgIndex_t         *CStringLen);
#endif



#if defined UNIXX
/**
 *
 * @ingroup DataServices
 *
 * #internalattributes exclude_alldoc
 */
LINKTOADDON void STDCALL TecUtilCheckActiveAllocs(void);
#endif

/**
 * Registers the script execution callback and client data with the
 * specified file extension. When Tecplot encounters a file with the given
 * extension it hands it off to the callback to execute.
 *
 * @since
 *     11.0-2-005
 *
 * @param FileExt
 *     File extension, such as "tcl" or "py", to associate with the script
 *     execution callback and client data.
 * @param ScriptLanguage
 *     Name of the scripting language. This name could be displayed in a
 *     Tecplot menu or dialog.
 * @param ScriptExecCallback
 *     Callback responsible for executing the script handed to it by
 *     Tecplot.
 * @param ClientData
 *     Any client data needed by the callback.
 *
 * @return
 *     TRUE if the script callback and client data were registered with the
 *     file extension, FALSE if the file extension is already registered with
 *     another callback.
 *
 *
 * @pre <em>ScriptLanguage</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * @sa ScriptExecCallback_pf
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ScriptSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilScriptExecRegisterCallback(const char            *FileExt,
                                                                const char            *ScriptLanguage,
                                                                ScriptExecCallback_pf  ScriptExecCallback,
                                                                ArbParam_t             ClientData);

/**
 * Executes the specified script file. Tecplot locates the script execute
 * callback based upon the file name extension.
 *
 * @since
 *     11.0-2-005
 *
 * @param FileName
 *     Script file name to execute.
 *
 * @return
 *     TRUE if the script was found and executed without errors, FALSE
 *     otherwise.
 *
 *
 * @pre <em>FileName</em>
 *   String must have a valid address and non-zero length.
 *
 *
 * @sa TecUtilScriptExecRegisterCallback
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ScriptSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilScriptExec(const char *FileName);


/**
 * Returns client data associated with a script processor.
 *
 * @since
 *     11.2-0-200
 *
 * @param ScriptExtension
 *     An extension for which the script processor was registered.
 *
 * @param ClientData
 *     A pointer to an ArbParam_t variable to receive the client data.
 *
 * @return
 *     Returns TRUE if the client data was successfully retrieved for the
 *     script extension. FALSE, if no script processor was registered
 *     for the given script extension.
 *
 *
 * @pre <em>ScriptExtension</em>
 *   String must have a valid address and non-zero length.
 *
 * @pre <em>ClientData</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * @sa TecUtilScriptExecRegisterCallback
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup ScriptSupport
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilScriptProcessorGetClientData(const char*        ScriptExtension,
                                                                  TP_OUT ArbParam_t* ClientData);

/**
 * Saves a view for a plot type (2D, 3D, etc.). The view can
 * later be restored using the return value. You must
 * call TecUtilViewDealloc() when you are finished with the
 * returned view object.
 * If the plot type is PlotType_Automatic, then the view cannot be restored.
 *
 * @since
 *    11.0-5-025
 *
 * @return
 *   Returns an abstract handle to a saved view which can be used
 *   later to restore the view.
 *
 * @code
 *  // Save the current view
 *  ViewState_pa ViewState = TecUtilViewGet();
 *  // ...do some other stuff which changes the view
 *  // Restore the saved view
 *  TecUtilViewSet(ViewState);
 *  TecUtilViewDealloc(&ViewState); // dealloc the object when finished
 * @endcode
 *
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMouseGetCurrentMode()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilViewSet, TecUtilViewGetPlotType, TecUtilViewDealloc
 *
 * @ingroup View
 *
 */
LINKTOADDON TP_GIVES ViewState_pa STDCALL TecUtilViewGet(void);


/**
 * Restores a saved view for a plot type (2D, 3D, etc.).
 *
 * @since
 *    11.0-5-025
 * @param ViewState
 *   ViewState abstract pointer returned from TecUtilViewGet
 *
 * @return
 *   Returns TRUE if the view was restored,
 *   FALSE if the view could not be restored because the
 *   plot type of the saved view did not match the current
 *   plot type. This function will also return FALSE is the
 *   plot type to restore is PlotType_Automatic or if the
 *   view could not be set for any reason.
 * @code
 *  // Save the current view
 *  ViewState_pa ViewState = TecUtilViewGet();
 *  // ...do some other stuff which changes the view
 *  // Restore the saved view
 *  TecUtilViewSet(ViewState);
 *  TecUtilViewDealloc(&ViewState); // dealloc the object when finished
 * @endcode
 *
 *
 * @pre <em>VALID_REF(ViewStatePA)</em>
 *   Pointer must be a valid address and non-NULL.
 * @pre Must have one or more frames.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewSet(ViewState)
 *    POINTER (ViewStatePtr, ViewState)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilViewGet, TecUtilViewGetPlotType, TecUtilViewDealloc
 *
 * @ingroup View
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewSet(ViewState_pa ViewState);


/**
 * Queries the plot type of a saved view object.
 *
 * @since
 *    11.0-5-025
 * @param ViewState
 *   ViewState abstract pointer returned from TecUtilViewGet
 *
 * @return
 *  Returns the plot type of a saved view.
 *
 * @code
 *  // Save the current view
 *  ViewState_pa ViewState = TecUtilViewGet();
 *  // ...do some other stuff which changes the view
 *  // Verify that the current view is the same type as
 *  // the saved view and restore the view
 *  if (TecUtilFrameGetPlotType() == TecUtilViewGetPlotType(ViewState) )
 *    TecUtilViewSet(ViewState);
 *  TecUtilViewDealloc(&ViewState); // dealloc the object when finished
 * @endcode
 *
 *
 * @pre <em>ViewState</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewGetPlotType(ViewState)
 *    POINTER (ViewStatePtr, ViewState)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilViewGet, TecUtilViewSet, TecUtilViewDealloc
 *
 * @ingroup View
 *
 */
LINKTOADDON PlotType_e STDCALL TecUtilViewGetPlotType(ViewState_pa ViewState);


/**
 * Deallocates a saved view returned by TecUtilViewGet and sets
 * the pointer to NULL
 *
 * @param ViewState
 *   Pointer to the ViewState to be deallocated.
 *
 *
 * @pre <em>ViewState</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>ViewState</em>
 *   Pointer must be a valid address or NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilViewDealloc(ViewState)
 *    POINTER (ViewStatePtr, ViewState)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *  // Save the current view
 *  ViewState_pa ViewState = TecUtilViewGet();
 *  // ...do some other stuff which changes the view
 *  // Verify that the current view is the same type as
 *  // the saved view and restore the view
 *  if (TecUtilFrameGetPlotType() == TecUtilViewGetPlotType(ViewState) )
 *    TecUtilViewSet(ViewState);
 *  TecUtilViewDealloc(&ViewState); // dealloc the object when finished
 * @endcode
 *
 * @sa TecUtilViewGet, TecUtilViewSet, TecUtilViewGetPlotType
 *
 * @ingroup View
 *
 */
LINKTOADDON void STDCALL TecUtilViewDealloc(TP_RECEIVES_GIVES ViewState_pa* ViewState);


/**
 * Opens a face-mapping assignment context. All face nodes and
 * elements must be delivered between the face map begin/end
 * assignment clause.
 *
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   This function should not be called in response to a custom face map load
 *   callback. For custom face map load callbacks Tecplot is responsible for
 *   allocating the face map, beginning an assignment context and ending it
 *   after the loader callback returns. The callback only need deliver face map
 *   information to Tecplot with calls to TecUtilDataFaceMapAssignBConns(),
 *   TecUtilDataFaceMapAssignElems(), TecUtilDataFaceMapAssignNodes() or 
 *   TecUtilDataFaceMapAssignElemToNodeMap().
 *
 * @since
 *     11.2-1-0
 *
 * @param FaceMap
 *     Face mapping to which face nodes and elements are assigned.
 *
 * @pre Current frame must have a data set with at least one zone.
 *
 * @pre <em>FaceMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataFaceMapBeginAssign (
 *   &           FaceMapPtr)
 *    POINTER   (FaceMapPtr, FaceMap)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON void STDCALL TecUtilDataFaceMapBeginAssign(FaceMap_pa FaceMap);


/**
 * Assigns the supplied face nodes to the open face-mapping
 * assignment context. The faces for the nodes are implicitly given
 * sequential face numbers. This function can deliver all the face
 * nodes at once or one or more faces at a time. Calls to this
 * routine may be intermingled with calls to
 * TecUtilDataFaceMapAssignElems. The only requirement is that the
 * nodes and elements be delivered in the same order.
 * That is, the nth face defined by TecUtilDataFaceMapAssignNodes
 * must correspond to the nth pair of neighboring elements
 * passed into TecUtilDataFaceMapAssignElems.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *     11.2-1-0
 *
 * @param FaceMap
 *     An open face-mapping assignment context to which the face
 *     nodes are assigned.
 * @param NumFaces
 *     The number of faces being delivered.
 * @param NumFaceNodes
 *     Array of face nodes counts dimensioned by NumFaces for
 *     ZoneType_FEPolyhedron, NULL for homogeneous polytope data such as
 *     ZoneType_FEPolygon where the number of nodes per face defined by the
 *     zone type.
 * @param FaceNodes
 *     Array of face nodes for the supplied faces dimensioned by
 *     the sum of the members of the NumFaceNodes array.
 *
 *
 * @pre <em>FaceMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>FaceNodes</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataFaceMapAssignNodes (
 *   &           FaceMapPtr,
 *   &           NumFaces,
 *   &           NumFaceNodes,
 *   &           FaceNodes)
 *    POINTER   (FaceMapPtr, FaceMap)
 *    INTEGER*4 NumFaces
 *    INTEGER*4 NumFaceNodes(*)
 *    INTEGER*4 FaceNodes(*)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON void STDCALL TecUtilDataFaceMapAssignNodes(FaceMap_pa       FaceMap,
                                                       LgIndex_t        NumFaces,
                                                       const LgIndex_t *NumFaceNodes,
                                                       const LgIndex_t *FaceNodes);


/**
 * Assigns the supplied face left and right elements to the
 * open face-mapping assignment context. The faces for the
 * elements are implicitly given sequential face numbers.
 * Calls to this routine may be intermingled with calls to
 * TecUtilDataFaceMapAssignNodes. The only requirement is that the
 * nodes and elements be delivered in the same order.
 * That is, the nth face defined by TecUtilDataFaceMapAssignNodes
 * must correspond to the nth pair of neighboring elements
 * passed into TecUtilDataFaceMapAssignElems.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *     11.2-1-0
 *
 * @param FaceMap
 *     An open face-mapping assignment context to which the face
 *     left and right elements are assigned.
 * @param NumFaces
 *     The number of faces being delivered.
 * @param FaceLeftElems
 *     Array of face left elements for the supplied faces
 *     dimensioned by NumFaces.
 * @param FaceRightElems
 *     Array of face right elements for the supplied faces
 *     dimensioned by NumFaces.
 *
 *
 * @pre <em>FaceMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>FaceLeftElems</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>FaceRightElems</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataFaceMapAssignElems (
 *   &           FaceMapPtr,
 *   &           NumFaces,
 *   &           FaceLeftElems,
 *   &           FaceRightElems)
 *    POINTER   (FaceMapPtr, FaceMap)
 *    INTEGER*4 NumFaces
 *    INTEGER*4 FaceLeftElems(*)
 *    INTEGER*4 FaceRightElems(*)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON void STDCALL TecUtilDataFaceMapAssignElems(FaceMap_pa        FaceMap,
                                                       LgIndex_t         NumFaces,
                                                       const LgIndex_t  *FaceLeftElems,
                                                       const LgIndex_t  *FaceRightElems);




/**
 * Assigns the supplied boundary connected elements and zones
 * to the open face-mapping assignment context. The
 * boundary faces were assigned using a negative number for the
 * left or right element of the face.
 *
 * This function is \ref threadsafe.
 *
 * @since
 *     11.2-1-0
 *
 * @param FaceMap
 *     An open face-mapping assignment context to which the boundary
 *     face elements (and zones if appropriate) are assigned.
 * @param NumBndryFaces
 *     The number of boundary faces being delivered.
 * @param NumBndryConns
 *     Array of boundary connection counts dimensioned by
 *     NumBndryFaces.
 * @param FaceBndryElems
 *     Array of boundary connected elements for the supplied faces
 *     dimensioned by the sum of the members of the NumBndryConns
 *     array. If only a portion of a boundary face has adjacent
 *     elements, use TECUTIL_NO_NEIGHBORING_ELEM for the first
 *     neighboring element, and follow this with the actual elements.
 * @param FaceBndryElemZones
 *     Array of zone numbers indicating which zone each of the
 *     elements in the FaceBndryElems array resides in.
 *     It has the same dimension as the FaceBndryElems array.
 *     For boundary faces that are only partially bounded by
 *     neighboring elements (indicated by TECUTIL_NO_NEIGHBORING_ELEM
 *     in the FaceBndryElems array), use TECUTIL_NO_NEIGHBORING_ZONE
 *     for the zone numbers corresponding to the 0-th entry in
 *     FaceBndryElems. Also use TECUTIL_NO_NEIGHBORING_ZONE to indicate
 *     the boundary element is in the current zone (do not use
 *     the current zone number).
 *
 *
 * @pre <em>FaceMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>NumBndryConns</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>FaceBndryElems</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>FaceBndryElemZones</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataFaceMapAssignBConns (
 *   &           FaceMapPtr,
 *   &           NumBndryFaces,
 *   &           NumBndryConns,
 *   &           FaceBndryElems,
 *   &           FaceBndryElemZones)
 *    POINTER   (FaceMapPtr, FaceMap)
 *    INTEGER*4 NumBndryFaces
 *    INTEGER*4 NumBndryConns(*)
 *    INTEGER*4 FaceBndryElems(*)
 *    INTEGER*4 FaceBndryElemZones(*)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON void STDCALL TecUtilDataFaceMapAssignBConns(FaceMap_pa        FaceMap,
                                                        LgIndex_t         NumBndryFaces,
                                                        const LgIndex_t  *NumBndryConns,
                                                        const LgIndex_t  *FaceBndryElems,
                                                        const EntIndex_t *FaceBndryElemZones);


/**
 * Ends a face-mapping assignment sequence. All face nodes and
 * elements must be delivered between the face map begin/end
 * assignment clause.
 *
 * This function is \ref threadsafe.
 *
 * @par Note:
 *   This function should not be called in response to a custom face map load
 *   callback. For custom face map load callbacks Tecplot is responsible for
 *   allocating the face map, beginning an assignment context and ending it
 *   after the loader callback returns. The callback only need deliver face map
 *   information to Tecplot with calls to TecUtilDataFaceMapAssignBConns(),
 *   TecUtilDataFaceMapAssignElems(), TecUtilDataFaceMapAssignNodes() or 
 *   TecUtilDataFaceMapAssignElemToNodeMap().
 *
 * @since
 *     11.2-1-0
 *
 * @param FaceMap
 *     An open face-mapping assignment context to which
 *     the face nodes and elements were assigned.
 *
 * @return
 *     TRUE if all face nodes and elements were supplied and the supplied face
 *     nodes and left and right elements are within the range of the zone
 *     structure, FALSE otherwise.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataFaceMapEndAssign (
 *   &           FaceMapPtr)
 *    POINTER   (FaceMapPtr, FaceMap)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataFaceMapEndAssign(FaceMap_pa FaceMap);

/**
 * Return the custom load-on-demand client data from a face map handle. The
 * client data should ONLY be retrieved in response to a custom load, unload,
 * or cleanup callback. At no other time is the request valid.
 * This function is \ref threadsafe.
 *
 * @param FaceMap
 *     Custom load-on-demand face map handle.
 *
 * @return
 *     Client data for the custom load-on-demand add-on.
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 *     Boolean_t STDCALL MyFaceMapLoader(FaceMap_pa FaceMap)
 *     {
 *       Boolean_t Result;
 *       MyClientData_s *MyClientData = (MyClientData_s *)TecUtilDataFaceMapGetClientData(FaceMap);
 *
 *       // load the custom face map using client data
 *       .
 *       .
 *       .
 *
 *     return Result;
 *   }
 * @endcode
 *
 * @sa TecUtilDataFaceMapCustomLOD()
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON ArbParam_t STDCALL TecUtilDataFaceMapGetClientData(FaceMap_pa FaceMap);

/**
 * Registers with Tecplot the load-on-demand callbacks and client data for a
 * face mapping for a specific zone. Tecplot will notify the add-on via the callbacks
 * when the face mapping needs to be loaded, unloaded, and cleaned up.
 *
 * All callbacks must be written in a thread-safe manner so that Tecplot can
 * make concurrent requests to load (and/or unload) multiple face mappings. The
 * easiest way to write thread-safe callbacks is not to use any shared state
 * (i.e. global or static state) in order to perform the requested action but
 * instead to use private client data to maintain all the information needed to
 * perform the requested action.
 *
 * Calls made back to Tecplot in response to a load, unload, or cleanup request
 * should be limited to queries except in the case where data is being loaded
 * into a face mapping. In addition, no state changes should be broadcast by the
 * callbacks.
 *
 * The method for loading and accessing face map data with custom load-on-demand is
 * similar to custom load-on-demand for field data (see ADK Users Manual for details):
 * The add-on supplied LoadCallback() callback is responsible for loading the entire
 * face map data into the Tecplot prepared face map backing.  Tecplot is responsible
 * for allocating and freeing the space for the face map backing.  In addition, the
 * add-on must supply the CleanupCallback() callback to receive notification of when
 * the face map is no longer needed.  Optionally, the add-on may supply the UnloadCallback()
 * callback to receive notification of when the face map is being unloaded.  Most add-ons
 * should supply NULL for the UnloadCallback() callback, instructing Tecplot to assume
 * responsibility for unloading the face map and re-loading it in an efficient form.
 *
 * @par Note:
 *     If using deferred assignment by calling TecUtilDataFaceMapAssignElemToNodeMap(),
 *     then NumFaces and NumFaceNodes should both be 0 since the number of unique faces
 *     and the number of faces nodes for them is determined by Tecplot.
 *
 * @since
 *     11.2-1-0
 *
 * @param Zone
 *   Zone for which the face map will now be custom load-on-demand.
 *
 * @param NumFaces
 *   Total number of unique faces.
 *
 * @param NumFaceNodes
 *   Total number of nodes for all faces. This is not the number of unique
 *   nodes but the total number. For example if a face map defined two
 *   triangles polygons that share a common face, NumFaces would be 5
 *   and NumFaceNodes would be 6, not 4.
 *
 * @param NumFaceBndryFaces
 *   Total number of faces boundary faces.
 *
 * @param NumFaceBndryConns
 *   Total number of boundary connected elements or element/zone pairs.
 *
 * @param LoadCallback
 *   Tecplot calls this callback when the face map is to be loaded. The
 *   LoadCallback() callback may never get called if the face map is not needed
 *   or it may get called immediately if load-on-demand capabilities are not
 *   available. This callback is called asynchronously.
 *
 * @param UnloadCallback
 *   Add-ons can supply NULL for this callback. Supplying NULL instructs Tecplot to handle the
 *   unloading (and subsequent reloading) of the face map without the intervention of the add-on,
 *   however Tecplot will be forced to write the data to its temporary directory when unloaded
 *   thereby incurring additional I/O expense. If the add-on does supply this callback, Tecplot
 *   calls it when the face map is to be unloaded. This query provides the add-on an opportunity to
 *   allow or deny a face map to be unloaded by returning TRUE or FALSE respectively. Unless there
 *   is a compelling reason, such as very expensive load costs (in which case NULL should probably
 *   be supplied for this callback), the add-on should honor Tecplot's request to unload the face
 *   map (i.e. the UnloadCallback() callback should return TRUE). An add-on may also cleanup any
 *   private resources that are not needed when the face map is unloaded, however the add-on must
 *   still maintain enough information to load the face map again if requested by Tecplot. The
 *   UnloadCallback() callback may never get called if the face map does not need to be unloaded nor
 *   will the UnloadCallback() callback necessarily be called before the CleanupCallback() callback.
 *   This callback is called asynchronously.
 *
 * @param CleanupCallback
 *   Tecplot calls this callback when the face map is to be cleaned up. This
 *   allows the add-on to cleanup any private resources that were used in
 *   conjunction with identifying or loading this face map. After a face map is
 *   cleaned up Tecplot will never again request it to be loaded. Tecplot may
 *   or may not call the UnloadCallback() callback before calling the
 *   CleanupCallback() callback. Additionally, the CleanupCallback() callback
 *   will be called even if the face map was never loaded. This callback is called
 *   asynchronously.
 *
 * @param ClientData
 *   Private client data needed by the custom load-on-demand callbacks to
 *   perform the duties of loading, unloading, and cleaning up the face map.
 *   Tecplot stores the client data in the face map structure and must be
 *   retrieved by the callbacks using TecUtilDataFaceMapGetClientData(). The
 *   client data should ONLY be retrieved in response to a custom load,
 *   unload, or cleanup callback. At no other time is the request valid.
 *
 * @return
 *   TRUE if successful, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * Following is an example of how to create a face map using the Custom Load
 * on Demand.
 * @code
 *   typedef struct
 *     {
 *       char      *DataFileName;
 *       long       SeekOffset;
 *       ... other information needed to load face map data
 *     } MyFaceMapClientData_s;
 *
 *   Boolean_t STDCALL MyFaceMapLoader(FaceMap_pa FaceMap)
 *   {
 *     REQUIRE(VALID_REF(FaceMap));
 *
 *     MyFaceMapClientData_s *MyClientData = (MyFaceMapClientData_s *)TecUtilDataFaceMapGetClientData(FaceMap);
 *
 *     // open the data file
 *     FILE *MyDataFile = fopen(MyClientData->DataFileName, "rb");
 *     Boolean_t IsOk = (MyDataFile != NULL);
 *
 *     // seek to the place in the file where the face map data is located
 *     IsOk = IsOk && (fseek(MyDataFile, MyClientData->SeekOffset, SEEK_SET) == 0);
 *     if (IsOk)
 *       {
 *         // load the data into the zone's face map
 *         IsOk = ReadMyFaceMapDataIntoZone(MyDataFile, MyClientData, FaceMap);
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
 *   Boolean_t STDCALL MyFaceMapUnload(FaceMap_pa FaceMap)
 *   {
 *     REQUIRE(VALID_REF(FaceMap));
 *
 *     // We don't have any private data to cleanup (i.e. in addition to the
 *     // private client data which we don't cleanup here) so all we have to do
 *     // is return TRUE or FALSE letting Tecplot know that it can or can not
 *     // unload the face map.
 *     Boolean_t Result = TRUE; // ...tell Tecplot to go ahead and unload the face map
 *
 *     ENSURE(VALID_BOOLEAN(Result));
 *     return Result;
 *   }
 *
 *   void STDCALL MyFaceMapCleanup(FaceMap_pa FaceMap)
 *   {
 *     REQUIRE(VALID_REF(FaceMap));
 *
 *     MyFaceMapClientData_s *MyClientData = (MyFaceMapClientData_s *)TecUtilDataFaceMapGetClientData(FaceMap);
 *
 *     // cleanup privately allocated resources
 *     free(MyClientData->DataFileName);
 *     free(MyClientData);
 *   }
 *
 *   .
 *   .
 *   .
 *   LgIndex_t NumFaces          = ... // number of unique faces
 *   LgIndex_t NumFaceNodes      = ... // total number of face nodes
 *   LgIndex_t NumFaceBndryFaces = ... // total number of boundary faces
 *   LgIndex_t NumFaceBndryConns = ... // total number of boundary face Connections
 *   MyFaceMapClientData_s *MyClientData = (MyFaceMapClientData_s *)malloc(sizeof(MyFaceMapClientData_s));
 *   const char *MyDataFileName = "MyDataFileName.dat";
 *   MyClientData->MyDataFileName = (char *)malloc(strlen(MyDataFileName)+1);
 *   strcpy(MyClientData->MyDataFileName, MyDataFileName);
 *   MyClientData->SeekOffset = ... determined somewhere else
 *   ... initialize any other client data information needed to load face map data
 *   IsOk = TecUtilDataFaceMapCustomLOD(3,
 *                                      NumFaces,
 *                                      NumFaceNodes,
 *                                      NumFaceBndryFaces,
 *                                      NumFaceBndryConns,
 *                                      MyFaceMapLoader,
 *                                      MyFaceMapUnload, // most add-ons should pass NULL instead of MyFaceMapUnload
 *                                      MyFaceMapCleanup,
 *                                      (ArbParam_t)MyClientData);
 * @endcode
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @sa TecUtilDataFaceMapAlloc(), TecUtilDataConnectShare(),
 *     TecUtilDataFaceMapBeginAssign(), TecUtilDataFaceMapAssignNodes(),
 *     TecUtilDataFaceMapAssignZones(), TecUtilDataFaceMapEndAssign(),
 *     TecUtilDataFaceMapAssignElemToNodeMap()
 *
 * @ingroup PolyhedralData
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataFaceMapCustomLOD(EntIndex_t                    Zone,
                                                          LgIndex_t                     NumFaces,
                                                          LgIndex_t                     NumFaceNodes,
                                                          LgIndex_t                     NumFaceBndryFaces,
                                                          LgIndex_t                     NumFaceBndryConns,
                                                          LoadOnDemandFaceMapLoad_pf    LoadCallback,
                                                          LoadOnDemandFaceMapUnload_pf  UnloadCallback,
                                                          LoadOnDemandFaceMapCleanup_pf CleanupCallback,
                                                          ArbParam_t                    ClientData);
/**
 * Allocates the space needed for the face mapping.
 *
 * @since
 *     11.2-1-0
 *
 * @param Zone
 *   The zone needing the face mapping allocated.
 *
 * @param NumFaces
 *   Total number of unique faces.
 *
 * @param NumFaceNodes
 *   Total number of nodes for all faces. This is not the number of unique
 *   nodes but the total number. For example if a face map defined two
 *   triangles polygons that share a common face, NumFaces would be 5
 *   and NumFaceNodes would be 6, not 4.
 *
 * @param NumFaceBndryFaces
 *   Total number of faces boundary faces.
 *
 * @param NumFaceBndryConns
 *   Total number of boundary face elements or boundary face element/zone pairs.
 *
 * @return
 *   TRUE if the face mapping was successfully allocated, FALSE otherwise.
 *
 * @pre Must have one or more frames.
 * @pre Current frame must have a data set with at least one zone.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataFaceMapAlloc(
 *   &                   Zone,
 *   &                   NumFaces,
 *   &                   NumFaceNodes,
 *   &                   NumFaceBndryFaces,
 *   &                   NumFaceBndryConns)
 *    INTEGER*4       Zone
 *    INTEGER*4       NumFaces
 *    INTEGER*4       NumFaceNodes
 *    INTEGER*4       NumFaceBndryFaces
 *    INTEGER*4       NumFaceBndryConns
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Allocate the face map of zone 3.
 *
 * @code
 *   IsOk = TecUtilDataFaceMapAlloc(3,
 *                                  NumFaces, NumFaceNodes,
 *                                  NumFaceBndryFaces, NumFaceBndryConns);
 * @endcode
 *
 * @sa TecUtilDataConnectShare(), TecUtilDataFaceMapCustomLOD()
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataFaceMapAlloc(EntIndex_t Zone,
                                                      LgIndex_t  NumFaces,
                                                      LgIndex_t  NumFaceNodes,
                                                      LgIndex_t  NumFaceBndryFaces,
                                                      LgIndex_t  NumFaceBndryConns);

/**
 * Assigns the nodes of each element to a polytope zone without having to 
 * provide the nodes and adjacent elements of each unique face, generating the
 * required connectivity information.  The assignment must be made for all
 * elements of the zone at once.  This function is used inside a face map load
 * callback specified by TecUtilDataFaceMapCustomLOD().
 *
 * If the number of faces in the zone (KMax) was not specified when the zone was
 * created (see TecUtilDataSetAddZoneX()), this function will count the faces
 * and set the target zone's KMax as a side effect. 
 *  
 * This function is \ref threadsafe.
 *
 * @par Note:
 *     This function is mutually exclusive with calls to TecUtilDataFaceMapAssignNodes(),
 *     TecUtilDataFaceMapAssignElems() and TecUtilDataFaceMapAssignBConns().  These
 *     functions should be preferred if the nodes and adjacent element information is
 *     readily available.
 *
 * @since
 *     12.1.1.7895
 *
 * @param faceMap
 *     An open face-mapping assignment context that uses the element to node 
 *     map to determine the nodes and elements for each unique face.
 *
 * @param numElements
 *     The number of elements being represented by the accompanying arrays.  This 
 *     value must be the number of elements for the zone.
 *
 * @param facesPerElem
 *     Array of the number of faces contained in each element.
 *
 * @param nodesPerFace
 *     Array of the number of nodes contained in each face for each element.  
 *     NULL for homogeneous polytope zones such as FEPolygon zones where the 
 *     number of nodes per face is defined by the zone type.
 *
 * @param elemToNodeMap
 *     Array of one based nodes for each polygonal element or the nodes for
 *     each face of each polyhedral element.  
 *
 *
 * @pre <em>faceMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>facesPerElem</em>
 *   Pointer must be a valid address and non-NULL.
 *
 * @pre <em>elemToNodeMap</em>
 *   Pointer must be a valid address and non-NULL.
 *
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataFaceMapAssignElemToNodeMap (
 *   &           faceMapPtr,
 *   &           numElements,
 *   &           facesPerElem,
 *   &           nodesPerFace,
 *   &           elemToFaceMap)
 *    POINTER   (faceMapPtr, faceMap)
 *    INTEGER*4 numElements
 *    INTEGER*4 facesPerElem(*)
 *    INTEGER*4 nodesPerFace(*)
 *    INTEGER*4 elemToFaceMap(*)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup PolyhedralData
 *
 */
LINKTOADDON void STDCALL TecUtilDataFaceMapAssignElemToNodeMap(FaceMap_pa       faceMap,
                                                               LgIndex_t        numElements,
                                                               const LgIndex_t* facesPerElem,
                                                               const LgIndex_t* nodesPerFace,
                                                               const LgIndex_t* elemToNodeMap);
/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback called whenever a page is created.
 *
 * @since
 *   11.0-5-17
 *
 * @deprecated
 *   Use TecAppPolarCacheGetInnerGridRadiusAndDelta
 *
 * @param InnerGridRadius
 *   receives inner grid radius from polar cache
 * @param GridRadiusRangeDelta
 *   receives grid radius range delta from polar cache
 *
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngPolarCacheGetInnerGridRadiusAndDelta(TP_OUT double* InnerGridRadius,
                                                                    TP_OUT double* GridRadiusRangeDelta);

/**
 * This function is intended to be used only be used by SDK parent applications.
 * Registers a callback called whenever a page is created.
 *
 * @since
 *   14.1-0-24645
 *
 * @param InnerGridRadius
 *   receives inner grid radius from polar cache
 * @param GridRadiusRangeDelta
 *   receives grid radius range delta from polar cache
 *
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecAppPolarCacheGetInnerGridRadiusAndDelta(TP_OUT double* InnerGridRadius,
                                                                    TP_OUT double* GridRadiusRangeDelta);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback called whenever a page is created.
 *
 * @deprecated
 *   Use TecAppThetaAxisIsCompleteCycle
 *
 * @since
 *   11.0-5-17
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecEngThetaAxisIsCompleteCycle(void);

/**
 * This function is intended to be used only be used by SDK parent applications.
 * Registers a callback called whenever a page is created.
 *
 * @since
 *   14.1-0-24645
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppThetaAxisIsCompleteCycle(void);

/* - NO DOXYGEN COMMENT GENERATION -
 * Allow/disallow implicit recording of TecUtil function calls.
 *
 * @deprecated
 *   Please use TecAppMacroAllowImplicitRecord
 *
 * @since
 *   12.0.1.5149
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngMacroAllowImplicitRecord(Boolean_t Allow);

/**
 * This function is intended to be used only be used by SDK parent applications.
 * Allow/disallow implicit recording of TecUtil function calls.
 *
 * @since
 *   14.1-0-24645
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecAppMacroAllowImplicitRecord(Boolean_t Allow);

/* - NO DOXYGEN COMMENT GENERATION -
 * Query whether or not implicit recording of TecUtil function calls is allowed.
 *
 * @deprecated
 *   Use TecAppMacroImplicitRecordIsAllowed
 *
 * @since
 *   12.0.1.5149
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecEngMacroImplicitRecordIsAllowed(void);

/**
 * This function is intended to be used only be used by SDK parent applications.
 * Query whether or not implicit recording of TecUtil function calls is allowed.
 *
 * @since
 *   14.1-0-24645
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppMacroImplicitRecordIsAllowed(void);

/* - NO DOXYGEN COMMENT GENERATION -
 * Begin recording to the given filename.
 *
 * @deprecated
 *   Use TecAppMacroRecordStart
 *
 * @since
 *   12.0.1.5149
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecEngMacroRecordStart(const char* fileName);

/**
 * This function is intended to be used only be used by SDK parent applications.
 * Begin recording to the given filename.
 *
 * @since
 *   14.1-0-24645
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppMacroRecordStart(const char* fileName);

/* - NO DOXYGEN COMMENT GENERATION -
 * Stop the current macro recording session.
 *
 * @deprecated
 *   Use TecAppMacroRecordEnd
 *
 * @since
 *   12.0.1.5149
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngMacroRecordEnd(void);

/**
 * This function is intended to be used only be used by SDK parent applications.
 * Stop the current macro recording session.
 *
 * @since
 *   14.1-0-24645
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecAppMacroRecordEnd(void);

/* - NO DOXYGEN COMMENT GENERATION -
 * Stop the current macro recording session, and delete the file to which command have
 * been recorded.
 *
 * @deprecated
 *   Use TecAppMacroRecordCancel
 *
 * @since
 *   12.0.1.5149
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngMacroRecordCancel(void);

/**
 * This function is intended to be used only be used by SDK parent applications.
 * Stop the current macro recording session, and delete the file to which command have
 * been recorded.
 *
 * @since
 *   14.1-0-24645
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecAppMacroRecordCancel(void);

/* - NO DOXYGEN COMMENT GENERATION -
 * Query whether it is okay to begin recording a macro file via TecEngMacroRecordStart.
 *
 * @deprecated
LINKTOADDON Boolean_t STDCALL TecAppMacroRecordIsAllowed(void);
 *   Use TecAppMacroRecordIsAllowed
 * @since
 *   12.0.1.5149
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecEngMacroRecordIsAllowed(void);

/**
 * This function is intended to be used only be used by SDK parent applications.
 * Query whether it is okay to begin recording a macro file via TecAppMacroRecordStart.
 *
 * @since
 *   14.1-0-24645
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppMacroRecordIsAllowed(void);

/* - NO DOXYGEN COMMENT GENERATION -
* Set LicenseValidationCode global variable to InvalidLicenseCode.
* This will cause the engine to suspend drawing and other data services.
*
* @deprecated
*   Use TecAppSetLicenseIsValid
*
* @since
*   12.0.1.5149
*
* <PythonSyntax>
* </PythonSyntax>
*
* @ingroup TecEng
*
* #internalattributes exclude_all, exclude_alldoc
*/

LINKTOADDON void STDCALL TecEngSetLicenseIsValid(Boolean_t licenseIsValid);

/**
* This function is intended to be used only be used by SDK parent applications.
* Set LicenseValidationCode global variable to InvalidLicenseCode.
* This will cause the engine to suspend drawing and other data services.
*
* @since
*   14.1-0-24645
*
* <PythonSyntax>
* </PythonSyntax>
*
* @ingroup TecApp
*
* #internalattributes exclude_all, exclude_alldoc
*/

LINKTOADDON void STDCALL TecAppSetLicenseIsValid(Boolean_t licenseIsValid);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback to be notified whenever layout is opened or saved.
 * 
 * @since
 *   14.1
 *
 * @param MouseButtonStateCallback
 *   MRU Layout filename callback function.
 * @param ClientData
 *   Any client data that is needed by the registered key state function.
 *
 * @sa TecEngInit
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngMRULayoutFilenameRegisterCallback(MRULayoutFilenameCallback_pf MRULayoutFilenameCallback,
                                                                 ArbParam_t                   ClientData);

/* - NO DOXYGEN COMMENT GENERATION -
 * Registers a callback so the engine can query the parent's app OpenGL
 * version string.
 * 
 * @since
 *   14.1
 *
 * @param OpenGLVersionStringCallback
 *   The callback that will return the OpenGL version string.
 * @param ClientData
 *   Any client data that is needed by the registered function.
 *
 * @sa TecEngInit
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecEng
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecEngOpenGLVersionStringRegisterCallback(OpenGLVersionStringCallback_pf OpenGLVersionStringCallback,
                                                                   ArbParam_t                   ClientData);

/**
 * Get the number of registered foreign loaders (internal
 * tecplot loader is not included in the count).
 *
 * @return
 *     Returns the number of registered foreign loaders.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON int STDCALL TecAppImportGetNumRegisteredForeignLoaders(void);

/**
 * Get the name of a registered foreign loader.
 *
 * @param Loader
 *     The number of the registered foreign loader (1-based).
 *
 * @param Name
 *     A pointer to a character pointer with the name of the loader.  You must
 *     free this string with TecUtilStringDealloc().
 *
 * @return
 *     TRUE if the name was retrieved correctly, FALSE otherwise.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppImportGetForeignLoaderName(int             Loader,
                                                               TP_GIVES char** Name);

/**
 * Increments the engine's entry counter. This is only used by applications that are responsible for
 * calling registered callbacks on behalf of the Tecplot Engine such as foreign loaders. They must
 * restore the count after returning from the call.
 * @sa TecAppEngineEntryCountDecrement(), TecAppImportGetForeignLoaderGUILaunchCallback().
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecAppEngineEntryCountIncrement(void);

/**
 * Decrements the engine's entry counter. This is only used by applications that are responsible for
 * calling registered callbacks on behalf of the Tecplot Engine such as foreign loaders.
 * @sa TecAppEngineEntryCountIncrement(), TecAppImportGetForeignLoaderGUILaunchCallback().
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecAppEngineEntryCountDecrement(void);

/**
 * Get the registered loader GUI launch callback.
 *
 * @param Loader
 *     The number of the registered foreign loader (1-based).
 *
 * @return callback
 *     The function to call to show the loader GUI.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void* STDCALL TecAppImportGetForeignLoaderGUILaunchCallback(int Loader);

/**
 * Turns off drawing of pick handles. Turning off the drawing of pick handles does not have an
 * effect on Tecplot engine populating the pick list.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecAppPickHandlesDisableDrawing(void);

/**
 * Records a geom and all properties, given a geom Id.
 *
 * @since 14.1
 *
 * @param geomId
 *      geom identifier
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppRecordUpdateOfSingleGeom(Geom_ID geomId);

/**
 * Get a spaced separated string of file extensions used by the loaders. For example "*.dat *.plt".
 *
 * @param Loader
 *     The number of the registered foreign loader (1-based).
 *
 * @param Extensions
 *     A pointer to a character pointer with the loader extensions. You must
 *     free this string with TecUtilStringDealloc().
 *
 * @return
 *     TRUE if the loader extensions were retrieved correctly, FALSE otherwise.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppImportGetForeignLoaderExtensions(int             Loader,
                                                                     TP_GIVES char** Extensions);

/**
 * Returns if the loader handles multiple files at once for its loader instructions.
 *
 * @param Loader
 *     The number of the registered foreign loader (1-based).
 *
 * @return
 *     TRUE if the loader handles multiple files at once, FALSE otherwise.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppImportGetForeignLoaderCanHandleMultipleFileSelection(int Loader);

/**
 * Returns if the loader allows using advanced options.
 *
 * @param Loader
 *     The number of the registered foreign loader (1-based).
 *
 * @return
 *     TRUE if the loader allows advanced options, FALSE otherwise.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppImportGetForeignLoaderAllowAdvancedOptions(int Loader);

/**
 * Returns if the loader allows appending data to current data set.
 *
 * @param Loader
 *     The number of the registered foreign loader (1-based).
 *
 * @return
 *     TRUE if the loader allows appending, FALSE otherwise.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppImportGetForeignLoaderAllowAppending(int Loader);

/**
 * Returns the version of the family of registered loader callbacks.
 *
 * @param Loader
 *     The number of the registered foreign loader (1-based).
 *
 * @return
 *     Valid values are LoaderCallbackVersion_V1 for compatibility with DataSetLoader_pf,
 *     DynamicMenuCallback_pf, and DataSetLoaderInstructionOverride_pf family of registered loader
 *     functions and LoaderCallbackVersion_V2 for compatibility with DataLoader_pf,
 *     DataLoaderSelected_pf, and DataLoaderInstructionOverride_pf.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON LoaderCallbackVersion_e STDCALL TecAppImportGetForeignLoaderCallbackVersion(int Loader);

/**
 * Returns client data associated with a loader.
 *
 * @param Loader
 *     The number of the registered foreign loader (1-based).
 *
 * @param ClientData
 *     A pointer to an ArbParam_t variable to receive the client data.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON ArbParam_t* STDCALL TecAppImportGetForeignLoaderClientData(int Loader);

/**
 * Returns if the engine had perform a work at startup.
 * Mainly used by the parent application to test to see if it should launch
 * the welcome screen
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppWorkPerformedOnEngineStartup(void);

/**
 * Returns TRUE if any active slice is a clip plane
 * Used by the IsoSurface Style page relevance.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppAnyActiveSliceIsClipPlane(void);

/*
 * We are not adding any Doxygen documentation for this function on purpose 
 * because in the future we are going to move this functionality out of the
 * engine and into the parent app.
 */
LINKTOADDON LgIndex_t STDCALL TecAppAnimationGetNumSteps(void);

/*
 * We are not adding any Doxygen documentation for this function on purpose 
 * because in the future we are going to move this functionality out of the
 * engine and into the parent app.
 */
LINKTOADDON void STDCALL TecAppAnimationJumpToStep(AnimationStep_e animationStep, LgIndex_t delta);

/*
 * We are not adding any Doxygen documentation for this function on purpose 
 * because in the future we are going to move this functionality out of the
 * engine and into the parent app.
 */
LINKTOADDON double STDCALL TecAppAnimationGetSolutionTimeForTimeStep(LgIndex_t timeStep);

/*
 * We are not adding any Doxygen documentation for this function on purpose 
 * because in the future we are going to move this functionality out of the
 * engine and into the parent app.
 */
LINKTOADDON LgIndex_t STDCALL TecAppAnimationGetTimeStepForSolutionTime(double solutionTime);

/**
 * Call this function to propagate the frame linking settings for the current frame to other frames.
 *
 * @return
 *   TRUE if successful
 *
 * @param frameCollection
 *  Frames receiving the new settings. Choose between
 *  FrameCollection_All or FrameCollection_Picked (current set of picked frames).
 *
 * @param linkType
 *  link type within frames or between frames
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPropagateLinking(INTEGER4 frameCollection, INTEGER4 linkType)
 * </FortranSyntax>
 *
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup FrameLinking
 *
 * @since 14.0.3.0
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilPropagateLinking(LinkType_e linkType, FrameCollection_e frameCollection);

/**
 * Returns the list of font names the engine can handle.
 *
 * @param fontList
 *     A string list where the font name list will be deposited. The caller is
 *     responsible for deallocating the list once it is no longer needed.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecAppGetFontNameList(StringList_pa fontList);

/**
 * Returns the engine's default (fallback) font
 *
 * @param defaultFontFamily 
 *     A pointer to a string where the default font familty will be stored. The
 *     string must be freed by the caller once it's no longer needed.
 *
 * @since 14.1
 *
 * @ingroup TecApp 
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecAppGetDefaultFontFamily(char** defaultFontFamily);

/**
 * Loads an equation file. (*.equ)
 *
 * @since 14.2
 *
 * @param fileName
 *  File name to load. Must not be non-zero length string.
 *
 * @param equationLines
 *  Populated with lines which should be displayed in the equation list multi-line widget.
 *
 * @return
 *   TRUE if the equation file was successfully parsed.
 *   The equationLines parameter is populated with the lines which should be displayed in the 
 *   equation list multi-line widget.
 *
 */

LINKTOADDON Boolean_t TecAppLoadEquationFile(const char *fileName, StringList_pa equationLines);

/**
 * Writes an equation (.equ) file.
 *
 * @since 14.1
 *
 * @param fileName
 *  File Name, must not be NULL. Must be non-zero length.
 *
 * @param equationLines
 *  Equation lines
 *
 * @param enabledZones
 *  Set of Enabled Zones (curFrame->dataSet->EnabledZones
 *
 * @param baseZoneSet
 *  Set of Zones to alter
 *
 * @param iMin
 *  Index range I Min
 *
 * @param iMax
 *  Index range I Max
 *
 * @param iSkip
 *  Index range I Skip
 *
 * @param jMin
 *  Index range J Min
 *
 * @param jMax
 *  Index range J Max
 *
 * @param jSkip
 *  Index range J Skip
 * 
 * @param kMin
 *  Index range K Min
 *
 * @param kMax
 *  Index range K Max
 *
 * @param kSkip
 *  Index range K Skip
 *
 * @param baseValueLocation
 *  Value location
 *
 * @param ignoreDivideByZero
 *  Set to TRUE to ignore divide by zero errors
 *
 */
LINKTOADDON Boolean_t TecAppWriteEquationFile(
    const char*     fileName,
    const char*     equationLines,
    Set_pa          baseZoneSet,
    LgIndex_t       iMin,
    LgIndex_t       iMax,
    LgIndex_t       iSkip,
    LgIndex_t       jMin,
    LgIndex_t       jMax,
    LgIndex_t       jSkip,
    LgIndex_t       kMin,
    LgIndex_t       kMax,
    LgIndex_t       kSkip,
    FieldDataType_e baseDataType,
    ValueLocation_e baseValueLocation,
    Boolean_t       ignoreDivideByZero);

/**
 * Sets the current slice group in the current frame's scratch variables.
 *
 * @param sliceGroup
 *     The new slice group.
 *
 * @since 14.1
 *
 * @ingroup TecApp 
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecAppUISliceGroupSet(SmInteger_t sliceGroup);

/**
 * Returns the current slice group for the current frame
 *
 * @return
 *     The current slice group.
 *
 * @since 14.1
 *
 * @ingroup TecApp 
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON SmInteger_t STDCALL TecAppUISliceGroupGet(void);

/**
 * Returns whether there are any active zones with a surfaces to plot
 * value other than SurfacesToPlot_None
 *
 * @return
 *     TRUE if there are any active zones with SurfacesToPlot value other
 *     than SurfacesToPlot_None, FALSE otherwise.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppPlotContainsSurfacesToPlot(void);

/**
 * Builds and returns a line map name using the provided format string
 *
 * @return
 *     TRUE if the line map name was built correctly, FALSE otherwise.
 *
 * @since 14.1
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppLineMapGetIntermediateName(EntIndex_t  map,
                                                               const char* format,
                                                               char**      name);

/**
 * Deposit DataLoadFinish instructions.
 *  
 * @param ArgList
 *   Set of ArgList entries. This is built using calls to
 *   TecUtilArListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_READDATAOPTION
 * Type:
 *   ReadDataOption_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   None
 * Required:
 *   Yes
 * Notes:
 *   Determine how to handle the situation where a data set already exists in the current frame.
 *   The possible values are: ReadDataOption_NewData, ReadDataOption_AppendData and
 *   ReadDataOption_ReplaceData.
 *   Set to ReadDataOption_AppendData to append the new zones to the zones in the 
 *   data set that existed prior to using this command. Set to ReadDataOption_NewData 
 *   to remove the data set from the active frame prior to reading in 
 *   the new data set. If other frames use the same data set they will 
 *   continue to use the old one. Set to ReadDataOption_ReplaceData to replace the 
 *   data set attached to the active frame and to all other frames 
 *   that use the same data set, with the new data set.
 *
 * </ArgListTable>
 *
 * <FortranSyntax>
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 * @since 14.2.0.0
 */

 LINKTOADDON void STDCALL TecAppDepositDataLoadOptionsX(ArgList_pa argList);

 /* - NO DOXYGEN COMMENT GENERATION -
  * 
  * Starts diagnostic macro recording.
  *
  * @param macroFileName
  *   Name of diagnostic macro record file.
  *
  * @param append
  *   True if appending to existing file.
  *
  * @param comment
  *   Information such as platform or machine configuration to add as comment at start of file.
  *
  * @result
  *  TRUE if background macro recording was successfully started, FALSE otherwise.
  *
  * @since
  *  14.1
  */
 LINKTOADDON Boolean_t STDCALL TecAppDiagnosticMacroRecordStart(const char* macroFileName, bool append, const char* comment);

/* - NO DOXYGEN COMMENT GENERATION -
 * Closes diagnostic macro recorder file.
 * Does nothing if diagnostic macro recording is not enabled.
 * This function is used by the 360 app to close the diagnostic
 * macro recorder in the event of crash.
 *
 * @param didCrash
 *  Pass TRUE if the app has crashed (that is, it's being called from an exception
 *  handler), FALSE otherwise.
 *
 * @since
 *   14.1
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecAppDiagnosticMacroRecordStop(Boolean_t didCrash, const char* comment);

/* - NO DOXYGEN COMMENT GENERATION -
 * Return true if diagnostic macro recording is enabled.
 *
 * @since
 *   14.1
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON Boolean_t STDCALL TecAppDiagnosticMacroRecordIsEnabled(void);

/* - NO DOXYGEN COMMENT GENERATION -
 * Invalidates undo state. This should only be used in situations where there is some
 * limitation in the tecplot engine where it cannot itself determine that the undo state is
 * invalid
 *
 * @since
 *   14.3
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TecApp
 *
 * #internalattributes exclude_all, exclude_alldoc
 */
LINKTOADDON void STDCALL TecAppInvalidateUndo(void);

#endif /* _TECUTILO_H */
