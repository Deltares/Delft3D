#include "TECADDON.h"
#include "tp360addonuiaction_Exports.h"

#if defined LINKTOADDON
  #undef LINKTOADDON
#endif

#define LINKTOADDON extern "C" tp360addonuiaction_API

/**
 * Launch a dialog to prompt the user for a file name.
 *
 * @par Note:
 *   This function cannot be called when Tecplot is running in batch mode.
 *
 * @param DialogOption
 *   Choose the mode of operation for the dialog. The possible values
 *   are: \ref SelectFileOption_ReadSingleFile (allows you to read a file).
 *   \ref SelectFileOption_WriteFile (allows you to bring up single file
 *   selection dialog to choose a file to write to).
 *
 * @param FileName
 *   Returns an allocated string containing the name of the file selected or
 *   NULL if one was not selected. If TecUtilDialogGetFileName() returns TRUE,
 *   you must call TecUtilStringDealloc() when it is no longer needed.
 *
 * @param FileTypeName
 *   A string describing the file type. Example: "Text file." Must not be NULL.
 *
 * @param DefaultFileName
 *   The initial file name. May be NULL. A full or partial path may be supplied
 *   to indicate the folder to be initially displayed in the browse dialog.   
 *   If a partial path is used, it is relative to the folder most recently used
 *   in a Tecplot file or folder browser dialog.
 *
 * @param DefaultFilter
 *   The default filter (that is, extension). Example: "*.txt." May be NULL
 *
 * @return
 *   TRUE if a file name was successfully entered, FALSE otherwise. FALSE
 *   usually indicates that Cancel on the dialog was clicked.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogGetFileName(
 *   &                   DialogOption,
 *   &                   FileName,
 *   &                   FileNameLength,
 *   &                   FileTypeName,
 *   &                   DefaultFileName,
 *   &                   DefaultFilter)
 *    INTEGER*4       DialogOption
 *    CHARACTER*(*)   FileName
 *    INTEGER*4       FileNameLength
 *    CHARACTER*(*)   FileTypeName
 *    CHARACTER*(*)   DefaultFileName
 *    CHARACTER*(*)   DefaultFilter
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Prompt the user for a single file to read:
 *
 * @code
 *   char *FileName;
 *   if (TecUtilDialogGetFileName(SelectFileOption_ReadSingleFile,
 *                                &FileName, "Text Files", "myfile.txt", "*.txt"))
 *     {
 *         .
 *         .
 *         .
 *         do something with FileName
 *         .
 *         .
 *         .
 *       // free Tecplot's copy
 *       TecUtilStringDealloc(&FileName);
 *     }
 * @endcode
 *
 * @sa TecUtilMacroIsBatchModeActive()
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogGetFileName(SelectFileOption_e dialogOption,
                                                       TP_GIVES char**    fileName,
                                                       const char*        fileTypeName,
                                                       const char*        defaultFileName,
                                                       const char*        defaultFilter);

/**
 * Launch a dialog to prompt the user for one or more file names. It is assumed
 * that the files selected will be opened only for reading. Use
 * TecUtilGetFileName() to open a file for writing.
 *
 * @par Note:
 *   This function cannot be called when Tecplot is running in batch mode.
 *
 * @param DialogOption
 *   Choose the mode of operation for the dialog. The possible values are:
 *   \ref SelectFileOption_ReadMultiFile (brings up the multi-file section
 *   dialog), \ref SelectFileOption_AllowMultiFileRead (brings up single file
 *   section to start with but includes a button the user can press to get a
 *   multi-file selection dialog)
 *
 * @param FileNames
 *   Returns an allocated string list containing the names of the files
 *   selected or NULL if none were selected. If TecUtilDialogGetFileNames()
 *   returns TRUE, you must call TecUtilStringDealloc() when it is no longer
 *   needed.
 *
 * @param FileTypeName
 *   A string describing the file type. Example: "Text file." Must not be NULL.
 *
 * @param DefaultFileNames
 *   A string list containing the default file name(s). May be NULL.
 *   Full or partial paths may be supplied to indicate the folder to be initially
 *   displayed in the browse dialog.  All the files should be in the same folder. 
 *   If a partial path is used, it is relative to the folder most recently used
 *   in a Tecplot file or folder browser dialog.  
 *
 * @param DefaultFilter
 *   The default filter (that is, extension). Example: "*.txt." May be NULL
 *
 * @return
 *   TRUE if a file name was successfully entered, FALSE otherwise. FALSE
 *   usually indicates that Cancel on the dialog was clicked.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogGetFileNames(
 *   &                   DialogOption,
 *   &                   FileNamesPtr,
 *   &                   FileTypeName,
 *   &                   DefaultFileNamesPtr,
 *   &                   DefaultFilter)
 *    INTEGER*4       DialogOption
 *    POINTER         (FileNamesPtr, FileNames)
 *    CHARACTER*(*)   FileTypeName
 *    POINTER         (DefaultFileNamesPtr, DefaultFileNames)
 *    CHARACTER*(*)   DefaultFilter
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Prompt the user for one or more files to read:
 *
 * @code
 *   StringList_pa FileNames;
 *   StringList_pa DefaultFileNames = TecUtilStringListAlloc();
 *
 *   TecUtilStringListAppendString(DefaultFileNames,"myfile.txt");
 *
 *   if (TecUtilDialogGetFileNames(SelectFileOption_ReadMultiFile,
 *                                 &FileNames,
 *                                 "Text Files",
 *                                 DefaultFileNames,
 *                                 "*.txt"))
 *     {
 *       // get the first file name
 *       char *FName = TecUtilStringListGetString(FileNames,1);
 *
 *       ... do something with FName...
 *
 *       TecUtilStringDealloc(&FName); // and free Tecplot's copy
 *
 *       .
 *       .
 *       .
 *       do some more things with FileNames
 *       .
 *       .
 *       .
 *       TecUtilStringListDealloc(FileNames);// done with file name list
 *     }
 *
 *   TecUtilStringListDealloc(&DefaultFileNames); // done with file name list
 * @endcode
 *
 * @sa TecUtilMacroIsBatchModeActive()
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogGetFileNames(SelectFileOption_e      dialogOption,
                                                        TP_GIVES StringList_pa* fileNames,
                                                        const char*             fileTypeName,
                                                        StringList_pa           defaultFileNames,
                                                        const char*             defaultFilter);

/**
 * Launch a dialog to prompt the user for one or more file names. It is assumed
 * that the files selected will be opened only for reading. Use
 * TecUtilGetFileName() to open a file for writing.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_FILESELECTDIALOGTYPE
 * Type:
 *   SelectFileOption_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   Choose the mode of operation for the dialog. The possible values are:
 *   \ref SelectFileOption_ReadSingleFile (allows you to read a file).
 *   \ref SelectFileOption_ReadMultiFile (brings up the multi-file section
 *   dialog), \ref SelectFileOption_AllowMultiFileRead (brings up single file
 *   section to start with but includes a button the user can press to get a
 *   multi-file selection dialog)
 *
 * Name:
 *   SV_FILESELECTDIALOGDEFAULTFILENAMES
 * Type:
 *   StringList_pa
 * Arg Function:
 *   TecUtilArgListAppendStringList()
 * Required:
 *   No
 * Default:
 *   NULL
 * Notes:
 *   Default file names to display in the dialog.  All files should be in the same
 *   folder. Full or partial paths may be supplied to indicate the folder to be 
 *   initially displayed. If a partial path is used, it is relative to the folder
 *   most recently used in a Tecplot file or folder browser dialog.  
 *
 * Name:
 *   SV_FILESELECTDIALOGFILTEREXTENSIONS
 * Type:
 *   StringList_pa
 * Arg Function:
 *   TecUtilArgListAppendStringList()
 * Required:
 *   No
 * Default:
 *   NULL
 * Notes:
 *   StringList contianing filter extensions.
 *
 * Name:
 *   SV_FILESELECTDIALOGFILTERTITLES
 * Type:
 *   StringList_pa
 * Arg Function:
 *   TecUtilArgListAppendStringList()
 * Required:
 *   No
 * Default:
 *   NULL
 * Notes:
 *   StringList contianing type of files corresponding to filter extensions in SV_FILESELECTDIALOGFILTEREXTENSIONS
 *   argument.
 *
 * Name:
 *   SV_FILESELECTDIALOGDEFAULTFILTERINDEX
 * Type:
 *   int
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   No
 * Default:
 *   1
 * Notes:
 *   Index of the filter to be selected by default.
 *
 * Name:
 *   SV_FILESELECTDIALOGRETURNFILENAMES
 * Type:
 *   StringList_pa *
 * Arg Function:
 *   TecUtilArgListAppendArbParamPtr()
 * Required:
 *   Yes
 * Notes:
 *   Pointer to the StringList_pa that will be populated with selected file names.
 *   If TecUtilDialogGetFileNamesX() returns TRUE, you must call
 *   TecUtilStringListDealloc() after using this parameter. If the return value
 *   is FALSE, FileNames is not changed and should not be deallocated.
 *
 * Name:
 *   SV_FILESELECTDIALOGRETURNFILTERINDEX
 * Type:
 *   int *
 * Arg Function:
 *   TecUtilArgListAppendArbParamPtr()
 * Required:
 *   No
 * Default:
 *   NULL
 * Notes:
 *  Returned index of the filer selected by the user.
 * </ArgListTable>
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogGetFileNamesX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 * StringList_pa  ReturnFiles       = NULL;
 * int            ReturnFilterIndex = 0;
 * bool           Result            = TRUE;
 * ArgList_pa     argList           = TecUtilArgListAlloc();
 *
 * TecUtilArgListAppendInt(argList, SV_FILESELECTDIALOGTYPE, SelectFileOption);
 * TecUtilArgListAppendStringList(argList, SV_FILESELECTDIALOGDEFAULTFILENAMES, DefaultFileNames);
 * TecUtilArgListAppendStringList(argList, SV_FILESELECTDIALOGFILTEREXTENSIONS, FilterExtensions);
 * TecUtilArgListAppendStringList(argList, SV_FILESELECTDIALOGFILTERTITLES, FilterTitles);
 * TecUtilArgListAppendInt(argList, SV_FILESELECTDIALOGDEFAULTFILTERINDEX, DefaultFilterIndex);
 * TecUtilArgListAppendArbParamPtr(argList, SV_FILESELECTDIALOGRETURNFILENAMES, (ArbParam_t *)ReturnFiles);
 * TecUtilArgListAppendArbParamPtr(argList, SV_FILESELECTDIALOGRETURNFILTERINDEX, (ArbParam_t *)ReturnFilterIndex);
 * Result = TecUtilDialogGetFileNamesX(argList);
 *
 * TecUtilArgListDealloc(&argList);
 * if (Result)
 *  TecUtilStringListDealloc(&ReturnFiles);
 * @endcode
 *
 * @since
 *    11.0-0-275
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogGetFileNamesX(ArgList_pa argList);

/**
 * Launch a dialog to prompt the user for a folder name.
 *
 * @param Title
 *    Zero-terminated string that is displayed above the tree view control in
 *    the dialog box. This string can be used to specify instructions to the
 *    user.
 *
 * @param FolderName
 *    Returns an allocated string containing the name of the folder selected or
 *    NULL if one was not selected. If TecUtilDialogGetFolderName() returns
 *    TRUE, you must call TecUtilStringDealloc() when it is no longer needed.
 *
 * @return
 *    TRUE if a folder name was successfully entered. FALSE if there was an
 *    error or if the user cancelled.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogGetFolderName(
 *   &                   Title,
 *   &                   FolderName)
 *    CHARACTER*(*)   Title
 *    CHARACTER*(*)   FileName
 *
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Prompt the user for a single folder to read:
 *
 * @code
 *   char *FolderName;
 *   if (TecUtilDialogGetFolderName("Please select a folder",
 *                                  &FolderName))
 *     {
 *         .
 *         .
 *         .
 *         do something with FolderName
 *         .
 *         .
 *         .
 *       // free Tecplot's copy
 *       TecUtilStringDealloc(&FolderName);
 *     }
 * @endcode
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogGetFolderName(const char*     Title,
                                                         TP_GIVES char** FolderName);

/**
 * Launch a dialog to prompt the user for a folder name.
 *
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_FILESELECTDIALOGRETURNFILENAMES
 * Type:
 *   char **
 * Arg Function:
 *   TecUtilArgListAppendArbParamPtr()
 * Required:
 *   Yes
 * Notes:
 *   Returns an allocated string containing the name of the folder selected or
 *   NULL if one was not selected. If TecUtilDialogGetFolderNameX() returns
 *   TRUE, you must call TecUtilStringDealloc() when it is no longer needed.
 *
 * Name:
 *   SV_DIALOGTITLE
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   Zero-terminated string that is displayed above the tree view control in
 *   the dialog box. This string can be used to specify instructions to the
 *   user.
 *
 * Name:
 *   SV_DEFAULTFNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   No
 * Default:
 *   NULL
 * Notes:
 *   If non-NULL it must be a zero-terminated string that specifies the default
 *   folder to select in the dialog. A full or partial path may be supplied.  
 *   If a partial path is used, it is relative to the folder most recently used
 *   in a Tecplot file or folder browser dialog.
 * </ArgListTable>
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogGetFolderNameX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @code
 * char*      FolderName        = NULL;
 * Boolean_t  FolderSelected    = FALSE;
 * ArgList_pa ArgList           = TecUtilArgListAlloc();
 *
 * TecUtilArgListAppendArbParamPtr(ArgList, SV_FILESELECTDIALOGRETURNFILENAMES, (ArbParam_t*)&FolderName);
 * TecUtilArgListAppendString     (ArgList, SV_DIALOGTITLE,   "Select a folder");
 * TecUtilArgListAppendString     (ArgList, SV_DEFAULTFNAME,  "/usr/data/");
 * FolderSelected = TecUtilDialogGetFileNamesX(ArgList);
 *
 * if (FolderSelected)
 *   {
 *       .
 *       .
 *       .
 *       do something with FolderName
 *       .
 *       .
 *       .
 *     // free the string Tecplot allocated
 *     TecUtilStringDealloc(&FolderName);
 *   }
 *
 * TecUtilArgListDealloc(&ArgList);
 * @endcode
 *
 * @since
 *    11.2-0-430
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogGetFolderNameX(ArgList_pa ArgList);

/**
 * Launch a dialog that prompts the user for one, two, or three variables.
 *
 * @par Note:
 *   This function cannot be called when Tecplot is running in batch mode.
 *
 * @param Instructions
 *   Character string displayed at the top of the dialog. Must not be NULL
 *
 * @param TextField1Label
 *   Character string displayed to the left of the first variable's selection
 *   menu. May be NULL
 *
 * @param TextField2Label
 *   Character string displayed to the left of the second variable's selection
 *   menu. Only used if Var2 is not NULL. May be NULL
 *
 * @param TextField3Label
 *   Character string displayed to the left of the third variable's selection
 *   menu. Only used if Var3 is not NULL. May be NULL.
 *
 * @param Var1
 *   Value passed in is the default value displayed in the first variable's
 *   selection menu. Must not be NULL, and must be a valid variable number.
 *   Upon return, this value holds the user-selected first variable value
 *
 * @param Var2
 *   Value passed in is the default value displayed in the second variable's
 *   selection menu. Use NULL to prompt for only one variable. May be NULL, but
 *   if not NULL, must be a valid variable number.  Upon return, this value
 *   holds the user-selected second variable value
 *
 * @param Var3
 *   Value passed in is the default value displayed in the third variable's
 *   selection menu. Use NULL to prompt for only one or two variables. May be
 *   NULL, but if not NULL, must be a valid variable number. Upon return, this
 *   value holds the user-selected third variable value.
 *
 * @return
 *   TRUE if successful, FALSE if not. FALSE usually indicates that Cancel on the dialog was clicked.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogGetVariables(
 *   &                   Instructions,
 *   &                   TextField1Label,
 *   &                   TextField2Label,
 *   &                   TextField3Label,
 *   &                   Var1,
 *   &                   Var2,
 *   &                   Var3)
 *    CHARACTER*(*)   Instructions
 *    CHARACTER*(*)   TextField1Label
 *    CHARACTER*(*)   TextField2Label
 *    CHARACTER*(*)   TextField3Label
 *    INTEGER*4       Var1
 *    INTEGER*4       Var2
 *    INTEGER*4       Var3
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Prompt the user for two variables:
 *
 * @code
 *   EntIndex_t Var1 = 1,Var2 = 1;
 *   if (TecUtilDialogGetVariables("Enter the values for V1 and V2:",
 *        "V1:", "V2:", NULL, &Var1, &Var2, NULL)
 *   {
 *     // values successfully entered
 *   }
 * @endcode
 *
 * FORTRAN Example.
 *
 * @code
 *   INTEGER*4 IErr
 *   INTEGER*4 Var1
 *   INTEGER*4 Var2
 *   POINTER   (NullPntr, Null)
 *   INTEGER*4 Null
 *
 *   NullPntr = 0
 *
 *   Call TecUtilLockStart(AddOnID)
 *
 *   Var1 = 1
 *   Var2 = 1
 *
 *   IErr = TecUtilDialogGetVariables(
 *  &       'Get some variables'//char(0),
 *  &       'label 1'//char(0),
 *  &       'label 2'//char(0),
 *  &       char(0),
 *  &       Var1,
 *  &       Var2,
 *  &       Null)
 *
 *  .... do something with Var1 and Var2 .....
 *
 *  Call TecUtilLockFinish(AddOnID)
 * @endcode
 *
 * @sa TecUtilMacroIsBatchModeActive()
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogGetVariables(const char*           Instructions,
                                                        const char*           TextField1Label,
                                                        const char*           TextField2Label,
                                                        const char*           TextField3Label,
                                                        TP_IN_OUT EntIndex_t* Var1,
                                                        TP_IN_OUT EntIndex_t* Var2,
                                                        TP_IN_OUT EntIndex_t* Var3);

/**
 * Launch a dialog that prompts the user for the minimum, maximum, and skip
 * values of a range.
 *
 * @par Note:
 *   This function cannot be called when Tecplot is running in batch mode.
 *
 * @param MaxRangeValue
 *   Maximum value for the range. Currently, this value is displayed at the top
 *   of the Enter Index Range dialog and also used to interpret zero and
 *   negative values for the other parameters, but it does not limit what
 *   values the user can enter. You must check the values of the other
 *   parameters upon return and check their validity in your code. A value of
 *   zero for MaxRangeValue will turn off the maximum value processing
 *
 * @param Min
 *   The value passed in is the default value displayed in the minimum text
 *   field. Upon return, this value holds the user-specified minimum value. If
 *   MaxRangeValue is not zero, a Min of zero is interpreted as MaxRangeValue,
 *   and negative values of Min are interpreted as subtracted from
 *   MaxRangeValue
 *
 * @param Max
 *   The value passed in is the default value displayed in the maximum text
 *   field. Upon return, this value holds the user-specified maximum value.
 *   Zero and negative values are interpreted as with Min above
 *
 * @param Skip
 *   The value passed in is the default value displayed in the skip text field.
 *   Upon return, this value holds the user-specified skip value. Zero and
 *   negative values are interpreted as with Min above
 *
 * @return
 *   TRUE if successful, FALSE if not. FALSE usually indicates that Cancel on
 *   the dialog was clicked.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogGetIndexRange(
 *   &                   MaxRangeValue,
 *   &                   Min,
 *   &                   Max,
 *   &                   Skip)
 *    INTEGER*4       MaxRangeValue
 *    INTEGER*4       Min
 *    INTEGER*4       Max
 *    INTEGER*4       Skip
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Launch a dialog that prompts the user for the minimum, maximum, and skip values of a range:
 *
 * @code
 *
 *   LgIndex_t Min = 0, Max = 100, Skip = 1;
 *   TecUtilDialogGetIndexRange(100,&Min,&Max,&Skip);
 *   // values returned in Min,Max, and skip
 * @endcode
 *
 * @sa TecUtilMacroIsBatchModeActive()
 *
 * @ingroup UserInterface
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogGetIndexRange(LgIndex_t         MaxRangeValue,
                                                         TP_OUT LgIndex_t* Min,
                                                         TP_OUT LgIndex_t* Max,
                                                         TP_OUT LgIndex_t* Skip);
