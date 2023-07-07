#pragma once

#include "tecgui_Exports.h"

#undef  LINKTOADDON /* Old style export */
#define LINKTOADDON EXTERNC tecgui_API /* New style export */

/*{{<exclude_fglue>
  TecGUIListGetSelectedItems
  TecGUIListSetSelectedItems
  TecGUIMFCSidebarRegister
  TecGUIMFCSidebarUpdateData
  TecGUICreateDialogBar
  TecGUIGetTPMainWnd
  TecGUIMFCAllocDialogBar_pf
  </exclude_fglue> }}*/


/*{{<exclude_engine> 
  TecGUIMFCSidebarRegister
  TecGUIMFCSidebarUpdateData
  TecGUIMFCAllocDialogBar_pf
</exclude_engine> }}*/

/*{{<windows_only> 
  TecGUIMFCSidebarRegister
  TecGUIMFCSidebarUpdateData
  TecGUIMFCAllocDialogBar_pf
</windows_only> }}*/

/*
 * Unfortunately "ENGINE" is defined in places where tecmock is used.
 * Defining engine must remove the TecGUI definitions from TECADDON.h 
 * to make building of libtecguiqt happy in RS.  Someday we need to 
 * unravel all of this so we can mock TecGUI as well.   Probably not
 * as important as mocking the rest of the TecUtil functions.  (bdp)
 */
/*{{<exclude_tecmock>
TecGUIDialogEnableActionArea
TecGUIDialogApplySetSensitivity
TecGUIDialogSetTopmost
TecGUIDialogSetPosition
TecGUIDialogSetLaunchPosition
TecGUIDialogSetPositionX
TecGUIDialogSetLaunchPositionX
TecGUIDialogCreateModeless
TecGUIDialogCreateModal
TecGUIBlockForModalDialog
TecGUIColoredButtonAdd
TecGUIButtonAdd
TecGUIColoredButtonSetColor
TecGUIBitmapButtonAdd
TecGUIBitmapToggleAdd
TecGUISetToolTip
TecGUISetStatusLine
TecGUIButtonSetDefault
TecGUIButtonSetText
TecGUISetSensitivity
TecGUISetVisibility
TecGUIOptionMenuAdd
TecGUIOptionMenuSet
TecGUIOptionMenuSetByString
TecGUIOptionMenuGet
TecGUIListAdd
TecGUIListGetItemCount
TecGUIListAppendItem
TecGUIListGetString
TecGUIListReplaceItem
TecGUIListDeleteAllItems
TecGUIListDeleteItemAtPos
TecGUIListDeselectAllItems
TecGUIListSetSelectedItem
TecGUIListGetSelectedItems
TecGUIListSetSelectedItems
TecGUIListGetSelectedItem
TecGUIListSelectAllItems
TecGUIListSetItems
TecGUIToggleAdd
TecGUIToggleSet
TecGUIToggleGet
TecGUIRadioBoxAdd
TecGUIRadioBoxSetToggle
TecGUIRadioBoxGetToggle
TecGUILabelAdd
TecGUILabelSetText
TecGUILabelSetLgIndex
TecGUILabelSetDouble
TecGUILabelSetSet
TecGUITextFieldAdd
TecGUITextAdd
TecGUITextAddKeyEventCallback
TecGUITextSetInsertPos
TecGUITextSetMinInsertPos
TecGUITextSetMaxInsertPos
TecGUITextSetString
TecGUITextGetString
TecGUITextInsertString
TecGUIScaleAdd
TecGUIScaleSetValue
TecGUIScaleSetLimits
TecGUIScaleGetValue
TecGUIVertSeparatorAdd
TecGUIHorzSeparatorAdd
TecGUIFrameAdd
TecGUITextFieldSetString
TecGUITextFieldGetString
TecGUITextFieldGetLgIndex
TecGUITextFieldGetDouble
TecGUITextFieldValidateLgIndex
TecGUITextFieldValidateDouble
TecGUITextFieldGetSet
TecGUITextFieldSetLgIndex
TecGUITextFieldSetDouble
TecGUITextFieldSetSet
TecGUIDialogLaunch
TecGUIDialogDrop
TecGUIDialogIsUp
TecGUIDialogSetTitle
TecGUITextAppendString
TecGUIMenuBarAdd
TecGUIMenuAdd
TecGUIMenuAddItem
TecGUIMenuAddToggle
TecGUIMenuAddSeparator
TecGUIMenuItemSetText
TecGUIMenuSetToggle
TecGUIMenuDeleteItem
TecGUITabAdd
TecGUITabAddPage
TecGUITabSetCurrentPage
TecGUIFormAdd
TecGUIFormAddPage
TecGUIFormSetCurrentPage
TecGUISpinTextFieldAdd
TecGUISpinTextFieldIncLgIndex
TecGUISpinTextFieldIncDouble
TecGUIOptionMenuDeleteItemAtPos
TecGUIOptionMenuAppendItem
TecGUIOptionMenuGetItemCount
TecGUIOptionMenuDeleteAllItems
TecGUIOptionMenuGetString
TecGUIOptionMenuReplaceItem
TecGUIScaleShowNumericDisplay
TecGUISidebarRegister
TecGUIMFCSidebarRegister
TecGUIMFCSidebarUpdateData
TecGUISidebarActivate
TecGUISidebarDeactivateAll
TecGUISidebarIsActive
TecGUIListGetCapacity
TecGUIListGetTopItemNum
TecGUIListSetTopItemNum
TecGUISetInputFocus
</exclude_tecmock> }}*/


/**
 * General GUI callback function with a const char * parameter. TecGUI
 * functions related to text fields and multi-line text fields require you to
 * provide a function that has this function prototype.
 *
 * @param TextString
 *   Read-only pointer. Text string sent to the callback function by the
 *   control issuing the call. Guaranteed to be non-NULL
 *
 * @return
 *   This function should return one if the text is valid, zero otherwise. For
 *   example, if the text field requires the user to enter a number and he or
 *   she enters a letter, you should return zero. It is the responsibility of
 *   the add-on to replace the text field value if the text is invalid.
 */
typedef LgIndex_t (*TecGUITextCallback_pf)(const char* TextString);
/**
 * General GUI callback function with a \ref LgIndex_t * parameter. Many of the
 * TecGUI functions require you to provide a function that has this function
 * prototype.
 * @param Data
 *   Read-only pointer. Depending on the calling function it could reference a
 *   single integer or an entire array of integers, where the end of the list
 *   is identified by a zero. The context of the control issuing the call
 *   governs the content. Guaranteed to be non-NULL.
 */
typedef void (*TecGUIIntCallback_pf)(const LgIndex_t* Data);
/**
 * General GUI callback function with no parameters. Many of the TecGUI functions
 * require you to provide a function that has this function prototype.
 */
typedef void (*TecGUIVoidCallback_pf)(void);

#define MAINDIALOGID          -1
#define BADDIALOGID           -2
#define BADGUIID              BADDIALOGID
#define TECGUITECPLOTSIDEBAR  -3
/* BEGINREMOVEFROMADDON */
#define TECGUINOSIDEBAR       -4
/* ENDREMOVEFROMADDON */


/* BEGINREMOVEFROMADDON */
#if defined MOTIF
# define VALID_TECGUI_ID(ID) ((ID) == MAINDIALOGID         || \
                              (ID) == TECGUITECPLOTSIDEBAR || \
                              ((0 <= (ID) && (ID) < GUINumWidgets) && \
                               VALID_WIDGET(GUIInstalledWidgetList[ID].Control)))
#if !defined USE_OLD_TABS
/*
 * Current uses:
 *   1) spin control: up and down arrow button widgets
 *   2) option menu:  Width and Height of the control
 *   3) tab widget:   Width and Height of the control
 */
#else
/*
 * Current uses:
 *   1) spin control: up and down arrow button widgets
 *   2) option menu:  Width and Height of the control
 */
#endif
# define MAXGUICOMPANIONWIDGETS 2
# define MAXGUIWIDGETS          40000


typedef struct
{
    void *Data;
    int   IsWidget; /* ...compiler indicates we should pass int instead of Boolean_t for va_arg */
} ControlClientData_s;

typedef struct
{
    Widget              Control;
    int                 NumItems;
    ControlClientData_s ClientData[MAXGUICOMPANIONWIDGETS];
} WidgetInfo_s;

EXTERN LgIndex_t    GUINumWidgets;
EXTERN WidgetInfo_s GUIInstalledWidgetList[MAXGUIWIDGETS];
#else

# define VALID_TECGUI_ID(ID) ((ID) == MAINDIALOGID          ||\
                              (ID) == TECGUITECPLOTSIDEBAR  ||\
                              (ID) >= 0)
#endif
/* ENDREMOVEFROMADDON */

/**
 * Set/Unset the dialog to always be on top of other windows
 */


/**
 * Shows or hides the Close and Help buttons at the bottom of TGB modeless
 * dialogs. Use the function if you do not need the standard set of buttons at
 * the bottom of the dialog, or if you wish to use other buttons or menu
 * options for these functions.
 *
 * This function must be called before the dialog is launched. It will assert
 * if called after a dialog is launched. This function does nothing if called
 * with a modal dialog argument.
 *
 * @par Note:
 *   Do not call this function more than once with different values for the
 *   ShowActionArea parameter. Once this function has been called with FALSE,
 *   it cannot be called again on the same dialog with TRUE.
 *
 * @param DialogID
 *   ID of the parent dialog.
 *
 * @param EnableActionArea
 *   Set to TRUE to show buttons at the bottom of the dialog, FALSE otherwise. Since TRUE is the
 *   default, it is unnecessary to call this function unless you wish to hide buttons.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIDialogEnableActionArea(
 *         DialogID,
 *         EnableActionArea)
 *    INTEGER*4 DialogID
 *    INTEGER*4 EnableActionArea
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIDialogEnableActionArea(LgIndex_t DialogID,
                                                      Boolean_t EnableActionArea);
/**
 * Sets the sensitivity of the Apply button in a dialog.
 *
 * @par Note:
 *   This function should only be called for modal dialogs that have been
 *   assigned an apply button when created. Typically, after a user presses the
 *   Apply button in a modal dialog an add-on should make the applicable
 *   changes and then set the Apply button to insensitive. When subsequent
 *   modification are made to fields in the dialog the Apply button should be
 *   made sensitive again.
 *
 * @param DialogID
 *   ID of the dialog for which the default action is to be reassigned
 *
 * @param IsSensitive
 *   Indicates if the Apply button should be made sensitive or not
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIDialogApplySetSensitivity(
 *         DialogID,
 *         IsSensitive)
 *    INTEGER*4 DialogID
 *    INTEGER*4 IsSensitive
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIDialogApplySetSensitivity(LgIndex_t DialogID,
                                                         Boolean_t IsSensitive);
/**
 * Sets a modal or modeless dialog to be the topmost window. In Windows,
 * calling this function with MakeTopmost equal to TRUE will add the
 * WS_EX_TOPMOST style to the dialog, otherwise the style will be cleared. If
 * set, the dialog will always remain on top of all other windows.  In UNIX,
 * this function does nothing.
 *
 * @param DialogID
 *   Dialog ID
 *
 * @param MakeTopmost
 *   Set to TRUE to make the dialog the topmost or FALSE to leave its stacking
 *   order unchanged
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIDialogSetTopmost(
 *         DialogID,
 *         MakeTopmost)
 *    INTEGER*4 DialogID
 *    INTEGER*4 MakeTopmost
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIDialogSetTopmost(LgIndex_t DialogID,
                                                Boolean_t MakeTopmost);

/**
 * Sets the position of the add-on dialog.
 *
 * @param DialogID
 *   ID of visible dialog to operate on.
 *
 * @param Placement
 *   The location on the dialog that is to be considered the anchor.
 *
 *   Possible values are:
 *
 * @verbatim
     AnchorAlignment_TopLeft
     AnchorAlignment_TopCenter
     AnchorAlignment_TopRight
     AnchorAlignment_MiddleLeft
     AnchorAlignment_MiddleCenter
     AnchorAlignment_MiddleRight
     AnchorAlignment_BottomLeft
     AnchorAlignment_BottomCenter
     AnchorAlignment_BottomRight
   @endverbatim
 *
 * @param OffsetX
 *   The X-position of the dialog
 *
 * @param OffsetY
 *   The Y-position of the dialog
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIDialogSetPosition(
 *         DialogID,
 *         Placement,
 *         OffsetX,
 *         OffsetY)
 *    INTEGER*4 DialogID
 *    INTEGER*4 Placement
 *    INTEGER*4 OffsetX
 *    INTEGER*4 OffsetY
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Set the launch position of the dialog with id D7 to be bottom left.
 *
 * @code
 *   TecGUIDialogSetPosition(D7,
 *                           AnchorAlignment_BottomLeft,
 *                           0,  // OffsetX
 *                           0); // OffsetY
 * @endcode
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIDialogSetPosition(LgIndex_t         DialogID,
                                                 AnchorAlignment_e Placement,
                                                 LgIndex_t         OffsetX,
                                                 LgIndex_t         OffsetY);

/**
 * @deprecated
 *   exclude_sdkdoc
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIDialogSetLaunchPosition(LgIndex_t         DialogID,
                                                       AnchorAlignment_e Placement,
                                                       LgIndex_t         OffsetX,
                                                       LgIndex_t         OffsetY);



/**
 * Set the position of an add-on dialog.
 *
 * @param ArgList
 *   Set of Arglist entries. This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *
 * Name:
 *   SV_DIALOGID
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   ID of visible dialog to operate on.
 *
 * Name:
 *   SV_ANCHORALIGNMENT
 * Type:
 *   AnchorAlignment_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   AnchorAlignment_MiddleCenter
 * Required:
 *   No
 * Notes:
 *   The Anchor alignment.
 *
 * Name:
 *   SV_ANCHORHORIZONTALINSIDE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to anchor horizontally on the inside of the Tecplot
 *   process window.  Set to FALSE to anchor on the outside of the process
 *   window.
 *
 * Name:
 *   SV_ANCHORVERTICALINSIDE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to anchor vertically on the inside of the Tecplot
 *   process window.  Set to FALSE to anchor on the outside of
 *   the process window.
 *
 * Name:
 *   SV_MINVISIBILITYPERCENTAGE
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   10
 * Required:
 *   No
 * Notes:
 *   This sets the minimum percentage of the dialog required to be
 *   visibile.  If the offsets requested force the dialog outside
 *   of the desktop the offset is adjusted to enforce this rule.
 *
 * Name:
 *   SV_IOFFSET
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   The offset from the initial alignment position in the X-Direction.
 *
 * Name:
 *   SV_JOFFSET
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   The offset from the initial alignment position in the Y-Direction
 *   where positive goes down.
 * </ArgListTable>
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIDialogSetPositionX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * Position the dialog outside of the tecplot process window and to the right.
 *
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *
 *   // DialogID supplied ....
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList, SV_DIALOGID,               DialogID);
 *   TecUtilArgListAppendInt(ArgList, SV_ANCHORALIGNMENT,        AnchorAlignment_TopRight);
 *   TecUtilArgListAppendInt(ArgList, SV_ANCHORHORIZONTALINSIDE, FALSE);
 *   TecGUIDialogSetPositionX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIDialogSetPositionX(ArgList_pa ArgList);

/**
 * @deprecated
 *   exclude_sdkdoc
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIDialogSetLaunchPositionX(ArgList_pa ArgList);





/**
 * Creates a modeless dialog and returns the ID of the dialog. A modeless
 * dialog is one that will allow the user to interact with Tecplot and the
 * controls within the dialog concurrently. The dialog is not displayed until
 * you call TecGUIDialogLaunch().
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog. You can also pass MAINDIALOGID for this parameter
 *
 * @param Width
 *   Width in character width units. Must be greater than or equal to zero
 *
 * @param Height
 *   Height in character height units. Must be greater than or equal to zero
 *
 * @param Title
 *   Caption of the dialog. Must not be NULL
 *
 * @param InitCallback
 *   Function that performs a user-defined operation immediately before the
 *   dialog isdisplayed. See TecGUIVoidCallback_pf for a definition example. If
 *   this parameter is NULL no initialization function will be called. The
 *   dialog will not be displayed until TecGUIDialogLaunch is called
 *
 * @param CloseButtonCallback
 *   Function that performs a user-defined operation when Close is clicked. See
 *   TecGUIVoidCallback_pf for a definition example
 *
 * @param HelpButtonCallback
 *   Function that performs a user-defined operation when Help is clicked.
 *   SeeTecGUIVoidCallback_pf for a definition example. If this parameter is
 *   NULL the Help button is not added
 *
 * @return
 *   Dialog ID.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIDialogCreateModeless(
 *         ParentDialogID,
 *         Width,
 *         Height,
 *         Title,
 *         InitCallback,
 *         CloseButtonCallback,
 *         HelpButtonCallback)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 Width
 *    INTEGER*4 Height
 *    CHARACTER*(*) Title
 *    POINTER (InitCallbackPtr, InitCallback)
 *    POINTER (CloseButtonCallbackPtr, CloseButtonCallback)
 *    POINTER (HelpButtonCallbackPtr, HelpButtonCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIDialogCreateModeless(LgIndex_t              ParentDialogID,
                                                         LgIndex_t              Width,
                                                         LgIndex_t              Height,
                                                         const char            *Title,
                                                         TecGUIVoidCallback_pf  InitCallback,
                                                         TecGUIVoidCallback_pf  CloseButtonCallback,
                                                         TecGUIVoidCallback_pf  HelpButtonCallback);

/**
 * Creates a modal dialog and returns the ID of the dialog. A modal dialog is
 * one that restricts the user to acting within the dialog, and locks
 * everything else on the screen, until the user clicks OK or Cancel. The
 * dialog is not displayed until you call TecGUIDialogLaunch().
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog. You can also pass MAINDIALOGID for this parameter
 *
 * @param Width
 *   Width in character width units. Must be greater than or equal to zero
 *
 * @param Height
 *   Height in character height units. Must be greater than or equal to zero
 *
 * @param Title
 *   Caption of the dialog. Must not be NULL
 *
 * @param InitCallback
 *   Function that performs a user-defined operation immediately before the
 *   dialog is displayed. See TecGUIVoidCallback_pf for a definition example.
 *   If this parameter is NULL no intialization function will be called. The
 *   dialog will not be displayed until TecGUIDialogLaunch is called.
 *
 * @param OkButtonCallback
 *   Function that performs a user-defined operation when OK is clicked.
 *
 * @param ApplyButtonCallback
 *   Function that performs a user-defined operation when the dialog's Apply
 *   button is clicked. If this option is NULL the Apply button is not added.
 *
 * @param CancelButtonCallback
 *   Function that performs a user-defined operation when Cancel is clicked.
 *   See TecGUIVoidCallback_pf for a definition example. If this parameter is
 *   NULL the Cancel button is not added
 *
 * @param HelpButtonCallback
 *   Function that performs a user-defined operation when Help is clicked. See
 *   TecGUIVoidCallback_pf for a definition example. If this parameter is NULL
 *   the Help button is not added.
 *
 * @return
 *   Dialog ID.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIDialogCreateModal(
 *         ParentDialogID,
 *         Width,
 *         Height,
 *         Title,
 *         InitCallback,
 *         OkButtonCallback,
 *         ApplyButtonCallback,
 *         CancelButtonCallback,
 *         HelpButtonCallback)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 Width
 *    INTEGER*4 Height
 *    CHARACTER*(*) Title
 *    POINTER (InitCallbackPtr, InitCallback)
 *    POINTER (OkButtonCallbackPtr, OkButtonCallback)
 *    POINTER (ApplyButtonCallbackPtr, ApplyButtonCallback)
 *    POINTER (CancelButtonCallbackPtr, CancelButtonCallback)
 *    POINTER (HelpButtonCallbackPtr, HelpButtonCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIDialogCreateModal(LgIndex_t              ParentDialogID,
                                                      LgIndex_t              Width,
                                                      LgIndex_t              Height,
                                                      const char            *Title,
                                                      TecGUIVoidCallback_pf  InitCallback,
                                                      TecGUIVoidCallback_pf  OkButtonCallback,
                                                      TecGUIVoidCallback_pf  ApplyButtonCallback,
                                                      TecGUIVoidCallback_pf  CancelButtonCallback,
                                                      TecGUIVoidCallback_pf  HelpButtonCallback);

/**
 * Call this function if your code is structured such that it must wait for a
 * modal dialog to close.
 *
 * @par Note:
 *   Calls to this function cannot be nested if the modal dialog launches its
 *   own modal dialogs. It is only valid for a single modal dialog and will
 *   issue an error if it is called when there is more than one modal dialog
 *   being displayed.
 *
 * @param DoneWithModalDialog
 *   Pointer to a boolean variable which the add-on sets to TRUE to stop blocking. Typically this is
 *   done by the add-on in the OK and Cancel callback functions.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIBlockForModalDialog(DoneWithModalDialog)
 *    INTEGER*4 DoneWithModalDialog
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Launch and block a modal dialog.
 *
 * @code
 *   Boolean_t DoneWithModalDialog = FALSE
 *
 *    {
 *       BuildDialog1(MAINDIALOGID);
 *       TecGUIDialogLaunch(Dialog1Manager);
 *
 *       // Will not return until DoneWithModalDialog is TRUE.
 *       // In the OK and Cancel dialog callbacks set DoneWithModalDialog to TRUE
 *       TecGUIBlockForModalDialog(&DoneWithModalDialog);
 *
 *       TecUtilDialogMessageBox("Finished blocking.",MessageBoxType_Information);
 *     }
 *
 * @endcode
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIBlockForModalDialog(Boolean_t *DoneWithModalDialog);

/**
 * Adds a colored button to a dialog. You must call this function before calling
 * TecGUIDialogLaunch().
 *
 * @since
 *   10.0-6-008
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under
 *   rare circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog. This must be a valid dialog ID
 *
 * @param X
 *   Left coordinate of the button in character width units relative to the dialog. Must be greater
 *   than or equal to zero
 *
 * @param Y
 *   Top coordinate of the button in character height units relative to the dialog. Must be greater
 *   than or equal to zero
 *
 * @param Width
 *   Width of the button in character width units. Must be greater than or equal to zero
 *
 * @param Height
 *   Height of the button in character height units. Must be greater than or equal to zero
 *
 * @param LabelString
 *   Label of the button. Must not be NULL
 *
 * @param ColorIndex
 *   Index of the color for the button to be initialized with. Must not be InvalidColor_C.
 *
 * @param ButtonCallback
 *   Function that performs a user-defined operation when clicked. See TecGUIVoidCallback_pf for a
 *   definition example
 *
 * @return
 *   The identifier of the button.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIColoredButtonAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         Width,
 *         Height,
 *         LabelString,
 *         ColorIndex
 *         ButtonCallback)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    INTEGER*4 Width
 *    INTEGER*4 Height
 *    CHARACTER*(*) LabelString
 *    INTEGER*4 ColorIndex
 *    POINTER (ButtonCallbackPtr, ButtonCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIColoredButtonAdd(LgIndex_t              ParentDialogID,
                                                     LgIndex_t              X,
                                                     LgIndex_t              Y,
                                                     LgIndex_t              Width,
                                                     LgIndex_t              Height,
                                                     const char            *LabelString,
                                                     ColorIndex_t           ColorIndex,
                                                     TecGUIVoidCallback_pf  ButtonCallback);
/**
 * Adds a button to a dialog. You must call this function before calling
 * TecGUIDialogLaunch().
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under
 *   rare circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog. This must be a valid dialog ID
 *
 * @param X
 *   Left coordinate of the button in character width units relative to the dialog. Must be greater
 *   than or equal to zero
 *
 * @param Y
 *   Top coordinate of the button in character height units relative to the dialog. Must be greater
 *   than or equal to zero
 *
 * @param Width
 *   Width of the button in character width units. Must be greater than or equal to zero
 *
 * @param Height
 *   Height of the button in character height units. Must be greater than or equal to zero
 *
 * @param LabelString
 *   Label of the button. Must not be NULL
 *
 * @param ButtonCallback
 *   Function that performs a user-defined operation when clicked. See TecGUIVoidCallback_pf for a
 *   definition example
 *
 * @return
 *   The identifier of the button.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIButtonAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         Width,
 *         Height,
 *         LabelString,
 *         ButtonCallback)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    INTEGER*4 Width
 *    INTEGER*4 Height
 *    CHARACTER*(*) LabelString
 *    POINTER (ButtonCallbackPtr, ButtonCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIButtonAdd(LgIndex_t              ParentDialogID,
                                              LgIndex_t              X,
                                              LgIndex_t              Y,
                                              LgIndex_t              Width,
                                              LgIndex_t              Height,
                                              const char            *LabelString,
                                              TecGUIVoidCallback_pf  ButtonCallback);
/**
 * Sets background color of a button.
 *
 * @since
 *   10.0-6-0
 *
 * @param ButtonID
 *   ID of the colored button widget. The button should have been created as &lt;&lt;COLORED;&gt;&gt.
 *
 * @param Color
 *   Color index.
 * <FortranSyntax>
 *    SUBROUTINE TecGUIButtonSetColor(
 *         ButtonID,
 *         Color)
 *    INTEGER*4 ButtonID
 *    INTEGER*4 Color
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIColoredButtonSetColor(LgIndex_t    ButtonID,
                                                     ColorIndex_t Color);

/**
 * Adds a bitmap button to a dialog. The bitmap is centered on the button. It
 * is not stetched to fit the button size.
 *
 * @par Note:
 *   TGB automatically generates code to call this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   D of the parent dialog, sidebar, form page, or tab page.
 *
 * @param X
 *   Left coordinate of the button. Must be greater than zero.
 *
 * @param Y
 *   Top coordinate of the button. Must be greater than zero.
 *
 * @param ButtonWidth
 *   Width of the button. Note that this is not the width of the bitmap. It is the width of the button
 *   on which the bitmap will be placed
 *
 * @param ButtonHeight
 *   Height of the button. Note that this is not the height of the bitmap. It is the height of the
 *   button on which the bitmap will be placed.
 *
 * @param BitmapWidth
 *   Pixel width of the bitmap to be placed on the button.
 *
 * @param BitmapHeight
 *   Pixel height of the bitmap to be placed on the button
 *
 * @param BitmapData_Array
 *   Array of bitmap data. Data is arranged from top line to bottom line, with each line in the form
 *   "RGBRGBRGB...RGBRGB" where R,G,B are byte (const char) values representing the Red, Green, and
 *   Blue components of each pixel
 *
 * @param UseTransparentColor
 *   TRUE if the bitmap has a transparent color, FALSE otherwise
 *
 * @param TransparentR
 *   Red component of the transparent color. Ignored if UseTransparentColor is FALSE
 *
 * @param TransparentG
 *   Green component of the transparent color. Ignored if UseTransparentColor is FALSE
 *
 * @param TransparentB
 *   Blue component of the transparent color. Ignored if UseTransparentColor is FALSE
 *
 * @param ButtonCallback
 *   Function that performs a user-defined operation whcn clicked. For more information, see
 *   TecGUIVoidCallback_pf
 *
 * @return
 *   The ID of the bitmap button.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIBitmapButtonAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         ButtonWidth,
 *         ButtonHeight,
 *         BitmapWidth,
 *         BitmapHeight,
 *         BitmapData_Array,
 *         UseTransparentColor,
 *         TransparentR,
 *         TransparentG,
 *         TransparentB,
 *         ButtonCallback)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    INTEGER*4 ButtonWidth
 *    INTEGER*4 ButtonHeight
 *    INTEGER*4 BitmapWidth
 *    INTEGER*4 BitmapHeight
 *    CHARACTER*(*) BitmapData_Array
 *    INTEGER*4 UseTransparentColor
 *    INTEGER*4 TransparentR
 *    INTEGER*4 TransparentG
 *    INTEGER*4 TransparentB
 *    POINTER (ButtonCallbackPtr, ButtonCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIBitmapButtonAdd(LgIndex_t              ParentDialogID,
                                                    LgIndex_t              X,
                                                    LgIndex_t              Y,
                                                    LgIndex_t              ButtonWidth,
                                                    LgIndex_t              ButtonHeight,
                                                    LgIndex_t              BitmapWidth,
                                                    LgIndex_t              BitmapHeight,
                                                    const char            *BitmapData_Array,
                                                    Boolean_t              UseTransparentColor,
                                                    LgIndex_t              TransparentR,
                                                    LgIndex_t              TransparentG,
                                                    LgIndex_t              TransparentB,
                                                    TecGUIVoidCallback_pf  ButtonCallback);

/**
 * Adds a bitmap toggle button to a dialog. A bitmap toggle button works like a
 * toggle, except that instead of the checkmark, a bitmap button is used which
 * has a "pushed" appearance when the toggle is selected. The bitmap is
 * centered on the button. It is not stretched to fit the button size.
 *
 * @par Note:
 *   TGB automatically generates code to call this function. Only under rare circumstances will
 *   you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog, sidebar, form page, or tab page
 *
 * @param X
 *   Left coordinate of the button. Must be greater than zero.
 *
 * @param Y
 *   Top coordinate of the button. Must be greater than zero.
 *
 * @param ButtonWidth
 *   Width of the button. Note that this is not the width of the bitmap. It is the width of the button
 *   on which the bitmap will be placed
 *
 * @param ButtonHeight
 *   Height of the button. Note that this is not the height of the bitmap. It is the height of the
 *   button on which the bitmap will be placed.
 *
 * @param BitmapWidth
 *   Pixel width of the bitmap to be placed on the button.
 *
 * @param BitmapHeight
 *   Pixel height of the bitmap to be placed on the button
 *
 * @param BitmapData_Array
 *   Array of bitmap data. Data is arranged from top line to bottom line, with each line in the form
 *   "RGBRGBRGB...RGBRGB" where R,G,B are byte (const char) values representing the Red, Green, and
 *   Blue components of each pixel
 *
 * @param UseTransparentColor
 *   TRUE if the bitmap has a transparent color, FALSE otherwise
 *
 * @param TransparentR
 *   Red component of the transparent color. Ignored if UseTransparentColor is FALSE
 *
 * @param TransparentG
 *   Green component of the transparent color. Ignored if UseTransparentColor is FALSE
 *
 * @param TransparentB
 *   Blue component of the transparent color. Ignored if UseTransparentColor is FALSE
 *
 * @param ValueChangedCallback
 *   Function that performs a user-defined operation whcn clicked. For more information, see
 *   TecGUIVoidCallback_pf
 *
 * @return
 *   The ID of the bitmap toggle button.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIBitmapToggleAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         ButtonWidth,
 *         ButtonHeight,
 *         BitmapWidth,
 *         BitmapHeight,
 *         BitmapData_Array,
 *         UseTransparentColor,
 *         TransparentR,
 *         TransparentG,
 *         TransparentB,
 *         ValueChangedCallback)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    INTEGER*4 ButtonWidth
 *    INTEGER*4 ButtonHeight
 *    INTEGER*4 BitmapWidth
 *    INTEGER*4 BitmapHeight
 *    CHARACTER*(*) BitmapData_Array
 *    INTEGER*4 UseTransparentColor
 *    INTEGER*4 TransparentR
 *    INTEGER*4 TransparentG
 *    INTEGER*4 TransparentB
 *    POINTER (ValueChangedCallbackPtr, ValueChangedCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIBitmapToggleAdd(LgIndex_t             ParentDialogID,
                                                    LgIndex_t             X,
                                                    LgIndex_t             Y,
                                                    LgIndex_t             ButtonWidth,
                                                    LgIndex_t             ButtonHeight,
                                                    LgIndex_t             BitmapWidth,
                                                    LgIndex_t             BitmapHeight,
                                                    const char           *BitmapData_Array,
                                                    Boolean_t             UseTransparentColor,
                                                    LgIndex_t             TransparentR,
                                                    LgIndex_t             TransparentG,
                                                    LgIndex_t             TransparentB,
                                                    TecGUIIntCallback_pf  ValueChangedCallback);

/**
 *   Sets a tool tip help string for the specified control.
 *
 * @param ControlID
 *   ID of the control needing tool tip help text
 *
 * @param ToolTipText
 *   Tool tip text to display when the mouse hovers over the control
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUISetToolTip(
 *         ControlID,
 *         ToolTipText)
 *    INTEGER*4 ControlID
 *    CHARACTER*(*) ToolTipText
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUISetToolTip(LgIndex_t   ControlID,
                                          const char *ToolTipText);
/**
 *   Sets a status line help string for the specified control.
 *
 * @param ControlID
 *   ID of the control needing status line text help
 *
 * @param StatusLineText
 *   Status line text to display when the mouse hovers over the control
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUISetStatusLine(
 *         ControlID,
 *         StatusLineText)
 *    INTEGER*4 ControlID
 *    CHARACTER*(*) StatusLineText
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUISetStatusLine(LgIndex_t   ControlID,
                                             const char *StatusLineText);



/**
 *   Used to identify the button control to receive the default action for a dialog. When a user
 *   presses return in a dialog the default action for the dialog is initiated. Usually one of the
 *   buttons in the action area of the dialog is defined to handle the default action however it can
 *   be assigned to one of your own buttons with this function.
 *
 * @param DialogID
 *   ID of the dialog for which the default action is to be reassigned
 *
 * @param ButtonID
 *   ID of the button to receive the default action
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIButtonSetDefault(
 *         DialogID,
 *         ButtonID)
 *    INTEGER*4 DialogID
 *    INTEGER*4 ButtonID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIButtonSetDefault(LgIndex_t DialogID,
                                                LgIndex_t ButtonID);
/**
 *   Sets the text of a button control.
 *
 * @param ButtonID
 *   ID of the button
 *
 * @param NewText
 *   New text for the button. This parameter cannot be NULL
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIButtonSetText(
 *         ButtonID,
 *         NewText)
 *    INTEGER*4 ButtonID
 *    CHARACTER*(*) NewText
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIButtonSetText(LgIndex_t   ButtonID,
                                             const char *NewText);




/**
 *   Sets the sensitivity (in Windows, the enabled state) of a control.
 *
 * @param ControlID
 *   ID of the control
 *
 * @param IsSensitive
 *   TRUE to set the state of the control to sensitive, FALSE otherwise
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUISetSensitivity(
 *         ControlID,
 *         IsSensitive)
 *    INTEGER*4 ControlID
 *    INTEGER*4 IsSensitive
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUISetSensitivity(LgIndex_t ControlID,
                                              Boolean_t IsSensitive);

/**
 *   Sets the visibility of a control.
 *
 * @param ControlID
 *   ID of the control to change visibility.
 *
 * @param MakeVisible
 *   TRUE to make the control visible, FALSE to make the control invisible
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUISetVisibility(
 *         ControlID,
 *         MakeVisible)
 *    INTEGER*4 ControlID
 *    INTEGER*4 MakeVisible
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUISetVisibility(LgIndex_t ControlID,
                                             Boolean_t MakeVisible);

/**
 * Adds an option menu control to a dialog.
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog. Must be a valid ID.
 *
 * @param X
 *   Left coordinate of the control in character width units relative to the
 *   dialog box. Must be greater than or equal to zero
 *
 * @param Y
 *   Top coordinate of the control in character height units relative to the dialog box. Must be
 *   greater than or equal to zero.
 *
 * @param Width
 *   Width of the control in character width units. Must be greater than or equal to zero
 *
 * @param Height
 *   Height of the control in character height units. Must be greater than or equal to zero
 *
 * @param OptionList
 *   Options in the control. Separate each option with a comma. For example,
 *   "Bart, Lisa, Homer, Marge." Options are not sorted; you can assume the
 *   order of the items will not change. Must not be NULL
 *
 * @param ValueChangedCallback
 *   Function that performs a user-defined operation when the option menu
 *   selection changes. The data passed to the callback is a reference to the
 *   one-based index of the selected item. See TecGUIIntCallback_pf for a
 *   single-value definition example
 *
 * @return
 *   ID of the control.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIOptionMenuAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         Width,
 *         Height,
 *         OptionList,
 *         ValueChangedCallback)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    INTEGER*4 Width
 *    INTEGER*4 Height
 *    CHARACTER*(*) OptionList
 *    POINTER (ValueChangedCallbackPtr, ValueChangedCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIOptionMenuAdd(LgIndex_t             ParentDialogID,
                                                  LgIndex_t             X,
                                                  LgIndex_t             Y,
                                                  LgIndex_t             Width,
                                                  LgIndex_t             Height,
                                                  const char           *OptionList,
                                                  TecGUIIntCallback_pf  ValueChangedCallback);

/**
 *   Set the current option in an option menu.
 *
 * @param OptionMenuID
 *   ID of the option menu
 *
 * @param Selection
 *   The number of the option to set as the default. Options are numbered starting at one
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIOptionMenuSet(
 *         OptionMenuID,
 *         Selection)
 *    INTEGER*4 OptionMenuID
 *    INTEGER*4 Selection
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIOptionMenuSet(LgIndex_t OptionMenuID,
                                             LgIndex_t Selection);

/**
 *   Set the current item of an option menu to the item that matches the string.
 *
 * @return
 *   The position index of the item if it was found otherwise -1.
Parameter:
OptionMenuID ID of the
 *   option menu
String String with which to look for a match.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIOptionMenuSetByString(
 *         OptionMenuID,
 *         Name)
 *    INTEGER*4 OptionMenuID
 *    CHARACTER*(*) Name
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIOptionMenuSetByString(LgIndex_t   OptionMenuID,
                                                          const char *Name);

/**
 *   Gets the position index currently selected option menu item.
 *
 * @param OptionMenuID
 *   ID of the option menu
 *
 * @return
 *   Position index of the currently selected option menu item or zero if no item is selected.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIOptionMenuGet(OptionMenuID)
 *    INTEGER*4 OptionMenuID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIOptionMenuGet(LgIndex_t OptionMenuID);


/**
 * Adds a single or multi-selection list control to a dialog. After adding the
 * list control, you can call TecGUIListAppendItem() to add items to the list
 * control.
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog. Must be a valid ID.
 *
 * @param X
 *   Left coordinate of the control in character width units relative to the dialog box. Must be
 *   greater than or equal to zero
 *
 * @param Y
 *   Top coordinate of the control in character height units relative to the dialog box. Must be
 *   greater than or equal to zero.
 *
 * @param Width
 *   Width of the control in character width units. Must be greater than or equal to zero
 *
 * @param Height
 *   Height of the control in character height units. Must be greater than or equal to zero
 *
 * @param IsMultiSelection
 *   Set to TRUE for a multi-selection list box, FALSE for a single-selection list
 *
 * @param ValueChangedCallback
 *   Function that performs a user-defined operation when the list selection
 *   changes. The data passed to the callback is a zero terminated array of
 *   integers where each integer is the one-based index of a selected item. If
 *   no items are selected then the first item of the array is the zero array
 *   terminator. See TecGUIIntCallback_pf for a multi-value definition example
 *
 * @return
 *   The ID of the control.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIListAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         Width,
 *         Height,
 *         IsMultiSelection,
 *         ValueChangedCallback)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    INTEGER*4 Width
 *    INTEGER*4 Height
 *    INTEGER*4 IsMultiSelection
 *    POINTER (ValueChangedCallbackPtr, ValueChangedCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIListAdd(LgIndex_t            ParentDialogID,
                                            LgIndex_t            X,
                                            LgIndex_t            Y,
                                            LgIndex_t            Width,
                                            LgIndex_t            Height,
                                            Boolean_t            IsMultiSelection,
                                            TecGUIIntCallback_pf ValueChangedCallback);



/**
 *   Gets the number of items in a list control.
 *
 * @param ListID
 *   ID of the list control
 *
 * @return
 *   The number of items in the list control.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIListGetItemCount(ListID)
 *    INTEGER*4 ListID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIListGetItemCount(LgIndex_t ListID);

/**
 *   Appends an item to a list control.
 *
 * @param ListID
 *   ID of the list control.
 *
 * @param Item
 *   New list item. Must not be NULL
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIListAppendItem(
 *         ListID,
 *         Item)
 *    INTEGER*4 ListID
 *    CHARACTER*(*) Item
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIListAppendItem(LgIndex_t   ListID,
                                              const char *Item);

/**
 *   Gets the text of an item in a list box.
 *
 * @param ListID
 *   ID of the list control.
 *
 * @param Position
 *   The one-based index of the item
 *
 * @return
 *   The text of the item at the specified position in the list control. Note that the position is a
 *   1-based index. You must call TecUtilStringDealloc() to free this pointer.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIListGetString(
 *         ListID,
 *         Position,
 *         Result,
 *         ResultLength)
 *    INTEGER*4 ListID
 *    INTEGER*4 Position
 *    CHARACTER*(*) Result
 *    INTEGER*4 ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON char * STDCALL TecGUIListGetString(LgIndex_t ListID,
                                               LgIndex_t Position);

/**
 *   Replaces the text of an item in a list control.
 *
 * @param ListID
 *   ID of the list control
 *
 * @param Item
 *   New text of the item. Must not be NULL
 *
 * @param Position
 *   New text of the item. Must not be NULL
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIListReplaceItem(
 *         ListID,
 *         Item,
 *         Position)
 *    INTEGER*4 ListID
 *    CHARACTER*(*) Item
 *    INTEGER*4 Position
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIListReplaceItem(LgIndex_t   ListID,
                                               const char *Item,
                                               LgIndex_t   Position);

/**
 *   Removes all the items from a list control.
 *
 * @param ListID
 *   ID of the list control
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIListDeleteAllItems(ListID)
 *    INTEGER*4 ListID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIListDeleteAllItems(LgIndex_t ListID);

/**
 *   Deletes an item in a list control.
 *
 * @param ListID
 *   ID of the list control
 *
 * @param Position
 *   One-based index of the item to delete
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIListDeleteItemAtPos(
 *         ListID,
 *         Position)
 *    INTEGER*4 ListID
 *    INTEGER*4 Position
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIListDeleteItemAtPos(LgIndex_t ListID,
                                                   LgIndex_t Position);

/**
 * Deselects all items in a list control.
 *
 * @param ListID
 *   ID of the list control
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIListDeselectAllItems(ListID)
 *    INTEGER*4 ListID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIListDeselectAllItems(LgIndex_t ListID);

/**
 * Selects an item in a list control. If the list is a single selection list
 * then any previously selected item is first deselected and then the item at
 * the specified position is selected. If the list is a multi-selection list
 * the item at the specified position is added to the list of already selected
 * items.
 *
 * @par Note:
 *   When an item is programatically selected the list selection callback is
 *   not notified.
 *
 * @param ListID
 *   ID of the list control
 *
 * @param Position
 *   One-based position index of the item to select
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIListSetSelectedItem(
 *         ListID,
 *         Position)
 *    INTEGER*4 ListID
 *    INTEGER*4 Position
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIListSetSelectedItem(LgIndex_t ListID,
                                                   LgIndex_t Position);







/**
 * Gets the indices of all selected items in a list control. You can use this
 * function for both single and multi-selected list controls.
 *
 * @param ListID
 *   ID of the list control
 *
 * @param SelectedItemList
 *   Address of a pointer to an LgIndex_t (see the example below). Upon return,
 *   the pointer will contain an array of integers dimensioned by
 *   SelectedItemCount (see below). Each element of the array is the 1-based
 *   index of the selected item. You must call TecUtilArrayDealloc() to free
 *   the array when it is no longer needed.
 *
 * @param SelectedItemCount
 *   The number of selected items is returned in this pointer
 *
 *   Get the selected items assuming ListID references a valid list.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIListGetSelectedItems(
 *         ListID,
 *         MaxSelectedItemCount,
 *         SelectedItemList,
 *         SelectedItemCount)
 *    INTEGER*4 ListID
 *    INTEGER*4 MaxSelectedItemCount
 *    INTEGER*4 SelectedItemList()
 *    INTEGER*4 SelectedItemCount
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @par Note:
 *   For Fortran you must supply a pre-dimensioned array SelectedItemList
 *   and the maximum size of that array (MaxSelectedItemCount).   Make sure
 *   the array is dimensioned large enough for your situation.  Do not
 *   call TecUtilArrayDealloc().
 *
 * @code
 *   LgIndex_t count;
 *   LgIndex_t *sel;
 *   LgIndex_t i;
 *   TecGUIListGetSelectedItems(ListID,&sel,&count);
 *   for (i=0;i<count;i++)
 *     {
 *       // Do something useful with sel[]
 *     }
 *   TecUtilArrayDealloc((void **)&sel); // Clean up when done.
 * @endcode
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_fglue, exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIListGetSelectedItems(LgIndex_t   ListID,
                                                    LgIndex_t **SelectedItemList,   /* OUT */
                                                    LgIndex_t  *SelectedItemCount); /* OUT */










/**
 * Selects one or more items in a multi-selection list control. The items at
 * the specified positions are added to the list of already selected items.
 *
 * @par Note:
 *   When items are programatically selected the list selection callback is
 *   not notified.
 *
 *   Selects one or more (if the list is multi-selection) items in a list
 *   control.
 *
 * @param ListID
 *   ID of the list control.
 *
 * @param SelectedItemList
 *   Array of one-based indices. Each element of the array is the index of an
 *   item to select in the List control.
 *
 * @param SelectedItemCount
 *   Number of elements in the array
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIListSetSelectedItems(
 *         ListID,
 *         SelectedItemList,
 *         SelectedItemCount)
 *    INTEGER*4 ListID
 *    INTEGER*4 SelectedItemList()
 *    INTEGER*4 SelectedItemCount
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_fglue, exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIListSetSelectedItems(LgIndex_t  ListID,
                                                    LgIndex_t *SelectedItemList,
                                                    LgIndex_t  SelectedItemCount);

/**
 *   Gets the position index of the single selected item in a list control.
 *
 * @param ListID
 *   ID of the list.
 *
 * @return
 *   Position index of the single selected list item or -1 if no item or more than one item is
 *   selected.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIListGetSelectedItem(ListID)
 *    INTEGER*4 ListID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIListGetSelectedItem(LgIndex_t ListID);

/**
 *   Selects all items in a list control (if multi-selection).
 *
 * @param ListID
 *   ID of the multi-selection list control. If called for a single selection list control it will
 *   ASSERT with an error message
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIListSelectAllItems(ListID)
 *    INTEGER*4 ListID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIListSelectAllItems(LgIndex_t ListID);


/**
 * Adds a toggle control to a dialog.
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog.
 *
 * @param X
 *   Left coordinate of the control in character width units relative to the dialog. Must be greater
 *   than or equal to zero.
 *
 * @param Y
 *   Top coordinate of the control in character height units relative to the dialog. Must be greater
 *   than or equal to zero.
 *
 * @param Width
 *   Width of the control in character width units. Must be greater than or equal to zero.
 *
 * @param Height
 *   Height of the control in character height units. Must be greater than or equal to zero.
 *
 * @param Label
 *   Text of the control. Must not be NULL.
 *
 * @param ValueChangedCallback
 *   Function that performs a user-defined operation when the toggle value changes. The data passed to
 *   the callback is a reference to the toggle state: one if the toggle is set, otherwise zero. See
 *   TecGUIIntCallback_pf for a definition example.
 *
 * @return
 *   ID of the toggle.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIToggleAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         Width,
 *         Height,
 *         Label,
 *         ValueChangedCallback)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    INTEGER*4 Width
 *    INTEGER*4 Height
 *    CHARACTER*(*) Label
 *    POINTER (ValueChangedCallbackPtr, ValueChangedCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIToggleAdd(LgIndex_t             ParentDialogID,
                                              LgIndex_t             X,
                                              LgIndex_t             Y,
                                              LgIndex_t             Width,
                                              LgIndex_t             Height,
                                              const char           *Label,
                                              TecGUIIntCallback_pf  ValueChangedCallback);

/**
 *   Sets or clears a toggle control.
 *
 * @param ToggleID
 *   ID of the toggle control.
 *
 * @param SetOn
 *   Pass TRUE to set the toggle, FALSE to clear it.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIToggleSet(
 *         ToggleID,
 *         SetOn)
 *    INTEGER*4 ToggleID
 *    INTEGER*4 SetOn
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIToggleSet(LgIndex_t ToggleID,
                                         Boolean_t SetOn);



/**
 *   Get the current value of a toggle.
 *
 * @param ToggleID
 *   ID of the toggle control.
 *
 * @return
 *   The current value of a toggle. Returns one if the toggle is set and zero if unset.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIToggleGet(ToggleID)
 *    INTEGER*4 ToggleID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecGUIToggleGet(LgIndex_t ToggleID);

/**
 * Adds a set of radio box controls to a dialog. You must call this function
 * before calling TecGUIDialogLaunch(). Radio boxes are limited to five
 * toggles.
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog. Must be a valid ID.
 *
 * @param X
 *   Left coordinate of the control in character width units relative to the dialog box. Must be
 *   greater than or equal to zero
 *
 * @param Y
 *   Top coordinate of the control in character height units relative to the dialog box. Must be
 *   greater than or equal to zero.
 *
 * @param Width
 *   Width of the control in character width units. Must be greater than or equal to zero
 *
 * @param Height
 *   Height of the control in character height units. Must be greater than or equal to zero
 *
 * @param Label1
 *   Label of the first option button. Must not be NULL
 *
 * @param Label2
 *   Label of the second radio button Must not be NULL
 *
 * @param Label3
 *   Label of the third radio button. Can be NULL if Label4 and Label5 are NULL.
 *
 * @param Label4
 *   Label of the fourth radio button. Can be NULL if Label5 is NULL
 *
 * @param Label5
 *   Label of the fifth radio button. Can be NULL
 *
 * @param ValueChangedCallback
 *   Function that performs a user-defined operation when the option menu selection changes. The data
 *   passed to the callback is a reference to the one-based index of the selected item. See
 *   TecGUIIntCallback_pf for a single-value definition example
 *
 * @return
 *   The ID of the radio box control.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIRadioBoxAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         Width,
 *         Height,
 *         Label1,
 *         Label2,
 *         Label3,
 *         Label4,
 *         Label5,
 *         ValueChangedCallback)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    INTEGER*4 Width
 *    INTEGER*4 Height
 *    CHARACTER*(*) Label1
 *    CHARACTER*(*) Label2
 *    CHARACTER*(*) Label3
 *    CHARACTER*(*) Label4
 *    CHARACTER*(*) Label5
 *    POINTER (ValueChangedCallbackPtr, ValueChangedCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIRadioBoxAdd(LgIndex_t             ParentDialogID,
                                                LgIndex_t             X,
                                                LgIndex_t             Y,
                                                LgIndex_t             Width,
                                                LgIndex_t             Height,
                                                const char           *Label1,
                                                const char           *Label2,
                                                const char           *Label3,
                                                const char           *Label4,
                                                const char           *Label5,
                                                TecGUIIntCallback_pf  ValueChangedCallback);

/**
 *   Sets a radio button in radio box control.
 *
 * @param RadioBox
 *   ID of the radio box
 *
 * @param ToggleNumber
 *   One-based index of the radio button to select
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIRadioBoxSetToggle(
 *         RadioBox,
 *         ToggleNumber)
 *    INTEGER*4 RadioBox
 *    INTEGER*4 ToggleNumber
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIRadioBoxSetToggle(LgIndex_t RadioBox,
                                                 LgIndex_t ToggleNumber);


/**
 *   Get the current radio box selection.
 *
 * @param RadioBox
 *   ID of the radio box
 *
 * @return
 *   Number of the radio box control that is active. Toggles are numbered starting at one.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIRadioBoxGetToggle(RadioBox)
 *    INTEGER*4 RadioBox
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIRadioBoxGetToggle(LgIndex_t RadioBox);

/**
 * Adds a static text label to a dialog.
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog
 *
 * @param X
 *   Left coordinate of the control in character width units relative to the dialog. Must be greater
 *   than or equal to zero.
 *
 * @param Y
 *   Top coordinate of the control in character height units relative to the dialog. Must be greater
 *   than or equal to zero.
 *
 * @param Label
 *   Text for the label.
 *
 * @return
 *   The ID of the label.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUILabelAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         Label)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    CHARACTER*(*) Label
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUILabelAdd(LgIndex_t   ParentDialogID,
                                             LgIndex_t   X,
                                             LgIndex_t   Y,
                                             const char *Label);

/**
 *   Sets the text of a static label control.
 *
 * @param LabelID
 *   ID of the label to receive the formatted value
 *
 * @param LabelString
 *   New text for the label.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUILabelSetText(
 *         LabelID,
 *         LabelString)
 *    INTEGER*4 LabelID
 *    CHARACTER*(*) LabelString
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUILabelSetText(LgIndex_t   LabelID,
                                            const char *LabelString);

/**
 *   Formats an integer value and assigns it as the label string.
 *
 * @param LabelID
 *   ID of the label to receive the formatted value
 *
 * @param Value
 *   Value used to create the label string
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUILabelSetLgIndex(
 *         LabelID,
 *         Value)
 *    INTEGER*4 LabelID
 *    INTEGER*4 Value
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUILabelSetLgIndex(LgIndex_t LabelID,
                                               LgIndex_t Value);

/**
 *   Formats a double value and assigns it as the label string.
 *
 * @param LabelID
 *   ID of the label to receive the formatted value
 *
 * @param Value
 *   Value used to create the label string
 *
 * @param Format
 *   Format string that conforms to the C language formatting conventions used by Tecplot to format
 *   dynamic text and macro variables
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUILabelSetDouble(
 *         LabelID,
 *         Value,
 *         Format)
 *    INTEGER*4 LabelID
 *    REAL*8 Value
 *    CHARACTER*(*) Format
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUILabelSetDouble(LgIndex_t   LabelID,
                                              double      Value,
                                              const char *Format);

/**
 *   Formats a set and assigns it as the label string.
 *
 * @param LabelID
 *   ID of the label to receive the formatted set
 *
 * @param Set
 *   Set used to create the label string
 *
 * @param IncludeSquareBrackets
 *   Indicates if the set should be surrounded by optional square brackets
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUILabelSetSet(
 *         LabelID,
 *         SetPtr,
 *         IncludeSquareBrackets)
 *    INTEGER*4 LabelID
 *    POINTER (SetPtr, Set)
 *    INTEGER*4 IncludeSquareBrackets
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUILabelSetSet(LgIndex_t LabelID,
                                           Set_pa    Set,
                                           Boolean_t IncludeSquareBrackets);

/**
 * Adds a text field control to a dialog.
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog.
 *
 * @param X
 *   Left coordinate of the control in character width units relative to the dialog. Must be greater
 *   than or equal to zero.
 *
 * @param Y
 *   Top coordinate of the control in character height units relative to the dialog. Must be greater
 *   than or equal to zero.
 *
 * @param Width
 *   Width of the control in character width units. Must be greater than or equal to zero.
 *
 * @param Height
 *   Height of the control in character height units. Must be greater than or equal to zero
 *
 * @param ValueChangedCallback
 *   Function that performs a user-defined operation when the text value changes. The data passed to
 *   the callback is the text's new value. See TecGUITextCallback_pf for a definition and example.
 *
 * @return
 *   The ID of the text control.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUITextFieldAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         Width,
 *         Height,
 *         ValueChangedCallback)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    INTEGER*4 Width
 *    INTEGER*4 Height
 *    POINTER (ValueChangedCallbackPtr, ValueChangedCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUITextFieldAdd(LgIndex_t             ParentDialogID,
                                                 LgIndex_t             X,
                                                 LgIndex_t             Y,
                                                 LgIndex_t             Width,
                                                 LgIndex_t             Height,
                                                 TecGUITextCallback_pf ValueChangedCallback);

/**
 * Adds a multi-line text control to a dialog.
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog.
 *
 * @param X
 *   ID of the parent dialog.
 *
 * @param Y
 *   Top coordinate of the control in character height units relative to the dialog. Must be greater
 *   than or equal to zero
 *
 * @param Width
 *   Width of the control in character width units. Must be greater than or equal to zero.
 *
 * @param Height
 *   Height of the control in character height units. Must be greater than or equal to zero.
 *
 * @param IsReadOnly
 *   Set to TRUE to make the control read-only, otherwise set to FALSE.
 *
 * @param ValueChangedCallback
 *   Function that performs a user-defined operation when the text value changes. The data passed to
 *   the callback is the text's new value. See TecGUITextCallback_pf for a definition and example.
 *   This parameter may be NULL if IsReadOnly is TRUE.
 *
 * @return
 *   The ID of the text control.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUITextAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         Width,
 *         Height,
 *         IsReadOnly,
 *         ValueChangedCallback)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    INTEGER*4 Width
 *    INTEGER*4 Height
 *    INTEGER*4 IsReadOnly
 *    POINTER (ValueChangedCallbackPtr, ValueChangedCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUITextAdd(LgIndex_t             ParentDialogID,
                                            LgIndex_t             X,
                                            LgIndex_t             Y,
                                            LgIndex_t             Width,
                                            LgIndex_t             Height,
                                            Boolean_t             IsReadOnly,
                                            TecGUITextCallback_pf ValueChangedCallback);


/**
 * Add a key event callback for a multi-line text or text field control.
 *
 * @since
 *   10.0-3-128
 *
 * @param TextOrTextFieldID
 *   ID of the multi-line text or text field control.
 *
 * @param KeyEventCallback
 *   Function to call when a key is pressed in the control.  This function will have
 *   a pointer to a single Integer value which is the key ordinal value.  A newline
 *   value may be received for multi-line Text controls but not for single line
 *   Text Field controls.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUITextAddKeyEventCallback(
 *         TextOrTextFieldID,
 *         KeyEventCallback)
 *    INTEGER*4 TextOrTextFieldID
 *    POINTER (KeyEventCallbackPtr, KeyEventCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUITextAddKeyEventCallback(LgIndex_t            TextOrTextFieldID,
                                                       TecGUIIntCallback_pf KeyEventCallback);




/**
 *   Set the text insert position at the specified position in the text string. Text is inserted to the
 *   right of the specified position. To insert text at the beginning, set the insert position to
 *   zero. To insert text at the end, set the insert position to the length of the string currently
 *   maintained by the multi-line text control.
See also TecGUITextSetMinInsertPos and
 *   TecGUITextSetMaxInsertPos.
In Windows, the insert position is the position of the caret.
 *
 * @param Text
 *   ID of the text control.
 *
 * @param Position
 *   Insert position within the text limits: greater than or equal to zero, and less than or equal to
 *   the length of the text string maintained by the multi-line text control.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUITextSetInsertPos(
 *         Text,
 *         Position)
 *    INTEGER*4 Text
 *    INTEGER*4 Position
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUITextSetInsertPos(LgIndex_t Text,
                                                LgIndex_t Position);
/**
 *   Set the insert position to before the first character in text string maintained by the multi-line
 *   text control. This is equivalent to calling TecGUITextSetInsertPos(id,0).
 *
 * @param Text
 *   ID of the text control.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUITextSetMinInsertPos(Text)
 *    INTEGER*4 Text
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUITextSetMinInsertPos(LgIndex_t Text);
/**
 *   Set the text insert position at the maximum position in the text string. Text inserted at the
 *   maximum position places the text at the end of the text string maintained by the multi-line text
 *   control.
 *
 * @param Text
 *   ID of the text control.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUITextSetMaxInsertPos(Text)
 *    INTEGER*4 Text
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUITextSetMaxInsertPos(LgIndex_t Text);
/**
 *   Sets the text in a multi-line text control. The previous contents of the multi-line text control
 *   are erased.
 *
 * @param Text
 *   ID of the text control.
 *
 * @param TextString
 *   New string to copy into the text control. Must not be NULL.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUITextSetString(
 *         Text,
 *         TextString)
 *    INTEGER*4 Text
 *    CHARACTER*(*) TextString
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUITextSetString(LgIndex_t   Text,
                                             const char *TextString);
/**
 *   Gets the text in a multi-line text control. Lines are separated by new line characters ('\n')
 *   only.
 *
 * @param Text
 *   ID of the text control.
 *
 * @return
 *   The text current in the control. You must call TecUtilStringDealloc() to
 *   free the returned pointer (non-FORTRAN addons only).
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUITextGetString(
 *         Text,
 *         Result,
 *         ResultLength)
 *    INTEGER*4 Text
 *    CHARACTER*(*) Result
 *    INTEGER*4 ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @par note
 *   In FORTRAN, if ResultLength is zero then the text field was empty and
 *   the contents of the Result character string is undefined.
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON char * STDCALL TecGUITextGetString(LgIndex_t Text);
/**
 * Inserts text into a multi-line text control. The next text is inserted to
 * the right of the current text insert position. Use TecGUITextSetInsertPos() to
 * set the text insert position. Individual lines of the text are delimited by
 * the '\n' character.
 *
 * @param Text
 *   ID of the text control.
 *
 * @param TextString
 *   Text to insert. Must not be NULL.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUITextInsertString(
 *         Text,
 *         TextString)
 *    INTEGER*4 Text
 *    CHARACTER*(*) TextString
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUITextInsertString(LgIndex_t   Text,
                                                const char *TextString);
/**
 * Adds a scale control to a dialog.
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog. Must be a valid ID.
 *
 * @param X
 *   Left coordinate of the control in character width units relative to the dialog box.
 *
 * @param Y
 *   Top coordinate of the control in character height units relative to the dialog box.
 *
 * @param Width
 *   Width of the control in character width units.
 *
 * @param Height
 *   Height of the control in character height units.
 *
 * @param ScaleMin
 *   The minimum position of the scale. Usually zero.
 *
 * @param ScaleMax
 *   The maximum position of the scale.
 *
 * @param DecimalPrecision
 *   Specifies the number of decimal points to shift the slider value when displaying it. For example,
 *   a slider value of 2,350 and a DecimalPrecision value of 2 results in a display value of 23.50.
 *
 * @param ValueChangedCallback
 *   Function that performs a user-defined operation when the option menu selection changes. The data
 *   passed to the callback is a reference to the one-based index of the selected item. See
 *   TecGUIIntCallback_pf for a single-value definition example
 *
 * @param DragValueChangedCallback
 *   Function that performs a user-defined operation when the scale's value changes while dragging the
 *   scale slider. The data passed to the callback is a reference to the current scale value. See
 *   TecGUIIntCallback_pf for a single-value definition example.
 *
 * @return
 *   The ID of the scale control.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIScaleAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         Width,
 *         Height,
 *         ScaleMin,
 *         ScaleMax,
 *         DecimalPrecision,
 *         ValueChangedCallback,
 *         DragValueChangedCallback)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    INTEGER*4 Width
 *    INTEGER*4 Height
 *    INTEGER*4 ScaleMin
 *    INTEGER*4 ScaleMax
 *    INTEGER*4 DecimalPrecision
 *    POINTER (ValueChangedCallbackPtr, ValueChangedCallback)
 *    POINTER (DragValueChangedCallbackPtr, DragValueChangedCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIScaleAdd(LgIndex_t            ParentDialogID,
                                             LgIndex_t            X,
                                             LgIndex_t            Y,
                                             LgIndex_t            Width,
                                             LgIndex_t            Height,
                                             LgIndex_t            ScaleMin,
                                             LgIndex_t            ScaleMax,
                                             LgIndex_t            DecimalPrecision,
                                             TecGUIIntCallback_pf ValueChangedCallback,
                                             TecGUIIntCallback_pf DragValueChangedCallback);

/**
 *   Sets the current position of a scale control.
 *
 * @param ScaleID
 *   ID of the scale
 *
 * @param NewValue
 *   New value of the scale
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIScaleSetValue(
 *         ScaleID,
 *         NewValue)
 *    INTEGER*4 ScaleID
 *    INTEGER*4 NewValue
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIScaleSetValue(LgIndex_t ScaleID,
                                             LgIndex_t NewValue);

/**
 *   Set the limits (that is, minimum and maximum values) and decimal precision of a scale control.
 *
 * @param ScaleID
 *   ID of the scale control
 *
 * @param ScaleMin
 *   Minimum value of the scale.
 *
 * @param ScaleMax
 *   Maximum value of the scale
 *
 * @param DecimalPrecision
 *   Decimal precision of the scale. See TecGUIScaleAdd() for a description.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIScaleSetLimits(
 *         ScaleID,
 *         ScaleMin,
 *         ScaleMax,
 *         DecimalPrecision)
 *    INTEGER*4 ScaleID
 *    INTEGER*4 ScaleMin
 *    INTEGER*4 ScaleMax
 *    INTEGER*4 DecimalPrecision
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIScaleSetLimits(LgIndex_t ScaleID,
                                              LgIndex_t ScaleMin,
                                              LgIndex_t ScaleMax,
                                              LgIndex_t DecimalPrecision);

/**
 *   Sets the current position of a scale control.
 *
 * @param ScaleID
 *   ID of the scale
 *
 * @return
 *   Current value of the scale.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIScaleGetValue(ScaleID)
 *    INTEGER*4 ScaleID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIScaleGetValue(LgIndex_t ScaleID);

/**
 * Adds a vertical separator to a dialog.
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog.
 *
 * @param X
 *   Left coordinate of the control in character width units relative to the dialog. Must be greater
 *   than or equal to zero.
 *
 * @param Y
 *   Top coordinate of the control in character height units relative to the dialog. Must be greater
 *   than or equal to zero.
 *
 * @param Height
 *   Height of the separator in character height units. Must be greater than or equal to zero.
 *
 * @return
 *   The ID of the vertical separator.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIVertSeparatorAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         Height)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    INTEGER*4 Height
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIVertSeparatorAdd(LgIndex_t ParentDialogID,
                                                     LgIndex_t X,
                                                     LgIndex_t Y,
                                                     LgIndex_t Height);

/**
 * Adds a horizontal separator to a dialog.
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog
 *
 * @param X
 *   Left coordinate of the control in character width units relative to the dialog. Must be greater
 *   than or equal to zero
 *
 * @param Y
 *   Top coordinate of the control in character height units relative to the dialog. Must be greater
 *   than or equal to zero.
 *
 * @param Width
 *   Width of the separator in character width units. Must be greater than or equal to zero.
 *
 * @return
 *   The ID of the horizontal separator.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIHorzSeparatorAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         Width)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    INTEGER*4 Width
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIHorzSeparatorAdd(LgIndex_t ParentDialogID,
                                                     LgIndex_t X,
                                                     LgIndex_t Y,
                                                     LgIndex_t Width);

/**
 * Add a frame to the specified parent dialog. A frame is a box used to
 * visually separate one control, or group of controls, from another.
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog
 *
 * @param X
 *   Left coordinate of the control in character width units relative to the dialog. Must be greater
 *   than or equal to zero
 *
 * @param Y
 *   Top coordinate of the control in character height units relative to the dialog. Must be greater
 *   than or equal to zero
 *
 * @param Width
 *   Width of the control in character width units. Must be greater than or equal to zero
 *
 * @param Height
 *   Height of the control in character height units. Must be greater than or equal to zero
 *
 * @param Label
 *   Text of the label added to the upper-left hand corner of the frame. If NULL, no label is added
 *
 * @return
 *   ID of the frame control.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIFrameAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         Width,
 *         Height,
 *         Label)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    INTEGER*4 Width
 *    INTEGER*4 Height
 *    CHARACTER*(*) Label
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIFrameAdd(LgIndex_t   ParentDialogID,
                                             LgIndex_t   X,
                                             LgIndex_t   Y,
                                             LgIndex_t   Width,
                                             LgIndex_t   Height,
                                             const char *Label);

/**
 *   Sets the text in a text field control. The previous contents of the text field control are erased.
 *
 * @param TextFieldID
 *   ID of the text field control.
 *
 * @param TextString
 *   New string to place in the text control. Must not be NULL.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUITextFieldSetString(
 *         TextFieldID,
 *         TextString)
 *    INTEGER*4 TextFieldID
 *    CHARACTER*(*) TextString
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUITextFieldSetString(LgIndex_t   TextFieldID,
                                                  const char *TextString);
/**
 *   Gets the text in a text field control.
 *
 * @param TextFieldID
 *   ID of the text field control.
 *
 * @return
 *   The text current in the control. You must call TecUtilStringDealloc() to
 *   free the returned pointer (non-FORTRAN addons only).
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUITextFieldGetString(
 *         TextFieldID,
 *         Result,
 *         ResultLength)
 *    INTEGER*4 TextFieldID
 *    CHARACTER*(*) Result
 *    INTEGER*4 ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @par note
 *   In FORTRAN, if ResultLength is zero then the text field was empty and
 *   the contents of the Result character string is undefined.
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON char * STDCALL TecGUITextFieldGetString(LgIndex_t TextFieldID);
/**
 *   Gets the integer value from the text field.
 *
 * @param TextFieldID
 *   Text field ID
 *
 * @param Value
 *   Address to an LgIndex_t variable to receive the resulting text field's value
 *
 * @return
 *   TRUE if the value in the text field was a valid integer number, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUITextFieldGetLgIndex(
 *         TextFieldID,
 *         Value)
 *    INTEGER*4 TextFieldID
 *    INTEGER*4 Value
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecGUITextFieldGetLgIndex(LgIndex_t  TextFieldID,
                                                        LgIndex_t *Value);
/**
 *   Gets the double precision value from the text field.
 *
 * @param TextFieldID
 *   Text field ID
 *
 * @param Value
 *   Address to a double precision variable to receive the resulting text field's value.
 *
 * @return
 *   TRUE if the value in the text field was a valid double precision number, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUITextFieldGetDouble(
 *         TextFieldID,
 *         Value)
 *    INTEGER*4 TextFieldID
 *    REAL*8 Value
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecGUITextFieldGetDouble(LgIndex_t  TextFieldID,
                                                       double    *Value);
/**
 *   Validates that the contents of the specified text field are within the specified domain. If not,
 *   an error message is displayed and the caller notified.
 *
 * @param TextFieldID
 *   ID of the text field to receive the formatted set.
 *
 * @param TextFieldName
 *   Name of the attributes represented by the text field. This name is used in error message if the
 *   value is not within the specified domain. If this value is NULL, no error message is presented.
 *
 * @param MinDomain
 *   Minimum acceptable domain for the value.
 *
 * @param MaxDomain
 *   Maximum acceptable domain for the value.
 *
 * @param AllowMxSyntax
 *   Allow "Mx" and "Mx-n" where n is any positive number, in place of an integer value. If this value
 *   is TRUE, then the MinDomain must be greater than or equal to 1 as zero indicates Mx was used, -1
 *   indicates Mx-1 was used, and so forth
 *
 * @return
 *   TRUE if the contents of the text field represented a value within the specified domain, FALSE
 *   otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUITextFieldValidateLgIndex(
 *         TextFieldID,
 *         TextFieldName,
 *         MinDomain,
 *         MaxDomain,
 *         AllowMxSyntax)
 *    INTEGER*4 TextFieldID
 *    CHARACTER*(*) TextFieldName
 *    INTEGER*4 MinDomain
 *    INTEGER*4 MaxDomain
 *    INTEGER*4 AllowMxSyntax
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecGUITextFieldValidateLgIndex(LgIndex_t   TextFieldID,
                                                             const char *TextFieldName,
                                                             LgIndex_t   MinDomain,
                                                             LgIndex_t   MaxDomain,
                                                             Boolean_t   AllowMxSyntax);
/**
 *   Validates that the contents of the specified text field are within the
 *   specified domain. If not, an error message is displayed and the caller
 *   notified.
 *
 * @param TextFieldID
 *   ID of the text field to receive the formatted set.
 *
 * @param TextFieldName
 *   Name of the attributes represented by the text field. This name is used in
 *   the error message if the value is not within the specified domain. If this
 *   value is NULL no error message is presented
 *
 * @param MinDomain
 *   Minimum acceptable domain for the value
 *
 * @param MaxDomain
 *   Maximum acceptable domain for the value.
 *
 * @return
 *   TRUE if the contents of the specified text field are within the specified
 *   domain. FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUITextFieldValidateDouble(
 *         TextFieldID,
 *         TextFieldName,
 *         MinDomain,
 *         MaxDomain)
 *    INTEGER*4 TextFieldID
 *    CHARACTER*(*) TextFieldName
 *    REAL*8 MinDomain
 *    REAL*8 MaxDomain
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecGUITextFieldValidateDouble(LgIndex_t   TextFieldID,
                                                            const char *TextFieldName,
                                                            double      MinDomain,
                                                            double      MaxDomain);
/**
 *   Gets the set represented by the contents of the text field. A set is made
 *   of comma separated members that may optionally have outer square brackets.
 *   Each set member may be one of the following: a positive number, or a
 *   number range of the form n-m:s where n is the starting value, m is the
 *   ending value. The :s notation is an optional skip value. The following set
 *   defines the members 1, 2, 5, 7, 9, and 11: [1, 2, 5-11:2]
 *
 * @param TextFieldID
 *   ID of the Text Field.
 *
 * @param Set
 *   Address to a Set_pa variable to receive the resulting text field's set representation. A return
 *   value of NULL is used to indicate all members.
 *
 * @return
 *   TRUE if the notation in the text field was a valid set, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUITextFieldGetSet(
 *         TextFieldID,
 *         SetPtr)
 *    INTEGER*4 TextFieldID
 *    POINTER (SetPtr, Set)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecGUITextFieldGetSet(LgIndex_t  TextFieldID,
                                                    Set_pa    *Set);
/**
 *   Formats an integer value and assigns it as the text field string.
 *
 * @param TextFieldID
 *   ID of the text field to receive the formatted value
 *
 * @param Value
 *   Value used to create the text field string.
 *
 * @param UseMx
 *   Indicates of Mx syntax should be used. If TRUE then values less than or
 *   equal to zero are replaced with Mx syntax: Mx or Mx-n where n is a
 *   positive number. If the Value is zero Mx is used, -1 indicates Mx-1 is
 *   used and so forth.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUITextFieldSetLgIndex(
 *         TextFieldID,
 *         Value,
 *         UseMx)
 *    INTEGER*4 TextFieldID
 *    INTEGER*4 Value
 *    INTEGER*4 UseMx
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUITextFieldSetLgIndex(LgIndex_t TextFieldID,
                                                   LgIndex_t Value,
                                                   Boolean_t UseMx);
/**
 *   Formats a double value and assigns it as the text field string.
 *
 * @param TextFieldID
 *   ID of the text field to receive the formatted value.
 *
 * @param Value
 *   Value used to create the text field string.
 *
 * @param Format
 *   Format string that conforms to the C language formatting conventions used
 *   by Tecplot to format dynamic text and macro variables.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUITextFieldSetDouble(
 *         TextFieldID,
 *         Value,
 *         Format)
 *    INTEGER*4 TextFieldID
 *    REAL*8 Value
 *    CHARACTER*(*) Format
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUITextFieldSetDouble(LgIndex_t   TextFieldID,
                                                  double      Value,
                                                  const char *Format);
/**
 *   Formats a set and assigns it as the text field string.
 *
 * @param TextFieldID
 *   ID of the text field to receive the formatted set.
 *
 * @param Set
 *   Set used to create the text field string.
 *
 * @param IncludeSquareBrackets
 *   Indicates if the set should be surrounded by optional square brackets.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUITextFieldSetSet(
 *         TextFieldID,
 *         SetPtr,
 *         IncludeSquareBrackets)
 *    INTEGER*4 TextFieldID
 *    POINTER (SetPtr, Set)
 *    INTEGER*4 IncludeSquareBrackets
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUITextFieldSetSet(LgIndex_t TextFieldID,
                                               Set_pa    Set,
                                               Boolean_t IncludeSquareBrackets);

/**
 *   Displays a dialog created with TecGUIDialogCreatexxx(). After a dialog is launched, you cannot add
 *   any new controls to the dialog.
 *
 * @param DialogID
 *   ID of the dialog to display.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIDialogLaunch(DialogID)
 *    INTEGER*4 DialogID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIDialogLaunch(LgIndex_t DialogID);

/**
 * Closes a dialog. Usually this is called from the OK, Close, or Cancel button callbacks.
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param DialogID
 *   The ID of the Dialog.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIDialogDrop(DialogID)
 *    INTEGER*4 DialogID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIDialogDrop(LgIndex_t DialogID);

/**
 *   Returns TRUE if a dialog is currently displayed.
 *
 * @param DialogID
 *   ID of the dialog
 *
 * @return
 *   TRUE if the dialog is currently displayed, FALSE if not.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIDialogIsUp(DialogID)
 *    INTEGER*4 DialogID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecGUIDialogIsUp(LgIndex_t DialogID);

/**
 *   Sets the title text of a dialog.
 *
 * @param DialogID
 *   ID of the dialog.
 *
 * @param NewTitle
 *   New title for the dialog. This parameter cannot be NULL
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIDialogSetTitle(
 *         DialogID,
 *         NewTitle)
 *    INTEGER*4 DialogID
 *    CHARACTER*(*) NewTitle
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIDialogSetTitle(LgIndex_t   DialogID,
                                              const char *NewTitle);

/**
 *   Appends a string to the end of a multi-line text control.
 *
 * @param TextID
 *   ID of the text control.
 *
 * @param TextString
 *   Text to insert. Must not be NULL.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUITextAppendString(
 *         TextID,
 *         TextString)
 *    INTEGER*4 TextID
 *    CHARACTER*(*) TextString
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUITextAppendString(LgIndex_t   TextID,
                                                const char *TextString);

/**
 *   Add a menu bar to an existing dialog.
 *
 * @param ParentDialogID
 *   ID of the parent dialog
 *
 * @return
 *   ID of the menu bar added.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIMenuBarAdd(ParentDialogID)
 *    INTEGER*4 ParentDialogID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIMenuBarAdd(LgIndex_t ParentDialogID);

/**
 *   Add a menu to a menu bar or a walking menu to a menu list.
 *
 * @param ParentMenuID
 *   ID of the menu bar or menu list that this menu is to be added
 *
 * @param Label
 *   Text to place on the menu. Use an ampersand (&) to mark the mnemonic. The character immediately
 *   following & will be the mnemonic and the & is removed from the final text
 *
 * @return
 *   Returns the ID of the menu.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIMenuAdd(
 *         ParentMenuID,
 *         Label)
 *    INTEGER*4 ParentMenuID
 *    CHARACTER*(*) Label
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Add a menu item to Menu Bar called Options with t as the mnemonic.
 *
 * @code
 *   {
 *     LgIndex_t OptionMenuID;
 *     OptionMenuID = TecGUIMenuAdd(MenuBar,"Op&tions");
 *   }
 * @endcode
 *   In the Options menu of the menu bar, the ampersand (&) will not show,
 *   but t will be underlined. The Options menu may be selected by pressing T on your keyboard.
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIMenuAdd(LgIndex_t   ParentMenuID,
                                            const char *Label);

/**
 *   Add a menu item to a menu list.
 *
 * @param ParentMenuID
 *   ID of the parent menu.
 *
 * @param Label
 *   Text to put on the menu item.
 *
 * @param StatusLineText
 *   Optional text displayed on status line when mouse hovers over menu item. If NULL, do not use
 *
 * @param Callback
 *   Name of the function to call when this menu item is selected. See Section , "Function Callback
 *   Prototypes."
 *
 * @return
 *   Returns the ID of the menu item.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIMenuAddItem(
 *         ParentMenuID,
 *         Label,
 *         StatusLineText,
 *         Callback)
 *    INTEGER*4 ParentMenuID
 *    CHARACTER*(*) Label
 *    CHARACTER*(*) StatusLineText
 *    POINTER (CallbackPtr, Callback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIMenuAddItem(LgIndex_t              ParentMenuID,
                                                const char            *Label,
                                                const char            *StatusLineText,
                                                TecGUIVoidCallback_pf  Callback);

/**
 *   Add a menu item with a toggle to a menu list.
 *
 * @param ParentMenuID
 *   ID of the parent menu list.
 *
 * @param Label
 *   Text to place on the menu item. Use an ampersand (&) to mark the mnemonic. The character
 *   immediately following & will be the mnemonic and the & is removed from the final text. See
 *   TecGUIMenuAdd for an example of using a mnemonic
 *
 * @param StatusLineText
 *   Optional text displayed on status line when mouse hovers over menu item. If NULL, do not use.
 *
 * @param Callback
 *   Name of the function called when the menu item toggle is selected. See Section , "Function
 *   Callback Prototypes."
 *
 * @return
 *   ID of the menu toggle.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIMenuAddToggle(
 *         ParentMenuID,
 *         Label,
 *         StatusLineText,
 *         Callback)
 *    INTEGER*4 ParentMenuID
 *    CHARACTER*(*) Label
 *    CHARACTER*(*) StatusLineText
 *    POINTER (CallbackPtr, Callback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIMenuAddToggle(LgIndex_t             ParentMenuID,
                                                  const char           *Label,
                                                  const char           *StatusLineText,
                                                  TecGUIIntCallback_pf  Callback);

/**
 * Add a separator to a menu list.
 *
 * @param ParentMenuID
 *   ID of the menu list
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIMenuAddSeparator(ParentMenuID)
 *    INTEGER*4 ParentMenuID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIMenuAddSeparator(LgIndex_t ParentMenuID);

/**
 * Set the text for a menu item.
 *
 * @param MenuItemID
 *   ID of the menu item who's text to set.
 *
 * @param NewText
 *   Text to assign to the menu item
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIMenuItemSetText(
 *         MenuItemID,
 *         NewText)
 *    INTEGER*4 MenuItemID
 *    CHARACTER*(*) NewText
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIMenuItemSetText(LgIndex_t   MenuItemID,
                                               const char *NewText);

/**
 * Set the state of a menu item toggle.
 *
 * @param MenuItemID
 *   ID of the menu item who's toggle to set.
 *
 * @param SetOn
 *   TRUE if the menu toggle should be selected, otherwise FALSE
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIMenuSetToggle(
 *         MenuItemID,
 *         SetOn)
 *    INTEGER*4 MenuItemID
 *    INTEGER*4 SetOn
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIMenuSetToggle(LgIndex_t MenuItemID,
                                             Boolean_t SetOn);

/**
 * Delete a menu item from a menu list.
 *
 * @param MenuItemID
 *   ID of the menu item to delete
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIMenuDeleteItem(MenuItemID)
 *    INTEGER*4 MenuItemID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIMenuDeleteItem(LgIndex_t MenuItemID);

/**
 * Adds a tab control to a dialog.
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog.
 *
 * @param X
 *   Left coordinate of the control in character width units relative to the dialog.
 *
 * @param Y
 *   Top coordinate of the control incharacter height units relative to the dialog.
 *
 * @param Width
 *   Width of the control in character width units.
 *
 * @param Height
 *   Height of the control in character width units.
 *
 * @param ActivateCallback
 *   Called when a tab page is activated. The data passed is the ID of the activated tab page
 *
 * @param DeactivateCallback
 *   Called when a tab page is deactivated. The data passed is the ID of the deactivated tab page
 *
 * @return
 *   ID of the tab control. This ID is used only to identify the tab control when adding tab pages to
 *   the control. To add controls to a tab page, you must call TecGUITabAddPage() with the ID returned
 *   from this function.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUITabAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         Width,
 *         Height,
 *         ActivateCallback,
 *         DeactivateCallback)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    INTEGER*4 Width
 *    INTEGER*4 Height
 *    POINTER (ActivateCallbackPtr, ActivateCallback)
 *    POINTER (DeactivateCallbackPtr, DeactivateCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUITabAdd(LgIndex_t            ParentDialogID,
                                           LgIndex_t            X,
                                           LgIndex_t            Y,
                                           LgIndex_t            Width,
                                           LgIndex_t            Height,
                                           TecGUIIntCallback_pf ActivateCallback,
                                           TecGUIIntCallback_pf DeactivateCallback);

/**
 * Adds a page to a tab control. The ID returned from this function may be
 * passed to any TecGUI*Add function to add controls such as buttons, text
 * fields, and so forth, to this tab page.
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param TabID
 *   Parent tab control ID.
 *
 * @param Caption
 *   Caption of this tab control. Must be a valid string of length greater than zero
 *
 * @return
 *   ID of the tab page. This ID is returned from TecGUITabAdd(). It may be passed to any TecGUI*Add
 *   function to add controls such as buttons, text fields, and so forth, to this tab page.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUITabAddPage(
 *         TabID,
 *         Caption)
 *    INTEGER*4 TabID
 *    CHARACTER*(*) Caption
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUITabAddPage(LgIndex_t   TabID,
                                               const char *Caption);

/**
 * Sets a specific tab page of a tab control as the current tab page.
 *
 * @par Note:
 *   Calling this function does not generate Activate and Deactivate callback
 *   events for the tab page argument. These callbacks are generated only when
 *   the user clicks a tab page with their mouse.
 *
 * @param TabID
 *   ID of the parent tab control.
 *
 * @param PageID
 *   ID of the page to set as the current tab page.
 *
 * @return
 *   The ID of the text control.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUITabSetCurrentPage(
 *         TabID,
 *         PageID)
 *    INTEGER*4 TabID
 *    INTEGER*4 PageID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUITabSetCurrentPage(LgIndex_t TabID,
                                                 LgIndex_t PageID);




/**
 * Adds a new form control.
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   ID of the parent dialog
 *
 * @param X
 *   Left coordinate of the control in character width units relative to the dialog. Must be greater
 *   than or equal to zero
 *
 * @param Y
 *   Top coordinate of the control in character height units relative to the dialog. Must be greater
 *   than or equal to zero
 *
 * @param Width
 *   Width of the control in character width units. Must be greater than or equal to zero
 *
 * @param Height
 *   Height of the control in character height units. Must be greater than or equal to zero
 *
 * @return
 *   ID of a form which can be passed to TecGUIFormAddPage.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIFormAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         Width,
 *         Height)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    INTEGER*4 Width
 *    INTEGER*4 Height
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIFormAdd(LgIndex_t ParentDialogID,
                                            LgIndex_t X,
                                            LgIndex_t Y,
                                            LgIndex_t Width,
                                            LgIndex_t Height);

/**
 * Creates a new form page.
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentFormID
 *   Parent form ID
 *
 * @return
 *   ID which can be passed to any TecGUI*Add() function to add controls to this form page, such as
 *   buttons, text fields, and so forth.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIFormAddPage(ParentFormID)
 *    INTEGER*4 ParentFormID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIFormAddPage(LgIndex_t ParentFormID);

/**
 *   Sets a specific form page to be displayed.
 *
 * @param FormID
 *   Form Page ID
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIFormSetCurrentPage(FormID)
 *    INTEGER*4 FormID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIFormSetCurrentPage(LgIndex_t FormID);



/**
 * Adds a spin text field to a dialog.
 *
 * @par Note:
 *   TGB automatically generates code that uses this function. Only under rare
 *   circumstances will you need to call this function directly yourself.
 *
 * @param ParentDialogID
 *   Parent dialog ID.
 *
 * @param X
 *   Left coordinate of the control in character width units relative to the dialog.
 *
 * @param Y
 *   Top coordinate of the control in character height units relative to the dialog.
 *
 * @param Width
 *   Width of the control in character width units.
 *
 * @param Height
 *   Height of the control in character width units.
 *
 * @param ValueChangedCallback
 *   Function that performs a user-defined operation when the text values changes. The data passed to
 *   the callback is the text's new value. See TecGUITextCallback_pf for an example.
 *
 * @param ButtonUpCallback
 *   Called when the up button is clicked. Typically you will increment and redisplay the numeric value
 *   in the text control, however, this is not a requirement.
 *
 * @param ButtonDownCallback
 *   Called when the down button is clicked. Typically you will decrement and redisplay the number
 *   value in the text field, however, this is not a requirement
 *
 * @return
 *   ID of the control. A spin control is a kind of text field control. Thus, the ID can be passed to
 *   any TecGUI* requiring a text field control ID.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUISpinTextFieldAdd(
 *         ParentDialogID,
 *         X,
 *         Y,
 *         Width,
 *         Height,
 *         ValueChangedCallback,
 *         ButtonUpCallback,
 *         ButtonDownCallback)
 *    INTEGER*4 ParentDialogID
 *    INTEGER*4 X
 *    INTEGER*4 Y
 *    INTEGER*4 Width
 *    INTEGER*4 Height
 *    POINTER (ValueChangedCallbackPtr, ValueChangedCallback)
 *    POINTER (ButtonUpCallbackPtr, ButtonUpCallback)
 *    POINTER (ButtonDownCallbackPtr, ButtonDownCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUISpinTextFieldAdd(LgIndex_t             ParentDialogID,
                                                     LgIndex_t             X,
                                                     LgIndex_t             Y,
                                                     LgIndex_t             Width,
                                                     LgIndex_t             Height,
                                                     TecGUITextCallback_pf ValueChangedCallback,
                                                     TecGUIVoidCallback_pf ButtonUpCallback,
                                                     TecGUIVoidCallback_pf ButtonDownCallback);

/**
 *   The Tecplot GUI Builder creates empty callbacks for the up and down arrow buttons of each spin box
 *   used by the add-on. The add-on developer can add this function to the callback to increment or
 *   decrement the value within the specified limits in response to the action.
 *
 * @param SpinTextFieldID
 *   Spin box text field ID.
 *
 * @param Increment
 *   The increment, either positive or negative, to add to the current contents of the spin box text
 *   field. The resulting value is clamped between the values specified for MinDomain and MaxDomain
 *   parameters.
 *
 * @param MinDomain
 *   The minimum domain value for the spin box text field after adding the increment.
 *
 * @param MaxDomain
 *   The maximum domain value for the spin box text field after adding the increment
 *
 * @return
 *   TRUE if the original value in the spin box text field was a valid integer number, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUISpinTextFieldIncLgIndex(
 *         SpinTextFieldID,
 *         Increment,
 *         MinDomain,
 *         MaxDomain)
 *    INTEGER*4 SpinTextFieldID
 *    INTEGER*4 Increment
 *    INTEGER*4 MinDomain
 *    INTEGER*4 MaxDomain
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecGUISpinTextFieldIncLgIndex(LgIndex_t   SpinTextFieldID,
                                                            LgIndex_t   Increment,
                                                            LgIndex_t   MinDomain,
                                                            LgIndex_t   MaxDomain);

/**
 *   The Tecplot GUI Builder creates empty callbacks for the up and down arrow buttons of each spin box
 *   used by the add-on. The add-on developer can add this function to the callback to increment or
 *   decrement the value within the specified limits in response to the action.
 *
 * @param SpinTextFieldID
 *   Spin box text field ID.
 *
 * @param Format
 *   Format string that conforms to the C language formatting conventions used by Tecplot to format
 *   dynamic text and macro variables
 *
 * @param Increment
 *   The increment, either positive or negative, to add to the current contents of the spin box text
 *   field. The resulting value is clamped between the values specified for MinDomain and MaxDomain
 *   parameters
 *
 * @param MinDomain
 *   The minimum domain value for the spin box text field after adding the increment.
 *
 * @param MaxDomain
 *   The maximum domain value for the spin box text field after adding the increment
 *
 * @return
 *   TRUE if the original value in the spin box text field was a valid double precision floating point
 *   number, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUISpinTextFieldIncDouble(
 *         SpinTextFieldID,
 *         Format,
 *         Increment,
 *         MinDomain,
 *         MaxDomain)
 *    INTEGER*4 SpinTextFieldID
 *    CHARACTER*(*) Format
 *    REAL*8 Increment
 *    REAL*8 MinDomain
 *    REAL*8 MaxDomain
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecGUISpinTextFieldIncDouble(LgIndex_t   SpinTextFieldID,
                                                           const char *Format,
                                                           double      Increment,
                                                           double      MinDomain,
                                                           double      MaxDomain);

/* Dynamic option menus */
/**
 * Delete an item in an option menu control.
 *
 * @param OptionMenuID
 *   ID of the option menu
 *
 * @param Position
 *   One-based index of the item to delete.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIOptionMenuDeleteItemAtPos(
 *         OptionMenuID,
 *         Position)
 *    INTEGER*4 OptionMenuID
 *    INTEGER*4 Position
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIOptionMenuDeleteItemAtPos(LgIndex_t OptionMenuID,
                                                         LgIndex_t Position);

/**
 *   Appends an item to an option menu control.
 *
 * @param OptionMenuID
 *   ID of the option menu control.
 *
 * @param Item
 *   New option menu item. Must be a valid string with length greater than zero
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIOptionMenuAppendItem(
 *         OptionMenuID,
 *         Item)
 *    INTEGER*4 OptionMenuID
 *    CHARACTER*(*) Item
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIOptionMenuAppendItem(LgIndex_t   OptionMenuID,
                                                    const char *Item);

/**
 *   Get the number of items in an option menu.
 *
 * @param OptionMenuID
 *   ID of the option menu
 *
 * @return
 *   Number of items in the option menu.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIOptionMenuGetItemCount(OptionMenuID)
 *    INTEGER*4 OptionMenuID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIOptionMenuGetItemCount(LgIndex_t OptionMenuID);

/**
 *   Remove all items from an option menu.
 *
 * @param OptionMenuID
 *   ID of the option menu control
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIOptionMenuDeleteAllItems(OptionMenuID)
 *    INTEGER*4 OptionMenuID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIOptionMenuDeleteAllItems(LgIndex_t OptionMenuID);

/**
 * Get the text of an item in an option menu.
 *
 * @param OptionMenuID
 *   ID of the option menu
 *
 * @param Position
 *   One-based index of the option menu item for which the text is desired.
 *
 * @return
 *   The text of the item at the specified position in the option menu control. The position is a
 *   one-based index. You must call TecUtilStringDealloc() to free this string.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIOptionMenuGetString(
 *         OptionMenuID,
 *         Position,
 *         Result,
 *         ResultLength)
 *    INTEGER*4 OptionMenuID
 *    INTEGER*4 Position
 *    CHARACTER*(*) Result
 *    INTEGER*4 ResultLength
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON char * STDCALL TecGUIOptionMenuGetString(LgIndex_t OptionMenuID,
                                                     LgIndex_t Position);

/**
 *   Replace the text of an item in an option menu control.
 *
 * @param OptionMenuID
 *   ID of the option menu
 *
 * @param NewText
 *   New text of the item. Must be a valid string with a length greater than zero
 *
 * @param Position
 *   The one-based index of the item.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIOptionMenuReplaceItem(
 *         OptionMenuID,
 *         NewText,
 *         Position)
 *    INTEGER*4 OptionMenuID
 *    CHARACTER*(*) NewText
 *    INTEGER*4 Position
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIOptionMenuReplaceItem(LgIndex_t   OptionMenuID,
                                                     const char *NewText,
                                                     LgIndex_t   Position);


/* Scale */
/**
 *   Turns numeric display of a scale on or off. This function may be called at any time.
 *
 * @param ScaleID
 *   ID of the scale
 *
 * @param ShowDisplay
 *   Use TRUE to show the numeric display, FALSE to hide it
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIScaleShowNumericDisplay(
 *         ScaleID,
 *         ShowDisplay)
 *    INTEGER*4 ScaleID
 *    INTEGER*4 ShowDisplay
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIScaleShowNumericDisplay(LgIndex_t ScaleID,
                                                       Boolean_t ShowDisplay);


/* Sidebar */
/**
 *   Creates and registers the specified sidebar by name with Tecplot. Registered sidebars are placed
 *   under Tecplot's Workspace->Sidebar menu.
 *
 * @param SidebarName
 *   Name used to identify the sidebar from within Tecplot's interface.
 *
 * @param AddOnID
 *   Add-on ID of the add-on registering the sidebar.
 *
 * @param Width
 *   Desired sidebar width. Note that this value is a hint to Tecplot and, depending on other options,
 *   may or may not be obeyed.
 *
 * @param Height
 *   Desired sidebar height. Note that this value is a hint to Tecplot and, depending on other options,
 *   may or may not be obeyed.
 *
 * @param ActivateCallback
 *   Function that Tecplot will call before the sidebar is activated. Set to NULL if knowledge of the
 *   sidebar activation is not needed.
 *
 * @param DeactivateCallback
 *   Function that Tecplot will call after the sidebar is deactivated. Set to NULL if knowledge of the
 *   sidebar deactivation is not needed
 *
 * @return
 *   The control ID of the newly created and registered sidebar or BADGUIID if
 *   Tecplot failed to register the sidebar.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUISidebarRegister(
 *         SidebarName,
 *         AddOnIDPtr,
 *         Width,
 *         Height,
 *         ActivateCallback,
 *         DeactivateCallback)
 *    CHARACTER*(*) SidebarName
 *    POINTER (AddOnIDPtr, AddOnID)
 *    INTEGER*4 Width
 *    INTEGER*4 Height
 *    POINTER (ActivateCallbackPtr, ActivateCallback)
 *    POINTER (DeactivateCallbackPtr, DeactivateCallback)
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUISidebarRegister(const char            *SidebarName,
                                                    AddOn_pa               AddOnID,
                                                    LgIndex_t              Width,
                                                    LgIndex_t              Height,
                                                    TecGUIVoidCallback_pf  ActivateCallback,
                                                    TecGUIVoidCallback_pf  DeactivateCallback);

#if defined (MSWIN_MFC)
/**
 *   Creates and registers the specified MFC sidebar by name with Tecplot. Registered sidebars are placed
 *   under Tecplot's Workspace->Sidebar menu. This function is available in Windows only.
 *
 * To use an MFC CDialogBar object as a sidebar:
 *
 *   1. Derive a class from CDialogBar and create a dialog Template.
 *
 *   2. Create a callback function (of type TecGUIMFCCreateDialogBar_pf). This callback
 *      should do nothing but allocate your CDialogBar derived object. You must not
 *      call MANAGESTATE or AFX_MANAGE_STATE in this callback.
 *
 *   3. Call TecGUIMFCSidebarRegister() to attach your sidebar to the Tecplot Window.
 *
 *   4. You are responsible for managing all dialog control callbacks on your sidebar.
 *
 *   5. Tecplot is responsible for creating and managing the visiblity and position of your sidebar. Thus,
 *      do NOT call call any MFC function which changes the visibility or the position of your
 *      sidebar.
 *
 * @param SidebarName
 *   Name used to identify the sidebar from within Tecplot's interface.
 *
 * @param DialogBarCreateCallback
 *   Callback function which will allocate the CDialogBar object.
 *   In this callback you must allocate (using C++ new) the CDialogBar object.
 *   Tecplot will call this callback and then create and attach the CDiaogBar object
 *   to Tecplot's main window.
 *
 * @param ActivateCallback
 *   Function that Tecplot will call before the sidebar is activated. Set to NULL if knowledge of the
 *   sidebar activation is not needed.
 *
 * @param DeactivateCallback
 *   Function that Tecplot will call after the sidebar is deactivated. Set to NULL if knowledge of the
 *   sidebar deactivation is not needed
 *
 * @return
 *   The control ID of the newly created and registered sidebar or BADGUIID if
 *   Tecplot failed to register and/or create the sidebar.
 *
 * <FortranSyntax>
 *   This function is not available to FORTRAN add-ons.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_all, exclude_fglue, exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIMFCSidebarRegister(const char *SidebarName,
                                                       TecGUIMFCAllocDialogBar_pf DialogBarCreateCallback,
                                                       TecGUIVoidCallback_pf  ActivateCallback,
                                                       TecGUIVoidCallback_pf  DeactivateCallback);


/**
 * Calls UpdateData() for the current MFC sidebar. If an addon needs to call UpdateData() on
 * an MFC sidebar, it must be called via this function. This function is available in Windows only.
 *
 * @param bSaveAndValidate
 *   See the MFC documentation for CWnd::UpdateData()
 *
 * <FortranSyntax>
 *   This function is not available to FORTRAN add-ons.
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_fglue, exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIMFCSidebarUpdateData(Boolean_t bSaveAndValidate);
#endif /* MSWIN_MFC */


/**
 *   Activates the specified sidebar replacing the current one.  If called with
 *   TECGUITECPLOTSIDEBAR, the default Tecplot sidebar is activated replacing
 *   the current one.
 *
 * @param SidebarID
 *   ID of the sidebar to activate.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUISidebarActivate(SidebarID)
 *    INTEGER*4 SidebarID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUISidebarActivate(LgIndex_t SidebarID);
/**
 *   Deactivates any activate sidebar causing it to no longer be visible.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUISidebarDeactivateAll()
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUISidebarDeactivateAll(void);


/**
 *   Given a sidebar ID, return TRUE if the sidebar is currently active, FALSE otherwise.
 *
 * @param SidebarID
 *   Sidebar ID. Use TECGUITECPLOTSIDEBAR to check if the Tecplot sidebar is active.
 *
 * @return
 *   TRUE if the sidebar is active, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUISidebarIsActive(SidebarID)
 *    INTEGER*4 SidebarID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Check if the Tecplot sidebar is active:
 *
 * @code
 *   Boolean_t TecplotSidebarIsActive=TecGUISidebarIsActive(TECGUITECPLOTSIDEBAR);
 * @endcode
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON Boolean_t STDCALL TecGUISidebarIsActive(LgIndex_t SidebarID);

/**
 *   Gets the number of items that the list control can visibly display. This number is dependant on
 *   the height of the list control.
 *
 * @param ListID
 *   ID of the list control
 *
 * @return
 *   The number of items that the list box can visually display. This value is approximately equal to
 *   the height of the list box divided by the font height.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIListGetCapacity(ListID)
 *    INTEGER*4 ListID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Change the selected item to appear in the middle of the list box:
 *
 * @code
 *   LgIndex_t SelectedItem = TecGUIListGetSelectedItem(ID)
 *   LgIndex_t ListCapacity = TecGUIListGetCapacity(ID)
 *   LgIndex_t NewTopItemPos = SelectedItem - ListCapacity / 2;
 *   if (NewTopItemPos > 0)
 *     TecGUIListSetTopItemPos(ID,NewTopItemPos);
 *   else
 *     // Current selected item is already visible and cannot be moved to the center of the list
 * @endcode
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIListGetCapacity(LgIndex_t ListID);
/**
 *   Gets the index of the first visible item in a list box. This function will assert if there are no
 *   items in the list box.
 *
 * @param ListID
 *   List control ID
 *
 * @return
 *   The index of the first visible item in a list box. This list box must contain at least one item.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecGUIListGetTopItemNum(ListID)
 *    INTEGER*4 ListID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Make the item at index 10 visible if it is not already.
 *
 * @code
 *   if ( TecGUIListGetTopItemNum(ListID) > 10 )
 *     TecGUIListSetTopItemNum(ListID,10);
 * @endcode
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON LgIndex_t STDCALL TecGUIListGetTopItemNum(LgIndex_t ListID);
/**
 *   Scrolls the list box until either the item specified appears at the top of the list box or the
 *   maximum scroll range has been reached.
 *
 * @param ListID
 *   List Control ID
 *
 * @param ItemNum
 *   New top item index
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIListSetTopItemNum(
 *         ListID,
 *         ItemNum)
 *    INTEGER*4 ListID
 *    INTEGER*4 ItemNum
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 *   Make the item at index 10 visible if it is not already.
 *
 * @code
 *   if ( TecGUIListGetTopItemNum(ID) > 10 )
 *     TecGUIListSetTopItemNum(ID,10)
 * @endcode
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIListSetTopItemNum(LgIndex_t ListID,
                                                 LgIndex_t ItemNum);

/**
 * Replaces the contents of the list control with the items in the string list.
 *
 * @since
 *   14.1.0.33299
 *
 * @param listID
 *   ID of the list control.
 *
 * @param stringList
 *   A string list with the items to set in the list control.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUIListSetItems(
 *        listID,
 *        stringList)
 *    INTEGER*4 listID
 *    INTEGER*4 stringList
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUIListSetItems(LgIndex_t     listID,
                                            StringList_pa stringList);


/**
 * Set the input focus to a GUI control.
 *
 * @param ControlID
 *   The control id.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecGUISetInputFocus(ControlID)
 *    INTEGER*4  ControlID
 * </FortranSyntax>
 *
 * <PythonSyntax>
 * </PythonSyntax>
 *
 * @ingroup TGB
 *
 * #internalattributes exclude_sdkdoc
 */
LINKTOADDON void STDCALL TecGUISetInputFocus(LgIndex_t ControlID);

