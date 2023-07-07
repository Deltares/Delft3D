#ifndef _ADDON_H
#define _ADDON_H

/* CORE SOURCE CODE REMOVED */

/**
 * Use this statechange callback API if you want the callback function to
 * receive client data.
 *
 * @param StateChange
 *   Identifies the state that changed.
 *
 * @param ClientData
 *   Client data supplied in TecUtilStateChangeAddCallbackX.
 *
 * <FortranSyntax>
 *   SUBROUTINE MyStateChangeCallback(
 *  &            StateChange,
 *  &            CallDataPtr)
 *   INTEGER*4 StateChange
 *   POINTER   (CallDataPtr, DummyClientData)
 * </FortranSyntax>
 */
typedef void (STDCALL * StateChangeAddOnCallbackWithClient_pf)(StateChange_e StateChange,
                                                               ArbParam_t    ClientData);

/**
 * This callback will be called whenever there is a state change in Tecplot.
 * Use one or more of the TecUtilStateChangeGetXXXX functions to retrieve
 * supplemental information corresponding the the current state change.  See
 * The ADK users manual for more information.
 *
 * @param StateChange
 *   Identifies the state that changed.
 *
 * <FortranSyntax>
 *   SUBROUTINE MyStateChangeV2Callback(
 *  &            StateChange)
 *   INTEGER*4 StateChange
 * </FortranSyntax>
 */
typedef void (STDCALL * StateChangeAddOnCallbackV2_pf)(StateChange_e StateChange);




/**
 * This is the earlier version (before version 10) of the State Change Callback
 * function. This callback will be called whenever there is a state change in
 * Tecplot.
 *
 * @param StateChange
 *   Identifies the state that changed.
 *
 * @param CallData
 *   Provides further information on the state change. The possible values for
 *   StateChange and the meaning of CallData for specific StateChange values are
 *   all described in the ADK Users manual.
 *
 * <FortranSyntax>
 *   SUBROUTINE MyStateChangeCallback(
 *  &            StateChange,
 *  &            CallDataPtr)
 *   INTEGER*4 StateChange
 *   POINTER   (CallDataPtr, DummyCallData)
 * </FortranSyntax>
 */
typedef void (STDCALL * StateChangeAddOnCallback_pf)(StateChange_e StateChange,
                                                     ArbParam_t    CallData);




/**
 * Execute an extended macro command.
 * This callback is responsible for performing the macro command action when
 * the $!EXTENDEDCOMMAND macro command associated with the
 * CommandProcessorIDString is processed.
 *
 * @return
 *   Return TRUE if the macro command completed successfully otherwise FALSE.
 *   If FALSE then using TecUtilStringAlloc() you must allocate space for, and
 *   populate *ErrMsg with an error message.
 *
 * @param CommandString
 *   The command string registered with the callback. The syntax for this
 *   string is determined by the callback designer.
 *
 * @param ErrMsg
 *   If this callback function's result is TRUE, *ErrMsg must be assigned NULL
 *   otherwise if the callback function's result is FALSE *ErrMsg must be
 *   assigned a string allocated by TecUtilStringAlloc() whose contents states
 *   the nature of the problem. Tecplot is responsible for releasing the error
 *   message string after displaying the error.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION MyMacroCommandCallback(
 *   &                   CommandString,
 *   &                   ErrMsgString)
 *    CHARACTER*(*)   CommandString
 *    CHARACTER*(*)   ErrMsgString
 * </FortranSyntax>
 */
typedef Boolean_t (STDCALL * MacroCommandExtCallback_pf)(char*           CommandString,
                                                         TP_GIVES char** ErrMsg);

/* DEPRECATED */
typedef Boolean_t (STDCALL * MacroCommandAddOnCallback_pf)(char*           CommandString,
                                                           TP_GIVES char** ErrMsg);



/**
 * This callback is called when tecplot is in the initial phases of quitting.
 *
 * @return
 *   Return FALSE if you want to stop tecplot from quitting.  Return TRUE if
 *   your addon is ok with allowing tecplot to quit.
 *
 * <FortranSyntax>
 * INTEGER*4 FUNCTION MyMopupQueryAddOnCallback()
 * </FortranSyntax>
 */
typedef Boolean_t (STDCALL * MopupQueryAddOnCallback_pf)(void);




/**
 *  This callback is not available as yet in the current API.
 */
typedef Boolean_t (*ForeignLibLoader_pf)(const char* LibraryName,
                                         const char* InitFunctionName,
                                         ArbParam_t  ClientData);


/* CORE SOURCE CODE REMOVED */

#endif /* _ADDON_H */
