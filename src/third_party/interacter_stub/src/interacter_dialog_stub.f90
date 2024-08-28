!> @file interacter_dialog_stub.f90
!! A stub library with all Interacter subroutine and function interfaces.
!! All routines ares empty, no code is executed, this is just for linking
!! with programs that call Interacter, but may not need it in batch mode.
!!
!! Originally from InteracterDialogEmulatorClass.f90

!! $Id: interacter_dialog_stub.f90 31265 2013-12-05 08:35:56Z dam_ar $

subroutine IFormDefaults(IFDEFS)
  integer IFDEFS ! Default settings to activate
  ! 1 : Attributes/colours
  ! 2 : Input parameters
  ! 3 : Frame style
  ! Activates default settings specified in a form definition file in a [defaults] section. The
  ! current form must have been loaded via IFormLoad. The value of IFDEFS
  ! corresponds to one of the records in the [defaults] section. This mechanism allows form
  ! definition files to specify 'global' settings for colours, etc. without hardcoding them into
  ! the application.
  ! Calling IFormDefaults is directly equivalent to calling the following routines,dependent on whether the corresponding default settings have been specified in the
  ! [defaults] section :
  ! IFDEFS = 1 : ITextAttribute/ITextColour
  ! IFDEFS = 2 : InCursorPos/InsertOver/InTypeWipe/etc.
  ! IFDEFS = 3 : IFrameType and IFrameOptions(6/7,N)
  ! It therefore updates the current global settings for colour, etc. Hence, it should be called
  !after IFormLoad and before any routines which cause screen output or input (e.g.
  !IFormShow, IFormEdit, etc.).
  !Form Creation & Editing INTERACTER Subroutine Reference
  !3-4
  !Note that a default frame type is not used by the forms manager itself, but can still be
  !useful to determine the style of a window frame for the form, if opened subsequently
  !via IFormWindow, IWinOpen, etc. The other parameters set by IFDEFS=3 (i.e. the
  !primary/secondary frame colours) can be used by the forms manager, to determine the
  !colour of any field frames for which the colour is set to 'use the default'.
  !e.g. CALL IFormLoad('W','myform.ifd',LFNERR)
  !C activate default attributes/colours
  !CALL IFormDefaults(1)
  !C activate default input parameters
  !CALL IFormDefaults(2)
  !C display form
  !CALL IFormShow
  !C edit form
  !CALL IFormEdit(IFINIT,IFEXIT)
end subroutine IFormDefaults

subroutine IFormDefine(TYPE,NFIELD,IX,IY,IWIDTH,ITYPE)
  character(len=*) TYPE ! Type of form
  ! = W : Window (single form)
  ! = S : Full Screen (single form)
  ! = T : Tabbed (multi-part form)
  integer NFIELD ! Number of fields (1 to max. number of fields)
  integer IX(NFIELD) ! Array of field column positions
  integer IY(NFIELD) ! Array of field row positions
  integer IWIDTH(NFIELD) ! Array of field widths
  integer ITYPE(NFIELD) ! Array of field types :
  ! = 1 : Unprotected string ) Add
  ! = 2 : Unprotected integer ) 1000
  ! = 3 : Unprotected real ) for
  ! = 4 : Cycling Menu ) protected
  ! = 5 : Push button ) fields
  ! = 6 : Unprotected double )
  ! precision )
  ! = 7 : Vertical menu )
  ! = 8 : Unprotected long-string )
  ! = 9 : Check box )
  ! = 10 : Check box description )
  ! Short-name : FMFORM
  ! Defines a new form. All previous form information is deleted. IFormDefine should
  ! be called to create a new form before calling other routines in the FM group. (To load a
  ! form definition from a file see the alternative routine IFormLoad).
  ! Forms can operate relative to the currently selected window or the full screen. The field
  ! co-ordinates should be defined in the IX and IY arrays accordingly, depending on
  ! TYPE. A tabbed form is treated as a multi-part window-based form. In this context, the
  ! 'current' window means the window which is currently selected for output when calling
  ! IFormShow, IFormShowUnp, etc. rather than that which is selected when
  ! IFormDefine is called. Typically a window is selected for output by calling routines
  ! such as IWinOpen or IWinSelect.
  ! INTERACTER Subroutine Reference Form Creation & Editing
  ! 3-5
  ! When TYPE is set to W or S, a single form is created. All of the form is visible at one
  ! time. Alternatively, a tabbed form can be created which consists of multiple windowed
  ! forms which are selectable via tabs at the edge of the form. In this case NFIELD
  ! specifies the total number of fields across all of the sub-forms. The contents of the IX,IY, IWIDTH and ITYPE arrays should then be organised into sub-form order. Hence,
  ! all the fields for sub-form 1 should be at the start of these arrays, followed by the fields
  !for sub-form 2 and so on. Any fields which are needed on all sub-forms (e.g.
  !OK/Cancel) buttons should be placed at the very end of these arrays. Call
  !IFormDefineTabs after IFormDefine to identify the last field on each sub-form
  !and the tab labels.
  !The maximum number of fields which can be defined is documented at the start of this
  !chapter. If NFIELD is larger than the maximum value, additional field definitions are
  !simply ignored. InfoForm can be called to check the number of available fields in the
  !current implementation. Field numbers are reset to 1 to NFIELD, though field
  !identifiers can be reassigned using IFormIdentifier.
  !Various field types can be selected. Type values less than 1000 are treated as
  !unprotected fields, whilst values greater than 1000 are assumed to be protected. Pushbutton
  !and check box fields should be unprotected to allow them to operate correctly.
  !Field types 1 and 8 (strings and long-strings) are equivalent except that an unprotected
  !long-string field allows left/right scrolling within the field, i.e. the enterable string
  !length is longer than the physical field width. Field type 10 is similar to field type 1, but
  !it is not editable and acts as a label for a check box field.
  !Calling IFormDefine suppresses the help-field feature. This can be activated by
  !calling IFormHelp following the call to IFormDefine.
  !IFormDefine generates no screen output. It clears all information from the internal
  !data area which is used to store form related information. Information about individual
  !field position, size and type can be retrieved subsequently via InfoField.
  !Form Creation & Editing INTERACTER Subroutine Reference
  !3-6
  !e.g. PARAMETER ( NFIELD = 9)
  !INTEGER IX(NFIELD),IY(NFIELD),IWIDTH(NFIELD),ITYPE(NFIELD)
  !character(len=4) OPTION(2)
  !character(len=8) LABEL
  !DOUBLE PRECISION DVAL
  !DATA IX / 1,16, 1,16, 1,16,10, 1,16/
  !DATA IY / 1, 1, 3, 3, 5, 5, 7, 9, 9/
  !DATA IWIDTH/ 15, 8, 15, 8, 6, 8, 4, 15,12/
  !DATA ITYPE /1001, 2,1001, 3,1001, 1, 4,1001, 6/
  !DATA OPTION/'Slow','Fast'/
  !NVALUE = 1
  !START = 0.0
  !ISPEED = 1
  !LABEL = ' '
  !CALL IScreenOpen(' ','T',80,25,16)
  !C create a new form
  !CALL IFormDefine('W',NFIELD,IX,IY,IWIDTH,ITYPE)
  !C put data into form fields
  !CALL IFormPutString(1,'No. of values')
  !CALL IFormPutInteger(2,NVALUE)
  !CALL IFormPutString(3,'Start value')
  !CALL IFormPutReal(4,START,'(F8.2)')
  !CALL IFormPutString(5,'Label')
  !CALL IFormPutMenu(7,OPTION,2,ISPEED)
  !CALL IFormPutString(8,'Double prec:')
  !C display form in a window & allow user to edit it
  !CALL IWinOpen(0,0,30,7)
  !CALL IFormShow
  !CALL IFormEdit(1,IFEXIT)
  !CALL IWinClose(1)
  !C retrieve values from form
  !CALL IFormGetInteger(2,NVALUE)
  !CALL IFormGetReal(4,START)
  !CALL IFormGetString(6,LABEL)
  !CALL IFormGetMenu(7,ISPEED)
  !CALL IFormGetDouble(9,DVAL)

  !TYPE,NFIELD,IX,IY,IWIDTH,ITYPE
  ! = 1 : Unprotected string ) Add
  ! = 2 : Unprotected integer ) 1000
  ! = 3 : Unprotected real ) for
  ! = 4 : Cycling Menu ) protected
  ! = 5 : Push button ) fields
  ! = 6 : Unprotected double )
  ! precision )
  ! = 7 : Vertical menu )
  ! = 8 : Unprotected long-string )
  ! = 9 : Check box )
  ! = 10 : Check box description )

  !        type(Fields), pointer :: this
  !        type(Property), pointer :: p
  !        integer i
  !        this => this_
  !
  !        if (this%propCount > 0) deallocate(this%props)
  !        this%propCount = NFIELD /2
  !        allocate(this%props(0:this%propCount + 100 - 1))
  !        do i = 0, this%propCount - 1
  !            p => this%props(i)
  !            select case(ITYPE((i + 1) * 2))
  !                case (1)
  !                  p%typ = PROP_STRING
  !                case (2)
  !                  p%typ = PROP_INT
  !                case (3)
  !                  p%typ = PROP_FLOAT
  !            end select
  !        end do
end subroutine IFormDefine

subroutine IFormDefineBox(NBOX,IX,IY,IW,IH)
  integer NBOX ! Number of boxes (1 to max. number of fields
  ! less the actual number of fields)
  integer IX(NBOX) ! Array of form-box left-hand column positions
  integer IY(NBOX) ! Array of form-box top-row positions
  integer IW(NBOX) ! Array of form-box widths
  integer IH(NBOX) ! Array of form-box heights
  ! Defines the layout of any form-boxes which are to be displayed as part of the current
  ! form. All previous form-box information is deleted.
  ! Form-box co-ordinates defined in the IX and IY arrays will either be relative to the
  ! current window or to the full screen depending on the TYPE parameter specified to
  ! IFormDefine or IFormLoad.
  ! If a tabbed form is selected, the form-boxes should be grouped in sub-form order. Any
  ! boxes which are to appear on all sub-forms should be defined at the end of the supplied
  ! arrays. See IFormDefineTabs.
  ! The maximum number of form-boxes which can be defined is determined by the
  ! maximum number of form fields (documented at the start of this chapter) less the actual
  ! number of form fields.
  ! Form-box numbers are reset to 1 to NBOX, though new identifiers can be reassigned
  ! using IFormIdentifierBox.
  ! INTERACTER Subroutine Reference Form Creation & Editing
  ! 3-7
  ! Form-boxes are either frames or horizontal/vertical lines. The latter have either a height
  ! or width of 1.
  ! IFormDefineBox generates no screen output. Use IFormShow or
  ! IFormShowBox to actually display the boxes. IFormBox/IFormBoxN can be
  ! used to specify their frame type and colours.
  !e.g. PARAMETER ( NBOX = 3 )
  !INTEGER IX(NBOX),IY(NBOX),IW(NBOX),IH(NBOX)
  !DATA IX/ 1, 1, 9/
  !DATA IY/ 1, 5, 1/
  !DATA IW/ 15, 15, 1/
  !DATA IH/ 9, 1, 9/
  !CALL IFormDefineBox(NBOX,IX,IY,IW,IH)
  !CALL IFormShow
end subroutine IFormDefineBox

subroutine IFormDefineTabs(EDGE,NTABS,LABELS,LIMITF,LIMITB)
  character(len=*) EDGE ! Edge to show tabs on :
  ! T/R/B : Top/Right/Bottom
  integer NTABS ! Number of tabs/sub-forms
  character(len=*) LABELS(NTABS) ! Array of tab labels
  integer LIMITF(NTABS) ! Array of last-field identifiers
  integer LIMITB(NTABS) ! Array of last-box identifiers
  ! Defines the layout of a tabbed multi-part form. This routine should be called after
  ! IFormDefine('T',..). The fields specified to IFormDefine should be
  ! grouped into sub-forms, with any global fields defined at the end of the field list.
  ! EDGE specifies where the form tabs will appear. If the tabs are placed at the top or
  ! bottom of the form, they will be laid out in either one or two rows, depending on the
  ! width of the form window and the significant lengths of the tab labels specified in
  ! LABELS. If the tabs are placed at the right of the form they appear as a single column
  ! of tabs. In all cases, only as many tabs as will fit in the window are actually displayed.
  ! The LIMITF array describes the identifiers of the last field which lies on each subform.
  ! Effectively this just divides up the list of fields specified to IFormDefine into
  ! sub-forms. If IFormIdentifier has not been called since IFormDefine,LIMITF simply specifies array subscripts, as supplied to IFormDefine.
  ! The LIMITB array is equivalent to LIMITF, but for field boxes. It describes the
  ! identifiers of the last boxes (if any) on each subform. If a particular sub-form contains
  ! no boxes, set that LIMITB value to zero or the same value as the previous sub-form.
  ! As for fields, form-boxes should be grouped into sub-form order in a multi-part tabbed
  ! form, with any common boxes appearing at the end of the list, after the sub-form
  ! specific boxes.
  !e.g. PARAMETER ( NTABS = 3 )
  !character(len=7) LABELS(NTABS)
  !INTEGER LIMITF(NTABS),LIMITB(NTABS)
  !DATA LABELS/'Style','Options','Values'/
  !DATA LIMITF/ 20, 40, 60/
  !DATA LIMITB/ 0, 0, 0/
  !CALL IFormDefine('T',NFIELD,IX,IY,IWIDTH,ITYPE)
  !CALL IFormDefineTabs('T',NTABS,LABELS,LIMITF,LIMITB)
  !Form Creation & Editing INTERACTER Subroutine Reference
  !3-8
end subroutine IFormDefineTabs

subroutine IFormEdit(IFINIT,IFEXIT)
  integer IFINIT ! Number of field in which to place cursor initially
  integer IFEXIT ! Number of field in which exit key was pressed
  ! (-999 if all fields protected)
  ! Short-name : FMEDIT
  ! Allows the current form to be edited. The form should already have been displayed
  ! using IFormShow. Think of IFormShow as an output operation and IFormEdit as
  ! an input operation.
  ! The cursor is placed initially on the field specified by IFINIT. If this is a protected
  ! field, IFormEdit will move the cursor to the next unprotected field in the sequence
  ! defined to IFormDefine. If all fields on the form are protected, IFormEdit will
  ! normally exit immediately, returning a negative IFEXIT value and setting an error
  ! code. The exception is tabbed forms, where it is possible to switch between sub-forms
  ! to view their contents.
  ! Since IFormEdit uses input routines such as InStringXYDef, InRealXYDef,InIntegerXYDef, etc. the user enters data in each field in the usual way. Whatever
  ! the field type, the current input field will be highlighted in the manner defined by the
  ! InHighlight routine. All the field editing control keys such as Insert and Delete
  ! work as normal, except for :
  ! �  Cycling menu fields. These use IMenuCycle by default, where the left/right
  ! cursor keys or the space bar cycle through the available menu options. If a pop-up
  ! menu has been enabled in a menu field, pressing control key 34 will toggle between
  ! a simple cycling menu and a vertical menu using IMenuScroll or
  ! IMenuVertic.
  ! �  Vertical menu fields. These use IMenuScroll, where the up/down cursor keys or
  ! the space bar move the highlight through the available menu options.
  ! �  Push button fields. It is only possible to tab in/out of these fields. Pressing confirm
  ! whilst the highlight is on such a field returns the exit-key code associated with that
  ! field.
  ! �  Check box fields. It is only possible to tab in/out of these fields. Pressing control
  ! key 35 ('next item' : usually the space bar) toggles the check box state. If the check
  ! box is attached to a check box description field the highlight actually moves
  ! through the description field rather than the check box itself.
  ! Where a mouse is available, a mouse cursor is automatically enabled on entry. It can be
  ! used within an enterable field to reposition the text input cursor. It can also be used in a
  ! cycling menu or check box field to cycle the available options. In the case of a cycling
  ! menu, the right mouse button which will cause the menu to pop up, if enabled via
  ! IFormPopUpMenu or the IFD file. The actual button used for this function is
  ! definable via InMouseOptions(102,n). If a cycling menu has a frame, a pop-up
  ! button (a downward arrow) will appear in the right-hand edge of the frame. The user
  ! can click on this button to display the pop-up menu. In a vertical menu field, clicking
  ! on a menu option moves the menu highlight to that option.
  ! Alternatively, the mouse can be used to click on any other unprotected field. This has
  ! one of two effects :
  ! INTERACTER Subroutine Reference Form Creation & Editing
  ! 3-9
  ! 1) If it is a push button field, IFormEdit will exit with InfoInput(55) set to the
  ! exit code associated with that field. For example, this might be an 'OK' button with
  ! an associated exit code of 21 (i.e. the same as the 'confirm' control key). Selection
  ! of a push button field takes place either on a button-down or a button-release,dependent on the mouse 'feel' setting (see InMouseOptions(3,n)). The latter
  ! is the default on most displays.
  ! 2) If it is any other type of field, the form highlight moves straight to that field. If it is
  ! a vertical menu field, that menu becomes 'active' a highlight appears in the menu at
  ! the currently selected option and border controls are displayed for mouse users.
  ! Clicking in a check box field (or on an attached description string field) will
  ! immediately toggle the state of the check box.
  ! Pressing a mouse button outside any unprotected field is ignored by default but this
  ! action can be changed via InMouseOptions(2,n). The mouse cursor is returned to
  ! its previous state on exit.
  ! In all fields, the 'exit' control keys are interpreted as follows :
  ! Control Key Action
  ! 21-23 and 26-28 exit from form
  ! 24 and 29 move to next unprotected field
  ! 25 and 30 move to previous unprotected field
  ! 36-70 exit from form
  ! In this sense 'next' and 'previous' fields refer to the order in which fields have been
  ! defined to IFormDefine or in the form definition file. Tab and back-tab will 'wrapround'
  ! at the end or beginning of a form.
  ! The 'spare' exit control keys numbered 26-30 perform the same functions as the
  ! equivalent control keys numbered 21-25. i.e. pressing any of the keys defined as control
  ! keys 26-28 will also exit the form whilst control keys 29 and 30 will act as additional
  ! tab keys. For example, you might wish to make the down/up cursor keys available as
  ! alternative tab/back-tab keys (see the example). The second group of 'spare' exit keys
  ! (36-70) have no special significance. They all simply exit from the form, if defined.
  ! If a pop-up menu has been enabled in a cycling menu field, control key 34 can be used
  ! to toggle a vertical pop-up menu on/off. When a pop-up menu has been activated in this
  ! way the behaviour of the various exit/control keys needs to be understood :
  ! �  Pressing confirm (key 21) within a pop-up menu, closes the pop-up menu and
  ! updates the field contents, but leaves the highlight in the field returning to the
  ! simple cycling menu format. Pressing control key 34 has the same effect.
  ! �  Pressing quit (key 23) or clicking outside the menu with the mouse has the same
  ! effect as confirm, except that the field contents are not updated.
  ! �  Pressing the tab/back-tab keys (24 & 25) or clicking in another field, whilst in a
  ! pop-up menu, closes that menu, updates the field contents and moves the form
  ! highlight accordingly.
  ! �  Pressing any of the remaining exit keys, closes the menu, updates the field contents
  ! and exits from IFormEdit.
  ! Form Creation & Editing INTERACTER Subroutine Reference
  ! 3-10
  ! On exit from IFormEdit, the exit key should be interrogated using
  ! InfoInput(55) in the usual manner. Normally, this will return a value in the range
  ! 21-23, 26-28 or 36-70 if a keyboard exit key was pressed. Alternatively it will return
  ! the exit key code associated with a push button field if a mouse button or confirm was
  ! pressed on such a field. The exit key number can be used in combination with IFEXIT
  ! which returns the number of the field in which the exit key was pressed to take field
  !dependent exit action (e.g. providing field sensitive help).
  !If a resize/expose event occurs in graphics mode in a windowing environment, the form
  !editor will terminate and InfoInput(55) will return a value of 259. The editor will
  !also terminate on a close-window request, returning InfoInput(55) set to 260.
  !If InMouseOptions(2,1/3) has been called and a mouse button has been pressed
  !outside of the 'input area', InfoInput(55) will return a value of -2. In this case, the
  !mouse button and position will be available via InfoInput(61-63) and/or
  !InfoGraphics(5)/(6). See InMouseOptions.
  !All of the above behaviour applies equally to tabbed forms. In addition, a number of
  !labelled tabs will appear at one edge of the form. The mouse can be clicked on these to
  !switch to the appropriate sub-form. Similarly, the Tab/Back-tab keys can be used to
  !move the highlight on/off the labelled tabs. In the latter case the left/right or up/down
  !cursor keys move the highlight through the tab labels. The confirm/Tab/Back-Tab keys
  !can be pressed on any tab to select that sub-form. Whichever method of sub-form
  !selection is used, switching between forms does not exit the form editor.
  !Application specific form processing can be performed using IFormEditUser, an
  !alternative entry point to IFormEdit which allows a user supplied routine to be
  !specified and called from within IFormEdit.
  !e.g. IFINIT = 1
  !IOLD29 = InfoInput(29)
  !IOLD30 = InfoInput(30)
  !CALL InControlKey(29,129)
  !CALL InControlKey(30,128)
  !CALL IOutJustifyNum('L')
  !CALL IFormShow
  !10 CALL IFormEdit(IFINIT,IFEXIT)
  !KEXIT = InfoInput(55)
  !CALL InControlKey(29,IOLD29)
  !CALL InControlKey(30,IOLD30)
  !IF (KEXIT.EQ.23) THEN
  !RETURN
  !ELSE IF (KEXIT.EQ.22) THEN
  !CALL HELP(IFEXIT)
  !GOTO 10
  !ELSE
  !CALL IFormGetInteger(2,IVAL)
  !ENDIF
  !Errors :
  !ErrNumToStr (18) : Numeric-to-string conversion error.
  !ErrAllProtected (30) : All fields protected
  !ErrFullDataArea (33) : Internal character data storage area full
  !INTERACTER Subroutine Reference Form Creation & Editing
  !3-11
end subroutine IFormEdit

subroutine IFormEditUser(IFINIT,IFEXIT,FMUSER)
  integer IFINIT ! Number of field in which to place cursor initially
  integer IFEXIT ! Number of field in which exit key was pressed
  ! (-999 if all fields protected)
  external FMUSER ! User supplied validation/customisation routine
  ! Short-name : FMEDIU
  ! Allows the user to edit the current form, in exactly the same manner as IFormEdit,but with the addition of a user definable processing option. IFormEditUser is an
  ! alternative entry point to IFormEdit with the addition of the FMUSER argument. The
  ! routine passed via the FMUSER argument can perform special processing 'on the fly'
  ! within IFormEdit rather than waiting until the user presses a form-exit key.
  ! Each time the user 'tabs' to another field or presses an exit key, FMUSER is called to
  ! allow special checks to be performed before moving to the next field or leaving the
  ! form altogether. This gives FMUSER the chance to cross validate multiple fields,display warning messages, etc. FMUSER can also modify the default action which
  ! IFormEdit would otherwise take as a result of the users input. The FMUSER userroutine
  ! takes two arguments which have specific meanings on entry and exit :
end subroutine IFormEditUser

subroutine FMUSER (IFIELD)
  integer IFIELD ! Field number
  !integer IEXITK ! 'Exit' key, not used right now.
  ! On entry, IFIELD specifies the field in which the user has just pressed an 'exit' key
  ! (which may have been a next/previous field key). IEXITK specifies the control key
  ! number of that key. It will be set to one of the following values on entry :
  ! IEXITK = 21-30 or 36-70
  ! See IFormEdit for the meaning of these exit key codes. The entry value of IEXITK
  ! is the same as would be returned by InfoInput(55).
  ! IEXITK = 259
  ! An expose/resize event occurred in graphics mode in a windowing environment. It is up
  ! to the application what action to take. IEXITK should only be reset to zero if the
  ! screen is repainted from within the user routine.
  ! IEXITK = 260
  ! A close-window program termination request has been received. The user routine
  ! should normally return this code unchanged to allow termination action to be handled
  ! by the calling program. (See InEventSelect(7,n)).
  ! IEXITK = -2
  ! A mouse button has been pressed outside the current field (regardless of the current
  ! InMouseOptions(2,n) setting). InfoInput(61-63) returns the mouse button
  ! and position in this case.
  ! Form Creation & Editing INTERACTER Subroutine Reference
  ! 3-12
  ! IEXITK = -3
  ! A mouse button has been clicked in the current field which is a vertical menu or a
  ! check.box. The form editor will re-enter the field in this situation, but first gives
  ! FMUSER the opportunity to 'see' the updated field value selection.
  ! If required, the identifier of the 'next' field (i.e. the one to which IFormEditUser is
  ! about to pass control) is accessible via InfoForm(3). In a multi-part tabbed form,the current form number is available via InfoForm(8).
  ! On exit, the return values of IEXITK and (optionally) IFIELD, control the behaviour
  ! of IFormEdit in the following manner :
  ! IEXITK returned as zero :
  ! If IFIELD is a valid, unprotected field, the data entry highlight moves straight to this
  ! field. To force the user to re-edit the current field, leave IFIELD unchanged.
  ! Otherwise, this feature can be used to make the user edit a related field or to force the
  ! highlight to move round the form in an order which is different to the default. If
  ! IFIELD is an invalid field or is protected, the data entry highlight remains on the
  ! current field.
  ! IEXITK returned as 21-30 or 36-70 :
  ! IFormEdit will continue processing as though the control key specified by IEXITK
  ! had been pressed. This effectively updates the 'exit key' code which will be returned by
  ! InfoInput(55). Leave IEXITK unchanged if you wish IFormEdit to continue
  ! processing in the normal manner.
  ! IEXITK returned as 259 or 260 :
  ! IEXITK should only be returned with a value of 259/260 if it was set to this value on
  ! entry.
  ! IEXITK returned as -2 :
  ! If IEXITK was set to -2 on entry it can safely be returned with this value, allowing
  ! IFormEdit to take the appropriate action according to where the mouse button was
  ! pressed. However, IEXITK must not be returned set to -2 if it was not set to this value
  ! on entry.
  ! IEXITK returned as -3 :
  ! IEXITK should only be returned with a value of -3 if it was set to this value on entry.
  ! IEXITK returned with any other value :
  ! The returned IEXITK code will be ignored. IFormEdit will continue processing the
  ! form based on the exit key code which was passed in to FMUSER.
  ! On return from FMUSER, IFormEdit always updates the on-screen contents of the
  ! field which the user has just entered or tabbed across (i.e. the field specified by the
  ! value of IFIELD on entry to FMUSER). As a result, FMUSER can update the contents
  ! of the current field using the appropriate IFormPut routine without needing to call
  ! IFormShowField to update the screen display.
  ! Behaviour in a tabbed form is essentially the same, except for the added consideration
  ! of what happens when tabs are selected. The following conditions can arise, in which
  ! case the indicated values will be passed to the the user-routine :
  ! INTERACTER Subroutine Reference Form Creation & Editing
  ! 3-13
  ! (a) Selecting a new sub-form using the mouse
  ! IFIELD = Current field, as normal
  ! IEXITK = -2, as normal
  ! InfoForm(3) = First unprotected field on new sub-form
  ! (b) Highlight moving onto a tab via keyboard :
  ! IFIELD = Current field, as normal
  ! IEXITK = Key used to access tabs, as normal
  ! InfoForm(3) = -(tab number)
  ! (c) Highlight moving off a tab via keyboard or mouse :
  ! IFIELD = -(tab number)
  ! IEXITK = Key code or mouse action used to select tab
  ! InfoForm(3) = Next field, as normal
  ! (d) As (c) but the sub-form which is about to be entered has no unprotected fields :
  ! IFIELD = -(tab number)
  ! IEXITK = Key code or mouse action used to select tab
  ! InfoForm(3) = -(tab number)
  ! Whilst FMUSER has a simple calling interface it provides a powerful 'hook' into the
  ! form editor. In particular, nearly all INTERACTER routines are available to be called by
  ! FMUSER. However, power and flexibility also imply a degree of responsibility, so some
  ! basic rules should be observed when writing your own FMUSER user-routines :
  ! �  Other Forms Manager routines can be called, but don't call IFormDefine,IFormLoad, IFormEdit, or IFormEditUser for obvious reasons. Do not
  ! call any routines which use pre-defined forms such as IdFilename.
  ! �  Text attributes are not automatically preserved across a call to FMUSER, so these
  ! should be saved and restored if they are modified during the course of a call.
  ! �  Avoid calling routines which change screen mode.
  ! �  If a particular task can be performed just as well at the level where IFormEdit
  ! would be called as it can using IFormEditUser/FMUSER, do it at the calling
  ! level not in FMUSER.
  ! �  If the form is displayed in a window, be sure that the current window remains
  ! available and selected on return from FMUSER. Other windows can be opened,provided they are closed before exit or IWinSelect is used to reselect the
  ! window which contains the form.
  ! �  The current pop-up mode as selected by InPopup is preserved across a call to
  ! FMUSER, so calls to InPopup are allowed but are only effective for the duration
  ! of that call to FMUSER.
  !e.g. See the formdem2 demonstration program
  !Form Creation & Editing INTERACTER Subroutine Reference
  !3-14
end subroutine FMUSER

subroutine IFormHelp(IX,IY,IWIDTH)
  integer IX ! Help field column (zero to suppress help field)
  integer IY ! Help field row
  integer IWIDTH ! Help field width
  ! Short-name : FMHELP
  ! Declares the position and size of a 'help' field. This is a unique field which is
  ! automatically updated by IFormEdit as the user tabs between fields. Each field can
  ! have its own message which appears in the help field whenever the cursor moves onto
  ! that field. Field dependent help messages are definable using IFormPutHelp.
  ! By default, IFormDefine disables the help field feature so a call to IFormHelp
  ! should follow the definition of a new form. To suppress the help field again without
  ! defining a new form simply re-call IFormHelp with a zero IX value.
  ! If an IFD file does not define a help field, calling IFormLoad will also disable the
  ! help field.
  ! In a multi-part tabbed form, the help field is visible on all forms, if enabled.
  !e.g. CALL IFormHelp(1,5,IWIDTH)
  !CALL IFormPutHelp(5,'Space bar cycles available options')
end subroutine IFormHelp

subroutine IFormIdentifier(IDENT)
  integer IDENT(*) ! Array of field identifiers
  ! Short-name : FMIDEN
  ! Defines field identifiers for all of the fields in the form. These are the field numbers
  ! which will be specified as IFIELD values in subsequent calls to other FM routines.
  ! They replace the default field numbers of 1 to NFIELD which are assigned by
  ! IFormDefine. Non-sequential field identifiers are used automatically when a form is
  ! defined using IFormLoad, so there will normally be no need to use
  ! IFormIdentifier if forms are defined from a file.
  ! The specified identifiers remain valid until another form is defined. Assigning field
  ! identifiers in this manner allows fields to be more easily added or deleted without
  ! having to change all the field number references in the various other FM routines.
  ! If IFormIdentifier is to be used, it must be called after IFormDefine and
  ! before any FM group subroutine calls which use the identifiers in the IDENT array.
  !e.g. PARAMETER ( NF = 4 )
  !INTEGER IX(NF),IY(NF),IWIDTH(NF),ITYPE(NF),IDENT(NF)
  !DATA IX / 1, 16, 1, 16/
  !DATA IY / 1, 1, 3, 3/
  !DATA IWIDTH/ 15, 8, 15, 8/
  !DATA ITYPE /1001, 2,1001, 3/
  !DATA IDENT / 1,201, 2,202/
  !C create a new form
  !CALL IFormDefine('W',NF,IX,IY,IWIDTH,ITYPE)
  !C define field identifiers
  !CALL IFormIdentifier(IDENT)
  !C put data into form fields
  !CALL IFormPutString(1,'No. of values')
  !CALL IFormPutString(2,'Start value')
  !CALL IFormPutInteger(201,1)
  !CALL IFormPutReal(202,0.0,'(F8.2)')
  !INTERACTER Subroutine Reference Form Creation & Editing
  !3-15
end subroutine IFormIdentifier

subroutine IFormIdentifierBox(IDENT)
  integer IDENT(*) ! Array of form-box identifiers
  ! Defines identifiers for all of the form-boxes in the current form. These are the box
  ! numbers which will be specified as IBOX values in subsequent calls to routines such as
  ! IFormBox or IFormShowBox. They replace the default form.box numbers of 1 to
  ! NBOX which are assigned by IFormDefineBox. Non-sequential form-box identifiers
  ! are used automatically when a form is defined using IFormLoad, so there will
  ! normally be no need to use IFormIdentifierBox if forms are defined from a file.
  ! The specified identifiers remain valid until another form is defined. Assigning formbox
  ! identifiers in this manner allows form-boxes to be more easily added or deleted
  ! without having to change all the form-box number references in the other FM routines.
  ! If IFormIdentifierBox is to be used, it must be called after IFormDefineBox
  ! and before any FM group subroutine calls which use the identifiers in the IDENT array.
  !e.g. PARAMETER ( NBOX = 3 )
  !INTEGER IX(NBOX),IY(NBOX),IW(NBOX),IH(NBOX),ID(NBOX)
  !DATA IX/ 1, 1, 9/
  !DATA IY/ 1, 5, 1/
  !DATA IW/ 15, 15, 1/
  !DATA IH/ 9, 1, 9/
  !DATA ID/ 100, 11, 12/
  !:
  !C define layout of form-boxes
  !CALL IFormDefineBox(NBOX,IX,IY,IW,IH)
  !C define form-box identifiers
  !CALL IFormIdentifierBox(ID)
  !C display form-box 100
  !CALL IFormShowBox(100)
end subroutine IFormIdentifierBox

subroutine IFormLoad(TYPE,FILNAM,LFNERR)
  character(len=*) TYPE ! Type of form
  ! = W : Window (single form)
  ! = S : Full Screen (single form)
  ! = T : Tabbed (multi-part form)
  character(len=*) FILNAM ! Form definition file name
  integer LFNERR ! Error reporting flag
  ! = -1 : Terminate on non-fatal error
  ! = -2 : Ignore non-fatal errors
  ! = other : Log errors to this logical file number
  ! Short-name : FMLOAD
  ! Defines a new form, loading the definition from a file. All previous form information is
  ! deleted. IFormLoad should be called to create a new form before calling other
  ! routines in the FM group. (To define a new form using program defined arrays see the
  ! alternative routine IFormDefine).
  ! Unlike IFormDefine, IFormLoad defines all of the information required to create
  ! a new form in a single subroutine call. In addition to field position, size and type, other
  ! information can be defined : field attributes, help field information, field values and
  ! ranges. The format of this file is described in chapter 20 of the User Guide. It should
  ! have been created using a text editor, INTFORM or IFormSave. The advantages of
  ! using IFormLoad are thus :
  ! Form Creation & Editing INTERACTER Subroutine Reference
  ! 3-16
  ! �  Forms can be modified without recompilation
  ! �  Form definitions are easier to edit, particularly with INTFORM
  ! �  Programs which use several forms will be smaller. Each call to IFormLoad
  ! replaces the initial call to IFormDefine and the subsequent calls to routines such
  ! as IFormAttribute, IFormPutxxx, IFormGetxxx, IFormRangexxx,IFormHelp, etc. (though the latter routines can still be called after IFormLoad
  ! if required). Similarly, data space requirements are reduced. Programs which use
  ! more than 2 or 3 forms will usually be smaller if IFormLoad is used in place of
  ! IFormDefine.
  ! FILNAM specifies the name of the form definition file. A file extension of .ifd is
  ! recommended ('INTERACTER Form Definition'). IFormLoad first tries to open the
  ! file as specified by FILNAM. If the file is not found it then looks in a directory
  ! specified by the operating system variable INTFMDIR or the initialisation file keyword
  ! FORMDIR. Form definitions can thus be collected in a single directory, the name of
  ! which can change from system to system, but the program need only specify the final
  ! local portion of the full file pathname.
  ! The TYPE argument has the same effect as the equivalent IFormDefine argument.
  ! LFNERR controls how non-fatal errors in the form definition will be handled :
  ! LFNERR = -1 : Terminate IFormLoad if any non-fatal errors occur
  ! LFNERR = -2 : Ignore non-fatal errors and attempt to load as much of
  ! the form definition as is valid
  ! LFNERR >= 0 : Log errors to this Fortran logical file number, which
  ! must be OPEN'ed and CLOSE'd by the calling
  ! program. The line numbers of records which contain
  ! errors are written to this output channel.
  ! During development it may prove useful to set LFNERR to a non-negative value and
  ! log errors to a diagnostics file. Normally, once a form definition has been debugged, a
  ! production program would set LFNERR to -1 or -2. The last line number at which an
  ! error occurred in IFormLoad is also available via InfoForm(4). Note that fatal
  ! errors always terminate IFormLoad regardless of the value of LFNERR.
  ! Errors 36 and 37 are treated as non-fatal. Error number 37 covers a variety of possible
  ! errors including :
  ! - Too many field definitions
  ! - Invalid field position/size/type
  ! - Unknown field identifier
  ! - Ranges not allowed for string/menu/button fields
  ! - Menu already defined
  ! - No. of menu options or initial option missing
  ! - Unknown field type
  ! - Incorrect number of sub-form/tab definitions
  !e.g. See formdem3/4/5 demonstration programs/form definition files
  !INTERACTER Subroutine Reference Form Creation & Editing
  !3-17
  !Errors :
  !ErrFileOpen (1) : Error opening file
  !ErrFileIO (2) : Error reading from file or unexpected end-of-file
  !ErrFullDataArea (33) : Internal character data storage area full
  !ErrOutOfRange (36) : Integer or real field value out of range
  !ErrIFDWarn (37) : General non-fatal error in form definition file
  !(warning only)
  !ErrNotFormFile (38) : Not an INTERACTER Form Definition file
end subroutine IFormLoad

subroutine IFormOpenWindow(IXTOPL,IYTOPL)
  integer IXTOPL ! Top left column of window (= 0 : centre horizontally)
  integer IYTOPL ! Top left row of window (= 0 : centre vertically)
  ! Opens a window to contain the current form. The form must already have been loaded
  ! from disk using IFormLoad. The form definition file must also have included a
  ! [window] section defining the window size and optional title. If these conditions are
  ! not met, an error is set and no window is opened.
  ! If successful, calling IFormOpenWindow is directly equivalent to calling IWinOpen
  ! or IWinOpenTitle. By opening a window via IFormOpenWindow, a program
  ! need not know how large a particular form is, allowing the same piece of code to
  ! operate on multiple forms.
  ! Just like IWinOpen/IWinOpenTitle, the colour, frame, pop-up status, etc. of the
  ! opened window are all determined by earlier calls to
  ! ITextColour/ITextColourN, IWinAction, etc. If default colours have been
  ! specified in the form file too (in a [defaults] section) it is advisable to call
  ! IFormDefaults before calling IFormOpenWindow.
  ! The presence of a [window] section in a form definition file does not oblige the loading
  ! program to display that form in a window. Similarly, it does not oblige the use of
  ! IFormOpenWindow. The IWinOpen/IWinOpenTitle routines can be called
  ! directly, if preferred.
  ! IXTOPL and IYTOPL have exactly the same meaning as the equivalent arguments to
  ! IWinOpen/IWinOpenTitle.
  !e.g. CALL IFormLoad('W','myform.ifd',LFNERR)
  !C activate default colours
  !CALL IFormDefaults(1)
  !C want a pop-up, framed, clear window
  !CALL IWinAction('PFC')
  !C open window in centre of screen
  !CALL IFormOpenWindow(0,0)
  !C display form
  !CALL IFormShow
  !C edit form
  !CALL IFormEdit(IFINIT,IFEXIT)
  !C close/remove window
  !CALL IWinClose(1)
  !Errors :
  !ErrFormWindow (50) : Form contains no [window] specification
  !Form Creation & Editing INTERACTER Subroutine Reference
  !3-18
end subroutine IFormOpenWindow

subroutine IFormRadioButton(IFIELD,NCHECK)
  integer IFIELD ! Identifier of first check box in radio-button group
  integer NCHECK ! Number of check-boxes in group (0 to unassociate group)
  ! Identifies a set of check box fields as a radio button group. Field IFIELD and the next
  ! NCHECK-1 check boxes in the current form then become a radio button group, where
  ! no more than one check box is ever enabled at one time.
  ! Specifying NCHECK as zero clears the radio button grouping. All the check boxes in the
  ! group then revert to their individual unassociated state.
  !e.g. see the formdem6 demo
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
  !ErrRadioFields (57) : Attempt to group more check-boxes than exist
  !after specified field
end subroutine IFormRadioButton

subroutine IFormSave(HEADER,FILNAM)
  character(len=*) HEADER ! Comment to be included in file header record
  character(len=*) FILNAM ! Form definition file name
  ! Short-name : FMSAVE
  ! Saves the currently defined form to a file in the format defined in chapter 20 of the
  ! User Guide. The file will be reloadable using IFormLoad. A file can be saved at any
  ! time after a call to IFormDefine or IFormLoad. If IFormEdit has been called,the values entered by the user will be saved rather than the field values initially defined
  ! by the IFormPut routines or by IFormLoad.
  ! The filename used by IFormSave is as defined by FILNAM. No attempt is made to
  ! save the file in the directory specified by the INTFMDIR o.s. variable.
  ! The output file will contain a header record containing the HEADER string. The date
  ! and time of creation is also appended to the header record.
  ! IFormSave can be used to convert programs written to use the IFormDefine
  ! routine, to be able to use IFormLoad instead. Simply insert a call to IFormSave
  ! after the subroutine calls which define a form, then replace the calls to IFormDefine,IFormSave, etc. by a single call to IFormLoad. In this case, IFormSave would be
  ! used during the development phase but not in a production program.
  !e.g. CALL IFormLoad('S','formdef.ifd',-2)
  !CALL IFormShow
  !CALL IFormEdit(IFINIT,IFEXIT)
  !IF (InfoInput(55).EQ.21) THEN
  !CALL IFormSave('Temporary form file save','temp.ifd')
  !ELSE ...
  !INTERACTER Subroutine Reference Form Field Characteristics
  !3-19
  !3.2 Group FM(2) : Field-specific Characteristics
  !The routines in this group determine the characteristics (though not the contents) of
  !individual fields and form-boxes in the Forms Manager. Field and form-box attributes,such as colour are controlled by IFormAttribute and IFormBox respectively.
  !Field frame styles are determined by IFormFrame. Input parameters such as case
  !conversion can be set using IFormInputParam. IFormPopupMenu and
  !IFormVerticalMenu define menu field characteristics. Numerical field ranges can
  !be set via IFormRangeDouble, IFormRangeInteger and IFormRangeReal.
  !The protection status of individual fields can be modified via IFormProtection.
  !All the routines in this group take a single form-field or form-box number as their first
  !argument. See the FM(1) group for routines which define forms at a global level (e.g.
  !IFormLoad).
end subroutine IFormSave

subroutine IFormAttribute(IFIELD,ATTRS,FORCOL,BACCOL)
  integer IFIELD ! Field number (or zero to define help field attributes)
  character(len=*) ATTRS ! String describing required attributes for this field in
  ! same format as argument to ITextAttribute routine
  ! i.e. any or none of the following
  ! B : Select bold text
  ! F : Select flashing text
  ! I : Select italics
  ! R : Select reverse video
  ! U : Select underlining
  ! or one of two special values :
  ! blank : use default form attribute
  ! N : None (all attributes disabled)
  character(len=*) FORCOL ! Foreground colour name ) As for ITextColour, or
  character(len=*) BACCOL ! Background colour name ) blank to use default
  ! Short-name : FMATTR
  ! Sets the attributes/colours for the specified field. By default all form fields are
  ! displayed using the currently enabled attribute/colour combination when
  ! IFormShowField, IFormShow, IFormShowUnp or IFormEdit are called,unless specified in a form definition file. IFormAttribute can be used to assign
  ! different attributes to individual fields.
  ! The ATTRS argument specifies the combination of highlight attributes required for
  ! field IFIELD, in a similar manner to the ITextAttribute routine. If ATTRS is
  ! left blank, the default highlight attributes being used for the form as a whole will be
  ! used. To explicitly disable all attributes in this field, regardless of the form default, set
  ! ATTRS to 'N'. It is advisable to check the values returned by InfoAttribute to
  ! determine which attributes are available in the current mode.
  ! Colours are specified using the same conventions as the ITextColour routine.
  ! Again, a blank argument causes the field to be displayed in the default colour being
  ! used to display the rest of the form.
  ! Form Field Charactereristics INTERACTER Subroutine Reference
  ! 3-20
  ! A special feature of this routine is that a field value of zero can be used to define the
  ! attribute/colour combination for the help field. (Refer to IFormHelp and
  ! IFormPutHelp for more information about this field.) This attribute/colour
  ! combination will only be used when displaying help messages. When the help field is
  ! blank, it reverts to the default form attributes/colour.
  ! Field-specific attribute information can be retrieved via InfoField.
  ! IFormAttribute uses CHARACTER arguments for readability. The alternative
  ! IFormAttributeN performs the same task, but uses numeric arguments.
  !e.g. CALL IFormDefine('W',NFIELD,IX,IY,IWIDTH,ITYPE)
  !CALL IFormHelp(5,10,30)
  !CALL IFormAttribute(0,'B',' ', ' ')
  !CALL IFormAttribute(1,' ','RED','W')
  !CALL IFormAttribute(2,'BU','YEL',' ')
  !CALL IFormShow
  !Portability notes :
  !See ITextAttribute and ITextColour in the AT subroutine group.
end subroutine IFormAttribute

subroutine IFormAttributeN(IFIELD,IATTRS,IFCOLR,IBCOLR)
  integer IFIELD ! Field number (or zero to define help field attributes)
  integer IATTRS ! Number describing required attributes for this field
  ! calculated as the sum of the following
  ! FieldBold (1) : Select bold text
  ! FieldFlash (2) : Select flashing text
  ! FieldItalics (4) : Select italics
  ! FieldReverse (8) : Select reverse video
  ! FieldUnderline (16) : Select underlining
  ! or one of two special values :
  ! FieldAttrDefault (0) : use default form attribute
  ! FieldAttrNone (32) : None (all attributes disabled)
  integer IFCOLR ! Foreground colour number ) As for ITextColourN
  integer IBCOLR ! Background colour number ) or -1 to use default
  ! Short-name : FMATTN
  ! Include : interfm.inc, interat.inc
  ! This routine is exactly equivalent to IFormAttribute, except that it allows
  ! attributes and colours to be specified numerically. The example given below has the
  ! same effect as the IFormAttribute example.
  !e.g. CALL IFormDefine('W',NFIELD,IX,IY,IWIDTH,ITYPE)
  !CALL IFormHelp(5,10,30)
  !CALL IFormAttributeN(0,1,-1,-1)
  !CALL IFormAttributeN(1,0,1,7)
  !CALL IFormAttributeN(2,17,2,-1)
  !CALL IFormShow
  !CALL IFormEdit(IFINIT,IFEXIT)
  !INTERACTER Subroutine Reference Form Field Characteristics
  !3-21
end subroutine IFormAttributeN

subroutine IFormBox(IBOX,IFTYPE,FORCOL,BACCOL,INCOL)
  integer IBOX ! Box number
  integer IFTYPE ! Frame type :
  ! 0 : None (clear inside only)
  ! 1 to 10 : As for IFrameType(IFTYPE)
  ! -1 to -10 : As for IFrameType(abs(IFTYPE))
  ! but only when graphical frames available
  ! other : Use current frame style (default)
  character(len=*) FORCOL ! Foreground colour name ) As for ITextColour, or
  character(len=*) BACCOL ! Background colour name ) blank to use default
  character(len=*) INCOL ! Colour to clear inside of frame to (blank=don't clear)
  ! Sets the type and colours for the specified form-box. By default all form-boxes are
  ! displayed using the currently selected frame type/colours when IFormShowBox or
  ! IFormShow are called, unless specified in a form definition file. IFormBox can be
  ! used to assign different attributes to individual form-boxes.
  ! Frame type will normally be specified as a value in the range 1-10, using the same
  ! conventions as IFrameType routine. A negative argument signifies that the requested
  ! frame type should only be used when graphical frames are available. A frame type of
  ! zero means 'no frame'. Use this in combination with the INCOL argument to clear an
  ! unframed area of the form. Any other value causes the form-box to be displayed using
  ! the current default frame type, as set by IFrameType.
  ! Colours are specified using the same conventions as ITextColour. A blank
  ! argument causes the form-box to be displayed in the default colour, as set by
  ! ITextColour or IFrameOptions.
  ! The INCOL argument determines what colour (if any) the area inside a form box should
  ! be cleared to. If this is blank, the area is not cleared. If the form-box is only a vertical or
  ! horizontal line, INCOL is not used.
  ! IFormBox uses CHARACTER arguments for readability. The alternative IFormBoxN
  ! performs the same task, but uses numeric arguments.
  !e.g. CALL IFormDefine('W',NFIELD,IX,IY,IWIDTH,ITYPE)
  !CALL IFormDefineBox(NBOX,IX,IY,IW,IH)
  !CALL IFormBox(1,5,'W','BLA',' ')
  !CALL IFormBox(2,0,' ',' ','BLUE')
  !CALL IFormShow
end subroutine IFormBox

subroutine IFormBoxN(IBOX,IFTYP,NFCOL,NBCOL,NICOL)
  integer IBOX ! Box number
  integer IFTYP ! Frame type (as for IFormBox)
  integer NFCOL ! Foreground colour number ) As for ITextColourN
  integer NBCOL ! Background colour number ) or -1 to use default
  integer NICOL ! Colour to clear inside of frame to (-1= don't clear)
  ! Sets the frame-type/colours for the specified form-box, using numeric arguments.
  ! IFormBoxN is directly equivalent to IFormBox which uses more readable character
  ! arguments. See IFormBox for more information. The example given below is exactly
  ! equivalent to the example given for the IFormBox routine.
  ! Form Field Charactereristics INTERACTER Subroutine Reference
  ! 3-22
  !e.g. CALL IFormDefine('W',NFIELD,IX,IY,IWIDTH,ITYPE)
  !CALL IFormDefineBox(NBOX,IX,IY,IW,IH)
  !CALL IFormBoxN(1,5,7,0,-1)
  !CALL IFormBoxN(2,0,-1,-1,5)
  !CALL IFormShow
end subroutine IFormBoxN

subroutine IFormFrame(IFIELD,IFTYPE,FORCOL,BACCOL)
  integer IFIELD ! Field number (or zero to define help field frame type)
  integer IFTYPE ! Frame type :
  ! 0 : none (default if value out of range)
  ! 1 to 10 : as for IFrameType(IFTYPE)
  ! -1 to -10 : as for IFrameType(abs(IFTYPE))
  ! but only when graphical frames available
  character(len=*) FORCOL ! Foreground colour name ) As for ITextColour
  character(len=*) BACCOL ! Background colour name ) or blank to use default
  ! Sets the frame type, if any, for the specified field. By default, all form fields except
  ! vertical menus have no frame. Vertical menus have a frame type of 1 by default and use
  ! the default frame colours.
  ! The IFTYPE argument specifies the required frame type. A positive value specifies that
  ! a frame is required on all types of screen. Since box-drawing characters may not be
  ! appropriate for framing fields in some forms, a negative IFTYPE value can be
  ! specified to tell the forms manager to only frame this field when graphical frames are
  ! available. This is a very useful portability feature, allowing form appearance to be
  ! enhanced on graphical displays whilst retaining compatibility with character based
  ! screens/modes. Specify IFTYPE as zero if no frame is required.
  ! Colours are specified using the same conventions as the ITextColour routine. A
  ! blank argument causes the frame to be displayed using the default frame colours as set
  ! by IFrameOptions(6,n)/(7,n). If these frame options are also set to 'default',the text colours set by ITextColour/ITextColourN determine the frame colours.
  ! Field frames are displayed by IFormShow or IFormShowFrame.
  ! Frame type 7 or 8 (or -7/-8) is strongly recommended for push-button fields. The form
  ! editor recognises this field type specifically and implements a 'button down' effect when
  ! the user holds a mouse button down on such a field. This effect is typical of how
  ! environments such as Windows and Motif behave.
  ! Cycling menu fields can effectively have two separate frame types. The frame type
  ! specified here defines the frame for the unexpanded menu field. If the field is given a
  ! pop-up capability, the frame type for the pop-up vertical menu is controlled
  ! independently via IFormVerticalMenu.
  ! IFormFrame uses CHARACTER arguments for readability. The alternative
  ! IFormFrameN performs the same task, but uses numeric arguments.
  !e.g. CALL IFormFrame(0,9,'W', 'BLA')
  !CALL IFormFrame(IFIELD,6,'BLA','BW')
  !CALL IFormFrame(IBUTTON,7,'BW','BLA')
  !CALL IFormShow
  !INTERACTER Subroutine Reference Form Field Characteristics
  !3-23
end subroutine IFormFrame

subroutine IFormFrameN(IFIELD,IFTYPE,NFCOL,NBCOL)
  integer IFIELD ! Field number (or zero to define help field frame type)
  integer IFTYPE ! Frame type :
  ! 0 : none (default if value out of range)
  ! 1 to 10 : as for IFrameType(IFTYPE)
  ! -1 to -10 : as for IFrameType(abs(IFTYPE))
  ! but only when graphical frames available
  integer NFCOL ! Foreground colour name ) As for ITextColourN
  integer NBCOL ! Background colour name ) or -1 to use default
  ! This routine is exactly equivalent to IFormFrame, except that it allows frame colours
  ! to be specified numerically.
end subroutine IFormFrameN

subroutine IFormInputParam(IFIELD,IPAR,IVALUE)
  integer IFIELD ! Field number
  integer IPAR ! Parameter number (see below)
  integer IVALUE ! Parameter value (see below)
  ! Short-name : FMIPAR
  ! Include : interfm.inc
  ! IPAR IVALUE
  ! FieldVisible (1) Visibility : FormDefault (0) : default
  ! FieldNo (1) : no
  ! FieldYes (2) : yes
  ! FieldJustify (2) Numeric justification : FormDefault (0) : default
  ! FieldLeft (1) : left
  ! FieldRight (2) : right
  ! FieldCursor (3) Initial cursor pos : FormDefault (0) : default
  ! FieldStart (1) : start
  ! FieldEnd (2) : end
  ! FieldEditMode (4) Overtype/insert : FormDefault (0) : default
  ! FieldOvertype (1) : overtype
  ! FieldInsert (2) : insert
  ! FieldTypeWipe (5) Type & wipe : FormDefault (0) : default
  ! FieldNo (1) : no
  ! FieldYes (2) : yes
  ! FieldCaseConvert (6) Case conversion : FormDefault (0) : default
  ! FormNo (1) : none
  ! FieldUpper (2) : upper
  ! FieldLower (3) : lower
  ! FieldGravity (7) Field gravity : FormDefault (0) : default
  ! FieldLeft (1) : left
  ! FieldRight (2) : right
  ! Defines input control parameters for the specified field. These parameters correspond
  ! directly to those which are definable on a global basis by Invisible,InJustifyNum, InCursorPos, InsertOver, InTypeWipe, InCase and
  ! InGravity.
  ! Form Field Charactereristics INTERACTER Subroutine Reference
  ! 3-24
  ! If IFormInputParam is not called to define a parameter for a given field or if
  ! IVALUE is specified as zero, the current default setting is used. This will be as defined
  ! by a call to the appropriate IP group routine, before the call to IFormShow,IFormShowUnp and IFormShowField or IFormEdit/IFormEditUser.
  ! Parameter 1 determines field visibility and can be used for the entry of passwords. This
  ! option also affects field output, since IFormShow, IFormShowUnp and
  ! IFormShowField will simply clear the field. Enabling this parameter causes the
  ! field to be invisible. i.e. only enable this feature for password fields.
  ! Parameter 2 selects left or right justification of numeric values when the cursor enters
  ! an integer, real or double precision field. This setting is ignored in non-numeric fields.
  ! Parameter 3 places the cursor at the beginning or end of the field value when the cursor
  ! enters a string, integer or real field.
  ! Parameter 4 forces insert/overtype to a specific state regardless of the last setting
  ! selected via the keyboard by the user.
  ! Parameter 5 enables or disables the 'type and wipe' feature. See InTypeWipe for
  ! details of the purpose of this feature.
  ! Parameter 6 enables/disables case conversion on input. Unlike the other parameters,this has three states, 'off', 'upper' and 'lower', in addition to 'default' as set by InCase.
  ! Parameter 7 determines whether the field gravitates towards the left or the right. The
  ! former is more conventional, but the latter can be useful when entering right-justified
  ! numeric values, in which case parameter 2 should also be set to 'right'. This setting is
  ! ignored in long-string fields, which always have left gravity.
  ! Normally, input control parameters will only be used to change the characteristics of an
  ! unprotected string or numeric field. However, it is valid to specify input control
  ! parameters for a protected field, since IFormProtection allows such fields to
  ! become unprotected at any time. Whilst allowable, the use of IFormInputParam to
  ! change parameters on menu or push button fields is not generally useful.
  ! Input control parameter values for a specific field can be retrieved subsequently via the
  ! InfoField function.
  !e.g.
  !C set general field justification to 'Left'
  !CALL InJustifyNum('L')
  !C but force justification of field number 5 to 'Right'
  !CALL IFormInputParam(5,2,2)
  !CALL IFormShow
  !CALL IFormEdit(IFINIT,IFEXIT)
  !INTERACTER Subroutine Reference Form Field Characteristics
  !3-25
end subroutine IFormInputParam

subroutine IFormPopUpMenu(IFIELD,NOPTN)
  integer IFIELD ! Field number (cycling menu field)
  integer NOPTN ! Pop-up menu availability/size :
  ! = 1 : Standard cycling menu, no pop-up option
  ! < MAXOPT : Enable scrolling pop-up menu of this size
  ! = MAXOPT : Enable non-scrolling pop-up menu
  ! Short-name : FMPOPM
  ! Defines the availability and type of a pop-up menu in a cycling (type 4) menu field.By
  ! default, such menus are displayed in a simple cycling format which utilises the
  ! IMenuCycle routine in the form editor. Optionally, the same menu options can be
  ! made to appear in a pop-up menu within the form editor, under keyboard or mouse
  ! control. IOPTN specifies whether such a pop-up menu is available and, if so, its type.
  ! When a pop-up menu is enabled in a cycling menu field, it can be activated by the user
  ! in three different ways :
  ! �  By pressing control key 34 (usually F2/PF2) when the highlight is on the field.
  ! �  Clicking the right mouse button (see InMouseOptions(102,n)) in the field,when the highlight is on that field.
  ! �  If the field has a frame, a pop-up 'button' (a downward arrow) will appear in the
  ! right-hand edge of the frame. The user can click on this button from anywhere on
  ! the form to move the highlight to that field and display the pop-up menu in a single
  ! operation.
  ! Assuming that MAXOPT is the number of options in the menu (as specified in the form
  ! definition file or in a call to IFormPutMenu), then 1<NOPTN<MAXOPT enables a
  ! scrolling pop-up menu in which IOPTN menu options are visible. This menu will be
  ! displayed using IMenuScroll. Alternatively, if NOPTN=MAXOPT, a non-scrolling
  ! pop-up menu will be enabled, which will be displayed using IMenuVertic.
  ! Reset NOPTN to 1 to disable pop-up menus in the specified field. This is the default
  ! setting assigned by IFormDefine. Menus definitions in an IFD file can include the
  ! initial pop-up menu size (see chapter 20 of the User Guide).
  ! If a pop-up menu is enabled, the operation of IFormPutMenu, IFormGetMenu and
  ! IFormPutOption is completely unaffected. Only the behaviour of
  ! IFormEdit/IFormEditUser is changed. Once a pop-up menu is enabled, control
  ! key 34 will toggle the expanded menu on and off. The effect of pressing other control
  ! keys in a pop-up menu is documented further under IFormEdit.
  ! As an alternative to calling IFormPopUpMenu, IFormVerticalMenu can be
  ! called for exactly the same purpose with the added benefit of being able to define the
  ! pop-up menu's frame type. By default, pop-up menus have a single line (type 1) frame.
  ! The pop-up menu availability/size parameter for a specific field can be retrieved
  ! subsequently via InfoField(10).
  ! Form Field Charactereristics INTERACTER Subroutine Reference
  ! 3-26
  !e.g. PARAMETER (MAXOPT = 50)
  !character(len=10) OPTION(MAXOPT)
  !CALL IFormPutMenu(IFIELD,OPTION,MAXOPT,IOPT1)
  !C enable a pop-up scrolling menu, 10 deep, in form editor
  !CALL IFormPopUpMenu(IFIELD,10)
  !CALL IFormShow
  !CALL IFormEdit(IFINIT,IFEXIT)
end subroutine IFormPopUpMenu

subroutine IFormProtection(IFIELD,PROT)
  integer IFIELD ! Field number
  character(len=*) PROT ! Type of protection required :
  ! = P : Protected
  ! = U : Unprotected (default)
  ! Short-name : FMPROT
  ! Resets a field's protection status. This allows the initial field type set in the call to
  ! IFormDefine to be modified without redefining the entire form. The contents of the
  ! field remain unchanged.
  !e.g. DO 100 IFIELD = 1,NFIELD
  !C protect all bar one field on the current form
  !CALL IFormProtection(IFIELD,'P')
  !100 CONTINUE
  !CALL IFormProtection(3,'U')
  !CALL IFormEdit(IFINIT,IFEXIT)
end subroutine IFormProtection

subroutine IFormRangeDouble(IFIELD,DFMIN,DFMAX)
  integer IFIELD ! Field number
  double precision DFMIN ! Minimum allowable double precision value
  double precision DFMAX ! Maximum allowable double precision value
  ! Short-name : FMRNGD
  ! Defines a valid range for a double precision field. If the field has not been declared as a
  ! double precision field an error code is set. In all other respects this routine behaves
  ! exactly like the equivalent integer range definition routine, IFormRangeInteger.
  ! Refer to the IFormRangeInteger documentation below for more information on
  ! range checking.
  !e.g. DOUBLE PRECISION THICK,THMIN,THMAX
  !C Field 2 is an unprotected double precision value
  !CALL IFormPutString(1,'Thickness')
  !THMIN = 1.0
  !THMAX = 1.0000001
  !CALL IFormRangeDouble(2,THMIN,THMAX)
  !CALL IFormPutDouble(2,THICK,'(F12.9)')
  !100 CALL IFormShow
  !CALL IFormEdit(IFINIT,IFEXIT)
  !IF (InfoInput(55).EQ.22) THEN
  !CALL HELP
  !IFINIT = IFEXIT
  !GOTO 100
  !ELSE IF (InfoInput(55).EQ.23) THEN
  !QUIT = .TRUE.
  !RETURN
  !ENDIF
  !C No need to validate returned value
  !CALL IFormGetDouble(2,THICK)
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
  !INTERACTER Subroutine Reference Form Field Characteristics
  !3-27
end subroutine IFormRangeDouble

subroutine IFormRangeInteger(IFIELD,IFMIN,IFMAX)
  integer IFIELD ! Field number
  integer IFMIN ! Minimum allowable integer value
  integer IFMAX ! Maximum allowable integer value
  ! Short-name : FMPUTI
  ! Defines a valid range for an integer field. If the field has not been declared as an integer
  ! field an error code is set. If the field number is unknown the call is ignored.
  ! This call has no immediate on-screen effect. It should be called before IFormEdit to
  ! define the range of values which the user may type into field number IFIELD. By
  ! default range checking on all fields is disabled. To disable range checking on a field
  ! which has had a range set by an earlier call to IFormRangeInteger, specify an
  ! IFMAX value which is less than IFMIN.
  ! To enforce 'minimum-only' range checking simply set the maximum value to a large
  ! positive number. Similarly, specify a large negative number for the minimum value for
  ! a 'maximum-only' range check.
  ! When IFormEdit is called to edit a form, the user will not be allowed to enter an out
  ! of range value in a range-checked field. If a range-checked field is initially undefined
  ! (i.e. it appears on screen as a blank field) the user will be able to tab across that field
  ! without entering a value. However, once a valid value has been entered in a rangechecked
  ! field (either by the user or by calling IFormPutInteger) the user cannot
  ! then clear the field to the undefined state. The calling program can still clear a rangechecked
  ! field using IFormClearField, if required.
  ! Pressing the help or quit keys (control keys 22 or 23) in a range-checked field causes
  ! IFormEdit to terminate immediately, in the normal way. The same is true if a
  ! resize/expose event (259) or close-window request (260) occurs. However, if the user
  ! has just entered an out of range value in that field, the previous (valid) field contents
  ! will be restored. This ensures that it is possible to get out of a form (to backtrack,display help or redraw the screen) while maintaining the integrity of the form's data.
  ! When an invalid value is entered in a range-checked field and an attempt is made to
  ! move to another field or to confirm the form, the bell will sound and the cursor will be
  ! repositioned to the start of the field with the highlight still on the invalid entry. No error
  ! message is output. Messages which describe the valid range can be included in either a
  ! protected prompt field and/or in the field sensitive help field.
  !e.g. CALL IFormPutString(1,'Percentage saturation :')
  !CALL IFormRangeInteger(2,0,100)
  !CALL IFormPutInteger(2,IPERC)
  !100 CALL IFormShow
  !CALL IFormEdit(IFINIT,IFEXIT)
  !IF (InfoInput(55).EQ.22) THEN
  !CALL HELP
  !IFINIT = IFEXIT
  !GOTO 100
  !ELSE IF (InfoInput(55).EQ.23) THEN
  !QUIT = .TRUE.
  !RETURN
  !ENDIF
  !C No need to validate returned value - it's bound to be in range
  !CALL IFormGetInteger(2,IPERC)
  !Form Field Charactereristics INTERACTER Subroutine Reference
  !3-28
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
end subroutine IFormRangeInteger

subroutine IFormRangeReal(IFIELD,RFMIN,RFMAX)
  integer IFIELD ! Field number
  real RFMIN ! Minimum allowable real value
  real RFMAX ! Maximum allowable real value
  ! Short-name : FMPUTR
  ! Defines a valid range for a real field. If the field has not been declared as type real, an
  ! error code is set. In all other respects this routine behaves exactly like the equivalent
  ! integer range routine, IFormRangeInteger. Refer to the IFormRangeInteger
  ! documentation above for more information on range checking.
  !e.g. CALL IFormPutString(1,'LOG of :')
  !C Field 2 is an unprotected real positive value
  !CALL IFormRangeReal(2,1.0E-37,1.0E37)
  !CALL IFormPutReal(2,RVAL,'(E10.1)')
  !100 CALL IFormShow
  !CALL IFormEdit(IFINIT,IFEXIT)
  !IF (InfoInput(55).EQ.22) THEN
  !CALL HELP
  !IFINIT = IFEXIT
  !GOTO 100
  !ELSE IF (InfoInput(55).EQ.23) THEN
  !QUIT = .TRUE.
  !RETURN
  !ENDIF
  !C No need to validate returned value
  !CALL IFormGetReal(2,RVAL)
  !X = ALOG(RVAL)
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
end subroutine IFormRangeReal

subroutine IFormVerticalMenu(IFIELD,NOPTN,IFRAME)
  integer IFIELD ! Field number (cycling or vertical menu field)
  integer NOPTN ! Number of visible menu options
  integer IFRAME ! Frame type (values as for IFormFrame, but see below)
  ! (out-of-range value leaves frame type unchanged)
  ! Short-name : FMVERM
  ! Defines the number of items which are visible in a vertical menu, plus the type and
  ! availability of a menu frame. When IFIELD references a cycling menu, this routine
  ! controls the size/appearance of a pop-up menu in exactly the same way as
  ! IFormPopUpMenu except that frame type can also be specified.
  ! The number of visible options should be less than or equal to the current maximum
  ! number of options stored in the field. If NOPTN is less than the maximum number of
  ! options, a scrolling menu is used. When IFIELD references a vertical menu (type 7)
  ! field, the menu remains visible at the specified size at all times.
  ! INTERACTER Subroutine Reference Form Field Characteristics
  ! 3-29
  ! The IFRAME argument determines the frame type of pop-up menus (if IFIELD
  ! identifies a cycling menu field) or the permanently visible frame around a vertical menu
  ! (i.e. a type 7 field). The latter is better specified via IFormFrame, or the [fields]
  ! section of an IFD file. Consequently, an out-of-range value can be specified meaning
  ! 'leave the current frame type unchanged'. A value of 999 is recommended in this case. If
  ! no frame is required, set IFRAME to zero, as for IFormFrame.
  ! The number of visible options in a menu field can be retrieved subsequently via
  ! InfoField(10).
  !e.g. PARAMETER (MAXOPT = 50)
  !character(len=10) OPTION(MAXOPT)
  !:
  !CALL IFormPutMenu(IFIELD,OPTION,MAXOPT,IOPT1)
  !C set visible number of options to 15 and leave frame type unchanged
  !CALL IFormVerticalMenu(IFIELD,15,999)
  !CALL IFormShow
  !
  !INTERACTER Subroutine Reference Assign/Retrieve Field Contents
  !3-31
  !3.3 Group FM(3) : Assign/Retrieve Field Contents
  !The routines in this group assign and retrieve the contents of individual fields in the
  !Forms Manager via a set of IFormPutxxxxx and IFormGetxxxxx routines. The
  !'Put' routines would normally be called before IFormEdit. The resulting field
  !contents can then be retrieved from the internal form data area after IFormEdit using
  !the 'Get' equivalents.
  !The help strings for individual fields can be specified via IFormPutHelp.
  !The contents of a field can be reset to 'undefined' by calling IFormClearField.
end subroutine IFormVerticalMenu

subroutine IFormClearField(IFIELD)
  integer IFIELD ! Field number
  ! Short-name : FMCLRF
  ! Clears a field and resets its contents to undefined. The field remains available for use.
  ! The on-screen contents of the field will not change until the next call to a field display
  ! routine such as IFormShow or IFormShowField.
  ! This routine effectively returns a field to the initial undefined state to which all fields
  ! are set when a form is first defined.
  ! IFormClearField has no effect on single check box fields (type 9) which are
  ! always defined (their default value is 'clear'). Where IFIELD specifies a field in a radio
  ! button group, all the check-boxes in that group are cleared.
  ! IFormGetRadioButton will then return zero until the user selects one of the radio
  ! button items.
  !e.g. CALL IFormClearField(2)
  !CALL IFormShowField(2)
  !CALL IFormEdit(IFINIT,IFEXIT)
  !CALL IFormGetInteger(2,IVALUE)
end subroutine IFormClearField

subroutine IFormGetCheckBox(IFIELD,IVALUE)
  integer IFIELD ! Field number
  integer IVALUE ! Returned check box value (0=clear, 1=set)
  ! Gets a value from a check box field in the current form. If the field has not been
  ! declared as a check box, an error code is set. Otherwise, a valid value is bound to be
  ! returned by this routine since check boxes always have a default value (clear) even if
  ! not explicitly defined by a call to IFormPutCheckBox or a form definition file.
  !e.g. LOGICAL UseColour
  !:
  !CALL IFormGetCheckBox(ICOLFIELD,IVALUE)
  !UseColour = IVALUE.NE.0
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
  !Assign/Retrieve Field Contents INTERACTER Subroutine Reference
  !3-32
end subroutine IFormGetCheckBox

subroutine IFormGetDouble(IFIELD,DVALUE)
  integer IFIELD ! Field number
  double precision DVALUE ! Returned double precision value
  ! (-999.0 if undefined)
  ! Short-name : FMGETD
  ! Gets a value from a double precision field on the current form. If the field has not been
  ! declared as a double precision field an error code is set.
  ! If the field contents are undefined a value of -999.0 is returned and another error code
  ! is set. A field is treated as undefined if IFormPutDouble has not been called to
  ! place data in the field and the user has not entered data in the field.
  ! It will normally only be meaningful to call IFormGetDouble following a call to
  ! IFormEdit and when IFIELD is an unprotected double precision field.
  ! IFormGetDouble will simply return the value stored by IFormPutDouble if the
  ! field is protected or if the user has not entered data in that field.
  !e.g. See IFormDefine
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
  !ErrFieldUndefined (32) : Field value is undefined
  !        type(Fields), pointer :: this
  !        this => this_
  !        DVALUE = this%props(IFIELD / 2 - 1)%floatValue
end subroutine IFormGetDouble

subroutine IFormGetInteger(IFIELD,IVALUE)
  integer IFIELD ! Field number
  integer IVALUE ! Returned integer value (-999 if undefined)
  ! Short-name : FMGETI
  ! Gets a value from an integer field on the current form. If the field has not been declared
  ! as an integer field an error code is set.
  ! If the field contents are undefined a value of -999 is returned and another error code is
  ! set. A field is treated as undefined if IFormPutInteger has not been called to place
  ! data in the field and the user has not entered data in the field.
  ! It will normally only be meaningful to call IFormGetInteger following a call to
  ! IFormEdit and when IFIELD is an unprotected integer field. IFormGetInteger
  ! will simply return the value stored by IFormPutInteger if the field is protected or
  ! if the user has not entered data in that field.
  !e.g. See IFormDefine
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
  !ErrFieldUndefined (32) : Field value is undefined
  !INTERACTER Subroutine Reference Assign/Retrieve Field Contents
  !3-33
  !        type(Fields), pointer :: this
  !        this => this_
  !        IVALUE = this%props(IFIELD / 2 - 1)%intValue
end subroutine IFormGetInteger

subroutine IFormGetMenu(IFIELD,IOPTN)
  integer IFIELD ! Field number
  integer IOPTN ! Returned number of selected menu item
  ! (zero if undefined)
  ! Short-name : FMGETM
  ! Gets a menu selection from a menu field on the current form. If the field has not been
  ! declared as a menu (i.e. field type 4 or 7) an error code is set.
  ! If the field contents are undefined a value of zero is returned and another error code is
  ! set. A field is treated as undefined if IFormPutMenu has not been called to place
  ! menu options in the field.
  ! It will normally only be meaningful to call IFormGetMenu following a call to
  ! IFormEdit and when IFIELD is an unprotected menu field. IFormGetMenu will
  ! simply return the initial option set by IFormPutMenu or IFormPutOption if the
  ! field is protected or if the user has not made any menu selection in that field.
  !e.g. See IFormDefine
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
  !ErrFieldUndefined (32) : Field value is undefined
end subroutine IFormGetMenu

subroutine IFormGetRadioButton(IFIELD,IVALUE)
  integer IFIELD ! Field number of any field in a radio button group
  integer IVALUE ! Relative position of currently selected item
  ! (1=1st, 2=2nd, etc. 0=none)
  ! Gets a value from the radio button group which includes the specified check-box field
  ! on the current form. If the field has not been declared as a check-box and a radio-button
  ! group member, an error code is set.
  ! The returned value is the relative position of the currently selected field within the
  ! group. So, if the third field is currently 'checked', IVALUE will be returned as 3. If all
  !the check boxes are clear (e.g. because IFormClearField has been called),IVALUE returns zero.
  !To interrogate the current state of a specific check box in a radio button group, call
  !IFormGetCheckBox.
  !e.g. See formdem6 demo program
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
  !Assign/Retrieve Field Contents INTERACTER Subroutine Reference
  !3-34
end subroutine IFormGetRadioButton

subroutine IFormGetReal(IFIELD,RVALUE)
  integer IFIELD ! Field number
  real RVALUE ! Returned real value (-999.0 if undefined)
  ! Short-name : FMGETR
  ! Gets a value from a real field on the current form. If the field has not been declared as a
  ! real field an error code is set.
  ! If the field contents are undefined a value of -999.0 is returned and another error code
  ! is set. A field is treated as undefined if IFormPutReal has not been called to place
  ! data in the field and the user has not entered data in the field.
  ! It will normally only be meaningful to call IFormGetReal following a call to
  ! IFormEdit and when IFIELD is an unprotected real field. IFormGetReal will
  ! simply return the value stored by IFormPutReal if the field is protected or if the
  ! user has not entered data in that field.
  !e.g. See IFormDefine
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
  !ErrFieldUndefined (32) : Field value is undefined

  !        type(Fields), pointer :: this
  !        this => this_
  !        RVALUE = this%props(IFIELD / 2 - 1)%floatValue
end subroutine IFormGetReal

subroutine IFormGetString(IFIELD,CVALUE)
  integer IFIELD ! Field number
  character(len=*) CVALUE ! Returned character value (blank if undefined)
  ! Short-name : FMGETS
  ! Gets a string from a character string or long-string field on the current form. If the field
  ! has not been declared as a character field (type 1, 8 or 10) an error code is set.
  ! If the field contents are undefined a blank string is returned and another error code is
  ! set. A field is treated as undefined if IFormPutString has not been called to place
  ! data in the field and the user has not entered data in the field.
  ! It will normally only be meaningful to call IFormGetString following a call to
  ! IFormEdit and when IFIELD is an unprotected character string or long-string field.
  ! IFormGetString simply returns the string stored by IFormPutString if the
  ! field is protected or the user has not entered data in that field.
  !e.g. See IFormDefine
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
  !ErrFieldUndefined (32) : Field value is undefined
  !INTERACTER Subroutine Reference Assign/Retrieve Field Contents
  !3-35

  !        type(Fields), pointer :: this
  !        this => this_
  !        CVALUE = this%props(IFIELD / 2 - 1)%stringValue
end subroutine IFormGetString

subroutine IFormPutButton(IFIELD,CVALUE,IEXITK)
  integer IFIELD ! Field number
  character(len=*) CVALUE ! Character string to be placed in field
  integer IEXITK ! Exit key code to return when a mouse button or the
  ! confirm key is pressed on this field
  ! Short-name : FMPUTB
  ! Puts a string and an exit key code into a push button field on the current form. If the
  ! field has not been declared as a push button field an error code is set. This call has no
  ! immediate on-screen effect. The field value will only be displayed when IFormShow
  ! or IFormShowField is called.
  ! Typically, CVALUE will contain a single word option such as 'OK', 'Confirm', 'Help','Cancel', 'Quit' etc. reflecting the exit key which the field simulates. When the user
  ! clicks a mouse button on the field or presses the confirm key when the form highlight is
  ! on the field, the form editor will exit with InfoInput(55) set to the value defined
  ! in IEXITK. This would normally be a value in the ranges 21-23, 26-28 or 36-70, for
  ! consistency with normal form exit keys, since this simplifies exit code checking on
  ! return from IFormEdit/IFormEditUser. However, IEXITK need not be
  ! restricted to these values.
  ! If field IFIELD is defined to be protected, it effectively becomes a protected string
  ! field in which case IEXITK is stored but not used, unless IFormProtection is
  ! called to change the field's protection status.
  ! The exit code assigned to a specific button field can be retrieved subsequently via
  ! InfoField.
  !e.g. CALL IFormPutButton(10,'Confirm',21)
  !CALL IFormPutButton(11,'Cancel',23)
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
  !ErrFullDataArea (33) : Internal character data storage area full
end subroutine IFormPutButton

subroutine IFormPutCheckBox(IFIELD,IVALUE,IDFIELD)
  integer IFIELD ! Field number
  integer IVALUE ! Check box field value (0=clear, 1=set)
  integer IDFIELD ! Field number of check box description string
  ! (0=none, -1=use current value)
  ! Puts a check box field value and optionally establishes a link to an associated
  ! description string field. If the field has not been declared as a check box, an error code
  ! is set. This call has no immediate on-screen effect. The field value will only be
  ! displayed when IFormShow or IFormShowField is called.
  ! IVALUE is a value of 0 or 1. When a check box is set to zero, the field is blank. When
  ! it is one, a character appears in the field to indicate it is set. The user can toggle the
  ! state of the field by clicking on it with the mouse. The same effect can be achieved via
  ! the keyboard using control key 35 (usually the space bar) when the field is highlighted.
  ! Assign/Retrieve Field Contents INTERACTER Subroutine Reference
  ! 3-36
  ! IDFIELD can be used to establish a link to a description field (i.e. a field of type 10).
  ! When such a link is established, clicking on the check box not only toggles its state but
  ! moves the field highlight to the description field rather than the check box. Further
  ! clicks on the check box (or the description string field) will continue to cycle the check
  ! box state whilst leaving the highlight on the description field. If using the keyboard, an
  ! attempt to tab into the check box field will also move the highlight into the description
  ! field, but will not cycle the field until control key 35 is pressed.
  ! Whilst the above behaviour is consistent with most popular GUI's, some developers
  ! may prefer to force the user to click in the check box itself and not have the highlight
  ! move through the description field. Set IDFIELD to zero in this case.
  !If the description field number has already been defined (e.g. in a form definition file)
  !and does not need to be changed, simply pass IDFIELD set to -1.
  !If check box and description fields are to be connected, it is advisable to define them
  !consecutively in the form. The contents of check box descriptions fields (type 10) can
  !be set via IFormPutString.
  !e.g. See formdem1/2/3/4/5 demonstration programs
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
end subroutine IFormPutCheckBox

subroutine IFormPutDouble(IFIELD,DVALUE,FRMAT)
  integer IFIELD ! Field number
  double precision DVALUE ! Double precision value to be placed in field
  character(len=*) FRMAT ! Fortran FORMAT to use when displaying
  ! field contents
  ! Short-name : FMPUTD
  ! Puts a value into a double precision field on the current form. If the field has not been
  ! declared as a double precision field an error code is set. This call has no immediate onscreen
  ! effect. The field value will only be displayed when IFormShow,IFormShowUnp or IFormShowField is called.
  ! FRMAT determines the format in which the double precision value will be displayed. If
  ! FRMAT is blank, IFormPutDouble takes one of two actions :
  ! 1) If, since the most recent call to IFormDefine, IFormPutDouble has been
  ! called with the specified value of IFIELD the format which was stored at that call
  ! will be used.
  ! 2) If IFormPutDouble has not been called with the specified value of IFIELD
  ! since IFormDefine was last called and the field contents were not defined in a
  ! form definitions file, a default format is set up. This uses Fortran F format, the field
  ! width and a single decimal place. Hence if the field width is 6 characters, the stored
  ! format will be (F6.1).
  ! To define FRMAT without specifying a field value, use IFormPutFormat instead.
  ! If range checking has been enabled on this field, using IFormRangeDouble, an
  ! attempt to put an out-of-range value into the field will be rejected and an error code
  ! will be set.
  ! INTERACTER Subroutine Reference Assign/Retrieve Field Contents
  ! 3-37
  ! Errors :
  ! ErrBadFieldType (31) : Field types do not match
  ! ErrFullDataArea (33) : Internal character data storage area full
  ! ErrOutOfRange (36) : Attempt to store an out of range field value

  !        type(Fields), pointer :: this
  !        this => this_
  !        this%props(IFIELD / 2 - 1)%floatValue = DVALUE
end subroutine IFormPutDouble

subroutine IFormPutFormat(IFIELD,FRMAT)
  integer IFIELD ! Field number
  character(len=*) FRMAT ! Fortran FORMAT to use when displaying field contents
  ! Short-name : FMPUTF
  ! Defines the output format to be used when displaying real or double precision fields. If
  ! the field has not been declared as real or double precision, an error code is set.
  ! IFormPutFormat is directly equivalent to IFormPutReal/IFormPutDouble,except that the field value is not specified.
  ! This routine will mainly be useful where a field format needs to be specified, but it is
  ! not desirable or meaningful to provide a default value.
  !e.g. CALL IFormPutFormat(IFIELD,'(F12.4)')
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
end subroutine IFormPutFormat

subroutine IFormPutHelp(IFIELD,HLPMES)
  integer IFIELD ! Field number
  character(len=*) HLPMES ! Help message
  ! Short-name : FMPUTH
  ! Defines a field-dependent message to be displayed in the help field. The position and
  ! size of the help field is declared using IFormHelp.
  ! Each field on the form can have its own help message which is only displayed when the
  ! user moves the cursor onto that field in IFormEdit. Initially, when IFormDefine
  ! is called, all help field messages are treated as undefined, though IFormLoad allows
  ! such messages to be defined. The help field will be cleared by IFormEdit whenever
  ! the cursor is in a field for which the help message is undefined. Hence it is only
  ! necessary to define help messages for those fields which require additional prompts.
  ! When defining a help string for a check box field be sure to specify the number of the
  ! field which the form highlight actually moves through, i.e. the actual check box field
  ! number if it has no associated description field or the description field number
  ! otherwise.
  !e.g. CALL IFormHelp(IX,IY,IWIDTH)
  !CALL IFormPutHelp(5,'Space bar cycles available options')
  !CALL IFormPutHelp(7,'Negative value for anti-clockwise motion')
  !Errors :
  !ErrFullDataArea (33) : Internal character data storage area full
  !Assign/Retrieve Field Contents INTERACTER Subroutine Reference
  !3-38
  !        type(Fields), pointer :: this
  !        this => this_
  !        this%props(IFIELD / 2 - 1)%helpText = HLPMES
end subroutine IFormPutHelp

subroutine IFormPutInteger(IFIELD,IVALUE)
  integer IFIELD ! Field number
  integer IVALUE ! Integer value to be placed in field
  ! Short-name : FMPUTI
  ! Puts a value into an integer field on the current form. If the field has not been declared
  ! as an integer field an error code is set. This call has no immediate on-screen effect. The
  ! field value will only be displayed when IFormShow, IFormShowUnp or
  ! IFormShowField is called.
  ! If range checking has been enabled on this field, using IFormRangeInteger, an
  ! attempt to put an out-of-range value into the field will be rejected and an error code
  ! will be set.
  !e.g. See IFormDefine
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
  !ErrOutOfRange (36) : Attempt to store an out of range field value
  !        type(Fields), pointer :: this
  !        this => this_
  !        this%props(IFIELD / 2 - 1).intValue = IVALUE
end subroutine IFormPutInteger

subroutine IFormPutMenu(IFIELD,OPTION,MAXOPT,IOPTN)
  integer IFIELD ! Field number
  integer MAXOPT ! Number of menu options
  character(len=*) OPTION(MAXOPT) ! Array of menu options
  integer IOPTN ! Number of initially highlighted option
  ! Short-name : FMPUTM
  ! Puts a set of options into a cycling or vertical menu field on the current form. If the
  ! field has not been declared as a menu field an error code is set. This call has no
  ! immediate on-screen effect. The field value will be displayed when IFormShow,IFormShowUnp or IFormShowField is called. At that point, the array element
  ! OPTION(IOPTN) will be displayed in the required field. IOPTN is also stored as the
  ! default menu selection and will be returned by IFormGetMenu if no menu selection
  ! is made by the user.
  ! The first time IFormPutMenu is called, for a given field, after a form has been
  ! created using IFormDefine, MAXOPT also defines the maximum number of menu
  ! options this field can hold. Subsequent calls to IFormPutMenu can then vary the
  ! number of menu options provided they do not increase the menu size beyond the
  ! originally defined maximum. Similarly, if the form is initially defined using a form
  ! definition file (via IFormLoad) subsequent calls to IFormPutMenu can vary the
  ! menu size, subject to the same limitation, that they may not increase the menu size.
  ! This allows the initial menu declaration to reserve the maximum amount of menu space
  ! which will ever be required, while still allowing the actual menu items to be changed
  ! dynamically without redefining the entire form.
  ! By default, type 4 menu fields are displayed in a simple cycling format, which uses
  ! IMenuCycle in the form editor. These fields can optionally be displayed in a vertical
  ! pop-up format, under keyboard/mouse control. See IFormPopUpMenu for details.
  ! The availability/size of a pop-up menu is unaffected by a call to IFormPutMenu.
  ! INTERACTER Subroutine Reference Assign/Retrieve Field Contents
  ! 3-39
  ! Type 7 menus are displayed as a vertical list of options, which uses IMenuScroll in
  ! the form editor. If a form is created using IFormDefine, the first subsequent call to
  ! IFormPutMenu sets the number of visible options equal to MAXOPT and suppresses a
  ! frame. However, these settings can be subsequently modified (to give a scrolling
  ! vertical menu and/or a frame) by calling IFormVerticalMenu. Forms defined via
  ! an IFD file can include the total menu size, number of visible options and frame type as
  ! separate parameters.
  ! The current and maximum number of options in a specific menu field can be retrieved
  ! subsequently via InfoField. The option strings are accessible via InfoFormMenu.
  !e.g. See IFormDefine
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
  !ErrFullDataArea (33) : Internal character data storage area full
end subroutine IFormPutMenu

subroutine IFormPutOption(IFIELD,IOPTN)
  integer IFIELD ! Field number
  integer IOPTN ! Menu option number
  ! Short-name : FMOPTN
  ! Defines a menu option number for the specified field. Calling IFormPutOption is
  ! equivalent to calling IFormPutMenu except that the menu option strings and the
  ! maximum number of options need not be respecified. IFormPutMenu must have
  ! been called at least once since the form was defined, before IFormPutOption can be
  ! used to update the current selection.
  ! If the field is not a menu (i.e. field type 4 or 7) or IFormPutMenu has not been
  ! called, the call is ignored. The current selection displayed in IFIELD will not be
  ! updated until the next call to IFormShow, IFormShowUnp or IFormShowField.
  !e.g. CALL IFormPutMenu(IFIELD,OPTION,MAXOPT,IOPT1)
  !CALL IFormShow
  !100 CALL IFormEdit(IFINIT,IFEXIT)
  !CALL IFormGetInteger(IFL2,INTVAL)
  !CALL IFormGetMenu(IFIELD,IOPT2)
  !IF (IOPT2.EQ.2.AND.INTVAL.GT.0) THEN
  !CALL IFormPutOption(IFIELD,IOPT1)
  !GOTO 100
  !ELSE ...
end subroutine IFormPutOption

subroutine IFormPutRadioButton(IFIELD)
  integer IFIELD ! Field number
  ! Enables the specified check-box field in a radio-button group and clears all the other
  ! check-boxes in the same group. The on-screen radio button group will be updated at the
  ! next call to IFormShow/IFormShowField/etc.
  !e.g. See formdem6 demo program
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
  !Assign/Retrieve Field Contents INTERACTER Subroutine Reference
  !3-40
end subroutine IFormPutRadioButton

subroutine IFormPutReal(IFIELD,RVALUE,FRMAT)
  integer IFIELD ! Field number
  real RVALUE ! Real value to be placed in field
  character(len=*) FRMAT ! Fortran FORMAT to use when displaying field contents
  ! Short-name : FMPUTR
  ! Puts a value into a real field on the current form. If the field has not been declared as a
  ! real field an error code is set. This call has no immediate on-screen effect. The field
  ! value will only be displayed when IFormShow, IFormShowUnp or
  ! IFormShowField is called.
  ! FRMAT determines the format in which the real value will be displayed. If FRMAT is
  ! blank, IFormPutReal takes one of two actions :
  ! 1) If, since the most recent call to IFormDefine, IFormPutReal has been called
  ! with the specified value of IFIELD the format which was stored at that call will be
  ! used.
  ! 2) If IFormPutReal has not been called with the specified value of IFIELD since
  ! IFormDefine was last called and the field contents were not defined in a form
  ! definitions file, a default format is set up. This uses Fortran F format, the field
  ! width and a single decimal place. Hence if the field width is 6 characters, the stored
  ! format will be (F6.1).
  ! To define FRMAT without specifying a field value, use IFormPutFormat instead.
  ! If range checking has been enabled on this field, using IFormRangeReal, an attempt
  ! to put an out-of-range value into the field will be rejected and an error code will be set.
  !e.g. See IFormDefine
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
  !ErrFullDataArea (33) : Internal character data storage area full
  !ErrOutOfRange (36) : Attempt to store an out of range field value
  !        type(Fields), pointer :: this
  !        this => this_
  !        this%props(IFIELD / 2 - 1)%floatValue = RVALUE
end subroutine IFormPutReal

subroutine IFormPutString(IFIELD,CVALUE)
  integer IFIELD ! Field number
  character(len=*) CVALUE ! Character string to be placed in field
  ! Short-name : FMPUTS
  ! Puts a string into a character string, long-string or check-box description string field on
  ! the current form. If the field has not been declared as a character field (i.e. type 1, 8 or
  ! 10) an error code is set. This call has no immediate on-screen effect. The field value
  ! will only be displayed when IFormShow, IFormShowUnp or IFormShowField
  ! is called.
  ! INTERACTER Subroutine Reference Assign/Retrieve Field Contents
  ! 3-41
  ! If IFIELD identifies a long-string field, then the passed length of CVALUE in the first
  ! call to IFormPutString after the form is defined determines the maximum string
  ! length which can be entered in this field. If the form editor is called before
  ! IFormPutString has been called for the first time and the user enters an undefined
  ! long-string field, the maximum length is set to an arbitrary default value of 80
  ! characters. If a form is loaded from a file via IFormLoad, the maximum field length
  ! can alternatively be defined in the appropriate [values] section record.
  !e.g. See IFormDefine
  !Errors :
  !ErrBadFieldType (31) : Field types do not match
  !ErrFullDataArea (33) : Internal character data storage area full
  !
  !INTERACTER Subroutine Reference Display Form Fields/Boxes
  !3-43
  !3.4 Group FM(4) : Display Form Fields/Boxes
  !This group consists of routines to display the form on screen. The whole form is
  !displayed by IFormShow. Alternatively selected fields can be redisplayed using
  !IFormShowField or IFormShowUnp. Individual field frames and form-boxes can
  !be displayed using IFormShowFrame and IFormShowBox respectively. In a tabbed
  !form, IFormShowSubForm displays a specific sub-form. These routines can be
  !called any number of times and in any order/combination, once a form has been
  !created.
  !        type(Fields), pointer :: this
  !        this => this_
  !        if (mod(IFIELD, 2) == 0) then
  !            call free(this%props(IFIELD / 2 - 1)%stringValue)
  !            this%props(IFIELD / 2 - 1)%stringValue = CVALUE
  !        else
  !            call free(this%props((IFIELD + 1) / 2 - 1)%caption)
  !            this%props((IFIELD + 1) / 2 - 1)%caption = CVALUE
  !        end if
end subroutine IFormPutString

subroutine IFormShow
  ! Short-name : FMSHOW
  ! Displays the current form on the screen. All of the fields and form-boxes in the form are
  ! redisplayed, including any field frames, reflecting any changes made by the various
  ! IFormPut routines. The screen or window is not cleared. This can be performed
  ! optionally using IClearScreen or IWinClear, before calling IFormShow.
  ! Any form-boxes defined as part of the form are displayed first, before the form fields.
  ! By default, all form-boxes are displayed using the frame-types/colours currently
  ! selected by the routines in the AT/CG groups (either directly or via
  ! IFormDefaults). Individual form-box attributes can be set using
  ! IFormBoxN/IFormBox. The form-box display incorporates a frame-intersection
  ! algorithm which ensures that frames are usually joined up neatly.
  ! After the form-boxes (if any), the field contents and their frames are displayed. By
  ! default, all form fields are displayed using the attributes/colours currently selected by
  ! the routines in the AT group (either directly or via IFormDefaults). Individual field
  ! attributes can be set using IFormAttributeN/IFormAttribute.
  ! If field frames are enabled they are displayed in the colours set by IFormFrame or the
  ! IFD file. If any field frames are enabled with their colour set to 'use the default' their
  ! colours are determined by ITextColour, ITextColourN or IFrameOptions.
  ! Any 'background' (i.e. text/graphics already on the screen) will not be affected by
  ! IFormShow unless the form-boxes and fields overlap with the background. If only the
  ! unprotected form fields need to be updated use the alternative IFormShowUnp. To
  ! update just one field, call IFormShowField. To redisplay an individual field frame,call IFormShowFrame.
  ! Numeric fields are displayed right justified by default. This can be changed by calling
  ! IOutJustifyNum.
  ! Vertical menus (type 7 fields) are displayed with an extra space at the left and right of
  ! the field (plus a frame if enabled). The currently selected menu item is marked with a '*'
  ! to the left of the option string.
  ! In a multi-part tabbed form, the current sub-form is displayed, Initially this is sub-form
  ! number one. Call IFormShowSubForm to display and select an alternative sub-form.
  !e.g. See IFormDefine
  !Display Form Fields/Boxes INTERACTER Subroutine Reference
  !3-44

  !        type(QTPropertyEditorHandle) editor
  !        integer i
  !        integer(INT32), dimension(0:this_%propCount - 1) :: ids
  !        type(Fields), pointer :: this
  !        this => this_
  !
  !        do i = 0, this%propCount - 1
  !            ids(i) = i
  !        end do
  !
  !        editor = QTPropertyEditor_new(this%propCount, ids, this%docProps, 0, &
  !            str(this%dialogCaption))
  !
  !        this%exitCode = CANCEL
  !
  !        call show(editor)

end subroutine IFormShow

subroutine IFormShowBox(IBOX)
  integer IBOX ! Form-box number
  ! Displays the specified form-box. Alternatively, all form-boxes and form fields can be
  ! displayed in a single call using IFormShow. In a tabbed form, no action is taken if the
  ! specified box is not on the current sub-form.
  ! By default, the form-box is displayed using the frame type and colours currently
  ! selected by the routines in the CG and/or AT groups (either directly or via
  ! IFormDefaults), unless attributes have been defined for this field in the form
  ! definition file or via IFormBoxN/IFormBox.
end subroutine IFormShowBox

subroutine IFormShowField(IFIELD)
  integer IFIELD ! Field number
  ! Short-name : FMSHOF
  ! Displays the contents of the specified form field on the screen. Only field IFIELD is
  ! redisplayed, reflecting any changes made to that field by the appropriate IFormPut
  ! routine. The field frame, if any, is not redrawn except when IFIELD specifies a
  ! vertical menu. In a tabbed form, no action is taken if the specified field is not on the
  ! current sub-form.
  ! Alternatively, all fields can be displayed in a single call using IFormShow, or all
  ! unprotected fields can be displayed using IFormShowUnp.
  ! By default, the field is displayed using the attributes and colours currently selected by
  ! the routines in the AT group (either directly or via IFormDefaults), unless
  ! attributes have been defined for this field in the form definition file or via
  ! IFormAttributeN/IFormAttribute.
  !e.g. CALL IFormPutString(10,'New value')
  !CALL IFormShowField(10)
end subroutine IFormShowField

subroutine IFormShowFrame(IFIELD)
  integer IFIELD ! Field number
  ! Displays the frame, if selected, around the specified field. The field contents are not
  ! redisplayed. This routine complements IFormShowField. Alternatively, all fields
  ! and their frames can be displayed in a single call using IFormShow. The field is
  ! displayed in the style specified by IFormFrame or IFormFrameN, or in the IFD
  ! file.
  ! In a tabbed form, no action is taken if the specified field is not on the current sub-form.
  !e.g. CALL IFormShowField(10)
  !CALL IFormShowFrame(10)
end subroutine IFormShowFrame

subroutine IFormShowSubForm(IFORM)
  integer IFORM ! Sub-form
  ! Displays and selects the specified sub-form in a multi-part tabbed form. The whole of
  ! the sub-form is displayed and the form tabs are updated to match the selected sub-form.
  ! Alternatively, call IFormShow to simply re-display the currently selected sub-form.
  ! INTERACTER Subroutine Reference Display Form Fields/Boxes
  ! 3-45
end subroutine IFormShowSubForm

subroutine IFormShowUnp
  ! Short-name : FMSHUN
  ! Displays all of the unprotected fields in the current form or sub-form on the screen.
  ! This routine is directly equivalent to IFormShow, except that protected fields are not
  ! displayed and field frames are not redrawn.
  ! Typically, IFormShow will be called once when a form is first defined and
  ! IFormShowUnp will be used thereafter to update the unprotected data entry fields.
  ! This cuts down on screen repainting, which may be noticeable on some slower displays,such as serial terminals. Further screen update optimisation may be achievable by
  ! calling IFormShowField which updates a single field.
  ! See IFormShow for additional information.
  !e.g. CALL IFormShow
  !100 CALL IFormEdit(IFINIT,IFNEXT)
  !CALL IFormGetInteger(IFIELD,INTVAL)
  !IF (INTVAL.LE.0) THEN
  !CALL IOutError('Value must be greater than zero')
  !IFINIT = IFIELD
  !CALL IFormPutInteger(IFIELD,1)
  !CALL IFormShowUnp
  !GOTO 100
  !ENDIF
  !
  !INTERACTER Subroutine Reference Screen Manipulation
  !4-1
  !4. General Functions : Subroutine Descriptions
  !4.1 Group SC : Screen Manipulation
  !The routines in this group provide a miscellaneous selection of general screen
  !manipulation facilities. Most important of all are the INTERACTER initialisation and
  !termination routines (IScreenOpen, IScreenClose). Other important routines
  !include screen mode selection (IScreenMode, IScreenModeN), save/load
  !(IScreenSaveImage, IScreenLoadImage), copy/paste (IScreenCopy,IScreenPaste) and printer dumps (IScreenDump, IScreenDumpOptions).
  !Also provided are routines to scroll the screen (IScreenScroll), control the bell
  !(IScreenBell) and fill the screen with a 'desktop' pattern
  !(IScreenBackground). IScreenModeOptions and IScreenTitle select
  !screen options/title. These are mainly useful under X Windows and MS Windows.
  !Screen output buffering, controlled by IScreenBuffer, may prove useful on multiuser
  !systems. IScreenCode identifies the text/graphics mode switching control codes
  !for dual VT/Tektronix terminals not otherwise explicitly supported by INTERACTER.
end subroutine IFormShowUnp


INTEGER function IFormExitCode() result(res)
  !        type(Fields), pointer :: this
  !        this => this_
  !
  !        res = this%exitCode
    res = 0
end function IFormExitCode

subroutine IFormSetCaption(STRING)
  character(len=*) STRING ! String to write out
  !        type(Fields), pointer :: this
  !        this => this_
  !
  !        call free(this%dialogCaption)
  !        this%dialogCaption = STRING
end subroutine IFormSetCaption
!end module


