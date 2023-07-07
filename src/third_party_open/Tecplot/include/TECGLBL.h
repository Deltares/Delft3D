/*
******************************************************************
******************************************************************
*******                                                   ********
******  (C) 1988-2010 Tecplot, Inc.                        *******
*******                                                   ********
******************************************************************
******************************************************************
*/
/*
 * TECGLBL.h  .... GLOBAL include file for all tecutil?.c files.
 */
#ifndef _TECGLBL_H
#define _TECGLBL_H

#include "MASTER.h"
#include "GLOBAL.h"

/* CORE SOURCE CODE REMOVED */



extern Boolean_t isInTecUtilLightweightLoopSequence; /* global */

/*
 * defgroup and definternalattribute items are processed by
 * processTecHeaders.py during the UNIX build process. This python
 * script is maintained in source control in the 'base\util' directory
 *
 * processTecHeaders.py will strip unused @defgroup items, to avoid
 * empty groups in the Reference manual. All @defgroup items must be
 * within this single comment block or they will not be properly stripped.
 */

/**
 * @defgroup AddOnLoaders          Data Loader Addons
 * @defgroup AddOnManagement       AddOn Management
 * @defgroup Animation             Animation
 * @defgroup ArgLists              Argument Lists
 * @defgroup AuxData               Auxiliary Data
 * @defgroup Axis                  Axis Control
 * @defgroup Blanking              Blanking
 * @defgroup ColorMap              Colormaps
 * @defgroup Contour               Contour
 * @defgroup DataFileSupport       Data Files (Tecplot Format)
 * @defgroup DataLoad              Data Loading
 * @defgroup DataManipulation      Data Manipulation
 * @defgroup DataSetInfo           Data Set Information
 * @defgroup DataServices          Data Services
 * @defgroup DataSharing           Data Sharing
 * @defgroup DataStructure         Data Structure
 * @defgroup DataValue             Data Value
 * @defgroup Drawing               Drawing Commands
 * @defgroup Export                Export
 * @defgroup FaceNeighbors         Face Neighbors
 * @defgroup FieldMap              Fieldmap Commands
 * @defgroup FrameManagement       Frame Commands
 * @defgroup Geom                  Geometry
 * @defgroup Image                 Images
 * @defgroup LayoutSupport         Layouts
 * @defgroup LineMap               Linemap Commands
 * @defgroup Lock                  Locking Commands
 * @defgroup Mouse                 Mouse
 * @defgroup PageManagement        Paper and Page Management
 * @defgroup Pick                  Pick
 * @defgroup PolyhedralData        Polyhedral Data
 * @defgroup Print                 Print
 * @defgroup Probe                 Probe
 * @defgroup ScriptSupport         Python and Macro Script Support
 * @defgroup Scatter               Scatter
 * @defgroup Set                   Set Commands
 * @defgroup StateChange           State Changes
 * @defgroup StatusBar             Status Bar
 * @defgroup Streamtraces          Streamtraces
 * @defgroup StringList            String Lists
 * @defgroup StyleValue            Style Value and Style Base Commands
 * @defgroup Stylesheets           Stylesheets
 * @defgroup Text                  Text
 * @defgroup Threading             Threading
 * @defgroup Time                  Time
 * @defgroup Undo                  Undo
 * @defgroup UserInterface         User Interface
 * @defgroup Utilities             Utilites
 * @defgroup Variables             Variables
 * @defgroup Vector                Vector
 * @defgroup View                  View
 * @defgroup WorkArea              WorkArea
 * @defgroup Zone                  Zone
 * @defgroup DataCreate            Zone Creation
 * @defgroup TGB                   Tecplot GUI Builder
 * @defgroup TecEng                Tecplot Engine
 * @defgroup TecApp                Tecplot Application Integration
 */


/*
 * List of allowed internal attributes. Note that we are using a syntax
 * similar to DOXYGEN, but we can't use the @ symbol otherwise DOXYGEN
 * picks it up and complains about it.
 *
 * #definternalattribute motif_only         Motif (UNIX) only item
 * #definternalattribute windows_only       Windows only item
 * #definternalattribute exclude_python     Exclude from python glue layer (gencode)
 * #definternalattribute exclude_fglue      Exclude from fortran glue layer (gencode)
 * #definternalattribute exclude_tcl        Exclude from tcl glue layer (gencode)
 * #definternalattribute exclude_all        Exclude from gencode processing entirely
 * #definternalattribute exclude_alldoc     Exclude from all documentation (Focus, 360, SDK)
 * #definternalattribute exclude_tecplotdoc Exclude from all Tecplot documentation (Focus, 360)
 * #definternalattribute exclude_sdkdoc     Exclude from SDK documentation
 */




/** @page notes Additional Notes
 * This page includes additional background information for the following topics:
 *
 * - \ref threadsafe
 *
 * <HR>
 *
 * @section threadsafe Thread Safe
 * Thread safe functions can be called from separate threads.  Refer to the
 * <a href ="../adkum.pdf">ADK User's Manual</a> for additional information.
 */
/* CORE SOURCE CODE REMOVED */

//This macro used to call AfxGetStaticModuleState() or AfxGetAppModuleState() or AFX_MANAGE_STATE(AFXMANAGESTATE) on Windows.
//It also murdered our performance with CFD
#  define MANAGESTATE

/* CORE SOURCE CODE REMOVED */


/* flag for TecUtilSet... functions */
#define TECUTILSETNOTMEMBER (0)
#define TECUTILINVALIDMAP   (0)
#define TECUTILINVALIDZONE  (0)
#define TECUTILINVALIDVAR   (0)
#define TECUTILINVALIDELEM  (0)



/* CORE SOURCE CODE REMOVED */

/* public ArgList structure */
typedef struct _ArgList_s *ArgList_pa;

#define TECUTILBADZONENUMBER 0
#define TECUTILBADVARNUMBER  0

#define TECUTILAUTOMNEMONIC  1

/* Definitions for polyhedral boundary connections. */
#define TECUTIL_NO_NEIGHBORING_ZONE 0
#define TECUTIL_NO_NEIGHBORING_ELEM 0
#define TECUTIL_BOUNDARY_FACE (-1)

/* CORE SOURCE CODE REMOVED */

#endif  /* _TECGLBL_H */
