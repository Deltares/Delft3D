#pragma once

#include "tecgui_Exports.h"
#include "CodeContract.h"

// TODO (davido) M 2012/10/05: Implement a real TU_ASSERT

#define TU_REQUIRE(exp, str) REQUIRE(exp)
#define TU_CHECK(exp, str)	 CHECK(exp)

//
// Some add-ons may call TecGUI functions during a shut down
// state change.
// If this happens then the 360 main window may have already been
// destroyed, along with all of our TecUtil widgets.
// Therefore, if the main window is destroyed, any
// TecGUI function should be a NOP.
//
#define TECGUIFUNCTIONSTART(RoutineName) /* Eventually will be used for TU asserts */ \
    if (TecGUIWidgetRepository::instance().mainWindowIsValid()) \
    { 


#define TECGUIFUNCTIONEND \
    }

class QMainWindow;

namespace tecplot { namespace tecgui {
    tecgui_API QMainWindow* findMainWindow();
}}

