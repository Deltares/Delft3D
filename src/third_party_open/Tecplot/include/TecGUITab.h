#pragma once

#include "ThirdPartyHeadersBegin.h"
#  include <QTabWidget>
#include "ThirdPartyHeadersEnd.h"

#include "ClassMacros.h"
#include "TECADDON.h"
#include "tecgui_Exports.h"

namespace tecplot { namespace tecgui {

class TecGUITab : 
    public QTabWidget
{
    Q_OBJECT
public:
    TecGUITab(
        LgIndex_t parentDialog,
        LgIndex_t x,
        LgIndex_t y,
        LgIndex_t width,
        LgIndex_t height,
        TecGUIIntCallback_pf activateCallback,
        TecGUIIntCallback_pf deactivateCallback);

    virtual ~TecGUITab();

private slots:
    void onCurrentChanged(int index);
    
private:
    TecGUIIntCallback_pf m_activateCallback;
    TecGUIIntCallback_pf m_deactivateCallback;
    int                  m_currentTabPageId;

    UNCOPYABLE_CLASS(TecGUITab);
};

}}



