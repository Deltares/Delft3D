#pragma once
#include "TecGUIBitmapButtonAbstract.h"

namespace tecplot { namespace tecgui {

class TecGUIBitmapButton
    : public tecplot::tecgui::TecGUIBitmapButtonAbstract
{
    Q_OBJECT

public:
    TecGUIBitmapButton(
        LgIndex_t              parentId,
        LgIndex_t              x,
        LgIndex_t              y,
        LgIndex_t              buttonWidth,
        LgIndex_t              buttonHeight,
        LgIndex_t              bitmapWidth,
        LgIndex_t              bitmapHeight,
        char const*            bitmapData,
        bool                   useTransparentColor,
        LgIndex_t              transparentR,
        LgIndex_t              transparentG,
        LgIndex_t              transparentB,
        TecGUIVoidCallback_pf  buttonCallback);

    virtual ~TecGUIBitmapButton();

private slots:
    void buttonClicked();

private:
    TecGUIVoidCallback_pf  m_buttonCallback;

    UNCOPYABLE_CLASS(TecGUIBitmapButton);
};

}}
