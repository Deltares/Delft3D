#pragma once
#include "TecGUIBitmapButtonAbstract.h"

namespace tecplot { namespace tecgui {

class TecGUIBitmapToggle
    : public TecGUIBitmapButtonAbstract
{
    Q_OBJECT

public:
    TecGUIBitmapToggle(
        LgIndex_t              parentId,
        LgIndex_t              x,
        LgIndex_t              y,
        LgIndex_t              buttonWidth,
        LgIndex_t              buttonHeight,
        LgIndex_t              bitmapWidth,
        LgIndex_t              bitmapHeight,
        char const*                     bitmapData,
        bool                            useTransparentColor,
        LgIndex_t              transparentR,
        LgIndex_t              transparentG,
        LgIndex_t              transparentB,
        TecGUIIntCallback_pf  buttonCallback);

    virtual ~TecGUIBitmapToggle();

private slots:
    void valueChangedCallback(bool isChecked);

private:
    TecGUIIntCallback_pf   m_buttonCallback;

    UNCOPYABLE_CLASS(TecGUIBitmapToggle);
};
}}

