#pragma once

#include "ThirdPartyHeadersBegin.h"
#  include <QPushButton>
#  include <boost/scoped_ptr.hpp>
#include "ThirdPartyHeadersEnd.h"

#include "ClassMacros.h"
#include "StandardIntegralTypes.h"
#include "tecgui_Exports.h"

namespace tecplot { namespace tecgui {

class TecGUIBitmapButtonAbstract
    : public QPushButton
{
public:
    TecGUIBitmapButtonAbstract(
        LgIndex_t   parentId,
        LgIndex_t   x,
        LgIndex_t   y,
        LgIndex_t   buttonWidth,
        LgIndex_t   buttonHeight,
        LgIndex_t   bitmapWidth,
        LgIndex_t   bitmapHeight,
        char const* bitmapData,
        bool        useTransparentColor,
        LgIndex_t   transparentR,
        LgIndex_t   transparentG,
        LgIndex_t   transparentB);

    virtual ~TecGUIBitmapButtonAbstract();

private:

    struct Impl;
    boost::scoped_ptr<Impl> m_impl;

    UNCOPYABLE_CLASS(TecGUIBitmapButtonAbstract);
};
}}
