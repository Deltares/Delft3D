#pragma  once

#include "ThirdPartyHeadersBegin.h"
#  include <QCheckBox>
#  include <string>
#  include <QString>
#  include <boost/scoped_ptr.hpp>
#include "ThirdPartyHeadersEnd.h"

#include "TECADDON.h"
#include "tecgui_Exports.h"

#include "ClassMacros.h"

namespace tecplot { namespace tecgui {

class TecGUIToggle
    : public QCheckBox
{
    Q_OBJECT
public:

    TecGUIToggle(
        LgIndex_t               parentDialogId,
        LgIndex_t               x,
        LgIndex_t               y,
        LgIndex_t               width,
        LgIndex_t               height,
        std::string const&      labelString,
        TecGUIIntCallback_pf    valueChangedCallback
        );

    virtual ~TecGUIToggle();

    private slots:
        void onValueChangedCallback(int state);

private:
    struct Impl;
    boost::scoped_ptr<Impl> m_impl;

    UNCOPYABLE_CLASS(TecGUIToggle);
};

}}
