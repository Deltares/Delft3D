#pragma  once

#include "ThirdPartyHeadersBegin.h"
#  include <QComboBox>
#  include <string>
#  include <boost/scoped_ptr.hpp>
#include "ThirdPartyHeadersEnd.h"

#include "TECADDON.h"
#include "tecgui_Exports.h"

#include "ClassMacros.h"

namespace tecplot { namespace tecgui {

class TecGUIOptionMenu
    : public QComboBox
{
    Q_OBJECT
public:

    TecGUIOptionMenu(
        LgIndex_t 	            parentDialogID,
        LgIndex_t 	            x,
        LgIndex_t 	            y,
        LgIndex_t 	            width,
        LgIndex_t 	            height,
        std::string const&      optionList,
        TecGUIIntCallback_pf 	valueChangedCallback);

    virtual ~TecGUIOptionMenu();

private slots:
    void onCurrentIndexChanged(int index);

private:
    struct Impl;
    boost::scoped_ptr<Impl> m_impl;

    UNCOPYABLE_CLASS(TecGUIOptionMenu);
};

}}
