#pragma  once

#include "ThirdPartyHeadersBegin.h"
#  include <QPlainTextEdit>
#  include <QFocusEvent>
#  include <string>
#  include <boost/scoped_ptr.hpp>
#include "ThirdPartyHeadersEnd.h"

#include "TECADDON.h"
#include "tecgui_Exports.h"

#include "ClassMacros.h"

namespace tecplot { namespace tecgui {

class TecGUIText
    : public QPlainTextEdit
{
    Q_OBJECT
public:

    TecGUIText(
        LgIndex_t 	            parentDialogID,
        LgIndex_t 	            x,
        LgIndex_t 	            y,
        LgIndex_t 	            width,
        LgIndex_t 	            height,
        bool 	                isReadOnly,
        TecGUITextCallback_pf 	valueChangedCallback	
        );

    virtual ~TecGUIText();

private slots:
    void onTextChanged();

private:
    struct Impl;
    boost::scoped_ptr<Impl> m_impl;

    UNCOPYABLE_CLASS(TecGUIText);
};

}}
