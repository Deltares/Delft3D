#pragma once

#include "ThirdPartyHeadersBegin.h"
#  include <QLineEdit>
#  include <QString>
#  include <boost/scoped_ptr.hpp>
#include "ThirdPartyHeadersEnd.h"

#include "TECADDON.h"
#include "tecgui_Exports.h"

#include "ClassMacros.h"

namespace tecplot { namespace tecgui {

class TecGUITextField
    : public QLineEdit
{
    Q_OBJECT
public:

    TecGUITextField(
        LgIndex_t parentDialogId,
        LgIndex_t x,
        LgIndex_t y,
        LgIndex_t width,
        LgIndex_t height,
        TecGUITextCallback_pf textCallback
        );

    /**
     * This constructor is used by SpinTextFields to
     * create the LineEdit used in the control.
     * We do this so that the spin text field can re-use the text callback
     * logic in this class.
     */
    TecGUITextField(TecGUITextCallback_pf textCallback);

    virtual ~TecGUITextField();

private slots:
    void editingFinished();

private:
    struct Impl;
    boost::scoped_ptr<Impl> m_impl;

    UNCOPYABLE_CLASS(TecGUITextField);
};

}}
