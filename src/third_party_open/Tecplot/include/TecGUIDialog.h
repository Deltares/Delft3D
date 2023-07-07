#pragma  once

#include "TECADDON.h"

#include "ThirdPartyHeadersBegin.h"
#  include <QDialog>
#  include <QPushButton>
#  include <boost/scoped_ptr.hpp>
#include "ThirdPartyHeadersEnd.h"

#include "ClassMacros.h"

namespace tecplot { namespace tecgui {

class TecGUIDialog
    : public QDialog
{
    Q_OBJECT
public:
    /**
     * Modal dialog constructor.
     */
    TecGUIDialog(
        LgIndex_t              parentDialogID,
        LgIndex_t              width,
        LgIndex_t              height,
        const char*            title,
        TecGUIVoidCallback_pf  initCallback,
        TecGUIVoidCallback_pf  okButtonCallback,
        TecGUIVoidCallback_pf  applyButtonCallback,
        TecGUIVoidCallback_pf  cancelButtonCallback,
        TecGUIVoidCallback_pf  helpButtonCallback);
                      
    /**
     * Modeless dialog constructor.
     */
    TecGUIDialog(
        LgIndex_t              parentDialogID,
        LgIndex_t              width,
        LgIndex_t              height,
        const char*            title,
        TecGUIVoidCallback_pf  initCallback,
        TecGUIVoidCallback_pf  closeButtonCallback,
        TecGUIVoidCallback_pf  helpButtonCallback);

    virtual ~TecGUIDialog();

    /**
     * WARNING: Even though this class inherits from QDialog,
       do NOT call any QDialog functions (such as show(), exec(), etc.).
       This class will defer launch/drop if they are called from within a
       button box callback.
       
       Use requestDialogDrop() and requestDialogLaunch() instead.
       But you shouldn't ever need to call those functions either, because
       they are already called in the only two places they should be called,
       and they should not be used anywhere else in the tecplot code.

       Unless, that is, you are changing the TecGUI API or its implementation, in which case it is presumed that
       you know what you are doing.
     */

    void requestDialogDrop();
    void requestDialogLaunch();

private slots:
    void dialogButtonBoxClicked (QAbstractButton * button);
    void dialogButtonBoxHelpRequested();
    virtual void accept();
    virtual void reject();

protected:
    virtual void closeEvent(class QCloseEvent* event);

private:
    struct Impl;
    boost::scoped_ptr<Impl> m_impl;

    UNCOPYABLE_CLASS(TecGUIDialog);
};

}}

