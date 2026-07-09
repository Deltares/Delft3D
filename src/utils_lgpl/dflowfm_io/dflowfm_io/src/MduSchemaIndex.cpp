#include <dflowfm_io/MduSchemaIndex.h>

namespace dflowfm_io
{

    const MduSchemaIndex& GetMduSchemaIndex()
    {
        static const MduSchemaIndex instance{MDU_SCHEMA};
        return instance;
    }

} // namespace dflowfm_io