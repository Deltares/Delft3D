#pragma once

#include <dflowfm_io/dflowfm_io_export.h>

namespace dflowfm_io
{

class DFLOWFM_IO_EXPORT MduModel
{
public:
    MduModel() = default;
    ~MduModel() = default;

    int GetDummyValue() const;
};

} // namespace dflowfm_io
