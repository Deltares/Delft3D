"""The MDU document API: the typed model, the validation report, and the document itself.

:class:`MduModel` is generated from the C header (see :mod:`dflowfm_io.mdu.model`); the document
lifecycle and report in :mod:`dflowfm_io.mdu.document` are hand-written.
"""

from dflowfm_io.mdu.document import MduDocument, MduReport
from dflowfm_io.mdu.model import MduModel

__all__ = ["MduModel", "MduReport", "MduDocument"]
