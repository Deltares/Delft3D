# Guidelines
This document contains a number of topics we decided on while developing pre_C_Sumo.

## Error handling
- A function in pre_C_Sumo *SHOULD NOT* throw any exceptions. It may still throw exeptions from called standard libraries.
- When functions can fail without a meaningful result, it *MUST* use the mechanism provided by `std::expected` to relay de error to the caller.

