#ifndef SRC_TOOLS_GPL_CSUMO_PRECICE_ASSIGN_OR_RETURN_HPP
#define SRC_TOOLS_GPL_CSUMO_PRECICE_ASSIGN_OR_RETURN_HPP

#include <expected>

#define EXPECTED_CONCAT_HELPER_(x, y) x##y
#define EXPECTED_CONCAT_HELPER(x, y) EXPECTED_CONCAT_HELPER_(x, y)

#define ASSIGN_OR_RETURN(lhs, expr)                                              \
    auto&& EXPECTED_CONCAT_HELPER(_res_, __LINE__) = (expr);                     \
    if (!EXPECTED_CONCAT_HELPER(_res_, __LINE__))                                \
        return std::unexpected(EXPECTED_CONCAT_HELPER(_res_, __LINE__).error()); \
    lhs = *std::move(EXPECTED_CONCAT_HELPER(_res_, __LINE__))

#endif // SRC_TOOLS_GPL_CSUMO_PRECICE_ASSIGN_OR_RETURN_HPP
