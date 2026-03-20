#ifndef SRC_TOOLS_GPL_CSUMO_PRECICE_ASSIGN_OR_RETURN_HPP
#define SRC_TOOLS_GPL_CSUMO_PRECICE_ASSIGN_OR_RETURN_HPP

#include <expected>
#include <optional>

namespace assign_or_return_detail
{
    template <typename T>
    struct IsOptional : std::false_type
    {
    };

    template <typename T>
    struct IsOptional<std::optional<T>> : std::true_type
    {
    };

    template <typename T>
    struct IsExpected : std::false_type
    {
    };

    template <typename T, typename E>
    struct IsExpected<std::expected<T, E>> : std::true_type
    {
    };

    template <typename T>
        requires IsOptional<std::remove_cvref_t<T>>::value
    std::nullopt_t returnNonValue(T&&)
    {
        return std::nullopt;
    }

    template <typename T>
        requires IsExpected<std::remove_cvref_t<T>>::value
    std::unexpected<typename std::remove_cvref_t<T>::error_type> returnNonValue(T&& expected)
    {
        return std::unexpected{std::move(expected).error()};
    }
} // namespace assign_or_return_detail

#define ASSIGN_OR_RETURN_CONCAT_(x, y) x##y
#define ASSIGN_OR_RETURN_CONCAT(x, y) ASSIGN_OR_RETURN_CONCAT_(x, y)

#define ASSIGN_OR_RETURN(lhs, expr)                                                                          \
    auto&& ASSIGN_OR_RETURN_CONCAT(_res_, __LINE__) = (expr);                                                \
    if (!ASSIGN_OR_RETURN_CONCAT(_res_, __LINE__))                                                           \
        return assign_or_return_detail::returnNonValue(std::move(ASSIGN_OR_RETURN_CONCAT(_res_, __LINE__))); \
    lhs = *std::move(ASSIGN_OR_RETURN_CONCAT(_res_, __LINE__))

#endif // SRC_TOOLS_GPL_CSUMO_PRECICE_ASSIGN_OR_RETURN_HPP
