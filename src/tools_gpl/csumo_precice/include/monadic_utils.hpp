#ifndef SRC_TOOLS_GPL_CSUMO_PRECICE_MONADIC_UTILS_HPP
#define SRC_TOOLS_GPL_CSUMO_PRECICE_MONADIC_UTILS_HPP

#include <expected>
#include <optional>

namespace monadic_utils::detail
{
    template <typename T>
    inline constexpr bool is_optional_v = false;
    template <typename T>
    inline constexpr bool is_optional_v<std::optional<T>> = true;

    template <typename T>
    inline constexpr bool is_expected_v = false;
    template <typename T, typename E>
    inline constexpr bool is_expected_v<std::expected<T, E>> = true;

    template <typename T>
    concept IsOptional = is_optional_v<std::remove_cvref_t<T>>;

    template <typename T>
    concept IsExpected = is_expected_v<std::remove_cvref_t<T>>;

    template <typename T>
    concept IsMonadic = IsOptional<T> || IsExpected<T>;

    template <typename T>
        requires IsOptional<T>
    std::nullopt_t returnNonValue(T&&)
    {
        return std::nullopt;
    }

    template <typename T>
        requires IsExpected<T>
    std::unexpected<typename std::remove_cvref_t<T>::error_type> returnNonValue(T&& expected)
    {
        return std::unexpected{std::forward<T>(expected).error()};
    }
} // namespace monadic_utils::detail

#define ASSIGN_OR_RETURN_CONCAT_(x, y) x##y
#define ASSIGN_OR_RETURN_CONCAT(x, y) ASSIGN_OR_RETURN_CONCAT_(x, y)

#define ASSIGN_OR_RETURN(lhs, expr)                                                                        \
    auto&& ASSIGN_OR_RETURN_CONCAT(_res_, __LINE__) = (expr);                                              \
    if (!ASSIGN_OR_RETURN_CONCAT(_res_, __LINE__))                                                         \
        return monadic_utils::detail::returnNonValue(std::move(ASSIGN_OR_RETURN_CONCAT(_res_, __LINE__))); \
    lhs = *std::move(ASSIGN_OR_RETURN_CONCAT(_res_, __LINE__))

namespace monadic_utils
{
    inline constexpr auto is_valid = []<detail::IsMonadic T>(const T& value) { return static_cast<bool>(value); };

    inline constexpr auto is_invalid = []<detail::IsMonadic T>(const T& value) { return !value; };

    inline constexpr auto unwrap = []<detail::IsMonadic T>(T&& value) { return *std::forward<T>(value); };
} // namespace monadic_utils

#endif // SRC_TOOLS_GPL_CSUMO_PRECICE_MONADIC_UTILS_HPP
