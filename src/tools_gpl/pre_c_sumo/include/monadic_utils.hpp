#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_MONADIC_UTILS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_MONADIC_UTILS_HPP

#include <expected>
#include <optional>

namespace monadic_utils::internal
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

    // Helper function template for ASSIGN_OR_RETURN. Being a function template makes the if constexpr
    // body properly dependent, so the discarded branch is not checked in non-template
    // call sites (unlike if constexpr directly inside the macro expansion).
    template <IsMonadic ExprType>
    [[nodiscard]] constexpr auto makeErrorReturn(ExprType&& res)
    {
        if constexpr (IsExpected<ExprType>)
        {
            return std::unexpected(std::forward<ExprType>(res).error());
        }
        else
        {
            return std::nullopt;
        }
    }

} // namespace monadic_utils::internal

#define MONADIC_UTILS_CONCAT_(x, y) x##y
#define MONADIC_UTILS_CONCAT(x, y) MONADIC_UTILS_CONCAT_(x, y)

#define ASSIGN_OR_RETURN(lhs, expr)                                                                        \
    auto&& MONADIC_UTILS_CONCAT(_res_, __LINE__) = (expr);                                                 \
    if (!MONADIC_UTILS_CONCAT(_res_, __LINE__))                                                            \
        return monadic_utils::internal::makeErrorReturn(std::move(MONADIC_UTILS_CONCAT(_res_, __LINE__))); \
    lhs = *std::move(MONADIC_UTILS_CONCAT(_res_, __LINE__))

#define RETURN_IF_ERROR(expr)                                                                                  \
    do                                                                                                         \
    {                                                                                                          \
        auto&& MONADIC_UTILS_CONCAT(_res_, __LINE__) = (expr);                                                 \
        if (!MONADIC_UTILS_CONCAT(_res_, __LINE__))                                                            \
            return monadic_utils::internal::makeErrorReturn(std::move(MONADIC_UTILS_CONCAT(_res_, __LINE__))); \
    } while (false)

namespace monadic_utils
{
    inline constexpr auto is_valid = []<internal::IsMonadic T>(const T& value) { return static_cast<bool>(value); };

    inline constexpr auto is_invalid = []<internal::IsMonadic T>(const T& value) { return !value; };

    inline constexpr auto unwrap = []<internal::IsMonadic T>(T&& value) { return *std::forward<T>(value); };
} // namespace monadic_utils

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_MONADIC_UTILS_HPP
