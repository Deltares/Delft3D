#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_MONADIC_UTILS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_MONADIC_UTILS_HPP

#include <expected>
#include <optional>

namespace monadic_utils::internal
{
    /// @brief Type trait that is true when @p T is a std::optional specialisation.
    template <typename T>
    inline constexpr bool is_optional_v = false;
    template <typename T>
    inline constexpr bool is_optional_v<std::optional<T>> = true;

    /// @brief Type trait that is true when @p T is a std::expected specialisation.
    template <typename T>
    inline constexpr bool is_expected_v = false;
    template <typename T, typename E>
    inline constexpr bool is_expected_v<std::expected<T, E>> = true;

    /// @brief Satisfied when @p T is a std::optional specialisation.
    template <typename T>
    concept IsOptional = is_optional_v<std::remove_cvref_t<T>>;

    /// @brief Satisfied when @p T is a std::expected specialisation.
    template <typename T>
    concept IsExpected = is_expected_v<std::remove_cvref_t<T>>;

    /// @brief Satisfied when @p T is either std::optional or std::expected.
    template <typename T>
    concept IsMonadic = IsOptional<T> || IsExpected<T>;

    /**
     * @brief Extracts an early-return value from an invalid monadic result.
     *
     * For std::expected, returns std::unexpected wrapping the error.
     * For std::optional, returns std::nullopt.
     * Used internally by the ASSIGN_OR_RETURN and RETURN_IF_ERROR macros.
     */
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

/**
 * @def ASSIGN_OR_RETURN(lhs, expr)
 * @brief Evaluates @p expr (which must yield a monadic type) and assigns the contained value to @p lhs.
 *        If the expression is invalid, returns the error from the enclosing function.
 * @param lhs   The variable declaration or name to receive the unwrapped value.
 * @param expr  An expression returning std::optional or std::expected.
 *
 * C++23 provides monadic operations on std::optional (and_then, transform, or_else) and
 * std::expected (and_then, transform, transform_error, or_else). However, chaining these
 * combinators becomes unwieldy when a function performs several fallible steps that each
 * need the result of the previous one: the logic ends up deeply nested inside lambdas,
 * variables cannot be reused across steps, and the control flow is harder to follow than
 * a simple sequence of assignments.
 *
 * These macros provide an alternative that keeps the code flat and imperative: each call
 * either yields a usable value or exits the function early with the propagated error,
 * similar to Rust's `?` operator or the `BOOST_OUTCOME_TRY` macro from Boost.Outcome.
 * Google uses similar ASSIGN_OR_RETURN and RETURN_IF_ERROR macros in Protobuf, for example.
 */
#define ASSIGN_OR_RETURN(lhs, expr)                                                                        \
    auto&& MONADIC_UTILS_CONCAT(_res_, __LINE__) = (expr);                                                 \
    if (!MONADIC_UTILS_CONCAT(_res_, __LINE__))                                                            \
        return monadic_utils::internal::makeErrorReturn(std::move(MONADIC_UTILS_CONCAT(_res_, __LINE__))); \
    lhs = *std::move(MONADIC_UTILS_CONCAT(_res_, __LINE__))

/**
 * @def RETURN_IF_ERROR(expr)
 * @brief Evaluates @p expr (which must yield a monadic type) and returns the error from the
 *        enclosing function if the expression is invalid. The contained value, if any, is discarded.
 * @param expr  An expression returning std::optional or std::expected.
 */
#define RETURN_IF_ERROR(expr)                                                                                  \
    do                                                                                                         \
    {                                                                                                          \
        auto&& MONADIC_UTILS_CONCAT(_res_, __LINE__) = (expr);                                                 \
        if (!MONADIC_UTILS_CONCAT(_res_, __LINE__))                                                            \
            return monadic_utils::internal::makeErrorReturn(std::move(MONADIC_UTILS_CONCAT(_res_, __LINE__))); \
    } while (false)

namespace monadic_utils
{
    /// @brief Predicate that returns true when a monadic value contains a value.
    inline constexpr auto is_valid = []<internal::IsMonadic T>(const T& value) { return static_cast<bool>(value); };

    /// @brief Predicate that returns true when a monadic value is in the error/empty state.
    inline constexpr auto is_invalid = []<internal::IsMonadic T>(const T& value) { return !value; };

    /// @brief Extracts the contained value from a monadic type via operator*.
    inline constexpr auto unwrap = []<internal::IsMonadic T>(T&& value) { return *std::forward<T>(value); };
} // namespace monadic_utils

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_MONADIC_UTILS_HPP
