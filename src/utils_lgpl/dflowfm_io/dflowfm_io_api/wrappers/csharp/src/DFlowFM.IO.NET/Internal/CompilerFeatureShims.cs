#if NETSTANDARD2_0

namespace System.Runtime.CompilerServices
{
    /// <summary>Shim enabling <c>init</c>-only setters on netstandard2.0.</summary>
    internal static class IsExternalInit;

    /// <summary>Shim enabling the <c>required</c> modifier on netstandard2.0.</summary>
    [AttributeUsage(
        AttributeTargets.Class | AttributeTargets.Struct |
        AttributeTargets.Field | AttributeTargets.Property, Inherited = false)]
    internal sealed class RequiredMemberAttribute : Attribute;

    /// <summary>Shim enabling compiler-feature checks (e.g. <c>required</c>) on netstandard2.0.</summary>
    [AttributeUsage(AttributeTargets.All, AllowMultiple = true, Inherited = false)]
    internal sealed class CompilerFeatureRequiredAttribute(string featureName) : Attribute
    {
        /// <summary>The name of the compiler feature.</summary>
        public string FeatureName { get; } = featureName;

        /// <summary>Whether the feature can be ignored by consumers that don't understand it.</summary>
        public bool IsOptional { get; init; }
    }
}

namespace System.Diagnostics.CodeAnalysis
{
    /// <summary>Shim enabling constructors to satisfy <c>required</c> members on netstandard2.0.</summary>
    [AttributeUsage(AttributeTargets.Constructor)]
    internal sealed class SetsRequiredMembersAttribute : Attribute;
}

#endif