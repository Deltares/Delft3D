#if !defined ADK_UTIL_H_
#define ADK_UTIL_H_

#include "ThirdPartyHeadersBegin.h"
#  include <string>
#include "ThirdPartyHeadersEnd.h"

# if defined (__cplusplus)
extern "C"
{
# endif

    /**
     * Compares the Tecplot version with the input version.
     *
     * @param MajorTecplotVersion
     *    Major Tecplot version to compare
     * @param MinorTecplotVersion
     *    Minor Tecplot version to compare
     * @param MajorTecplotRevision
     *    Major Tecplot revision to compare
     * @param MinorTecplotRevision
     *    Minor Tecplot revision to compare
     *
     * @return
     *    -1 if input version is less that current Tecplot version
     *     0 if input version is equal to current Tecplot version
     *     1 if input version is greater than current Tecplot version
     */
    extern int TecVersionCmp(SmInteger_t MajorTecplotVersion,
    SmInteger_t MinorTecplotVersion,
    SmInteger_t MajorTecplotRevision,
    SmInteger_t MinorTecplotRevision);


    /**
     * Scans the input string for the first non-white space character and
     * determines if it matches the specified symbol character. Scanning will not
     * go beyond the end of the string.
     *
     * @param CPtr
     *     Pointer into a zero terminated string of characters where the scanning
     *     is to begin. If the function return value is TRUE then CPtr will be
     *     advanced to the character following the located symbol character. If the
     *     fuunction return value is FALSE then CPtr will point to the non-matching
     *     character.
     * @param Symbol
     *     Symbol with which to search the input string.
     *
     * @return
     *     TRUE if the symbol was found, FALSE otherwise.
     */
    extern Boolean_t Str_ScanForSymbol(char **CPtr,
                                       char   Symbol);

    /**
     * Lexicographically compares, at most, the first 'Len' characters of
     * s1 and s2.
     *
     * @param s1
     *     First string or NULL.
     * @param s2
     *     Second string or NULL.
     * @param Len
     *     Maximum number of characters to compare.
     * @return
     *     Integer value greater than, equal to, or less than zero according
     *     to whether the first 'Len' characters of 's1' are greater than,
     *     equal to, or less than 's2'.
     */
    extern int Str_ustrncmp(const char *s1,
                            const char *s2,
                            LgIndex_t   Len);

    /**
     * Lexicographically compares the characters of s1 and s2.
     *
     * param s1
     *     First string or NULL.
     * param s2
     *     Second string or NULL.
     * return
     *     Integer value greater than, equal to, or less than zero according to
     *     whether the characters of 's1' are greater than, equal to, or less
     *     than 's2'.
     */
    extern int Str_ustrcmp(const char *s1,
                           const char *s2);

    /**
     * Scans the input string, skipping all characters until it reaches a newline
     * character, '\n', or the end of the string.
     *
     * @param CPtr
     *     Pointer into a zero terminated string of characters where the scanning
     *     is to begin. Upon return CPtr will either point to the end of the string
     *     or to the character following the newline character.
     */
    extern void Str_RemoveThroughEndOfLine(char **CPtr);

    /**
     * Copy up to 'Count' characters from the 'Source' string beginning at position
     * 'Index' to the 'Target' string. The actual number of characters copied may
     * be less than 'Count' if a '\0' was encountered in the 'Source' string before
     * 'Count' characters were copied.
     *
     * @par Note:
     *     The 'Target' and 'Source' strings may overlap.
     *
     * @param Target
     *     Target string to receive the sub string. The target string must be sized
     *     such that it can receive at least the number of characters requested
     *     plus one for the zero string terminator.
     * @param Source
     *     Zero terminated source string from which to copy.
     * @param Index
     *     Index into the source string indentifying the start of the sub-string.
     * @param Count
     *     Maximum number of characters to copy from the source string. The actual
     *     number of characters may be less if then end of the source string is
     *     encountered first.
     */
    extern void Str_CopySubString(char       *Target,
                                  const char *Source,
                                  LgIndex_t   Index,
                                  LgIndex_t   Count);

    /**
     * Scans the input string, skipping all "plain" white space characters until
     * it reaches a character that is not "plain" white space or the end of the
     * string. Plain white space characters are defined as a space or a tab.
     * Newline characters are not considered "plain" white space.
     *
     * @param CPtr
     *     Pointer into a zero terminated string of characters where the scanning
     *     is to begin. Upon return CPtr will either point to the end of the string
     *     or to the first non-plain white space character.
     */
    extern void Str_RemoveWhiteSpace(char **CPtr);

    /**
     * Scans the input string, skipping all "plain" white space characters until
     * it reaches a token string and then copies up to MaxChars of it to the
     * 'Token' string. A token string is defined as a sequence of characters that
     * are not "plain" white space or is defined as a quoted string. If the token
     * string is not quoted then it is copied to the Token string and converted to
     * upper case. If the token string is quoted then it is copied as-is (meaning
     * it is not converted to upper case) but without the quotes.
     *
     * @param CPtr
     *     Pointer into a zero terminated string of characters where the scanning
     *     is to begin. Upon return CPtr will either point to the end of the string
     *     or to the first non-plain white space character.
     * @param MaxChars
     *     Maximum number for the token.
     * @param Token
     *     String to receive the token string. The target string must be sized
     *     such that it can receive at least 'MaxChars' characters plus one for the
     *     zero string terminator.
     */
    extern Boolean_t Str_GetToken(char **CPtr,
                                  int    MaxChars,
                                  char  *Token);
    /**
     * Deallocates 'OldString' if it is non-NULL and allocates a new one that is a
     * copy of 'NewString'.
     *
     * @param OldString
     *     Pointer to the old string that was allocated by the this library or the
     *     TecUtil library or a pointer to NULL.
     * @param NewString
     *     Source string to copy or NULL. If NULL no new string will be allocated.
     *
     * @return
     *     TRUE if the allocation was successful, otherwise FALSE.
     */
    extern Boolean_t Str_ReplaceString(char      **OldString,
                                       const char *NewString);

    /**
     * Appends 'StringToTack' to the end of the contents of the 'BaseString'. The
     * 'BaseString' is reallocated to accommodate the larger size.
     *
     * @param BaseString
     *     Pointer to the string to which 'StringToTack' will be appended or a
     *     pointer to NULL if this is the first string. Upon return the BaseString
     *     points to a new string that has 'StringToTack' appended or NULL if there
     *     was an error.
     * @param StringToTack
     *     String to append to the base string.
     */
    extern void Str_TackOnString(char      **BaseString,
                                 const char *StringToTack);

    /**
     * Builds a string that represents the Tecplot set.
     *
     * @param Set
     *     Tecplot set or NULL.
     * @param IncludeSquareBrackets
     *     Indicates if the string built should include square brackets.
     * @param SetString
     *     Pointer to NULL or pointer to a previously allocated string. If it
     *     points to a previously allocated string it is first deallocated. The
     *     resulting string is allocated.
     */
    extern void Str_BuildSetString(Set_pa    Set,
                                   Boolean_t IncludeSquareBrackets,
                                   char    **SetString);

    /**
     * Builds a Tecplot set from a string representation. A set string is a
     * sequence of set elements where each element can be a positve non-zero
     * integer, a range such as 5-10, or a range with a skip such as 5-10:2.
     * If specified the entire set can be surrounded by square brackets.
     *
     * @param IString
     *     String representation of a Tecplot set.
     * @param Set
     *     Pointer to resulting set or NULL if it was unable to create a set from
     *     the string.
     * @param HasOUterSquareBrackets
     *     Indicates if surrounding square brackets are expected.
     *
     * @return
     *     TRUE if a set could be made from the string representation, FALSE
     *     otherwise.
     */
    extern Boolean_t Str_GetSet(const char *IString,
                                Set_pa     *Set,
                                Boolean_t   HasOuterSquareBrackets);


    /**
     * Scans the input string, skipping all "plain" white space characters and then
     * parses for a value of type LgIndex_t.
     *
     * @param CPtr
     *     Pointer into a zero terminated string of characters where the scanning
     *     is to begin. Upon return CPtr will either point to the end of the string
     *     or the character where scanning stopped.
     * @param Value
     *     Pointer to resulting value.
     *
     * @return
     *     TRUE if a value of type LgIndex_t was parsed, FALSE otherwise.
     */
    extern Boolean_t Str_ScanForLgIndex(char      **CPtr,
                                        LgIndex_t  *Value);

    /**
     * Scans the input string, skipping all "plain" white space characters and then
     * parses for a value of type double.
     *
     * @param CPtr
     *     Pointer into a zero terminated string of characters where the scanning
     *     is to begin. Upon return CPtr will either point to the end of the string
     *     or the character where scanning stopped.
     * @param Value
     *     Pointer to resulting value.
     *
     * @return
     *     TRUE if a value of type double was parsed, FALSE otherwise.
     */
    extern Boolean_t Str_ScanForDouble(char   **CPtr,
                                       double  *Value);


    /*******************************************************
     *           Data Loader Utility Functions             *
     *******************************************************/

    /**
     * Convenience function to add both the command string and command value to the
     * string list.
     *
     * @param Commands
     *     String list to receive the command/value string pair.
     * @param Command
     *     Command string.
     * @param CommandValue
     *     String command value.
     *
     * @return
     *     TRUE if successful, FALSE otherwise.
     */
    extern Boolean_t DLoad_AddStringArg(StringList_pa  Commands,
                                        const char    *Command,
                                        const char    *CommandValue);

    /**
     * Convenience function to add both the command string and a string
     * representation of the boolean command value to the string list.
     *
     * @param Commands
     *     String list to receive the command string and value string
     *     representation.
     * @param Command
     *     Command string.
     * @param CommandValue
     *     Boolean command value.
     *
     * @return
     *     TRUE if successful, FALSE otherwise.
     */
    extern Boolean_t DLoad_AddBooleanArg(StringList_pa  Commands,
                                         const char    *Command,
                                         Boolean_t      CommandValue);

    /**
     * Convenience function to add both the command string and a string
     * representation of the integer command value to the string list.
     *
     * @param Commands
     *     String list to receive the command string and value string
     *     representation.
     * @param Command
     *     Command string.
     * @param CommandValue
     *     Integer command value.
     *
     * @return
     *     TRUE if successful, FALSE otherwise.
     */
    extern Boolean_t DLoad_AddLgIndexArg(StringList_pa  Commands,
                                         const char    *Command,
                                         LgIndex_t      CommandValue);

    /**
     * Convenience function to add both the command string and a string
     * representation of the set command value to the string list.
     *
     * @param Commands
     *     String list to receive the command string and value string
     *     representation.
     * @param Command
     *     Command string.
     * @param CommandValue
     *     Set command value.
     *
     * @return
     *     TRUE if successful, FALSE otherwise.
     */
    extern Boolean_t DLoad_AddSetArg(StringList_pa  Commands,
                                     const char    *Command,
                                     Set_pa         CommandValue);

    /**
     * Convenience function to fetch a boolean value from the instruction string
     * list at the specified offset.
     *
     * @param Instructions
     *     String list containing the add-on instructions.
     * @param Offset
     *     Offset in the string list to the desired boolean value.
     * @param ParamName
     *     Parameter name to fetch; this is only used for error messages.
     * @param BValue
     *     Pointer to the boolean result value.
     *
     * @return
     *     TRUE if successful, FALSE otherwise.
     */
    extern Boolean_t DLoad_GetBooleanFromInstruction(StringList_pa  Instructions,
                                                     int            Offset,
                                                     const char    *ParamName,
                                                     Boolean_t     *BValue);

    /**
     * Convenience function to fetch an integer value from the instruction string
     * list at the specified offset.
     *
     * @param Instructions
     *     String list containing the add-on instructions.
     * @param Offset
     *     Offset in the string list to the desired integer value.
     * @param ParamName
     *     Parameter name to fetch; this is only used for error messages.
     * @param BValue
     *     Pointer to the integer result value.
     *
     * @return
     *     TRUE if successful, FALSE otherwise.
     */
    extern Boolean_t DLoad_GetLgIndexFromInstruction(StringList_pa  Instructions,
                                                     int            Offset,
                                                     const char    *ParamName,
                                                     LgIndex_t     *IValue);

    /**
     * Convenience function to fetch a Tecplot set value from the instruction
     * string list at the specified offset.
     *
     * @param Instructions
     *     String list containing the add-on instructions.
     * @param Offset
     *     Offset in the string list to the desired Tecplot set value.
     * @param ParamName
     *     Parameter name to fetch; this is only used for error messages.
     * @param BValue
     *     Pointer to the Tecplot set result value.
     *
     * @return
     *     TRUE if successful, FALSE otherwise.
     */
    extern Boolean_t DLoad_GetSetFromInstruction(StringList_pa  Instructions,
                                                 int            offset,
                                                 const char    *ParamName,
                                                 Set_pa        *Set);

    /**
     * Scans the input string, skipping all "plain" white space characters and then
     * parses for a name value pair. The value is always returned as a string.
     *
     * @param CPtr
     *     Pointer into a zero terminated string of characters where the scanning
     *     is to begin. Upon return CPtr will either point to the end of the string
     *     or the character where scanning stopped.
     * @param CommandName
     *     String to receive the command name string. The string must be sized such
     *     that it can receive at least 'MaxChars' characters plus one for the zero
     *     string terminator.
     * @param ValueString
     *     String to receive the command value string. The string must be sized
     *     such that it can receive at least 'MaxChars' characters plus one for the
     *     zero string terminator.
     * @param MaxChars
     *     Maximum number for the command name and command value strings.
     * @param ErrMsgString
     *     Allocated error string if not successful otherwise it is not used.
     *
     * @return
     *     TRUE if successful, FALSE otherwise. If FALSE the ErrorMsgString
     *     contains an allocated string describing the problem.
     */
    extern Boolean_t GetArgPair(char **CPtr,
                                char  *CommandName,
                                char  *ValueString,
                                int    MaxChars,
                                char **ErrMsgString);

    /**
     * Convenience function to fetch an integer value from 'ArgString' and ensure
     * that it is within the specified domain.
     *
     * @param Command
     *     Macro command name; this is only used for error messages.
     * @param ArgString
     *     Macro argument string containing a string representation of the integer
     *     to fetch.
     * @param Min
     *     Domain value floor.
     * @param Max
     *     Domain value ceiling.
     * @param Value
     *     Pointer to resulting value.
     * @param ErrMsg
     *     Allocated error string if not successful otherwise it is not used.
     *
     * @return
     *     TRUE if successful, FALSE otherwise. If FALSE the ErrMsg contains an
     *     allocated string describing the problem.
     */
    extern Boolean_t Macro_GetLgIndexArg(const char *Command,
                                         const char *ArgString,
                                         LgIndex_t   Min,
                                         LgIndex_t   Max,
                                         LgIndex_t  *Value,
                                         char      **ErrMsg);

    /**
     * Convenience function to fetch a Tecplot set value from 'ArgString' and
     * ensure that it is within the specified domain.
     *
     * @param Command
     *     Macro command name; this is only used for error messages.
     * @param ArgString
     *     Macro argument string containing a string representation of the Tecplot
     *     set to fetch.
     * @param Value
     *     Pointer to resulting allocated set.
     * @param ErrMsg
     *     Allocated error string if not successful otherwise it is not used.
     *
     * @return
     *     TRUE if successful, FALSE otherwise. If FALSE the ErrMsg contains an
     *     allocated string describing the problem.
     */
    extern Boolean_t Macro_GetSetArg(const char *Command,
                                     const char *ArgString,
                                     Set_pa     *Value,
                                     char      **ErrMsg);

    /**
     * Convenience function to fetch a boolean value from 'ArgString' and ensure
     * that it is within the specified domain.
     *
     * @param Command
     *     Macro command name; this is only used for error messages.
     * @param ArgString
     *     Macro argument string containing a string representation of the boolean
     *     to fetch.
     * @param Value
     *     Pointer to resulting value.
     * @param ErrMsg
     *     Allocated error string if not successful otherwise it is not used.
     *
     * @return
     *     TRUE if successful, FALSE otherwise. If FALSE the ErrMsg contains an
     *     allocated string describing the problem.
     */
    extern Boolean_t Macro_GetBooleanArg(const char *Command,
                                         const char *ArgString,
                                         Boolean_t  *Value,
                                         char      **ErrMsg);

    /**
     * Convenience function to fetch a double value from 'ArgString' and ensure
     * that it is within the specified domain.
     *
     * @param Command
     *     Macro command name; this is only used for error messages.
     * @param ArgString
     *     Macro argument string containing a string representation of the double
     *     to fetch.
     * @param Min
     *     Domain value floor.
     * @param Max
     *     Domain value ceiling.
     * @param Value
     *     Pointer to resulting value.
     * @param ErrMsg
     *     Allocated error string if not successful otherwise it is not used.
     *
     * @return
     *     TRUE if successful, FALSE otherwise. If FALSE the ErrMsg contains an
     *     allocated string describing the problem.
     */
    extern Boolean_t Macro_GetDoubleArg(const char *Command,
                                        const char *ArgString,
                                        double      Min,
                                        double      Max,
                                        double     *Value,
                                        char      **ErrMsg);

    /**
     * Convenience function to fetch an enumeration value from 'ArgString' and
     * ensure that it is within the specified domain.
     *
     * @param Command
     *     Macro command name; this is only used for error messages.
     * @param ArgString
     *     Macro argument string containing a string representation of the
     *     enumeration to fetch.
     * @param EnumList
     *     Macro argument enumeration list; this is only used for error messages.
     * @param Value
     *     Pointer to resulting value.
     * @param ErrMsg
     *     Allocated error string if not successful otherwise it is not used.
     *
     * @return
     *     TRUE if successful, FALSE otherwise. If FALSE the ErrMsg contains an
     *     allocated string describing the problem.
     */
    extern Boolean_t Macro_GetEnumArg(const char *Command,
                                      const char *ArgString,
                                      char       *EnumList,
                                      int        *Value,
                                      char      **ErrMsg);


    /**
     * Indicates if any native variable in the current frame is cell centered.
     *
     * @return
     *     TRUE if any native variable in the current frame is cell centered, FALSE
     *     otherwise.
     */
    extern Boolean_t Data_AnyVariableIsCellCentered(void);

    /**
     * Indicates if any variable in the current frame is shared.
     *
     * @return
     *     TRUE if any variable in the current frame is shared, FALSE otherwise.
     */
    extern Boolean_t Data_AnyVariableIsShared(void);

# if defined (__cplusplus)
}
# endif

/**
 * @return the name of the indexed variable and all its aliases delimited by a semicolon (;)
 */
extern std::string VariableNameFormattedForAliases(EntIndex_t Index);

#endif /*ADK_UTIL_H_*/
