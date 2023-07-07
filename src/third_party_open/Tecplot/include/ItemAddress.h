#pragma once

#include "RawArray.h"


/**
 * Calculates the size of the identifier in bits.
 */
#define SIZE_IN_BITS(identifier) (sizeof(identifier)*8)

/**
 * Invalid subzone index value. Since we are using unsigned integers we sacrifice the last item.
 */
#define INVALID_ITEM_ADDRESS_SZINDEX \
    static_cast<tecplot::ItemAddress::SzIndex_t>((UInt64_t(2) << (tecplot::ItemAddress::SzIndexBitSize - UInt64_t(1))) - UInt64_t(1))

/**
 * Maximum value for a item address subzone index. The maximum value is one less than the invalid
 * index.
 */
#define MAX_ITEM_ADDRESS_SZINDEX \
    (INVALID_ITEM_ADDRESS_SZINDEX - static_cast<tecplot::ItemAddress::SzIndex_t>(1))

/**
 * Maximum value for a item address offset. Unlike the maximum subzone index which is one less than
 * the invalid value, we don't have that flexibility with offsets because we need all 8 bits to
 * represent the offsets so there is no invalid offset.
 */
#define MAX_ITEM_ADDRESS_OFFSET \
    static_cast<tecplot::ItemAddress::Offset_t>((UInt64_t(2) << (tecplot::ItemAddress::OffsetBitSize - UInt64_t(1))) - UInt64_t(1))

/**
 * Macro to verify that a item's subzone index has not exceeded the allowable number of bits
 * required to be represented by a tecplot::ItemAddress subzone index.
 */
#define VALID_ITEM_ADDRESS_SZINDEX(itemAddressSzIndex) \
    (itemAddressSzIndex != INVALID_ITEM_ADDRESS_SZINDEX && \
     (SIZE_IN_BITS(tecplot::ItemAddress::SzIndex_t) == tecplot::ItemAddress::SzIndexBitSize || \
      (SIZE_IN_BITS(tecplot::ItemAddress::SzIndex_t) > tecplot::ItemAddress::SzIndexBitSize && \
       UInt64_t(itemAddressSzIndex) >> tecplot::ItemAddress::SzIndexBitSize == UInt64_t(0))))

/**
 * Macro to verify that a subzone item offset has not exceeded the allowable number of bits
 * required to be represented by a tecplot::ItemAddress offset.
 */
#define VALID_ITEM_ADDRESS_OFFSET(itemAddressOffset) \
    (SIZE_IN_BITS(tecplot::ItemAddress::Offset_t) == tecplot::ItemAddress::OffsetBitSize || \
     (SIZE_IN_BITS(tecplot::ItemAddress::Offset_t) > tecplot::ItemAddress::OffsetBitSize && \
      UInt64_t(itemAddressOffset) >> tecplot::ItemAddress::OffsetBitSize == UInt64_t(0)))

/**
 * Macro to verify that an item address is valid.
 */
#define VALID_UNIFORM_ITEM_ADDRESS(itemAddress) \
    (itemAddress.addressType() == tecplot::ItemAddress::UniformType)
#define VALID_SINGLE_ITEM_ADDRESS(itemAddress) \
    (itemAddress.addressType() == tecplot::ItemAddress::SingleItemType && \
     VALID_ITEM_ADDRESS_SZINDEX((itemAddress).szIndex()) && \
     VALID_ITEM_ADDRESS_OFFSET((itemAddress).offset()))
#define VALID_IJK_ITEM_ADDRESS(itemAddress) \
    (itemAddress.addressType() == tecplot::ItemAddress::IJKItemType && \
     VALID_ITEM_ADDRESS_SZINDEX((itemAddress).szIndex()) && \
     VALID_ITEM_ADDRESS_OFFSET((itemAddress).ijkOffset().i) && \
     VALID_ITEM_ADDRESS_OFFSET((itemAddress).ijkOffset().j) && \
     VALID_ITEM_ADDRESS_OFFSET((itemAddress).ijkOffset().k))
#define VALID_ITEM_ADDRESS(itemAddress) \
    (VALID_UNIFORM_ITEM_ADDRESS(itemAddress) || \
     VALID_SINGLE_ITEM_ADDRESS(itemAddress) || \
     VALID_IJK_ITEM_ADDRESS(itemAddress))

namespace tecplot {

/**
 * An item address that support subzone/offset, subzone/[ijk]Offset and uniform addressing. The
 * class represents the address of an item (cell, node or face) within a subzone using a subzone
 * index and an offset, i, j, and K offset, or uniform addressing of a classic block of data. The
 * copy performance of the class is nearly identical to a word of the same size.
 *
 * @note
 *     Do no create large arrays of this data structure without careful consideration as the number
 *     of bits required to index a subzone item is only 40 bits (32 for the index and 8 for the
 *     offset) thereby wasting 24 bits for each item in a 64 bit word aligned array. Instead store
 *     the items and offsets in two separate arrays reducing the amount of unused bits.
 */
class ItemAddress
{
public:
    /**
     * Type of an index of the subzone containing the item (cell, node, or face). All index values
     * must fit within the number of bits specified by SzIndexBitSize.
     */
    typedef UInt32_t SzIndex_t;

    /**
     * Type of an offset within the subzone containing the item (cell, node, or face). All offset
     * values must fit within the number of bits specified by OffsetBitSize.
     */
    typedef UInt16_t Offset_t;

    /**
     * Type of an IJK offset within the subzone containing the item (cell, or node). Each component
     * must fit within the number of bits specified by OffsetBitSize.
     */
    struct IJK
    {
        IJK(Offset_t i, Offset_t j, Offset_t k)
            : i(i)
            , j(j)
            , k(k)
        {
            REQUIRE(VALID_ITEM_ADDRESS_OFFSET(i));
            REQUIRE(VALID_ITEM_ADDRESS_OFFSET(j));
            REQUIRE(VALID_ITEM_ADDRESS_OFFSET(k));
        }

        Offset_t i;
        Offset_t j;
        Offset_t k;
    };

    /**
     * The type used to hold a uniform address of an item. This allows non-SZL addressing to be
     * held by an ItemAddress for that existing algorithms in Tecplot work with both forms of data.
     * The uniform offset address is passed with a Int64_t because it needs to hold uniform node
     * value addresses which are NumElements*PtsPerElement in size which is 2B*8.
     */
    typedef Int64_t UniformOffset_t;

    /**
     * Number of bits needed to represent the index of the subzone.
     */
    static UInt32_t const SzIndexBitSize = 32U;

    /**
     * Number of bits needed to represent the offset or i, j, or k offset within the subzone.
     */
    static UInt32_t const OffsetBitSize = 8U;

    /**
     * Number of bits needed to represent the integer identifying the addressing scheme used by
     * ItemAddress.
     */
    static UInt32_t const AddressTypeBitSize = 2U;

    /**
     * Number of bits needed to represent a uniform offset that doesn't use subzone/offset
     * (aka non-uniform) addressing. The uniform offset address is held with a 40 bits because it
     * needs to hold uniform node value addresses which are NumElements*PtsPerElement in size which
     * is 2B*8.
     */
    static UInt32_t const UniformOffsetBitSize = 40U;

    static UInt32_t const UniformType    = 0U;
    static UInt32_t const IJKItemType    = 1U;
    static UInt32_t const SingleItemType = 2U;

    /**
     * Fills a raw array of subzone item addresses from a subzone index and a consecutive sequence
     * of offsets within that subzone between 0 and numOffsets-1.
     * @param szIndex
     *     The index of the subzone containing the item (cell, node, or face). All index values
     *     must fit within the number of bits specified by SzIndexBitSize.
     * @param numOffsets
     *     The number number of offsets to create.
     * @param itemAddresses
     *     Raw array filled with the resulting item addresses for the subzone, one for each
     *     consecutive offset between 0 and numOffsets-1.
     * @pre VALID_ITEM_ADDRESS_SZINDEX(szIndex)
     * @pre numOffsets == 0 || VALID_ITEM_ADDRESS_OFFSET(numOffsets-1)
     */
    static void fromSubzoneOffsets(
        ItemAddress::SzIndex_t          szIndex,
        ItemAddress::Offset_t           numOffsets,
        tecplot::RawArray<ItemAddress>& itemAddresses)
    {
        INVARIANT(validInvariants());
        REQUIRE(numOffsets == 0 || VALID_ITEM_ADDRESS_OFFSET(numOffsets-1));

        itemAddresses.reserve(numOffsets);
        itemAddresses.setSize(numOffsets);
        if (numOffsets != 0)
        {
            /* initialize each member as fast as possible using direct access */
            ItemAddress* itemAddressesArray = &itemAddresses[0];
            for (ItemAddress::Offset_t offset = 0; offset < numOffsets; ++offset)
            {
                itemAddressesArray[offset].m.addressType                 = SingleItemType;
                itemAddressesArray[offset].m.szSingleItemAddress.szIndex = szIndex;
                itemAddressesArray[offset].m.szSingleItemAddress.offset  = offset;
            }
        }
    }

    /**
     * Fills a raw array of item addresses fro an array of uniform offsets.
     */
    template <typename T>
    static void fromUniformOffsets(
        T*                              uniformOffsets,
        size_t                          numUniformOffsets,
        tecplot::RawArray<ItemAddress>& itemAddresses)
    {
        INVARIANT(validInvariants());
        REQUIRE(VALID_REF(uniformOffsets));

        /* initialize each member as fast as possible using direct access */
        itemAddresses.reserve(numUniformOffsets);
        itemAddresses.setSize(numUniformOffsets);
        if (numUniformOffsets != 0)
        {
            ItemAddress* itemAddressesArray = &itemAddresses[0];
            for (size_t uOffset = 0; uOffset < numUniformOffsets; ++uOffset)
            {
                itemAddressesArray[uOffset].m.addressType           = UniformType;
                itemAddressesArray[uOffset].m.uniformAddress.offset =
                    static_cast<UniformOffset_t>(uniformOffsets[uOffset]);
            }
        }
    }

    /**
     * Constructs an item address with an invalid value.
     */
    ItemAddress()
    {
        INVARIANT(validInvariants());

        m.rawBits = 0; // IMPORTANT: See note in constructor above.

        m.addressType                 = SingleItemType;
        m.szSingleItemAddress.szIndex = INVALID_ITEM_ADDRESS_SZINDEX;
        m.szSingleItemAddress.offset  = 0;
    }

    /**
     * Constructs a subzone item address from an subzone index and offset within that subzone.
     * @param szIndex
     *     The index of the subzone containing the item (cell, node, or face). All index values
     *     must fit within the number of bits specified by SzIndexBitSize.
     * @param offset
     *     The offset within the subzone containing the item (cell, node, or face). All offset
     *     values must fit within the number of bits specified by OffsetBitSize.
     * @pre VALID_ITEM_ADDRESS_SZINDEX(szIndex)
     * @pre VALID_ITEM_ADDRESS_OFFSET(offset)
     */
    ItemAddress(
        SzIndex_t szIndex,
        Offset_t  offset)
    {
        INVARIANT(validInvariants());
        REQUIRE(VALID_ITEM_ADDRESS_SZINDEX(szIndex));
        REQUIRE(VALID_ITEM_ADDRESS_OFFSET(offset));

        m.rawBits = 0; // IMPORTANT: See note in constructor above.

        m.addressType                 = SingleItemType;
        m.szSingleItemAddress.szIndex = szIndex;
        m.szSingleItemAddress.offset  = offset;
    }

    /**
     * Constructs a subzone item address from an subzone index and a local IJK offset within that
     * subzone.
     * @param szIndex
     *     The index of the subzone containing the item (cell, node, or face). All index values
     *     must fit within the number of bits specified by SzIndexBitSize.
     * @param offset
     *     The local IJK offset within the subzone containing the item (cell, node, or face). All
     *     offset values must fit within the number of bits specified by OffsetBitSize.
     * @pre VALID_ITEM_ADDRESS_SZINDEX(szIndex)
     * @pre VALID_ITEM_ADDRESS_OFFSET(offset)
     */
    ItemAddress(
        SzIndex_t               szIndex,
        ItemAddress::IJK const& offset)
    {
        INVARIANT(validInvariants());
        REQUIRE(VALID_ITEM_ADDRESS_SZINDEX(szIndex));
        REQUIRE(VALID_ITEM_ADDRESS_OFFSET(offset.i));
        REQUIRE(VALID_ITEM_ADDRESS_OFFSET(offset.j));
        REQUIRE(VALID_ITEM_ADDRESS_OFFSET(offset.k));

        m.rawBits = 0; // IMPORTANT: See note in constructor above.

        m.addressType              = IJKItemType;
        m.szIJKItemAddress.szIndex = szIndex;
        m.szIJKItemAddress.iOffset = offset.i;
        m.szIJKItemAddress.jOffset = offset.j;
        m.szIJKItemAddress.kOffset = offset.k;
    }

    /**
     * Constructs a subzone item address from a uniform offset.
     */
    explicit ItemAddress(UniformOffset_t uniformOffset)
    {
        INVARIANT(validInvariants());

        /*
         * IMPORTANT:
         *     First clear the entire structure prior to assigning the members. Since we want fast
         *     bitwise comparisons, see operator==, using the 64 bit m.rawBits member variable we
         *     need to make sure the entire word is cleared because assignment to the smaller bit
         *     fields will not clear it out for us leaving garbage thereby causing our bitwise
         *     equality test to fail when it should not have.
         */
        m.rawBits = 0;

        m.addressType           = UniformType;
        m.uniformAddress.offset = uniformOffset;
    }

    /**
     * @return
     *     true if the item address is using uniform addressing, false otherwise
     */
    bool isUniform() const
    {
        return (m.addressType == UniformType);
    }

    /**
     * @return
     *     true if the item address is using IJK-item addressing, false otherwise
     */
    bool isIJK() const
    {
        return (m.addressType == IJKItemType);
    }

    /**
     * @return
     *     true if the item address is using single-item addressing, false otherwise
     */
    bool isSingleItem() const
    {
        return (m.addressType == SingleItemType);
    }

    /**
     * @return
     *     The type of addressing of the item address
     */
    UInt32_t addressType() const
    {
        return (m.addressType);
    }

    /**
     * @return
     *     index of the subzone containing the item (cell, node, or face)
     */
    SzIndex_t szIndex() const
    {
        REQUIRE(!isUniform());
        SzIndex_t result;
        if (m.addressType == SingleItemType)
            result = m.szSingleItemAddress.szIndex;
        else
            result = m.szIJKItemAddress.szIndex;
        ENSURE(VALID_ITEM_ADDRESS_SZINDEX(result));
        return result;
    }

    /**
     * @return
     *     offset within the subzone containing the item (cell, node, or face)
     */
    Offset_t offset() const
    {
        REQUIRE(m.addressType == SingleItemType);
        ENSURE(VALID_ITEM_ADDRESS_OFFSET(m.szSingleItemAddress.offset));
        return m.szSingleItemAddress.offset;
    }

    /**
     * @return
     *     ordered IJK offset within the subzone containing the item (cell, node, or face)
     */
    ItemAddress::IJK ijkOffset() const
    {
        REQUIRE(m.addressType == IJKItemType);
        ENSURE(VALID_ITEM_ADDRESS_OFFSET(m.szIJKItemAddress.iOffset));
        ENSURE(VALID_ITEM_ADDRESS_OFFSET(m.szIJKItemAddress.jOffset));
        ENSURE(VALID_ITEM_ADDRESS_OFFSET(m.szIJKItemAddress.kOffset));
        return ItemAddress::IJK(
                   m.szIJKItemAddress.iOffset,
                   m.szIJKItemAddress.jOffset,
                   m.szIJKItemAddress.kOffset);
    }

    /**
     * @return
     *     uniform offset of the item (cell, node, or face)
     */
    UniformOffset_t uniformOffset() const
    {
        REQUIRE(isUniform());
        return m.uniformAddress.offset;
    }

    /**
     * See binary comparison operations for other operations.
     * @return
     *     true if the two items have bitwise equality
     */
    bool operator==(ItemAddress const& other) const
    {
        return m.rawBits == other.m.rawBits;
    }

private:
    #if !defined NO_ASSERTS
    static bool validInvariants()
    {
        return (SIZE_IN_BITS(ItemAddress) <= 64U && // ...otherwise consider passing ItemAddress' by reference instead of by value
                AddressTypeBitSize + UniformOffsetBitSize <= SIZE_IN_BITS(ItemAddress) &&
                AddressTypeBitSize + SzIndexBitSize + 3 * OffsetBitSize <= SIZE_IN_BITS(ItemAddress));
    }
    #endif

    /**
     * IMPORTANT:
     *     Courtesy of Visual Studio, all bit field members within a structure must be of the same
     *     type to instruct the compiler to pack into one 64 bit space. Because of this limitation,
     *     and because we must preserve the sign bit required by uniform addresses in Tecplot (e.g.
     *     we use -1 and -2 as flags in cell and node numbering), we have to make them all members
     *     of the UniformAddress_s structure signed 64 bit integers. The same is true for the
     *     SzSingleItemAddress_s structure except all the members must be 64 bit unsigned values.
     *     Basically, a structure containing bit fields must either all be signed or unsigned to
     *     work. If this were not the case we would have to abandon bit fields altogether and
     *     perform the bit twiddling ourselves. After all the grief of supposedly portable bit
     *     fields I'm tempted to abandon them anyway.
     */

    struct SzSingleItemAddress_s
    {
        // IMPORTANT: see note above for explanation of type choices for bit fields.
        UInt64_t addressType:AddressTypeBitSize;
        UInt64_t offset:OffsetBitSize;
        UInt64_t szIndex:SzIndexBitSize;
    };

    struct SzIJKItemAddress_s
    {
        // IMPORTANT: see note above for explanation of type choices for bit fields.
        UInt64_t addressType:AddressTypeBitSize;
        UInt64_t iOffset:OffsetBitSize;
        UInt64_t jOffset:OffsetBitSize;
        UInt64_t kOffset:OffsetBitSize;
        UInt64_t szIndex:SzIndexBitSize;
    };

    struct UniformAddress_s
    {
        // IMPORTANT: see note above for explanation of type choices for bit fields.
        Int64_t addressType:AddressTypeBitSize;
        Int64_t offset:UniformOffsetBitSize;
    };

    union
    {
        // IMPORTANT: see note above for explanation of type choices for bit fields.
        UInt64_t              addressType:AddressTypeBitSize;
        SzSingleItemAddress_s szSingleItemAddress;
        SzIJKItemAddress_s    szIJKItemAddress;
        UniformAddress_s      uniformAddress;
        UInt64_t              rawBits; // ...used for bitwise equality
    } m;
};

/*
 * Binary infix comparison operators for ItemAddress. ItemAddress supplies a bitwise unary
 * operator== and a binary operator<. All other comparison operators are written in terms of these
 * two operators.
 */

 inline bool operator<(
     ItemAddress::IJK const& lhs,
     ItemAddress::IJK const& rhs)
 {
    if (lhs.k != rhs.k)
        return lhs.k < rhs.k;
    else if (lhs.j != rhs.j)
        return lhs.j < rhs.j;
    else
        return lhs.i < rhs.i;
 }

inline bool operator<(
    ItemAddress const& lhs,
    ItemAddress const& rhs)
{
    UInt32_t const lhsType = lhs.addressType();
    UInt32_t const rhsType = rhs.addressType();
    if (lhsType != rhsType)
    {
        return lhsType < rhsType;
    }
    else if (lhsType == tecplot::ItemAddress::UniformType)
    {
        return lhs.uniformOffset() < rhs.uniformOffset();
    }
    else
    {
        if (lhs.szIndex() == rhs.szIndex())
        {
            if (lhsType == tecplot::ItemAddress::SingleItemType)
                return lhs.offset() < rhs.offset();
            else
                return lhs.ijkOffset() < rhs.ijkOffset();
        }
        else
        {
            return lhs.szIndex() < rhs.szIndex();
        }
    }
}

inline bool operator!=(
    ItemAddress const& lhs,
    ItemAddress const& rhs)
{
    return !(lhs == rhs);
}

inline bool operator<=(
    ItemAddress const& lhs,
    ItemAddress const& rhs)
{
    return lhs < rhs || lhs == rhs;
}

inline bool operator>(
    ItemAddress const& lhs,
    ItemAddress const& rhs)
{
    return !(lhs < rhs) && !(lhs == rhs);
}

inline bool operator>=(
    ItemAddress const& lhs,
    ItemAddress const& rhs)
{
    return !(lhs < rhs);
}

}
