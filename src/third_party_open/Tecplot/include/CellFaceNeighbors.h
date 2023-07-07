#pragma once

#include "ClassMacros.h"
#include "ItemAddress.h"
#include "RawArray.h"
#include "tpsdkbase_Exports.h"

namespace tecplot {

/**
 * Maintains the neighbor information about a cell face. The class doesn't have connectivity
 * knowledge of the cell face it represents and therefore must be populated with the cell and zone
 * face neighbors. It is strictly used as a convenience for transporting results during queries.
 */
class tpsdkbase_API CellFaceNeighbors
{
public:
    /**
     * Uniform offset indicating that there isn't a cell face neighbor.
     */
    static LgIndex_t const NoCellFaceNeighbor;

    /**
     * Uniform offset indicating that there may or may not be a cell face neighbor.
     */
    static LgIndex_t const UnknownFaceNeighbor;

    /**
     * Uniform offset indicating that a neighbor isn't in another zone.
     */
    static EntIndex_t const NoZoneFaceNeighbor;

    /**
     * Constructs an empty cell face neighbor that will expand dynamically to hold cell and zone
     * neighbors.
     */
    CellFaceNeighbors();

    /**
     */
    ~CellFaceNeighbors();

    /**
     * Resets the class and reserves space for it to hold the specified number of neighbors.
     * @param cellAddress
     *     Cell address context
     * @param cellFace
     *     Cell face contex
     * @param numNeighbors
     *     Number of cell face neighbors that this class will hold for this cell face.
     * @param isFaceCompletelyObscured
     *     Indicates if the face is entirely obscured by its neighbors.
     * @throws std::bad_alloc if sufficient memory is not available
     */
    void reset(
        tecplot::ItemAddress cellAddress,
        LgIndex_t            cellFace,
        LgIndex_t            numNeighbors,
        bool                 isFaceCompletelyObscured);

    /**
     * Returns the cell address context.
     */
    tecplot::ItemAddress cellAddress();

    /**
     * Returns the cell face context.
     */
    LgIndex_t cellFace();

    /**
     * Adds a single cell/zone neighbor to the neighbors maintained by this class.
     * @param offset
     *     Identifies which neighbor.
     * @param cellNeighbor
     *     Cell address of a neighbor cell (whether in this zone or another), NoCellFaceNeighbor or
     *     UnknownFaceNeighbor.
     * @param zoneNeighbor
     *     Zone number of the neighboring cell or NoZoneFaceNeighbor if the cell is within the same
     *     zone as the cell represented by this class.
     */
    void addNeighbor(
        LgIndex_t            offset,
        tecplot::ItemAddress cellNeighbor,
        EntIndex_t           zoneNeighbor);

    /**
     * Indicates if the cell face is completely obscured by its neighbors.
     * @param isFaceCompletelyObscured
     *     TRUE if the face is completely obscured, FALSE otherwise.
     */
    void setIsFaceCompletelyObscured(bool isFaceCompletelyObscured);

    /**
     * Returns the number of neighbors currently assigned to this cell.
     */
    LgIndex_t numNeighbors() const;

    /**
     * Returns a reference to the cell neighbors array.
     */
    tecplot::RawArray<ItemAddress> const& cellNeighbors() const;

    /**
     * Returns the cell neighbor at the offset.
     */
    ItemAddress cellNeighbor(LgIndex_t nOffset) const;

    /**
     * Returns a reference to the zone neighbors array.
     */
    tecplot::RawArray<EntIndex_t> const& zoneNeighbors() const;

    /**
     * Returns the zone neighbor at the offset.
     */
    EntIndex_t zoneNeighbor(LgIndex_t nOffset) const;

    /**
     * Indicates if the cell face is completely obscured by its neighbors.
     */
    bool isFaceCompletelyObscured() const;

private:
    UNCOPYABLE_CLASS(CellFaceNeighbors);

    tecplot::ItemAddress                    m_cellAddress;
    LgIndex_t                               m_cellFace;
    tecplot::ItemAddress*                   m_cellNeighborsArray;
    tecplot::RawArray<tecplot::ItemAddress> m_cellNeighbors;
    EntIndex_t*                             m_zoneNeighborsArray;
    tecplot::RawArray<EntIndex_t>           m_zoneNeighbors;
    bool                                    m_isFaceCompletelyObscured;
};

}
