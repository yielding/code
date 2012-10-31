#include "VFLBase.h"
#include "NAND.h"

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
VFLBase::VFLBase(NAND const& nand)
    : _nand(nand)
{
    _ce_count          = nand.ce_count();
    _banks_per_ce      = nand.banks_per_ce();
    _blocks_per_ce     = nand.blocks_per_ce();
    _vendor_type       = nand.vendor_type();
    _pages_per_block   = nand.pages_per_block();
    _pages_per_block_2 = nand.pages_per_block2();
    _banks_total       = nand.banks_total();
    _page_size         = nand.page_size();

    _blocks_per_bank     = _blocks_per_ce / _banks_per_ce;
    _pages_per_sublk     = _pages_per_block * _banks_per_ce * _ce_count;
    _blocks_per_bank_vfl = _blocks_per_ce / _banks_per_ce;

    _current_version   = 0;
}

auto VFLBase::nand_page_size() const -> uint32_t
{
    return _page_size;
}

auto VFLBase::pages_per_sublk() const -> uint32_t
{
    return _pages_per_sublk;
}

auto VFLBase::nand() const -> NAND const&
{
    return _nand;
}

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
