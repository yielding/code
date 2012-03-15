#include "attribute_btree.h"

#include <boost/algorithm/string.hpp>
#include <iostream>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
AttributeTree::AttributeTree(HFSFile* file)
    : BTree<AttributeTree>(file)
{
}

AttributeTree::~AttributeTree()
{
}

int AttributeTree::compare_keys(HFSPlusAttrKey const& l, HFSPlusAttrKey const& r) const
{
    if (l.fileID < r.fileID)
        return -1;

    if (l.fileID > r.fileID)
        return 1;

    uint16_t i;
    for (i=0; i<l.name.length; i++)
    {
        if(i >= r.name.length) 
            return 1;

        uint16_t cl = l.name.unicode[i];
        uint16_t cr = r.name.unicode[i];

        if (cl < cr) return -1;
        if (cl > cr) return  1;
    }

    if (i < r.name.length) 
        return -1;

    return 0;
}

auto AttributeTree::get_all_attributes(HFSCatalogNodeID folderID) -> AttrNode
{
    assert(0);
    AttrNode node;
    return node;
}

auto AttributeTree::get_attribute(HFSCatalogNodeID cnid, string const& name) 
    -> ByteBuffer
{
    HFSPlusAttrKey key(cnid, 0,  name);
    ByteBuffer buffer;

    auto record = search(key);
    if (!record.empty())
    {
        for (size_t i=0; i<record.data.size; ++i)
            buffer.append(record.data.data[i]);
    }

    return buffer;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
