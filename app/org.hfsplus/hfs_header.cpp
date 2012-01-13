#include "hfs.h"

using namespace utility::hex;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
bool HFSPlusVolumeHeader::read_from(ByteBuffer& b)
{
  signature          = b.get_uint2_be();
  version            = b.get_uint2_be();
  attributes         = b.get_uint4_be();
  lastMountedVersion = b.get_uint4_be();
  journalInfoBlock   = b.get_uint4_be();
  createDate         = b.get_uint4_be();
  modifyDate         = b.get_uint4_be();
  backupDate         = b.get_uint4_be();
  checkedDate        = b.get_uint4_be();

  fileCount          = b.get_uint4_be();
  folderCount        = b.get_uint4_be();

  blockSize          = b.get_uint4_be();
  totalBlocks        = b.get_uint4_be();
  freeBlocks         = b.get_uint4_be();

  nextAllocation     = b.get_uint4_be();
  rsrcClumpSize      = b.get_uint4_be();
  dataClumpSize      = b.get_uint4_be();
  nextCatalogID      = b.get_uint4_be();
  writeCount         = b.get_uint4_be();
  encodingsBitmap    = b.get_uint8_be();

  for (int i=0; i<8; i++)
    finderInfo[i] = b.get_uint4_le();

  allocationFile.read_from(b);
  extentsFile.read_from(b);
  catalogFile.read_from(b);
  attributesFile.read_from(b);
  startupFile.read_from(b);

  return true;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
