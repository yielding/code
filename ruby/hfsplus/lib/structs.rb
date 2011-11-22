require "bindata"

class HFSPlusExtentDescriptor < BinData::Record
  endian :big

  uint32 :startBlock
  uint32 :blockCount
end

class HFSPlusExtentRecord < BinData::Record
  array 
end

class HFSPlusForkData < BinData::Record
  endian :big

  uint64 :logicalSize
  uint32 :clumpSze
  uint32 :totalBlocks
  array  :HFSPlusExtentDescriptor, :type => :HFSPlusExtentDescriptor, :initial_length => 8
end

class HFSPlusVolumeHeader < BinData::Record
  endian :big

  uint16 :signature
  uint16 :version
  uint32 :attributes
  uint32 :lastMountedVersion
  uint32 :journalInfoBlock
  uint32 :createDate
  uint32 :modifyDate
  uint32 :backupDate
  uint32 :checkedData
  uint32 :fileCount
  uint32 :folderCount
  uint32 :blockSize
  uint32 :totalBlocks
  uint32 :freeBlocks
  uint32 :nextAllocation
  uint32 :rsrcClumpSize
  uint32 :dataClumpSize
  uint32 :nextCatalogID
  uint32 :writeCount
  uint64 :encodingsBitmap
  array  :finderInfo, :type => :uint32, :initial_length => 8
  allocationFile :HFSPlusForkData
  extentsFile    :HFSPlusForkData
  attributesFile :HFSPlusForkData
  startupFile    :HFSPlusForkData
end
