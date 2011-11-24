require "bindata"

class HFSPlusExtentDescriptor < BinData::Record
  endian :big
  uint32 :startBlock
  uint32 :blockCount
end

class HFSPlusExtentRecord < BinData::Array
  default_parameter :initial_length => 8
  hfs_plus_extent_descriptor
end

class HFSPlusForkData < BinData::Record
  endian :big
  uint64 :logicalSize
  uint32 :clumpSze
  uint32 :totalBlocks
  hfs_plus_extent_record :extentRecords
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

  hfs_plus_fork_data :allocationFile
  hfs_plus_fork_data :extentsFile
  hfs_plus_fork_data :attributesFile
  hfs_plus_fork_data :startupFile
end
