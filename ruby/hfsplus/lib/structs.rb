require "bindata"

KHFSRootParentID            = 1
KHFSRootFolderID            = 2
KHFSExtentsFileID           = 3
KHFSCatalogFileID           = 4
KHFSBadBlockFileID          = 5
KHFSAllocationFileID        = 6
KHFSStartupFileID           = 7
KHFSAttributesFileID        = 8
KHFSRepairCatalogFileID     = 14
KHFSBogusExtentFileID       = 15
KHFSFirstUserCatalogNodeID  = 16

KBTLeafNode       = -1
KBTIndexNode      =  0
KBTHeaderNode     =  1
KBTMapNode        =  2

KHFSPlusFolderRecord        = 0x0001
KHFSPlusFileRecord          = 0x0002
KHFSPlusFolderThreadRecord  = 0x0003
KHFSPlusFileThreadRecord    = 0x0004

KHFSPlusAttrInlineData  = 0x10
KHFSPlusAttrForkData    = 0x20
KHFSPlusAttrExtents     = 0x30

KForkTypeData = 0
KForkTypeRsrc = 0xFF

KHFSVolumeHardwareLockBit       =  7
KHFSVolumeUnmountedBit          =  8
KHFSVolumeSparedBlocksBit       =  9
KHFSVolumeNoCacheRequiredBit    = 10
KHFSBootVolumeInconsistentBit   = 11
KHFSCatalogNodeIDsReusedBit     = 12
KHFSVolumeJournaledBit          = 13
KHFSVolumeSoftwareLockBit       = 15

DECMPFS_MAGIC = 0x636d7066  #cmpf

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
