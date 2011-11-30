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
  hfs_plus_fork_data :catalogFile
  hfs_plus_fork_data :attributesFile
  hfs_plus_fork_data :startupFile
end

class BTNodeDescriptor < BinData::Record
  endian :big
  uint32 :fLink
  uint32 :bLink
  int8   :kind
  uint8  :height
  uint16 :numRecords
  uint16 :reserved
end
  
class BTHeaderRec < BinData::Record
  endian :big
  uint16 :treeDepth
  uint32 :rootNode
  uint32 :leafRecords
  uint32 :firstLeafNode
  uint32 :lastLeafNode
  uint16 :nodeSize
  uint16 :maxKeyLength
  uint32 :totalNodes
  uint32 :freeNodes
  uint16 :reserved1
  uint32 :clumpSize
  uint8  :btreeType
  uint8  :keyCompareType
  uint32 :attributes

  array  :reserved3, :type => :uint32, :initial_length => 16
end

class HFSUniStr255 < BinData::Record
  endian :big
  uint16 :length_
  array  :unicode, :type => :uint8, :inital_length => lambda { length_ * 2 }
end

# HFSPlusAttrKey = Struct("HFSPlusAttrKey",
#     uint16  :keyLength
#     uint16  :pad
#     uint32  :fileID
#     uint32  :startBlock
#     HFSUniStr255,
#     #uint32 ("nodeNumber")
# )

# HFSPlusAttrData = Struct("HFSPlusAttrData",
#     uint32  :recordType
#     Array(2, uint32 ("reserved")),
#     uint32  :size
#     MetaField("data", lambda ctx: ctx["size"])
# )

# HFSPlusCatalogKey = Struct("HFSPlusCatalogKey",
#     uint16  :keyLength
#     uint32  :parentID
#     HFSUniStr255
# )

# HFSPlusBSDInfo = Struct("HFSPlusBSDInfo",
#     uint32  :ownerID
#     uint32  :groupID
#     uint8 ("adminFlags"),
#     uint8 ("ownerFlags"),
#     uint16 ("fileMode"),
#     uint32 ("union_special")
# )

# Point = Struct("Point",
#     int16 ("v"),
#     int16 ("h")
# )

# Rect = Struct("Rect",
#     int16 ("top"),
#     int16 ("left"),
#     int16 ("bottom"),
#     int16 ("right")
# )

# FileInfo = Struct("FileInfo",
#     uint32 ("fileType"),
#     uint32 ("fileCreator"),
#     uint16 ("finderFlags"),
#     Point,
#     uint16 ("reservedField")
# )
