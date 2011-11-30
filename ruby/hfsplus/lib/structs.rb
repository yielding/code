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

KBTLeafNode                 = -1
KBTIndexNode                =  0
KBTHeaderNode               =  1
KBTMapNode                  =  2

KHFSPlusFolderRecord        = 0x0001
KHFSPlusFileRecord          = 0x0002
KHFSPlusFolderThreadRecord  = 0x0003
KHFSPlusFileThreadRecord    = 0x0004

KHFSPlusAttrInlineData      = 0x10
KHFSPlusAttrForkData        = 0x20
KHFSPlusAttrExtents         = 0x30

KForkTypeData               = 0
KForkTypeRsrc               = 0xFF

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

class HFSPlusAttrKey < BinData::Record
end

# HFSPlusAttrKey = Struct("HFSPlusAttrKey",
#     uint16  :keyLength
#     uint16  :pad
#     uint32  :fileID
#     uint32  :startBlock
#     HFSUniStr255,
#     #uint32 ("nodeNumber")
# )

class HFSPlusAttrData < BinData::Record
end
# HFSPlusAttrData = Struct("HFSPlusAttrData",
#     uint32  :recordType
#     Array(2, uint32 ("reserved")),
#     uint32  :size
#     MetaField("data", lambda ctx: ctx["size"])
# )

class HFSPlusCatalogKey < BinData::Record
  endian :big
  uint16 :keyLength
  uint32 :parentID
  hfs_uni_str255 :nodeName
end

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

class ExtendedFileInfo < BinData::Record
end

# ExtendedFileInfo = Struct("ExtendedFileInfo",
#     Array(4, SBInt16("reserved1")),
#     UBInt16("extendedFinderFlags"),
#     SBInt16("reserved2"),
#     SBInt32("putAwayFolderID")
# )

class FolderInfo < BinData::Record
end

# FolderInfo = Struct("FolderInfo",
#     Rect,
#     UBInt16("finderFlags"),
#     Point,
#     UBInt16("reservedField")
# )

class ExtendedFileInfo < BinData::Record
end

# ExtendedFolderInfo = Struct("ExtendedFolderInfo", 
#     Point,
#     SBInt32("reserved1"),
#     UBInt16("extendedFinderFlags"),
#     SBInt16("reserved2"),
#     SBInt32("putAwayFolderID")
# )

class HFSPlusCatalogFolder < BinData::Record
end

# HFSPlusCatalogFolder = Struct("HFSPlusCatalogFolder",
#     UBInt16("flags"),
#     UBInt32("valence"),
#     UBInt32("folderID"),
#     UBInt32("createDate"),
#     UBInt32("contentModDate"),
#     UBInt32("attributeModDate"),
#     UBInt32("accessDate"),
#     UBInt32("backupDate"),
#     HFSPlusBSDInfo,
#     FolderInfo,
#     ExtendedFolderInfo,
#     UBInt32("textEncoding"),
#     UBInt32("reserved")
# )

class HFSPlusCatalogFile < BinData::Record
end

# HFSPlusCatalogFile = Struct("HFSPlusCatalogFile",
#     UBInt16("flags"),
#     UBInt32("reserved1"),
#     UBInt32("fileID"),
#     UBInt32("createDate"),
#     UBInt32("contentModDate"),
#     UBInt32("attributeModDate"),
#     UBInt32("accessDate"),
#     UBInt32("backupDate"),
#     HFSPlusBSDInfo,
#     FileInfo,
#     ExtendedFileInfo,
#     UBInt32("textEncoding"),
#     UBInt32("reserved2"),
#     Struct("dataFork", Embed(HFSPlusForkData)),
#     Struct("resourceFork", Embed(HFSPlusForkData))
# )

class HFSPlusCatalogThread< BinData::Record
end

# HFSPlusCatalogThread = Struct("HFSPlusCatalogThread",
#     SBInt16("reserved"),
#     UBInt32("parentID"),
#     HFSUniStr255,
# )


class HFSPlusCatalogData < BinData::Record
end

# HFSPlusCatalogData = Struct("HFSPlusCatalogData",
#     UBInt16("recordType"),
#     Switch("data", lambda ctx: ctx["recordType"],
#     {
#         kHFSPlusFolderRecord : HFSPlusCatalogFolder,
#         kHFSPlusFileRecord : HFSPlusCatalogFile,
#         kHFSPlusFolderThreadRecord: HFSPlusCatalogThread,
#         kHFSPlusFileThreadRecord: HFSPlusCatalogThread
#     },
#     #default=HFSPlusCatalogFolder #XXX: should not reach
#     )
# )

class HFSPlusExtentKey < BinData::Record
end

# HFSPlusExtentKey = Struct("HFSPlusExtentKey",
#     UBInt16("keyLength"),
#     UBInt8("forkType"),
#     UBInt8("pad"),
#     UBInt32("fileID"),
#     UBInt32("startBlock")
# )

class HFSPlusDecmpfs < BinData::Record
end

# HFSPlusDecmpfs = Struct("HFSPlusDecmpfs ",
#    ULInt32("compression_magic"),
#    ULInt32("compression_type"),
#    ULInt64("uncompressed_size"),
# )

class HFSPlusCmpfRsrcHead < BinData::Record
end

# HFSPlusCmpfRsrcHead = Struct("HFSPlusCmpfRsrcHead",
#     UBInt32("headerSize"),
#     UBInt32("totalSize"),
#     UBInt32("dataSize"),
#     UBInt32("flags")
# )

class HFSPlusCmpfRsrcBlock < BinData::Record
end
# HFSPlusCmpfRsrcBlock = Struct("HFSPlusCmpfRsrcBlock",
#     ULInt32("offset"),
#     ULInt32("size")
# )

class HFSPlusCmpfRsrcBlockHead< BinData::Record
end

# HFSPlusCmpfRsrcBlockHead = Struct("HFSPlusCmpfRsrcBlockHead",
#     UBInt32("dataSize"),
#     ULInt32("numBlocks"),
#     Array(lambda ctx:ctx["numBlocks"], HFSPlusCmpfRsrcBlock)
# )

class HFSPlusCmpfEnd< BinData::Record
end

# HFSPlusCmpfEnd = Struct("HFSPlusCmpfEnd",
#     Array(6, UBInt32("pad")),
#     UBInt16("unk1"),
#     UBInt16("unk2"),
#     UBInt16("unk3"),
#     UBInt32("magic"),
#     UBInt32("flags"),
#     UBInt64("size"),
#     UBInt32("unk4")
# )

#
# Journal stuff
#

class JournalInfoBlock < BinData::Record
end

# JournalInfoBlock = Struct("JournalInfoBlock",
#     UBInt32("flags"),
#     Array(8, UBInt32("device_signature")),
#     UBInt64("offset"),
#     UBInt64("size"),
#     Array(32, UBInt32("reserved"))
# )

class JournalHeader < BinData::Record
end

# journal_header = Struct("journal_header",
#     ULInt32("magic"),
#     ULInt32("endian"),
#     ULInt64("start"),
#     ULInt64("end"),
#     ULInt64("size"),
#     ULInt32("blhdr_size"),
#     ULInt32("checksum"),
#     ULInt32("jhdr_size")
# )

class JournalBlockInfo < BinData::Record
end

# block_info = Struct("block_info",
#     ULInt64("bnum"),
#     ULInt32("bsize"),
#     ULInt32("next")
# )

class JournalBlockListHeader < BinData::Record
end

# block_list_header = Struct("block_list_header",
#     ULInt16("max_blocks"),
#     ULInt16("num_blocks"),
#     ULInt32("bytes_used"),
#     SLInt32("checksum"),
#     UBInt32("pad"),
#     Array(lambda ctx:ctx["num_blocks"], block_info)
# )
