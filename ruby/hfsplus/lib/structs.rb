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

class HFSUnicode255 < BinData::Record
  endian :big
  uint16 :length_
  array  :unicode, :type => :uint8, :inital_length => lambda { length_ * 2 }
end

class HFSPlusAttrKey < BinData::Record
  endian :big

  uint16 :keyLength
  uint32 :pad
  uint32 :fileID
  uint32 :startBlock
  hfs_unicode255 :nodeName
end

class HFSPlusAttrData < BinData::Record
  endian :big
  uint32 :recordType
  array  :reserved, :type => :uint32, :inital_length => 2
  uint32 :size_
  array  :data, :type => :uint8, :inital_length => lambda { size_ }
end

class HFSPlusCatalogKey < BinData::Record
  endian :big
  uint16 :keyLength
  uint32 :parentID
  hfs_unicode255 :nodeName
end

class HFSPlusBSDInfo < BinData::Record
  endian :big

  uint32 :ownerID
  uint32 :groupID
  uint8  :adminFlags
  uint8  :ownerFlags
  uint16 :fileMode
  uint32 :union_special
end

class Point < BinData::Record
  int16be :v
  int16be :h
end

class Rect < BinData::Record
  endian :big
  int16  :top
  int16  :left
  int16  :bottom
  int16  :right
end

class FileInfo < BinData::Record
  endian :big
  uint32 :fileType
  uint32 :fileCreator
  uint16 :finderFlags
  point  :location
  uint16 :reservedField
end

class ExtendedFileInfo < BinData::Record
  endian :big
  array  :reserved1, :type => :int16, :initial_length => 4
  uint16 :extendedFinderFlags
  int16  :reserved2
  int32  :putAwayFolderID
end

class FolderInfo < BinData::Record
  endian :big
  rect   :windodwBounds
  uint16 :finderFlags
  point  :location
  uint16 :reservedField
end

class ExtendedFolderInfo < BinData::Record
  endian :big
  point  :scrollPosition
  int32  :reserved1
  uint16 :extendedFinderFlags
  int16  :reserved2
  int16  :putAwayFolderID
end

class HFSPlusCatalogFolder < BinData::Record
  endian :big
  # int16  :recordType # python에서는 다른 곳에서 이 데이타를 읽은 후 이 레코드를 해석
  uint16 :flags
  uint32 :valence
  uint32 :folderID
  uint32 :createDate
  uint32 :contentModDate
  uint32 :attributeModDate
  uint32 :accessDate
  uint32 :backupDate
  hfs_plus_bsd_info :permissions
  folder_info :userInfo
  uint32 :textEncoding
  uint32 :folderCount
end

class HFSPlusCatalogFile < BinData::Record
  endian :big
  # int16  :recordType
  uint16 :flags
  uint32 :reserved1
  uint32 :fileID
  uint32 :createDate
  uint32 :contentModDate
  uint32 :attributeModDate
  uint32 :accessDate
  uint32 :backupDate
  hfs_plus_bsd_info :permissions
  file_info :userInfo
  extended_file_info :finderInfo
  uint32 :textEncoding
  uint32 :reserved2

  hfs_plus_fork_data :dataFork
  hfs_plus_fork_data :resourceFork
end

class HFSPlusCatalogThread < BinData::Record
  endian :big
  # int16  :recordType
  int16  :reserved
  uint32 :parentID
  hfs_unicode255 :nodeName
end

class HFSPlusCatalogData < BinData::Record
  endian :big
  uint16 :recordType, :check_value => lambda { value > 0 and value < 5 }
  choice :data, :selection => :recordType do
    HFSPlusCatalogFolder 1
    HFSPlusCatalogFile   2
    HFSPlusCatalogThread 3
    HFSPlusCatalogThread 4
  end
end

class HFSPlusExtentKey < BinData::Record
  endian :big
  uint16 :keyLength
  uint8  :forkType
  uint8  :pad
  uint32 :fileID
  uint32 :startBlock
end

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
  endian :big
  uint32 :falgs
  array  :device_signature, :type => :uint32, :initial_length => 8
  uint64 :offset_       # name conflict
  uint64 :size_         # name conflict
  array  :reserved, :type => :uint32, :initial_length => 32 
end

class JournalHeader < BinData::Record
  endian :little
  uint32 :magic
  uint32 :endian
  uint64 :start
  uint64 :end_          # name conflict
  uint64 :size_         # name conflict
  uint32 :blhdr_size
  uint32 :checksum
  uint32 :jhdr_size
end

class JournalBlockInfo < BinData::Record
  endian :little
  uint64 :bnum
  uint32 :bsize
  uint32 :next_        # name conflict
end

class JournalBlockListHeader < BinData::Record
  endian :little
  uint16 :max_blocks
  uint16 :num_blocks
  uint32 :bytes_used
  int32  :checksum
  uint32be :pad
  array  :binfo, :type => :journal_block_info, :initial_length => lambda { num_blocks }
end
