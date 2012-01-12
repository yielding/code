#ifndef HFSPLUS_HFSPLUS_H
#define HFSPLUS_HFSPLUS_H

#include "ByteBuffer.h"

#include <cstring>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#include "begin_packed.h"

/*
   struct BTKey
   {
   uint16_t keyLength;
   uint8_t  data[0];
   } PACKED;

   typedef BTKey* PBTKey;
   */

#define STR_SIZE(str) (sizeof(uint16_t) + (sizeof(uint16_t) * (str).length))

typedef uint32_t HFSCatalogNodeID;

enum {
  kHFSRootParentID            = 1,
  kHFSRootFolderID            = 2,
  kHFSExtentsFileID           = 3,
  kHFSCatalogFileID           = 4,
  kHFSBadBlockFileID          = 5,
  kHFSAllocationFileID        = 6,
  kHFSStartupFileID           = 7,
  kHFSAttributesFileID        = 8,
  kHFSRepairCatalogFileID     = 14,
  kHFSBogusExtentFileID       = 15,
  kHFSFirstUserCatalogNodeID  = 16
};

struct HFSUniStr255
{
  bool operator==(HFSUniStr255 const& rhs) const 
  {
    if (this != &rhs)
    {
      if (length != rhs.length) return false;

      for (uint16_t i=0; i<length; i++)
        if (unicode[i] != rhs.unicode[i]) return false;
    }

    return true;
  }

  bool operator != (HFSUniStr255 const& rhs) const
  {
    return !(*this == rhs);
  }

  void read_from(utility::hex::ByteBuffer& b)
  {
    length = b.get_uint2_be();
    if (length > 255)
      throw std::runtime_error("key name out of bounds");

    for (auto i=0; i<length; i++) unicode[i] = b.get_uint2_be();
  }

  uint16_t length;
  uint16_t unicode[255];
};

typedef const HFSUniStr255 *ConstHFSUniStr255Param;

struct HFSPlusExtentDescriptor
{
  static size_t size_of() { return 2*4; }

  void read_from(utility::hex::ByteBuffer& b)
  {
    startBlock = b.get_uint4_be();
    blockCount = b.get_uint4_be();
  }

  uint32_t startBlock;
  uint32_t blockCount;
};

typedef HFSPlusExtentDescriptor* PHFSPlusExtentDescriptor;
typedef HFSPlusExtentDescriptor  HFSPlusExtentRecord[8];
typedef std::vector<HFSPlusExtentDescriptor> HFSPlusExtentDescriptors;

struct HFSPlusForkData
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    logicalSize = b.get_uint8_be();
    clumpSize   = b.get_uint4_be();
    totalBlocks = b.get_uint4_be();
    for (int i=0; i<8; i++) extents[i].read_from(b);
  }

  uint64_t logicalSize;
  uint32_t clumpSize;
  uint32_t totalBlocks;
  HFSPlusExtentRecord extents;
} PACKED;

struct HFSPlusVolumeHeader 
{
  bool read_from(utility::hex::ByteBuffer& s);

  uint16_t signature;
  uint16_t version;
  uint32_t attributes;
  uint32_t lastMountedVersion;
  uint32_t journalInfoBlock;

  uint32_t createDate;
  uint32_t modifyDate;
  uint32_t backupDate;
  uint32_t checkedDate;

  uint32_t fileCount;
  uint32_t folderCount;

  uint32_t blockSize;
  uint32_t totalBlocks;
  uint32_t freeBlocks;

  uint32_t nextAllocation;
  uint32_t rsrcClumpSize;
  uint32_t dataClumpSize;
  HFSCatalogNodeID nextCatalogID;

  uint32_t writeCount;
  uint64_t encodingsBitmap;

  uint32_t finderInfo[8];

  HFSPlusForkData allocationFile;
  HFSPlusForkData extentsFile;
  HFSPlusForkData catalogFile;
  HFSPlusForkData attributesFile;
  HFSPlusForkData startupFile;
} PACKED;

#define FLAG_DECRYPTING 0x454d4664  // EMFd big endian
#define FLAG_DECRYPTED  0x454d4644  // EMFD big endian

enum  // BTree Node type
{  
  kBTLeafNode       = -1,
  kBTIndexNode      =  0,
  kBTHeaderNode     =  1,
  kBTMapNode        =  2
};

struct BTNodeDescriptor
{
  static size_t size_of() { return 2*4 + 2*1 + 2*2; }

  void read_from(utility::hex::ByteBuffer& b, size_t offset=0)
  {
    if (offset > 0)
      b.offset(offset);

    fLink  = b.get_uint4_be();
    bLink  = b.get_uint4_be();
    kind   = b.get_int1();
    height = b.get_uint1();
    numRecords = b.get_uint2_be();
    reserved   = b.get_uint2_be();
  }

  uint32_t fLink;
  uint32_t bLink;
  int8_t   kind;
  uint8_t  height;
  uint16_t numRecords;
  uint16_t reserved;
};

#define kHFSCaseFolding   0xCF
#define kHFSBinaryCompare 0xBC

struct BTHeaderRec
{
  void read_from(utility::hex::ByteBuffer& b, size_t offset)
  {
    if (offset > 0)
      b.offset(offset);

    treeDepth      = b.get_uint2_be();
    rootNode       = b.get_uint4_be();
    leafRecords    = b.get_uint4_be();
    firstLeafNode  = b.get_uint4_be();
    lastLeafNode   = b.get_uint4_be();
    nodeSize       = b.get_uint2_be();
    maxKeyLength   = b.get_uint2_be();
    totalNodes     = b.get_uint4_be();
    freeNodes      = b.get_uint4_be();
    reserved1      = b.get_uint2_be();
    clumpSize      = b.get_uint4_be();      // misaligned
    btreeType      = b.get_uint1();
    keyCompareType = b.get_uint1();
    attributes     = b.get_uint4_be();      // long aligned again

    for (int i=0; i<16; i++) 
      reserved3[i] = b.get_uint4_be();
  }

  uint16_t treeDepth;
  uint32_t rootNode;
  uint32_t leafRecords;
  uint32_t firstLeafNode;
  uint32_t lastLeafNode;
  uint16_t nodeSize;
  uint16_t maxKeyLength;
  uint32_t totalNodes;
  uint32_t freeNodes;
  uint16_t reserved1;
  uint32_t clumpSize;      // misaligned
  uint8_t  btreeType;
  uint8_t  keyCompareType;
  uint32_t attributes;     // long aligned again
  uint32_t reserved3[16];
} PACKED;

struct HFSPlusExtentKey
{
  HFSPlusExtentKey()
  {}

  HFSPlusExtentKey(uint8_t fork_type, HFSCatalogNodeID id, uint32_t start)
    :forkType(fork_type), fileID(id), keyLength(10), startBlock(start)
  {}

  void read_from(utility::hex::ByteBuffer& b)
  {
    keyLength  = b.get_uint2_be();
    forkType   = b.get_uint1();
    pad        = b.get_uint1();
    fileID     = b.get_uint4_be();
    startBlock = b.get_uint4_be();
  }

  uint16_t keyLength; // sizeof(this) - sizeof(uint16_t)
  uint8_t  forkType;
  uint8_t  pad;
  HFSCatalogNodeID fileID;
  uint32_t startBlock;
} PACKED;

struct HFSPlusCatalogKey
{
  bool operator==(HFSPlusCatalogKey const& rhs) const
  {
    if (this != &rhs)
    {
      if (keyLength != rhs.keyLength) return false;
      if (parentID != rhs.parentID) return false;
      if (nodeName != rhs.nodeName) return false;
    }

    return true;
  }

  void read_from(utility::hex::ByteBuffer& b)
  {
    keyLength = b.get_uint2_be();
    parentID  = b.get_uint4_be();
    nodeName.read_from(b);
  }

  uint16_t         keyLength;
  HFSCatalogNodeID parentID;
  HFSUniStr255     nodeName;
} PACKED;

typedef HFSPlusCatalogKey* PHFSPlusCatalogKey;

struct HFSPoint
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    v = b.get_int2_be();
    h = b.get_int2_be();
  }

  int16_t v;
  int16_t h;
} PACKED;

struct HFSRect
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    top    = b.get_int2_be();
    left   = b.get_int2_be();
    bottom = b.get_int2_be();
    right  = b.get_int2_be();
  }

  int16_t top;
  int16_t left;
  int16_t bottom;
  int16_t right;
} PACKED;

// OSType is a 32-bit value made by packing four 1-byte characters 
// together.
typedef uint32_t      FourCharCode;
typedef FourCharCode  OSType;

// Finder flags (finderFlags, fdFlags and frFlags)
enum {
  kIsOnDesk       = 0x0001,     // Files and folders (System 6) 
  kColor          = 0x000E,     // Files and folders 
  kIsShared       = 0x0040,     // Files only (Applications only) If 
  // clear, the application needs to write to its resource fork, 
  // and therefore cannot be shared on a server 
  kHasNoINITs     = 0x0080,     // Files only (Extensions/Control 
  // Panels only) 
  // This file contains no INIT resource 
  kHasBeenInited  = 0x0100,     // Files only.  Clear if the file contains desktop database resources 
  // ('BNDL', 'FREF', 'open', 'kind'...) that have not been added yet.
  // Set only by the Finder. 
  // Reserved for folders 
  kHasCustomIcon  = 0x0400,     // Files and folders 
  kIsStationery   = 0x0800,     // Files only
  kNameLocked     = 0x1000,     // Files and folders
  kHasBundle      = 0x2000,     // Files only 
  kIsInvisible    = 0x4000,     // Files and folders 
  kIsAlias        = 0x8000      // Files only 
};

// Extended flags (extendedFinderFlags, fdXFlags and frXFlags)
enum {
  kExtendedFlagsAreInvalid    = 0x8000, /* The other extended flags */
  /* should be ignored */
  kExtendedFlagHasCustomBadge = 0x0100, /* The file or folder has a */
  /* badge resource */
  kExtendedFlagHasRoutingInfo = 0x0004  /* The file contains routing */
    /* info resource */
};

enum {
  kSymLinkFileType  = 0x736C6E6B, // 'slnk'
  kSymLinkCreator   = 0x72686170  // 'rhap'
};

struct FileInfo
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    fileType    = b.get_uint4_be();
    fileCreator = b.get_uint4_be();
    finderFlags = b.get_uint2_be();
    location.read_from(b);
    reservedField = b.get_uint2_le();
  }

  OSType    fileType;           // The type of the file
  OSType    fileCreator;        // The file's creator
  uint16_t  finderFlags;
  HFSPoint  location;           // File's location in the folder.
  uint16_t  reservedField;
} PACKED;

struct ExtendedFileInfo
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    for(int i=0; i<4; i++) reserved1[i] = b.get_int2_le();
    extendedFinderFlags = b.get_uint2_be();
    reserved2           = b.get_int2_le();
    putAwayFolderID     = b.get_int4_be();
  }

  int16_t   reserved1[4];
  uint16_t  extendedFinderFlags;
  int16_t   reserved2;
  int32_t   putAwayFolderID;
} PACKED;

struct FolderInfo
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    windowBounds.read_from(b);
    finderFlags = b.get_uint2_be();
    location.read_from(b);
    reservedField = b.get_uint2_be();
  }

  HFSRect   windowBounds;       // The position and dimension of the folder's window
  uint16_t  finderFlags;
  HFSPoint  location;           // Folder's location in the parent folder. If set to {0, 0}, the Finder 
  uint16_t  reservedField;      // will place the item automatically 
} PACKED;

struct ExtendedFolderInfo
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    scrollPosition.read_from(b);
    reserved1 = b.get_int4_le();
    extendedFinderFlags = b.get_uint2_be();
    reserved2 = b.get_int2_le();
    putAwayFolderID = b.get_int4_be();
  }

  HFSPoint     scrollPosition;     // Scroll position (for icon views)
  int32_t   reserved1;
  uint16_t  extendedFinderFlags;
  int16_t   reserved2;
  int32_t   putAwayFolderID;
} PACKED;

#ifndef _STAT_H_
#ifndef _SYS_STAT_H
#define S_ISUID 0004000     /* set user id on execution */
#define S_ISGID 0002000     /* set group id on execution */
#define S_ISTXT 0001000     /* sticky bit */

#define S_IRWXU 0000700     /* RWX mask for owner */
#define S_IRUSR 0000400     /* R for owner */
#define S_IWUSR 0000200     /* W for owner */
#define S_IXUSR 0000100     /* X for owner */

#define S_IRWXG 0000070     /* RWX mask for group */
#define S_IRGRP 0000040     /* R for group */
#define S_IWGRP 0000020     /* W for group */
#define S_IXGRP 0000010     /* X for group */

#define S_IRWXO 0000007     /* RWX mask for other */
#define S_IROTH 0000004     /* R for other */
#define S_IWOTH 0000002     /* W for other */
#define S_IXOTH 0000001     /* X for other */

#define S_IFMT   0170000    /* type of file mask */
#define S_IFIFO  0010000    /* named pipe (fifo) */
#define S_IFCHR  0020000    /* character special */
#define S_IFDIR  0040000    /* directory */
#define S_IFBLK  0060000    /* block special */
#define S_IFREG  0100000    /* regular */
#define S_IFLNK  0120000    /* symbolic link */
#define S_IFSOCK 0140000    /* socket */
#define S_IFWHT  0160000    /* whiteout */

#define UF_COMPRESSED 040

#endif
#endif

struct HFSPlusBSDInfo
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    ownerID = b.get_uint4_be();
    groupID = b.get_uint4_be();
    adminFlags = b.get_uint1();
    ownerFlags = b.get_uint1();
    fileMode   = b.get_uint2_be();
    *((uint32_t*)&special) = b.get_uint4_be();
  }

  uint32_t  ownerID;
  uint32_t  groupID;
  uint8_t   adminFlags;
  uint8_t   ownerFlags;
  uint16_t  fileMode;
  union 
  {
    uint32_t  iNodeNum;
    uint32_t  linkCount;
    uint32_t  rawDevice;
  } special;
} PACKED;

enum {
  kHFSPlusFolderRecord        = 0x0001,
  kHFSPlusFileRecord          = 0x0002,
  kHFSPlusFolderThreadRecord  = 0x0003,
  kHFSPlusFileThreadRecord    = 0x0004
};

enum {
  kHFSFileLockedBit       = 0x0000,   // file is locked and cannot be written to
  kHFSFileLockedMask      = 0x0001,

  kHFSThreadExistsBit     = 0x0001,   // a file thread record exists for this file
  kHFSThreadExistsMask    = 0x0002,

  kHFSHasAttributesBit    = 0x0002,   // object has extended attributes
  kHFSHasAttributesMask   = 0x0004,

  kHFSHasSecurityBit      = 0x0003,   // object has security data (ACLs) */
  kHFSHasSecurityMask     = 0x0008,

  kHFSHasFolderCountBit   = 0x0004,   // only for HFSX, folder maintains a separate sub-folder count
  kHFSHasFolderCountMask  = 0x0010,   // (sum of folder records and directory hard links)

  kHFSHasLinkChainBit     = 0x0005,   // has hardlink chain (inode or link)
  kHFSHasLinkChainMask    = 0x0020,

  kHFSHasChildLinkBit     = 0x0006,   // folder has a child that's a dir link
  kHFSHasChildLinkMask    = 0x0040
};

struct HFSPlusCatalogFolder
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    recordType = b.get_int2_be();
    flags      = b.get_uint2_be();
    valence    = b.get_uint4_be();
    folderID   = b.get_uint4_be();
    createDate = b.get_uint4_be();
    contentModDate   = b.get_uint4_be();
    attributeModDate = b.get_uint4_be();
    accessDate       = b.get_uint4_be();
    backupDate       = b.get_uint4_be();
    permissions.read_from(b);
    userInfo   .read_from(b);
    finderInfo .read_from(b);
    textEncoding = b.get_uint4_be();
    folderCount  = b.get_uint4_be();
  }

  int16_t            recordType;
  uint16_t           flags;
  uint32_t           valence;
  HFSCatalogNodeID   folderID;
  uint32_t           createDate;
  uint32_t           contentModDate;
  uint32_t           attributeModDate;
  uint32_t           accessDate;
  uint32_t           backupDate;
  HFSPlusBSDInfo     permissions;
  FolderInfo         userInfo;
  ExtendedFolderInfo finderInfo;
  uint32_t           textEncoding;
  uint32_t           folderCount;
} PACKED;

typedef HFSPlusCatalogFolder* PHFSPlusCatalogFolder;

struct HFSPlusCatalogFile
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    recordType = b.get_int2_be();
    flags      = b.get_uint2_be();
    reserved1  = b.get_uint4_be();
    fileID     = b.get_uint4_be();
    createDate = b.get_uint4_be();
    contentModDate   = b.get_uint4_be();
    attributeModDate = b.get_uint4_be();
    accessDate       = b.get_uint4_be();
    backupDate       = b.get_uint4_be();
    permissions.read_from(b);
    userInfo   .read_from(b);
    finderInfo .read_from(b);
    textEncoding = b.get_uint4_be();
    reserved2    = b.get_uint4_le();
    dataFork.read_from(b);
    resourceFork.read_from(b);
  }

  bool is_symlink()
  {
    return (permissions.fileMode & S_IFLNK) == S_IFLNK;
  }

  int16_t             recordType;
  uint16_t            flags;
  uint32_t            reserved1;
  HFSCatalogNodeID    fileID;
  uint32_t            createDate;
  uint32_t            contentModDate;
  uint32_t            attributeModDate;
  uint32_t            accessDate;
  uint32_t            backupDate;
  HFSPlusBSDInfo      permissions;
  FileInfo            userInfo;
  ExtendedFileInfo    finderInfo;
  uint32_t            textEncoding;
  uint32_t            reserved2;

  HFSPlusForkData     dataFork;
  HFSPlusForkData     resourceFork;
} PACKED;

typedef HFSPlusCatalogFile* PHFSPlusCatalogFile;

struct HFSPlusCatalogThread
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    recordType = b.get_int2_be();
    reserved   = b.get_int2_le();
    parentID   = b.get_uint4_be();
    nodeName.read_from(b);
  }

  int16_t             recordType;
  int16_t             reserved;
  HFSCatalogNodeID    parentID;
  HFSUniStr255        nodeName;
} PACKED;

typedef HFSPlusCatalogThread* PHFSPlusCatalogThread;

enum {
  kHFSPlusAttrInlineData = 0x10,
  kHFSPlusAttrForkData = 0x20,
  kHFSPlusAttrExtents	= 0x30
};

struct HFSPlusAttrForkData
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    recordType = b.get_uint4_be();
    reserved   = b.get_uint4_be();
    theFork.read_from(b);
  }

  uint32_t 	recordType;
  uint32_t 	reserved;
  HFSPlusForkData theFork;
} PACKED;

struct HFSPlusAttrExtents
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    recordType = b.get_uint4_be();
    reserved   = b.get_uint4_be();
    for (int i=0; i<8; i++) extents[i].read_from(b);
  }

  uint32_t recordType;
  uint32_t reserved;
  HFSPlusExtentRecord	extents;
};

struct HFSPlusAttrData
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    recordType = b.get_uint4_be();
    for (int i=0; i<2; i++) reserved[i] = b.get_uint4_be();
    size = b.get_uint4_be();

    // 아래 코드는 별로도 채워야 한다.
    // for (uint32_t i=0; i<size; ++i)  data.push_back(b.get_uint1());
  }

  uint32_t recordType;
  uint32_t reserved[2];
  uint32_t size;
  uint8_t  data[0];
} PACKED;

struct HFSPlusAttrRecord
{
  uint32_t 	recordType;
  uint8_t   data[0];
};

typedef HFSPlusAttrRecord* PHFSPlusAttrRecord;

struct HFSPlusAttrKey
{
  HFSPlusAttrKey() 
  {}

  HFSPlusAttrKey(uint32_t cnid, uint32_t sb, uint16_t name_len)
  {
    std::memset(&name, 0, sizeof(HFSUniStr255));
    keyLength   = sizeof(HFSPlusAttrKey) - sizeof(HFSUniStr255) + 2 + (2 * name_len);
    pad         = 0;
    fileID      = cnid;
    startBlock  = sb;
    name.length = name_len;
  }

  bool operator==(HFSPlusAttrKey const& rhs) const
  {
    if (this != &rhs)
    {
      if (keyLength != rhs.keyLength) return false;
      if (pad != rhs.pad) return false;
      if (fileID != rhs.fileID) return false;
      if (name != rhs.name) return false;
    }

    return true;
  }

  void read_from(utility::hex::ByteBuffer& b)
  {
    keyLength   = b.get_uint2_be();
    pad         = b.get_uint2_be();
    fileID      = b.get_uint4_be();
    startBlock  = b.get_uint4_be();

    // 읽는 부분이 나누어져 있다. 주의
    name.length = b.get_uint2_be();
    // name.read_from(b);
  }

  uint16_t     keyLength;
  uint16_t     pad;
  uint32_t     fileID;
  uint32_t     startBlock;
  HFSUniStr255 name;
} PACKED;

typedef HFSPlusAttrKey* PHFSPlusAttrKey;

enum {
  kHardLinkFileType = 0x686C6E6B,  /* 'hlnk' */
  kHFSPlusCreator   = 0x6866732B   /* 'hfs+' */
};

struct HFSPlusCatalogRecord
{
  bool is_folder() { return recordType == kHFSPlusFolderRecord;   }
  bool is_file()   { return recordType == kHFSPlusFileRecord;     }
  PHFSPlusCatalogThread to_thread() { return PHFSPlusCatalogThread(this); }
  PHFSPlusCatalogFile   to_file()   { return PHFSPlusCatalogFile(this);   }
  PHFSPlusCatalogFolder to_folder() { return PHFSPlusCatalogFolder(this); }

  //bool is_attr_inline { return recordType == kHFSPlusAttrInlineData; }

  int16_t recordType;
  uint8_t data[0];
} PACKED;

typedef HFSPlusCatalogRecord* PHFSPlusCatalogRecord;

struct CatalogRecord
{
  HFSUniStr255 name;
  HFSPlusCatalogRecord* record;
};

typedef std::vector<CatalogRecord> CatalogRecordList;

typedef std::vector<std::string> XAttrList;

////////////////////////////////////////////////////////////////////////////////
//
// Journal
//
////////////////////////////////////////////////////////////////////////////////
struct JournalInfoBlock 
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    flags = b.get_uint4_be();
    for (uint16_t i=0; i<8; i++) device_signature[i] = b.get_uint4_be();
    offset = b.get_uint8_be();
    size   = b.get_uint8_be();
    for (uint16_t i=0; i<32; i++) reserved[i] = b.get_uint4_be();
  }

  uint32_t flags;
  uint32_t device_signature[8];
  uint64_t offset;
  uint64_t size;
  uint32_t reserved[32];
};

struct journal_header
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    magic      = b.get_uint4_le();
    endian     = b.get_uint4_le();
    start      = b.get_uint8_le();
    end        = b.get_uint8_le();
    size       = b.get_uint8_le();
    blhdr_size = b.get_uint4_le();   // block header size;
    checksum   = b.get_uint4_le();
    jhdr_size  = b.get_uint4_le();
  }

  uint32_t magic;
  uint32_t endian;
  uint64_t start;
  uint64_t end;
  uint64_t size;
  uint32_t blhdr_size;   // block header size;
  uint32_t checksum;
  uint32_t jhdr_size;
};

#define JOURNAL_HEADER_MAGIC 0x4a4e4c78
#define ENDIAN_MAGIC         0x12345678

struct block_info
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    bnum  = b.get_uint8_le();
    bsize = b.get_uint4_le();
    next  = b.get_uint4_le();
  }

  uint64_t   bnum;
  uint32_t   bsize;
  uint32_t   next;
};

struct block_list_header
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    max_blocks = b.get_uint2_le();
    num_blocks = b.get_uint2_le();
    bytes_used = b.get_uint4_le();
    checksum   = b.get_int4_le();
    pad        = b.get_uint4_be();
    for (uint32_t i=0; i<num_blocks; i++)
      binfo[i].read_from(b);
  }

  uint16_t   max_blocks;
  uint16_t   num_blocks;
  uint32_t   bytes_used;
  uint32_t   checksum;
  uint32_t   pad;
  block_info binfo[1];
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#define TIME_OFFSET_FROM_UNIX 2082844800L

#define APPLE_TO_UNIX_TIME(x) ((x) - TIME_OFFSET_FROM_UNIX)
#define UNIX_TO_APPLE_TIME(x) ((x) + TIME_OFFSET_FROM_UNIX)

#include "end_packed.h"
#endif
