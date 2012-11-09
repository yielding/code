#include <fcntl.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <hfs/hfs_format.h>
#include <libkern/OSByteOrder.h>

int main()
{
    struct stat stat_buf;
    HFSPlusVolumeHeader vheader;

    const char *vname = "/dev/rdisk0s2";

    if (lstat(vname, &stat_buf) == -1)
    {
        fprintf(stderr, "Couldn't stat %s\n", vname);
        perror(NULL);
        exit(1);
    }

    if ((stat_buf.st_mode & S_IFMT) != S_IFCHR)
    {
        fprintf(stderr, "%s is not a raw char device\n", vname);
        perror(NULL);
        exit(2);
    }

    int fd = open(vname, O_RDONLY);
    if (fd == -1)
    {
        fprintf(stderr, "%s couldn't be opened for reading\n", vname);
        perror(NULL);
        exit(3);
    }

    // The volume header starts at offset 1024
    if (pread(fd, &vheader, sizeof vheader, 1024) != sizeof vheader)
    {
        fprintf(stderr, "couldn't read %s's volume header\n", vname);
        perror(NULL);
        exit(4);
    }

    printf("fileCount   = %u\n"
            "folderCount = %u\n"
            "blockSize   = %u\n"
            "totalBlocks = %u\n"
            "freeBlocks  = %u\n",
            OSSwapBigToHostInt32(vheader.fileCount),
            OSSwapBigToHostInt32(vheader.folderCount),
            OSSwapBigToHostInt32(vheader.blockSize),
            OSSwapBigToHostInt32(vheader.totalBlocks),
            OSSwapBigToHostInt32(vheader.freeBlocks));

    close(fd);

    return 0;
}
