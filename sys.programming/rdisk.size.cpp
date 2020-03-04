#include <sys/disk.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <unistd.h>

#include <iostream>

using namespace std;

int main()
{
  // Open disk
  uint32_t dev = open("/dev/disk0", O_RDONLY);

  if (dev == -1) 
  {
    cout << "Failed to open disk" << endl;
    return -1;
  }

  char buffer[0x200] = { 0 };

  cout << "read size: " << read(dev, buffer, 0x200) << endl;

  uint64_t sector_count = 0;
  // Query the number of sectors on the disk
  ioctl(dev, DKIOCGETBLOCKCOUNT, &sector_count);

  uint32_t sector_size = 0;
  // Query the size of each sector
  ioctl(dev, DKIOCGETBLOCKSIZE, &sector_size);

  uint64_t disk_size = sector_count * sector_size;
  cout << disk_size << endl;

  return 0;
}
