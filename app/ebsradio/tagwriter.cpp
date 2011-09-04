#include <boost/filesystem.hpp>

#include <taglib/tlist.h>
#include <taglib/fileref.h>
#include <taglib/tfile.h>
#include <taglib/tag.h>

#include <iostream>
#include <string>

using namespace std;
      namespace fs = boost::filesystem;

bool isArgument(string const& s)
{
  return s.size() == 2 && s[0] == '-';
}

void usage()
{
  cout << endl;
  cout << "Usage: tagwriter <fields> <files>" << endl;
  cout << endl;
  cout << "Where the valid fields are:" << endl;
  cout << "  -t <title>"   << endl;
  cout << "  -a <artist>"  << endl;
  cout << "  -A <album>"   << endl;
  cout << "  -c <comment>" << endl;
  cout << "  -g <genre>"   << endl;
  cout << "  -y <year>"    << endl;
  cout << "  -T <track>"   << endl;
  cout << endl;
}

int main(int argc, char *argv[])
{
  TagLib::List<TagLib::FileRef> fileList;
  string filename(argv[argc - 1]);

  while (argc > 0 && fs::exists(argv[argc - 1]))
  {
    TagLib::FileRef f(argv[argc - 1]);

    if (!f.isNull() && f.tag())
      fileList.append(f);

    argc--;
  }

  if (fileList.isEmpty())
  {
    usage();
    return 1;
  }

  for (int i=1; i <argc-1; i += 2)
  {
    if (isArgument(argv[i]) && i + 1 < argc && !isArgument(argv[i + 1]))
    {
      char field = argv[i][1];
      TagLib::String value = argv[i + 1];

      for (auto& file: fileList)
      {
        auto t = file.tag();

        switch (field) {
        case 't':
          t->setTitle(value);
          break;
        case 'a':
          t->setArtist(value);
          break;
        case 'A':
          t->setAlbum(value);
          break;
        case 'c':
          t->setComment(value);
          break;
        case 'g':
          t->setGenre(value);
          break;
        case 'y':
          t->setYear(value.toInt());
          break;
        case 'T':
          t->setTrack(value.toInt());
          break;
        default:
          usage();
          break;
        }
      }
    }
    else
    {
      usage();
      return 1;
    }
  }

  for (auto& ref: fileList)
      ref.file()->save();

  return 0;
}
