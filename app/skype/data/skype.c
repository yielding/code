/**********************************************************
SkypeLog: Display a Skype log as text.
Copyright 2008 Neal Krawetz, Hacker Factor
Created for the DoD Cyber Crime Center (DC3) Forensic Challenge 2008.
Challenge name: 302_SKYPE_Communications_Logs_Challenge2008

=========================================================
Licensing and distribution:
This code is provided as open source but is NOT licensed under the GPL
or other common open source licenses.  This code may not be re-licensed
without the explicit, written permission of Neal Krawetz.

This software is provided AS-IS with no warranty expressed or implied.
It may not be accurate and many not be suitable for any specific need.
In locations where a warranty is required, this code may not be used.
The copyright holder is not liable for any costs associated with using
this software.

This code, or portions of it, may be incorporated into other projects as
long as the code is not re-licensed and the following acknowledgement is
included along with any licensing files, copyright statements, and
source code:
This software includes code from skypelog by Neal Krawetz,
Hacker Factor Solutions, Copyright 2008, All Rights Reserved.

=========================================================
To compile:  gcc skypelog.c -o skypelog

=========================================================
About the log file format:

Skype logs are in ".dbb" files.  These are binary files.

There are some files out there that try to decode the file format,
but they are wrong.
Example:
http://dl.free.fr/gHps09KVK/SkypeLogFileAnalysis.pdf
http://dmytry.pandromeda.com/texts/skype_chatlogs_friday_13.html
These documents try to identify 2-byte tags and the content that follows.
However, the "tags" in the documents are actually coincidental.

The actual format appears to be a data dump.
There are informative portions, but also some random garbage.
(The garbage may have meaning, but I haven't found it yet.)
How to parse:
1st 16 bytes:
'1331' -- denotes start of a record
record size (4 bytes)
session (4 bytes)
unk (4 bytes)
Then...
For all of the bytes in the record (record size):
All content begins with a 0x03.
Read the data until you hit one of these bytes.
This does NOT need to be on the even byte offset!
- If the byte is a 0x03, then it is followed by a number.
Numbers are in a 7-bit format.  The MSB identifies whether it is
the last byte in the number sequence.
- If the MSB is set (Byte & 0x80), then it is not the last byte
in the number.
- If the MSB is not set, then it is the last byte in the number.
The Number identifies the TYPE of the data field.
All data sections end with 0x00, 0x01, 0x02, or 0x03.
- If it is 0x03, then it denotes a new dataset immediately after
the last data set.  Process this next set of data.
- If it is 0x00, then the next bytes are junk.  Read until you hit
another 0x03.

The files themselves are separated by record size.
For example, if the record is 256 bytes or smaller, then it is stored in
a 256 byte block and separated into a filename that contains "256" in
the name.
call256.dbb
callmember256.dbb
chat256.dbb
chatmsg256.dbb   <-- the actual chat log
...
If the record is larger than 256 but smaller than 512 bytes, then it
goes into chat512.dbb, chatmsg512.dbb, etc.
This means, a single conversation is likely split across multiple files.

To put together the whole conversation, you need all of the parts.
An easy way to assemble them:
./skypelog chatmsg256.dbb chatmsg512.dbb charmsg1024.dbb | sort
or
./skypelog chatmsg*.dbb | sort

This exacts data from each of the files, then uses sort to put them in
the proper order (grouped by session and then date).

To just see the time, sender, and message:
skypelog chatmsg*.dbb | sort | awk -F\| '{print $2 "|" $3 "|" $4}'

Each conversation includes a session ID.  For example:
#useralice/$userbob;12345678abcdef10
The first name is the caller.
The second name is the recipient of the call.
The 16 hex digits is a unique ID.  This ID appears to change with each
new connection.
The Session is also listed in the chat log (e.g., chat256.dbb or
    chat512.dbb). 

Let's assume you only have one of the chat files.  How much of the
conversation are you missing?
Each of the chat lines (chatmsg*.dbb) contains a Log ID.  (Denoted
    by this program with "LogId:".)  The maximum ID is stored in the basic
chat file (e.g., chat256.dbb or chat512.dbb).  Each line in chatmsg*.dbb
should have a Log ID that increments by one.  Thus, you can count the
number of missing lines.

HOWEVER:  Watch the timestamps!  Each time they reconnect, the Log ID
seems to restart.  Thus: Sort by date THEN by Log.  And then watch for
missing sequences in the Log ID.
skypelog chatmsg*.dbb | \
           awk -F\| '{print $2"|"$5"|"$3"|"$4"|"$6"|"$7 }' | \
sort
(Best way to use the Log ID: if two lines happen in the same second,
 then use the Log ID to find which one came a fraction faster.)

Finally: This parsing was found by reverse-engineering existing logfiles.
There are plenty of bytes that are skipped (use -vvv to see them) and
they may have important meanings.  Even the blocking at 0x03 may not be
accurate.  (There may be a better way to identify data segments.)

  To see the unknown stuff, use -vvv:
  [num] = identified number for type of data
  *byte* = byte skipped because the type of data is unknown.
{bytes} = bytes skipped because it is outside of an identified data set.
**********************************************************/

#include <stdlib.h>

/* support files > 2G */
#if 0
# define __USE_LARGEFILE64
# define __USE_FILE_OFFSET64
typedef struct stat64 mystat;
#else
// # define O_LARGEFILE 0
// # define fstat64(x,y) fstat(x,y)
// # define mmap64(a,b,c,d,e,f) mmap(a,b,c,d,e,f)
typedef struct stat mystat;
#endif

#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <sys/mman.h>  /* for mmap */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

typedef uint8_t byte;
typedef uint64_t offset;

/****************************************************************/
/****************************************************************/
/****************************************************************/

int FileIn=0;
byte *Memory=NULL;
offset MemorySize=0;
offset Index=0;	/* index into memory */

int Verbose=0;
char Divider='|';
int BlankFlag=0;

/****************************************************************/
/****************************************************************/
/****************************************************************/

/*********************************************
  SkypeCloseFile(): Close the memory map.
  This sets the globals.
 *********************************************/
void	SkypeCloseFile	()
{
  if (Memory) { munmap(Memory,MemorySize); }
  MemorySize=0;
  if (FileIn) { close(FileIn); }
} /* SkypeCloseFile() */

/*********************************************
  SkypeOpenFile(): Memory map a file.
  This sets the globals.
 *********************************************/
void	SkypeOpenFile	(char *Filename)
{
  mystat Stat;

  /* block re-opens */
  SkypeCloseFile();

  /* Open file */
  // FileIn = open(Filename,O_RDONLY|O_LARGEFILE);
  FileIn = open(Filename,O_RDONLY);
  if (FileIn == -1)
  {
    fprintf(stderr,"ERROR: Unable to open file (%s)\n",Filename);
    exit(-1);
  }

  if (fstat64(FileIn,&Stat) == -1)
  {
    fprintf(stderr,"ERROR: Unable to stat file (%s)\n",Filename);
    close(FileIn);
    exit(-1);
  }

  MemorySize=Stat.st_size;
  if (MemorySize > 0)
  {
    // Memory=mmap64(0,MemorySize,PROT_READ,MAP_PRIVATE,FileIn,0);
    Memory=mmap(0,MemorySize,PROT_READ,MAP_PRIVATE,FileIn,0);
    if (Memory == MAP_FAILED)
    {
      fprintf(stderr,"ERROR: Unable to mmap file (%s)\n",Filename);
      close(FileIn);
      exit(-1);
    }
  }
} /* SkypeOpenFile() */

/*********************************************
  SkypeFindRecord(): Find the next record start.
  Skype records begin with "l33l".
  Returns 1st byte AFTER the record tag.
  Returns (offset)(-1) on EOF.
  This modifies Index.
 *********************************************/
offset	SkypeFindRecord	()
{
  if (memcmp(Memory+Index,"l33l",4) == 0) { Index+=4; return(Index); }
  for( ; Index+4 < MemorySize; Index++)
  {
    if (memcmp(Memory+Index,"l33l",4) == 0)
    {
      Index+=4;
      return(Index);
    }
  }
  return(-1);
} /* SkypeFindRecord() */

/*********************************************
  ReadNumber(): Read a number stored in the
  Skype data format.  Assume number starts
  at Memory[Index].
  The format:
  If the byte is < 0x80, then it is the number.
  If the byte is > 0x80, then num & 0x7f are
  seven bits to the number.
  Returns the number and increments Index.
 *********************************************/
uint64_t	ReadNumber	()
{
  int Shift=0;
  uint64_t Num=0;
  while((Index < MemorySize) && (Memory[Index] & 0x80))
  {
    Num = Num | ((Memory[Index] & 0x7f) << Shift);
    Shift += 7;
    Index++;
  }
  if (Index < MemorySize)
  {
    Num = Num | ((Memory[Index] & 0x7f) << Shift);
    Index++;
  }
  return(Num);
} /* ReadNumber() */

/*********************************************
  Bytes2Val(): Read/combine bytes and return a number.
 *********************************************/
uint64_t	Bytes2Val	(int Len)
{
  uint64_t Val=0;
  int Shift=0;

  if (Index+Len >= MemorySize)
  {
    fprintf(stderr,"ERROR: Fractional Skype record.\n");
    return(0);
  }

  Len--;
  Shift=0;
  while(Len >= 0)
  {
    Val |= (Memory[Index] << Shift);
    Shift+=8;
    Len--;
    Index++;
  }
  return(Val);
} /* Bytes2Val() */

/***************************************************************/
/***************************************************************/
/***************************************************************/

/*********************************************
  Usage():
 *********************************************/
void	Usage	(char *Name)
{
  if (strchr(Name,'/'))
  {
    Name = strrchr(Name,'/');
    Name++;
  }
  printf("Usage: [options] %s\n",Name);
  printf("  -b  :: insert a blank line between records\n");
  printf("  -Fc :: set the field divider (default: -F'|')\n");
  printf("  -v  :: verbose (-vv = more verbose)\n");
} /* Usage() */

/*********************************************
  main():
 *********************************************/
int	main	(int argc, char *argv[])
{
  int c;
  long RecordNum=0;
  long RecordLen=0;
  offset RecordEnd=0;
  time_t Time;
  struct tm *TimeS;
  uint64_t SequenceNum;
  uint64_t Number;
  int First=0;
  int PrintString=0;
  char *Label;

  while((c = getopt(argc,argv,"bF:v")) != -1)
  {
    switch(c)
    {
      case 'b':	BlankFlag=1; break;
      case 'F':
                Divider=optarg[0];
                if (!strcmp(optarg,"\\n")) Divider='\n';
                break;
      case 'v':	Verbose++; break;
      default:
                Usage(argv[0]);
                exit(-1);
    }
  }

  /* Process each file */
  for( ; optind < argc; optind++)
  {
    SkypeOpenFile(argv[optind]);
    Index=0;
    while( (Index < MemorySize) && (SkypeFindRecord() != (offset)(-1)) )
    {
      RecordNum++;
      if (Verbose) printf("# Record %ld in %s\n",RecordNum,argv[optind]);
      RecordLen = Bytes2Val(4);
      RecordEnd = Index + RecordLen;
      if (Verbose)
      {
        printf("# Record length: %ld bytes (0x%08lx)\n",
            (long)RecordLen,(long)RecordLen);
      }

      if (Index+RecordLen > MemorySize)
      {
        fprintf(stderr,"WARNING: Fractional record being processed.\n");
        /* Permit it to go on! */
        RecordLen = MemorySize - Index;
      }

      SequenceNum = Bytes2Val(4);
      if (Verbose)
      {
        printf("# Sequence: %08lx\n",(unsigned long)SequenceNum);
      }

      /* Process the record */
      First = 0; /* no output yet (first entry not seen) */
      Time=0;
      while(Index < RecordEnd)
      {
        /* Skip until we hit 0x03 */
        while ((Index < RecordEnd) && (Memory[Index] != 0x03))
        {
          if (Verbose > 2) printf(" {%02x}",Memory[Index]);
          Index++;
        }

        /* Check if we found the start */
        while ((Index < RecordEnd) && (Memory[Index] == 0x03))
        {
          Number=0;
          /* Skip multiple 0x03 */
          while (Memory[Index] == 0x03) Index++;
          Number = ReadNumber();

          PrintString=1;
          Label=NULL;
          switch(Number)
          {
            case 15: Label="VoicemailFile"; break;
            case 16: Label="Call"; break;
            case 20: Label="Summary"; break;
            case 36: Label="Language"; break;
            case 40: Label="Country"; break;
            case 48: Label="City"; break;
            case 51: Label="File"; break;
            case 55: Label="Peek"; break;
            case 64: Label="Email"; break;
            case 68: Label="URL"; break;
            case 72: Label="Description"; break;
            case 116: Label="Country"; break;
            case 184: Label="Phone"; break;
            case 296: Label="Type"; break;
            case 404: Label="User"; break; /* voicemail */
            case 408: Label="User"; break; /* voicemail */
            case 440: Label="Session"; break;
            case 456: Label="Members"; break; /* username */
            case 460: Label="Members"; break;
            case 468: Label="User"; break;
            case 472: Label="Name"; break;
            case 480: Label="Session"; break;
            case 488: Label="Sender"; break; /* username */
            case 492: Label="Sender"; break; /* screenname */
            case 500: Label="Recipient"; break;
            case 508: Label="Message"; break;
            case 584: Label="Session"; break;
            case 588: Label="Member"; break;
            case 828: Label="User"; break;
            case 840: Label="User"; break;
            case 868: Label="Number"; break;
            case 920: Label="Screenname"; break;
            case 924: Label="Fullname"; break;
            case 3160: Label="LogBy"; break; /* username */
            default:
                       PrintString=0;
                       /* Chatmsg lines have TWO big integers that
                          look like timestamps.  The first is really
                          a timestamp.  The second is the log ID number. */
                       if (!Time && (Number > 1000000000))
                       {
                         if (First) { printf(" %c ",Divider); }
                         Time = Number;
                         TimeS = gmtime(&Time);
                         printf("%04d-%02d-%02d %02d:%02d:%02d",
                             TimeS->tm_year+1900, TimeS->tm_mon+1,
                             TimeS->tm_mday, TimeS->tm_hour,
                             TimeS->tm_min, TimeS->tm_sec);
                         First=1;
                       }
                       else if (Time && (Number > 1000000000))
                       {
                         if (First) { printf(" %c ",Divider); }
                         printf("LogId: %ld",(long)Number);
                       }
                       break;
          } /* switch */

          /* Print the string */
          if (PrintString && First) { printf(" %c ",Divider); }
          if (Verbose > 1) printf(" [%ld] ",(long)Number);
          if (PrintString && Label) { printf("%s: ",Label); }
          while((Index < RecordEnd) && (Memory[Index] > 0x03))
          {
            if (PrintString && isprint(Memory[Index]))
            {
              fputc(Memory[Index],stdout);
              First=1;
            }
            else
            {
              if (Verbose > 2) { printf(" *%02x*",Memory[Index]); }
            }
            Index++;
          }
        }
      } /* while reading record */
      if (First || Verbose) { printf("\n"); }
      if (BlankFlag && (First || Verbose)) { printf("\n"); }
    } /* while records exist */
    SkypeCloseFile(argv[optind]);
  } /* for each file */
  return(0);
} /* main() */

