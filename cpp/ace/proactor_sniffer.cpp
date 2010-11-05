//Sniffer and Proactor example
#include <ace/Log_Msg.h>
#include <ace/INET_Addr.h>
#include <ace/OS.h>
#include <ace/Proactor.h>
#include <ace/WIN32_Proactor.h>

#include <mstcpip.h>//SIO_RCVALL

//ACE_NDEBUG will be defined if you define _DEBUG on windows.
#ifdef ACE_NDEBUG
#define ACE_VERIFY(X) ((void)(X))
#else
#define ACE_VERIFY(X) ACE_ASSERT(X)
#endif //ACE_NDEBUG

class Sniffer : public ACE_Handler
{
 static const size_t PACKET_SIZE = 65526;
public:
 Sniffer(ACE_Proactor& proactor)
  :socket_(ACE_INVALID_HANDLE)
  ,ACE_Handler(&proactor)
  ,mblk_(PACKET_SIZE)
 {
 }

 ACE_HANDLE handle() const
 {
  return socket_;
 }

 ~Sniffer()
 {
  if (ACE_INVALID_HANDLE != socket_)
  {
   ACE_VERIFY(ACE_OS::closesocket(socket_) == 0);
  }
 }

 /*
 MSDN On SIO_RCVALL:
 Enables a socket to receive all IP packets on the network. The socket handle passed to the
 WSAIoctl function must be of AF_INET address family, SOCK_RAW socket type, and IPPROTO_IP
 protocol. The socket also must be bound to an explicit local interface, which means that you cannot
 bind to INADDR_ANY. 
 Once the socket is bound and the ioctl set, calls to the WSARecv or recv functions return IP
 datagrams passing through the given interface. Note that you must supply a sufficiently large
 buffer. Setting this ioctl requires Administrator privilege on the local computer. SIO_RCVALL is
 available in Windows 2000 and later versions of Windows.
 */
 int open (const ACE_INET_Addr& listen_addr)
 {
  if (create_sniffer_socket(listen_addr) < 0)
  {
   return -1;
  }

  // register the event handle
  if (reader_.open(*this) < 0)
  {
   ACE_ERROR((LM_ERROR, ACE_TEXT("open: %m\n")));
   return -1;
  }

  ACE_DEBUG((LM_DEBUG, ACE_TEXT("Sniffer::open\n")));

  //initiate our first asynchronous read
  reader_.read(mblk_, PACKET_SIZE);

  return 0;
 }

 void handle_read_stream(const ACE_Asynch_Read_Stream::Result &result)
 {
  if (!result.success() || result.bytes_transferred() == 0)
  {
   ACE_ERROR((LM_ERROR, ACE_TEXT("read_streamhandle_read_stream error\n")));
   return;
  }

  ACE_ASSERT(&mblk_ == &result.message_block());

  ACE_HEX_DUMP((LM_NOTICE, mblk_.rd_ptr(), result.bytes_transferred()));

  //Reset the write pointer and read again
  mblk_.wr_ptr(mblk_.rd_ptr());
  reader_.read(mblk_, PACKET_SIZE);
 }

private:
 ACE_Asynch_Read_Stream reader_;
 ACE_HANDLE socket_;
 ACE_Message_Block mblk_;

 int create_sniffer_socket(const ACE_INET_Addr& listen_addr)
 {
  socket_ = ACE_OS::socket(AF_INET, SOCK_RAW, IPPROTO_IP);

  if (ACE_INVALID_HANDLE == socket_)
  {
   ACE_ERROR((LM_ERROR, ACE_TEXT("%p\n"), ACE_TEXT("socket() failed in Sniffer::open()")));
   return -1;
  }

  sockaddr* laddr = reinterpret_cast<sockaddr*>(listen_addr.get_addr());
  size_t size = listen_addr.get_size();
  ACE_VERIFY(ACE_OS::bind(socket_, laddr, size) == 0);

  DWORD YES = 1;
  ACE_VERIFY(ACE_OS::setsockopt(socket_, IPPROTO_IP, IP_HDRINCL, reinterpret_cast<const char*>(&YES), sizeof(YES)) == 0);

  DWORD dwBytesRet = 0 ;
  if (WSAIoctl((SOCKET)socket_, SIO_RCVALL, &YES, sizeof(YES), NULL, 0, &dwBytesRet, NULL, NULL) != 0)
  {
   ACE_ERROR((LM_ERROR, ACE_TEXT("%p\n"), ACE_TEXT("WSAIoctl() failed in Sniffer::open()")));
   return -1;
  }

  DWORD rcvtimeo = 1000 ; // 1 sec instead of 45(default)
  ACE_VERIFY(ACE_OS::setsockopt(socket_, SOL_SOCKET, SO_RCVTIMEO, reinterpret_cast<const char*>(&rcvtimeo) , sizeof(rcvtimeo)) == 0);

  return 0;
 }
};

int ACE_TMAIN(int /*argc*/, ACE_TCHAR** /*argv*/)
{
 ACE_WIN32_Proactor win32_proactor;
 ACE_Proactor proactor(&win32_proactor);

 //TODO bind to your local address!!!
 //Do not work with loopback address (127.0.0.1)
 ACE_INET_Addr addr("192.168.0.100:0");

 Sniffer sniffer(proactor);
 if (sniffer.open(addr) == -1)
 {
  ACE_ERROR((LM_ERROR, ACE_TEXT("Can not open to listen: %m\n")));
  return -1;
 }

 //Run event loop
 ACE_DEBUG((LM_NOTICE, ACE_TEXT("Sniffer started\n")));
 proactor.proactor_run_event_loop();
 return 0;
}