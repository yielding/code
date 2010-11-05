#include <ace/Log_Msg.h>
#include <ace/INET_Addr.h>
#include <ace/OS.h>
#include <ace/Proactor.h>
#include <ace/WIN32_Proactor.h>

class PortScanner : public ACE_Handler
{
public: 
  PortScanner() { } 

  int open(ACE_Proactor& proactor, const ACE_INET_Addr& remote_addr) 
  {  
    this->proactor(&proactor);  
    this->remote_addr_.set(remote_addr);  
    if (connect_.open(*this, ACE_INVALID_HANDLE, 0, this->proactor()) < 0)  
    {   
      return -1;  
    }  
    return connect_.connect(ACE_INVALID_HANDLE, remote_addr_, ACE_Addr::sap_any, 1); 
  }

  void handle_connect(const ACE_Asynch_Connect::Result &result) 
  {
    //This handler will be called if connection is established or RST is received,  
    //You may no get any response in a timely manner even if there is no firewall.  
    ACE_ASSERT(result.connect_handle () != ACE_INVALID_HANDLE);  
    if (result.success())
    {   
      //Connection is established 
      ACE_DEBUG((LM_NOTICE, "%s:%d is open\n",        
            remote_addr_.get_host_addr(),        
            remote_addr_.get_port_number()));  
    }  
    else 
    {   
      //Got a RST  
    } 
  }
private: 
  ACE_INET_Addr remote_addr_; 
  ACE_Asynch_Connect connect_;
};

int ACE_TMAIN(int /*argc*/, ACE_TCHAR** /*argv*/)
{ 
  ACE_Proactor proactor; 
  ACE_INET_Addr scan_target("127.0.0.1:0"); 
  // Asynchronous connection are simulated on Both Windows and Unix 
  // (C++NP v2 page 283, sidebar 57 is inaccurate). 
  // Since ACE_Select_reactor is used internally, the handles limit 
  // applies. You can either increase the limit(1024 by default), or 
  // simply do not register too much handles simultaneously. 

  unsigned short start_port = 1; 
  unsigned short end_port = 1024; 
  ACE_ASSERT(end_port > start_port); 
  const size_t size = end_port - start_port + 1; //'size' is not const expression so we can not create the array on stack 
  PortScanner* portscanners = new PortScanner[size]; 
  for (unsigned short i = start_port; i < end_port; ++i) 
  {  
    scan_target.set_port_number(i);  
    portscanners[i].open(proactor, scan_target); 
  } 
  // Run event loop for 10 seconds 
  ACE_DEBUG((LM_NOTICE, ACE_TEXT("Portscan started\n"))); 
  ACE_Time_Value timeout(30); 
  proactor.proactor_run_event_loop(timeout); 
  delete[] portscanners; 
  return 0;
}
