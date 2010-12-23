// pipe_test.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include <windows.h>
#include <process.h>
#include <string>
#include <iostream>

using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class IPhonePhysicalExtractor
{
public:
    IPhonePhysicalExtractor(wstring const& bs = L"4096KB")
        : BlockSize_(bs)
    {
        hProgressRd_     = 0;
        hProgressWr_     = 0;
        hExtractThread_  = 0;
        hMonitorThread_  = 0;
    }

    ~IPhonePhysicalExtractor()
    {
        if (hExtractThread_) CloseHandle(hExtractThread_);
        if (hMonitorThread_) CloseHandle(hMonitorThread_);
    }

public:
    bool StartExtract(wstring const& extractCommand=L"")
    {
        if (extractCommand.empty())
            return false;

        ExtractCommand_ = extractCommand;
        hExtractThread_ = (HANDLE)_beginthreadex(0, 0, ExtractThread, (void*)this, 0, 0);
        return (hExtractThread_ != 0);
    }

    bool StartMonitor(wstring const& monitorCommand=L"")
    {
        if (monitorCommand.empty())
            return false;

        MonitorCommand_ = monitorCommand; 
        hMonitorThread_ = (HANDLE)_beginthreadex(0, 0, ProgressThread, (void*)this, 0, 0);
        return (hMonitorThread_ != 0);
    }

protected:
    static unsigned __stdcall ExtractThread(void* pThis)
    {
        SECURITY_ATTRIBUTES saAttr  = { 0 }; 
        saAttr.nLength              = sizeof(SECURITY_ATTRIBUTES); 
        saAttr.bInheritHandle       = TRUE; 
        saAttr.lpSecurityDescriptor = NULL; 

        // Create a pipe for the child process's STDOUT. 
        HANDLE hChildStdOutRd = 0;
        HANDLE hChildStdOutWr = 0;
        if (!CreatePipe(&hChildStdOutRd, &hChildStdOutWr, &saAttr, 0)) 
            return false;

        // Ensure the read handle to the pipe for STDOUT is not inherited.
        // if (!SetHandleInformation(hChildStdOutRd, HANDLE_FLAG_INHERIT, 0))
        //     return false;

        // Create a pipe for the child process's STDOUT. 
        HANDLE hChildStdInRd = 0;
        HANDLE hChildStdInWr = 0;
        if (!CreatePipe(&hChildStdInRd, &hChildStdInWr, &saAttr, 0)) 
            return false;

        // Ensure the write handle to the pipe for STDIN is not inherited.
        // if (!SetHandleInformation(hChildStdInWr, HANDLE_FLAG_INHERIT, 0))
        //     return false;

        PROCESS_INFORMATION processInfo = { 0 };
        STARTUPINFO         startupInfo = { 0 };

        startupInfo.cb          = sizeof(startupInfo);
        //startupInfo.hStdError   = hChildStdOutWr;
        startupInfo.hStdOutput  = hChildStdOutWr;
        startupInfo.hStdInput   = hChildStdInRd;
        startupInfo.dwFlags     = STARTF_USESTDHANDLES; //STARTF_USESTDHANDLES;
        startupInfo.wShowWindow = SW_HIDE; // SW_HIDE, SW_SHOW;

        // wstring cmdArgs = L"ping.exe -t localhost ";

        IPhonePhysicalExtractor* pExtractor = (IPhonePhysicalExtractor*)pThis;
        if (!CreateProcess(L"C:\\tmp\\ssh.exe",
                LPWSTR(pExtractor->ExtractCommand_.c_str()),
                NULL, NULL, TRUE, CREATE_NO_WINDOW, NULL, NULL, // 나중에 CREATE_NO_WINDOW, CREATE_NEW_CONSOLE
                &startupInfo,
                &processInfo))
            return false;

        if (!CloseHandle(hChildStdOutWr))
            cout << "error\n";

        for (;;)
        {
            DWORD res = ::WaitForSingleObject(processInfo.hProcess, 1000);
            if (res == WAIT_OBJECT_0)
                break;

            if (res == WAIT_TIMEOUT)
            {
                char buf[4097]  = {0};
                DWORD readBytes = 0;
                DWORD available = 0;

//                 PeekNamedPipe(hChildStdOutRd, 0, 0, 0, &available, 0);
//                 PeekNamedPipe(hChildStdInWr, 0, 0, 0, &available, 0);

                if (!PeekNamedPipe(hChildStdOutRd, 0, 0, 0, &available, 0)) 
                    continue;

                if (available == 0)
                    continue;
                
                if (!ReadFile(hChildStdOutRd, buf, 4096, &readBytes, 0)) 
                {
                    // data는 있는데 read를 실패한 경우
                    break;
                }

                if (readBytes == 0)
                {
                    // data가 있고 read를 성공했는데 읽은 데이타가 없는 경우
                    break;        
                }

                cout << std::string(buf) << endl;
                cout.flush();
            }
        }

        CloseHandle(processInfo.hThread);
        CloseHandle(processInfo.hProcess);
        CloseHandle(hChildStdOutRd);
        CloseHandle(hChildStdOutWr);
        CloseHandle(hChildStdInRd);
        CloseHandle(hChildStdInWr);

        return 0;
    }


    static unsigned __stdcall ProgressThread(void* pThis)
    {
        return 0;
    }

private:
    std::wstring BlockSize_;
    std::wstring MonitorCommand_;
    std::wstring ExtractCommand_;

private:
    HANDLE hExtractThread_;
    HANDLE hMonitorThread_;

    HANDLE hProgressRd_;
    HANDLE hProgressWr_;
};

int _tmain(int argc, _TCHAR* argv[])
{
    IPhonePhysicalExtractor ext;
    ext.StartExtract(L"ssh.exe  yielding@192.168.0.124");
    //ext.StartExtract(L"C:\\Windows\\system32\\ping.exe -t localhost");

    for (size_t i=0; i<1000; i++)
    {
        Sleep(100000);
    }

    return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
