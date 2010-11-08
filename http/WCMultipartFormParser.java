package wdl;

import javax.servlet.*;
import javax.servlet.http.*;
import java.util.*;
import java.io.*;

public class WCMultipartFormParser 
{
  public String m_sCharset;
  private HttpServletResponse resp;
  private ServletContext m_oApp;
  public Vector m_vElement = null;
  public WCProperties m_rParam = null;
  public WCPage m_oPage = new WCPage();

  public WCMultipartFormParser() 
  {
    refresh();
  }

  public WCMultipartFormParser(WCPage oPage) 
  {
    refresh();
    setPage(oPage);
  }

  public void init()
  {
    WCPage oPage = getPage();
    if (oPage == null)
      return;
    HttpServletRequest oRequest = (javax.servlet.http.HttpServletRequest)oPage.getRequest();
    parseContent(oRequest);
  }

  public void init(WCPage oPage)
  {
    setPage(oPage);
    init();
  }

  public int m_nContentLength = 0;
  public byte[] m_btBuf = null;
  public int m_nBufIdx = 0;
  public int m_nBoundaryLen = 0;
  public byte m_btBoundary[] = null;
  public int m_nBufReadSize = 0;
  public int m_nBufAllocSize = 1024*64;
  public int m_nCurrentBuffer = 0; 
  public int m_nBufferCount = 0;
  public int m_nAccumRead = 0;
  public InputStream m_oInputStream = null;
  public int m_nAccumBufIdx = 0;

  public String getEnvValue(String sProp)
  {
    WCEnv env = WCEnv.getInstance();
    if (env != null)
    {
      WCProperties rcd = env.getProperties();
      if (rcd != null)
      {
        String sVal = rcd.getStrValue(sProp);
        return sVal;
      }
    }
    return "";
  }

  public void refresh()
  {
    m_vElement = new Vector();
    m_rParam = new WCProperties();
    String sSrcCharset = getEnvValue("charset"); 
    if (WCString.isEmpty(sSrcCharset))
    {
      sSrcCharset = "KSC5601"; 
    }
    m_sCharset = sSrcCharset;
    m_nContentLength = 0;
    m_btBuf = null;
    m_nBufIdx = 0;
    m_nBoundaryLen = 0;
    m_btBoundary = null;
    m_nBufReadSize = 0;
    m_nBufAllocSize = 1024*64;
    m_nCurrentBuffer = 0; 
    m_nAccumRead = 0;
    m_oInputStream = null;
    m_nAccumBufIdx = 0;
    m_nBufferCount = 0;
    m_nPrevAccumBufIdx = 0;
  }
  public byte[] setByte(byte btBuf[],int nIdx,byte btChar)
  {
    {
      int nSize = 0;
      if (btBuf != null)
        nSize = btBuf.length;
      if (nIdx >= nSize)
      {
        int nMax = nIdx+32;
        byte btNew[] = new byte[nMax];
        for (int i=0;i<nMax;i++)
        {
          btNew[i] = 0;
        }
        for (int i=0;i<nSize;i++)
        {
          btNew[i] = btBuf[i];
        }
        btNew[nIdx] = btChar;
        btBuf = btNew;
      }
      else
      {
        btBuf[nIdx] = btChar;
      }
      return btBuf;
    }
  }

  public byte[] m_btDebugBuf = null;
  public byte[] getDebugBuf()
  {
    return m_btDebugBuf;
  }

  public byte[] append(byte btBuf[],byte btAppend[],int nLen)
  {
    try
    {
      if (btAppend == null)
        return null;
      if (btBuf == null)
      {
        btBuf = new byte[nLen];
      }
      int nSize = btBuf.length;
      int nNewSize = nSize+nLen;
      byte btNew[] = new byte[nNewSize];
      for (int i=0;i<nNewSize;i++)
      {
        btNew[i] = 0;
      }
      for (int i=0;i<nSize;i++)
      {
        btNew[i] = btBuf[i];
      }
      for (int i=0;i<nLen;i++)
      {
        btNew[i+nSize] = btAppend[i];
      }
      btBuf = btNew;
      return btBuf;
    }
    catch (Exception ex)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormParser.append "+ex.toString());
    }
    return null;
  }

  public byte[] copyBytes(byte btSrc[],int nStartIdx,int nEndIdx)
  {
    try
    {
      int nLen = nEndIdx-nStartIdx;
      if (nLen <= 0)
        return null;
      int nAlloc = nLen;
      byte btNew[] = new byte[nAlloc];
      for (int i=0;i<nAlloc;i++)
      {
        btNew[i] = 0;
      }
      for (int i=0;i<nLen;i++)
      {
        btNew[i] = btSrc[i+nStartIdx];
      }
      return btNew;
    }
    catch (Exception ex)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormParser.copyBytes "+ex.toString());
    }
    return null;
  }

  public void parseContent(HttpServletRequest oRequest) 
  {
    try
    {
      Vector vDataHeader = parseContentEx(oRequest);
    }
    catch(Exception crfe)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormParser.parseContent "+crfe.toString());
      crfe.printStackTrace();
    }
  }

  public void setResponse(HttpServletResponse resp) 
  {
    this.resp = resp;
  }

  public void setContext(ServletContext oApp) 
  {
    this.m_oApp = oApp;
  }

  public boolean findBoundary()
  {
    try
    {
      m_btBoundary = null;
      m_nBoundaryLen = 0;
      int nLen = 0;
      m_btBoundary = setByte(m_btBoundary,nLen,(byte)13); 
      nLen ++;
      m_btBoundary = setByte(m_btBoundary,nLen,(byte)10); 
      nLen ++;
      boolean bBoundaryFound = false;
      while (isEof() != true)
      {
        byte btByte = getByte();
        if (btByte == 13)
        {
          bBoundaryFound = true;
          m_nBoundaryLen = nLen;
          getByte();
          break;
        }
        m_btBoundary = setByte(m_btBoundary,nLen,btByte);
        nLen ++;
      }
      if (bBoundaryFound == true)
      {
        m_btBoundary = copyBytes(m_btBoundary,0,nLen);
        return true;
      }
    }
    catch (Exception ex)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormParser.findBoundary "+ex.toString());
    }
    return false;
  }

  public String findHeader()
  {
    try
    {
      int nLen = 0;
      byte btHeader[] = null;
      boolean bFound = false;
      byte btByte = 0;
      byte btByte2 = 0;
      while (isEof() != true)
      {
        btByte = getByte();
        btByte2 = getByte(); 
        btByte2 = getByte(); 
        putBackByte();
        putBackByte();
        if (btByte == 13 && btByte2 == 13) 
        {
          getByte();
          getByte();
          getByte(); 
          bFound = true;
          break;
        }
        btHeader = setByte(btHeader,nLen,btByte);
        nLen ++;
      }
      btHeader = copyBytes(btHeader,0,nLen);
      String sDataHeader = new String(btHeader,m_sCharset); 
      return sDataHeader;	
    }
    catch (Exception ex)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormParser.findHeader "+ex.toString());
    }
    return null;
  }

  public boolean isBoundary()
  {
    try
    {
      int boundaryKeyPosition = 0;
      boundaryKeyPosition = 0;
      boolean binaryEndFound = false;
      binaryEndFound = false;
      byte btByte = (byte)0;
      int nLen = 0;
      while (isEof() != true)
      {
        btByte = getByte();
        nLen ++;
        if (btByte == m_btBoundary[boundaryKeyPosition])
        {
          if (boundaryKeyPosition == m_nBoundaryLen-1 )
          {
            binaryEndFound = true;
            getByte(); 
            getByte(); 
            break;
          }
          boundaryKeyPosition++;
        }
        else
        {
          for (int i=0;i<nLen;i++)
          {
            putBackByte();
          }
          break;
        }
      }
      return binaryEndFound;
    }
    catch (Exception ex)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormParser.isBoundary "+ex.toString());
    }
    return false;
  }

  public void putBackByte()
  {
    m_nBufIdx --;
    m_nAccumBufIdx --;
    if (m_nBufIdx < 0)
    {
      m_nCurrentBuffer --;
      if (m_nCurrentBuffer < 0)
      {
      }
      m_nBufIdx = (m_nBufAllocSize/2) - 1;
    }
  }

  public boolean m_bDebug = false;
  public void setDebug(boolean bDebug)
  {
    m_bDebug = bDebug;
  }

  public boolean isDebug()
  {
    return m_bDebug;
  }

  public boolean isEof()
  {
    if (m_nAccumBufIdx < m_nContentLength)
    {
      return false;
    }
    return true;
  }

  public byte getByte() throws Exception
  {
    byte btData = getByteEx();
    setProgress(m_nAccumBufIdx);
    return btData;
  }

  public byte getByteEx() throws Exception
  {
    try
    {
      if (m_oInputStream == null)
      {
        m_oInputStream = m_oPage.getRequest().getInputStream();
      }
      if (m_btBuf == null)
      {
        m_btBuf = new byte[m_nBufAllocSize+32];
        if (m_btBuf == null)
        {
          return (byte)0;
        }
        for (int i=0;i<m_nBufAllocSize+32;i++)
        {
          m_btBuf[i] = (byte)0;
        }
      }
      if (m_nBufIdx >= m_nBufReadSize && m_nAccumRead < m_nContentLength && m_nBufferCount == m_nCurrentBuffer)
      {
        if (m_nBufReadSize == m_nBufAllocSize/2)
        {
          m_nBufReadSize = 0;
          m_nCurrentBuffer ++;
          m_nBufferCount ++;
        }
        int nMaxRead = m_nBufAllocSize/2 - m_nBufReadSize; 
        if (nMaxRead > m_nContentLength-m_nAccumRead)
        {
          nMaxRead = m_nContentLength-m_nAccumRead;
        }
        int nRead = m_oInputStream.read(m_btBuf
            ,(m_nCurrentBuffer%2)*(m_nBufAllocSize/2) + m_nBufReadSize
            ,nMaxRead);
        if (this.isDebug())
        {
          m_btDebugBuf = this.append(m_btDebugBuf,m_btBuf,nRead);
        }
        m_nBufReadSize += nRead;
        m_nAccumRead += nRead;
        if (m_nBufReadSize >= m_nBufAllocSize/2)
        {
        }
        m_nBufIdx = m_nBufIdx % (m_nBufAllocSize/2);
      }
      byte btByte = m_btBuf[(m_nCurrentBuffer%2)*(m_nBufAllocSize/2) + m_nBufIdx];
      m_nBufIdx ++;
      if (m_nBufIdx == m_nBufAllocSize/2 && m_nCurrentBuffer < m_nBufferCount)
      {
        m_nCurrentBuffer ++;
        m_nBufIdx = 0;
      }
      m_nAccumBufIdx ++;
      return btByte;
    }
    catch (Exception ex)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormParser.getByteEx "+ex.toString());
      setProgress(m_nContentLength);
      Exception oEx = new Exception("exception at getByte");
      throw oEx;
    }
  }

  public int procContent()
  {
    try
    {
      if (m_oCurElement.isFile())
      {
        try
        {
          FileOutputStream oFileStream = m_oCurElement.createTempFile();
          if (oFileStream == null)
          {
            return -1;
          }
          int nBufSize = 1024*4;
          byte[] btBuf = new byte[nBufSize];
          int nLen = 0;
          byte btByte = 0;
          while (isEof() != true)
          {
            btByte = getByte();
            if (btByte == (byte)13) 
            {
              putBackByte();
              if (isBoundary() == true)
              {
                break;
              }
              btByte = getByte(); 
            }
            btBuf[nLen] = btByte;
            nLen ++;
            if (nLen >= nBufSize)
            {
              if (oFileStream != null)
              {
                oFileStream.write(btBuf,0,nBufSize);
              }
              nLen = 0;
            }
          }
          if (nLen > 0)
          {
            if (oFileStream != null)
            {
              oFileStream.write(btBuf,0,nLen);
            }
          }
          m_oCurElement.closeTempFile();
        }
        catch (Exception ex)
        {
          WCLog.getInstance().printLog("exception at WCMultipartFormParser.procContent "+ex.toString());
          m_oCurElement.closeTempFile();
          m_oCurElement.deleteTempFile();
          return -1;
        }
      }
      else
      {
        byte btContent[] = null;
        int nLen = 0;
        byte btByte = 0;
        while (isEof() != true)
        {
          if (isBoundary() == true)
            break;
          btByte = getByte();
          btContent = setByte(btContent,nLen,btByte);
          nLen ++;
        }
        String sValue = null;
        if (nLen > 0)
        {
          btContent = copyBytes(btContent,0,nLen);
          sValue = new String(btContent,m_sCharset);
        }
        else
        {
          sValue = "";
        }
        String sProp = m_oCurElement.getFieldName();
        if (btContent != null)
        {
        }
        else
        {
        }
        if (m_rParam == null)
        {
          m_rParam = new WCProperties();
        }
        m_rParam.addValue(sProp,sValue);
        m_oCurElement.setValue(sValue);
      }
    }
    catch (Exception ex)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormParser.procContent "+ex.toString());
      return -1;
    }
    return 0;
  }

  public WCMultipartFormElement m_oCurElement = null;
  public Vector parseContentEx(HttpServletRequest oRequest) 
  {
    int nLen = oRequest.getContentLength();
    setContentLength(nLen);
    m_nBufIdx = 0;
    m_btBoundary = null;
    m_nBoundaryLen = 0;
    int nState = 0; 
    try
    {
      m_nBufReadSize = 0;
      int nRead = 0;
      m_oInputStream = oRequest.getInputStream();
      while (isEof() != true)
      {
        if (nState == 0)
        {
          findBoundary();
          nState = 100;
        }
        else if (nState == 100)
        {
          String sHeader = findHeader();
          if (sHeader != null)
          {
            nState = 200;
            m_oCurElement = new WCMultipartFormElement(this);
            m_oCurElement.setHeader(sHeader);
            if (m_vElement == null)
              m_vElement = new Vector();
            m_vElement.addElement(m_oCurElement);
          }
          nState = 200;
        }
        else if (nState == 200)
        {
          int nRet = procContent();
          if (nRet < 0)
          {
          }
          nState = 100;
        }
      }
    }
    catch(IOException ioe)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormParser.parseContentEx "+ioe.toString());
      ioe.printStackTrace();
    }
    setProgress(m_nAccumBufIdx);
    return m_vElement;
  }

  public Vector getElements()
  {
    return m_vElement;
  }

  public String getFileName()
  {
    Vector vElement = getElements();
    int nCount = WCVector.size(vElement);
    for (int i=0;i<nCount;i++)
    {
      WCMultipartFormElement oElmt = (WCMultipartFormElement)WCVector.getObjAt(vElement,i);
      if (oElmt == null)
        continue;
      if (oElmt.isFile())
      {
        return oElmt.getName();
      }
    }
    return null;
  }

  public void deleteTempFile()
  {
    Vector vElement = getElements();
    int nCount = WCVector.size(vElement);
    for (int i=0;i<nCount;i++)
    {
      WCMultipartFormElement oElmt = (WCMultipartFormElement)WCVector.getObjAt(vElement,i);
      if (oElmt == null)
        continue;
      if (oElmt.isFile())
      {
        oElmt.deleteTempFile();
      }
    }
  }

  public String getPathName()
  {
    Vector vElement = getElements();
    int nCount = WCVector.size(vElement);
    for (int i=0;i<nCount;i++)
    {
      WCMultipartFormElement oElmt = (WCMultipartFormElement)WCVector.getObjAt(vElement,i);
      if (oElmt == null)
        continue;
      if (oElmt.isFile())
      {
        return oElmt.getPathName();
      }
    }
    return null;
  }

  public int getFileCount()
  {
    try
    {
      int nRet = 0;
      Vector vElement = getElements();
      int nCount = WCVector.size(vElement);
      for (int i=0;i<nCount;i++)
      {
        WCMultipartFormElement oElmt = (WCMultipartFormElement)WCVector.getObjAt(vElement,i);
        if (oElmt == null)
          continue;
        if (oElmt.isFile())
        {
          nRet ++;
        }
      }
      return nRet;
    }
    catch (Exception ex)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormParser.getFileCount "+ex.toString());
    }
    return 0;
  }

  public void setCharset(String sCharset)
  {
    m_sCharset = sCharset;
  }		

  public String getCharset()
  {
    return m_sCharset;
  }

  public WCProperties getParam()
  {
    if (m_rParam == null)
      m_rParam = new WCProperties();
    return m_rParam;
  }

  public WCPage getPage()
  {
    return m_oPage;
  }

  public void setPage(WCPage oPage)
  {
    m_oPage = oPage;
  }

  public int getContentLength()
  {
    return m_nContentLength;
  }

  public int getRead()
  {
    return m_nAccumBufIdx;
  }

  public void setContentLength(int nLen)
  {
    m_nContentLength = nLen;
    if (m_oPage != null)
    {
      String sLen = new String(""+m_nContentLength);
      HttpSession oSession = m_oPage.getSession();
      if (oSession != null)
      {
        oSession.putValue("wdl.WCMultipartFormParser.contentLength",sLen);
      }
    }
  }

  public int m_nPrevAccumBufIdx = 0;
  public void setProgress(int nParam)
  {
    int nRead = nParam;  
    m_nAccumBufIdx = nRead;
    if (m_nAccumBufIdx - m_nPrevAccumBufIdx < 1024 && (m_nAccumBufIdx < m_nContentLength))
    {
      return;
    }
    m_nPrevAccumBufIdx = m_nAccumBufIdx;
    if (m_oPage != null)
    {
      String sLen = new String(""+m_nAccumBufIdx);
      HttpSession oSession = m_oPage.getSession();
      if (oSession != null)
      {
        oSession.putValue("wdl.WCMultipartFormParser.read",sLen);
      }
    }
  }		

  public void saveFile(String sDir)
  {
    try
    {
      Vector vElement = this.getElements();
      int nCount = WCVector.size(vElement);
      for (int i=0;i<nCount;i++)
      {
        WCMultipartFormElement oElmt = (WCMultipartFormElement)WCVector.getObjAt(vElement,i);
        if (oElmt == null)
        {
          continue;
        }
        if (!oElmt.isFile())
        {
          continue;
        }
        String sTempFileName = oElmt.getCloneFileName();
        if (WCString.isEmpty(sTempFileName))
        {
          continue;
        }
        String sTempPathFileName = WCString.buildFilePath(sDir,sTempFileName,"/");
        oElmt.saveFile(sTempPathFileName);
      }
    }
    catch (Exception ex)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormParser.saveFile "+ex.toString());
    }
  }		
}
