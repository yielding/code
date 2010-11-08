package wdl;

import javax.servlet.*;
import javax.servlet.http.*;
import java.util.*;
import java.io.*;

public class WCMultipartFormElement 
{
  private boolean m_bFile = false;
  private String fieldName = null;
  private String fullPathName = null;
  private String m_sFileName = null;
  private String fileExtension = null;
  private String pathName = null;
  private String contentType = null;
  private String contentDisposition = null;
  private String mimeType = null;
  private String mimeSubType = null;
  private String m_sValue = null;
  public WCMultipartFormParser m_oParser;

  public WCMultipartFormElement(WCMultipartFormParser oParser) 
  {
    setMultipartFormParser(oParser);
    m_sValue = null;
    m_rUserData = new WCProperties();
    m_oFileOutputStream = null;
  }

  public void setMultipartFormParser(WCMultipartFormParser oParser)
  {
    m_oParser = oParser;
  }

  public WCMultipartFormParser getMultipartFormParser()
  {
    return m_oParser;
  }

  public FileOutputStream createTempFile()
  {
    try
    {		
      String sSourceFileName = getFileName();
      if (WCString.isEmpty(sSourceFileName))
      {
        return null;
      }
      java.io.File oFile = null;
      m_sTempFileName = getTempFile(sSourceFileName);
      oFile = new java.io.File(m_sTempFileName);
      m_oFileOutputStream = new FileOutputStream(oFile);
    }
    catch (Exception ex)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormElement.createTempFile "+ex.toString());
    }
    return m_oFileOutputStream;
  }

  public void closeTempFile()
  {
    try
    {
      if (m_oFileOutputStream != null)
      {
        m_oFileOutputStream.close();
      }
      m_oFileOutputStream = null;
    }
    catch (Exception ex)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormElement.closeTempFile "+ex.toString());
    }
  }

  public FileOutputStream getTempFileOutputStream()
  {
    FileOutputStream oRet = null;
    oRet = m_oFileOutputStream;
    return oRet;
  }	

  public void setHeader(String headerString) 
  {
    try
    {
      if (headerString == null)
        return;
      this.m_bFile = headerString.indexOf("filename=\"") > 0;
      int startPosition = headerString.indexOf("name=\"") + "name=\"".length();
      int endPosition = headerString.indexOf("\"", startPosition);
      if( (startPosition > 0) && (endPosition > 0) )
      {
        this.fieldName = headerString.substring(startPosition, endPosition);
      }
      if (this.isFile())
      {
        startPosition = headerString.indexOf("filename=\"") + "filename=\"".length();
        endPosition = headerString.indexOf("\"", startPosition);
        if( (startPosition > 0) && (endPosition > 0) )
        {
          this.fullPathName = headerString.substring(startPosition, endPosition);
        }
        startPosition = headerString.indexOf("Content-Type: ") + "Content-Type: ".length();
        if( startPosition > 0 )
        {
          this.contentType = headerString.substring(startPosition);
        }
        startPosition = headerString.indexOf("Content-Disposition: ") + "Content-Disposition: ".length();
        endPosition = headerString.indexOf(";", startPosition);
        this.contentDisposition = headerString.substring(startPosition, endPosition);
        if ((startPosition = this.fullPathName.lastIndexOf(47)) > 0)
        {
          this.m_sFileName = this.fullPathName.substring(startPosition + 1);
        }
        else if((startPosition = this.fullPathName.lastIndexOf(92)) > 0)
        {
          this.m_sFileName = this.fullPathName.substring(startPosition + 1);
        }
        else 
        {
          this.m_sFileName = this.fullPathName;
        }
        if((startPosition = this.m_sFileName.lastIndexOf(46)) > 0 )
        {
          this.fileExtension = this.m_sFileName.substring(startPosition+1);
        }
        else
        {
          this.fileExtension = "";
        }
        if((startPosition = this.contentType.indexOf("/")) > 0)
        {
          this.mimeType = this.contentType.substring(0,startPosition);
          this.mimeSubType = this.contentType.substring(startPosition+1);
        }
        else
        {
          this.mimeType = this.contentType;
          this.mimeSubType = this.contentType;
        }
      }
    }
    catch (Exception ex)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormElement.WCMultipartFormElement "+ex.toString());
    }
  }

  public boolean isFile() 
  {
    return this.m_bFile;
  }

  public boolean isValidFile()
  {
    if (this.m_bFile == true && !WCString.isEmpty(getName()))
    {
      return true;
    }
    return false;
  }

  public String getFieldName() 
  {
    return this.fieldName;
  }

  public String getFullPathName() 
  {
    return this.fullPathName;
  }

  public String getFileName() 
  {
    return this.m_sFileName;
  }

  public String getName()
  {
    return getFileName();
  }

  public String getPathName()
  {
    return this.pathName;
  }

  public String getFileExtension() 
  {
    return this.fileExtension;
  }

  public String getContentType() 
  {
    return this.contentType;
  }

  public String getContentDisposition() 
  {
    return this.contentDisposition;
  }

  public String getMimeType() 
  {
    return this.mimeType;
  }

  public String getMimeSubType() 
  {
    return this.mimeSubType;
  }

  public void setValue(String sValue)
  {
    m_sValue = sValue;
  }

  public String getValue()
  {
    return m_sValue;
  }

  private int size = 0;
  private byte [] contents;
  public WCProperties m_rUserData = new WCProperties();

  public String m_sTempDirectory = "";
  public void setTempDirectory(String sVal)
  {
    m_sTempDirectory = sVal;
  }

  public String getTempDirectory()
  {
    if (WCString.isEmpty(m_sTempDirectory))
    {
      m_sTempDirectory = WCString.nullCheck(getEnvValue("tempdir"),getEnvValue("wdl.tempdir"));
      if (WCString.isEmpty(m_sTempDirectory))
      {
        String sPath = getHome();
        m_sTempDirectory = WCString.buildFilePath(sPath,"wdl/tmp","/");
      }
    }
    return m_sTempDirectory;
  }

  public String getHome()
  {
    String sHome = WCString.trim(getEnvValue("wdl.home"));
    if (WCString.isEmpty(sHome))
    {
      sHome = WCString.left(WCEnv.getInstance().getEnvFile(),"/WEB-INF");
    }
    return sHome;
  }

  public String getTempName()
  {
    try
    {
      java.util.Date dat = new java.util.Date();
      java.text.SimpleDateFormat formatter = new java.text.SimpleDateFormat ("yyyy");
      String sYear = formatter.format(dat);
      formatter = new java.text.SimpleDateFormat("MM");
      String sMonth = formatter.format(dat);
      formatter = new java.text.SimpleDateFormat("dd");
      String sDate = formatter.format(dat);
      formatter = new java.text.SimpleDateFormat("HH");
      String sHour = formatter.format(dat);
      formatter = new java.text.SimpleDateFormat("mm");
      String sMin = formatter.format(dat);
      formatter = new java.text.SimpleDateFormat("ss");
      String sSec = formatter.format(dat);			
      int nRand0 = (int)(Math.random()*10000);
      int nRand1 = (int)(Math.random()*10000);
      int nRand2 = (int)(Math.random()*10000);
      String sName = sYear+sMonth+sDate+sHour+sMin+sSec+"_"+nRand0+"_"+nRand1+"_"+nRand2;
      return sName;
    }
    catch (Exception ex)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormElement.getTempName "+ex.toString());
    }
    return "";
  }

  public String getTempFile(String sSourceFileName)
  {
    try
    {
      String sExt = WCString.nullCheck(WCString.getFileExt(sSourceFileName),"");
      String sName = getTempName();
      if (!WCString.isEmpty(sExt))
      {
        sName += "."+sExt;
      }
      String sDir = getTempDirectory();
      WCSystem oSys = WCSystem.getInstance();
      oSys.CreateDir(sDir);			
      String sRet = WCString.buildFilePath(sDir,sName,"/");
      return sRet;
    }
    catch (Exception ex)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormElement.getTempFile "+ex.toString());
    }
    return null;
  }

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

  public FileOutputStream m_oFileOutputStream = null;
  public String m_sTempFileName = null;

  public String getTempFileName()
  {
    return m_sTempFileName;
  }
  public int getSize() 
  {
    return this.size;
  }

  public void setSize(int size) 
  {
    this.size = size;
  }

  public byte [] getContents() 
  {
    return this.contents;
  }

  public byte getContentByte(int index) 
  {
    return this.contents[index];
  }

  public void setContents(byte[] contents) 
  {
    this.contents = contents;
  }

  public void setContentByte(int index, byte content) 
  {
    this.contents[index] = content;
  }

  public String m_sSaveFileName = null;
  public String getSaveFileName()
  {
    return m_sSaveFileName;
  }

  public void setSaveFileName(String sFileName)
  {
    m_sSaveFileName = sFileName;
  }

  public int saveFile(String sPathFile)
  {
    try
    {
      if (sPathFile == null)
        return -1;
      String sDir = WCString.getPath(sPathFile);
      WCSystem oSys = WCSystem.getInstance();
      oSys.CreateDir(sDir);
      String sSaveFileName = sPathFile;
      setSaveFileName(sSaveFileName);
      File oSrcFile = new File(getTempFileName());
      File oDestinationFile = new File(sSaveFileName);
      FileInputStream oFIS = null;
      FileOutputStream oFOS = null;
      try
      {
        oFIS = new FileInputStream(oSrcFile);
        oFOS = new FileOutputStream(oDestinationFile);
        byte[] oBuffer = new byte[1024 * 4];
        int nRead = 0;
        while ((nRead = oFIS.read(oBuffer)) != -1) 
        {
          oFOS.write(oBuffer, 0, nRead);
        }
        oFIS.close();
        oFOS.close();
        oFIS = null;
        oFOS = null;
        oSrcFile.delete();
      }
      catch (Exception e) 
      {
        WCLog.getInstance().printLog("exception at WCMultipartFormElement.saveFile "+e.toString());
        if (oFIS != null)
        {
          oFIS.close();
        }
        if (oFOS != null)
        {
          oFOS.close();
        }
        e.printStackTrace(); 
      }
    }
    catch(IOException ioe)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormElement.saveFile "+ioe.toString());
      ioe.printStackTrace();
    }
    return 0;
  }

  public WCProperties getUserData()
  {
    return m_rUserData;
  }

  public void setUserData(String sProp,String sValue)
  {
    if (m_rUserData == null)
      m_rUserData = new WCProperties();
    m_rUserData.setValue(sProp,sValue);
  }

  public void setUserData(String sProp,int nValue)
  {
    if (m_rUserData == null)
      m_rUserData = new WCProperties();
    m_rUserData.setValue(sProp,nValue);
  }

  public void deleteTempFile()
  {
    try
    {
      File oFile = new File(m_sTempFileName);
      boolean bSucc = oFile.delete(); 
    }
    catch (Exception ex)
    {
      WCLog.getInstance().printLog("exception at WCMultipartFormElement.deleteTempFile "+ex.toString());
    }
  }

  public String getCloneFileName()
  {
    String sFullPathName = this.getFullPathName();
    if (WCString.isEmpty(sFullPathName))
    {
      return null;
    }
    String sExt = WCString.getFileExt(sFullPathName);
    String sTempName = this.getTempName();
    String sTempFileName = sTempName+"."+sExt;
    return sTempFileName;
  }

  public long m_nFileSize = -1;
  public long getFileSize()
  {
    if (m_nFileSize > (long)0)
      return m_nFileSize;
    String sFileName = getSaveFileName();
    try
    {
      File oFile = new File(sFileName);
      if (oFile != null)
      {
        m_nFileSize = oFile.length();
        return m_nFileSize;
      }
    }
    catch (Exception ex)
    {
    }
    sFileName = m_sTempFileName;
    try
    {
      File oFile = new File(sFileName);
      if (oFile != null)
      {
        m_nFileSize = oFile.length();
        return m_nFileSize;
      }
    }
    catch (Exception ex)
    {
    }
    return 0;
  }
}
