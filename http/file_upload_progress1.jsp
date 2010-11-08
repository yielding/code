<%@ page language="java" import="wdl.*,java.util.*,java.sql.*,java.lang.*,java.io.*,java.io.File " contentType="text/html; charset=EUC-KR"%>
<%
	WCPage oPage = new WCPage();
	if (oPage.initCtrl(pageContext) > 0)
	{
		return;
	}
%>
<%@ include file="/wdl/src/java/WCMultipartFormParser.java"%>
<%@ include file="/wdl/src/java/WCMultipartFormElement.java"%>
<%!
public class WCPage
{
	public int initCtrl(javax.servlet.jsp.PageContext oPageContext)
	{
		return initCtrl(
			(javax.servlet.http.HttpServletRequest)oPageContext.getRequest()
			,(javax.servlet.http.HttpServletResponse)oPageContext.getResponse()
			,(javax.servlet.jsp.JspWriter)oPageContext.getOut()
			,(javax.servlet.http.HttpSession)oPageContext.getSession()
			,(javax.servlet.ServletContext)oPageContext.getServletContext()
			,oPageContext
			,(javax.servlet.ServletConfig)oPageContext.getServletConfig()
			);
	}
	public int initCtrl(
		javax.servlet.http.HttpServletRequest oRequest
		,javax.servlet.http.HttpServletResponse oResponse
		,javax.servlet.jsp.JspWriter oOut
		,javax.servlet.http.HttpSession oSession
		,javax.servlet.ServletContext oApplication
		,javax.servlet.jsp.PageContext oPageContext
		,javax.servlet.ServletConfig oConfig)
	{
		try
		{
			m_request = oRequest;
			m_response = oResponse;
			m_out = oOut;
			m_session = oSession;
			m_application = oApplication;
			m_pageContext = oPageContext;
			m_config = oConfig;
			
			if (isMultipart())
			{
				WCMultipartFormParser oFormParser = new WCMultipartFormParser(this);
				oFormParser.setDebug(true);
				oFormParser.init();
				byte[] btDebugBuf = oFormParser.getDebugBuf();
				String sDebug = new String(btDebugBuf);
				this.printOut("<pre>");
				this.printOut(sDebug);
				this.printOut("</pre>");
				
				WCProperties rParam = oFormParser.getParam();
				this.printOut(rParam.serializeOut());
				
				String sDir = "c:/tmp3";
				oFormParser.saveFile(sDir);
				return 1;
			}
		}
		catch (Exception ex)
		{
		}
		return 0;
	}
	public javax.servlet.http.HttpServletRequest m_request = null; 
	public javax.servlet.http.HttpServletResponse m_response = null; 
	public javax.servlet.jsp.JspWriter m_out = null; 
	public javax.servlet.http.HttpSession m_session = null; 
	public javax.servlet.ServletContext m_application = null; 
	public javax.servlet.jsp.PageContext m_pageContext = null; 
	public javax.servlet.ServletConfig m_config = null; 
	
	public javax.servlet.http.HttpSession getSession()
	{
		return m_session;
	}
	public javax.servlet.http.HttpServletRequest getRequest()
	{
		return m_request;
	}
	public javax.servlet.jsp.PageContext getPageContext()
	{
		return m_pageContext;
	}

	public boolean isMultipart()
	{
		HttpServletRequest oRequest = (javax.servlet.http.HttpServletRequest)m_request;
		String content_type = oRequest.getHeader("content-type");
		if (WCString.indexOf(WCString.toUpper(content_type),"MULTIPART") == 0)
		{
			return true;
		}
		return false;
	}
	public void printOut(String sData)
	{
		try
		{
			if (m_out != null)
			{
				m_out.write(sData);
			}
		}
		catch (IOException ex)
		{
		}
	}
}
%>

<form name="fParam" encType="multipart/form-data">
<input type="hidden" name="_cmd" value="file_data">
<input type="file" name="file">
</form>

<a href="javascript:uploadFile();">upload the file</a>

<iframe id="ifrmAction" name="ifrmAction" width="0" height="0"></iframe> 

<script>
function uploadFile()
{
	var fParam = window.document.fParam;
	fParam._cmd.value = "uploadFile";
	fParam.method = "post";
	fParam.target = "_blank";
	fParam.submit();
}
</script>