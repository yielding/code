import java.io.*;
import org.xml.sax.*;
import org.xml.sax.helpers.DefaultHandler;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;

public class SAX2Test extends DefaultHandler {
  static private Writer out;

  public static void main(String args[]) {
    if (args.length != 1) {
      System.err.println("Usage: cmd filename");
      System.exit(1);
    }

    DefaultHandler   handler = new SAX2Test();
    SAXParserFactory factory = SAXParserFactory.newInstance();
    try {
      out = new OutputStreamWriter(System.out, "UTF8");
      SAXParser saxParser = factory.newSAXParser();
      saxParser.parse(new File(args[0]), handler);
    } catch (Throwable t) {
      t.printStackTrace();
    }

    System.exit(0);
  }

  private void emit(String s) throws SAXException {
    try {
      out.write(s);
      out.flush();
    } catch (IOException e) {
      throw new SAXException("I/O error", e);
    }
  }

  private void nl() throws SAXException {
    String lineEnd =  System.getProperty("line.separator");
    try {
      out.write(lineEnd);

    } catch (IOException e) {
      throw new SAXException("I/O error", e);
    }
  }

  public void startDocument() throws SAXException {
    emit("<?xml version='1.0' encoding='UTF-8'?>");
    nl();
  }

  public void endDocument() throws SAXException {
    try {
      nl();
      out.flush();
    } catch (IOException e) {
      throw new SAXException("I/O error", e);
    }
  }

  public void startElement(String nsURI, String sName, String qName, Attributes attrs) 
    throws SAXException {
    String eName = sName;                // element name
    if ("".equals(eName)) eName = qName; // namesapceAware = false
    emit("<" + eName);
    if (attrs != null) {
      for (int i = 0; i < attrs.getLength(); i++) {
        String aName = attrs.getLocalName(i);
        if ("".equals(aName)) aName = attrs.getQName(i);
        emit(" ");
        emit(aName + "=\"" + attrs.getValue(i) + "\"");
      }
    }
    emit(">");
  }

  public void endElement(String nsURI, String sName, String qName) 
    throws SAXException {
    emit("</"+sName+">");
  }

  public void characters(char buf[], int offset, int len) 
    throws SAXException {
    String s = new String(buf, offset, len);
    emit(s);
  }
}
