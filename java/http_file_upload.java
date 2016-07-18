package FileManager;
 
import java.io.BufferedOutputStream;
import java.io.DataOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
 
public class UploadTest {
    public static void main(String[] args) {
        String filePath = "c:/1.jpg";
 
        try {
            upload(filePath);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
 
    public static void upload(String filePath) throws IOException {
        String url = "http://localhost:8080/web/Uploader";
 
        // 데이터 구분문자. 아무거나 정해도 상관없지만 꼭 나타날 수 없는 형태의 문자열로 정한다.
        String boundary = "^******^";
 
        // 데이터 경계선
        String delimiter = "\r\n--" + boundary + "\r\n";
 
        StringBuffer postDataBuilder = new StringBuffer();
 
        // 추가하고 싶은 Key & Value 추가
        // key & value를 추가한 후 꼭 경계선을 삽입해줘야 데이터를 구분할 수 있다.
        postDataBuilder.append(delimiter);
        postDataBuilder.append(setValue("type", "image"));
        postDataBuilder.append(delimiter);
        postDataBuilder.append(setValue("name", "이지형"));
        postDataBuilder.append(delimiter);
 
        // 파일 첨부
        postDataBuilder.append(setFile("uploadedFile", "temp.jpg"));
        postDataBuilder.append("\r\n");
 
        // 커넥션 생성 및 설정
        HttpURLConnection conn = (HttpURLConnection) new URL(url)
                .openConnection();
        conn.setDoInput(true);
        conn.setDoOutput(true);
        conn.setUseCaches(false);
        conn.setRequestMethod("POST");
        conn.setRequestProperty("Connection", "Keep-Alive");
        conn.setRequestProperty("Content-Type", "multipart/form-data;boundary=" + boundary);
 
        // 전송 작업 시작
        FileInputStream   in = new FileInputStream(filePath);
        DataOutputStream out = new DataOutputStream(new BufferedOutputStream(conn.getOutputStream()));
 
        // 위에서 작성한 메타데이터를 먼저 전송한다. (한글이 포함되어 있으므로 UTF-8 메소드 사용)
        out.writeUTF(postDataBuilder.toString());
 
        // 파일 복사 작업 시작
        int maxBufferSize = 1024;
        int bufferSize = Math.min(in.available(), maxBufferSize);
        byte[] buffer = new byte[bufferSize];
 
        // 버퍼 크기만큼 파일로부터 바이트 데이터를 읽는다.
        int byteRead = in.read(buffer, 0, bufferSize);
 
        // 전송
        while (byteRead > 0) {
            out.write(buffer);
            bufferSize = Math.min(in.available(), maxBufferSize);
            byteRead = in.read(buffer, 0, bufferSize);
        }
 
        out.writeBytes(delimiter); // 반드시 작성해야 한다.
        out.flush();
        out.close();
        in.close();
 
        // 결과 반환 (HTTP RES CODE)
        conn.getInputStream();
        conn.disconnect();
    }
 
    /**
     * Map 형식으로 Key와 Value를 셋팅한다.
     * @param key : 서버에서 사용할 변수명
     * @param value : 변수명에 해당하는 실제 값
     * @return
     */
    public static String setValue(String key, String value) {
        return "Content-Disposition: form-data; name=\"" + key + "\"r\n\r\n"
                + value;
    }
 
    /**
     * 업로드할 파일에 대한 메타 데이터를 설정한다.
     * @param key : 서버에서 사용할 파일 변수명
     * @param fileName : 서버에서 저장될 파일명
     * @return
     */
    public static String setFile(String key, String fileName) {
        return "Content-Disposition: form-data; name=\"" + key
                + "\";filename=\"" + fileName + "\"\r\n";
    }
}


protected void doPost(HttpServletRequest request,
    HttpServletResponse response) throws ServletException, IOException {
    request.setCharacterEncoding("UTF-8");
    response.setCharacterEncoding("UTF-8");
  
    int postMaxSize = 10 * 1024 * 1024;
    String folderPath = "../"; // 파일이 저장될 경로
    String encoding = "UTF-8";
  
    MultipartRequest mRequest = new MultipartRequest(request, folderPath,
            postMaxSize, encoding, new DefaultFileRenamePolicy());
  
    // 받은 데이터 출력
    String key, value;
    Enumeration<String> enumer = mRequest.getParameterNames();
    while (enumer.hasMoreElements()) {
        key = enumer.nextElement();
        value = mRequest.getParameter(key);
  
        System.out.println(key + " : " + value);
    }
  
    // 파일 이름 출력
    File file;
    File rename = new File("picture1.jpg");
    enumer = mRequest.getFileNames();
    while (enumer.hasMoreElements()) {
    key = enumer.nextElement();
        file = mRequest.getFile(key);
        System.out.println(key + " : " + file.getName());
          
        // 파일명 수정
        // renameTo() 메소드는 플랫폼 의존적 메소드이므로 제대로 동작이 되는지
        // 반드시 확인해야 한다. 만약 false가 반환된다면 복사 방식으로
        // 구현해야 한다. (퍼미션 관련)
        System.out.println("rename : " + file.renameTo(rename));
    }
}
