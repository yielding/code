package com.gmdmfix.KakaoDecryptor;

import java.net.*;
import java.io.*;

import java.security.Security;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;

import com.Ostermiller.util.*;

import org.bouncycastle.jce.provider.BouncyCastleProvider;

class DecryptServer {
    ServerSocket   server = null;
    KakaoDecryptor decrypter = null;

    // DEBUG Point
    // String tmpCiperedText = "";
    // String tmpSalt = "";

    public DecryptServer(int port, KakaoDecryptor decrypter) {
        try {
            server = new ServerSocket(port);
            this.decrypter = decrypter;
        } catch(IOException e) {
        }
    }

    public boolean startServer() {
        if (server == null)
            return false;

        boolean shouldTerminate = false;
        while (true) {
            if (shouldTerminate)
                break;

            Socket client = null;
            DataInputStream in = null;
            DataOutputStream out = null;
            try {
                client = server.accept();
                in  = new DataInputStream (new BufferedInputStream (client.getInputStream()));
                out = new DataOutputStream(new BufferedOutputStream(client.getOutputStream()));

                while (true)
                {
                    byte header = in.readByte();
                    switch (header) {
                        case 0: shouldTerminate = true;        break;
                        case 1: processPingPacket(in, out);    break;
                        case 2: processDecryptPacket(in, out);  
                                // DEBUG Point
                                // System.out.println("Success!");
                                break;
                        case 5: processPCHDecryptPacket(in, out); 
                                // DEBUG Point
                                // System.out.println("Success!");
                                break;
                    }
                }

            } catch (Exception e) {
                // e.printStackTrace();
                // DEBUG Point
                // System.out.println("Error! data : " + tmpCiperedText + ", salt: " + tmpSalt);
                processErrorPacket(in, out, e.getMessage());
            } finally {
                try { client.close(); } catch(Exception ex) {}
            }
        }

        return true;
    }

    private void processErrorPacket(DataInputStream in, DataOutputStream out, String message) {
        try {
            byte[] bytes = message.getBytes("UTF-8");
            out.writeByte(4);
            out.writeInt(bytes.length);
            out.write(bytes, 0, bytes.length);
            out.flush();
        } catch(Exception e) {
        }
    }

    private void processPingPacket(DataInputStream in, DataOutputStream out) throws Exception {
        try {
            int l0 = in.readInt();
            if (l0 != 5)
                throw new Exception("wrong ping packet arraibed");

            byte[] body = readBytes(in, 5);
            out.writeByte(1);
            out.write(body, 0, 5);
            out.flush();
        } catch(Exception e) {
            throw e;
        }
    }

    private void processPCHDecryptPacket(DataInputStream in, DataOutputStream out) throws Exception {
        int l0 = in.readInt();
        byte[] cipheredText = readBytes(in, l0);
        byte[]    plainText = decrypter.decryptPCH(cipheredText);
        int length = plainText.length;
        out.writeByte(3);
        out.writeInt(length);
        out.write(plainText, 0, length);
        out.flush();
    }

    private void processDecryptPacket(DataInputStream in, DataOutputStream out) throws Exception {
        int l0 = in.readInt();
        byte[] cipheredText = readBytes(in, l0);
        // DEBUG Point
        // tmpCiperedText = new String(cipheredText);

        int l1 = in.readInt();
        byte[] salt = readBytes(in, l1);
        // DEBUG Point
        // tmpSalt = new String(salt);

        byte[] plainText = decrypter.decrypt(cipheredText, salt, 1);

        //
        // 1: ok, 2: fail
        //
        if (plainText != null) {
            out.writeByte(3);
            int length = plainText.length;
            out.writeInt(length);
            out.write(plainText, 0, length);
            out.flush();
        } else {
            String message = "decrypt failed";
            processErrorPacket(in, out, message);
        }
    }

    private byte[] readBytes(DataInputStream in, int len) throws Exception {
        try {
            byte[]  buffer = new byte[len];
            int actualRead = in.read(buffer);
            return len == actualRead ? buffer : null;
        } catch (IOException e) {
            throw e;
        }
    }
}

public class KakaoDecryptor {
    char[] password1 = { 22, 8, 9, 111, 2, 23, 43, 8, 33, 33, 10, 16, 3, 3, 7, 6 }; 
    byte[] iv1       = { 15, 8, 1, 0, 25, 71, 37, -36, 21, -11, 23, -32, -31, 21, 12, 53 }; 

    char[] password2 = { 42, 10, 8, 120, 7, 55, 11, 33, 9, 33, 10,    13, 5, 2, 10, 7 };
    byte[] iv2       = { 10, 2, 3, -4, 20, 73, 47, -38, 27, -22, 11, -20, -22, 37, 36, 54 };
    
    public KakaoDecryptor() { 
        try { Security.addProvider(new BouncyCastleProvider());
        } catch (Exception ex) {
        }
    }

    /*
    public String encrypt(String s, byte[] salt) {
        try {
            SecretKeySpec localSecretKeySpec = getLocalSecretKeySpec(salt);

            Cipher c = Cipher.getInstance("AES/CBC/PKCS5Padding");
            c.init(1, localSecretKeySpec, new IvParameterSpec(iv));

            return Base64.encodeToString(c.doFinal(s.getBytes("UTF-8")));
        } catch (Exception ex) {
        }

        return "";
    }
    */

    public String decryptPCH(String cipheredText) throws Exception {
        byte[] ciphered = cipheredText.getBytes("UTF-8");
        byte[] plain    = decryptPCH(ciphered);
        return new String(plain, "UTF-8");
    }

    public byte[] decryptPCH(byte[] ciphered) throws Exception {
        byte[] saltBytes = { 12, 10, -8, -43, -12, 44, 5, -8, -32, 7, 34, -24, -2, 3, 33, -33 };
        // TODO
        // byte[] saltBytes = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }; 
        return decrypt(ciphered, saltBytes, 2);
    }

    public String decrypt(String cipheredText, String saltStr) throws Exception {
        byte[] saltBytes = saltStr.getBytes("UTF-8");
        byte[] ciphered  = cipheredText.getBytes("UTF-8");
        byte[] plain     = decrypt(ciphered, saltBytes, 1);

        return new String(plain, "UTF-8");
    }

    public byte[] decrypt(byte[] cipheredText, byte[] saltbytes, int choice) throws Exception {
        if (saltbytes.length > 16) 
            throw new Exception("salt length too long");

        byte[] salt = new byte[16];
        for (int i = 0; i < 16; i++)
            salt[i] = (i < saltbytes.length) ? saltbytes[i] : 0;

        SecretKeySpec localSecretKeySpec = getLocalSecretKeySpec(salt, choice);
        Cipher  d = Cipher.getInstance("AES/CBC/PKCS5Padding");
        byte[] iv = (choice == 1) ? iv1 : iv2;

        d.init(2, localSecretKeySpec, new IvParameterSpec(iv));
        byte[] decryptedBytes = Base64.decodeToBytes(cipheredText);

        int remainder = decryptedBytes.length % 16;
        if (remainder != 0)
        {
            byte[] paddedRes = new byte[decryptedBytes.length + (16 - remainder)];
            for (int i=0; i<paddedRes.length; i++)
                paddedRes[i] = (i < decryptedBytes.length) ? decryptedBytes[i] : 0;

            return d.doFinal(paddedRes);
        }
        else
        {
            return d.doFinal(decryptedBytes);
        }
    }

    private SecretKeySpec getLocalSecretKeySpec(byte[] salt, int choice) {
        try {
            PBEKeySpec pbeKeySpec = null;
            char[] pw = (choice == 1) ? password1 : password2;

            pbeKeySpec = new PBEKeySpec(pw, salt, 2, 256);

            SecretKeyFactory keyFactory = SecretKeyFactory.getInstance("PBEWithSHAAnd256BitAES-CBC-BC");
            SecretKeySpec localSecretKeySpec = new SecretKeySpec(keyFactory.generateSecret(pbeKeySpec).getEncoded(), "AES");
            return localSecretKeySpec;
        } catch (Exception ex) {
        }

        return null;
    }

    public static void main(String[] args) throws Exception {
        KakaoDecryptor   dec = new KakaoDecryptor();
        DecryptServer server = new DecryptServer(7781, dec);

        System.out.println("Starting decrypt server");
        if (!server.startServer()) {
            System.out.println("failed to start server");
            System.exit(1);
        }

        // DEBUG Point
        /*
        // Test 1: PCH field (in xml file) decrypt test
        System.out.println(dec.decryptPCH("qmza6Is0boO+IoxkLwKz/A=="));

        // Test 2: fields in table decrypt test
        System.out.println(dec.decrypt("m+oavcl6PVEo1RBcCFlKSQ==", "23303370"));
        */
    }
}
