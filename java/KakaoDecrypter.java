// TODO
// package com.gmdmfix.KakaoDecryptor;

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
    KakaoDecrypter decrypter = null;

    public DecryptServer(int port, KakaoDecrypter decrypter) {
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
            try {
System.out.println("1.0 accepting...");
                client = server.accept();
System.out.println("1.1: accepted");
                DataInputStream   in = new DataInputStream (new BufferedInputStream (client.getInputStream()));
                DataOutputStream out = new DataOutputStream(new BufferedOutputStream(client.getOutputStream()));

System.out.println("1.2: reading header");
                byte header = in.readByte();
System.out.println("2: header=" + header);
                switch (header) {
                    case 0: shouldTerminate = true;        break;
                    case 1: processPingPacket(in, out);    break;
                    case 2: processDecryptPacket(in, out); break;
                }

                client.close();

            } catch(Exception e) {
                e.printStackTrace();
                break;
            }
        }

        return true;
    }

    private void processPingPacket(DataInputStream in, DataOutputStream out) {
        try {
            byte[] body = readBytes(in, 5);
            out.writeByte(1);
            out.write(body, 0, 5);
            out.flush();
        } catch(Exception e) {
        }
    }

    private void processDecryptPacket(DataInputStream in, DataOutputStream out) throws Exception {
System.out.println("1");
        try {
            int l0 = in.readInt();
            byte[] cipheredText = readBytes(in, l0);
System.out.println(new String(cipheredText));

            int l1 = in.readInt();
            byte[] salt = readBytes(in, l1);
System.out.println(new String(salt));

            byte[] plainText = decrypter.decrypt(cipheredText, salt);

            //
            // 1: ok, 2: fail
            //
            if (plainText != null) {
                out.writeByte(3);
                int length = plainText.length;
                out.writeInt(length);
                out.write(plainText, 0, length);
            } else {
                out.writeByte(4);
            }
            out.flush();

        } catch(IOException e) {
        }
    }

    private byte[] readBytes(DataInputStream in, int len) {
        try {
            byte[]  buffer = new byte[len];
            int actualRead = in.read(buffer);
            if (len != actualRead)
                return null;

            return buffer;

        } catch(IOException e) {

        }

        return null;
    }
}

public class KakaoDecrypter {

    char[] password = { 22, 8, 9, 111, 2, 23, 43, 8, 33, 33, 10, 16, 3, 3, 7, 6 };
    byte[] iv       = { 15, 8, 1, 0, 25, 71, 37, -36, 21, -11, 23, -32, -31, 21, 12, 53 };

    public KakaoDecrypter() {
        try {
            Security.addProvider(new BouncyCastleProvider());
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

    public String decrypt(String ciphertedText, String saltStr) throws Exception {
        byte[] saltBytes = saltStr.getBytes("UTF-8");
        byte[] ciphered  = ciphertedText.getBytes("UTF-8");
        byte[] plain     = decrypt(ciphered, saltBytes);
        
        return new String(plain, "UTF-8");
    }

    public byte[] decrypt(byte[] ciphertedText, byte[] saltbytes) throws Exception {
        if (saltbytes.length > 16) 
            throw new Exception("salt length too long");

        byte[] salt = new byte[16];
        for (int i = 0; i < 16; i++)
            salt[i] = (i < saltbytes.length) ? saltbytes[i] : 0;

        try {
            SecretKeySpec localSecretKeySpec = getLocalSecretKeySpec(salt);
            Cipher d = Cipher.getInstance("AES/CBC/PKCS5Padding");
            d.init(2, localSecretKeySpec, new IvParameterSpec(iv));
            return d.doFinal(Base64.decodeToBytes(ciphertedText));
        } catch (Exception ex) {
        	ex.printStackTrace();
        }

        return null;
    }

    private SecretKeySpec getLocalSecretKeySpec(byte[] salt) {
        try {
            PBEKeySpec pbeKeySpec = new PBEKeySpec(password, salt, 2, 256);
            SecretKeyFactory keyFactory = SecretKeyFactory.getInstance("PBEWithSHAAnd256BitAES-CBC-BC");
            SecretKeySpec localSecretKeySpec = new SecretKeySpec(keyFactory.generateSecret(pbeKeySpec).getEncoded(), "AES");
            return localSecretKeySpec;
        } catch (Exception ex) {

        }

        return null;
    }

    public static void main(String[] args) throws Exception {
        /*
        if (args.length != 1) {
            System.out.println("usage: java KakaoDecrypter portNo");
            System.exit(1);
        }
        */

        KakaoDecrypter dec = new KakaoDecrypter();
        DecryptServer server = new DecryptServer(7781, dec);

        System.out.println("Starting decrypt server");
        if (!server.startServer())
        {
            System.out.println("failed to start server");
            System.exit(1);
        }

        /*
        try
        {
        	String r1 = dec.decrypt("m+oavcl6PVEo1RBcCFlKSQ==", "23303370");
            System.out.println(r1);
        }
        catch (Exception ex)
        {
        }
        */
    }
}

