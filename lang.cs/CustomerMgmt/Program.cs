using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Mail;
using System.Net;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

using CSharpFunctionalExtensions;

namespace CustomerManagement
{
    class Request 
    {
        public string Name;
        public string Email;

        public Request(string name, string email)
        {
            Name = name;
            Email = email;
        }
    }

    class Dbms
    {
        public static Dbms Connect(string conn) 
        { 
            return new Dbms(); 
        }

        public bool UpdateFrom(Request r) 
        { 
            return true; 
        }
    }

    public class LicenseRepository
    {
        private Dbms db;

        public LicenseRepository()
        {
            db = Dbms.Connect("Oracle.Gmd.Costumer");
        }

        public string UpdateCustomer()
        {
            return ReceiveRequest()
                .Ensure(request => Validate(request))
                .Bind(request => UpdateDbFrom(request))
                .Bind(request => CanonicalizeEmail(request))
                .Bind(email => SendEmail(email))
                .Match(success => "OK",
                       failure => PrintRes(failure));
                //.Finally(ret => ret.IsSuccess ? "OK" : ret.Error);
        }

        private string PrintRes(string res)
        {
            Console.WriteLine(res);
            return res;
        }

        private Result<Request> ReceiveRequest()
        {
            //return new Request(" Zoro ", "zorokim2020@naver.com  ");  // Success
            return new Request(" Zoro ", "zorokim2020naver.com  ");    // Failure
        }

        private Result<bool> Validate(Request r)
        {
            if (string.IsNullOrEmpty(r.Name))
                return Result.Failure<bool>("Name must not be blank");
            
            if (string.IsNullOrEmpty(r.Email))
                return Result.Failure<bool>("Name must not be blank");

            if (!r.Email.Contains("@"))
                return Result.Failure<bool>("Email is not valid");

            return true;  // true => Result.Success(true);
        }

        private Result<Request> UpdateDbFrom(Request r)
        {
            try
            {
                db.UpdateFrom(r);
            }
            catch
            {
                return Result.Failure<Request>("Customer record not updated");
            }

            return r;
        }

        private Result<string> CanonicalizeEmail(Request r)
        {
            return r.Email.Trim().ToLower();
        }

        private Result<bool> SendEmail(string email)
        {
            string smtpAddress = "smtp.naver.com";
            int portNumber = 587;
            bool enableSSL = true;

            string emailFrom = "zorokim2020@naver.com";   // Sender's email
            string password = "gmd#3201";                 // Sender's email password or app-specific password
            string emailTo = email;
            string subject = "Hello from C#";
            string body = "This is a test email sent from a C# program!";

            try
            {
                using (var mail = new MailMessage())
                {
                    mail.From = new MailAddress(emailFrom);
                    mail.To.Add(emailTo);
                    mail.Subject = subject;
                    mail.Body = body;
                    mail.IsBodyHtml = true; // Set to true if body is HTML

                    // Configure SMTP client
                    using (var smtp = new SmtpClient(smtpAddress, portNumber))
                    {
                        smtp.Credentials = new NetworkCredential(emailFrom, password);
                        smtp.EnableSsl = enableSSL;

                        // Send the email
                        smtp.Send(mail);
                        Console.WriteLine("Email sent successfully!");
                    }
                }
            }
            catch (Exception ex)
            {
                return Result.Failure<bool>($"Error sending email: {ex.Message}");
            }

            return true;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var repo = new LicenseRepository();
            var result = repo.UpdateCustomer();

            Console.WriteLine(result);
        }
    }
}
