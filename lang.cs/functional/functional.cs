using System.IO;
using System.Text.RegularExpressions;
using System.Xml.Serialization;
using CSharpFunctionalExtensions;
using FluentAssertions.Execution;
using FluentAssertions.Primitives;

namespace FluentAssertions;

public class Email
{
    private readonly string _value;

    private Email(string value) 
    {
        _value = value; 
    }

    public static Result<Email> Create(string email)
    {
        if (string.IsNullOrEmpty(email))
            return Result.Failure<Email>("E-mail cannot be empty");

        if (email.Length > 100)
            return Result.Failure<Email>("E-mail is too long");

        if (!Regex.IsMatch(email, @"^([\w\.\-]+)@([\w\-]+)((\.(\w){2,3})+)$"))
            return Result.Failure<Email>("E-mail is invalid");

        return Result.Success(new Email(email));
    }

    public static implicit operator string(Email email)
    {
        return email._value;
    }

    public override bool Equals(object? obj)
    {
        var email = obj as Email;
        if (ReferenceEquals(email, null))
            return false;

        return _value == email._value;
    }

    public override int GetHashCode()
    {
        return _value.GetHashCode();
    }
}

public class CustomerName
{
    private readonly string _value;

    private CustomerName(string value)
    {
        _value = value;
    }

    public static Result<CustomerName> Create(string name)
    {
        if (string.IsNullOrWhiteSpace(name))
            return Result.Failure<CustomerName>("Name can't be empty");

        if (name.Length > 50)
            return Result.Failure<CustomerName>("Name is too long");

        return Result.Success(new CustomerName(name));
    }

    public static implicit operator string(CustomerName name)
    {
        return name._value;
    }

    public override bool Equals(object? obj)
    {
        var name = obj as CustomerName;

        if (ReferenceEquals(name, null))
            return false;

        return _value == name._value;
    }

    public override int GetHashCode()
    {
        return _value.GetHashCode();
    }

    public override string ToString()
    {
        return _value;
    }
}

public class Customer
{
    public CustomerName Name { get; set; }
    public Email Email { get; set; }

    public Customer(CustomerName name, Email email)
    {
        if (name == null) throw new ArgumentNullException("name");
        if (email == null) throw new ArgumentNullException("email");

        Name = name;
        Email = email;
    }
}

class Program
{
    private static void Main2(string[] args)
    {
        var name = CustomerName.Create("yielding");
        var email = Email.Create("yielding@gmail.com");

        var result = Result.Combine(name, email);
        if (result.IsFailure)
        {
            Console.WriteLine("error");
            return;
        }

        var customer = new Customer(name.Value, email.Value);
        var s = customer.Name;
        Console.WriteLine($"{s}");

        var maybe = Maybe.From("");
        maybe.Should().HaveSomeValue();
    }
}