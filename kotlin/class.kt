class Customer

class Contact(val id: Int, var email:String)

fun main() {
  val customer = Customer()
  val contact = Contact(1, "mary@gmail.com")

  println(contact.id)
  contact.email = "jane@gmail.com"
}