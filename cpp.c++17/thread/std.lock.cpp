#include <mutex>
#include <thread>
#include <iostream>
#include <vector>
#include <chrono>
#include <string>

using namespace std;

struct Employee
{
  Employee(string id): id(id)
  {}

  string to_s() const
  {
    string ret = "Employee " + id + " has lunch partners: ";
    for (const auto& p: lunch_partners) ret += p + " ";

    return ret;
  }

  vector<string> lunch_partners;
  mutex m;
  string id;
};

void send_mail(Employee& e1, Employee& e2)
{
  this_thread::sleep_for(chrono::seconds(1));
}

void assign_lunch_partner(Employee& e1, Employee& e2)
{
  static mutex io;
  {
    lock_guard<mutex> lk(io);
    cout << e1.id << " and " << e2.id << " are waiting for locks" << endl;
  }

  {
    scoped_lock lk(e1.m, e2.m);
    {
      lock_guard<mutex> lk(io); 
      cout << e1.id << " and " << e2.id << " got locks" << endl; 
    }
  }

  send_mail(e1, e2);
  send_mail(e2, e1);
}

int main(int argc, char *argv[])
{
  Employee alice("alice"), 
           bob("bob"), 
           christina("christina"), 
           dave("dave");

  // assign in parallel threads because mailing users about lunch assignments
  // takes a long time
  vector<thread> threads;
  threads.emplace_back(assign_lunch_partner, ref(alice), ref(bob));
  threads.emplace_back(assign_lunch_partner, ref(christina), ref(bob));
  threads.emplace_back(assign_lunch_partner, ref(christina), ref(alice));
  threads.emplace_back(assign_lunch_partner, ref(dave), ref(bob));

  for (auto &thread : threads) 
    thread.join();

  cout << alice.to_s() << '\n'  << bob.to_s() << '\n'
       << christina.to_s() << '\n' << dave.to_s() << '\n';
}
