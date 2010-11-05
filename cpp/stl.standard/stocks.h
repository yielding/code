#include <locale>
#include <string>
#include <deque>
#include <stdexcept>
#include <iostream>
#include <sstream>

#include <locale.h> 

using namespace std;

//Stock exchange displays stock quote at various
//exchanges distributed globally. Stock prices varies
//at random(so much for real life!).

//Objective of the example is to show money,
//number,date and time formated using facets

static size_t rand_   = 0;  //to manipulate radomness of stocks
static struct tm* tmb = 0;  //to use by 'localTime()'

struct Company
{
public:
  //
  Company(const string&s, double p):companyName(s),                                                                offerPrice(p){} 

  //In actuality a company should not manipulate stocks.
  //For simplicity let the company play with its own stock.

  void updateStock()
  {
    double change =offerPrice+randomChange(offerPrice);
    if(randomChange(marketOutlook())%2)
      stockPrice+=change;
    else
      stockPrice-=change;
    if(stockPrice<0)stockPrice=-(stockPrice);
  }
  unsigned long randomChange (unsigned long i) 
  { 
    if(i)return rand() % i;
    else return rand() % 2;
  }
  string companyName;
  double offerPrice; //initial offer price
  double stockPrice; //current market price
private:
  string companyNews;//Make use of messaging (if at all it works !)
  size_t marketOutlook(){ return rand_++;}
};

class StockXchange: public locale::facet
{
public:
  //
  typedef basic_ostream<char,char_traits<char> > outStream;
  typedef ostreambuf_iterator<char, char_traits<char> > iter_type;
  typedef deque<Company*,allocator<Company*> > database;

  StockXchange(size_t refs=0):locale::facet(refs){}
  StockXchange(const  StockXchange& se)
  {companyDatabase = se.companyDatabase;}
  virtual ~StockXchange(){}

  static locale::id id;
  virtual  bool      put(ostream& os) const;
  virtual  void      add(const string& name, double initPrice);
  virtual  void      localTime(ostream&) const;

protected:
  database companyDatabase;  
  friend StockXchange::outStream& operator<<
    (StockXchange::outStream&, const StockXchange&);

private: 
#ifdef _RWSTD_NO_MEMBER_TEMPLATES
  locale::id &__get_id (void) const { return id; }
#endif
};

class TokyoStockXchange : public StockXchange
{
public:
  //
  TokyoStockXchange(size_t refs=0)
    :StockXchange(refs){}

  virtual  void      localTime(ostream& os) const
  {
    time_t tm = time(NULL);
    tmb = gmtime(&tm); 
    StockXchange::localTime(os);
  }

  virtual  bool  put(ostream& os) const 
  { 
    os<<'\n';
    os<<"######## TOKYO STOCK EXCHANGE #########"<<endl;
    if(StockXchange::put(os)) return 1;
    else return 0;
  }
}; 

class LondonStockXchange : public StockXchange
{
public:
  //

  LondonStockXchange(size_t refs=0)
    :StockXchange(refs){}

  virtual  void      localTime(ostream& os) const
  {
    time_t tm = time(NULL);
    tmb = gmtime(&tm); 
    StockXchange::localTime(os);
  }

  virtual  bool   put(ostream& os) const 
  { 
    os<<'\n';
    os<<"######## LONDON STOCK EXCHANGE #########"<<endl;
    if(StockXchange::put(os)) return 1;
    else return 0;
  }
}; 

class FrankFurtStockXchange : public StockXchange
{
public:
  //
  FrankFurtStockXchange(size_t refs=0)
    :StockXchange(refs){}

  virtual  void      localTime(ostream& os) const
  {
    time_t tm = time(NULL);
    tmb = gmtime(&tm); 
    StockXchange::localTime(os);
  }

  virtual  bool      put(ostream& os) const 
  { 
    os<<'\n';
    os<<"######## FRANKFURTER WERTPAPIERB\366RSE #########"<<endl;
    if(StockXchange::put(os)) return 1;
    else return 0; 
  }
};

class NewYorkStockXchange : public StockXchange
{
public:
  //
  NewYorkStockXchange(size_t refs=0)
    :StockXchange(refs){}

  virtual  void      localTime(ostream& os) const
  {
    time_t tm = time(NULL);
    tmb = localtime(&tm); 
    StockXchange::localTime(os);
  }

  virtual  bool    put(ostream& os) const 
  { 
    os<<'\n';
    os<<"######## NEW YORK STOCK EXCHANGE ########"<<endl;
    if(StockXchange::put(os)) return 1;
    else return 0; 
  }
}; 

class ParisStockXchange : public StockXchange
{
public:
  //
  ParisStockXchange(size_t refs=0)
    :StockXchange(refs){}

  virtual  void      localTime(ostream& os) const
  {
    time_t tm = time(NULL);
    tmb = gmtime(&tm); 
    StockXchange::localTime(os);
  }

  virtual  bool   put(ostream& os) const
  { 
    os<<'\n';
    os<<"######## BOURSE DE PARIS #########"<<endl;
    if(StockXchange::put(os)) return 1;
    else return 0;     
  }
};
