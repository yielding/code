#include <boost/log/core.hpp>
#include <boost/log/attributes/clock.hpp>
#include <boost/log/attributes/counter.hpp>
#include <boost/log/sources/basic_logger.hpp>
#include <boost/log/sources/global_logger_storage.hpp>
#include <boost/log/sinks/sink.hpp>
#include <boost/log/sinks/text_ostream_backend.hpp>
#include <boost/log/utility/rotating_ofstream.hpp>
#include <boost/log/utility/empty_deleter.hpp>
#include <boost/log/formatters/format.hpp>
#include <boost/log/formatters/attr.hpp>
#include <boost/log/formatters/date_time.hpp>
#include <boost/log/formatters/message.hpp>

namespace logging = boost::log;
namespace attrs   = boost::log::attributes;
namespace src     = boost::log::sources;
namespace sinks   = boost::log::sinks;
namespace fmt     = boost::log::formatters;

using namespace std;
using boost::shared_ptr;

enum 
{ 
  LOG_RECORDS_TO_WRITE = 10000 
};

int main(int argc, char* argv[])
{
  try
  {
    // Open a rotating text file
    shared_ptr<ostream> 
      strm(new logging::rotating_ofstream("test_%Y%m%d.log", 
            logging::keywords::open_mode=ios_base::trunc));

    if (!strm->good())
      throw runtime_error("Failed to open a text log file");

    // Create a text file sink
    shared_ptr<sinks::synchronous_sink<sinks::text_ostream_backend> > 
      sink(new sinks::synchronous_sink<sinks::text_ostream_backend>);

    sink->locked_backend()->add_stream(strm);
    sink->locked_backend()->auto_flush(true);

    sink->locked_backend()->set_formatter(
      fmt::format("%1%: [%2%] - %3%")
      % fmt::attr<unsigned int>("Line #")
      % fmt::date_time<boost::posix_time::ptime>("TimeStamp", "%Y.%m.%d %H:%M:%S")
      % fmt::message()
    );

    // Add it to the core
    logging::core::get()->add_sink(sink);

    // Add some attributes too
    shared_ptr<logging::attribute> attr(new attrs::local_clock);
    logging::core::get()->add_global_attribute("TimeStamp", attr);
    attr.reset(new attrs::counter<unsigned int>);
    logging::core::get()->add_global_attribute("Line #", attr);

    // Do some logging
    src::logger lg;
    for (unsigned int i=0; i<LOG_RECORDS_TO_WRITE; ++i)
    {
      BOOST_LOG(lg) << "Some log record";
      //sleep(1);
    }

    return 0;
  }
  catch (exception& e)
  {
    cout << "FAILURE: " << e.what() << endl;
    return 1;
  }
}
