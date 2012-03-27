#include <cassert>
#include <boost/proto/proto.hpp>
using namespace boost;
 
struct arg1_tag {};
proto::terminal<arg1_tag>::type const arg1 = {};
 
// A Proto "algorithm": a grammar with embedded transforms for
// evaluating lambda expressions (explained in the previous article):
struct Lambda
  : proto::or_<
        // When evaluating a placeholder terminal, return the state.
        proto::when< proto::terminal<arg1_tag>, proto::_state >

        // Otherwise, do the "default" thing.
      , proto::otherwise< proto::_default< Lambda > >
    >
{};
 
int main()
{
    // Evaluate the lambda arg1 + 42, replacing arg1 with 1
    int i = Lambda()(arg1 + 42, 1);
    assert(i == 43);
}
