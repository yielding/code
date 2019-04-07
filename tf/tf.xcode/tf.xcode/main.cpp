//#include "tensorflow/core/framework/tensor.h"
#include "tensorflow/cc/client/client_session.h"
#include "tensorflow/cc/ops/standard_ops.h"

#include <iostream>

using namespace std;
using namespace tensorflow;
using namespace tensorflow::ops;

int main()
{
#if 1
  auto root = Scope::NewRootScope();

  // Matrix A = [ 3 2]
  //            [-1 0]
  auto A = Const(root, { {3.f, 2.f}, {-1.f, 0.f} });

  // Vector b = [3 5]
  auto b = Const(root, { {3.f, 5.f} });
  
  // v = Ab^T
  auto v = MatMul(root.WithOpName("v"), A, b, MatMul::TransposeB(true));
  ClientSession session(root);
  
  // Run and fetch v
  vector<Tensor> outputs;
  TF_CHECK_OK(session.Run({v}, &outputs));
  // Expect outputs[0] == [19; -3]
  //LOG(INFO) << outputs[0].matrix<float>();
  cout << outputs[0].matrix<float>() << endl;
#endif
  
#if 1
  auto a0 = Placeholder(root, DT_FLOAT);
  auto a1 = Placeholder(root, DT_FLOAT);
  auto r0 = Multiply(root, a0, a1);
  auto s  = session.Run({{a0, {2.f}},
                         {a1, {3.f}}}, {r0}, &outputs);
  if (s.ok())
    cout << outputs[0].scalar<float>() << endl;
#endif

#if 0
  auto a = Placeholder(root, DT_INT32);
  auto c = Add(root, a, {41});
  auto s = session.Run({ {a, {2}} }, {c}, &outputs);
  if (s.ok())
    cout << outputs[0].scalar<int>() << endl;
  
#endif

  return 0;
}
