import torch
import torch.nn as nn
from torch.autograd import Variable

h = [1, 0, 0, 0]
e = [0, 1, 0, 0]
l = [0, 0, 1, 0]
o = [0, 0, 0, 1]

# one cell RNN
cell = nn.RNN(input_size=4, hidden_size=2, batch_first=True)

# hidden
# (num_layers * num_directions, batch, hidden_size) whether batch_first=True or False
hidden = Variable(torch.randn(1, 1, 2))

inputs = Variable(torch.Tensor([h, e, l, l, o]))
for one in inputs:
    one = one.view(1, 1, -1)
    out, hidden = cell(one, hidden)
    print("one input size", one.size(), "out size", out.size())


# We can do the whole at once
# propagate input through RNN
# input: (batch, seq_len, input_size) when batch_first=True
inputs = inputs.view(1, 5, -1)
out, hidden = cell(inputs, hidden)

print("sequence input size", inputs.size(), "out sizse", out.size())


hidden = Variable(torch.randn(1, 3, 2))

# one cell RNN input_dim(4) -> output_dim(2), sequence: 5, batch 3
# 3 batches 'hello', 'eolll', 'lleel'
# rank = (3, 5, 4)

inputs = Variable(torch.Tensor(
    [[h, e, l, l, o],
     [e, l, l, o, h],
     [l, l, e, e, l]]))

# propagate input through RNN
# input: (batch, seq_len, input_size) when batch_first=True
# B x S x I
out, hidden = cell(inputs, hidden)
print("batch input size", inputs.size(), "out sizse", out.size())

cell   = nn.RNN(input_size=4, hidden_size=2)
inputs = inputs.transpose(dim0=0, dim1=1)
out, hidden = cell(inputs, hidden)
print("batch input size", inputs.size(), "out size", out.size())


