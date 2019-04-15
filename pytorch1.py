from __future__ import print_function
import torch
x = torch.randn(4, 4)
y = x.view(16)
z = x.view(-1, 8)  # the size -1 is inferred from other dimensions
print(x.size(), y.size(), z.size())


x = torch.randn(1)
print(x)
print(x.item())
