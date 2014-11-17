#!/usr/bin/python

from sametree import TreeNode
from sametree import Solution

from sametree import for_test_module

print TreeNode(1, None, None)

a1 = TreeNode(1, None, None)
a2 = TreeNode(1, None, None)

a3 = TreeNode(2, a1, a2)

b1 = TreeNode(1, None, None)
b2 = TreeNode(1, None, None)

b3 = TreeNode(2, b1, b2)

print Solution().isSameTree(None, None)
print Solution().isSameTree(a3, b3)