class TreeNode:
    def __init__(self, x, left, right):
        self.val   = x
        self.left  = left
        self.right = right

class Solution:
    def isSameTree(self, treeP, treeQ):

        if not treeP and not treeQ:
            return True

        try:
            if treeP.val == treeQ.val and self.isSameTree(treeP.left, treeQ.left) \
                                      and self.isSameTree(treeP.right, treeQ.right):

                return True
        except AttributeError:
            return False

        return False

def for_test_module(x):
    print x

if __name__ == "__main__":
    print Solution().isSameTree(None, None)

    a1 = TreeNode(1, None, None)
    a2 = TreeNode(1, None, None)
    a3 = TreeNode(2, a1  , a2  )

    b1 = TreeNode(1, None, None)
    b2 = TreeNode(1, None, None)
    b3 = TreeNode(2, b1  , b2  )

    print Solution().isSameTree(a3, b3)