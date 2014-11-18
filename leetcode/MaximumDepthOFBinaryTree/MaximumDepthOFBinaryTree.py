# Definition for a  binary tree node
class TreeNode:
    def __init__(self, x):
        self.val = x
        self.left = None
        self.right = None

class Solution:
    # @param root, a tree node
    # @return an integer
    def maxDepth(self, root):
        return self.getmax(root, 0)

    def getmax(self, root, depth):
        if not root:
            return depth

        return max(self.getmax(root.left, depth + 1), self.getmax(root.right, depth + 1))


if __name__ == '__main__':
    a = TreeNode(1)

    print Solution().maxDepth(a)