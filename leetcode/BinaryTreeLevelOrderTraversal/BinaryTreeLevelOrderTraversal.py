class TreeNode:
    def __init__(self, x):
        self.val = x
        self.left = None
        self.right = None

class Solution:

    def preorder(self, root, level, res):
        if root:
            print "$$$$$$$$"
            if len(res) < level+1: 
                res.append([])
                print res
            res[level].append(root.val)
            print res
            self.preorder(root.left, level+1, res)
            self.preorder(root.right, level+1, res)
            print "----------"
    def levelOrder(self, root):
        res=[]
        self.preorder(root, 0, res)
        return res

if __name__=="__main__":
    nodes = [TreeNode(i) for i in range(3)]
    nodes[0].left = nodes[1]
    nodes[1].left = nodes[2]
    print Solution().levelOrder(nodes[0])