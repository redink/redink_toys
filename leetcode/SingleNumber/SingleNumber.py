class Solution:
    def singleNumber(self, A):
        """
        Cannot use hash table since extra memory
        Use XOR instead
        Algorithm:
        Concept of RAID
        XOR
        bits:
        consider a list of 4-bit number:
        0000
        0001
        0010
        ...
        1111
        appear twice:
        num^num is 0
        storage ^= (num^num) is storage ^= 0, which is storage itself
        if only appear once:
        storage ^= num is the num, since the storage is 0 initially
        :param A: a list of integer
        :return: int
        """
        storage = 0
        for element in A:
            print element
            print storage

            storage ^= element # XOR
            print storage

            print "-------"
        return storage

if __name__ == '__main__':
    a = [1,2,1,3,3,2]
    print Solution().singleNumber(a)