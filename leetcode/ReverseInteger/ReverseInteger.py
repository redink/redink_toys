class Solution:
    # @return an integer
    def reverse(self, x):
        if x > 2147483647 or x < -2147483648:
            return 0
        
        flag = 1
        if x < 0:
            flag = -1
            
        x = x * flag
        
        '''
        1100(int) -> 11(int)
        '''
        while x:
            if x % 10 == 0:
                x = x / 10
            else:
                break
        '''
        123 (int) -> '123' -> ['1', '2', '3'] -> ['3', '2', '1'] -> '321' -> 321(int)
                  str()    list()            reverse()         join()    int()
        '''
        lst = list(str(x))
        lst.reverse()
    
        result = flag * (int(''.join(lst)))
        
        if result > 2147483647 or result < -2147483648:
            return 0
        return result

if __name__ == '__main__':
    21 == Solution().reverse(120)
    123 == Solution().reverse(321)
    111 == Solution().reverse(111)