import math 

def split_at(num,m):
    m1 = str(num)[:m]
    m2 = str(num)[m:]
    return (int(m1),int(m2))


# El algoritmo de karatsuba tiene un tiempo de ejecucion de n ^log3

def Karatsuba(num1,num2):
    if num1 < 10 or num2 < 10:
        return num1*num2

    m = min(int(math.log10(num1))+1,int(math.log10(num1))+1)
    m2 = math.floor(m/2) 


    high1,low1 = split_at(num1,m2)
    high2,low2 = split_at(num2,m2)
    print(high1," ",low1)
    print(high2," ",low2)
    z0 = Karatsuba(low1,low2)
    print(z0)
    z1 = Karatsuba((low1+high1),(low2+high2))
    print(z1)
    z2 = Karatsuba(high1,high2)
    print(z2)
    return (z2 * 10^ (m2*2) ) + ((z1-z2-z0) * 10 ^ m2) + z0

def main():
    num1 = 12
    num2 = 12

    print(Karatsuba(num1,num2))

main()
