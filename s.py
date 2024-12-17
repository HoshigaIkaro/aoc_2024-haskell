a = 48744869
b = 0
c = 0

output = []

while a != 0:
    b = a % 8
    b = b ^ 2
    c = a // (2 ** b)
    b = b ^ 3
    b = b ^ c
    output.append(b % 8)
    a = a // 8

print(output)