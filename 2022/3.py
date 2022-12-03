s=[];A=0
for b in open('3.input'):
    p=len(b)//2
    b=[(ord(c)-96)%58for c in b[:-1]]
    A+=max(set(b[:p])&set(b[p:]))
    s+=[set(b)]
print(A,sum(max(s[i]&s[i+1]&s[i+2])for i in range(0,len(s),3)))
