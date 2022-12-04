A=B=0
for l in open('4.input'):z,x,c,v=map(int,l.replace(',','-').split('-'));A+=(z-c)*(x-v)<=0;B+=c<=x>=z<=v
print(A,B)
