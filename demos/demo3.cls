let A=seq(10):rw, B=seq(10):rw
#0`demo3.cl`test<8>(A)
#0`demo3.cl[-cl-fp32-correctly-rounded-divide-sqrt]`test<8>(B)
print<uint>(A); print<uint>(B)
diff<float,0.001>(A,B)