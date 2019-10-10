// let DST=image<rgb,u8,512x512>:w
let SRC=image<rgba,u8>('lena.png'):r
let IMG1=image<rgba,u8,512x512>:rw
let IMG2=image<rgba,u8,512x512>:rw
let IMG3=image<rgba,u8,512x512>:rw
#1`blur9.cl`blur<512x512>(IMG1,SRC)
#1`blur9.cl`blur<512x512>(IMG2,IMG1)
#1`blur9.cl`blur<512x512>(IMG3,IMG2)
save("b1.png",IMG1)
save("b2.png",IMG2)
save("b3.png",IMG3)