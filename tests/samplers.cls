let SRC=image<rgba,u8>('tests/lena256.png'):r
// let OU1=image<rgba,u8,768x768>:w
// #1`tests/samplers.cl`inlineClampToEdge<768x768>(SRC,OU1)
// save("ou-cte.png",OU1)
//
// let OU2=image<rgba,u8,768x768>:w
// #1`tests/samplers.cl`inlineRepeat<768x768>(SRC,OU2)
// save("ou-rep.png",OU1)

// let OU3=image<rgba,u8,768x768>:w
// #1`tests/samplers.cl`indirect<768x768>(SRC,OU3,sampler(CL_FALSE,CL_ADDRESS_CLAMP,CL_FILTER_NEAREST))
// save("ou-ind.png",OU3)

// let OU4=image<rgba,u8,768x768>:w
// #1`tests/samplers.cl`inlineRepeat<768x768>(SRC,OU4)
// save("ou-ind.png",OU4)

let OU5=image<rgba,u8,768x768>:w
#1`tests/samplers.cl`inlineMirroredRepeat<768x768>(SRC,OU5)
save("ou-ind.png",OU5)

