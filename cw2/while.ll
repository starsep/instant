define i32 @f(i32 %b) {
  %b2 = alloca i32
  %c =  alloca i32
  store i32 %b, i32* %b2
  br label %loop
loop:
  %t1 = load i32, i32* %b2
  %t2 = icmp eq i32 %t1, 0
  br i1 %t2, label %endf, label %body
body:
  %t3 = icmp slt i32 %t1, 0
  br i1 %t3, label %ct, label %cf
ct:
  store i32 1, i32* %c
  br label %eif
cf:
  store i32 -1, i32* %c
  br label %eif
eif:
  %t4 = load i32, i32* %b2
  %t5 = load i32, i32* %c
  %t6 = add i32 %t4, %t5
  store i32 %t6, i32* %b2
  br label %loop
endf:
  %t7 = load i32, i32* %b2
  ret i32 %t7
}
