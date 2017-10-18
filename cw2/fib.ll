define i32 @fib(i32 %n) {
entry:
  %n2 = alloca i32
  store i32 %n, i32* %n2
  %x = alloca i32
  store i32 0, i32* %x
  %y = alloca i32
  store i32 1, i32* %y
  %z = alloca i32
  br label %check
check:
  %c = icmp sgt i32 %n, 0
  br i1 %c, label %loop, label %return
loop:
  %xv = load i32, i32* %x
  %yv = load i32, i32* %y
  %zv = add i32 %xv, %yv
  store i32 %zv, i32* %y
  store i32 %yv, i32* %x
  %nv = load i32, i32* %n2
  %n1 = add i32 %nv, -1
  br label %check
return:
  %result = load i32, i32* %x
  ret i32 %result
}
