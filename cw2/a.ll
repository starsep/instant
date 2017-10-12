define i32 @f(i32* %a, i32 %b, i32 %c, i32 %d, i32 %e) {
  entry:
    %0 = sub i32 %c, 1
    %1 = mul i32 %b, %0
    %2 = sub i32 %e, %1
    %3 = add i32 %2, %d
    %i = alloca i32
    store i32 %3, i32* %i
    ret i32 42
}
