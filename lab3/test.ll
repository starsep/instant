declare void @printInt(i32) ;  w innym module
define i32 @main() {
  %i1 = add i32 2, 2
  call void @printInt(i32 %i1)
  ret i32 0
}
