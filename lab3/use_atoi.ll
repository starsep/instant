declare i32 @atoi(i8*);

define i32 @main(i32 %argc, i8** %argv) {
  %ptr = getelementptr i8*, i8** %argv, i32 1 
  %val = load i8*, i8** %ptr 
  %res = call i32 @atoi(i8* %val)
  ret i32 %res 
}
