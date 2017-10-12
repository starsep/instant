%2 = mul i32 %k, 2
%3 = add i32 %2, 1
%4 = getelementptr i32* %a, i32 %3
%5 = getelementptr i32* %e, i32 %2
%6 = load i32, i32* %4
store i32 %6, i32* %5
