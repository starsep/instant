.class public FibArg
.super java/lang/Object

.method public <init>()V
  aload_0
  invokespecial java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
.limit stack 3
getstatic java/lang/System/out Ljava/io/PrintStream;
.catch java/lang/Exception from ParseBegin to ParseEnd using Handler
ParseBegin:
  aload 0
  iconst_0
  aaload
  invokestatic java/lang/Integer/parseInt(Ljava/lang/String;)I
  goto PrintResult
ParseEnd:
Handler:
  ldc "Failure :C"
  invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
  goto Ret
PrintResult:
  invokestatic Fib/fib(I)I
  invokevirtual java/io/PrintStream/println(I)V
Ret:
  return
.end method
