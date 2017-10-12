.class public Fib
.super java/lang/Object

.method public <init>()V
  aload_0
  invokespecial java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
.limit stack 2
  getstatic java/lang/System/out Ljava/io/PrintStream;
  bipush 13
  invokestatic Fib/fib(I)I
  invokevirtual java/io/PrintStream/println(I)V
  return
.end method

.method public static fib(I)I
.limit stack 2
.limit locals 4
  ; int x = 1
  iconst_1
  istore 1
  ; int y = 1
  iconst_1
  istore 2
While:
  ; while (n != 0)
  iload 0
  ifeq Ret
  ; int z = x + y
  iload 1
  iload 2
  iadd
  istore 3
  ; x = y
  iload 2
  istore 1
  ; y = z
  iload 3
  istore 2
  ; n = n + (-1)
  iload 0
  iconst_m1
  iadd
  istore 0
  goto While
Ret:
  iload 1
  ireturn
.end method
