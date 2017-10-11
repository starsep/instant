.class public Inc
.super java/lang/Object

.method public <init>()V
  aload_0
  invokespecial java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
.limit stack 2
  getstatic java/lang/System/out Ljava/io/PrintStream;
  bipush 41
  invokestatic Inc/foo(I)I
  invokevirtual java/io/PrintStream/println(I)V
  return
.end method

.method private static foo(I)I
.limit stack 2
  iload_0
  iconst_1
  iadd
  ireturn
.end method
