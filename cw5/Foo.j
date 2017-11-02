.class public Foo
.super java/lang/Object

.method public static foo([III)V
.limit locals 6
.limit stack 10

goto L58

L3:
iconst_1
iadd
istore_3
iload_3
iconst_1
iadd
istore 4
iload_3
istore 5
iload 4
iload_2

if_icmpge L35

aload_0
iload 4
iaload
aload_0
iload 5
iaload

if_icmple L35

iload 4
istore 5

L35:
aload_0
iload 5
iaload
aload_0
iload_1
iaload

if_icmpgt L48

goto L69

L48:
aload_0
iload 5
istore_1
iload_1
invokestatic C/bar([III)V
iload 5
istore_1

L58:
iload_1
iconst_1
ishl
dup
iload_2
iconst_1
isub

if_icmplt L3

pop
L69:
return

.end method
