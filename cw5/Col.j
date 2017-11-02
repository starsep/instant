.class public Col
.super java/lang/Object

.method public static f([III)V
    aload_0
    iload_2
    iconst_1
    isub
calculate_expr:
    iload_1
    iflt zero
    iload_1
    iload_2
    if_icmpge zero
    aload_0
    iload_1
    iaload
    ifgt zero
one:
    iconst_1
    iastore
    return
zero:
    iconst_0
    iastore
    return
.end method
