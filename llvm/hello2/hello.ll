@msg = internal constant [13 x i8] c"Hello World!\00"

declare i32 @puts(i8*)

define i32 @main() {
  call i32 @puts(i8* getelementptr inbounds ([13 x i8]* @msg, i32 0, i32 0))
  ret i32 0
}
