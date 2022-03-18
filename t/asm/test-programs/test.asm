foo:    
  irmovq $12,%rdx
  irmovq $14,%rcx
  addq %rdx,%rcx
  jmp foo
