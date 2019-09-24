module Stack
where

type Stack a = a -- complete this type definition (if you don't want to use a synonym type, that is also fine)

test   = ( "s0 = newStack = ",        s0,'\n'
         , "s1 = push 1 s0 = ",       s1,'\n'
         , "s2 = pushes [2..5] s1 = ",s2,'\n'
         , "s3 = pop s2 = ",          s3,'\n'
         , "s4 = popn 3 s3 = ",       s4,'\n'
         , "s5 = top s4 = ",          s5,'\n'
         , "s6 = topn 3 s2 = ",       s6,'\n'
         , "s7 = elements s2 = ",     s7,'\n'
         )
  where
      s0 = undefined -- newStack
      s1 = undefined -- push 1 s0
      s2 = undefined -- pushes [2..5] s1
      s3 = undefined -- pop s2
      s4 = undefined -- popn 3 s3
      s5 = undefined -- top s4
      s6 = undefined -- topn 3 s2
      s7 = undefined -- elements s2
